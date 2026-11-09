;;;; scan.lisp --- the rules: a string in, a verdict and masked findings out.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Deterministic and local: no network, no file, no model. A line is read
;;;; once, every rule in turn, and what comes back never carries the secret --
;;;; a finding names the rule, the line, and a mask that is the token's
;;;; public prefix and its length, which is what the original plugin showed.
;;;;
;;;; Verdicts, unchanged from the original:
;;;;   CLEAR  -- no rule matched
;;;;   REVIEW -- suspicious, not conclusive: a long high-entropy string, or a
;;;;             path to a credential file. A person decides.
;;;;   LEAKED -- a known provider format. Fail closed: treat it as leaked.
;;;;
;;;; The scanner never signals: an internal error is one REVIEW finding, so a
;;;; broken scan can never quietly pass.

(in-package #:nodecode-secret-gate)

(defparameter +clear+ "CLEAR")
(defparameter +review+ "REVIEW")
(defparameter +leaked+ "LEAKED")

;;; --- the rules ---------------------------------------------------------------
;;; A rule is (NAME SOURCE HINT VALUE-P): SOURCE is a cl-ppcre expression,
;;; and VALUE-P says the secret is what follows the assignment's first : or =
;;; rather than the whole match.

(defparameter +rules+
  (list
   (list "aws_access_key" "\\bAKIA[0-9A-Z]{16}\\b"
         "AWS access key ID" nil)
   (list "github_token" "\\b(?:ghp|gho|ghu|ghs|ghr)_[A-Za-z0-9]{20,}\\b"
         "GitHub personal / OAuth / refresh token" nil)
   (list "github_pat" "\\bgithub_pat_[A-Za-z0-9_]{22,}\\b"
         "GitHub fine-grained personal access token" nil)
   (list "slack_token" "\\bxox[baprs]-[A-Za-z0-9-]{10,}\\b"
         "Slack token" nil)
   (list "private_key" "-----BEGIN (?:RSA |EC |OPENSSH |PGP |DSA )?PRIVATE KEY-----"
         "Private key block" nil)
   (list "jwt" "\\beyJ[A-Za-z0-9_-]{10,}\\.eyJ[A-Za-z0-9_-]{10,}\\.[A-Za-z0-9_-]{10,}\\b"
         "JSON Web Token" nil)
   (list "openai_key" "\\bsk-(?:proj-)?[A-Za-z0-9_-]{20,}\\b"
         "OpenAI-style API key" nil)
   (list "generic_api_key"
         "(?i)\\b(?:api[_-]?key|apikey|client[_-]?secret|access[_-]?token)\\s*[:=]\\s*['\"]?([A-Za-z0-9_\\-/.+=]{20,})['\"]?"
         "Key or secret assigned in code or config" 1)
   (list "password_assignment"
         "(?i)\\bpassword\\s*[:=]\\s*['\"]([^'\"]{8,})['\"]"
         "Password assigned in code or config" 1))
  "The named credential formats: a match is LEAKED.")

(defparameter +scanners+
  (mapcar (lambda (rule)
            (list rule (cl-ppcre:create-scanner (second rule))))
          +rules+)
  "RULE and its compiled scanner, built once at load.")

(defparameter +secret-path+
  (cl-ppcre:create-scanner
   (concatenate 'string
                "(?:~|/home/[A-Za-z0-9_.-]+|/Users/[A-Za-z0-9_.-]+|%USERPROFILE%|C:[\\\\]Users[\\\\][A-Za-z0-9_.-]+)"
                "[/\\\\](?:\\.ssh[/\\\\](?:id_[a-z0-9]+|known_hosts|config)|\\.aws[/\\\\]credentials|"
                "\\.gnupg[/\\\\]|\\.kube[/\\\\]config|\\.docker[/\\\\]config\\.json)"
                "|(?:^|[\\s\"'])(?:\\.env(?:\\.[A-Za-z0-9_-]+)?|credentials\\.json|id_rsa|id_ed25519)(?:[\\s\"']|$)")
   :case-insensitive-mode t)
  "A path to a credential file: REVIEW, since a path is not the secret.")

(defparameter +token-candidate+
  (cl-ppcre:create-scanner "[A-Za-z0-9_\\-/.+=]{32,}"))

(defparameter *min-entropy* 4.0
  "The Shannon entropy, in bits per character, a token must reach to be REVIEW.
The cell sets it from its section at start.")

(defparameter +min-entropy-length+ 32)

;;; --- the mask ------------------------------------------------------------------

(defun public-prefix (match)
  "The recognisable, non-secret head of MATCH, as the original showed it."
  (let ((prefix (cl-ppcre:scan-to-strings "\\A[A-Za-z]{2,4}_" match)))
    (cond (prefix prefix)
          ((cl-ppcre:scan "\\AAKIA" match) "AKIA")
          ((cl-ppcre:scan "\\AeyJ" match) "eyJ")
          ((cl-ppcre:scan "\\Ask-" match) "sk-")
          ((cl-ppcre:scan "\\Axox" match) (subseq match 0 (min 4 (length match))))
          ((cl-ppcre:scan "\\A-----BEGIN" match) "-----BEGIN")
          (t (subseq match 0 (min 4 (length match)))))))

(defun masked (match)
  "MATCH as a finding shows it: its public prefix and its length, never itself."
  (format nil "~a... (~d chars)" (public-prefix match) (length match)))

;;; --- the entropy heuristic -------------------------------------------------------

(defun shannon-entropy (text)
  "Bits per character over TEXT's own symbol distribution."
  (let ((counts (make-hash-table :test #'eql)) (n (length text)))
    (when (zerop n) (return-from shannon-entropy 0.0))
    (loop for c across text do (incf (gethash c counts 0)))
    (let ((entropy 0.0))
      (maphash (lambda (character count)
                 (declare (ignore character))
                 (let ((p (/ count n)))
                   (decf entropy (* p (log p 2)))))
               counts)
      entropy)))

(defun char-classes (text)
  "How many of lowercase, uppercase, digit and other TEXT draws on."
  (+ (if (find-if #'lower-case-p text) 1 0)
     (if (find-if #'upper-case-p text) 1 0)
     (if (find-if #'digit-char-p text) 1 0)
     (if (find-if (lambda (c) (not (alphanumericp c))) text) 1 0)))

(defun entropy-candidate-p (token)
  "TOKEN looks like a credential by shape alone: long, mixed, high entropy."
  (and (>= (length token) +min-entropy-length+)
       (not (cl-ppcre:scan "://" token))
       (not (cl-ppcre:scan "\\A(?:https?|www\\.)" token))
       (>= (char-classes token) 3)
       (>= (shannon-entropy token) *min-entropy*)))

;;; --- one line, then the whole text ------------------------------------------------

(defun finding (rule line masked severity hint)
  (list :rule rule :line line :masked masked :severity severity :hint hint))

(defun matched-secret (line start end value-p)
  "The secret inside the match LINE[START,END): the whole match, or -- for an
assignment rule, VALUE-P -- what stands after its first : or =, unquoted. The
group a regex would capture, read here, so one expression carries it."
  (let ((match (subseq line start end)))
    (if (not value-p)
        match
        (let ((at (position-if (lambda (c) (member c '(#\: #\=))) match)))
          (and at (string-trim " '\"" (subseq match (1+ at))))))))

(defun scan-line (line &optional (lineno 1))
  "Every finding in LINE, which is line LINENO of the text being scanned."
  (let ((found '()))
    (dolist (pair +scanners+)
      (destructuring-bind (rule scanner) pair
        (destructuring-bind (name source hint value-p) rule
          (declare (ignore source))
          (loop for (start end) on (cl-ppcre:all-matches scanner line) by #'cddr
                for secret = (matched-secret line start end value-p)
                when (and secret (plusp (length secret)))
                  do (push (finding name lineno (masked secret) +leaked+ hint) found)))))
    (when (cl-ppcre:scan +secret-path+ line)
      (push (finding "secret_path" lineno "path to a credential file" +review+
                     "References a credential file (SSH key, AWS credentials, kubeconfig, .env). Review before sharing.")
            found))
    (loop for token in (cl-ppcre:all-matches-as-strings +token-candidate+ line)
          when (entropy-candidate-p token)
            do (push (finding "high_entropy_token" lineno (masked token) +review+
                              "Long high-entropy string that looks like a token. Confirm it is not a credential before sharing.")
                     found))
    (nreverse found)))

(defun lines-of (text)
  "TEXT split the way the original's splitlines did: one entry, at least."
  (let ((lines '()) (start 0))
    (loop for at = (position #\Newline text :start start)
          do (push (subseq text start at) lines)
          while at do (setf start (1+ at)))
    (or (nreverse lines) (list ""))))

(defun dedupe (findings)
  "FINDINGS without a repeat of the same rule, line and mask."
  (let ((seen '()) (kept '()))
    (dolist (item findings (nreverse kept))
      (let ((key (list (getf item :rule) (getf item :line) (getf item :masked))))
        (unless (member key seen :test #'equal)
          (push key seen)
          (push item kept))))))

(defun scan-text (text)
  "Scan TEXT. => (:VERDICT one of CLEAR/REVIEW/LEAKED, :FINDINGS a list,
: LINES a count). Never signals; an internal error is one REVIEW finding."
  (let ((findings '()))
    (handler-case
        (loop for line in (lines-of text)
              for lineno from 1
              do (setf findings (nconc findings (scan-line line lineno))))
      (error (condition)
        (push (finding "scanner_error" 0
                       (format nil "scanner failed (~a)" (type-of condition))
                       +review+
                       "The scan itself errored; treat the text as unreviewed.")
              findings)))
    (setf findings (dedupe findings))
    (list :verdict (cond ((find-if (lambda (f) (string= (getf f :severity) +leaked+)) findings) +leaked+)
                         (findings +review+)
                         (t +clear+))
          :findings findings
          :lines (if (plusp (length text)) (length (lines-of text)) 0))))
