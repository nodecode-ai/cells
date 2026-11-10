;;;; scanner.lisp --- deterministic credential detection, no model, no network.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's table, unchanged: vendor credential formats plus one
;;;; assignment context (`api_key = ...', `password: ...'), so ordinary prose
;;;; and code identifiers do not trip the shield. A finding never carries the
;;;; secret itself into a ledger or a message: only its kind, its length and a
;;;; masked preview (the first four characters).
;;;;
;;;; Every pattern is the Python one, spelled for cl-ppcre: the same
;;;; boundaries, the same minimum lengths. A token glued to a word character
;;;; is not a token, and two matches over one run of characters are one
;;;; credential.

(in-package #:nodecode-secretshield)

(defparameter +redaction+ "[REDACTED by SecretShield]"
  "What a credential is replaced with in text this cell rewrites.")

(defparameter +detectors+
  ;; (KIND PATTERN GROUP): GROUP is the register whose span is the secret,
  ;; 0 the whole match -- the original's third column.
  '(("private_key_pem"
     "-----BEGIN (?:RSA |EC |DSA |OPENSSH |PGP |ENCRYPTED )?PRIVATE KEY-----" 0)
    ("anthropic_key" "sk-ant-[A-Za-z0-9_\\-]{20,}" 0)
    ("openai_key" "sk-(?:proj-)?[A-Za-z0-9_\\-]{20,}" 0)
    ("github_token" "(?:gh[pousr]_[A-Za-z0-9]{36,}|github_pat_[A-Za-z0-9_]{22,})" 0)
    ("aws_access_key"
     "\\b(?:AKIA|AGPA|AIDA|AROA|AIPA|ANPA|ANVA|ASIA|A3T[A-Z0-9])[A-Z0-9]{16}\\b" 0)
    ("google_api_key" "AIza[0-9A-Za-z_\\-]{35}" 0)
    ("slack_token" "xox[baprs]-[A-Za-z0-9\\-]{10,}" 0)
    ("stripe_key" "\\b[sr]k_(?:live|test)_[A-Za-z0-9]{16,}" 0)
    ("sendgrid_key" "SG\\.[A-Za-z0-9_\\-]{22}\\.[A-Za-z0-9_\\-]{43}" 0)
    ("npm_token" "npm_[A-Za-z0-9]{36}" 0)
    ("pypi_token" "pypi-[A-Za-z0-9_\\-]{16,}" 0)
    ("telegram_bot_token" "\\b[0-9]{8,10}:[A-Za-z0-9_\\-]{35}\\b" 0)
    ("discord_token"
     "\\b[MN][A-Za-z0-9_\\-]{23,28}\\.[A-Za-z0-9_\\-]{6}\\.[A-Za-z0-9_\\-]{27,40}\\b" 0)
    ("jwt"
     "\\beyJ[A-Za-z0-9_\\-]{8,2048}\\.eyJ[A-Za-z0-9_\\-]{8,2048}\\.[A-Za-z0-9_\\-]{8,2048}\\b" 0)
    ("bearer_credential" "(?i)\\bbearer\\s+([A-Za-z0-9._~+/=\\-]{16,})" 1)
    ("basic_auth_url" "(?i)\\bhttps?://[^/\\s:@]{2,}:([^/\\s:@]{4,})@" 1)
    ("assigned_secret"
     "(?i)\\b(?:api[_-]?key|apikey|client[_-]?secret|secret|passwd|password|pwd|access[_-]?token|auth[_-]?token|refresh[_-]?token|private[_-]?key)\\b\\s*[:=]\\s*[\"']?([A-Za-z0-9._~+/=\\-]{12,})[\"']?"
     1))
  "The detectors, in the original's order.")

(defun detector-kinds ()
  "Every kind this cell knows, as the section and the ledger spell them."
  (mapcar #'first +detectors+))

(defvar *scanners* (make-hash-table :test #'equal)
  "PATTERN -> the scanner compiled for it, built once and reused.")

(defun scanner-for (pattern)
  "PATTERN as a scanner: compiled once, then out of the cache."
  (or (gethash pattern *scanners*)
      (setf (gethash pattern *scanners*) (ppcre:create-scanner pattern))))

(defstruct (finding (:constructor make-finding (kind start end preview &optional path)))
  "One credential found: its kind, where it sits, and a preview that is not
the secret."
  (kind "" :type string)
  (start 0 :type fixnum)
  (end 0 :type fixnum)
  (preview "" :type string)
  (path nil))

(defun finding-length (finding)
  "How many characters the credential spans."
  (- (finding-end finding) (finding-start finding)))

(defun masked (value)
  "VALUE as a ledger may keep it: its first four characters, and four marks."
  (if (<= (length value) 4)
      "****"
      (concatenate 'string (subseq value 0 4) "****")))

(defun match-spans (text pattern group)
  "Every match of PATTERN in TEXT as (START . END) of its GROUP."
  (let ((spans '()) (from 0) (size (length text)))
    (loop while (< from size) do
      (multiple-value-bind (match-start match-end registers-start registers-end)
          (ppcre:scan (scanner-for pattern) text :start from)
        (if (null match-start)
            (return)
            (let* ((index (max 0 (1- group)))
                   (start (if (zerop group) match-start
                              (and registers-start
                                   (> (length registers-start) index)
                                   (aref registers-start index))))
                   (end (if (zerop group) match-end
                            (and registers-end
                                 (> (length registers-end) index)
                                 (aref registers-end index)))))
              (when (and start end (< start end))
                (push (cons start end) spans))
              (setf from (if (> match-end match-start) match-end (1+ match-start)))))))
    (nreverse spans)))

(defun overlaps-p (start end spans)
  "Whether START..END shares a character with any span in SPANS."
  (some (lambda (span) (and (< start (cdr span)) (> end (car span)))) spans))

(defun scan-text (text &optional disabled)
  "Every credential in TEXT as findings in text order: :KIND, :PREVIEW, spans,
never the secret itself. Spans never overlap; an earlier detector keeps its
match."
  (if (or (null text) (zerop (length text)))
      '()
      (let ((spans '()) (findings '()))
        (dolist (detector +detectors+)
          (destructuring-bind (kind pattern group) detector
            (unless (member kind disabled :test #'string=)
              (dolist (span (match-spans text pattern group))
                (destructuring-bind (start . end) span
                  (unless (overlaps-p start end spans)
                    (push span spans)
                    (push (make-finding kind start end (masked (subseq text start end)))
                          findings)))))))
        (sort findings #'< :key #'finding-start))))

(defun redact-text (text &optional disabled)
  "TEXT with every credential in it replaced, and the findings.
=> (values TEXT-OR-NIL FINDINGS)."
  (let ((findings (scan-text text disabled)))
    (if (null findings)
        (values nil findings)
        (values (with-output-to-string (out)
                  (let ((cursor 0))
                    (dolist (finding findings)
                      (write-string (subseq text cursor (finding-start finding)) out)
                      (write-string +redaction+ out)
                      (setf cursor (finding-end finding)))
                    (write-string (subseq text cursor) out)))
                findings))))

;;; --- a tool call's arguments, walked -------------------------------------------

(defun word-chars (path)
  "PATH with a member name quoted, so a ledger line reads as the call did."
  path)

(defun walk-value (value path disabled findings)
  "Push what VALUE (a string, a table, an array, or anything else) holds onto
FINDINGS, under PATH."
  (cond
    ((stringp value)
     (dolist (finding (scan-text value disabled) findings)
       (push (make-finding (finding-kind finding) (finding-start finding)
                           (finding-end finding) (finding-preview finding) path)
             findings)))
    ((hash-table-p value)
     ;; The pairs are read out first: the accumulator is threaded through
     ;; WALK-VALUE's own body, never closed over by MAPHASH's function.
     (let ((pairs '()))
       (maphash (lambda (key item) (push (cons key item) pairs)) value)
       (dolist (pair (nreverse pairs) findings)
         (setf findings
               (walk-value (cdr pair)
                           (if (and path (plusp (length path)))
                               (format nil "~a.~a" path (car pair))
                               (string (car pair)))
                           disabled findings)))))
    ((vectorp value)
     (dotimes (index (length value) findings)
       (setf findings
             (walk-value (aref value index) (format nil "~a[~d]" (or path "") index)
                         disabled findings))))
    (t findings)))

(defun scan-arguments (arguments &optional disabled)
  "Every credential in ARGUMENTS, each finding carrying the path it sat at."
  (stable-set (nreverse (walk-value arguments "" disabled '()))))

(defun stable-set (findings)
  "FINDINGS, each one's path kept, in the order they were met."
  findings)

(defun redacted-value (value disabled)
  "VALUE (a string, a table, an array, or anything else) with every credential
in it replaced: a copy where anything changed, VALUE itself where nothing did."
  (cond
    ((stringp value)
     (multiple-value-bind (redacted findings) (redact-text value disabled)
       (if findings redacted value)))
    ((hash-table-p value)
     (let ((copy (make-hash-table :test (hash-table-test value)))
           (changed nil))
       (maphash (lambda (key item)
                  (let ((new (redacted-value item disabled)))
                    (unless (eq new item) (setf changed t))
                    (setf (gethash key copy) new)))
                value)
       (if changed copy value)))
    ((vectorp value)
     (let* ((copy (copy-seq value)) (changed nil))
       (dotimes (index (length copy))
         (let ((new (redacted-value (aref copy index) disabled)))
           (unless (eq new (aref copy index)) (setf changed t))
           (setf (aref copy index) new)))
       (if changed copy value)))
    (t value)))

(defun redact-arguments (arguments &optional disabled)
  "ARGUMENTS with every credential replaced, and the findings that named them."
  (values (redacted-value arguments disabled) (scan-arguments arguments disabled)))

(defun describe-findings (findings)
  "One line naming what was found, as a person reads it: kind, preview, length."
  (let ((parts '()))
    (dolist (finding (subseq findings 0 (min 6 (length findings))))
      (push (format nil "~a (~a, ~d chars)~@[ in ~a~]"
                    (finding-kind finding) (finding-preview finding)
                    (finding-length finding) (finding-path finding))
            parts))
    (format nil "~{~a~^; ~}~@[, and ~d more~]"
            (nreverse parts) (let ((extra (- (length findings) 6)))
                               (and (plusp extra) extra)))))
