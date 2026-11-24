;;;; cell.lisp --- the scan, the redactor, the local audit, and the wiring.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Three tools and one command over local string and file work: nothing here
;;;; dials a service, and nothing here writes. A scan reads a text and answers
;;;; a decision with the rules that made it; the redactor masks the spans its
;;;; patterns and validators agree on; the audit reads the Nodecode home and
;;;; reports what it found, masked, with the names of things and never their
;;;; values. The rule engine is fail-open by design: a text too long for the
;;;; scan budget is reported as incomplete rather than as clean.

(in-package #:nodecode-secure-audit)

(defparameter +scan-tool+ "security_scan_text")
(defparameter +redact-tool+ "security_redact_text")
(defparameter +audit-tool+ "security_audit")

(defvar *scans* 0 "How many texts were scanned since the cell started.")
(defvar *blocked* 0 "How many of them came back block.")

(defparameter +manual+
  "Three read-only tools, all local: no service, no model, no key.
security_scan_text reads one text and answers allow, review or block with the
severity-weighted rules that fired (English and Chinese), after folding
zero-width characters, full-width letters and lookalike Cyrillic and Greek
ones; a long base64 run in the text is decoded and scanned too. block_score and
review_score in the cell's section move the two lines. An incomplete scan
says so: never read its allow as clean.
security_redact_text masks what its patterns and their checks agree is
personal: email addresses, credentials in a URL, API keys, mainland ID cards
(the check digit decides), bank cards (Luhn decides), mobile numbers and IPv4
addresses, plus any random-looking token.
security_audit reads the Nodecode home and reports, in a shareable form:
config values that look like secrets, a config file other users can read,
secret-like environment variables, how many cell folders are installed, and a
gateway bound to every interface.
  /secure-audit          the local audit, summarised
  /secure-audit TEXT     that text scanned, in one line per hit"
  "What (help :secure-audit) answers while the cell runs.")

;;; --- the scan ------------------------------------------------------------------

(defun rule-hits (text)
  "The rules TEXT trips, each as (RULE . VIA), VIA naming the derived text
that tripped it."
  (let ((hits '()))
    (dolist (rule +injection-rules+)
      (when (cl-ppcre:scan (injection-rule-scanner rule) text)
        (push (cons rule "plain") hits)))
    (when (and (null hits) (needs-normalising-p text))
      (let ((normalised (normalise text)))
        (dolist (rule +injection-rules+)
          (when (cl-ppcre:scan (injection-rule-scanner rule) normalised)
            (push (cons rule "normalised") hits)))))
    (when (null hits)
      (cl-ppcre:do-matches-as-strings (token +base64-token-scanner+ text)
        (let ((decoded (base64-decode token)))
          (when (and decoded (plusp (length decoded)))
            (dolist (rule +injection-rules+)
              (when (cl-ppcre:scan (injection-rule-scanner rule) decoded)
                (push (cons rule "base64") hits)))))))
    hits))

(defun snippet (text rule)
  "What TEXT shows of RULE's match: one masked line, at most 80 characters."
  (let* ((scanner (injection-rule-scanner rule))
         (start (or (cl-ppcre:scan scanner text) 0))
         (end (or (nth-value 1 (cl-ppcre:scan scanner text)) (length text)))
         (from (max 0 (- start 20)))
         (to (min (length text) (+ end 20))))
    (subseq (redact-text (subseq text from to)) 0 (min 80 (- to from)))))

(defun scan-text (text)
  "Scan TEXT. Answers a plist: :decision allow|review|block, :score 0-100,
:hits ((:id :severity :description :snippet :via) ...), :complete-p."
  (let* ((limit (max 1 (or (ignore-errors (setting :scan-max-length)) 200000)))
         (input (or text ""))
         (complete-p (<= (length input) limit))
         (scanned (subseq input 0 (min (length input) limit)))
         (hits (rule-hits scanned))
         (score (min 100 (reduce #'+ hits :key (lambda (hit) (injection-rule-weight (car hit)))
                                 :initial-value 0)))
         (critical (find-if (lambda (hit) (string= "critical" (injection-rule-severity (car hit)))) hits))
         (block-score (or (ignore-errors (setting :block-score)) 80))
         (review-score (or (ignore-errors (setting :review-score)) 50))
         (decision (cond ((or critical (>= score block-score)) "block")
                         ((>= score review-score) "review")
                         (t "allow"))))
    (incf *scans*)
    (when (string= decision "block") (incf *blocked*))
    (list :decision decision :score score :complete-p complete-p
          :hits (mapcar (lambda (hit)
                          (let ((rule (car hit)))
                            (list :id (injection-rule-id rule)
                                  :severity (injection-rule-severity rule)
                                  :description (injection-rule-description rule)
                                  :snippet (snippet scanned rule)
                                  :via (cdr hit))))
                        (sort (copy-list hits) #'string< :key (lambda (hit) (injection-rule-id (car hit))))))))

(defun scan-report (plist)
  "The scan as the tool and the command answer it: the decision first, then
one line per rule that fired."
  (let ((hits (getf plist :hits)))
    (with-output-to-string (out)
      (format out "scan: ~a (risk score ~d/100)~%" (getf plist :decision) (getf plist :score))
      (unless (getf plist :complete-p)
        (format out "~%  incomplete: the text ran past scan_max_length, so this covers the start of it only"))
      (if (null hits)
          (format out "~%  no rule fired: an allow means nothing matched, not that the text is safe")
          (dolist (hit hits)
            (format out "~%  - ~a (~a, via ~a): ~a — \"~a\""
                    (getf hit :id) (getf hit :severity) (getf hit :via)
                    (getf hit :description) (getf hit :snippet)))))))

;;; --- the redactor --------------------------------------------------------------

(defun collect-spans (text)
  "Every span of TEXT a rule claims and its validator agrees with, as
(START END ID), widest first where two overlap."
  (let ((spans '()))
    (dolist (rule +pii-rules+)
      (cl-ppcre:do-matches (start end (pii-rule-scanner rule) text)
        (let ((value (subseq text start end)))
          (when (or (null (pii-rule-validator rule))
                    (funcall (pii-rule-validator rule) value))
            (push (list start end (pii-rule-id rule)) spans)))))
    (let ((sorted (sort spans (lambda (a b)
                                (or (< (first a) (first b))
                                    (and (= (first a) (first b)) (> (second a) (second b))))))))
      (let ((end -1) (kept '()))
        (dolist (span sorted (nreverse kept))
          (when (>= (first span) end)
            (push span kept)
            (setf end (second span))))))))

(defun mask-span (length)
  "LENGTH copies of the operator's mask character."
  (let ((ch (let ((configured (ignore-errors (setting :mask-char))))
              (if (and (stringp configured) (plusp (length configured))) (char configured 0) #\*))))
    (make-string length :initial-element ch)))

(defun redact-text (text)
  "TEXT with every confirmed span masked, and the finding count per type."
  (let* ((text (or text ""))
         (spans (collect-spans text))
         (counts '()))
    (dolist (span spans)
      (let ((cell (assoc (third span) counts :test #'string=)))
        (if cell (incf (cdr cell)) (push (cons (third span) 1) counts))))
    (values (if (null spans)
                text
                (with-output-to-string (out)
                  (let ((position 0))
                    (dolist (span spans)
                      (write-string text out :start position :end (first span))
                      (write-string (mask-span (- (second span) (first span))) out)
                      (setf position (second span)))
                    (write-string text out :start position))))
            (sort counts #'string< :key #'car))))

(defun pii-label (id)
  "What ID is called in an answer."
  (or (let ((rule (find id +pii-rules+ :key #'pii-rule-id :test #'string=)))
        (and rule (pii-rule-label rule)))
      id))

(defun redact-report (text)
  "The redacted TEXT and what was masked, as the tool answers it."
  (multiple-value-bind (masked counts) (redact-text text)
    (format nil "~a~%~%~a"
            masked
            (if (null counts)
                "redacted: nothing found; the text comes back as it was"
                (format nil "redacted: ~{~a~^, ~}"
                        (loop for (id . count) in counts
                              collect (format nil "~d ~a~@[~a~]" count (pii-label id)
                                              (if (= count 1) "" "s"))))))))

;;; --- the local audit -----------------------------------------------------------

(defun audit-base (path)
  "The directory the audit reads: PATH when the call names one, else the
operator's Nodecode home."
  (if (and (stringp path) (plusp (length (nlk:trimmed path))))
      (uiop:ensure-directory-pathname (nlk:trimmed path))
      (let ((home (uiop:getenv "NODECODE_HOME")))
        (if (and home (plusp (length home)))
            (uiop:ensure-directory-pathname home)
            (uiop:ensure-directory-pathname
             (merge-pathnames ".nodecode/" (user-homedir-pathname)))))))

(defparameter +secret-name-scanner+
  (cl-ppcre:create-scanner "(?:token|secret|password|passwd|api[_-]?key|access[_-]?key|private[_-]?key|client[_-]?secret|credential)"
                            :case-insensitive-mode t))

(defparameter +config-secret-scanner+
  (cl-ppcre:create-scanner "\"([A-Za-z0-9_.-]+)\"\\s*:\\s*\"([^\"]{8,})\"")
  "A key and a value written in the config, both quoted.")

(defparameter +reference-value-scanner+
  (cl-ppcre:create-scanner "^(?:\\$\\{[^}]*\\}|\\$[A-Za-z_][A-Za-z0-9_]*|env:[A-Za-z_][A-Za-z0-9_]*|<[^>]*>)$")
  "A value that names where the secret lives instead of holding it.")

(defun config-file (base)
  "The config file the audit reads under BASE, or NIL."
  (let ((file (merge-pathnames "config.jsonc" base)))
    (and (probe-file file) file)))

(defun read-config (base)
  "BASE's config text, or NIL when there is none to read."
  (let ((file (config-file base)))
    (and file (handler-case (uiop:read-file-string file) (error () nil)))))

(defun config-secret-keys (text)
  "The key names in TEXT whose value looks like a secret written in clear."
  (let ((names '()))
    (when text
      (cl-ppcre:do-register-groups (key value) (+config-secret-scanner+ text)
        (when (and (cl-ppcre:scan +secret-name-scanner+ key)
                   (not (cl-ppcre:scan +reference-value-scanner+ value)))
          (pushnew (string-downcase key) names :test #'string=))))
    (sort names #'string<)))

(defun file-mode-octal (file)
  "FILE's permission bits, or NIL where the platform will not say."
  (handler-case (logand (sb-posix:stat-mode (sb-posix:stat (namestring file))) #o777)
    (error () nil)))

(defun env-secret-names ()
  "The names, never the values, of secret-shaped environment variables."
  (let ((names '()))
    (dolist (entry (sb-ext:posix-environ))
      (let ((split (position #\= entry)))
        (when (and split (plusp split))
          (let ((name (subseq entry 0 split))
                (value (subseq entry (1+ split))))
            (when (and (plusp (length value)) (cl-ppcre:scan +secret-name-scanner+ name))
              (push name names))))))
    (sort names #'string<)))

(defun cell-folder-count (base)
  "How many folders sit under BASE/cells, or NIL when there is no such
directory."
  (let ((dir (merge-pathnames "cells/" base)))
    (when (probe-file dir)
      (length (uiop:directory-files dir)))))

(defun wildcard-bind-p (text)
  "Whether TEXT writes a host that answers on every interface."
  (and text
       (cl-ppcre:scan "\"(?:host|bind|listen)\"\\s*:\\s*\"(?:0\\.0\\.0\\.0|::|\\*)\"" text)))

(defun audit-checks (base)
  "The audit's rows, in a fixed order: (ID STATUS MESSAGE) with STATUS one of
pass, warn, info."
  (let* ((config (read-config base))
         (keys (config-secret-keys config))
         (file (config-file base))
         (mode (and file (file-mode-octal file)))
         (env (env-secret-names))
         (cells (cell-folder-count base))
         (wild (wildcard-bind-p config)))
    (list (list "config-secrets" (if keys "warn" "pass")
                (if keys
                    (format nil "~d secret-shaped value(s) written in clear: ~{~a~^, ~}" (length keys) keys)
                    "no secret-shaped value written in clear"))
          (list "config-permissions"
                (cond ((null mode) "info")
                      ((zerop (logand mode #o077)) "pass")
                      (t "warn"))
                (cond ((null mode) "no config file to read, or the platform keeps the mode to itself")
                      ((zerop (logand mode #o077)) "the config is readable by its owner only")
                      (t (format nil "the config is group or world readable (mode ~o): ~a"
                                 mode "chmod 600 it"))))
          (list "env-secrets" (if env "warn" "pass")
                (if env
                    (format nil "~d secret-shaped environment variable(s): ~{~a~^, ~}" (length env) env)
                    "no secret-shaped environment variable is set"))
          (list "cells-inventory" "info"
                (if cells
                    (format nil "~d cell folder(s) installed" cells)
                    "no cells directory at this home"))
          (list "gateway-bind" (if wild "warn" "pass")
                (if wild
                    "the gateway is configured to bind every interface"
                    "no wildcard bind in the config")))))

(defun audit-report (path)
  "The audit of PATH (or the operator's home) as one shareable text: the
counts first, then a line per check."
  (let* ((base (audit-base path))
         (rows (audit-checks base))
         (statuses (mapcar #'second rows)))
    (format nil "audit of ~a~%pass ~d / warn ~d / info ~d~%~{~a~^~%~}"
            (namestring base)
            (count "pass" statuses :test #'string=)
            (count "warn" statuses :test #'string=)
            (count "info" statuses :test #'string=)
            (loop for (id status message) in rows
                  collect (format nil "  ~a ~a: ~a" status id message)))))

;;; --- /secure-audit -------------------------------------------------------------

(defun run-slash (args session-id)
  "The /secure-audit command: the audit with nothing typed, the scan of ARGS
otherwise."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (values (if (plusp (length text))
                (scan-report (scan-text text))
                (audit-report nil))
            nil)))

;;; --- the tools ----------------------------------------------------------------

(nlk:define-contract scan-text-request
  (:doc "Scan a text for prompt-injection and jailbreak attempts here, on this machine, with no service and no model: the text is folded (zero-width, full-width, lookalike letters), a long base64 run in it is decoded, and the rule rows that fire answer allow, review or block with a risk score and a masked snippet each. It is a heuristic: an allow means no rule matched, not that the text is safe.")
  ("text" :text :required t :doc "the text to scan: a user message, a prompt, a file, a tool result")
  ("mask_text" :boolean :default t :doc "mask personal data in the snippets the answer quotes"))

(nlk:define-contract redact-text-request
  (:doc "Mask personal data in TEXT here, on this machine, with no service and no model: email addresses, credentials in a URL, vendor API keys, mainland ID cards (check digit), bank cards (Luhn), mobile numbers and IPv4 addresses. Answers the masked text and a count per kind, so read the counts: a kind it has no pattern for is not masked.")
  ("text" :text :required t :doc "the text to mask"))

(nlk:define-contract audit-request
  (:doc "Run a read-only audit of the local Nodecode home: config values that look like secrets, a config file other users can read, secret-like environment variables, how many cell folders are installed, and a gateway bound to every interface. It reads and never writes, and it names things rather than quoting their values, so its report is safe to paste. Absence of findings is not a clean bill: the checks are a fixed, short list.")
  ("path" :string :default "" :doc "the home to audit; empty reads the operator's own Nodecode home")
  ("profile" :string :default "full" :options '("quick" "full") :doc "quick reads only the config; full runs every check"))

(defun scan-tool (request)
  "The security_scan_text tool."
  (scan-report (scan-text (scan-text-request-text request))))

(defun redact-tool (request)
  "The security_redact_text tool."
  (redact-report (redact-text-request-text request)))

(defun audit-tool (request)
  "The security_audit tool: whose profile picks how much of the home is read."
  (let ((path (audit-request-path request)))
    (if (string= "quick" (audit-request-profile request))
        (let* ((base (audit-base path))
               (rows (remove "cells-inventory" (audit-checks base) :key #'first :test #'string=)))
          (format nil "audit (quick) of ~a~%~{~a~^~%~}" (namestring base)
                  (loop for (id status message) in rows
                        collect (format nil "  ~a ~a: ~a" status id message))))
        (audit-report path))))

;;; --- the cell ------------------------------------------------------------------

(nlk:define-peripheral secure-audit :not-running t)

(nle:define-cell secure-audit
  (:section ("secure-audit")
    (:guide "nothing here is required: the rules run on their five defaults; raise block_score to block less, lower it to block more; audit_path points the audit at another home; mask_char is what a masked span shows")
    ("block_score" :integer :default 80 :min 1
                   :doc "risk score, 0-100, at or above which a scan answers block; any critical rule hit blocks on its own")
    ("review_score" :integer :default 50 :min 1
                    :doc "risk score at or above which a scan answers review")
    ("scan_max_length" :integer :default 200000 :min 1
                       :doc "how much of a text one scan reads; the rest is reported as unscanned")
    ("mask_char" :string :default "*" :doc "the character a masked span repeats")
    ("audit_path" :path :doc "another home for security_audit to read; empty reads this operator's Nodecode home"))
  (:tool +scan-tool+ (nlk:contract 'scan-text-request) 'scan-tool)
  (:tool +redact-tool+ (nlk:contract 'redact-text-request) 'redact-tool)
  (:tool +audit-tool+ (nlk:contract 'audit-request) 'audit-tool)
  (:command "secure-audit" 'run-slash
            :description "Audit this home, or scan the text you typed"
            :argument-hint "[TEXT]")
  (:help :secure-audit "secure-audit: scans text for injection, masks Chinese PII, audits this home" +manual+)
  (:start (lambda ()
            (setf *scans* 0 *blocked* 0)
            (nle:on-stop (lambda () (setf *scans* 0 *blocked* 0))))))
