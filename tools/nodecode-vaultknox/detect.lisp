;;;; detect.lisp --- the detector registry, the overlap merge and the redaction.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The 28 patterns of the plugin's detectors.py, carried over one to one as
;;;; cl-ppcre scanners: a key, the name the operator sees, a severity and the
;;;; pattern. Nothing here reads a socket, a file or an environment variable:
;;;; one text in, the spans found in it out, and the spans replaced once.

(in-package #:nodecode-vaultknox)

(defparameter +redaction+ "[REDACTED-SENSITIVE-VALUE]"
  "What every matched span becomes, as the plugin spelled it.")

(defstruct (detector (:constructor make-detector (key name severity pattern)))
  "One registry row: KEY names it in the setting, NAME in an answer."
  (key "" :type string)
  (name "" :type string)
  (severity "high" :type string)
  (pattern "" :type string)
  (scanner nil))

(defparameter +detector-specs+
  '(("openai" "OpenAI API key" "critical" "sk-[A-Za-z0-9_-]{20,}")
    ("github-pat" "GitHub personal access token (classic)" "critical" "ghp_[A-Za-z0-9]{36,}")
    ("github-oauth" "GitHub OAuth access token" "critical" "gho_[A-Za-z0-9]{36,}")
    ("github-fine-grained" "GitHub fine-grained token" "critical" "ghs_[A-Za-z0-9]{36,}")
    ("github-impersonation" "GitHub impersonation token" "critical" "ghu_[A-Za-z0-9]{36,}")
    ("github-refresh" "GitHub refresh token" "critical" "ghr_[A-Za-z0-9]{36,}")
    ("anthropic" "Anthropic API key" "critical" "sk-ant-[A-Za-z0-9_-]{40,}")
    ("aws-key-id" "AWS access key id" "critical" "AKIA[A-Z0-9]{16}")
    ("aws-secret" "AWS secret access key" "critical"
     "(?i)aws_secret_access_key\\s*[=:]\\s*['\"]?[A-Za-z0-9/+=]{40,}['\"]?")
    ("api-key" "API key assignment" "high"
     "(?i)(?:api[_-]?key|apikey)\\s*[=:]\\s*['\"]?[A-Za-z0-9_-]{20,}['\"]?")
    ("secret-key" "secret key assignment" "high"
     "(?i)(?:secret[_-]?key|secretkey)\\s*[=:]\\s*['\"]?[A-Za-z0-9_-]{20,}['\"]?")
    ("access-token" "access token assignment" "high"
     "(?i)access[_-]?token\\s*[=:]\\s*['\"]?[A-Za-z0-9_-]{20,}['\"]?")
    ("auth-token" "auth token assignment" "high"
     "(?i)auth[_-]?token\\s*[=:]\\s*['\"]?[A-Za-z0-9_-]{20,}['\"]?")
    ("password" "hardcoded password" "medium"
     "(?i)(?:password|passwd|pwd)\\s*[=:]\\s*['\"][^'\"]{8,}['\"]")
    ("bearer" "bearer token" "high"
     "(?i)bearer\\s+(?:token\\s*[=:]\\s*)?['\"]?[A-Za-z0-9_-]{20,}['\"]?")
    ("slack" "Slack token" "critical"
     "xox[baprs]-[0-9]{10,13}-[0-9]{10,13}-[A-Za-z0-9]{24,}")
    ("stripe-secret" "Stripe secret key" "critical" "sk_live_[A-Za-z0-9]{24,}")
    ("stripe-publishable" "Stripe publishable key" "medium" "pk_live_[A-Za-z0-9]{24,}")
    ("twilio" "Twilio API key" "critical" "SK[0-9a-fA-F]{32}")
    ("sendgrid" "SendGrid API key" "critical" "SG\\.[A-Za-z0-9_-]{22}\\.[A-Za-z0-9_-]{43}")
    ("npm" "npm access token" "critical" "npm_[A-Za-z0-9]{36}")
    ("private-key" "private key block" "critical"
     "-----BEGIN (?:RSA |DSA |EC |OPENSSH |PGP )?PRIVATE KEY-----")
    ("generic-secret" "secret variable assignment" "high"
     "(?i)[A-Za-z_][A-Za-z0-9_]*(?:secret)\\s*[=:]\\s*['\"]?[A-Za-z0-9_-]{16,}['\"]?")
    ("google" "Google API key" "critical" "AIza[0-9A-Za-z_\\-]{35}")
    ("gcp-service-account" "GCP service account private key" "critical"
     "-----BEGIN PRIVATE KEY-----")
    ("azure-storage" "Azure storage connection string" "critical"
     "DefaultEndpointsProtocol=https;AccountName=[^;\\s]+;AccountKey=[^;\\s]+;EndpointSuffix=[^;\\s]+")
    ("jwt" "JSON web token" "high"
     "eyJ[A-Za-z0-9_-]{10,}\\.[A-Za-z0-9_-]{10,}\\.[A-Za-z0-9_-]{10,}")
    ("high-entropy" "high entropy secret assignment" "high"
     "(?i)(?:token|secret|key|password)\\s*[=:]\\s*['\"]?[A-Za-z0-9+/=_-]{24,}['\"]?"))
  "The registry, in the plugin's order: key, name, severity, cl-ppcre pattern.")

(defun build-detectors ()
  "One detector a spec, each pattern compiled once at load."
  (mapcar (lambda (spec)
            (destructuring-bind (key name severity pattern) spec
              (let ((detector (make-detector key name severity pattern)))
                (setf (detector-scanner detector)
                      (ignore-errors (cl-ppcre:create-scanner pattern)))
                detector)))
          +detector-specs+))

(defparameter +detectors+ (build-detectors)
  "Every detector the registry carries, compiled once.")

(defparameter +detector-keys+ (mapcar #'detector-key +detectors+)
  "The keys the `detectors' setting accepts, in registry order.")

(defvar *detectors* (copy-list +detector-keys+)
  "The detector keys the running cell applies.")

(defparameter +severity-rank+ '(("critical" . 4) ("high" . 3) ("medium" . 2) ("low" . 1))
  "Which detector names a span two of them matched, higher wins.")

(defun severity-rank (severity)
  (or (cdr (assoc severity +severity-rank+ :test #'string-equal)) 0))

(defun find-detector (key)
  "The detector KEY names, case-blind, or NIL."
  (find key +detectors+ :test #'string-equal :key #'detector-key))

;;; --- spans ---------------------------------------------------------------------

(defstruct (hit (:constructor make-hit (start end key name severity)))
  "One detected span: [START, END) of the text, and what read it as a secret."
  (start 0 :type fixnum)
  (end 0 :type fixnum)
  (key "" :type string)
  (name "" :type string)
  (severity "high" :type string))

(defun match-spans (scanner text)
  "Every non-empty match of SCANNER in TEXT, as (START . END), left to right."
  (let ((spans '()) (from 0) (length (length text)))
    (when scanner
      (loop
        (multiple-value-bind (start end) (cl-ppcre:scan scanner text :start from)
          (unless start (return))
          (when (> end start) (push (cons start end) spans))
          (setf from (if (> end start) end (1+ start)))
          (when (>= from length) (return)))))
    (nreverse spans)))

(defun detector-hits (detector text)
  "Every match of DETECTOR in TEXT, as a HIT."
  (loop for (start . end) in (match-spans (detector-scanner detector) text)
        collect (make-hit start end (detector-key detector)
                          (detector-name detector) (detector-severity detector))))

(defun wanted-p (detector keys)
  (or (null keys) (member (detector-key detector) keys :test #'string-equal)))

(defun scan-hits (text &key (detectors *detectors*))
  "The hits in TEXT from DETECTORS (keys or detectors), merged so no two overlap,
the highest severity naming the group, in text order."
  (let ((keys (mapcar (lambda (one) (if (stringp one) one (detector-key one))) detectors)))
    (when (and text (plusp (length text)))
      (let ((hits (loop for detector in +detectors+
                        when (wanted-p detector keys)
                          append (detector-hits detector text))))
        (merge-hits hits (length text))))))

(defun hit-before-p (a b)
  "Whether A sorts before B: by start, the longer span first at one start."
  (or (< (hit-start a) (hit-start b))
      (and (= (hit-start a) (hit-start b)) (> (hit-end a) (hit-end b)))))

(defun merge-hits (hits length)
  "HITS as disjoint groups in text order: every overlap covered once, the
highest severity (then the earlier detector) naming the group."
  (let ((groups '()))
    (dolist (hit (sort (remove-if-not (lambda (hit)
                                        (and (<= 0 (hit-start hit))
                                             (< (hit-start hit) (hit-end hit))
                                             (<= (hit-end hit) length)))
                                      (copy-list hits))
                       #'hit-before-p))
      (let ((group (first groups)))
        (cond ((and group (< (hit-start hit) (hit-end group)))
               (setf (hit-end group) (max (hit-end group) (hit-end hit)))
               (when (> (severity-rank (hit-severity hit)) (severity-rank (hit-severity group)))
                 (setf (hit-severity group) (hit-severity hit)
                       (hit-name group) (hit-name hit)
                       (hit-key group) (hit-key hit))))
              (t (push (copy-hit hit) groups)))))
    (nreverse groups)))

(defun redact-with-hits (text hits)
  "TEXT with every HIT replaced by +REDACTION+ in one pass, or TEXT itself."
  (if (null hits)
      text
      (with-output-to-string (out)
        (let ((cursor 0))
          (dolist (hit hits)
            (write-string (subseq text cursor (hit-start hit)) out)
            (write-string +redaction+ out)
            (setf cursor (hit-end hit)))
          (write-string (subseq text cursor) out)))))

(defun redact-text (text &key (detectors *detectors*))
  "TEXT with every secret value replaced by a placeholder: (values REDACTED HITS).
Never signals: text a detector chokes on comes back as it was."
  (if (or (null text) (zerop (length text)))
      (values text '())
      (handler-case
          (let ((hits (scan-hits text :detectors detectors)))
            (values (redact-with-hits text hits) hits))
        (error () (values text '())))))

;;; --- what a found set says -----------------------------------------------------

(defun found-summary (hits)
  "One line on what HITS found, or that nothing was."
  (if (null hits)
      "nothing found: the text comes back as it was"
      (let ((counts '()))
        (dolist (hit hits)
          (let ((cell (assoc (hit-name hit) counts :test #'string=)))
            (if cell
                (incf (cdr cell))
                (push (cons (hit-name hit) 1) counts))))
        (format nil "redacted ~d secret value~:p: ~{~a~^, ~}"
                (length hits)
                (loop for (name . count) in (nreverse counts)
                      collect (format nil "~d ~a" count name))))))
