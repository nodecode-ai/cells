;;;; scrub.lisp --- what leaves this machine, made safe to paste.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two readings of a text a user is about to share or is about to take in:
;;;; SCRUB-PII takes out what looks like a secret or a person (an email, a
;;;; token, a home path, a phone, an address, the account's own name) and
;;;; answers the counts it redacted; SCAN-TEXT reads a stranger's agents.md for
;;;; the things an agents.md can do to a model (a prompt injection, a piped
;;;; shell, a destructive command, a credential hunt, a hidden character).
;;;;
;;;; Both are regular expressions over text, so both are best effort and the
;;;; cell says so every time it answers: no model is asked, nothing is
;;;; guaranteed, and the user is the one who decides a file is safe to share.

(in-package #:nodecode-myswarm)

;;; --- the scrubber ---------------------------------------------------------------

(defparameter +redaction-rules+
  (list (list "url_credentials"
              "([a-z][a-z0-9+.-]*://)([^/\\s:@]+:[^/\\s@]+)@"
              "\\1[REDACTED_CREDENTIALS]@")
        (list "secrets_and_tokens"
              "\\b(?:sk-[A-Za-z0-9_-]{16,}|gh[pousr]_[A-Za-z0-9]{20,}|github_pat_[A-Za-z0-9_]{20,}|xox[baprs]-[A-Za-z0-9-]{10,}|AIza[0-9A-Za-z_-]{30,}|AKIA[0-9A-Z]{16}|glpat-[A-Za-z0-9_-]{16,})\\b"
              "[REDACTED_SECRET]")
        (list "secrets_and_tokens"
              "(?i)\\b(api[-_]?key|secret[-_]?key|access[-_]?token|auth[-_]?token|password|passwd|bearer|client[-_]?secret)(\\s*[:=]\\s*|\\s+)(['\"]?[A-Za-z0-9_.~+/-]{12,}['\"]?)"
              "\\1\\2[REDACTED_SECRET]")
        (list "emails" "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}\\b" "[REDACTED_EMAIL]")
        (list "user_paths" "/(?:Users|home)/[A-Za-z0-9._-]+" "~")
        (list "user_paths" "[A-Za-z]:\\\\Users\\\\[A-Za-z0-9._-]+" "~")
        (list "ssn" "\\b\\d{3}-\\d{2}-\\d{4}\\b" "[REDACTED_SSN]")
        (list "phone_numbers"
              "(^|[^0-9])(?:\\+\\d{1,3}[ .-]?)?(?:\\(\\d{3}\\)[ .-]?|\\d{3}[ .-])\\d{3}[ .-]\\d{4}([^0-9]|$)"
              "\\1[REDACTED_PHONE]\\2")
        (list "ip_addresses"
              "\\b(?:(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)\\.){3}(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)\\b"
              "[REDACTED_IP]")
        (list "personal_identifiers"
              "(?im)^(\\s*(?:[-*•]\\s*)?(?:\\*\\*|__)?(?:owner|author|user\\s*name|real\\s*name|full\\s*name|contact|created\\s*by|maintainer|employee|manager\\s*name|slack\\s*id|discord\\s*id|telegram\\s*id)(?:\\*\\*|__)?\\s*[:-]\\s*)(.+)$"
              "\\1[REDACTED_PII]"))
  "The deterministic rules, in the order they run: (CATEGORY PATTERN REPLACEMENT).
A replacement refers to the rule's own groups by number, so what names the
value (an API key's key, a URL's scheme) stays and the value itself goes.")

(defparameter +generic-users+
  '("user" "admin" "root" "ubuntu" "default" "runner" "home" "test" "guest" "agent")
  "Account names that are words in ordinary prose, so never redacted as one.")

(defun account-names ()
  "The names this machine's account goes by, as far as the text can tell: the
login, and the home folder's own name."
  (let ((names '()))
    (dolist (candidate (list (ignore-errors (uiop:getenv "USER"))
                              (ignore-errors (uiop:getenv "LOGNAME"))
                              (ignore-errors (pathname-name
                                              (car (last (pathname-directory (user-homedir-pathname))))))))
      (let ((name (trimmed (or candidate ""))))
        (when (and (>= (length name) 3)
                   (not (member (string-downcase name) +generic-users+ :test #'string=)))
          (pushnew name names :test #'string=))))
    names))

(defun export-header (agent-count)
  "The comment an exported file opens with, so a reader knows where it came from."
  (format nil "<!-- Exported via MySwarm cell | Agents: ~d -->" agent-count))

(defun scrub-pii (text)
  "TEXT with what looks like a secret or a person redacted.
=> (VALUES SCRUBBED COUNTS), COUNTS an alist of (CATEGORY . HOW-MANY)."
  (let ((text (cl-ppcre:regex-replace-all "(?im)^<!--\\s*Exported via (?:Hermes )?MySwarm[^>]*-->\\s*"
                                           (or text "") ""))
        (counts '()))
    (flet ((tally (category n)
             ;; cl-ppcre answers NIL when a rule matched nothing, and a bare T
             ;; (a match it counted no further) when it did.
             (let ((n (cond ((numberp n) n) (n 1) (t 0))))
               (when (plusp n)
                 (let ((cell (assoc category counts :test #'string=)))
                   (if cell (incf (cdr cell) n) (push (cons category n) counts)))))))
      (dolist (rule +redaction-rules+)
        (multiple-value-bind (clean n) (cl-ppcre:regex-replace-all (second rule) text (third rule))
          (setf text clean)
          (tally (first rule) n)))
      (dolist (name (account-names))
        (multiple-value-bind (clean n)
            (cl-ppcre:regex-replace-all (format nil "(?i)\\b~a\\b" (cl-ppcre:quote-meta-chars name))
                                         text "[REDACTED_USER]")
          (setf text clean)
          (tally "os_username" n))))
    (values text (nreverse counts))))

(defun counts-text (counts)
  "COUNTS as one line a person reads, or the words saying nothing was found."
  (if (null counts)
      "none found"
      (join (mapcar (lambda (cell) (format nil "~a ~d" (substitute #\Space #\_ (car cell)) (cdr cell)))
                    counts)
            ", ")))

;;; --- the stranger's file --------------------------------------------------------

(defparameter +zero-width-chars+
  (format nil "[~c~c~c~c~c]" (code-char 8203) (code-char 8204) (code-char 8205)
          (code-char 65279) (code-char 8232))
  "The characters a difference makes invisible: zero-width space, joiners, BOM.")

(defparameter +scan-rules+
  (list (list "prompt_injection" "medium"
              "(?i)(ignore (?:all |the |your )?(?:previous|prior|above) instructions|disregard (?:all |the |your )?(?:previous|prior|above)|you are now|new system prompt|jailbreak|developer mode)")
        (list "remote_code_execution" "high"
              "(?i)(?:\\b(?:curl|wget)\\b[^\\n|]{1,160}\\|\\s*(?:sudo\\s+)?(?:ba|z)?sh\\b|\\bbase64\\s+(?:-d|--decode)\\b|\\beval\\s*\\$\\()")
        (list "destructive_command" "high"
              "(?i)(?:\\brm\\s+-[rf]{1,2}\\s+(?:/|~|\\$HOME|\\*)|\\bmkfs\\.|\\bdd\\s+if=/dev/)")
        (list "credential_exfiltration" "high"
              "(?i)(?:\\.ssh/id_(?:rsa|ed25519)|\\.aws/credentials|/etc/shadow|\\b(?:send|upload|post|exfiltrate)\\b[^\\n]{0,80}\\b(?:api[_-]?key|token|secret|password)\\b)")
        (list "persistence_or_backdoor" "medium"
              "(?i)(?:>>\\s*~?/\\.(?:bashrc|zshrc|profile|bash_profile)|crontab\\s+-)")
        (list "hidden_obfuscation" "medium" +zero-width-chars+))
  "What an agents.md can say to a model that a person did not mean: (KIND SEVERITY PATTERN).")

(defun scan-text (text)
  "TEXT read for the things an imported agents.md could do to this workspace.
=> a list of plists, one a finding: :KIND, :SEVERITY, :LINE and :TEXT."
  (let ((findings '())
        (lines (lines-of (or text ""))))
    (dolist (rule +scan-rules+)
      (loop for line in lines
            for number from 1
            do (let ((match (cl-ppcre:scan-to-strings (third rule) line)))
                 (when match
                   (push (list :kind (first rule) :severity (second rule) :line number
                               :text (abbreviate line 120))
                         findings)))))
    (nreverse findings)))

(defun scan-high-p (findings)
  (some (lambda (finding) (string= (getf finding :severity) "high")) findings))

(defun findings-text (findings)
  "FINDINGS as the lines a person reads, or the words saying the file is clean."
  (if (null findings)
      "no warning: nothing an agents.md can do to a model was found (best effort)"
      (join (mapcar (lambda (finding)
                      (format nil "[~a] line ~d: ~a~%    ~a"
                              (getf finding :severity) (getf finding :line)
                              (substitute #\Space #\_ (getf finding :kind))
                              (getf finding :text)))
                    findings)
            (string #\Newline))))
