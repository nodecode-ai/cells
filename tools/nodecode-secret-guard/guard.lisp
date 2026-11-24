;;;; guard.lisp --- layer 1: the read a tool call may not make.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; WHAT THE ORIGINAL REFUSED, KEPT. Hermes checked every tool call's
;;;; arguments, not its tool's name -- a built-in, a plugin and an MCP server
;;;; all got the same check -- and refused four things: the keychain printing a
;;;; stored value, a password manager being asked for one, the whole environment
;;;; dumped (printenv, env, export -p), and a reader pointed at a file whose
;;;; name says it holds credentials (.env, *credential*, *token*, *.pem,
;;;; id_rsa, auth.json and the rest). A Nodecode call carries its arguments as
;;;; one JSON object, whose strings are read here; the argument names are not
;;;; consulted, because the one tool the model runs a command through is `eval`
;;;; and its form is where the command lives.
;;;;
;;;; WHAT A PATH HAS TO LOOK LIKE. The original read file names out of a
;;;; command's argv; here every string of the call is cut into tokens, and a
;;;; token is read as a path only when it holds a slash or a dot -- otherwise
;;;; the word "secret" in ordinary prose, or a call to a function called
;;;; SECRET, would be refused as a file.

(in-package #:nodecode-secret-guard)

(defparameter +secret-file-patterns+
  '("*.env" ".env" "*.env.*" "*credential*" "*token*" "*secret*" "*.pem" "*.pfx" "*.p12"
    "*.ppk" "*.key" "*.jks" "*.keystore" "id_rsa" "id_ed25519" "id_ecdsa" "*-keys.json"
    "auth.json" ".netrc" ".pgpass" ".npmrc" ".pypirc" ".git-credentials" "config.yaml")
  "A file name that says it holds credentials. The original's list, and the
  private-key formats and dotfiles a Linux home carries beside them.")

(defparameter +safe-extensions+ '(".sh" ".py" ".md" ".lisp" ".rb" ".js" ".ts")
  "Code and docs, which the original let through even when the name says
  `secret': a secret.sh is a script, not a credential store.")

(defparameter +gate-command-rules+
  '(("asks the keychain to print a stored value (find-generic-password -w)"
     . "find-(?:generic|internet)-password[^\\n]*\\s-[a-z]*[wg](?:\\s|$)")
    ("dumps the keychain" . "\\bdump-keychain\\b")
    ("asks a password manager for a stored value (secret-tool, pass show)"
     . "\\b(?:secret-tool\\s+(?:lookup|search)|pass\\s+show)\\b")
    ("dumps the whole environment, which holds live keys"
     . "(?:^|[\\s;&|(\"'\\n])(?:printenv|env|export\\s+-p)\\s*(?:$|[\"'|;&)\\n])"))
  "The commands that print secrets without naming a file: the reason, and the
  scanner. Read against the whole call's text, not token by token.")

(defun wildcard-regex (pattern)
  "PATTERN, a shell glob, as an anchored, case-insensitive regex."
  (with-output-to-string (out)
    (write-string "^" out)
    (loop for char across (string-downcase pattern)
          do (case char
               (#\* (write-string ".*" out))
               (#\? (write-string "." out))
               (t (when (find char ".+()[]{}|^$") (write-char #\\ out))
                  (write-char char out))))
    (write-string "$" out)))

(defparameter +file-scanners+
  (mapcar (lambda (pattern) (cons pattern (cl-ppcre:create-scanner (wildcard-regex pattern))))
          +secret-file-patterns+)
  "One scanner a pattern, made once.")

(defun ends-with-p (text suffix)
  (let ((at (- (length text) (length suffix))))
    (and (>= at 0) (string= text suffix :start1 at))))

(defun base-name (token)
  "TOKEN's last path segment."
  (let ((at (max (or (position #\/ token :from-end t) -1)
                 (or (position (code-char 92) token :from-end t) -1))))
    (subseq token (1+ at))))

(defun path-like-p (token)
  "Whether TOKEN could be a file name at all: it holds a slash or a dot."
  (or (find #\/ token) (find #\. token) (find (code-char 92) token)))

(defun secret-shaped-p (token)
  "Whether TOKEN names a file whose contents are credentials."
  (let ((name (string-downcase (base-name token))))
    (and (path-like-p token)
         (plusp (length name))
         (not (char= #\- (char name 0)))
         (not (some (lambda (extension) (ends-with-p name extension)) +safe-extensions+))
         (some (lambda (entry) (cl-ppcre:scan (cdr entry) name)) +file-scanners+))))

(defparameter +token-breaks+
  (map 'list #'char-code " \t\n\r\"'`;,|&()[]{}<>")
  "What separates one token of a call from the next.")

(defun argument-tokens (text)
  "TEXT cut into the tokens a path could be in."
  (remove "" (uiop:split-string text :separator +token-breaks+) :test #'string=))

(defun argument-text (arguments &optional (limit 20000))
  "Every string ARGUMENTS carries, one per line, at most LIMIT characters:
the call as the gate reads it. Objects, arrays and plists are walked."
  (let ((parts '()))
    (labels ((walk (value depth)
               (when (< depth 4)
                 (cond ((stringp value) (push value parts))
                       ((hash-table-p value)
                        (maphash (lambda (key member)
                                   (declare (ignore key))
                                   (walk member (1+ depth)))
                                 value))
                       ((vectorp value)
                        (loop for member across value do (walk member (1+ depth))))
                       ((consp value)
                        (loop for member in value do (walk member (1+ depth))))))))
      (walk arguments 0))
    (let ((text (format nil "~{~a~^~%~}" (nreverse parts))))
      (if (> (length text) limit) (subseq text 0 limit) text))))

(defun allowed-p (text allow)
  "Whether one of ALLOW's regexes exempts the call."
  (some (lambda (pattern)
          (and (stringp pattern)
               (plusp (length pattern))
               (ignore-errors (and (cl-ppcre:scan pattern text) t))))
        (if (listp allow) allow (coerce allow 'list))))

(defun gate-reason (text &optional allow)
  "Why TEXT may not run: one sentence, or NIL when it may. ALLOW, a list of
regexes, exempts the whole call."
  (when (and (stringp text) (plusp (length (nlk:trimmed text))))
    (unless (allowed-p text allow)
      (or (loop for (reason . pattern) in +gate-command-rules+
                when (ignore-errors (cl-ppcre:scan pattern text))
                  return reason)
          (loop for token in (argument-tokens text)
                when (secret-shaped-p token)
                  return (format nil "reads a secret-shaped file (~a)" (base-name token)))))))
