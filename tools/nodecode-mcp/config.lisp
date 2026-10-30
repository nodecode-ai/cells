;;;; config.lisp --- the mcp section of the shared config, typed.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The contract is the retired Zig client's, verbatim:
;;;;
;;;;   "mcp": {
;;;;     "servers": {
;;;;       "files":  {"command": "mcp-files", "args": ["--root", "/tmp"],
;;;;                  "env": {"K": "v"}, "timeout_ms": 30000},
;;;;       "remote": {"url": "https://host/mcp", "headers": {"Authorization": "..."}},
;;;;       "off":    {"command": "x", "enabled": false}
;;;;     },
;;;;     "inherit_env_allowlist": ["PATH", "HOME", ...]
;;;;   }
;;;;
;;;; A url without a command is streamable HTTP, anything else is stdio; an
;;;; explicit "transport" ("stdio" | "http" | "streamable-http", or an object
;;;; with that "type") wins. No cwd, no per-tool allow/deny, no OAuth — as
;;;; the Zig client had none. Header values are secrets: nothing in this cell
;;;; ever prints one; surfaces show a count. A header or env value may name
;;;; its secret rather than hold it (references, below), so the config need
;;;; carry none.
;;;;
;;;; Each entry validates on its own: a refused entry becomes a spec carrying
;;;; the refusal text (the server shows as `refused` in every status) and its
;;;; siblings still run. The kernel's typed accessors (nlk:config-*) signal
;;;; NLK:CONFIG-REFUSAL, which is the one condition caught here.

(in-package #:nodecode-mcp)

(defparameter +default-timeout-ms+ 30000
  "Per-request budget when an entry names none (the Zig default_call_timeout_ms).")

(defparameter +default-inherit-env+
  '("PATH" "HOME" "USER" "LANG" "LC_ALL" "LC_CTYPE" "TMPDIR" "TMP" "TEMP"
    "SHELL" "TERM" "COLORTERM" "NO_COLOR" "TZ")
  "Environment a child server inherits when the section names no allowlist.")

;;; The serving image's own fd 2 is severed to a log while a TUI owns the
;;; terminal, so a child must never inherit it; a file the operator can name
;;; beats /dev/null when a server dies at startup.
(nlk:define-startup-parameter *log-directory*
  (nlk:home "mcp/")
  "Where a stdio server's stderr lands, one append-only file per server.")

(defstruct (server-spec (:copier nil))
  "One validated mcp.servers entry."
  (name "" :type string)
  (transport :stdio :type keyword)    ; :stdio | :http
  (command nil)                       ; string, stdio
  (args '() :type list)               ; strings, stdio
  (env '() :type list)                ; ((name . value) ...), stdio
  (url nil)                           ; string, http
  (headers '() :type list)            ; ((name . value) ...), http
  (timeout-ms +default-timeout-ms+ :type integer)
  (enabled-p t)
  (refusal nil))                      ; config-refusal text, or NIL

(nlk:access (spec server-spec))

(defun string-alist (section key)
  "KEY's object as ((name . value) ...) with string values, or '() when absent."
  (multiple-value-bind (value present) (nlk::section-value section key)
    (cond ((not present) '())
          ((hash-table-p value)
           (sort (loop for name being the hash-keys of value using (hash-value entry)
                       do (unless (stringp entry)
                            (nlk:config-error "~a.~a must be a string, got ~s" key name entry))
                       collect (cons name entry))
                 #'string< :key #'car))
          (t (nlk:config-error "~a must be an object, got ~s" key value)))))

;;; --- references -------------------------------------------------------------
;;;
;;; A header or env value may name its secret rather than hold it:
;;;
;;;   "Authorization": "Bearer ${GITHUB_TOKEN}"          a variable of this process
;;;   "Authorization": "Bearer ${env:GITHUB_TOKEN}"      the same
;;;   "Authorization": "Bearer ${file:~/.secrets/gh}"    a file's text, trimmed
;;;
;;; ${NAME} is how Claude Code's .mcp.json and trellage write one. The spec
;;; keeps the reference and each connect resolves it afresh, so a rotated
;;; secret takes the next reconnect and the value lives only in the
;;; transport: never in a spec, a status or a refusal. A background
;;; gateway's unit passes none of a shell's variables; ${file:...} is the
;;; reference that reaches it.

(nlk:define-error reference-unresolved (error) (detail)
  (:report "~a" detail)
  (:documentation "A reference names a variable that is unset or empty, or a
file that cannot be read: DETAIL names the reference, never a value."))

(defun variable-name-p (text)
  "Whether TEXT is a variable name: [A-Za-z_][A-Za-z0-9_]*."
  (and (plusp (length text))
       (not (digit-char-p (char text 0)))
       (every (lambda (char) (or (char= char #\_) (char<= #\a char #\z)
                                 (char<= #\A char #\Z) (char<= #\0 char #\9)))
              text)))

(defun parse-reference (inner where)
  "(:env NAME INNER) or (:file PATH INNER) for INNER, the text between ${ and }."
  (flet ((refuse ()
           (nlk:config-error "~a: ${~a} is not a reference this client reads: ${NAME}, ${env:NAME} or ${file:PATH}"
                             where inner)))
    (cond ((variable-name-p inner) (list :env inner inner))
          ((uiop:string-prefix-p "env:" inner)
           (if (variable-name-p (subseq inner 4)) (list :env (subseq inner 4) inner) (refuse)))
          ((uiop:string-prefix-p "file:" inner)
           (let ((path (subseq inner 5)))
             ;; The gateway's working directory is nobody's choice: no relative path.
             (unless (or (uiop:string-prefix-p "~/" path)
                         (uiop:absolute-pathname-p (ignore-errors (uiop:parse-native-namestring path))))
               (nlk:config-error "~a: ${~a} needs an absolute path or one under ~~/" where inner))
             (list :file path inner)))
          (t (refuse)))))

(defun reference-pieces (text where &aux (pieces '()) (start 0))
  "TEXT as its pieces: literal strings, and a PARSE-REFERENCE list for each
${...}. WHERE (\"headers.Authorization\") heads a config refusal."
  (loop
    (let ((open (search "${" text :start2 start)))
      (unless open
        (when (< start (length text)) (push (subseq text start) pieces))
        (return (nreverse pieces)))
      ;; Only the reference is ever quoted: an unclosed ${ may be a literal secret's.
      (let ((close (or (position #\} text :start open)
                       (nlk:config-error "~a: a ${ is unclosed" where))))
        (when (< start open) (push (subseq text start open) pieces))
        (push (parse-reference (subseq text (+ open 2) close) where) pieces)
        (setf start (1+ close))))))

(defun checked-references (alist key)
  "ALIST, the ((name . value) ...) of KEY (\"headers\", \"env\"), once every
reference in its values parses."
  (dolist (pair alist alist)
    (reference-pieces (cdr pair) (format nil "~a.~a" key (car pair)))))

(defun reference-path (path)
  "PATH as a pathname, a leading ~/ read as the home."
  (uiop:parse-native-namestring
   (if (uiop:string-prefix-p "~/" path)
       (concatenate 'string (uiop:native-namestring (user-homedir-pathname)) (subseq path 2))
       path)))

(defun reference-value (piece where one-line)
  "What PIECE names, trimmed; REFERENCE-UNRESOLVED when it names nothing,
holds a NUL, or (ONE-LINE, a header's rule) a line break."
  (destructuring-bind (kind name inner) piece
    (flet ((unresolved (control &rest arguments)
             (error 'reference-unresolved :detail (format nil "~a: ~?" where control arguments))))
      (let* ((raw (ecase kind
                    (:env (uiop:getenv name))
                    (:file (handler-case (uiop:read-file-string (reference-path name))
                             (error (condition)
                               (unresolved "cannot read ${~a}: ~a" inner condition))))))
             (value (and raw (nlk:trimmed raw))))
        (cond ((null raw) (unresolved "${~a} is unset" inner))
              ((zerop (length value)) (unresolved "${~a} is empty" inner))
              ((find (code-char 0) value) (unresolved "${~a} holds a NUL" inner))
              ((and one-line (find-if (lambda (char) (member char '(#\Return #\Newline))) value))
               (unresolved "${~a} holds a line break" inner))
              (t value))))))

(defun resolve-references (text where &key (one-line t))
  "TEXT with each reference in it replaced by what it names. WHERE heads a
refusal; ONE-LINE, a header's rule, refuses a value holding a line break."
  (if (search "${" text)
      (format nil "~{~a~}" (mapcar (lambda (piece)
                                     (if (stringp piece) piece (reference-value piece where one-line)))
                                   (reference-pieces text where)))
      text))

(defun resolved-alist (alist key &key (one-line t))
  "ALIST, the ((name . value) ...) of KEY, with every value's references resolved."
  (mapcar (lambda (pair)
            (cons (car pair)
                  (resolve-references (cdr pair) (format nil "~a.~a" key (car pair)) :one-line one-line)))
          alist))

(defun parse-server-entry (name entry)
  "A SERVER-SPEC for ENTRY, or one carrying the refusal text."
  (nlk:with-handlers ((nlk:config-refusal (condition)
                        (make-server-spec :name (if (stringp name) name (princ-to-string name))
                                          :enabled-p nil
                                          :refusal (princ-to-string condition))))
    ;; [A-Za-z0-9_-]+: what a generated symbol and a log file name can carry.
    (unless (and (stringp name)
                 (plusp (length name))
                 (every (lambda (char) (or (alphanumericp char) (find char "_-"))) name))
      (nlk:config-error "server name ~s: only letters, digits, _ and -" name))
    (unless (hash-table-p entry)
      (nlk:config-error "server ~a must be an object" name))
    (let* ((command (nlk:config-string entry "command"))
           (url (nlk:config-string entry "url"))
           ;; An explicit transport (a string, or an object with that type) wins.
           (transport (multiple-value-bind (value present) (nlk::section-value entry "transport")
                        (if (not present)
                            (if (and url (null command)) :http :stdio)
                            (let ((type (cond ((stringp value) value)
                                              ((hash-table-p value) (nlk:config-string value "type"))
                                              (t (nlk:config-error
                                                  "transport must be a string or an object, got ~s"
                                                  value)))))
                              (cond ((member type '("stdio") :test #'string-equal) :stdio)
                                    ((member type '("http" "streamable-http" "streamable_http")
                                             :test #'string-equal)
                                     :http)
                                    (t (nlk:config-error
                                        "transport must be stdio or http, got ~s" type))))))))
      (ecase transport
        (:stdio
         (unless command
           (nlk:config-error "server ~a: stdio needs a command" name)))
        (:http
         (unless (and url (or (uiop:string-prefix-p "http://" url)
                              (uiop:string-prefix-p "https://" url)))
           (nlk:config-error "server ~a: http needs an http(s) url" name))))
      (make-server-spec
       :name name
       :transport transport
       :command command
       :args (nlk:config-string-list entry "args")
       :env (checked-references (string-alist entry "env") "env")
       :url url
       :headers (checked-references (string-alist entry "headers") "headers")
       :timeout-ms (let ((value (nlk:config-integer entry "timeout_ms" 0)))
                     (cond ((zerop value) +default-timeout-ms+)
                           ((minusp value)
                            (nlk:config-error "timeout_ms must be positive, got ~a" value))
                           (t value)))
       :enabled-p (nlk:config-boolean entry "enabled" t)))))

(defun parse-mcp-section (section)
  "(values SPECS ALLOWLIST): the servers of SECTION in name order, each its
own validation, and the env allowlist."
  ;; A missing or non-object section is no servers at all.
  (if (hash-table-p section)
      (let ((servers (gethash "servers" section))
            (allowlist
              (if (nth-value 1 (nlk::section-value section "inherit_env_allowlist"))
                  (handler-case
                      (nlk:config-string-list section "inherit_env_allowlist")
                    (nlk:config-refusal (condition)
                      (warn "mcp: ~a; using the default allowlist" condition)
                      +default-inherit-env+))
                  +default-inherit-env+)))
        (values
         (when (hash-table-p servers)
           (mapcar (lambda (name) (parse-server-entry name (gethash name servers)))
                   (sort (loop for name being the hash-keys of servers collect name)
                         #'string<)))
         allowlist))
      (values '() +default-inherit-env+)))

(defun child-environment (spec allowlist
                          &aux (env (resolved-alist spec.env "env" :one-line nil)))
  "The child's whole environment as \"NAME=VALUE\" strings: the allowlisted
variables that exist here (exported shell functions, the \"() {\" values,
skipped as the reference SDK skips them), then the entry's env on top, its
references resolved."
  (mapcar (lambda (pair) (format nil "~a=~a" (car pair) (cdr pair)))
          (append (loop for name in allowlist
                        for value = (uiop:getenv name)
                        when (and value (not (uiop:string-prefix-p "() {" value))
                                  (not (assoc name env :test #'string=)))
                          collect (cons name value))
                  env)))

(defun stderr-log-path (name)
  (merge-pathnames (format nil "~a.stderr.log" name) *log-directory*))

(defun transport-text (spec)
  "The transport for a status line: the command or the url, a header count
and never a header value."
  (ecase spec.transport
    (:stdio (format nil "stdio ~a~{ ~a~}"
                    spec.command spec.args))
    (:http (let ((count (length spec.headers)))
             (format nil "http ~a~@[ (~d header~:p)~]" spec.url (and (plusp count) count))))))
