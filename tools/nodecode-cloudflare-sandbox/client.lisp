;;;; client.lisp --- the bridge to the Worker the operator deployed.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin this cell rewrites has two halves: a Worker (a Durable Object
;;;; over a Cloudflare Container) that the operator deploys to their own
;;;; account, and a client that talks to it. The Worker is the only half that
;;;; meets a Container, and the protocol between them is public (the plugin's
;;;; docs/protocol.md), so the rewrite is this file: one bearer token, one
;;;; NDJSON command stream, redirects never followed, no request replayed --
;;;; a lost answer can mean the command already ran, and every verb says so.

(in-package #:nodecode-cloudflare-sandbox)

;;; --- the section, read ------------------------------------------------------------

(defparameter +instances+ '("lite" "standard-1" "standard-2" "standard-3" "standard-4")
  "The Container instance types the Worker's own configuration allows.")

(defun trimmed (text)
  "TEXT without its edges, or NIL when it is not a string or is empty."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun endpoint-origin-p (endpoint)
  "Whether ENDPOINT is an origin the bridge may be dialled at: https, or the
plain http a local Worker is served on."
  (and (or (ppcre:scan "^https://[A-Za-z0-9.-]+(:[0-9]{1,5})?/?$" endpoint)
           (ppcre:scan "^http://(localhost|127\\.0\\.0\\.1|\\[::1\\])(:[0-9]{1,5})?/?$" endpoint))
       t))

(defun read-settings (values table)
  "The declared VALUES, plus what the section names: the token's value, the
endpoint without its trailing slash, and the names the workspace is made of."
  (declare (ignore table))
  (let* ((endpoint (trimmed (getf values :endpoint)))
         (image (or (trimmed (getf values :image)) "hermes"))
         (instance (trimmed (getf values :instance)))
         (token-env (trimmed (getf values :token-env)))
         (namespace (or (trimmed (getf values :namespace)) "default"))
         (workspace (trimmed (getf values :workspace)))
         (token (or (trimmed (getf values :token))
                    (and token-env (trimmed (uiop:getenv token-env))))))
    (when (and endpoint (not (endpoint-origin-p endpoint)))
      (nlk:config-error "endpoint ~s is not an https origin, and http answers only on loopback" endpoint))
    (when (and instance (not (member instance +instances+ :test #'equal)))
      (nlk:config-error "instance ~s is not one of ~{~a~^, ~}" instance +instances+))
    (unless (ppcre:scan "^[A-Za-z0-9_-]{1,128}$" image)
      (nlk:config-error "image ~s is not a name wrangler.jsonc deploys (letters, digits, _ and -)" image))
    (append (list :endpoint (and endpoint (string-right-trim "/" endpoint))
                  :token token
                  :token-env token-env
                  :namespace namespace
                  :workspace workspace
                  :image image
                  :instance instance)
            values)))

;;; --- the workspace, named ------------------------------------------------------------

(defvar *sandbox-name* nil
  "The workspace this run of the cell commands run in: stable across calls
when the section says persistent, one name per start when it does not.")

(defun hex-digest (text &optional (length 40))
  "LENGTH hex digits of TEXT's SHA-256, without the algorithm's prefix."
  (let* ((digest (nlk:sha256-text text))
         (start (1+ (or (position #\: digest) -1))))
    (subseq digest start (min (length digest) (+ start length)))))

(defun random-hex (length)
  "LENGTH lowercase hex digits no one else will send."
  (hex-digest (format nil "~a-~a-~a" (get-universal-time)
                       (get-internal-real-time) (random most-positive-fixnum))
              length))

(defun sandbox-id (namespace workspace &optional ephemeral)
  "The workspace name NAMESPACE and WORKSPACE hash to. The identity is
hashed: a path of the operator's never travels to the Worker. EPHEMERAL adds
a per-call suffix, so a recreated workspace never reattaches the last one."
  (let ((digest (hex-digest (format nil "~a~c~a" namespace #\Nul (or workspace "")))))
    (if ephemeral
        (format nil "nc-~a-~a" (subseq digest 0 24) (random-hex 16))
        (format nil "nc-~a" (subseq digest 0 39)))))

(defun default-workspace ()
  "What the workspace is named after when the section names none: this
machine's home, hashed before it leaves."
  (or (trimmed (uiop:getenv "HOME"))
      (ignore-errors (nlk:trimmed (namestring (user-homedir-pathname))))
      "nodecode"))

(defun install-sandbox-name ()
  "Name the workspace this start uses, from the section as it stands."
  (setf *sandbox-name* (sandbox-id (setting :namespace)
                                    (or (setting :workspace) (default-workspace))
                                    (not (setting :persistent)))))

(defun sandbox-name ()
  "The workspace this cell commands run in, named at start."
  (running-settings)
  (or *sandbox-name* (install-sandbox-name)))

(defun sandbox-path (&optional suffix)
  "The Worker's route for this workspace, SUFFIX appended."
  (format nil "/v1/sandboxes/~a~a" (sandbox-name) (or suffix "")))

(defun check-cwd (cwd)
  "CWD as the Worker takes it: an absolute path inside the container."
  (let ((cwd (or (trimmed cwd) (setting :cwd) "/workspace")))
    (unless (char= #\/ (char cwd 0))
      (fail "cwd ~s is not an absolute path in the sandbox" cwd))
    (when (find #\Nul cwd)
      (fail "cwd carries a NUL byte"))
    cwd))

;;; --- one exchange with the Worker ----------------------------------------------------

(defun worker-url (route)
  "ROUTE on the endpoint the section names."
  (let ((endpoint (setting :endpoint)))
    (unless endpoint
      (fail "the cloudflare-sandbox section names no endpoint: deploy the plugin's Worker and set its origin"))
    (concatenate 'string endpoint route)))

(defun http-request (method route &key body timeout)
  "METHOD ROUTE on the Worker, BODY an object or NIL encoded as JSON.
=> (values TEXT STATUS)."
  (let* ((json (and body (nlk:encode-json-object body)))
         (token (setting :token))
         (headers (append (list (cons "Authorization" (format nil "Bearer ~a" (or token "")))
                                (cons "Accept" "application/json"))
                          (when json (list (cons "Content-Type" "application/json"))))))
    (handler-case
        (nlk:http method (worker-url route) :headers headers :content json
                  :timeout (or timeout 90))
      (error (e)
        (if (search "timeout" (string-downcase (princ-to-string e)))
            (fail "the Cloudflare bridge did not answer within ~a seconds; the command may already have run: read the workspace before repeating it"
                  (or timeout 90))
            (fail "the Cloudflare bridge could not be reached (~a); the command may already have run"
                  (nlk:one-line (princ-to-string e) :cap 200)))))))

(defun exchange (method route &key body timeout)
  "METHOD ROUTE => (values TEXT STATUS), the status read off HTTP when the
call answers one."
  (multiple-value-bind (text status) (http-request method route :body body :timeout timeout)
    (values text (or status 200))))

(defun error-code (text)
  "The code the Worker's failure body TEXT carries, or a plain one."
  (let* ((object (ignore-errors (and (stringp text) (plusp (length (trimmed text)))
                                      (nlk:decode-json text))))
         (code (and object (nlk:json-value object :string "error" "code"))))
    (if (and code (ppcre:scan "^[a-z_]{1,64}$" code)) code "http_error")))

(defun bridge-failure (route status text)
  "Refuse with what the Worker said, and never promise the command did not run."
  (fail "the Cloudflare bridge answered HTTP ~d (~a) for ~a. A lost or refused answer can mean the command already ran: read the workspace before repeating it."
        status (error-code text) route))

(defun call-bridge (method route &key body timeout accept)
  "One request to the Worker. ACCEPT is the status list besides 2xx that
answers success. => the response's text."
  (multiple-value-bind (text status) (exchange method route :body body :timeout timeout)
    (unless (or (<= 200 status 299) (member status accept))
      (bridge-failure route status text))
    text))

(defun ensure-sandbox (&key (timeout 60))
  "Declare the workspace the section names. A 409 is a workspace already
declared with exactly these settings, which is what every call after the
first finds."
  (let ((members (list "persistent" (if (setting :persistent) t :false)
                       "cwd" (check-cwd nil)
                       "image" (setting :image))))
    (when (setting :instance)
      (setf members (append members (list "instance" (setting :instance)))))
    (call-bridge "POST" (sandbox-path)
                 :body (apply #'nlk:make-json-object members)
                 :timeout timeout
                 :accept (list 409))))

;;; --- the command stream ----------------------------------------------------------------

(defun split-lines (text)
  "TEXT's lines, without their terminators."
  (let ((lines '())
        (start 0))
    (loop for at = (position #\Newline text :start start)
          do (push (subseq text start (or at (length text))) lines)
          while at
          do (setf start (1+ at)))
    (nreverse lines)))

(defun parse-events (text)
  "The output and exit status the NDJSON command stream TEXT carries.
=> (values OUTPUT EXIT-CODE)."
  (let ((output (make-string-output-stream))
        (exit nil)
        (seen nil))
    (dolist (line (split-lines (or text "")))
      (let ((event (and (plusp (length line)) (ignore-errors (nlk:decode-json line)))))
        (when event
          (let ((type (nlk:json-value event :string "type")))
            (cond ((equal type "output")
                   (write-string (or (nlk:json-value event :string "data") "") output))
                  ((equal type "exit")
                   (setf exit (nlk:json-value event :integer "exit_code") seen t))
                  ((equal type "error")
                   (fail "the sandbox refused the command (~a); its outcome is unknown: read the workspace before repeating it"
                         (or (nlk:json-value event :string "code") "exec_failed"))))))))
    (unless seen
      (fail "the command stream ended without an exit status: its outcome is unknown"))
    (values (get-output-stream-string output) exit)))

;;; --- the verbs ------------------------------------------------------------------------

(define-verb run (command &key cwd stdin timeout login)
  "Run COMMAND, as bash reads it, in the operator's Cloudflare sandbox.
CWD is an absolute directory there, STDIN text for the command, TIMEOUT
seconds before the Worker kills it. => a plist (:exit-code N :output TEXT)."
  (running-settings)
  (unless (and (stringp command) (plusp (length (trimmed command))))
    (fail "run was given no command"))
  (let* ((seconds (or timeout (setting :timeout-seconds)))
         (body (nlk:make-json-object
                "request_id" (random-hex 32)
                ;; The Worker takes one command for its fixed supervisor, so the
                ;; directory is entered the way Hermes's own wrapping entered it.
                "command" (format nil "cd ~a && ~a" (shell-quote (check-cwd cwd)) command)
                "timeout" seconds
                "login" (and login t)
                "stdin" (and (stringp stdin) stdin))))
    (ensure-sandbox)
    (multiple-value-bind (output exit)
        (parse-events (call-bridge "POST" (sandbox-path "/exec") :body body
                                   :timeout (+ seconds 75)))
      (list :exit-code exit :output output))))

(define-verb health ()
  "Whether the Worker answers, without starting a container. => the Worker's
health object."
  (decode-answer (call-bridge "GET" "/health" :timeout 20)))

(define-verb release ()
  "Snapshot the workspace and stop its container: the files stay, the machine
does not. => the Worker's answer."
  (decode-answer (call-bridge "POST" (sandbox-path "/release")
                              :body (nlk:make-json-object) :timeout 120)))

(define-verb reset ()
  "Discard the workspace and its container, so the next command starts a clean
one. The Cloudflare-side snapshot itself is not deleted by this. => the
Worker's answer."
  (decode-answer (call-bridge "DELETE" (sandbox-path)
                              :body (nlk:make-json-object "confirm" t) :timeout 60)))

(defun decode-answer (text)
  "TEXT, a JSON body, as an object; an empty body is an empty object."
  (if (and (stringp text) (plusp (length (trimmed text))))
      (or (ignore-errors (nlk:decode-json text)) (nlk:make-json-object))
      (nlk:make-json-object)))

;;; --- files, over the same command stream ------------------------------------------------

(defun shell-quote (text)
  "TEXT as one word a shell hands back unchanged."
  (with-output-to-string (out)
    (write-char #\' out)
    (loop for char across (or text "")
          do (if (char= char #\')
                 (write-string "'\\''" out)
                 (write-char char out)))
    (write-char #\' out)))

(defun read-file (path)
  "The contents of PATH in the sandbox, with its exit status. => (values TEXT EXIT-CODE)."
  (let ((result (run (format nil "cat -- ~a" (shell-quote path)))))
    (values (getf result :output) (getf result :exit-code))))

(defun write-file (path text)
  "Write TEXT to PATH in the sandbox, through the command's standard input, so
no quoting of TEXT is involved. => (values EXIT-CODE OUTPUT)."
  (let ((result (run (format nil "cat > ~a" (shell-quote path)) :stdin (or text ""))))
    (values (getf result :exit-code) (getf result :output))))
