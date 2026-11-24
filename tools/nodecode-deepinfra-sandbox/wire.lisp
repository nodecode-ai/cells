;;;; wire.lisp --- DeepInfra's sandbox API, spoken to directly with nlk:http.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin wrapped DeepInfra's Python SDK. The SDK is a client of
;;;; a plain REST API -- create, get, start, stop, delete, exec (NDJSON),
;;;; read and write a file under /workspace -- so this cell is the client: one
;;;; bearer key, JSON bodies, no second program and nothing to install.
;;;;
;;;; One sandbox per home, named in a small id file beside the config so it
;;;; survives a restart: a stopped sandbox keeps /workspace and costs nothing,
;;;; and the next command resumes it. Only /workspace survives that cycle;
;;;; everything else in the container is gone on the next start, which is why
;;;; the file verbs refuse a path outside it.
;;;;
;;;; Every exchange goes through API, so a test stubs one name and reaches
;;;; nothing. A refused status or a transport failure becomes
;;;; DEEPINFRA-SANDBOX-ERROR carrying the service's own words, which is what
;;;; the model repairs from.

(in-package #:nodecode-deepinfra-sandbox)

(defparameter +default-endpoint+ "https://api.deepinfra.com"
  "Where every call goes unless the section names another address.")

(defparameter +exec-timeout-cap+ 1800
  "The longest one command may run, in seconds: the service's own cap.")

(defparameter +boot-attempts+ 90
  "How many times a wait looks at a sandbox that is still booting, a second apart.")

(defparameter +workspace+ "/workspace"
  "The one path a stop/start cycle keeps; the file calls reach nowhere else.")

(defvar *sandbox-id* nil
  "The sandbox this cell drives, or NIL before the first call makes one.")

(defvar *sandbox-state* nil
  "The state the last answer named: creating, starting, running, stopping, stopped, failed.")

(defvar *state-read* nil
  "Whether the id file has been read since the cell started; a test binds it.")

(defvar *state-file* nil
  "The file the id survives a restart in, or NIL for the one under the home.")

;;; --- the section ----------------------------------------------------------------

(defun nonempty (value)
  "VALUE when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) value))

(defun api-key ()
  "The key the section names inline or by variable, else DEEPINFRA_API_KEY."
  (or (nonempty (setting :api-key))
      (let ((variable (nonempty (setting :api-key-env))))
        (and variable (nonempty (uiop:getenv variable))))
      (nonempty (uiop:getenv "DEEPINFRA_API_KEY"))))

(defun check-config ()
  "Refuse a section that could reach DeepInfra with no key, naming the member."
  (unless (api-key)
    (error 'nlk:config-refusal
           :detail (concatenate 'string
                                "deepinfra-sandbox: this cell needs a key: set DEEPINFRA_API_KEY, or "
                                "\"api_key\" (or \"api_key_env\") in the deepinfra-sandbox section; "
                                "a key is made at deepinfra.com/dash/api_keys"))))

(defun endpoint ()
  "The API address this cell talks to, without a trailing slash."
  (let ((given (string-right-trim "/" (or (setting :base-url) ""))))
    (if (plusp (length given)) given +default-endpoint+)))

;;; --- the id that outlives a restart ---------------------------------------------

(defun id-file ()
  "The file the sandbox id is kept in."
  (or *state-file* (nlk:home "deepinfra-sandbox.id")))

(defun read-id-file ()
  "The id the last run left, or NIL."
  (let ((file (id-file)))
    (and (probe-file file)
         (ignore-errors
           (nonempty (string-trim '(#\Space #\Tab #\Newline #\Return)
                                   (uiop:read-file-string file)))))))

(defun write-id-file (id)
  "Keep ID on disk, or remove the file when ID is NIL. Best effort."
  (let ((file (id-file)))
    (ignore-errors
      (if id
          (progn (ensure-directories-exist file)
                 (with-open-file (out file :direction :output :if-exists :supersede
                                           :if-does-not-exist :create
                                           :external-format :utf-8)
                   (write-string id out)
                   (write-char #\Newline out)))
          (when (probe-file file) (delete-file file)))))
  id)

(defun remember (id &optional state)
  "Make ID the sandbox this cell drives, and keep it on disk."
  (setf *sandbox-id* id *sandbox-state* state *state-read* t)
  (write-id-file id)
  id)

(defun remembered-id ()
  "The sandbox this cell holds, reading the id file once after a start."
  (unless *state-read*
    (setf *state-read* t
          *sandbox-state* nil)
    ;; an id this session already made outranks the file the last one left
    (unless *sandbox-id*
      (setf *sandbox-id* (ignore-errors (read-id-file)))))
  *sandbox-id*)

(defun forget ()
  "Drop the sandbox this cell drives, on disk too."
  (remember nil nil))

;;; --- one exchange ---------------------------------------------------------------

(defparameter +unreserved+
  (load-time-value (map 'vector #'char-code "-._~/"))
  "The bytes a query path leaves as they are.")

(defun percent-encode (text)
  "TEXT as UTF-8 with everything outside the unreserved set percent-encoded."
  (with-output-to-string (out)
    (loop for byte across (nlk:utf-8-octets text)
          do (if (or (<= (char-code #\a) byte (char-code #\z))
                     (<= (char-code #\A) byte (char-code #\Z))
                     (<= (char-code #\0) byte (char-code #\9))
                     (find byte +unreserved+ :test #'=))
                 (write-char (code-char byte) out)
                 (format out "%~2,'0X" byte)))))

(defun header-value (headers name)
  "NAME's value from the response headers, or NIL: both of dexador's
backends lowercase every name."
  (and (hash-table-p headers) (gethash name headers)))

(defun api (method path &key content content-type binary (limit nil))
  "One call on the sandbox API => (values BODY STATUS HEADERS). A transport
failure is DEEPINFRA-SANDBOX-ERROR."
  (let* ((timeout (setting :timeout-seconds))
         (limit (or limit timeout))
         (headers (list (cons "authorization" (format nil "Bearer ~a" (or (api-key) "")))
                        (cons "accept" "application/json"))))
    (when content-type (push (cons "content-type" content-type) headers))
    (handler-case
        (multiple-value-bind (body status response-headers)
            (nlk:http method (concatenate 'string (endpoint) path)
                      :headers headers
                      :content content
                      :binary binary
                      :timeout limit
                      :connect-timeout (min 10 limit))
          (values body status response-headers))
      (error (condition)
        (fail "deepinfra sandbox: ~a"
              (nle:transport-failure-label condition (concatenate 'string (endpoint) path)))))))

(defun body-json (body)
  "BODY -- a JSON answer, text or octets -- as a decoded value; an empty body is an empty object."
  (cond ((hash-table-p body) body)
        ((and (stringp body) (plusp (length (nlk:trimmed body))))
         (handler-case (nlk:decode-json body) (error () (nlk:json-object))))
        ((and (vectorp body) (plusp (length body)))
         (handler-case (nlk:decode-json body) (error () (nlk:json-object))))
        (t (nlk:json-object))))

(defun service-error (body)
  "What a refused answer says, in the one line the model reads."
  (let ((json (and (or (stringp body) (vectorp body)) (ignore-errors (nlk:decode-json body)))))
    (or (nlk:json-value json :text "error")
        (nlk:json-value json :text "message")
        (nlk:json-value json :text "detail")
        (and (stringp body) (subseq body 0 (min 200 (length body)))))))

(defun check-status (what status body)
  "Refuse a refused STATUS, saying what the service said."
  (cond ((and (integerp status) (< status 300)) t)
        ((eql status 401)
         (fail "deepinfra sandbox ~a: HTTP 401, the key was refused; check api_key in the ~
                deepinfra_sandbox section" what))
        ((eql status 404)
         (fail "deepinfra sandbox ~a: HTTP 404~@[: ~a~]" what (service-error body)))
        ((eql status 409)
         (fail "deepinfra sandbox ~a: HTTP 409, the sandbox is in the wrong state for that; ~
                call deepinfra_sandbox with status and try again" what))
        ((eql status 429)
         (fail "deepinfra sandbox ~a: HTTP 429, the account is at its sandbox limit or rate ~
                limited; wait a moment and try again" what))
        (t (fail "deepinfra sandbox ~a: HTTP ~a~@[: ~a~]" what status (service-error body)))))

;;; --- the sandbox ---------------------------------------------------------------

(defun sandbox-path (id)
  "The sandbox's own address."
  (format nil "/v1/sandboxes/~a" id))

(defun create-sandbox ()
  "Make a sandbox and answer its id: the plan the section names, else the
service's default, kept as long as the section says."
  (let ((request (nlk:make-json-object
                  "tags" (nlk:make-json-object "nodecode_cell" "deepinfra-sandbox")
                  "timeout_seconds" (* 60 (setting :idle-minutes)))))
    (let ((plan (nonempty (setting :plan))))
      (when plan (setf (gethash "plan" request) plan)))
    (multiple-value-bind (body status)
        (api :post "/v1/sandboxes"
             :content (nlk:encode-json-object request)
             :content-type "application/json")
      (check-status "create" status body)
      (let ((id (nlk:json-value (body-json body) :text "sandbox_id")))
        (unless (nonempty id)
          (fail "deepinfra sandbox create: the answer named no sandbox_id"))
        (remember id "creating")
        id))))

(defun state-of (id)
  "The state ID is in, or NIL when the sandbox is gone."
  (multiple-value-bind (body status) (api :get (sandbox-path id))
    (cond ((eql status 404) nil)
          (t (check-status "get" status body)
             (setf *sandbox-state* (nlk:json-value (body-json body) :text "state"))))))

(defun plan-of (id)
  "The plan the sandbox runs on, or NIL."
  (multiple-value-bind (body status) (api :get (sandbox-path id))
    (and (integerp status) (< status 300)
         (nlk:json-value (body-json body) :text "plan"))))

(defun post-action (id action)
  "One lifecycle verb on ID -- start, stop -- answering the service."
  (multiple-value-bind (body status)
      (api :post (concatenate 'string (sandbox-path id) "/" action))
    (check-status action status body)
    t))

(defun wait-for-running (id &key (attempts +boot-attempts+))
  "Wait for ID to be running; a sandbox that failed or vanished answers NIL."
  (loop repeat attempts
        for state = (state-of id)
        do (cond ((equal state "running") (return t))
                 ((null state) (return nil))
                 ((member state '("failed" "deleted") :test #'string=) (return nil))
                 (t (sleep 1)))
        finally (return nil)))

(defun ensure-running ()
  "The id of a sandbox that is running: the one this cell holds, resumed when
it was stopped, or a fresh one when it is gone or failed."
  (let ((id (remembered-id)))
    (when id
      (let ((state (state-of id)))
        (cond ((equal state "running") (return-from ensure-running id))
              ((equal state "stopped") (post-action id "start"))
              ((null state) (forget))
              ((member state '("failed" "deleted") :test #'string=) (forget)))
        (when (and (member id (list (remembered-id)) :test #'equal)
                   (wait-for-running id))
          (return-from ensure-running id))))
    (let ((fresh (create-sandbox)))
      (unless (wait-for-running fresh)
        (fail "deepinfra sandbox: sandbox ~a did not reach running; call deepinfra_sandbox with ~
               status in a moment" fresh))
      fresh)))

(defun stop-remote (id)
  "Stop ID: it keeps /workspace and costs nothing until the next command wakes it."
  (post-action id "stop"))

(defun delete-remote (id)
  "Delete ID and everything in it, /workspace included."
  (multiple-value-bind (body status) (api :delete (sandbox-path id))
    (check-status "delete" status body)
    (forget)
    t))

;;; --- running a command ---------------------------------------------------------

(defun parse-exec-stream (text)
  "TEXT -- the NDJSON answer of an exec -- => (values STDOUT STDERR EXIT ERROR)."
  (let ((out (make-string-output-stream))
        (err (make-string-output-stream))
        (exit nil)
        (problem nil))
    (dolist (line (uiop:split-string (or text "") :separator '(#\Newline)))
      (let ((message (and (plusp (length (nlk:trimmed line)))
                          (ignore-errors (nlk:decode-json line)))))
        (when (hash-table-p message)
          (let ((stdout (nlk:json-value message :text "stdout"))
                (stderr (nlk:json-value message :text "stderr"))
                (code (nlk:json-value message :integer "returncode"))
                (error (nlk:json-value message :text "error")))
            (when stdout (write-string stdout out))
            (when stderr (write-string stderr err))
            (when code (setf exit code))
            (when error (setf problem error))))))
    (values (get-output-stream-string out)
            (get-output-stream-string err)
            exit problem)))

(defun exec-stream (command &key (timeout 60))
  "Run COMMAND as bash -c in the sandbox => (values STDOUT STDERR EXIT ERROR)."
  (let* ((id (ensure-running))
         (timeout (max 1 (min (or timeout 60) +exec-timeout-cap+)))
         (request (nlk:make-json-object
                   "command" (coerce (list "bash" "-c" command) 'vector)
                   "timeout_seconds" timeout)))
    (multiple-value-bind (body status)
        (api :post (concatenate 'string (sandbox-path id) "/exec")
             :content (nlk:encode-json-object request)
             :content-type "application/json"
             :limit (max (setting :timeout-seconds) (+ timeout 30)))
      (check-status "exec" status body)
      (multiple-value-bind (stdout stderr exit problem)
          (parse-exec-stream (cond ((stringp body) body)
                                   ((vectorp body) (nlk:decode-octets body))))
        (values stdout stderr exit
                (or problem (and (null exit) "the answer ended without a return code")))))))

;;; --- files ---------------------------------------------------------------------

(defun content-path (id path)
  "The file call's address for PATH inside sandbox ID."
  (format nil "~a/fs/content?path=~a" (sandbox-path id) (percent-encode path)))

(defun check-workspace (path)
  "Refuse a path outside /workspace, which is the one place a sandbox keeps."
  (unless (and (uiop:string-prefix-p (concatenate 'string +workspace+ "/") path)
               (> (length path) (1+ (length +workspace+))))
    (fail "deepinfra sandbox: ~a is outside /workspace, the only path a sandbox keeps across a ~
           stop; use a path such as /workspace/notes.txt" path))
  path)

(defun read-remote (path)
  "PATH's bytes as text, out of /workspace in the sandbox."
  (check-workspace path)
  (let ((id (ensure-running)))
    (multiple-value-bind (body status headers)
        (api :get (content-path id path) :binary t)
      (cond ((eql status 404)
             (fail "deepinfra sandbox: there is no file ~a in the sandbox; deepinfra_run with ~
'ls /workspace' lists what is there" path))
            (t (check-status "read" status body)
               (cond ((stringp body) body)
                     ((vectorp body) (nlk:decode-octets body (header-value headers "content-type")))
                     (t "")))))))

(defun write-remote (path content)
  "Put CONTENT at PATH in /workspace in the sandbox => the bytes written."
  (check-workspace path)
  (let* ((id (ensure-running))
         (octets (nlk:utf-8-octets (or content ""))))
    (multiple-value-bind (body status)
        (api :put (content-path id path)
             :content octets
             :content-type "application/octet-stream")
      (check-status "write" status body)
      (length octets))))
