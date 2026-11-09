;;;; cell.lisp --- an Upstash Box cloud container: run a command there, read and write its files.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of upstash/box's Hermes terminal backend (see NOTICE). The
;;;; original moves Hermes's whole terminal into a Box; Nodecode's shell is
;;;; its own, so what crosses over here is the Box itself: four tools
;;;; (box_exec, box_read, box_write, box_list) that create, resume and use a
;;;; cloud Linux container over Upstash's REST API, and /box to see, pause or
;;;; delete the one this cell holds.
;;;;
;;;; The wire is the Python SDK's, reimplemented with NLK:HTTP: POST /v2/box
;;;; to create, GET /v2/box/<id> for the state, POST /v2/box/<id>/exec with
;;;; {"command": ["sh", "-c", ...]}, /files/read and /files/write, GET
;;;; /v2/box to list, /pause, /resume and DELETE for the end of it. The key
;;;; rides X-Box-Api-Key and never enters the Box.
;;;;
;;;; Persistent mode is the original's default and this cell's only mode: the
;;;; Box this cell made is written down under the home, resumed (paused Boxes
;;;; woken) when it is next wanted, and paused rather than deleted on exit, so
;;;; files and installed tools are still there next time. /box delete is the
;;;; way to be rid of one.

(defpackage #:nodecode-upstash-box
  (:use #:cl)
  (:export #:upstash-box-error))

(in-package #:nodecode-upstash-box)

(nlk:define-peripheral upstash-box :not-running t :lock "upstash-box")

(defparameter +default-root+ "https://us-east-1.box.upstash.com"
  "Upstash's Box API, where a section naming no base_url goes.")

(defparameter +label+ "nodecode"
  "The label every Box this cell creates carries, and how /box lists them:
what `box list --label nodecode' shows in the console too.")

(defparameter +create-poll-seconds+ 2
  "How long to wait between reads of a Box that is still being created.")

(defparameter +create-ceiling-seconds+ 300
  "How long a Box may take to come up before the creation is a failure.")

(defvar *box* :unread
  "The Box this cell holds: an id string, NIL for none, :UNREAD before the
one written under the home has been read. Reset at start.")

;;; --- the section, read back --------------------------------------------------

(defun api-key ()
  "The key the section names, inline or by environment variable, else UPSTASH_BOX_API_KEY."
  (or (let ((inline (setting :api-key)))
        (and inline (plusp (length inline)) inline))
      (let ((variable (setting :api-key-env)))
        (and variable (plusp (length variable)) (uiop:getenv variable)))
      (uiop:getenv "UPSTASH_BOX_API_KEY")))

(defun root ()
  "The API root this cell talks to, without a trailing slash."
  (let ((given (string-right-trim "/" (or (setting :base-url) ""))))
    (if (plusp (length given)) given +default-root+)))

(defun check-config ()
  "Refuse a section that could never reach the API, naming the member."
  (unless (api-key)
    (error 'nlk:config-refusal
           :detail (concatenate 'string
                                 "upstash-box: a Box API key is needed: set UPSTASH_BOX_API_KEY, or "
                                 "\"api_key\" (or \"api_key_env\") in the upstash-box section; the key "
                                 "is in the Upstash console's Box section"))))

;;; --- the wire -----------------------------------------------------------------

(defun decode-body (answer)
  "ANSWER -- an HTTP body -- as a decoded value; an empty body is an empty object."
  (cond ((stringp answer)
         (if (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) answer)))
             (handler-case (nlk:decode-json answer) (error () (nlk:json-object)))
             (nlk:json-object)))
        ((or (vectorp answer) (hash-table-p answer)) answer)
        (t (nlk:json-object))))

(defun body-error-text (answer)
  "What a failed answer says, for the one line the model reads."
  (let ((body (and (stringp answer) (ignore-errors (nlk:decode-json answer)))))
    (or (nlk:json-value body :string "error")
        (nlk:json-value body :string "message")
        (and (stringp answer) (subseq answer 0 (min 200 (length answer)))))))

(defun json-body (&rest pairs)
  "PAIRS as JSON text, a member with a NIL value left out."
  (nlk:encode-json-object
   (apply #'nlk:make-json-object
          (loop for (key value) on pairs by #'cddr
                when value nconc (list key value)))))

(defun request (method path &key body)
  "METHOD PATH on the configured root, BODY its JSON text => (values TEXT STATUS).
METHOD is a keyword or a name. A failure below HTTP signals UPSTASH-BOX-ERROR; a
status is the caller's to read."
  (let ((method (intern (string-upcase (string method)) :keyword))
        (url (concatenate 'string (root) path))
        (limit (setting :timeout-seconds))
        (answer nil)
        (status nil))
    (handler-case
        (multiple-value-setq (answer status)
          (nlk:http method url
                    :headers (list (cons "content-type" "application/json")
                                   (cons "accept" "application/json")
                                   (cons "x-box-api-key" (or (api-key) "")))
                    :content body
                    :timeout limit
                    :connect-timeout (min 10 limit)))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "~a ~a: ~a" (string-upcase (string method)) path
              (nle:transport-failure-label condition url))))
    (values answer status)))

(defun call (method path &key body)
  "METHOD PATH, its answer decoded; a refused status signals UPSTASH-BOX-ERROR."
  (multiple-value-bind (answer status) (request method path :body body)
    (cond ((< status 300) (decode-body answer))
          ((= status 404) (fail "~a ~a: HTTP 404, nothing of that name on the account"
                                (string-upcase (string method)) path))
          (t (fail "~a ~a: HTTP ~d~@[: ~a~]" (string-upcase (string method)) path status
                   (body-error-text answer))))))

;;; --- the Box this cell holds ---------------------------------------------------

(defun state-path ()
  "Where the id of the Box this cell holds is written, under the home."
  (nlk:home "boxes/upstash-box.id"))

(defun stored-box ()
  "The Box id written under the home, or NIL."
  (let ((path (state-path)))
    (when (probe-file path)
      (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return)
                                (or (ignore-errors (uiop:read-file-string path)) ""))))
        (and (plusp (length text)) text)))))

(defun current-box ()
  "The Box this cell holds, or NIL; the one under the home on first ask."
  (when (eq *box* :unread)
    (setf *box* (stored-box)))
  *box*)

(defun (setf current-box) (id)
  "Hold ID as this cell's Box, and write it down; NIL forgets it."
  (setf *box* id)
  (cond (id (nlk:write-file-atomically (state-path) (format nil "~a~%" id)))
        (t (ignore-errors (delete-file (state-path)))))
  id)

(defun box-path (id &rest tail)
  "The API path of Box ID, TAIL appended."
  (format nil "/v2/box/~a~{~a~}" id tail))

(defun box-status (id)
  "The status of Box ID: a string, :GONE when the API says it is not there."
  (multiple-value-bind (answer status) (request "GET" (box-path id))
    (cond ((= status 404) :gone)
          ((>= status 300) "")
          (t (or (nlk:json-value (decode-body answer) :string "status") "")))))

(defun box-field (box name)
  (nlk:json-value box :string name))

(defun create-box ()
  "Create a Box for this cell, wait for it to come up, hold it and answer it."
  (let* ((body (json-body "name" (format nil "nodecode-~(~{~2,'0x~}~)"
                                         (loop repeat 4 collect (random 256)))
                           "labels" (vector +label+)
                           "size" (setting :size)
                           "runtime" (let ((runtime (setting :runtime)))
                                       (and runtime (plusp (length runtime)) runtime))
                           "keep_alive" (and (setting :keep-alive) t)))
         (box (call :post "/v2/box" :body body))
         (id (box-field box "id")))
    (unless (and id (plusp (length id)))
      (fail "the API answered a Box with no id: ~a" box))
    (let ((opened (get-universal-time)))
      (loop while (equal "creating" (box-field box "status"))
            do (when (> (- (get-universal-time) opened) +create-ceiling-seconds+)
                 (fail "Box ~a was still creating after ~d seconds" id +create-ceiling-seconds+))
               (sleep +create-poll-seconds+)
               (let ((again (ignore-errors (call :get (box-path id)))))
                 (when again (setf box again)))))
    (when (equal "error" (box-field box "status"))
      (fail "the API made Box ~a and put it in the error state" id))
    (setf (current-box) id)
    box))

(defun resume-box (id)
  "Wake a paused Box ID, so a command can run in it."
  (call :post (box-path id "/resume"))
  id)

(defun ensure-box (&optional box-id)
  "The Box a command runs in: BOX-ID, else this cell's, paused woken, a new
one when there is none or the one written down is gone. => its id."
  (let ((id (or box-id (current-box))))
    (cond ((null id) (box-field (create-box) "id"))
          ((equal id (current-box))
           (let ((status (box-status id)))
             (cond ((eq status :gone) (setf (current-box) nil) (box-field (create-box) "id"))
                   ((equal status "paused") (resume-box id))
                   ((equal status "error")
                    (fail "Box ~a is in the error state: /box delete forgets it, /box list shows the account's" id))
                   (t id))))
          (t ;; a Box named by the call, not this cell's: leave it alone
           id))))

(defun recover-box (id)
  "ID after a command found it unusable: resumed when paused, replaced when gone."
  (let ((status (box-status id)))
    (cond ((eq status :gone) (setf (current-box) nil) (box-field (create-box) "id"))
          ((equal status "paused") (resume-box id))
          ((equal status "error") (fail "Box ~a is in the error state" id))
          (t id))))

;;; --- one command ---------------------------------------------------------------

(defun exec-once (id command folder)
  "Run COMMAND in Box ID, in FOLDER or the Box user's home => (values BODY STATUS)."
  (request "POST" (box-path id "/exec")
           :body (json-body "command" (vector "sh" "-c" command)
                            "folder" (let ((folder (and folder (nlk:trimmed folder))))
                                       (and folder (plusp (length folder)) folder)))))

(defun run-command (command &key cwd box-id)
  "Run COMMAND in a Box, creating or resuming it as needed.
=> (values OUTPUT EXIT-CODE BOX-ID GONE), GONE the id of the Box that went
away and was replaced, when the command ran in a new one."
  (let* ((before (current-box))
         (id (ensure-box box-id))
         (gone (and (null box-id) before (not (equal id before)) before)))
    (multiple-value-bind (answer status) (exec-once id command cwd)
      (when (>= status 300)
        ;; The Box paused behind this cell's back, or went away: once.
        (let ((replacement (recover-box id)))
          (unless (equal replacement id)
            (setf gone id))
          (setf id replacement)
          (multiple-value-setq (answer status) (exec-once id command cwd))
          (when (>= status 300)
            (fail "the command could not start in Box ~a: HTTP ~d~@[: ~a~]"
                  id status (body-error-text answer)))))
      (let* ((result (decode-body answer))
             (output (box-field result "output"))
             (exit (or (nlk:json-value result :integer "exit_code") 0)))
        ;; on a failed command the reason is "error"; on a good one stderr is
        ;; noise, so it is read only when the exit code says the command failed.
        (values (cond ((plusp (length output)) output)
                      ((plusp exit) (or (box-field result "error") ""))
                      (t ""))
                exit
                id
                gone)))))

(defun clip-output (text)
  (let ((limit (setting :max-output-chars)))
    (if (<= (length text) limit)
        text
        (format nil "~a~%[~d more characters]" (subseq text 0 limit) (- (length text) limit)))))

(defun command-text (command &key cwd box-id)
  "COMMAND's answer as the model reads it: where it ran, its exit code, its output."
  (multiple-value-bind (output exit id gone) (run-command command :cwd cwd :box-id box-id)
    (concatenate 'string
                 (if gone
                     (format nil "the Box ~a this cell held was gone; a new one, ~a, ran it.~%" gone id)
                     "")
                 (format nil "box ~a, exit ~d~%~a" id exit
                         (if (plusp (length (nlk:trimmed output))) (clip-output output) "(no output)")))))

;;; --- files ------------------------------------------------------------------------

(defun read-file (path &key box-id)
  "PATH's content in the Box, as text."
  (let ((id (ensure-box box-id)))
    (box-field (call :get (format nil "~a?path=~a&encoding=utf-8"
                                  (box-path id "/files/read")
                                  (quri:url-encode path :encoding :utf-8))
                     )
               "content")))

(defun write-file (path content &key box-id)
  "CONTENT written to PATH in the Box => PATH."
  (let ((id (ensure-box box-id)))
    (call :post (box-path id "/files/write")
          :body (json-body "path" path "content" content))
    path))

(defun list-boxes (&key all)
  "The account's Boxes, labelled with this cell's label unless ALL => a list of
(BOX-ID NAME STATUS SIZE), newest first as the API answers them."
  (let ((body (call :get (if all "/v2/box" (format nil "/v2/box?label=~a" +label+)))))
    (loop for box across (coerce (or body #()) 'vector)
          when (hash-table-p box)
            collect (list (box-field box "id")
                          (box-field box "name")
                          (box-field box "status")
                          (box-field box "size")))))

(defun box-lines (boxes)
  "BOXES as the lines the model and /box read."
  (if (null boxes)
      "No Box of this cell's on the account."
      (with-output-to-string (out)
        (format out "~d ~a:" (length boxes) (if (= 1 (length boxes)) "Box" "Boxes"))
        (loop for (id name status size) in boxes
              do (format out "~%  ~a  ~a  ~a~@[  ~a~]" id (or name "?") (or status "?") size)))))

;;; --- the tools ------------------------------------------------------------------

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the `ERROR:' line the model repairs from."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: upstash-box: ~a" condition)))))

(defun argument-text (args name)
  "ARGS' NAME as a trimmed non-empty string, or a refusal naming it."
  (let ((value (gethash name args)))
    (if (and (stringp value) (plusp (length (string-trim '(#\Space) value))))
        (string-trim '(#\Space) value)
        (fail "~a is required" (substitute #\_ #\- name)))))

(defun argument-string (args name)
  "ARGS' NAME as a string, \"\" when absent."
  (let ((value (gethash name args)))
    (if (stringp value) value "")))

(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES -- (NAME TYPE DOC REQUIRED-P) ... -- as a tool schema."
  (list :description description
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (apply #'nlk:make-json-object
                             (loop for (name type doc) in properties
                                   nconc (list name (nlk:json-object "type" type
                                                                     "description" doc))))
         "required" (coerce (loop for property in properties
                                   when (fourth property) collect (first property))
                            'vector))))

(defparameter *exec-schema*
  (tool-schema
   "Run a shell command in a cloud Linux Box (an Upstash Box container), not on this machine. The first call in a session creates the Box and takes a few seconds; later calls run in the same one, with its files and installed tools still there. Commands run through `sh -c`, start in the Box user's home directory, and answer their exit code and output. Use it for anything that should not touch the user's own machine: installing packages, building, running untrusted code, a scratch Linux environment."
   '(("command" "string" "The shell command, as one line, run with sh -c." t)
     ("cwd" "string" "Absolute directory in the Box to start in; default the Box's own user home." nil)
     ("box_id" "string" "Another Box of the account to run in, instead of the one this cell holds." nil)))
  "The box_exec tool: the original's terminal tool, narrowed to one Box.")

(defparameter *read-schema*
  (tool-schema
   "Read a text file from the Box. The path is in the Box's filesystem, not this machine's."
   '(("path" "string" "Absolute path in the Box." t)
     ("box_id" "string" "Another Box of the account, instead of the one this cell holds." nil)))
  "The box_read tool.")

(defparameter *write-schema*
  (tool-schema
   "Write a text file in the Box, replacing it whole, creating the directories above it if the Box needs to. The path is in the Box's filesystem, not this machine's."
   '(("path" "string" "Absolute path in the Box." t)
     ("content" "string" "The whole file content." t)
     ("box_id" "string" "Another Box of the account, instead of the one this cell holds." nil)))
  "The box_write tool.")

(defparameter *list-schema*
  (tool-schema
   "List the Boxes this cell may use: the ones its label marks, with id, name and state (running, paused, ...). A paused Box is woken by the next command in it; /box delete drops the one this cell holds."
   '(("all" "boolean" "List every Box on the account, not only this cell's." nil)))
  "The box_list tool.")

(defun tool-exec (args)
  (answering
    (command-text (argument-text args "command")
                  :cwd (argument-string args "cwd")
                  :box-id (let ((id (argument-string args "box_id")))
                            (and (plusp (length id)) id)))))

(defun tool-read (args)
  (answering
    (read-file (argument-text args "path")
               :box-id (let ((id (argument-string args "box_id")))
                         (and (plusp (length id)) id)))))

(defun tool-write (args)
  (answering
    (let ((path (argument-text args "path"))
          (content (gethash "content" args)))
      (unless (stringp content) (fail "content is required"))
      (write-file path content
                  :box-id (let ((id (argument-string args "box_id")))
                            (and (plusp (length id)) id)))
      (format nil "Wrote ~d character~:p to ~a in the Box." (length content) path))))

(defun tool-list (args)
  (answering
    (box-lines (list-boxes :all (gethash "all" args)))))

;;; --- the manual and /box ---------------------------------------------------------

(defparameter +primer+
  "Upstash Box is a cloud Linux container the tools below run in, so a build, an install or an
untrusted program never touches this machine. (help :upstash-box) is this page.
  box_exec {command, cwd?, box_id?}   run a shell command in the Box, with its exit code and output
  box_read {path, box_id?}            read a text file in the Box
  box_write {path, content, box_id?}  write a text file in the Box, whole
  box_list {all?}                      the Boxes this account may use, with their state
The first command creates the Box and takes a few seconds; every later one runs in the same
Box, paused between sessions and woken when next needed, so installed tools and files stay.
Files do not cross over: a file this machine has is not in the Box until box_write puts it there.
The upstash-box section of config.jsonc holds the API key, the Box size and runtime (the base
image), and keep_alive: a keep-alive Box is never paused, and stays paid for while it runs.
/box shows the Box this session holds, /box list the account's, /box pause and /box delete end
the one held (delete destroys its files)."
  "What (help :upstash-box) answers while the cell runs.")

(defun held-box-line ()
  "The Box this cell holds, as one line about it."
  (let ((id (current-box)))
    (if (null id)
        "No Box yet: the first box_exec creates one."
        (let ((status (box-status id)))
          (format nil "Box ~a: ~a~[~;~:; (the API no longer has it: the next box_exec makes a new one; \
/box delete forgets it)~]"
                  id (if (eq status :gone) "gone" (if (plusp (length status)) status "unknown"))
                  (if (eq status :gone) 1 0))))))

(defun run-command-slash (args session-id)
  "One /box invocation: nothing shows the Box this cell holds, `list' the
account's, `pause' and `delete' end the one it holds."
  (declare (ignore session-id))
  (let ((words (uiop:split-string (nlk:trimmed (or args "")) :separator '(#\Space #\Tab))))
    (cond ((null words)
           (format nil "~a~%Upstash Box at ~a. /box list shows the account's Boxes; /box pause stops the one held; /box delete destroys it."
                   (held-box-line) (root)))
          ((string-equal (first words) "list")
           (box-lines (list-boxes :all (second words))))
          ((string-equal (first words) "pause")
           (let ((id (current-box)))
             (if (null id)
                 "No Box to pause."
                 (progn (call :post (box-path id "/pause"))
                        (format nil "Paused Box ~a: it keeps its files and is woken by the next box_exec." id)))))
          ((member (first words) '("delete" "stop") :test #'string-equal)
           (let ((id (current-box)))
             (if (null id)
                 "No Box to delete."
                 (progn (call :delete (box-path id))
                        (setf (current-box) nil)
                        (format nil "Deleted Box ~a and its files." id)))))
          (t (format nil "Unknown /box argument ~s: list, pause or delete." (first words))))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell upstash-box
  (:section ("upstash-box")
    (:guide "a Box API key is required: set UPSTASH_BOX_API_KEY, or api_key here; size and runtime pick the container (runtime python gives a Box with pip); next step: /box shows the Box this session holds")
    ("api_key" :secret
     :doc "the Upstash Box API key, from the console's Box section")
    ("api_key_env" :env
     :doc "the environment variable holding the key, when it is not written here (UPSTASH_BOX_API_KEY is read either way)")
    ("base_url" :string
     :doc "the API address: https://us-east-1.box.upstash.com unless answered")
    ("size" :choice :options '("small" "medium" "large") :default "small"
     :doc "the Box's size; Boxes come in fixed sizes")
    ("runtime" :string
     :doc "the Box base image: node, python, golang, ruby, rust, ...; unset for the Box default")
    ("keep_alive" :boolean :default nil
     :doc "create a Box that is never paused: it keeps running, and billing, between sessions")
    ("max_output_chars" :integer :default 20000 :min 200
     :doc "how much of one command's output the model is shown")
    ("timeout_seconds" :integer :default 120 :min 5
     :doc "one request's ceiling, the command's own time included"))
  (:help :upstash-box
         "upstash-box: box_exec runs a shell command in a cloud Upstash Box, box_read, box_write and box_list its files and Boxes"
         +primer+)
  (:tool "box_exec" *exec-schema* 'tool-exec)
  (:tool "box_read" *read-schema* 'tool-read)
  (:tool "box_write" *write-schema* 'tool-write)
  (:tool "box_list" *list-schema* 'tool-list)
  (:command "box" 'run-command-slash
            :description "The Upstash Box this session holds: show it, list the account's, pause or delete it"
            :argument-hint "list | pause | delete")
  (:start (lambda ()
            (check-config)
            (setf *box* :unread)
            (nle:on-stop
             (lambda ()
               ;; paused, not deleted: the original's persistent mode, and the
               ;; reason the next session finds its files where it left them.
               (let ((id (current-box)))
                 (when id
                   (ignore-errors
                     (let ((status (box-status id)))
                       (cond ((eq status :gone) (setf (current-box) nil))
                             ((equal status "paused") nil)
                             (t (call :post (box-path id "/pause"))))))))
               (setf *box* :unread))))))
