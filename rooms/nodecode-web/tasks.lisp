;;;; tasks.lisp --- the Board's To do list, kept in the home.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The page's Board (page/board.js) lists tasks: a prompt, the folder it
;;;; runs in, whether it gets a worktree of its own, and once started the
;;;; session it became. The list is one JSON file in the home, so every window
;;;; and tab of this organism shows the same board and a restart keeps it.
;;;; GET /api/web/tasks answers {version, tasks}; PUT sends the whole list
;;;; back with the version it was read at, and a list read at an older
;;;; version is refused (409, the current one in the answer): two windows
;;;; never overwrite each other's edits, the later one merges and sends again.
;;;; The route takes the operator token like every /api route.

(in-package #:nodecode-web)

(defconstant +tasks-max+ 500 "The most tasks a board keeps.")
(defconstant +task-prompt-max+ 20000 "The longest prompt one task carries, in characters.")
(defconstant +tasks-body-max+ (* 4 1024 1024) "The largest PUT body read, in bytes.")

(defvar *tasks-lock* (sb-thread:make-mutex :name "web tasks"))

(defun tasks-path ()
  "The board's file in the home."
  (nlk:home "web/tasks.json"))

(defun read-tasks (&aux (path (tasks-path)))
  "The board as kept => (values TASKS VERSION): a vector of task objects and
the integer every PUT must name; an empty board at version 0 when there is
no file, or one that is not the board's JSON."
  (let ((kept (and (probe-file path)
                   (ignore-errors (nlk:decode-json (uiop:read-file-string path))))))
    (values (or (nlk:json-value kept :array "tasks") #())
            (or (nlk:json-value kept :integer "version") 0))))

(defun text-field (object key limit &key (required nil))
  "OBJECT's string KEY when it is one of at most LIMIT characters; :BAD when
it is not a string, or REQUIRED and absent or empty."
  (multiple-value-bind (value present) (gethash key object)
    (cond ((or (not present) (eq value :null))
           (if required :bad nil))
          ((not (stringp value)) :bad)
          ((> (length value) limit) :bad)
          ((and required (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) value)))) :bad)
          (t value))))

(defun clean-task (object &aux why)
  "OBJECT as the board keeps a task => (values TASK NIL), or (values NIL WHY):
only the fields a task has, each of its type, so the file never carries what
the page did not mean to keep."
  (unless (hash-table-p object)
    (return-from clean-task (values nil "a task is an object")))
  (flet ((field (key limit &rest options)
           (let ((value (apply #'text-field object key limit options)))
             (when (eq value :bad)
               (setf why (or why (format nil "~a is not a string of at most ~:d characters~:[~; (and is required)~]"
                                         key limit (getf options :required)))))
             (and (stringp value) value))))
    (let ((task (nlk:json-object
                 "id" (field "id" 64 :required t)
                 "prompt" (field "prompt" +task-prompt-max+ :required t)
                 "folder" (or (field "folder" 4096) "")
                 "worktree" (eq (gethash "worktree" object) t)
                 "created_at" (or (field "created_at" 64) "")
                 :opt "session_id" (field "session_id" 128)
                 :opt "started_at" (field "started_at" 64)
                 :opt "branch" (field "branch" 256))))
      (if why (values nil why) (values task nil)))))

(defun json-answer (status object &aux (body (nlk:encode-json-octets object)))
  "OBJECT as JSON with STATUS."
  (list status (list :content-type "application/json" :content-length (length body)) body))

(defun tasks-refused (status code message &rest more)
  "A refusal the page shows: {error: {code, message}}, MORE beside it."
  (let ((object (nlk:json-object "error" (nlk:json-object "code" code "message" message))))
    (loop for (key value) on more by #'cddr do (setf (gethash key object) value))
    (json-answer status object)))

(defun request-octets (env limit &aux (body (getf env :raw-body))
                                      (declared (getf env :content-length)))
  "ENV's body as octets, or NIL when it declares or runs past LIMIT bytes. The
body is read to its end either way, so the connection is left clean."
  (let ((buffer (make-array 65536 :element-type '(unsigned-byte 8)))
        (kept (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (over (and declared (> declared limit))))
    (when body
      (loop with left = (or declared most-positive-fixnum)
            while (plusp left)
            do (let ((got (read-sequence buffer body :end (min left (length buffer)))))
                 (when (zerop got) (return))
                 (decf left got)
                 (unless over
                   (if (> (+ (fill-pointer kept) got) limit)
                       (setf over t)
                       (loop for index below got
                             do (vector-push-extend (aref buffer index) kept (length buffer))))))))
    (and (not over) (coerce kept '(simple-array (unsigned-byte 8) (*))))))

(defun board-json (tasks version)
  (nlk:json-object "version" version "tasks" tasks))

(defun put-tasks (octets)
  "OCTETS, a PUT's body {version, tasks}, kept when VERSION is the board's
own => the answer the route sends."
  (let* ((sent (handler-case (nlk:decode-json octets :whole t) (error () :bad)))
         (version (nlk:json-value sent :integer "version"))
         (list (nlk:json-value sent :array "tasks")))
    (cond ((or (eq sent :bad) (not (hash-table-p sent)))
           (tasks-refused 400 "not_json" "the body is not a JSON object"))
          ((not version) (tasks-refused 400 "no_version" "version: the board's version this list was read at"))
          ((not list) (tasks-refused 400 "no_tasks" "tasks: the whole list, as an array"))
          ((> (length list) +tasks-max+)
           (tasks-refused 413 "too_many" (format nil "a board keeps at most ~d tasks" +tasks-max+)))
          (t
           (let ((clean '()) (ids (make-hash-table :test 'equal)))
             (loop for each across list
                   do (multiple-value-bind (task why) (clean-task each)
                        (unless task (return-from put-tasks (tasks-refused 400 "bad_task" why)))
                        (when (gethash (gethash "id" task) ids)
                          (return-from put-tasks
                            (tasks-refused 400 "bad_task" (format nil "two tasks are ~a" (gethash "id" task)))))
                        (setf (gethash (gethash "id" task) ids) t)
                        (push task clean)))
             (sb-thread:with-mutex (*tasks-lock*)
               (multiple-value-bind (tasks current) (read-tasks)
                 (if (/= version current)
                     (tasks-refused 409 "moved" "the board changed since this list was read"
                                    "version" current "tasks" tasks)
                     (let ((kept (coerce (nreverse clean) 'simple-vector)))
                       (nlk:write-file-atomically (tasks-path)
                                                  (nlk:encode-json-object (board-json kept (1+ current))))
                       (board-json kept (1+ current)))))))))))

(defun tasks-route (env)
  "/api/web/tasks: GET the board {version, tasks}; PUT {version, tasks} keeps
the whole list when VERSION is the board's own, 409 with the current one when
another window moved it first."
  (case (getf env :request-method)
    ((:get :head)
     (sb-thread:with-mutex (*tasks-lock*)
       (multiple-value-bind (tasks version) (read-tasks)
         (board-json tasks version))))
    (:put
     (let ((octets (request-octets env +tasks-body-max+)))
       (if octets
           (put-tasks octets)
           (tasks-refused 413 "too_large" (format nil "over the ~:d-byte ceiling" +tasks-body-max+)))))
    (t (tasks-refused 405 "method" "GET reads the board; PUT sends the whole list back"))))
