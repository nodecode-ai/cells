;;;; support.lisp --- plan-canvas test runner and shared fixtures.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Plan-canvas tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a PLAN-CANVAS-CELL-
;;;; name prefix; RUN-PLAN-CANVAS-TESTS runs exactly that slice.
;;;;
;;;; Nothing is dialed and nothing is stubbed: a canvas is a file in a scratch
;;;; folder, and the work the hooks read is a durable fact built by hand, in the
;;;; shape the engine publishes one.

(in-package #:nodecode.test)

(define-test-slice "plan-canvas" "PLAN-CANVAS-CELL-" :start nodecode-plan-canvas:start-cell)

(defvar *folders* 0
  "How many scratch canvas folders this image made, so no two tests share one.")

(defun canvas-folder ()
  "A fresh folder for one test's canvas files."
  (merge-pathnames (format nil "plan-canvas-~d-~d/" (get-universal-time) (incf *folders*))
                   (uiop:temporary-directory)))

(defmacro with-canvas (&body body)
  "BODY with the cell started on its own scratch canvas folder."
  `(let ((folder (namestring (canvas-folder))))
     (with-cell-stop ((plan-canvas-start "folder" folder))
       ,@body)))

(defmacro as-chat ((session) &body body)
  "BODY as SESSION's evaluation, so the tools find the chat they belong to."
  `(let ((nlk:*scribe-session-id* ,session))
     ,@body))

(defun args (&rest pairs)
  "A decoded tool-arguments object from alternating key/value PAIRS."
  (apply #'nlk:make-json-object pairs))

(defun answer-of (text)
  "TEXT, a tool's answer, decoded."
  (cell-json text))

(defun error-of (text)
  "The refusal TEXT, one tool's answer, carries."
  (gethash "error" (cell-json text)))

(defun box-args (id label &key files check after options kind)
  "One plan box as canvas_draw is given it."
  (apply #'nlk:make-json-object
         (append (list "id" id "label" label)
                 (when kind (list "kind" kind))
                 (when files (list "files" (coerce files 'vector)))
                 (when check (list "check" check))
                 (when after (list "after" (coerce after 'vector)))
                 (when options (list "options" (coerce options 'vector))))))

(defun draw-plan (&key (title "Ship the payments API") (root "/work/project") boxes)
  "canvas_draw's answer for BOXES."
  (plan-canvas:tool-draw
   (args "title" title "root" root "boxes" (coerce (or boxes '()) 'vector))))

(defun view-of (session)
  "SESSION's canvas as canvas_get answers it."
  (cell-json (as-chat (session) (plan-canvas:tool-get (args)))))

(defun boxes-of (session)
  "SESSION's boxes, as canvas_get lists them."
  (coerce (gethash "boxes" (view-of session)) 'list))

(defun box-of (session id)
  "SESSION's box ID as canvas_get lists it."
  (find id (boxes-of session) :key (lambda (box) (gethash "id" box)) :test #'string=))

(defun update-plan (session &rest ops)
  "canvas_update over OPS, as SESSION's evaluation."
  (as-chat (session)
    (cell-json (plan-canvas:tool-update
                 (args "ops" (coerce ops 'vector))))))

(defun result-paths (paths &key (session "s1"))
  "A turn.tool_result :frame op whose call wrote PATHS."
  (cell-frame-op "turn.tool_result"
                 :session session
                 :payload
                 (nlk:json-object
                  "text" "wrote it"
                  "metadata"
                  (nlk:json-object
                   "workspace_diff"
                   (nlk:json-object
                    "files" (coerce (mapcar (lambda (path) (nlk:json-object "path" path))
                                             paths)
                                    'vector))))))

(defun check-result (command exit &key (session "s1"))
  "A turn.tool_result :frame op whose call ran COMMAND and exited EXIT."
  (cell-frame-op "turn.tool_result"
                 :session session
                 :payload
                 (nlk:json-object
                  "text" (format nil "test output~@[~&[exit ~d]~]" (unless (zerop exit) exit))
                  "metadata"
                  (nlk:json-object
                   "calls" (vector (nlk:json-object "verb" "sh"
                                                     "source" command
                                                     "exit" exit))))))

(defun work-seen (op)
  "Run OP through the cell's :frame observer, as the live stream would."
  (cell-publish "nodecode-plan-canvas" op))
