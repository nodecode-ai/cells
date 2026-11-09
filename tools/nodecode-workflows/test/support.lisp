;;;; support.lisp --- workflow test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Workflow tests register into the SAME nodecode.test registry under a
;;;; WORKFLOWS-CELL- name prefix; RUN-WORKFLOWS-TESTS runs exactly that slice.
;;;;
;;;; A node's turn is the one thing stubbed: LAUNCH-FUNCTION records the
;;;; prompt it was given and answers the session and turn it would have made,
;;;; and POLL-FUNCTION answers what that turn came to. Everything else -- the
;;;; graph parse, the readiness, the fan-out expansion, the disk, the settle
;;;; -- is the cell's own code.

(in-package #:nodecode.test)

(define-test-slice "workflows" "WORKFLOWS-CELL-" :start nodecode-workflows:start-cell)

(defvar *launched* '()
  "The (RUN NODE PROMPT) the launch seam was handed, newest first.")

(defvar *say* (lambda (id) (format nil "done: ~a" id))
  "A node id to the text its turn answers, or :RUNNING to leave it in flight.")

(defun fake-launch (run node)
  (push (list (nodecode-workflows::wrun-id run) (nodecode-workflows::node-id node)
              (nodecode-workflows::prompt-for run node))
        *launched*)
  (values (format nil "test/~a" (nodecode-workflows::node-id node))
          (format nil "turn-~a" (nodecode-workflows::node-id node))))

(defun fake-poll (run node)
  (declare (ignore run))
  (let ((said (funcall *say* (nodecode-workflows::node-id node))))
    (if (eq said :running) (values :running) (values :done said))))

(defmacro with-runner ((&key (say nil say-p)) &body body)
  "BODY with the two turn seams stubbed and *LAUNCHED* empty."
  `(let ((*launched* '())
         (nodecode-workflows::*launch-function* #'fake-launch)
         (nodecode-workflows::*poll-function* #'fake-poll)
         ,@(when say-p `((*say* ,say))))
     ,@body))

(defmacro with-cell ((&rest config) &body body)
  "BODY with the cell started on CONFIG (the defaults when none) and stopped on unwind."
  `(with-cell-stop ((workflows-start ,@config))
     ;; the worker is the cell's; a test drives ADVANCE itself, so the timer
     ;; thread is stopped and nothing races the assertions
     (setf nodecode-workflows::*worker*
           (nlk:worker-stop nodecode-workflows::*worker*))
     ,@body))

(defmacro signals-p (condition &body body)
  "Whether BODY signals CONDITION."
  `(handler-case (progn ,@body nil) (,condition () t)))

(defun launched-ids (&optional (run nil))
  "The node ids launched, oldest first; of one run when RUN is given."
  (loop for (id node prompt) in (reverse *launched*)
        when (or (null run) (string= id run))
          collect node))

(defun launched-prompt (node)
  "The prompt the launch seam was handed for NODE, newest."
  (let ((entry (find node *launched* :key #'second :test #'string=)))
    (and entry (third entry))))

(defun run-of (id)
  (nodecode-workflows::find-run id))

(defun drive (id)
  "One turn of the engine over run ID, as the worker would make it."
  (nodecode-workflows::advance (run-of id)))

(defun state-of (id node)
  (nodecode-workflows::node-state (run-of id) node))

(defun status-of (id)
  (nodecode-workflows::wrun-status (run-of id)))

(defun wf-node (id type text &key after inputs from items items-from schema)
  "One node of a test graph: TYPE decides which field TEXT fills."
  (apply #'nlk:make-json-object
         (append (list "id" id "type" type)
                 (cond ((string= type "gate") (list "question" text))
                       ((string= type "echo") (list "echo" text))
                       (t (list "goal" text)))
                 (when after (list "after" (coerce after 'vector)))
                 (when inputs (list "inputs" (coerce inputs 'vector)))
                 (when from (list "from" from))
                 (when schema (list "schema" schema))
                 (when items (list "fanout" (nlk:make-json-object "items" (coerce items 'vector))))
                 (when items-from
                   (list "fanout" (nlk:make-json-object "items_from" items-from))))))

(defun wf-graph (name &rest nodes)
  "A graph NAMED NAME over NODES, as a decoded JSON object."
  (nlk:make-json-object "name" name "nodes" (coerce nodes 'vector)))
