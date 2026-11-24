;;;; cell-test.lisp --- the Duet queue, offline.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here reaches a network or another machine: the CLI that runs a task
;;;; is bound to a scripted answer, so "the task ran in the background", "a task
;;;; that ends by asking waits again" and "the model cannot approve its own
;;;; work" are facts a test reads rather than a CLI's mood. One test runs the
;;;; real runner with `echo', which needs nothing but a shell.

(in-package #:nodecode.test)

(define-test-slice "duet-queue" "DUET-QUEUE-CELL-" :start nodecode-duet-queue:start-cell)

;;; --- helpers ------------------------------------------------------------------

(defun duet-tool (name json)
  "The tool NAME called the way the engine calls it, on the JSON a model sent:
its contract decoded first, then the body."
  (let ((entry (nle::find-tool name))
        (decoder (if (string= name "duet_swarm")
                     #'nodecode-duet-queue::decode-duet-swarm-call
                     #'nodecode-duet-queue::decode-duet-queue-call)))
    (unless entry (error "the ~a tool is not registered" name))
    (funcall (third entry) (funcall decoder (nlk:decode-json json)))))

(defmacro with-duet ((&rest pairs) &body body)
  "BODY inside a started queue on PAIRS, over an empty one, stopped on unwind.
The worker's wake is an hour, so no thread takes a task mid-test."
  `(with-cell-stop ((duet-queue-start "command" "echo" "interval_seconds" 3600 ,@pairs))
     (nodecode-duet-queue::reset-queue)
     ,@body))

(defun duet-task (id)
  (nodecode-duet-queue::find-task id))

(defun duet-status (id)
  (nodecode-duet-queue::task-status (duet-task id)))

(defmacro with-scripted-cli ((&rest answers) &body body)
  "BODY with the cell's runner answering each ANSWER in turn, an ANSWER being
(OUTPUT STATUS); a call past the last answer is an empty, successful run."
  `(let* ((answers (list ,@(mapcar (lambda (answer) `(list ,@answer)) answers)))
          (nodecode-duet-queue::*run-command*
           (lambda (argv &key directory timeout)
             (declare (ignore argv directory timeout))
             (let ((answer (pop answers)))
               (if answer
                   (values (first answer) (second answer))
                   (values "" 0))))))
     ,@body))

;;; --- the lifecycle every cell owes ---------------------------------------------

(define-cell-lifecycle-tests "duet-queue"
  (:config "command" "echo" "interval_seconds" 3600)
  (:help :duet-queue)
  (:command "duet")
  (:refused () ("command" 42) ("max_tasks" 0)))
;; () is the missing command the section cannot do without; the other two are a
;; member of the wrong shape and one under its minimum.

;;; --- the gate ------------------------------------------------------------------

(deftest duet-queue-cell-a-model-task-waits-for-the-operator (with-duet ()
  (let ((answer (duet-tool "duet_queue"
                            "{\"action\": \"add\", \"title\": \"summarise the release notes\"}")))
    (is (search "awaiting_approval" answer) "a model's task is queued, not run")
    (is (search "/duet approve 1" answer) "and the answer says who unblocks it"))
  (is (equal "awaiting_approval" (duet-status 1)))
  (is (null (nodecode-duet-queue::run-one)) "the worker leaves it alone")))

(deftest duet-queue-cell-the-operator-approves-and-the-worker-runs-it (with-duet ()
  (duet-tool "duet_queue" "{\"action\": \"add\", \"title\": \"summarise the release notes\"}")
  (is (search "approved" (cell-entry "nodecode-duet-queue" "duet" "approve 1")) "/duet approve makes it runnable")
  (with-scripted-cli (("the notes, summarised" 0))
    (is (eql 1 (nodecode-duet-queue::run-one)) "the worker takes the approved task"))
  (is (equal "done" (duet-status 1)))
  (is (equal "the notes, summarised" (nodecode-duet-queue::task-result (duet-task 1))))))

(deftest duet-queue-cell-a-task-that-ends-by-asking-waits-again (with-duet ()
  (duet-tool "duet_queue" "{\"action\": \"add\", \"title\": \"draft the invoice\"}")
  (cell-entry "nodecode-duet-queue" "duet" "approve 1")
  (with-scripted-cli (("I drafted it. [APPROVAL_REQUIRED: publish the invoice]" 0))
    (nodecode-duet-queue::run-one))
  (is (equal "awaiting_approval" (duet-status 1)) "the task waits on the next decision")
  (is (equal "publish the invoice" (gethash "approval_action" (duet-task 1))))
  (is (search "I drafted it" (nodecode-duet-queue::task-result (duet-task 1))))
  (is (search "publish the invoice" (nodecode-duet-queue::show-verb 1)))))

(deftest duet-queue-cell-a-failing-cli-is-a-failed-task (with-duet ()
  (cell-entry "nodecode-duet-queue" "duet" "add reindex the archive")
  (with-scripted-cli (("no such index" 3))
    (nodecode-duet-queue::run-one))
  (is (equal "failed" (duet-status 1)))
  (is (search "no such index" (nodecode-duet-queue::task-result (duet-task 1))))
  (is (search "pending" (cell-entry "nodecode-duet-queue" "duet" "retry 1")) "retry puts it back")))

(deftest duet-queue-cell-the-model-cannot-approve-its-own-task (with-duet ()
  (duet-tool "duet_queue" "{\"action\": \"add\", \"title\": \"delete the old builds\"}")
  (let ((answer (duet-tool "duet_queue" "{\"action\": \"approve\", \"task_id\": 1}")))
    (is (search "operator" answer) "the answer names whose verb it is")
    (is (search "/duet approve 1" answer)))
  (is (equal "awaiting_approval" (duet-status 1)) "and nothing moved")))

(deftest duet-queue-cell-add-needs-a-task (with-duet ()
  (is (signals-error nodecode-duet-queue:duet-queue-error
        (duet-tool "duet_queue" "{\"action\": \"add\"}")))
  (is (null (duet-task 1)) "nothing was queued")))

;;; --- the store, and the CLI it runs ----------------------------------------------

(deftest duet-queue-cell-the-queue-outlives-a-restart ()
  (with-cell-stop ((duet-queue-start "command" "echo" "interval_seconds" 3600))
    (nodecode-duet-queue::reset-queue)
    (duet-tool "duet_queue" "{\"action\": \"add\", \"title\": \"survives a restart\"}"))
  (with-cell-stop ((duet-queue-start "command" "echo" "interval_seconds" 3600))
    (let ((summary (duet-tool "duet_queue" "{\"action\": \"list\"}")))
      (is (search "survives a restart" summary) "the file is the queue")
      (is (search "awaiting_approval" summary)))
    (is (eql 2 (nodecode-duet-queue::add-task "the next one")) "ids carry on")))

(deftest duet-queue-cell-the-real-runner-runs-the-command (with-duet ()
  (let* ((real nodecode-duet-queue::*run-command*)
         (seen nil)
         (nodecode-duet-queue::*run-command*
          (lambda (argv &key directory timeout)
            (setf seen argv)
            (funcall real argv :directory directory :timeout timeout))))
    (cell-entry "nodecode-duet-queue" "duet" "add say hi")
    (is (eql 1 (nodecode-duet-queue::run-one)) "the real runner executes echo")
    (is (equal "done" (duet-status 1)))
    (is (search "say hi" (car (last seen))) "the instruction is in the CLI's last argument"))))

(deftest duet-queue-cell-the-swarm-tool-reads-the-cli (with-duet ()
  (let* ((seen nil)
         (nodecode-duet-queue::*run-command*
          (lambda (argv &key directory timeout)
            (declare (ignore directory timeout))
            (setf seen argv)
            (values "3 agents idle" 0))))
    (is (equal "3 agents idle" (duet-tool "duet_swarm" "{\"action\": \"status\"}")))
    (is (equal '("ruflo" "swarm" "status") seen) "the Ruflo CLI, as the original called it"))))
