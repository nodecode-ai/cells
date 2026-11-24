;;;; cell-test.lisp --- the muster cell, offline.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No fork is started and nothing is submitted: a fork progress frame is a
;;;; literal, the sweeper is called by hand, and the ingress that would start
;;;; a re-run is stubbed. What "a quiet row is stuck" and "a failed job is
;;;; run again once" mean is then a fact a test reads, not a thread it waits
;;;; for.

(in-package #:nodecode.test)

(defun muster-test-directory ()
  "The folder the test ledger lives in, under the test home."
  (uiop:ensure-directory-pathname (merge-pathnames "muster-cell-test/" (nlk:home "test/"))))

(define-test-slice "muster" "MUSTER-CELL-"
  :start nodecode-muster:start-cell
  :defaults (("dir" (namestring (muster-test-directory)))))

(define-cell-lifecycle-tests "muster"
  (:hooks :frame)
  (:help :muster)
  (:command "muster")
  (:refused ("stall_seconds" 5)))

;;; --- a frame becomes a row ---------------------------------------------------

(defun muster-frame (session &rest forks)
  "A session_fork_progress :frame op carrying FORKS, each a key/value plist."
  (list :session-id session
        :kind "session_fork_progress"
        :payload (apply #'nlk:make-json-object
                        (list "session_id" session
                              "tool_call_id" "call-1"
                              "forks" (coerce (mapcar (lambda (fork)
                                                         (apply #'nlk:make-json-object fork))
                                                       forks)
                                              'vector)))))

(defun muster-row (id label status &rest more)
  (list* "session_id" id "label" label "status" status "calls" 2 more))

(defun muster-flight (id)
  (gethash id nodecode-muster::*flights*))

(defun muster-age (id seconds)
  "Put the row ID this many seconds into the past."
  (let ((flight (muster-flight id)))
    (setf (getf flight :last-seen) (- (get-universal-time) seconds))
    flight))

(deftest muster-cell-a-fork-frame-becomes-a-row ()
  (with-cell-stop ((muster-start "show_goals" t))
    (cell-publish "nodecode-muster"
                  (muster-frame "parent-1"
                                (muster-row "child-1" "audit the retry loop" "running")
                                (muster-row "child-2" "index the corpus" "completed")))
    (nodecode-muster::tick)
    (let ((board (nodecode-muster::table)))
      (is (search "audit the retry loop" board) "the job is on the board")
      (is (search "working" board) "a running fork reads working")
      (is (search "index the corpus" board) "so is the one that finished")
      (is (search "done" board) "and it reads done")
      (is (equal "SUCCEEDED" (getf (muster-flight "child-2") :state)) "the row keeps the state"))))

(deftest muster-cell-a-quiet-row-is-stuck-and-heals ()
  (with-cell-stop ((muster-start "stall_seconds" 30))
    (cell-publish "nodecode-muster" (muster-frame "parent-1" (muster-row "child-1" "index the corpus" "running")))
    (nodecode-muster::tick)
    (is (search "working" (nodecode-muster::table)) "a fresh row is working")
    (muster-age "child-1" 600)
    (nodecode-muster::sweep (get-universal-time))
    (is (equal "STALLED" (getf (muster-flight "child-1") :state)) "a silent row is stuck")
    (is (search "stuck" (nodecode-muster::table)) "and the board says so")
    (is (search "/muster stop" (nodecode-muster::table)) "with the command that answers it")
    (cell-publish "nodecode-muster" (muster-frame "parent-1" (muster-row "child-1" "index the corpus" "running")))
    (nodecode-muster::tick)
    (is (equal "RUNNING" (getf (muster-flight "child-1") :state)) "the next frame unsticks it")))

;;; --- a bad ending is run again, as often as the section says ------------------

(deftest muster-cell-a-failed-job-is-run-again-once ()
  (let ((nodecode-muster::*workers* nil))
   (with-cell-stop ((muster-start "auto_requeue" t "max_requeues" 1))
    (let ((sent '()))
      (with-stubbed-fdefinition (nodecode-muster::resubmit (session text)
                                  (push (list session text) sent))
        (cell-publish "nodecode-muster" (muster-frame "parent-1" (muster-row "child-1" "index the corpus" "failed")))
        (nodecode-muster::tick)
        (is (= 1 (length sent)) "one re-run for one bad ending")
        (is (equal "child-1" (first (first sent))) "in the flight's own session")
        (is (search "index the corpus" (second (first sent))) "with the job it was given")
        (is (= 1 (getf (muster-flight "child-1") :requeues)) "the attempt is counted")
        (is (equal "RUNNING" (getf (muster-flight "child-1") :state)) "and the row waits on the re-run")
        (nodecode-muster::tick)
        (is (= 1 (length sent)) "the same ending is never run again twice"))))))

(deftest muster-cell-a-failed-job-past-its-limit-is-left-alone ()
  (let ((nodecode-muster::*workers* nil))
   (with-cell-stop ((muster-start "auto_requeue" t "max_requeues" 0))
    (let ((sent '()))
      (with-stubbed-fdefinition (nodecode-muster::resubmit (session text)
                                  (push (list session text) sent))
        (cell-publish "nodecode-muster" (muster-frame "parent-1" (muster-row "child-1" "index the corpus" "failed")))
        (nodecode-muster::tick)
        (is (null sent) "max_requeues 0 means watch only")
        (is (equal "FAILED" (getf (muster-flight "child-1") :state)) "the row keeps its state"))))))

;;; --- the commands and the digest ---------------------------------------------

(deftest muster-cell-the-command-answers-each-verb ()
  (with-cell-stop ((muster-start))
    (cell-publish "nodecode-muster" (muster-frame "parent-1" (muster-row "child-1" "audit the retry loop" "running")))
    (nodecode-muster::tick)
    (let ((board (cell-entry "nodecode-muster" "muster" "" "muster-cell-cmd")))
      (is (search "audit the retry loop" board) "/muster alone is the board")
      (is (search "child-1" board) "with the id to act on"))
    (let ((help (cell-entry "nodecode-muster" "muster" "help" "muster-cell-cmd")))
      (is (search "/muster stop" help) "/muster help lists the verbs"))
    (let ((unknown (cell-entry "nodecode-muster" "muster" "stop nothing-here" "muster-cell-cmd")))
      (is (search "no worker called" unknown) "an unknown id says so"))))

(deftest muster-cell-a-stop-asks-the-kernel-and-says-so ()
  (with-cell-stop ((muster-start))
    (let ((asked '()))
      (with-stubbed-fdefinition (nlk:request-cancel-turn (session turn &optional reason retract)
                                  (declare (ignore retract))
                                  (push (list session turn reason) asked)
                                  t)
        (cell-publish "nodecode-muster"
                      (muster-frame "parent-1"
                                    (list* "session_id" "child-1" "turn_id" "turn-9"
                                           "label" "audit the retry loop" '("status" "running"))))
        (nodecode-muster::tick)
        (let ((answer (cell-entry "nodecode-muster" "muster" "stop child-1" "muster-cell-cmd")))
          (is (= 1 (length asked)) "the kernel was asked once")
          (is (equal "turn-9" (second (first asked))) "for the flight's own turn")
          (is (search "stopping" answer) "and the board says it is stopping"))))))

(deftest muster-cell-a-note-goes-to-the-flights-own-turn ()
  (with-cell-stop ((muster-start))
    (let ((sent '()))
      (with-stubbed-fdefinition (nle:submit (session prompt &key steer &allow-other-keys)
                                  (push (list session prompt steer) sent)
                                  t)
        (cell-publish "nodecode-muster"
                      (muster-frame "parent-1"
                                    (list* "session_id" "child-1" "turn_id" "turn-9"
                                           "label" "audit the retry loop" '("status" "running"))))
        (nodecode-muster::tick)
        (let ((answer (cell-entry "nodecode-muster" "muster" "note child-1 skip the tests" "muster-cell-cmd")))
          (is (= 1 (length sent)) "one message sent")
          (is (equal "child-1" (first (first sent))) "to the flight's session")
          (is (equal "turn-9" (third (first sent))) "steering its turn")
          (is (search "skip the tests" (second (first sent))) "with what was typed")
          (is (search "message sent" answer) "and the command says so"))))))

(deftest muster-cell-the-digest-names-what-needs-a-decision ()
  (with-cell-stop ((muster-start "stall_seconds" 30))
    (cell-publish "nodecode-muster" (muster-frame "parent-1" (muster-row "child-1" "index the corpus" "running")))
    (nodecode-muster::tick)
    (is (search "nothing needs you" (nodecode-muster::digest)) "a working fleet owes nothing")
    (muster-age "child-1" 600)
    (nodecode-muster::sweep (get-universal-time))
    (let ((report (nodecode-muster::digest)))
      (is (search "1 stuck" report) "the counts carry it")
      (is (search "needs a decision" report) "and the decision is named")
      (is (search "index the corpus" report) "with the job"))))

;;; --- the document ------------------------------------------------------------

(deftest muster-cell-a-row-of-another-boot-reads-gone ()
  (with-cell-stop ((muster-start "auto_requeue" nil))
    (cell-publish "nodecode-muster" (muster-frame "parent-1" (muster-row "child-1" "index the corpus" "running")))
    (nodecode-muster::tick)
    (is (search "working" (nodecode-muster::table)) "the row runs")
    (is (probe-file (nodecode-muster::store-file)) "the ledger is on disk")
    (funcall stop)
    (setf stop (muster-start "auto_requeue" nil))
    (is (equal "LOST" (getf (muster-flight "child-1") :state)) "the last image cut it off")
    (is (search "gone" (nodecode-muster::table)) "and the board says gone")))
