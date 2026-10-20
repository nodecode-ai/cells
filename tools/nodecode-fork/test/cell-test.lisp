;;;; cell-test.lisp --- forks that hold the context, the order, the refusals,
;;;; the clock and the stop.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the verb over real sessions, a real store and real
;;;; turn workers: that each task runs in an anchored fork of the calling
;;;; session at the calling turn, whose request carries the parent's exchange
;;;; and the parent's open eval call answered by the fold; that the answers
;;;; come back one a task, in the order asked; that a fork cannot fork and
;;;; each other refusal says why; that a fork past its time is stopped and the
;;;; call does not wait it out; that a call that stops waiting stops its forks;
;;;; and that what a fork ran reads back as the notes of its evals.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "fork"
  (:hooks :frame 'nle:turn-budget 'nle:name-session)
  (:help :fork)
  (:running (is (equal nodecode-fork::+primer+ (nle:help :fork)))
            (is (= 900 (nodecode-fork::setting :seconds)) "a fork's turn runs fifteen minutes at most by default"))
  (:refused ("seconds" 0))
  (:idle fork:fork-error (fork:run "a task")))

;;; --- the forks ---------------------------------------------------------------------

(deftest fork-cell-each-task-runs-in-a-fork-that-holds-the-context ()
  (let ((seen (make-hash-table :test #'equal :synchronized t)))
    (with-fork-call ("p" turn)
      (with-provider-stub (context
                           (let* ((task (fork-request-text context "[fork "))
                                  (which (if (and task (search "alpha" task)) "alpha" "beta")))
                             (setf (gethash which seen)
                                   (list (and (fork-request-text context "the parent's own question") t)
                                         (and (fork-request-text context "no result recorded (interrupted)") t)
                                         (and task (search (format nil "[fork ~:[2~;1~] of 2]" (equal which "alpha")) task) t)))
                             (wire-message "assistant" :content (format nil "~a done" which))))
        (let ((results (fork:run "the alpha task" "the beta task")))
          (is (equal '("the alpha task" "the beta task") (fork-field results :task)) "one answer a task, in the order asked")
          (is (equal '(:completed :completed) (fork-field results :status)))
          (is (equal '("alpha done" "beta done") (fork-field results :answer)) "each answer is its own fork's last words")
          (is (equal '(nil nil) (fork-field results :ran)) "neither fork ran a tool")
          (dolist (session (fork-field results :session))
            (is (equal (list "p" (nlk:durable-turn-turn-id turn)) (subseq (nlk::session-fork-edge session) 0 2)) "each is a fork of the calling session at the calling turn"))
          (is (equal '(t t t) (gethash "alpha" seen)) "the alpha fork's request carried the parent's exchange, the open eval call answered, and its own task")
          (is (equal '(t t t) (gethash "beta" seen)) "and so did the beta fork's")
          (is (zerop (hash-table-count nodecode-fork::*waiting*)) "no fork is left waited on"))))))

(deftest fork-cell-refuses-what-it-cannot-fork ()
  (with-fork-call ("q" turn)
    (is-carrying (text (princ-to-string (signals-error fork:fork-error (fork:run "a" "b" "c" "d" "e"))))
      ("at most 4 forks"))
    (is (signals-error fork:fork-error (fork:run)) "no task")
    (is (signals-error fork:fork-error (fork:run "  ")) "a blank task")
    (is (signals-error fork:fork-error (fork:run "a" :timeout 5)) "an option it does not take")
    (let ((nlk:*scribe-session-id* "fork-turn-x-1"))
      (is-carrying (text (princ-to-string (signals-error fork:fork-error (fork:run "a"))))
        ("a fork cannot fork")))
    (let ((nle::*current-durable-turn* nil))
      (is-carrying (text (princ-to-string (signals-error fork:fork-error (fork:run "a"))))
        ("inside a turn")))
    (is (not (nlk:session-exists-p (format nil "fork-~a-1" (nlk:durable-turn-turn-id turn)))) "no refusal made a fork")))

(deftest fork-cell-a-fork-past-its-time-is-stopped ()
  (with-fork-call ("r" turn)
    (let ((nodecode-fork::*grace-seconds* 0))
      (with-provider-stub (nil (sleep 3) (wire-message "assistant" :content "late"))
        (let* ((began (get-internal-real-time))
               (result (first (fork:run "a slow task" :seconds 1)))
               (took (/ (- (get-internal-real-time) began) internal-time-units-per-second)))
          (is (eq :timeout (getf result :status)) "the fork ran past its second and was asked to stop")
          (is (< took 12) "the call did not wait out the slow fork")
          (is (not (nlk:active-turn-p (getf result :session))) "the fork's turn is over")
          (is (zerop (hash-table-count nodecode-fork::*waiting*)) "no fork is left waited on"))))))

(deftest fork-cell-a-call-that-stops-waiting-stops-its-forks ()
  (with-fork-call ("s" turn)
    (with-provider-stub (nil (sleep 3) (wire-message "assistant" :content "never read"))
      (let* ((session nlk:*scribe-session-id*)
             (current nle::*current-durable-turn*)
             (child (format nil "fork-~a-1" (nlk:durable-turn-turn-id current)))
             (caller (bt2:make-thread (lambda ()
                                        (let ((nlk:*scribe-session-id* session)
                                              (nle::*current-durable-turn* current))
                                          (catch 'stop (fork:run "a task the caller gives up on"))))
                                      :name "fork-caller")))
        (is (await (:timeout 10) (nlk:active-turn-p child)) "the fork's turn runs")
        (bt2:interrupt-thread caller (lambda () (throw 'stop :stopped)))
        (is (eq :stopped (bt2:join-thread caller)) "the call stopped waiting")
        (is (await (:timeout 10) (nlk:events :session-id child :kind "turn.cancelled" :as :exists)) "and its fork's turn was cancelled")
        (is (zerop (hash-table-count nodecode-fork::*waiting*)) "no fork is left waited on")))))

(deftest fork-cell-what-a-fork-ran-reads-as-its-notes ()
  (with-fork-call ("t" turn)
    (let* ((worked (seed-completed-turn "worker" "work" "done"
                                        :tools '(("c-1" "eval" "{\"form\":\";; read the hook header\\n(sh \\\"ls\\\")\"}" "ok")
                                                 ("c-2" "report_issue" "{\"tool\":\"eval\"}" "noted"))))
           (ran (nodecode-fork::ran "worker" (nlk:durable-turn-turn-id worked))))
      (is (equal '(";; read the hook header" "report_issue") ran) "an eval reads as its note, any other tool as its name"))
    (let ((named (lambda (&rest args) (declare (ignore args)) "named")))
      (is (equal "named" (nodecode-fork::naming-hook named "t" "a name")) "any other session is named as before")
      (is (null (nodecode-fork::naming-hook named "fork-turn-x-1" "a name")) "a fork is never named"))))
