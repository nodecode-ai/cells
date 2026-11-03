;;;; wake-test.lisp --- the wake job: its verbs, its clock, its gates, its level.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the design wake.lisp argues for: the first wake
;;;; fires, a picked instant fires with or without news, the timer's own
;;;; instants wait for news or the longest gap at no cost, the money gates
;;;; skip before a submit, the next instant after a settle, the stop that
;;;; only (cron:wake :on) undoes, the record across a reload, the turn's
;;;; tool-call budget, and which calls the observe level lets through.

(in-package #:nodecode.test)

(defparameter +cron-wake-operator+ "s-cron-wake-operator")

(defun cron-wake-job () (nodecode-cron::find-job "wake"))

(defmacro with-cron-wake ((&key (min "30m") (max "4h") allowance (news ''()) (spent 0d0)) &body body)
  "BODY over a started cron cell, a store, captured submits, the wake on at
+CRON-NOW+ from the operator's session with no jitter, its news NEWS and its
spend SPENT."
  `(with-cron-runtime (:store t :submits t)
     (nlk:create-session :id +cron-wake-operator+)
     (let ((nodecode-cron::*wake-sources* (list (lambda (since) (declare (ignore since)) ,news)))
           (nodecode-cron::*wake-jitter* (constantly 0)))
       (with-stubbed-fdefinitions ((nodecode-cron::wake-spent-24h (session now)
                                     ,spent))
         (let ((nlk:*scribe-session-id* +cron-wake-operator+))
           (nodecode-cron::wake-verb :on :min ,min :max ,max
                                     ,@(when allowance `(:allowance ,allowance))
                                     :now +cron-now+))
         ,@body))))

(defun cron-wake-as-wake (thunk)
  (let ((nlk:*scribe-session-id* "cron-wake")) (funcall thunk)))

(defun cron-wake-settle (at answer)
  (cron-finish "cron-wake" answer)
  (nodecode-cron::tick at))

;;; --- the clock --------------------------------------------------------------------------

(deftest cron-cell-wake-the-first-wake-fires-then-the-timer-waits-for-news ()
  (with-cron-wake (:min "30m" :max "4h")
    (let ((first (+ +cron-now+ 1800)))
      (is (= first (nodecode-cron::job-next-at (cron-wake-job))) "the first wake is one shortest gap away")
      (is (= 1 (nodecode-cron::tick first)) "the first wake fires with no news")
      (cron-wake-settle (+ first 60) "SILENT")
      (is (= (+ first 3600) (nodecode-cron::job-next-at (cron-wake-job))) "silent: the gap doubles, from the start")
      (is (= 0 (nodecode-cron::tick (+ first 3600))) "no news: no fire")
      (is (= 1 (length (cron-submits))) "and no model call")
      (is (equal "skipped" (getf (nodecode-cron::job-last (cron-wake-job)) :status)))
      (is (= (+ first 3600 7200) (nodecode-cron::job-next-at (cron-wake-job))) "the gap doubled again"))))

(deftest cron-cell-wake-a-picked-instant-fires-without-news ()
  (with-cron-wake (:min "30m" :max "4h")
    (let ((first (+ +cron-now+ 1800)))
      (nodecode-cron::tick first)
      ;; Inside the fire the wake picks its next instant; the settle applies it.
      (is (search "next wake in 45m" (cron-wake-as-wake
                                       (lambda () (nodecode-cron::wake-next-verb "45m" "check CI" :now (+ first 60))))))
      (cron-wake-settle (+ first 120) "SILENT")
      (is (= (+ first 120 2700) (nodecode-cron::job-next-at (cron-wake-job))) "the picked delay, from the wake's end")
      (is (= 1 (nodecode-cron::tick (+ first 120 2700))) "a picked instant fires with no news")
      (is (search "the time you picked (check CI)" (getf (second (cron-submits)) :prompt)) "it reads why"))))

(deftest cron-cell-wake-next-is-held-to-the-bounds-and-to-its-own-session ()
  (with-cron-wake (:min "30m" :max "2h")
    (is (search "held to the bounds 30m to 2h"
                (cron-wake-as-wake (lambda () (nodecode-cron::wake-next-verb "9h" nil :now +cron-now+)))))
    (is (= (+ +cron-now+ 7200) (nodecode-cron::job-next-at (cron-wake-job))) "outside a fire the clock moves now")
    (let ((nlk:*scribe-session-id* +cron-wake-operator+))
      (is (signals-error cron:cron-error (cron:wake-next "1h")) "not from another session"))))

(deftest cron-cell-wake-a-wake-that-spoke-resets-the-gap-and-reports ()
  (with-cron-wake (:min "30m" :max "4h")
    (let ((first (+ +cron-now+ 1800)))
      (nodecode-cron::tick first)
      (cron-wake-settle (+ first 60) "PR #178 went red on macOS.")
      (let ((job (cron-wake-job)))
        (is (= (+ first 60 1800) (nodecode-cron::job-next-at job)) "spoke: the shortest gap, from its end")
        (is (equal "PR #178 went red on macOS." (nodecode-cron::wake-get job :said)))
        ;; The answer went to the session that turned wakes on.
        (is (search "PR #178 went red" (getf (first (cron-submits-to +cron-wake-operator+)) :prompt)))))))

;;; --- the gates ---------------------------------------------------------------------------

(deftest cron-cell-wake-the-allowance-skips-before-any-submit ()
  (with-cron-wake (:allowance 1 :spent 1.25d0)
    (is (= 0 (nodecode-cron::tick (+ +cron-now+ 1800))) "spent: no fire")
    (is (null (cron-submits)))
    (is (search "allowance spent: $1.25 of $1.00" (getf (nodecode-cron::job-last (cron-wake-job)) :line))))
  (with-cron-wake (:allowance 1 :spent nil)
    (is (= 0 (nodecode-cron::tick (+ +cron-now+ 1800))) "unknown spend under an allowance: no fire")
    (is (search "spend unknown" (getf (nodecode-cron::job-last (cron-wake-job)) :line))))
  (with-cron-wake (:spent nil)
    (is (= 1 (nodecode-cron::tick (+ +cron-now+ 1800))) "no allowance, no money gate")))

(deftest cron-cell-wake-a-source-that-fails-reads-unavailable-and-is-not-news ()
  (with-cron-runtime (:store t :submits t)
    (let ((nodecode-cron::*wake-sources* (list 'cron-wake-broken-source)))
      (multiple-value-bind (news gaps) (nodecode-cron::wake-news 0)
        (is (null news))
        (is (search "cron-wake-broken-source: unavailable" (first gaps)))))))

(defun cron-wake-broken-source (since)
  (declare (ignore since))
  (error "the prs cell is not running"))

;;; --- the stop and the record ----------------------------------------------------------------

(deftest cron-cell-wake-off-sticks-until-the-next-on ()
  (with-cron-wake ()
    (nodecode-cron::tick (+ +cron-now+ 1800))
    (let ((cancelled '()))
      (with-stubbed-fdefinitions ((nodecode-cron::cancel-wake-turn (session turn)
                                    (push (list session turn) cancelled)))
        (let ((nlk:*scribe-session-id* +cron-wake-operator+))
          (is (search "was asked to stop" (cron:wake :off)) "off stops the wake in flight")))
      (is (equal '(("cron-wake" "t1")) cancelled)))
    (cron-wake-settle (+ +cron-now+ 1900) "SILENT")
    (is (eq :paused (nodecode-cron::job-state (cron-wake-job))) "a settle does not turn it back on")
    (let ((nlk:*scribe-session-id* +cron-wake-operator+))
      (is (signals-error cron:cron-error (cron:resume "wake")) "resume leaves the wake alone")
      (is (signals-error cron:cron-error (cron:run "wake")) "so does run")
      (is (signals-error cron:cron-error (cron:edit "wake" :schedule "5m")) "and edit"))
    (cron-wake-as-wake
     (lambda () (is (signals-error cron:cron-error (cron:wake :on)) "the wake cannot turn itself on")))
    (setf nodecode-cron::*jobs* '())
    (nodecode-cron::load-jobs)
    (is (eq :paused (nodecode-cron::job-state (cron-wake-job))) "off survives a reload")
    (is (equal +cron-wake-operator+ (nodecode-cron::wake-get (cron-wake-job) :off-by)))
    (let ((nlk:*scribe-session-id* +cron-wake-operator+))
      (is (search "wakes on" (nodecode-cron::wake-verb :on :now (+ +cron-now+ 4000)))))
    (is (eq :active (nodecode-cron::job-state (cron-wake-job))))))

(deftest cron-cell-wake-the-record-round-trips ()
  (with-cron-wake (:min "20m" :max "3h" :allowance 2.5d0)
    (cron-wake-as-wake (lambda () (nodecode-cron::wake-next-verb "1h" "see #178" :now +cron-now+)))
    (let ((before (copy-list (nodecode-cron::job-wake (cron-wake-job)))))
      (setf nodecode-cron::*jobs* '())
      (nodecode-cron::load-jobs)
      (let ((after (nodecode-cron::job-wake (cron-wake-job))))
        (dolist (key '(:min :max :gap :calls :allowance :cause :why))
          (is (equalp (getf before key) (getf after key)) (format nil "~a comes back" key)))))))

;;; --- the turn ---------------------------------------------------------------------------------

(deftest cron-cell-wake-a-wake-turn-is-held-to-its-calls ()
  (with-cron-wake ()
    (is (equal '(:seconds 600 :calls 40) (nle::live-turn-budget (nle::open-live-turn "cron-wake" "t1"))))))

(deftest cron-cell-wake-the-observe-level-lets-reads-through-and-refuses-writes ()
  (flet ((verdict (form &optional strings)
           (let ((arguments (make-hash-table :test #'equal)))
             (setf (gethash "form" arguments) form)
             (when strings (setf (gethash "strings" arguments) strings))
             (nodecode-cron::wake-call-refusal (list :name "eval" :arguments arguments)))))
    (dolist (read '("(sh \"git log -3 --oneline\")"
                    "(sh \"gh pr checks 178 --repo nodecode-ai/nodecode | head -20\")"
                    "(sh \"rg -n wake /home/vise/Lab/nodecode\")"
                    "(recall-log :match \"keep an eye\" :limit 20)"
                    "(cron:wake-next \"2h\" \"check CI\")"
                    "(cron:wake :status)"))
      (is (null (verdict read)) (format nil "a read passes: ~a" read)))
    (dolist (write '("(write-file \"x\" \"y\")"
                     "(edit \"x\" ($ \"a\") ($ \"b\"))"
                     "(defun foo () 1)"
                     "(setf *wake-prompt* \"\")"
                     "(sh \"git push origin main\")"
                     "(sh \"rm -rf build\")"
                     "(sh \"ls > listing.txt\")"
                     "(sh \"echo $(cat ~/.token)\")"
                     "(sh \"gh pr merge 178\")"
                     "(sh \"sed -i s/a/b/ f\")"
                     "(sh ($ \"script\"))"
                     "(ncd:request \"POST\" \"/channels/1/messages\" :body (:content \"hi\"))"
                     "(nck:board-post \"1\" :claim \"x\")"
                     "(cron:wake :on :max \"1m\")"
                     "(cron:add \"1m\" \"loop\")"))
      (is (verdict write) (format nil "a write is refused: ~a" write)))))

(deftest cron-cell-wake-the-gate-holds-the-wake-session-only ()
  (with-cron-wake ()
    (let ((arguments (make-hash-table :test #'equal))
          (ran 0))
      (setf (gethash "form" arguments) "(write-file \"x\" \"y\")")
      (flet ((call (session)
               (let ((nlk:*scribe-session-id* session))
                 (nodecode-cron::wake-gate (list :arguments arguments)
                                           (lambda (op) (declare (ignore op)) (incf ran) :ran)))))
        (is (eq :ran (call +cron-wake-operator+)) "another session's write runs")
        (is (not (eq :ran (call "cron-wake"))) "the wake's write does not")
        ;; As a real turn calls it: the :tool point runs before the eval binds
        ;; the scribe, so only the running turn's record names the session.
        (let ((nle::*live-turn* (nle::open-live-turn "cron-wake" "t9")))
          (is (not (eq :ran (call nil))) "the live turn's session is the wake's"))
        (is (= 1 ran))))))
