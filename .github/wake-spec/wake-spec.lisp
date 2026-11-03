;;;; wake-spec.lisp --- the wake PR's shared scorer: what a wake must do.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A behaviour spec for the wake (a profile's own scheduled check-in, built
;;;; as a cron job of kind wake inside tools/nodecode-cron), run against any
;;;; branch of nodecode-ai/cells by .github/wake-spec/score.sh. It names only
;;;; the agreed surface:
;;;;
;;;;   (cron:wake :on :min "30m" :max "4h" [:allowance USD])  the operator turns it on
;;;;   (cron:wake :off) (cron:wake :status) (cron:wake :now)
;;;;   (cron:wake-next DELAY WHY)   the wake session picks its next fire
;;;;   NODECODE-CRON::*WAKE-SOURCES*  functions (since) -> one-line facts
;;;;   NODECODE-CRON::WAKE-SPENT-24H  (session now) -> USD, or NIL for unknown
;;;;   the wake job's id is "wake"; the cell's :tool hook gates its session
;;;;
;;;; and drives the cron cell the way its own tests do: the ingress seam
;;;; captured, TICK run by hand at the instant meant, frames published
;;;; through the installed observer. One DEFTEST is one point.

(in-package #:nodecode.test)

(define-test-slice "wakespec" "WAKE-SPEC-")

(defparameter +wake-operator+ "s-wake-operator"
  "The operator session that turns the wake on; the wake reports to it.")

(defvar *wake-sinces* '()
  "Every SINCE a WAKE-NEWS source was called with, newest first.")

(defun wake-job () (nodecode-cron::find-job "wake"))
(defun wake-session () (nodecode-cron::job-session (wake-job)))
(defun wake-next-at () (nodecode-cron::job-next-at (wake-job)))
(defun wake-tick (instant) (nodecode-cron::tick instant))

(defmacro wake-at ((instant) &body body)
  "BODY with the wall clock reading INSTANT."
  `(with-stubbed-fdefinitions ((get-universal-time () ,instant)) ,@body))

(defmacro wake-as ((session) &body body)
  "BODY as a call from SESSION."
  `(let ((nlk:*scribe-session-id* ,session)) ,@body))

(defun wake-news (&rest lines)
  "A facts source that records its SINCE and answers LINES."
  (lambda (since) (push since *wake-sinces*) lines))

(defun wake-on (&key (min "30m") (max "4h") allowance (now +cron-now+))
  (wake-at (now)
    (wake-as (+wake-operator+)
      (apply #'cron:wake :on :min min :max max
             (when allowance (list :allowance allowance))))))

(defun wake-status () (wake-as (+wake-operator+) (cron:wake :status)))

(defun wake-prompts () (mapcar (lambda (submit) (getf submit :prompt)) (cron-submits-to (wake-session))))

(defun wake-settle (instant answer)
  "The wake turn fired at INSTANT answers ANSWER and the next tick settles it."
  (cron-finish (wake-session) answer)
  (wake-tick (+ instant 5)))

(defmacro with-wake ((&key (min "30m") (max "4h") allowance (sources '((wake-news "a fact"))))
                     &body body)
  "BODY over a fresh cron runtime with the wake on, its facts from SOURCES
(:DEFAULT keeps the branch's own sources)."
  `(with-cron-runtime (:store t :submits t)
     (nlk:create-session :id +wake-operator+)
     (let ((*wake-sinces* '()))
       ,(if (eq sources :default)
            `(progn (wake-on :min ,min :max ,max :allowance ,allowance) ,@body)
            `(let ((nodecode-cron::*wake-sources* (list ,@sources)))
               (wake-on :min ,min :max ,max :allowance ,allowance)
               ,@body)))))

(defun wake-verdict (session form)
  "The cron cell's :tool hook answer for an eval of FORM from SESSION:
:ALLOWED, or the refusal."
  (wake-as (session)
    (funcall (cell-hook "nodecode-cron" :tool)
             (list :name "eval" :arguments (nlk:json-object "form" form) :call-id "c")
             (lambda (op) (declare (ignore op)) :allowed))))

(defun wake-word-p (word text)
  (and (stringp text) (cl-ppcre:scan (format nil "(?i)\\b~a\\b" word) text) t))

;;; --- on, status, the first fire ---------------------------------------------

(deftest wake-spec-on-makes-one-job-that-status-reads ()
  (with-wake ()
    (is-present (job (wake-job)) "on made the wake job")
    (wake-on :min "1h" :max "8h")
    (is (= 1 (length nodecode-cron::*jobs*)) "a second on re-bounds the one job")
    (let ((status (wake-status)))
      (is (wake-word-p "on" status) "status says on")
      (is (search "1h" status) "and the new min")
      (is (search "8h" status) "and the new max"))))

(deftest wake-spec-first-fire-falls-within-the-bounds ()
  (with-wake (:min "30m" :max "4h")
    (let ((due (wake-next-at)))
      (is (<= (+ +cron-now+ 1800) due (+ +cron-now+ 14400)) "first fire between min and max")
      (is (= 0 (wake-tick (1- due))) "not before it is due")
      (is (= 1 (wake-tick due)) "fires when due")
      (is (equal (list (wake-session)) (mapcar (lambda (s) (getf s :session)) (cron-submits)))
          "into the wake's own session"))))

;;; --- the agent's clock ------------------------------------------------------

(deftest wake-spec-next-clamps-to-the-bounds ()
  (with-wake (:min "30m" :max "4h")
    (let ((session (wake-session)) (t0 (+ +cron-now+ 100)))
      (wake-at (t0) (wake-as (session) (cron:wake-next "5m" "soon")))
      (is (= (+ t0 1800) (wake-next-at)) "below min reads min")
      (wake-at (t0) (wake-as (session) (cron:wake-next "12h" "later")))
      (is (= (+ t0 14400) (wake-next-at)) "above max reads max")
      (wake-at (t0) (wake-as (session) (is (stringp (cron:wake-next "2h" "the deploy lands")))))
      (is (= (+ t0 7200) (wake-next-at)) "inside the bounds as asked"))))

(deftest wake-spec-status-names-the-next-wake-and-why ()
  (with-wake ()
    (wake-at (+cron-now+) (wake-as ((wake-session)) (cron:wake-next "2h" "the deploy lands")))
    (is (search "the deploy lands" (wake-status)))))

(deftest wake-spec-next-is-refused-outside-its-own-session ()
  (with-wake ()
    (cron-add "30m" "other" :name "other")
    (let ((before (wake-next-at)))
      (wake-as ("cron-other")
        (is (signals-error cron:cron-error (cron:wake-next "1h" "x")) "another job's session"))
      (is (= before (wake-next-at)) "the wake did not move"))))

(deftest wake-spec-a-silent-wake-doubles-the-gap-up-to-max ()
  (with-wake (:min "30m" :max "2h")
    (wake-at (+cron-now+) (wake-as ((wake-session)) (cron:wake-next "30m" "start")))
    (let ((due (+ +cron-now+ 1800)))
      (dolist (gap '(3600 7200 7200))
        (is (= due (wake-next-at)) "due as last set")
        (is (= 1 (wake-tick due)) "fires")
        (wake-settle due "SILENT")
        (is (= (+ due gap) (wake-next-at)) "the gap doubled, capped at max")
        (setf due (+ due gap))))))

;;; --- the $0 check before any model call -------------------------------------

;; An instant the wake picked always fires ("check #178's CI in 30m" must
;; not be dropped because no source watches CI); the $0 gate holds only the
;; timer's own instants, the backoff after a silent wake.

(deftest wake-spec-a-picked-instant-fires-without-news ()
  (with-wake (:min "30m" :max "4h" :sources ((wake-news)))
    (wake-at (+cron-now+) (wake-as ((wake-session)) (cron:wake-next "30m" "check #178's CI")))
    (wake-tick (+ +cron-now+ 1800))
    (is (= 1 (length (cron-submits))) "the wake's own pick fires")))

(deftest wake-spec-no-news-skips-at-zero-cost-and-backs-off ()
  (with-wake (:min "30m" :max "4h" :sources ((wake-news)))
    (wake-at (+cron-now+) (wake-as ((wake-session)) (cron:wake-next "30m" "start")))
    (let ((due (+ +cron-now+ 1800)))
      (wake-tick due)
      (wake-settle due "SILENT")
      (is (= (+ due 3600) (wake-next-at)) "a silent wake doubled the gap")
      (setf due (+ due 3600))
      (wake-tick due)
      (is (= 1 (length (cron-submits))) "the backoff instant with no news: no model call")
      (is (= (+ due 7200) (wake-next-at)) "and the gap doubled again"))))

(deftest wake-spec-quiet-at-max-still-fires ()
  (with-wake (:min "30m" :max "1h" :sources ((wake-news)))
    (wake-at (+cron-now+) (wake-as ((wake-session)) (cron:wake-next "30m" "start")))
    (let ((due (+ +cron-now+ 1800)))
      (wake-tick due)
      (wake-settle due "SILENT")
      (is (= (+ due 3600) (wake-next-at)) "the backoff reached max")
      (wake-tick (+ due 3600))
      (is (= 2 (length (cron-submits))) "at max the wake fires with no news"))))

(deftest wake-spec-the-prompt-carries-the-facts-and-the-verbs ()
  (with-wake (:sources ((wake-news "PR #90: checks went red" "s-work: 2 turns ended")))
    (wake-tick (wake-next-at))
    (let ((prompt (first (wake-prompts))))
      (is (search "PR #90: checks went red" prompt))
      (is (search "s-work: 2 turns ended" prompt))
      (is (search "wake-next" prompt) "names the verb that sets the clock")
      (is (search "SILENT" prompt) "and the silent answer"))))

(deftest wake-spec-a-failing-source-reads-unavailable ()
  (with-wake (:sources ((lambda (since) (declare (ignore since)) (error "boom"))
                        (wake-news "other fact")))
    (wake-tick (wake-next-at))
    (let ((prompt (first (wake-prompts))))
      (is (search "unavailable" prompt) "an erroring source reads unavailable, never empty")
      (is (search "other fact" prompt) "the rest still ride"))))

(deftest wake-spec-the-cursor-moves-only-on-admission ()
  (with-wake ()
    (let ((due (wake-next-at)))
      (with-captured-submits (:disposition :duplicate :turn-id nil)
        (wake-tick due))
      (let ((first-since (first *wake-sinces*))
            (due2 (wake-next-at)))
        (wake-tick due2)
        (is (eql first-since (first *wake-sinces*)) "a fire not admitted leaves the window open")
        (wake-settle due2 "SILENT")
        (wake-tick (wake-next-at))
        (is (and (first *wake-sinces*) first-since (< first-since (first *wake-sinces*))))
        (is (<= due2 (first *wake-sinces*)) "an admitted fire moves the cursor to it")))))

;;; --- the operator's switches ------------------------------------------------

(deftest wake-spec-off-is-durable ()
  (with-wake ()
    (let ((due (wake-next-at)))
      (wake-as (+wake-operator+) (cron:wake :off))
      (is (wake-word-p "off" (wake-status)) "status says off")
      (setf nodecode-cron::*jobs* '())
      (nodecode-cron::load-jobs)
      (is (= 0 (wake-tick (+ due 86400))) "no fire after a reload")
      (is (null (cron-submits)))
      (is (wake-word-p "off" (wake-status)) "still off"))))

(deftest wake-spec-now-fires-at-once ()
  (with-wake ()
    (wake-as (+wake-operator+) (cron:wake :now))
    (is (equal (list (wake-session)) (mapcar (lambda (s) (getf s :session)) (cron-submits))))))

;;; --- what the wake session may not do ---------------------------------------

(deftest wake-spec-the-wake-cannot-turn-itself-on-or-rebound ()
  (with-wake (:min "30m" :max "4h")
    (let ((session (wake-session)))
      (wake-as (session)
        (is (signals-error cron:cron-error (cron:wake :on :min "1m" :max "1m")) "no new bounds"))
      (is (search "4h" (wake-status)) "the bounds held")
      (wake-as (+wake-operator+) (cron:wake :off))
      (wake-as (session)
        (is (signals-error cron:cron-error (cron:wake :on)) "an operator off sticks"))
      (is (wake-word-p "off" (wake-status))))))

(deftest wake-spec-the-wake-cannot-schedule-other-work ()
  (with-wake ()
    (cron-add "30m" "other" :name "other")
    (wake-as ((wake-session))
      (is (signals-error cron:cron-error (cron:add "30m" "z")) "add")
      (is (signals-error cron:cron-error (cron:run "other")) "run another job")
      (is (signals-error cron:cron-error (cron:remove "other")) "remove another job"))
    (is-present (job (nodecode-cron::find-job "other")) "the other job is still there")))

(deftest wake-spec-the-wake-session-observes-only ()
  (with-wake ()
    (let ((session (wake-session)))
      (is (eq :allowed (wake-verdict session "(+ 1 2)")) "a read passes")
      (is (eq :allowed (wake-verdict session "(cron:wake-next \"2h\" \"why\")")) "its own clock passes")
      (is (eq :allowed (wake-verdict session "(sh \"git log --oneline -3\")")) "a read-only sh passes")
      (is (stringp (wake-verdict session "(write-file \"x\" \"y\")")) "a write is refused")
      (is (stringp (wake-verdict session "(sh \"rm -rf /tmp/x\")")) "a destructive sh is refused")
      (is (eq :allowed (wake-verdict "s-other" "(write-file \"x\" \"y\")")) "other sessions untouched"))))

(deftest wake-spec-a-wake-turn-has-a-budget ()
  (with-wake ()
    (let ((budget (nle::live-turn-budget (nle::open-live-turn (wake-session) "t1"))))
      (is (getf budget :seconds) "a time cap")
      (is (getf budget :calls) "a call cap"))))

;;; --- money and reports ------------------------------------------------------

(deftest wake-spec-the-allowance-gates-the-fire ()
  (macrolet ((spent (usd &body body)
               `(with-stubbed-fdefinitions ((nodecode-cron::wake-spent-24h (session now)
                                              (declare (ignore session now)) ,usd))
                  ,@body)))
    (with-wake (:allowance 1.0)
      (spent 1.5 (wake-tick (wake-next-at)))
      (is (null (cron-submits)) "spent past the allowance: no fire")
      (spent nil (wake-tick (wake-next-at)))
      (is (null (cron-submits)) "unknown spend with an allowance: no fire")
      (spent 0.25 (wake-tick (wake-next-at)))
      (is (= 1 (length (cron-submits))) "under the allowance: fires"))
    (with-wake ()
      (spent nil (wake-tick (wake-next-at)))
      (is (= 1 (length (cron-submits))) "no allowance set: unknown spend does not block"))))

(deftest wake-spec-silence-reports-nothing-an-answer-reaches-the-operator ()
  (with-wake ()
    (let ((due (wake-next-at)))
      (wake-tick due)
      (wake-settle due "SILENT")
      (is (null (cron-submits-to +wake-operator+)) "SILENT posts nothing"))
    (let ((due (wake-next-at)))
      (wake-tick due)
      (wake-settle due "PR #90 went red: the lint job.")
      (is (some (lambda (s) (search "PR #90 went red" (getf s :prompt)))
                (cron-submits-to +wake-operator+))
          "an answer reaches the operator"))))

;;; --- the in-image digest ----------------------------------------------------

(deftest wake-spec-a-turn-ended-elsewhere-rides-the-next-wake ()
  (with-wake (:sources :default)
    (nlk:create-session :id "s-work")
    (with-durable-turn (turn "s-work" :input "fix the lint" :complete t))
    (wake-tick (wake-next-at))
    (is (some (lambda (prompt) (search "s-work" prompt)) (wake-prompts))
        "the session whose turn ended since the cursor is named")))
