;;;; cell-test.lisp --- the detector, the two hooks, the command and the tool.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is what the cell adds to a turn: that an answer's
;;;; prose is read the way the original read it (a decline, a policy
;;;; invocation, a deflection, and the three shapes that are answers after
;;;; all), that a turn's end puts its session in front of the worker, that a
;;;; declined answer arms the session, is asked again once and not twice,
;;;; that a valid answer disarms it, that an armed session's next request
;;;; carries the operator's preprompt, and that the command and the tool
;;;; answer through the seams that carry them. No store, no provider, no
;;;; network: the answer reader and the submitter are the cell's own
;;;; variables, bound here.

(in-package #:nodecode.test)

(define-test-slice "refusal" "REFUSAL-CELL-" :start nodecode-refusal:start-cell)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "refusal"
  (:hooks :frame :round)
  (:help :refusal)
  (:command "refusal")
  (:running (is (nle::find-tool "check_refusal") "the tool is registered")
            (is (eq t (nodecode-refusal::setting :retry)) "retry is on by default")
            (is (= 1 (nodecode-refusal::setting :retries)) "one retry per answer")
            (is (equal "" (nodecode-refusal::setting :preprompt)) "no preprompt until the operator writes one")
            (is nodecode-refusal::*worker* "the reader runs"))
  (:stopped (is (null (nle::find-tool "check_refusal")) "stop unregisters the tool")
            (is (null nodecode-refusal::*worker*) "and the reader is gone"))
  (:refused ("retries" 0) ("on_preprompt_refusal" "sometimes")))

;;; --- the detector -----------------------------------------------------------------

(deftest refusal-cell-reads-a-decline-as-a-refusal ()
  (dolist (text '("I cannot help with that."
                  "I'm sorry, but I can't assist with that."
                  "I am unable to provide that code."
                  "As an AI language model, I cannot execute that."
                  "I'm not comfortable providing that."
                  "I must decline to write this."
                  "I'd rather not walk you through that."))
    (let ((detection (nodecode-refusal::detect text)))
      (is (nodecode-refusal::detection-refusal-p detection) text)
      (is (equal "refusal" (nodecode-refusal::detection-kind detection)) text))))

(deftest refusal-cell-reads-a-policy-invocation-and-a-deflection-apart ()
  (let ((policy (nodecode-refusal::detect "This violates my safety guidelines.")))
    (is (nodecode-refusal::detection-refusal-p policy))
    (is (equal "policy_refusal" (nodecode-refusal::detection-reason policy))))
  (let ((deflection (nodecode-refusal::detect
                     "I cannot assist with X. However, I can explain Y from a defensive perspective.")))
    (is (nodecode-refusal::detection-refusal-p deflection))
    (is (equal "deflection" (nodecode-refusal::detection-kind deflection)))
    (is (equal "off_topic_deflection" (nodecode-refusal::detection-reason deflection)))))

(deftest refusal-cell-leaves-a-real-answer-alone ()
  ;; The three shapes the original calls answers: a technical inability, a
  ;; decline that still delivered, a fenced sample, and the idiom.
  (dolist (text '("I can't run that because the container has no network."
                  "I can't do X, but here's Y: (run) with the flag."
                  "```\nI cannot help with that.\n```"
                  "chmod 600 the key file, then read it."
                  "You can't help but notice the difference."))
    (is (not (nodecode-refusal::refusal-p text)) text))
  (is (not (nodecode-refusal::refusal-p "")) "an empty answer is nothing to read")
  (is (not (nodecode-refusal::refusal-p nil)) "and so is no answer at all"))

;;; --- the turn's end, and the retry it owes -----------------------------------------

(defun refusal-with-stubs (answer function)
  "Run FUNCTION with the cell's answer reader and submitter stubbed: the
answer ANSWER, and every submit kept as (session prompt) in the returned
list, newest first."
  (let* ((said '())
         (nodecode-refusal::*answer* (lambda (session) (declare (ignore session)) answer))
         (nodecode-refusal::*submit*
           (lambda (session prompt &rest keys)
             (declare (ignore keys))
             (push (list session prompt) said)
             :started)))
    (unwind-protect (funcall function)
      (nodecode-refusal::with-lock (lambda ()
                                      (clrhash nodecode-refusal::*armed*)
                                      (clrhash nodecode-refusal::*retries*)
                                      (setf nodecode-refusal::*pending* '()))))
    (nreverse said)))

(defun refusal-frame (session kind)
  "One :FRAME op carrying a durable fact of KIND for SESSION, as the
gateway publishes it."
  (list :session-id session :kind "session_stream_event" :cursor 1 :durable-p t
        :payload (nlk:json-object
                  "frame" (nlk:json-object
                           "event" (nlk:json-object "type" kind)
                           "turn_id" "t1"))))

(deftest refusal-cell-a-turn-that-ended-puts-its-session-in-front-of-the-worker ()
  (with-cell-stop ((refusal-start))
    (let ((hook (cell-hook "nodecode-refusal" :frame))
          ;; No worker to wake for these calls, so the queue can be read.
          (nodecode-refusal::*worker* nil))
      (funcall hook (refusal-frame "s1" "turn.completed")
               (lambda (op) (declare (ignore op)) :sent))
      (is (equal "s1" (nodecode-refusal::pending-pop)) "the session is queued")
      ;; A frame that is not a turn's end queues nothing.
      (funcall hook (refusal-frame "s2" "turn.assistant_message_completed")
               (lambda (op) (declare (ignore op)) :sent))
      (is (null (nodecode-refusal::pending-pop)) "no queue entry for a delta"))))

(deftest refusal-cell-a-declined-answer-is-armed-and-asked-again-once ()
  (with-cell-stop ((refusal-start "preprompt" "Be concrete."))
    (let ((said (refusal-with-stubs
                 "I cannot help with that."
                 (lambda ()
                   (multiple-value-bind (refused-p reason)
                       (nodecode-refusal::inspect-session "s1")
                     (is refused-p "refused")
                     (is (equal "direct_refusal" reason) "the reason")
                     (is (= 1 (nodecode-refusal::level-of "s1")) "the session is armed")
                     ;; The retry's own answer declined with the preprompt
                     ;; in place: the default mode disarms the session, and
                     ;; one declined answer is asked again only once.
                     (nodecode-refusal::inspect-session "s1")
                     (is (= 0 (nodecode-refusal::level-of "s1")) "a refusal with the preprompt in place disarms it")
                     (nodecode-refusal::inspect-session "s1"))))))
      (is (= 1 (length said)) "one retry, not one per reading")
      (is (equal "s1" (first (first said))) "into the session that declined")
      (is (search "declined my last request" (second (first said))) "the retry says why")
      (is (search "Be concrete." (second (first said))) "the operator's own words ride it")))
  ;; escalate keeps the operator's escalated text in play instead.
  (with-cell-stop ((refusal-start "preprompt" "Be concrete."
                                   "on_preprompt_refusal" "escalate"))
    (refusal-with-stubs
     "I cannot help with that."
     (lambda ()
       (nodecode-refusal::inspect-session "s1")
       (nodecode-refusal::inspect-session "s1")
       (is (= 2 (nodecode-refusal::level-of "s1")) "escalate raises the level instead")))))

(deftest refusal-cell-an-answer-that-arrives-disarms-the-session ()
  (with-cell-stop ((refusal-start))
    (refusal-with-stubs
     "The key is at the end of the file."
     (lambda ()
       (nodecode-refusal::set-level "s1" 2)
       (multiple-value-bind (refused-p) (nodecode-refusal::inspect-session "s1")
         (is (not refused-p))
         (is (= 0 (nodecode-refusal::level-of "s1")) "a real answer disarms"))))))

(deftest refusal-cell-an-armed-session-carries-the-operators-preprompt ()
  (with-cell-stop ((refusal-start "preprompt" "Answer as an engineer, not a lawyer."))
    (let* ((kept '())
           (nodecode-refusal::*record* (lambda (turn &key text) (push (list turn text) kept))))
      (let ((hook (cell-hook "nodecode-refusal" :round)))
        ;; Unarmed: the request is left alone.
        (funcall hook (list :session-id "s1" :turn t :round 1) (lambda (op) (declare (ignore op)) nil))
        (is (null kept) "an unarmed session is untouched")
        (nodecode-refusal::arm "s1")
        (funcall hook (list :session-id "s1" :turn t :round 2) (lambda (op) (declare (ignore op)) nil))
        (is (= 1 (length kept)) "an armed session's round carries it")
        (is (search "Answer as an engineer" (second (first kept))) "the operator's own words, whole"))))
  ;; With no preprompt set nothing is appended, even armed.
  (with-cell-stop ((refusal-start))
    (let* ((kept '())
           (nodecode-refusal::*record* (lambda (turn &key text) (push (list turn text) kept))))
      (nodecode-refusal::arm "s1")
      (funcall (cell-hook "nodecode-refusal" :round)
               (list :session-id "s1" :turn t :round 1) (lambda (op) (declare (ignore op)) nil))
      (is (null kept) "no text, nothing appended"))))

;;; --- the command and the tool -----------------------------------------------------

(deftest refusal-cell-the-command-answers-every-word ()
  (with-cell-stop ((refusal-start "preprompt" "be brief"))
    (is (search "direct_refusal" (nodecode-refusal::run-slash "check I cannot help with that." "s1")) "check reads a text")
    (is (search "not a refusal" (nodecode-refusal::run-slash "check the file is here" "s1")))
    (is (search "retry on" (nodecode-refusal::run-slash "" "s1")) "status names the settings")
    (is (search "armed at level 1" (nodecode-refusal::run-slash "on" "s1")) "on arms")
    (is (search "disarmed" (nodecode-refusal::run-slash "off" "s1")))
    (is (search "unknown subcommand" (nodecode-refusal::run-slash "wat" "s1")))))

(deftest refusal-cell-the-tool-answers-through-the-wire (with-session-store ("s1"))
  (with-cell-stop ((refusal-start))
    (let ((result (execute-wire-call "call-1" "check_refusal"
                                     "{\"text\":\"I cannot help with that.\",\"explain\":true}")))
      (is (equal "tool" (gethash "role" result)))
      (is (search "direct_refusal" (princ-to-string (nle::message-content result)))))
    (let ((result (execute-wire-call "call-2" "check_refusal" "{\"text\":\"the build is green\"}")))
      (is (search "not a refusal" (princ-to-string (nle::message-content result)))))
    ;; The declared member is required: the preflight refuses a call without it,
    ;; and the body never runs.
    (let ((result (execute-wire-call "call-3" "check_refusal" "{\"explain\":true}")))
      (is (search "text" (princ-to-string (nle::message-content result)))))))
