;;;; cell-test.lisp --- lifecycle, the tool, /checkin, the rule and the two hooks.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring over a stubbed ingress and a
;;;; fixed clock: that a start installs the tool, /checkin, the manual and the
;;;; two hooks and a stop takes every one back; that a section of the wrong
;;;; shape refuses; that the tool reads the rule and can only quiet it; that a
;;;; chat never bound refuses in words; that a due check-in goes out once and
;;;; its wait doubles; that nothing fires outside the operator's hours; that
;;;; their own turn starts the quiet over; that the voice note rides the first
;;;; round only; and that the command binds, reaches and unbinds.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "checkin"
  (:config "session" "chat-1")
  (:hooks :frame :round)
  (:help :checkin)
  (:command "checkin")
  (:running (is (equal "chat-1" (nodecode-checkin::live :session)) "the section's chat is bound")
            (is (= 120 (nodecode-checkin::live :base)) "and the rule is the section's"))
  (:stopped (is (null (nle::find-tool "checkin")) "stop unregisters the tool"))
  (:refused ("base_minutes" 0) ("utc_offset_minutes" "east") ("voice" "shout")))

;;; --- the tool ----------------------------------------------------------------------

(deftest checkin-cell-the-tool-reads-the-rule ()
  (with-checkin ()
    (let* ((answer (tool-answer "action" "status"))
           (rule (gethash "rule" answer)))
      (is (gethash "ok" answer) "status answers")
      (is (gethash "bound" rule) "the bound chat is reported")
      (is (equal "normal" (gethash "level" rule)) "the level the section's wait is")
      (is (= 120 (gethash "base_minutes" rule)))
      (is (stringp (gethash "next_check_in" rule)) "and when the next one is due"))))

(deftest checkin-cell-the-tool-can-only-quiet-the-rule ()
  (with-checkin ()
    (let ((answer (tool-answer "action" "set" "level" "more")))
      (is (null (gethash "ok" answer)) "more often is refused")
      (is (equal "/checkin less" (gethash "ask_user_to_send" answer)) "with the command to send")
      (is (= 120 (nodecode-checkin::live :base)) "and nothing moved"))
    (let ((answer (tool-answer "action" "set" "level" "less")))
      (is (gethash "ok" answer) "less often is allowed")
      (is (= 240 (nodecode-checkin::live :base)) "and moved the wait"))))

(deftest checkin-cell-a-chat-never-bound-refuses-in-words ()
  (with-checkin ("session" "")
    (is (not (nodecode-checkin::bound-p)) "no chat yet")
    (nodecode-checkin::tick)
    (is (null (prompts)) "nothing goes out without a chat to put it in")
    (is (search "No chat is bound" (nodecode-checkin::status-text)) "and the rule says how to bind one")
    (is (null (gethash "bound" (tool-answer "action" "status"))))))

;;; --- the rule ----------------------------------------------------------------------

(deftest checkin-cell-a-due-check-in-goes-out-and-the-wait-doubles ()
  (with-checkin ()
    (let ((base *now*))
      (nodecode-checkin::tick)
      (is (null (prompts)) "nothing while the quiet is short")
      (setf *now* (+ base (* 2 3600) 60))
      (nodecode-checkin::tick)
      (is (= 1 (length (prompts))) "one check-in once it is long enough")
      (is (uiop:string-prefix-p "[Check-in]" (first (prompts))) "the prompt says what it is")
      (is (= 1 (nodecode-checkin::state :nudges)) "and the back-off counts it")
      (setf *now* (+ *now* (* 2 3600)))
      (nodecode-checkin::tick)
      (is (= 1 (length (prompts))) "the next wait is twice as long")
      (setf *now* (+ *now* (* 2 3600) 60))
      (nodecode-checkin::tick)
      (is (= 2 (length (prompts))) "and the second one goes out after it"))))

(deftest checkin-cell-nothing-fires-outside-the-hours ()
  (let ((*now* (at-hour 3)))
    (with-checkin ()
      (setf (nodecode-checkin::state :bound-at) (- *now* (* 20 3600)))
      (nodecode-checkin::tick)
      (is (null (prompts)) "an overdue check-in waits for the hours to open")
      (setf *now* (at-hour 9))
      (nodecode-checkin::tick)
      (is (= 1 (length (prompts))) "and goes out once they do"))))

(deftest checkin-cell-the-operators-own-turn-starts-the-quiet-over ()
  (with-checkin ()
    (setf *now* (+ *now* (* 2 3600) 60))
    (nodecode-checkin::tick)
    (is (= 1 (nodecode-checkin::state :nudges)) "one check-in went out")
    (nodecode-checkin::observe-frame (frame "t9"))
    (is (= 0 (nodecode-checkin::state :nudges)) "their own turn resets the back-off")
    (is (= *now* (nodecode-checkin::state :last-user)) "and is the exchange counted from")
    (nodecode-checkin::tick)
    (is (= 1 (length (prompts))) "so the wait starts over")
    (nodecode-checkin::observe-frame (frame "t9"))
    (nodecode-checkin::observe-frame (frame "t9"))
    (is (= *now* (nodecode-checkin::state :last-user)) "and one turn is noted once")))

(deftest checkin-cell-the-morning-brief-is-one-a-day ()
  (with-checkin ("morning_brief" t)
    (setf *now* (at-hour 8))
    (nodecode-checkin::tick)
    (is (= 1 (length (prompts))) "the day's brief goes out when the hours open")
    (is (uiop:string-prefix-p "[Morning brief]" (first (prompts))) "and says it is the brief")
    (nodecode-checkin::tick)
    (is (= 1 (length (prompts))) "never twice")))

;;; --- the right to speak --------------------------------------------------------------

(deftest checkin-cell-the-voice-note-rides-the-first-round-only ()
  (with-checkin ()
    (nodecode-checkin::voice-at-round (list :session-id "chat-1" :turn 'turn :round 0))
    (is (= 1 (length *noted*)) "the note rides the first round")
    (nodecode-checkin::voice-at-round (list :session-id "chat-1" :turn 'turn :round 1))
    (is (= 1 (length *noted*)) "not every round")
    (nodecode-checkin::set-live :voice "default")
    (nodecode-checkin::voice-at-round (list :session-id "chat-1" :turn 'turn :round 0))
    (is (= 1 (length *noted*)) "and not while the voice is the model's own")))

;;; --- /checkin ----------------------------------------------------------------------

(deftest checkin-cell-the-command-binds-reaches-and-unbinds ()
  (with-checkin ()
    (multiple-value-bind (text) (nodecode-checkin::run-command "status" "chat-7")
      (is (search "2 hour" text) "status names the wait"))
    (multiple-value-bind (text) (nodecode-checkin::run-command "on" "chat-7")
      (is (equal "chat-7" (nodecode-checkin::live :session)) "on binds the chat it was typed in")
      (is (search "chat-7" text) "and says so"))
    (multiple-value-bind (text) (nodecode-checkin::run-command "less" "chat-7")
      (is (= 240 (nodecode-checkin::live :base)) "less reaches earlier")
      (is (search "4 hour" text) "and says the new wait"))
    (nodecode-checkin::run-command "off" "chat-7")
    (is (not (nodecode-checkin::bound-p)) "off unbinds")))
