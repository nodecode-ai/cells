;;;; cell-test.lisp --- the thresholds, the notice in the request, the report.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here runs over a real store and a real turn loop, the one
;;;; stub being the provider: that a notice rides the very request in which a
;;;; threshold is first passed, that it comes once per level, that the urgent
;;;; wording is the one at the second level, that nothing is said below the
;;;; first threshold, that both levels arm again once the fill drops, and that
;;;; /chat-length and the chat_length tool answer the same numbers on demand.

(in-package #:nodecode.test)

(define-test-slice "chat-length" "CHAT-LENGTH-CELL-"
  :start nodecode-chat-length:start-cell
  :defaults (("notice_percent" 70) ("urgent_percent" 90) ("language" "auto")))

(define-cell-lifecycle-tests "chat-length"
  (:hooks :round)
  (:help :chat-length)
  (:command "chat-length")
  (:running
   (is (= 70 (nodecode-chat-length::setting :notice-percent)) "the first notice at 70 %")
   (is (= 90 (nodecode-chat-length::urgent-percent)) "the urgent one at 90 %"))
  (:refused ("notice_percent" 0)))

;;; --- the fixture ------------------------------------------------------------

(defmacro with-chat-length ((&rest pairs) &body body)
  "BODY over a temp store with session s1 and the cell started on PAIRS,
stopped on unwind."
  `(with-temp-store ()
     (ensure-durable-session "s1")
     (with-cell-stop ((chat-length-start ,@pairs))
       ,@body)))

(defun chat-window (session tokens)
  "Keep TOKENS as SESSION's window, the way a former round's measurement does."
  (nlk:session-state-put session nodecode-chat-length::*window-key* (princ-to-string tokens)))

(defun chat-measured (session prompt-tokens &key (output-tokens 0))
  "One completed turn on SESSION whose one round billed PROMPT-TOKENS in and
OUTPUT-TOKENS out: the fact the fill is read from."
  (let ((turn (nlk:admit-turn session (nlk::make-durable-id "command") "measured")))
    (nlk:record-turn-usage turn :input-tokens prompt-tokens :output-tokens output-tokens
                                :provider "test" :model "")
    (nlk:complete-turn turn nil)))

(defun chat-texts (context)
  "The chat-length instructions CONTEXT, one compiled request, carries."
  (loop for message in (coerce (nle::compiled-turn-context-messages context) 'list)
        for text = (nle::content-text (gethash "content" message))
        when (and (stringp text) (search "[chat-length" text))
          collect text))

(defun chat-ask (session seen &optional (prompt "hello"))
  "One turn on SESSION, the chat-length texts of every request it made unshifted
onto SEEN, newest first; SEEN."
  (with-provider-stub (context
                       (push (chat-texts context) seen)
                       (wire-message "assistant" :content "ok"))
    (nle:run prompt :session (nle:intern-session session)
              :command-id (nlk:make-durable-id "command")))
  seen)

(defun asked-once (session)
  "The chat-length texts the first request of one fresh turn on SESSION carried."
  (let ((seen (chat-ask session '())))
    (car (last seen))))

(defun chat-notice-count (session)
  "How many messages of SESSION's next request carry a chat-length notice."
  (length (asked-once session)))

(defun chat-level (session)
  "The level SESSION's chat has announced so far, 0 when none."
  (or (nodecode-chat-length::state-number session nodecode-chat-length::*level-key*) 0))

;;; --- the round's notice ------------------------------------------------------

(deftest chat-length-cell-a-notice-rides-the-request-at-the-threshold ()
  (with-chat-length ()
    (chat-window "s1" 100000)
    (chat-measured "s1" 75000)
    (let ((text (format nil "~{~a~}" (asked-once "s1"))))
      (is (search "[chat-length" text) "the request carries the instruction")
      (is (search "Heads-up" text) "at the first level")
      (is (search "75 %" text) "with the fill it measured")
      (is (search "session s1" text) "and the session id the opener names")
      (is (search "nodecode resume s1" text) "with the way back to the old chat"))))

(deftest chat-length-cell-the-urgent-wording-is-the-second-level ()
  (with-chat-length ()
    (chat-window "s1" 100000)
    (chat-measured "s1" 95000)
    (let ((text (format nil "~{~a~}" (asked-once "s1"))))
      (is (search "Urgent" text) "the urgent notice")
      (is (search "95 %" text) "at its own fill"))))

(deftest chat-length-cell-nothing-is-said-below-the-first-threshold ()
  (with-chat-length ()
    (chat-window "s1" 100000)
    (chat-measured "s1" 30000)
    (is (null (asked-once "s1")) "no notice under 70 %")))

(deftest chat-length-cell-a-level-is-announced-once-per-chat ()
  (with-chat-length ()
    (chat-window "s1" 100000)
    (chat-measured "s1" 75000)
    (is (= 1 (chat-notice-count "s1")) "the first turn is told once")
    (is (= 1 (chat-level "s1")) "and the level it announced is kept")
    (is (= 1 (chat-notice-count "s1")) "the next turn adds no notice")
    (chat-measured "s1" 95000)
    (let ((text (format nil "~{~a~}" (asked-once "s1"))))
      (is (= 2 (chat-notice-count "s1")) "past 90 % the urgent level speaks once more")
      (is (search "Urgent" text) "in the urgent wording"))
    (is (= 2 (chat-notice-count "s1")) "and then it is quiet again")))

(deftest chat-length-cell-both-levels-arm-again-once-the-fill-drops ()
  (with-chat-length ()
    (chat-window "s1" 100000)
    (chat-measured "s1" 95000)
    (is (= 1 (chat-notice-count "s1")) "the urgent notice")
    (is (= 2 (chat-level "s1")) "and the level it announced is kept")
    (chat-measured "s1" 5000)
    (is (= 1 (chat-notice-count "s1")) "a fill far below the first threshold adds nothing")
    (is (= 0 (chat-level "s1")) "and both levels arm again")
    (chat-measured "s1" 75000)
    (let ((text (format nil "~{~a~}" (asked-once "s1"))))
      (is (= 1 (chat-level "s1")) "and the first level is back at 75 %")
      (is (search "Heads-up" text) "with its own wording"))))

;;; --- the command and the tool ------------------------------------------------

(deftest chat-length-cell-the-command-answers-without-a-measurement ()
  (with-chat-length ()
    (multiple-value-bind (text dialog) (cell-entry "nodecode-chat-length" "chat-length" "" "s1")
      (is (null dialog) "a plain answer, no panel")
      (is (search "no measurement yet" text) text))))

(deftest chat-length-cell-the-command-and-the-tool-report-the-fill ()
  (with-chat-length ()
    (chat-window "s1" 100000)
    (chat-measured "s1" 95000)
    (let ((by-command (cell-entry "nodecode-chat-length" "chat-length" "" "s1"))
          (by-tool (let ((nlk:*scribe-session-id* "s1"))
                     (nodecode-chat-length::chat-length-tool (nlk:json-object)))))
      (is (search "95 %" by-command) by-command)
      (is (search "urgent at 90 %" by-command) "both thresholds are named")
      (is (search "nodecode resume" by-command) "and the opener once it is urgent")
      (is (search "95 %" by-tool) "the tool answers the same numbers"))))
