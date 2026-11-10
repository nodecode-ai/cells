;;;; cell-test.lisp --- the follow-through cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every test runs offline and in a scratch home: the round is a hash table
;;;; the test writes, the turn is a stubbed NLE:TURN, and the nudge is caught
;;;; in this cell's own DELIVER-NUDGE, so nothing is admitted into a session
;;;; and no provider is dialled.

(in-package #:nodecode.test)

(define-test-slice "follow-through" "FOLLOW-THROUGH-CELL-" :start nodecode-follow-through:start-cell)

(define-cell-lifecycle-tests "follow-through"
  (:help :follow-through)
  (:command "follow-through")
  (:hooks 'nle::record-assistant-message 'nle:submit)
  (:running (is (nle::find-tool "follow_through") "the tool is registered"))
  (:refused ("max_nudges" -1) ("max_nudges" "two")))

;;; --- the sentence ----------------------------------------------------------

(deftest follow-through-cell-quotes-a-promise ()
  (is (equal "I've started the deep dive and will send you the report shortly."
             (nodecode-follow-through:promise-sentence
              "Here is round one. I've started the deep dive and will send you the report shortly.")) "the promise sentence, whole")
  (is (equal "Ho iniziato l'analisi e ti aggiorno presto."
             (nodecode-follow-through:promise-sentence
              "Fatto il primo passo. Ho iniziato l'analisi e ti aggiorno presto.")) "and its Italian"))

(deftest follow-through-cell-leaves-alone ()
  (dolist (text '("If you want, I'll send you the report tomorrow."
                  "Tell me when you want the report and I'll send it."
                  "Should I start the migration now?"
                  "I'm not going to touch the crawler this week."
                  "Done: the report is in your folder already."
                  "I'll go."))
    (is (null (nodecode-follow-through:promise-sentence text))
        (format nil "left alone: ~a" text))))

;;; --- the round, and the nudge ----------------------------------------------

(defmacro with-follow-through-round ((sent &key (session "s1")) &body body)
  "BODY with the cell started on its defaults, the count cleared, NLE:TURN
answering SESSION's live turn and DELIVER-NUDGE collecting into SENT."
  `(with-cell-stop ((follow-through-start))
     (clrhash nodecode-follow-through::*nudges*)
     (setf nodecode-follow-through::*on* t)
     (let ((,sent '()))
       (with-stubbed-fdefinitions
           ((nle:turn () (list :session-id ,session :turn-id "t1"))
            (nodecode-follow-through::deliver-nudge (session-id text)
              (push (list session-id text) ,sent)))
         (flet ((follow-through-say (message)
                  (nodecode-follow-through:watch-turn
                   (lambda (turn message &rest arguments)
                     (declare (ignore turn arguments))
                     message)
                   nil message))
                (follow-through-prompt (prompt)
                  (nodecode-follow-through:watch-prompt
                   (lambda (session-id prompt &rest arguments)
                     (declare (ignore session-id prompt arguments))
                     nil)
                   ,session prompt)))
           ,@body)))))

(defun follow-through-answer (text &optional calls)
  "A model round's message: TEXT, and CALLS when the round is still acting."
  (if calls
      (nlk:json-object "role" "assistant" "content" text
                        "tool_calls" (vector (nlk:json-object "id" "c1" "type" "function")))
      (nlk:json-object "role" "assistant" "content" text)))

(deftest follow-through-cell-nudges-a-promise ()
  (with-follow-through-round (sent)
    (follow-through-say
     (follow-through-answer "All good so far. I've started the deep dive and will send the report shortly."))
    (is (= 1 (length sent)) "one nudge")
    (let ((prompt (second (first sent))))
      (is (search "[follow-through]" prompt) "tagged as this cell's own prompt")
      (is (search "I've started the deep dive and will send the report shortly." prompt) "quoting the promise")
      (is (search "do that work in this turn" prompt) "and naming the exits"))))

(deftest follow-through-cell-leaves-a-call-alone ()
  (with-follow-through-round (sent)
    (follow-through-say (follow-through-answer "I'll start the migration now." t))
    (is (null sent) "a round that calls a tool is still acting")
    (follow-through-say (nlk:json-object "role" "tool" "content" "I'll start it now."))
    (is (null sent) "a tool result is nobody's promise")))

(deftest follow-through-cell-caps-and-forgets ()
  (with-follow-through-round (sent)
    (let ((message (follow-through-answer "I'll start the migration and report back to you.")))
      (follow-through-say message)
      (follow-through-say message)
      (follow-through-say message)
      (is (= 2 (length sent)) "two nudges at most for one prompt")
      (follow-through-prompt "Something else, please.")
      (follow-through-say message)
      (is (= 3 (length sent)) "the operator's own prompt starts the count over")
      (follow-through-prompt (second (first sent)))
      (follow-through-say message)
      (is (= 4 (length sent)) "this cell's own nudge does not"))))

;;; --- the tool and the command -----------------------------------------------

(deftest follow-through-cell-answers-its-verbs ()
  (with-cell-stop ((follow-through-start))
    (let ((answer (nodecode-follow-through:read-follow-through
                   (nodecode-follow-through::decode-follow-through-ask
                    (nlk:json-object "quote" "I'll send the report shortly.")))))
      (is (search "I'll send the report shortly." answer) "the tool quotes the draft")
      (is (search "do that work in this turn" answer) "and answers the rule"))
    (is (search "follow-through is on" (cell-entry "nodecode-follow-through" "follow-through" "" "s1")) "the command says it is on")
    (is (search "off" (cell-entry "nodecode-follow-through" "follow-through" "off" "s1")) "and turns it off")
    (is (search "follow-through is off" (cell-entry "nodecode-follow-through" "follow-through" "status" "s1")) "and the status says so")
    (is (search "on" (cell-entry "nodecode-follow-through" "follow-through" "on" "s1")) "on turns it back on")))
