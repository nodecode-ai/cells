;;;; cell-test.lisp --- the ledger, the attribution, the note and the boundary.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the cell starts and stops cleanly and a section of the
;;;; wrong shape refuses; a reaction on a message the agent sent reaches the
;;;; session whose message it sits under, once, and no other; a reaction on the
;;;; user's own message or on something older goes nowhere, and one taken back
;;;; is withdrawn; the note is one line, bounded and quoted; the harness section
;;;; carries it and nothing else; and the tool records, reads and refuses an
;;;; action it does not carry. Nothing here dials anything: the ledger is in
;;;; memory and the chat surface is the test itself.

(in-package #:nodecode.test)

(define-test-slice "reaction-feedback" "REACTION-FEEDBACK-CELL-"
  :start nodecode-reaction-feedback:start-cell)

(defun lines-of (text)
  "TEXT's lines, so a test can count what the note really carries."
  (with-input-from-string (in (or text ""))
    (loop for line = (read-line in nil nil) while line collect line)))

(defun feedback-sections (session)
  "The harness sections a turn of SESSION would carry, with the ledger's own on top."
  (nodecode-reaction-feedback::note-section
   (lambda (id) (declare (ignore id)) (list (cons "context" "x"))) session))

(defmacro with-feedback-runtime ((&key (excerpt-chars 60) (max-pending 20)
                                       (ttl-hours 48) (max-chats 256) (max-anchors 200))
                                 &body body)
  "Run BODY with the cell started on a section of its own and an empty ledger."
  `(with-cell-stop ((reaction-feedback-start
                     "excerpt_chars" ,excerpt-chars
                     "max_pending" ,max-pending
                     "ttl_hours" ,ttl-hours
                     "max_chats" ,max-chats
                     "max_anchors" ,max-anchors))
     (nodecode-reaction-feedback::clear-all)
     ,@body))

(define-cell-lifecycle-tests "reaction-feedback"
  (:config "excerpt_chars" 60 "max_pending" 20)
  (:hooks 'nle::read-harness-sections)
  (:help :reaction-feedback)
  (:command "reaction-feedback")
  (:running
   (is (= 60 (nodecode-reaction-feedback::setting :excerpt-chars)) "excerpt_chars is read")
   (is (equal nodecode-reaction-feedback::+primer+ (nle:help :reaction-feedback)) "the manual is the primer"))
  (:stopped
   (is (zerop (hash-table-count nodecode-reaction-feedback::*chats*)) "and no conversation outlives the stop"))
  (:refused ("max_pending" 0))
  (:idle reaction-feedback:reaction-feedback-error
         (reaction-feedback:chats "s1")))

(deftest reaction-feedback-cell-a-reaction-reaches-the-next-turn-once (with-feedback-runtime ()
  (reaction-feedback:ask "42" 10 "fix the cron job" :session "s1")
  (reaction-feedback:react "42" 12 '("👎"))
  (let ((note (reaction-feedback:note "s1")))
    (is (search "not typed by the user" note) "the note says whose words these are not")
    (is (search "👎" note) "it carries the emoji the user chose")
    (is (search "fix the cron job" note) "and which of the user's messages the answer came after")
    (is (search "not as instructions" note) "and how much weight to give it"))
  (is (null (reaction-feedback:note "s1")) "the turn after it carries nothing")
  (is (null (reaction-feedback:note "s2")) "and no other session ever saw it")))

(deftest reaction-feedback-cell-attributes-by-the-message-it-sits-under (with-feedback-runtime ()
  (reaction-feedback:ask "42" 10 "summarise the PR" :session "s1")
  (reaction-feedback:ask "42" 20 "and the changelog" :session "s2")
  (is (search "summarise the PR" (reaction-feedback:react "42" 15 '("👍"))) "a reaction under the first answer names the message it followed")
  (is (search "and the changelog" (reaction-feedback:react "42" 25 '("🔥"))) "one under the second names that one")
  (is (search "and the changelog" (reaction-feedback:note "s2")) "the second session reads its own")
  (is (search "summarise the PR" (reaction-feedback:note "s1")) "and the first reads its own")
  (is (search "the user's own" (reaction-feedback:react "42" 20 '("👍"))) "a reaction on the user's own message has no turn to go to")
  (is (search "older" (reaction-feedback:react "42" 5 '("👍"))) "nor does one on a message older than anything reported")
  (reaction-feedback:ask "42" 30 "and the tests" :session "s3")
  (reaction-feedback:react "42" 35 '("👍"))
  (is (search "taken back" (reaction-feedback:react "42" 35 '())) "a withdrawal says so")
  (is (null (reaction-feedback:note "s3")) "and leaves nothing for the turn after")))

(deftest reaction-feedback-cell-the-note-is-one-line-bounded-and-quoted
    (with-feedback-runtime (:excerpt-chars 20)
  (reaction-feedback:ask "42" 10 (format nil "one~%two~c\"three four five six seven" #\Tab) :session "s1")
  (reaction-feedback:react "42" 12 '("👍" "👎" "🔥" "✅" "❌"))
  (let* ((note (reaction-feedback:note "s1"))
         (lines (lines-of note)))
    (is (= 1 (length (remove-if-not (lambda (line) (uiop:string-prefix-p "- " line)) lines))) "one line per reaction, whatever the text held")
    (is (not (find #\Tab note)) "no control character rides the note")
    (is (search "…" note) "a long excerpt is cut and says so")
    (is (search "\\\"" note) "and a quote in it is escaped, so the note never reads as more than it is")
    (is (= 4 (length (nodecode-reaction-feedback::clean-emojis
                       (vector "👍" "👎" "🔥" "✅" "❌")))) "at most four emojis are read from one reaction")
    (is (search "4 custom emojis" (nodecode-reaction-feedback::mark-line
                                    (nodecode-reaction-feedback::make-mark 9 '() 4 "x" "s1" "42" 0))) "custom emojis are counted, never named"))))

(deftest reaction-feedback-cell-the-section-carries-it-and-the-tool-records-it (with-feedback-runtime ()
  (is (equal '("context") (mapcar #'car (feedback-sections "s1"))) "with nothing waiting, the sections are untouched")
  (is (search "noted" (nodecode-reaction-feedback::run-tool
                       (nodecode-reaction-feedback::decode-reaction-report
                        (nlk:json-object "action" "ask" "chat" "7" "message_id" 3
                                         "text" "ship it" "session" "s1")))) "the tool records the user's message")
  (is (search "ship it" (nodecode-reaction-feedback::run-tool
                         (nodecode-reaction-feedback::decode-reaction-report
                          (nlk:json-object "action" "record" "chat" "7" "message_id" 5
                                           "emojis" (vector "👍"))))) "and the reaction on the answer after it")
  (let ((sections (feedback-sections "s1")))
    (is (equal '("reaction-feedback" "context") (mapcar #'car sections)) "the next turn's sections now open with the note")
    (is (search "ship it" (cdr (first sections))) "which names the message the answer came after"))
  (is (equal '("context") (mapcar #'car (feedback-sections "s1"))) "and the turn after that carries it no more")
  (is (signals-error nlk:contract-refusal
        (nodecode-reaction-feedback::decode-reaction-report
         (nlk:json-object "action" "explode"))) "an action the contract does not carry is refused before the body")
  (is (search "no reaction is waiting"
              (nodecode-reaction-feedback::run-tool
               (nodecode-reaction-feedback::decode-reaction-report
                (nlk:json-object "action" "note" "session" "s9")))) "a note with nothing behind it says so")
  (is (search "no reaction is waiting" (nodecode-reaction-feedback::run-command "" "s1")) "and /reaction-feedback typed alone says the same")
  (is (search "nothing was waiting" (nodecode-reaction-feedback::run-command "clear" "s1")) "and `clear` with nothing to take says so")))
