;;;; cell-test.lisp --- a pin, its delivery, its edits, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the cell starts and stops cleanly, its tool and
;;;; command answer, a pin outlives a restart and reaches a turn's first
;;;; request, an unset pins_file refuses, and a setting outside its range
;;;; refuses at start. The pins file is a file in the scratch home, so no
;;;; network is touched at all.

(in-package #:nodecode.test)

(define-test-slice "memlock" "MEMLOCK-CELL-" :start nodecode-memlock:start-cell)

;;; --- helpers -------------------------------------------------------------------

(defvar *memlock-calls* 0
  "How many tool calls this test file made, for call ids.")

(defun memlock-pins-file ()
  "The pins file this test's scratch home holds."
  (namestring (nlk:home "memlock/pins.json")))

(defmacro with-memlock ((&rest pairs) &body body)
  "BODY with a session store open and the cell started on its defaults,
PAIRS overriding the section."
  `(with-session-store ("memlock-test")
     (with-cell-stop ((memlock-start "pins_file" (memlock-pins-file) ,@pairs))
       ,@body)))

(defun memlock-json (&rest pairs)
  "PAIRS as the JSON body a tool call carries."
  (nlk:encode-json-object (apply #'nlk:make-json-object pairs)))

(defun tool-text (name &rest arguments)
  "The text the tool called NAME answers, called as the provider's call is."
  (princ-to-string
   (nle::message-content
    (execute-wire-call (format nil "memlock-call-~d" (incf *memlock-calls*)) name
                        (if arguments (apply #'memlock-json arguments) "{}")))))

(defun memlock-reminders (context)
  "The memlock reminder blocks one compiled request carries."
  (loop for message in (coerce (nle::compiled-turn-context-messages context) 'list)
        for text = (nle::content-text (gethash "content" message))
        when (and (stringp text) (search "[Standing instructions" text))
          collect text))

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "memlock"
  (:config "pins_file" (memlock-pins-file))
  (:hooks :round)
  (:help :memlock)
  (:command "pins")
  (:running (is (string= (memlock-pins-file) (namestring (nodecode-memlock::pins-pathname)))
                "the section's file is where the pins go")
            (is (= 16 (nodecode-memlock::setting :max-pins)))
            (is (= 600 (nodecode-memlock::setting :reminder-chars))))
  (:refused ("max_pins" 0) ("reminder_chars" 10))
  (:idle nodecode-memlock::memlock-error (nodecode-memlock::status-text)))

;;; --- the tool and the command ---------------------------------------------------

(deftest memlock-cell-pin-list-update-unpin ()
  (with-memlock ()
    (let ((answer (tool-text "pin" "text" "Always reply in bullet points"
                              "priority" 80 "reminder" "Reply in bullet points")))
      (is-carrying answer "Pinned" ("Always reply in bullet points") ("priority=80"))
      (is (search "pin_" answer) "the answer names the id"))
    (let ((listed (nodecode-memlock::status-text)))
      (is-carrying listed ("1 pinned standing instruction") ("Always reply in bullet points"))
      (is (search "priority 80" listed)))
    (let ((id (getf (first (nodecode-memlock::read-pins)) :id)))
      (is-carrying (updated (tool-text "pin" "action" "update" "pin_id" id
                                       "text" "Always reply in numbered lists"))
                   "Updated" ("Always reply in numbered lists"))
      (is (= 1 (length (nodecode-memlock::read-pins))) "an update keeps one pin")
      (is-carrying (listing (nodecode-memlock::run-slash "" nil))
                   ("Always reply in numbered lists"))
      (is-carrying (dropped (nodecode-memlock::run-slash (format nil "unpin ~a" id) nil))
                   ("Unpinned"))
      (is (null (nodecode-memlock::read-pins)) "the file holds no pin now")
      (is (search "No pins" (nodecode-memlock::status-text))))))

(deftest memlock-cell-a-pin-that-does-not-exist-refuses ()
  (with-memlock ()
    (is-carrying (answer (tool-text "pin" "action" "unpin" "pin_id" "pin_nope"))
                 "ERROR" ("no pin") ("pin_nope"))
    (is-carrying (listed (tool-text "pin" "action" "list")) ("No pins"))))

(deftest memlock-cell-the-pin-cap-refuses-one-more ()
  (with-memlock ("max_pins" 2)
    (tool-text "pin" "text" "first rule")
    (tool-text "pin" "text" "second rule")
    (is-carrying (answer (tool-text "pin" "text" "third rule"))
                 "ERROR" ("already 2 pins"))
    (is (= 2 (length (nodecode-memlock::read-pins))))))

(deftest memlock-cell-without-a-pins-file-the-tool-names-it ()
  (with-session-store ("memlock-nofile")
    (with-cell-stop ((memlock-start))
      (is (nle::find-tool "pin") "the tool is there")
      (is-carrying (answer (tool-text "pin" "text" "always be terse"))
                   "ERROR" ("memlock.pins_file is not set"))
      (is-carrying (listed (handler-case (nodecode-memlock::run-slash "" nil)
                                 (nodecode-memlock::memlock-error (condition) (princ-to-string condition))))
                   ("memlock.pins_file is not set")))))

;;; --- every turn -------------------------------------------------------------------

(deftest memlock-cell-a-pin-opens-the-next-turn ()
  (with-memlock ()
    (ensure-durable-session "memlock-a")
    (tool-text "pin" "text" "Never touch the main branch" "priority" 90)
    (let ((asked '()))
      (with-provider-stub (context
                           (push (memlock-reminders context) asked)
                           (wire-message "assistant" :content "ok"))
        (nle:run "go" :session (nle:intern-session "memlock-a") :command-id "memlock-a-1"))
      (is (= 1 (length asked)) "one request")
      (is-carrying (reminder (first (first asked)))
                   ("[Standing instructions — still active]") ("Never touch the main branch")))))

(deftest memlock-cell-a-pin-outlives-a-restart ()
  (with-session-store ("memlock-restart")
    (let ((stop (memlock-start "pins_file" (memlock-pins-file))))
      (unwind-protect
           (tool-text "pin" "text" "Answer in British English")
        (funcall stop)))
    (with-cell-stop ((memlock-start "pins_file" (memlock-pins-file)))
      (is-carrying (listed (nodecode-memlock::status-text)) ("Answer in British English")))))
