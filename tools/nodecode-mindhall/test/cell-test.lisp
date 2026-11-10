;;;; cell-test.lisp --- lifecycle, the five tools, the block every turn reads, /mindhall.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: that a start installs the
;;;; five tools, the standing section, /mindhall and the manual and a stop takes
;;;; every bit of it back; that a section that cannot be honoured refuses and
;;;; leaves nothing behind; that the five tools read and write one file, sign
;;;; the change log, move a date instead of copying it and refuse what they
;;;; cannot do; that the block a turn reads carries the mind and an empty one
;;;; says so; that /mindhall answers rooms; and that the mind survives a
;;;; restart because the file is the store.
;;;;
;;;; Offline: each test reads and writes a mind file of its own under the
;;;; scratch home, and nothing dials a network.

(in-package #:nodecode.test)

(define-test-slice "mindhall" "MINDHALL-" :start nodecode-mindhall:start-cell)

;;; --- fixtures ------------------------------------------------------------------

(defvar *mindhall-scratch* 0
  "A counter, so each test gets a mind file of its own.")

(defun mindhall-store-file ()
  "The path of a mind file that no other test writes."
  (namestring (merge-pathnames
               (format nil "mindhall-~d-~d.json" (get-universal-time) (incf *mindhall-scratch*))
               (user-homedir-pathname))))

(defun mindhall-call (name &rest pairs)
  "Run the registered tool NAME the way dispatch does: its arguments as the
JSON object a provider sends, decoded through the tool's own contract."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defun mindhall-block ()
  "The block the standing harness sections carry while the cell runs."
  (cdr (assoc "mindhall" (nle::read-harness-sections "session-1") :test #'string=)))

(defun mindhall-lines-with (text needle)
  (count-if (lambda (line) (search needle line))
            (uiop:split-string (or text "") :separator '(#\Newline))))

(defmacro with-mind ((&rest pairs) &body body)
  "BODY inside a started cell, on the mind file the pairs name or a fresh one."
  `(with-cell-stop ((mindhall-start "agent" "Test Agent" "store" (mindhall-store-file)
                     ,@pairs))
     ,@body))

;;; --- lifecycle ------------------------------------------------------------------

(define-cell-lifecycle-tests "mindhall"
  (:config "agent" "Test Agent" "store" (mindhall-store-file))
  (:hooks 'nle::read-harness-sections)
  (:help :mindhall)
  (:command "mindhall")
  (:refused ("max_chars" 10) ("days_ahead" 0) ("max_text" 2))
  (:running
   (is (nle::find-tool "mindhall_read") "mindhall_read is registered")
   (is (nle::find-tool "mindhall_add_note"))
   (is (nle::find-tool "mindhall_update_work"))
   (is (nle::find-tool "mindhall_set_date"))
   (is (nle::find-tool "mindhall_remove"))
   (is (equal "Test Agent" (nodecode-mindhall::agent-name)) "changes are signed with the section's name")
   (is (search "Mindhall is empty" (mindhall-block)) "an empty mind still asks to be written to"))
  (:idle mindhall:mindhall-error
         (mindhall:add-note "a note")
         (mindhall:read-mind "about"))
  (:stopped
   (is (null (nodecode-mindhall::standing-section
              (lambda (session) (declare (ignore session)) nil) "s1")) "and the block is gone from the standing sections")))

;;; --- the five tools ------------------------------------------------------------

(deftest mindhall-cell-the-tools-write-read-and-sign-one-mind ()
  (with-mind ()
    (is (search "Saved as note 1: I drink green tea"
                (mindhall-call "mindhall_add_note" "note" "I drink green tea")) "a note is saved and numbered")
    (is (search "I drink green tea" (mindhall-call "mindhall_read" "room" "about")) "and reads back from About you")
    (let ((log (mindhall-call "mindhall_read" "room" "changes")))
      (is (search "Test Agent: Added note: I drink green tea" log) "the change is signed with the agent's name and nothing else's"))
    (is (search "Added Mindhall hackathon entry (Active)"
                (mindhall-call "mindhall_update_work" "name" "Mindhall hackathon entry"
                               "status" "Active" "next_step" "ship it")) "work is added with a status and one next step")
    (is (search "Updated Mindhall hackathon entry: status Active to Done"
                (mindhall-call "mindhall_update_work" "name" "mindhall HACKATHON entry"
                               "status" "Done")) "an existing name matches whatever its case, and the change is logged")
    (is (search "Done" (mindhall-call "mindhall_read" "room" "work")))
    (is (search "Removed note 1: I drink green tea"
                (mindhall-call "mindhall_remove" "room" "about" "id" 1)))
    (is (search "About you is empty" (mindhall-call "mindhall_read" "room" "about")))))

(deftest mindhall-cell-a-date-with-the-same-name-moves-instead-of-copying ()
  (with-mind ()
    (is (search "Added Launch on 2026-11-20 (date 1)."
                (mindhall-call "mindhall_set_date" "what" "Launch" "day" "2026-11-20")))
    (is (search "Moved Launch from 2026-11-20 to 2026-11-22"
                (mindhall-call "mindhall_set_date" "what" "launch" "day" "2026-11-22")) "the same name in another case moves the one date")
    (let ((dates (mindhall-call "mindhall_read" "room" "dates" "include_past" t)))
      (is (= 1 (mindhall-lines-with dates "Launch")) "one Launch, not two")
      (is (search "2026-11-22" dates) "on the new day"))
    (is (search "is already on 2026-11-22"
                (mindhall-call "mindhall_set_date" "what" "Launch" "day" "2026-11-22")) "saying it twice changes nothing")
    (is (search "Removed Launch (2026-11-22)"
                (mindhall-call "mindhall_remove" "room" "dates" "id" 1)))))

(deftest mindhall-cell-a-request-it-cannot-honour-refuses-and-writes-nothing ()
  (let ((file (mindhall-store-file)))
    (with-cell-stop ((mindhall-start "agent" "Test Agent" "store" file))
      (is (search "ERROR: mindhall: Dates are written as YYYY-MM-DD"
                  (mindhall-call "mindhall_set_date" "what" "Launch" "day" "next friday")) "a day that is not a day is refused in words")
      (is (search "ERROR: mindhall: 2026-02-31 is not a day on the calendar"
                  (mindhall-call "mindhall_set_date" "what" "Launch" "day" "2026-02-31")) "and so is a day that does not exist")
      (is (search "ERROR: mindhall: The note is empty"
                  (mindhall-call "mindhall_add_note" "note" "   ")))
      (is (search "ERROR: mindhall: There is no note with id 7"
                  (mindhall-call "mindhall_remove" "room" "about" "id" 7)))
      (is (signals-error nlk:contract-refusal
                         (mindhall-call "mindhall_update_work" "name" "x" "status" "Someday")) "a status outside the set is refused before the body runs")
      (is (search "ERROR: mindhall: Status must be one of: Active, Parked, Done"
                  (handler-case (mindhall:update-work "x" :status "Someday")
                    (mindhall:mindhall-error (condition) (format nil "ERROR: mindhall: ~a" condition)))) "and the body refuses one the contract could not see")
      (is (signals-error nlk:contract-refusal (mindhall-call "mindhall_add_note")) "a call without its required member never reaches the body")
      (with-open-file (out file :direction :output :if-exists :overwrite :if-does-not-exist :create)
        (write-line "the mind, written by hand" out))
      (is (search "ERROR: mindhall:" (mindhall-call "mindhall_read" "room" "about")) "a file that is not a mind refuses to be read")
      (is (search "written by hand" (uiop:read-file-string file)) "and is left exactly as it was, never overwritten"))))

;;; --- the block a turn reads ----------------------------------------------------

(deftest mindhall-cell-the-standing-block-carries-the-mind ()
  (with-mind ()
    (mindhall-call "mindhall_add_note" "note" "Prefers short answers")
    (mindhall-call "mindhall_update_work" "name" "Mindhall" "status" "Active" "next_step" "port it")
    (mindhall-call "mindhall_set_date" "what" "Launch" "day" (nodecode-mindhall::horizon 3))
    (mindhall-call "mindhall_set_date" "what" "Far away" "day" (nodecode-mindhall::horizon 60))
    (let ((block (mindhall-block)))
      (is (search "Mindhall is right" block) "the block says which one wins")
      (is (search "Prefers short answers" block) "about you")
      (is (search "Mindhall [Active, next: port it]" block) "work and its next step")
      (is (search "Launch" block) "a date inside the window")
      (is (not (search (format nil "  ~a  Far away" (nodecode-mindhall::horizon 60)) block)) "a date still far off stays out of the dates section, though the log names it")
      (is (not (search "Done" block)) "and work that is done is left out"))
    (mindhall-call "mindhall_update_work" "name" "Mindhall" "status" "Done")
    (is (not (search "Mindhall [" (mindhall-block))) "the block is read fresh each turn")))

(deftest mindhall-cell-inject-false-keeps-the-block-out-of-the-turn ()
  (with-cell-stop ((mindhall-start "agent" "Test Agent" "store" (mindhall-store-file)
                    "inject" nil))
    (mindhall-call "mindhall_add_note" "note" "Prefers short answers")
    (is (null (mindhall-block)) "nothing is put in front of the turn")
    (is (search "Prefers short answers" (mindhall-call "mindhall_read" "room" "about")) "while the tools still read and write the mind")))

;;; --- /mindhall -----------------------------------------------------------------

(deftest mindhall-cell-the-command-shows-the-mind-and-its-rooms ()
  (with-mind ()
    (mindhall-call "mindhall_add_note" "note" "Works in the morning")
    (let ((answer (nodecode-mindhall::run-command "" nil)))
      (is (search "About you" answer))
      (is (search "Works in the morning" answer))
      (is (search "Changes sign as Test Agent" answer) "it says who signs the changes"))
    (is (search "Works in the morning" (nodecode-mindhall::run-command "about" nil)) "/mindhall about shows that room")
    (is (search "Changes sign as Test Agent" (nodecode-mindhall::run-command "nonsense" nil)) "an argument it does not know still shows the mind, and how to use it")))

;;; --- the file is the store -------------------------------------------------------

(deftest mindhall-cell-the-mind-survives-a-restart-and-a-second-agent ()
  (let ((file (mindhall-store-file)))
    (with-cell-stop ((mindhall-start "agent" "First Agent" "store" file))
      (mindhall-call "mindhall_add_note" "note" "Runs a Friday demo")
      (mindhall-call "mindhall_set_date" "what" "Demo" "day" "2026-12-04"))
    (with-cell-stop ((mindhall-start "agent" "Second Agent" "store" file))
      (is (search "Runs a Friday demo" (mindhall-call "mindhall_read" "room" "about")) "a restarted cell reads the same mind")
      (mindhall-call "mindhall_set_date" "what" "Demo" "day" "2026-12-11")
      (let ((log (mindhall-call "mindhall_read" "room" "changes")))
        (is (search "First Agent: Added note" log) "the first agent's change is still signed")
        (is (search "Second Agent: Moved Demo from 2026-12-04 to 2026-12-11" log) "and the second agent signs its own")))))
