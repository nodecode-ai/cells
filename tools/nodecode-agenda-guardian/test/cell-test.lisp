;;;; cell-test.lisp --- the lifecycle, the five tools, /agenda-guardian, the plan.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved: the cell starts and stops, each tool answers through the
;;;; seam the model calls it by, a task outlives a restart because the file
;;;; holds it, a plan places a task inside the work window and says why, a
;;;; calendar event moves it past the event and the buffer, three finished
;;;; tasks of a kind correct that kind's estimate and the plan uses it, a full
;;;; day says what does not fit instead of overbooking, a rule changed in the
;;;; chat outranks the section's, and a missing folder refuses by name.
;;;; Nothing here reads the operator's own planner: every file is the test's.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defun agenda-folder ()
  "The folder one test's planner file is kept in, inside the test's own home."
  (namestring (nlk:home "agenda-test/")))

(defun agenda-file ()
  (merge-pathnames "agenda.json" (nlk:home "agenda-test/")))

(defun agenda-ics ()
  (namestring (nlk:home "agenda-test/calendar.ics")))

(defun agenda-reset ()
  "One test's planner: no task and no calendar before it runs."
  (dolist (file (list (agenda-file) (agenda-ics)))
    (when (probe-file file) (delete-file file))))

(defun agenda-write-ics (text)
  (ensure-directories-exist (agenda-ics))
  (with-open-file (out (agenda-ics) :direction :output :if-exists :supersede
                                    :if-does-not-exist :create)
    (write-string text out)))

(defun agenda-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun agenda-answer (text)
  "TEXT -- a tool's answer -- decoded, as the model reads it."
  (nlk:decode-json text))

(defun agenda-string (text key)
  (nlk:json-value (agenda-answer text) :string key))

(defun agenda-text (text key)
  "KEY of TEXT, or TEXT itself when it did not decode -- so a refusal shows in the failure."
  (handler-case (nlk:json-value (nlk:decode-json text) :string key)
    (error () text)))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "agenda-guardian" "AGENDA-GUARDIAN-CELL-" :start nodecode-agenda-guardian:start-cell)

(define-cell-lifecycle-tests "agenda-guardian"
  (:config "data_dir" (agenda-folder))
  (:help :agenda-guardian)
  (:command "agenda-guardian")
  (:running (is (nle::find-tool "agenda_add_task") "agenda_add_task is registered")
            (is (nle::find-tool "agenda_tasks"))
            (is (nle::find-tool "agenda_done"))
            (is (nle::find-tool "agenda_plan"))
            (is (nle::find-tool "agenda_rules"))
            (is (equal "03:00" (nodecode-agenda-guardian::clock 180)) "the clock reads minutes as a person writes them"))
  (:stopped (is (null (nle::find-tool "agenda_plan")) "stop unregisters the tools"))
  (:refused ("max_tasks_per_day" 0) ("buffer_min" -1))
  (:idle nodecode-agenda-guardian:agenda-guardian-error
         (nodecode-agenda-guardian::run-command "" nil)))

(deftest agenda-guardian-cell-a-missing-data-dir-refuses-by-name ()
  (is-carrying (refusal (refusal-text nlk:config-refusal (agenda-guardian-start)))
               "data_dir"
               "the folder the planner keeps")
  (is (null (nle::find-tool "agenda_plan")) "a refused start installed nothing"))

;;; --- the file ------------------------------------------------------------------

(deftest agenda-guardian-cell-a-task-outlives-a-restart ()
  (agenda-reset)
  (with-cell-stop ((agenda-guardian-start "data_dir" (agenda-folder)))
    (let ((answer (agenda-answer (agenda-tool "agenda_add_task"
                                               (nlk:json-object "name" "Write the report"
                                                                 "minutes" 30
                                                                 "category" "writing")))))
      (is (eq t (nlk:json-value answer :boolean "ok")) "the tool answers ok")
      (is (equal "t1" (nlk:json-value answer :string "id")) "the first task is t1")
      (is (search "Write the report" (agenda-string (agenda-tool "agenda_tasks" nil) "text")))))
  (with-cell-stop ((agenda-guardian-start "data_dir" (agenda-folder)))
    (is (search "Write the report" (agenda-string (agenda-tool "agenda_tasks" nil) "text")) "a second start reads the same file")))

;;; --- the plan -------------------------------------------------------------------

(deftest agenda-guardian-cell-a-plan-places-a-task-and-says-why ()
  (agenda-reset)
  (with-cell-stop ((agenda-guardian-start "data_dir" (agenda-folder)))
    (agenda-tool "agenda_add_task" (nlk:json-object "name" "Write the report" "minutes" 30))
    (is-carrying (text (agenda-text (agenda-tool "agenda_plan" (nlk:json-object "day" "2030-01-07")) "text"))
      ("09:00-09:30" "the block starts at the work window's edge")
      ("Write the report")
      ("your estimate 30 min" "the block says where its length came from")
      ("priority 2 of 3"))))

(deftest agenda-guardian-cell-a-calendar-event-moves-the-block ()
  (agenda-reset)
  (agenda-write-ics (format nil "BEGIN:VCALENDAR~%BEGIN:VEVENT~%SUMMARY:Standup~%DTSTART:20300107T093000~%DTEND:20300107T100000~%END:VEVENT~%END:VCALENDAR~%"))
  (with-cell-stop ((agenda-guardian-start "data_dir" (agenda-folder) "calendar" (agenda-ics)))
    (agenda-tool "agenda_add_task" (nlk:json-object "name" "Write the report" "minutes" 30))
    (let ((text (agenda-string (agenda-tool "agenda_plan" (nlk:json-object "day" "2030-01-07")) "text")))
      (is (search "Standup" text) "the calendar's event is shown")
      (is (search "10:10-10:40" text) "the block starts after the event and the buffer")
      (is (not (search "09:00-09:30" text)) "and not before it"))))

(deftest agenda-guardian-cell-finished-times-correct-the-estimate ()
  (agenda-reset)
  (with-cell-stop ((agenda-guardian-start "data_dir" (agenda-folder)))
    (dolist (pair '(("Draft" 45) ("Outline" 60) ("Email" 45)))
      (let* ((name (first pair))
             (answer (agenda-answer (agenda-tool "agenda_add_task"
                                                  (nlk:json-object "name" name
                                                                    "minutes" 30
                                                                    "category" "writing"))))
             (id (nlk:json-value answer :string "id")))
        (agenda-tool "agenda_done" (nlk:json-object "id" id "minutes" (second pair)))))
    (agenda-tool "agenda_add_task" (nlk:json-object "name" "Write the report" "minutes" 30 "category" "writing"))
    (let ((text (agenda-string (agenda-tool "agenda_plan" (nlk:json-object "day" "2030-01-07")) "text")))
      (is (search "3 finished of writing run 1.50x" text) "the median error is used and said")
      (is (search "09:00-09:45" text) "a 30-minute estimate becomes a 45-minute block"))))

(deftest agenda-guardian-cell-a-full-day-says-what-does-not-fit ()
  (agenda-reset)
  (with-cell-stop ((agenda-guardian-start "data_dir" (agenda-folder) "max_tasks_per_day" 1))
    (agenda-tool "agenda_add_task" (nlk:json-object "name" "First" "minutes" 30))
    (agenda-tool "agenda_add_task" (nlk:json-object "name" "Second" "minutes" 300))
    (let ((text (agenda-string (agenda-tool "agenda_plan" (nlk:json-object "day" "2030-01-07")) "text")))
      (is (search "First" text) "the day's one task is placed")
      (is (search "Not placed: Second (the cap: 1 task a day)" text) "what the cap leaves out is named, not silently dropped"))))

;;; --- the command -----------------------------------------------------------------

(deftest agenda-guardian-cell-the-command-plans-adds-and-sets-a-rule ()
  (agenda-reset)
  (with-cell-stop ((agenda-guardian-start "data_dir" (agenda-folder)))
    (is (search "Agenda Guardian" (cell-entry "nodecode-agenda-guardian" "agenda-guardian" "")))
    (is (search "Walk the dog" (cell-entry "nodecode-agenda-guardian" "agenda-guardian" "add 30m Walk the dog")) "add takes a length and a name")
    (is (search "09:00-09:30"
                (cell-entry "nodecode-agenda-guardian" "agenda-guardian" "plan 2030-01-07")))
    (is (search "max_tasks_per_day is now" (cell-entry "nodecode-agenda-guardian" "agenda-guardian" "set max_tasks_per_day=2")) "a rule set in the chat")
    (is (search "max_tasks_per_day 2 (changed in the chat)" (cell-entry "nodecode-agenda-guardian" "agenda-guardian" "rules")) "and the rules say where it came from")))

(deftest agenda-guardian-cell-the-rules-tool-changes-a-rule-and-keeps-the-rest ()
  (agenda-reset)
  (with-cell-stop ((agenda-guardian-start "data_dir" (agenda-folder)))
    (agenda-tool "agenda_rules" (nlk:json-object "max_block_min" 30))
    (let ((answer (agenda-answer (agenda-tool "agenda_rules" nil)))
          (rules (nlk:json-value (agenda-answer (agenda-tool "agenda_rules" nil)) :array "rules")))
      (is (eq t (nlk:json-value answer :boolean "ok")))
      (is (= 8 (length rules)) "every rule is reported")
      (is (search "max_block_min 30 (changed in the chat)"
                  (cell-entry "nodecode-agenda-guardian" "agenda-guardian" "rules")))
      (is (search "work_hours 09:00-18:00 (from the section)" (cell-entry "nodecode-agenda-guardian" "agenda-guardian" "rules")) "the rest still come from the section"))))
