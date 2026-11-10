;;;; cell-test.lisp --- the lifecycle, the four tools, /long-conversations, the delivery.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved: the cell starts and stops; each tool answers through
;;;; the seam the model calls it by; a saved brief is a file, so it outlives
;;;; a restart; a brief saved in one session reaches another session's first
;;;; request exactly once; /long-conversations lists, shows and prunes; a missing
;;;; section refuses by naming the member. Every call runs against a folder
;;;; inside the test's own home, so nothing here reads the operator's briefs,
;;;; and no test dials anything: a brief is a file, not a service.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defun handoff-test-folder ()
  "The folder one test's briefs are kept in, inside the test's own home."
  (namestring (nlk:home "handoff-test/")))

(defun handoff-empty ()
  "One test's folder: every brief and the marker removed, so it starts from none."
  (dolist (file (directory (merge-pathnames "*.json" (handoff-test-folder))))
    (ignore-errors (delete-file file))))

(defun handoff-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun handoff-answer (text)
  "TEXT -- a tool's answer -- decoded, as the model reads it; a refusal says so."
  (handler-case (nlk:decode-json text)
    (error (condition) (error "~a: ~a" condition text))))

(defun handoff-session (name)
  "NAME as a session id, the session created if the store has none by it."
  (ensure-durable-session name))

(defun handoff-save (&rest pairs)
  "Save a brief through the tool seam. => its id."
  (nlk:json-value (handoff-answer (handoff-tool "handoff_save" (apply #'nlk:make-json-object pairs)))
                  :string "id"))

(defun handoff-turn (session)
  "A turn admitted on SESSION, as a model request would admit one."
  (let ((command (nlk:make-durable-id "command")))
    (nlk:admit-turn session command "go"
                    :invocation (nlk:default-invocation "turn.start" command
                                                         :source "gateway" :id nil))))

(defun handoff-told (session)
  "Every reminder text SESSION's turns were told, oldest first."
  (loop for payload in (nlk:events :session-id session :kind "turn.context.prompt" :as :payloads)
        for text = (nlk:json-value payload :string "text")
        when text collect text))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "long-conversations" "LONG-CONVERSATIONS-CELL-" :start nodecode-long-conversations:start-cell)

(define-cell-lifecycle-tests "long-conversations"
  (:config "data_dir" (handoff-test-folder))
  (:help :long-conversations)
  (:command "long-conversations")
  (:running (is (nle::find-tool "handoff_save") "handoff_save is registered")
            (is (nle::find-tool "handoff_list"))
            (is (nle::find-tool "handoff_load"))
            (is (nle::find-tool "handoff_prune"))
            (is (pathnamep (nodecode-long-conversations::brief-file "any")) "the brief folder is resolved")
            (is (eq t (nodecode-long-conversations::setting :pending)) "a saved brief is handed on unless the section says otherwise"))
  (:stopped (is (null (nle::find-tool "handoff_save")) "stop unregisters the tools"))
  (:refused ("pending" t))
  (:idle nodecode-long-conversations:long-conversations-error (nodecode-long-conversations::run-command "" nil)))

(deftest long-conversations-cell-a-missing-data-dir-refuses-by-name ()
  (is-carrying (refusal (refusal-text nlk:config-refusal (long-conversations-start "pending" t)))
               "data_dir"
               "the folder the briefs are kept in")
  (is (null (nle::find-tool "handoff_save")) "a refused start installed nothing"))

;;; --- the tools ------------------------------------------------------------------

(deftest long-conversations-cell-a-saved-brief-is-listed-and-loaded ()
  (handoff-empty)
  (with-cell-stop ((long-conversations-start "data_dir" (handoff-test-folder)))
    (let ((id (handoff-save "topic" "Ship 1.2"
                             "work_in_progress" "the changelog is half written"
                             "decisions_made" (vector "keep the old flag"
                                                       "cut on Thursday")
                             "active_state_files" (vector "release notes draft")
                             "dead_ends" (vector "rewriting the loader")
                             "next_step" "finish the changelog")))
      (is (stringp id) (format nil "the save answers the brief's id (~a)" (handoff-tool "handoff_list" (nlk:json-object))))
      (let ((listed (handoff-answer (handoff-tool "handoff_list" (nlk:json-object)))))
        (is (= 1 (nlk:json-value listed :integer "count")) "the brief is listed")
        (let ((row (aref (nlk:json-value listed :array "briefs") 0)))
          (is (equal "Ship 1.2" (nlk:json-value row :string "topic")))
          (is (equal id (nlk:json-value row :string "id")))))
      (let ((loaded (handoff-answer (handoff-tool "handoff_load" (nlk:json-object "id" id)))))
        (is-carrying (context (nlk:json-value loaded :string "context"))
                     "Ship 1.2"
                     "the changelog is half written"
                     "keep the old flag"
                     "rewriting the loader"
                     "finish the changelog"
                     "not an instruction"))
      (let ((latest (handoff-answer (handoff-tool "handoff_load" (nlk:json-object "id" "latest")))))
        (is (equal id (nlk:json-value latest :string "id")) "\"latest\" answers the newest brief"))
      (is-carrying (refused (handoff-tool "handoff_load" (nlk:json-object "id" "no-such-brief")))
                   "ERROR: long-conversations:"
                   "no brief no-such-brief"
                   "handoff_list"))))

(deftest long-conversations-cell-a-brief-outlives-a-restart ()
  (handoff-empty)
  (let ((id (with-cell-stop ((long-conversations-start "data_dir" (handoff-test-folder)))
              (handoff-save "topic" "Migration half done"))))
    ;; nothing of the cell is running; the file is what a new process reads
    (is (null (nle::find-tool "handoff_list")) "the tools are gone between starts")
    (with-cell-stop ((long-conversations-start "data_dir" (handoff-test-folder)))
      (let* ((listed (handoff-answer (handoff-tool "handoff_list" (nlk:json-object))))
             (newest (nlk:json-value (aref (nlk:json-value listed :array "briefs") 0) :string "id")))
        (is (equal id newest) "the brief written before the restart is still there")))))

(deftest long-conversations-cell-prune-removes-only-the-old-briefs ()
  (handoff-empty)
  (with-cell-stop ((long-conversations-start "data_dir" (handoff-test-folder) "keep_days" 30))
    (handoff-save "topic" "Fresh")
    (let ((pruned (handoff-answer (handoff-tool "handoff_prune" (nlk:json-object "older_than_days" 0)))))
      (is (= 1 (nlk:json-value pruned :integer "pruned")) "a day count of 0 takes every brief")
      (is (= 0 (nlk:json-value pruned :integer "kept")))
      (let ((left (handoff-answer (handoff-tool "handoff_list" (nlk:json-object)))))
        (is (= 0 (nlk:json-value left :integer "count")) "the folder is empty again")))
    (handoff-save "topic" "Kept")
    (let ((pruned (handoff-answer (handoff-tool "handoff_prune" (nlk:json-object "older_than_days" 30)))))
      (is (= 0 (nlk:json-value pruned :integer "pruned")) "a brief younger than the window stays")
      (is (= 1 (nlk:json-value pruned :integer "kept"))))))

(deftest long-conversations-cell-the-section-prunes-old-briefs-at-start ()
  (handoff-empty)
  (with-cell-stop ((long-conversations-start "data_dir" (handoff-test-folder) "keep_days" 1))
    (handoff-save "topic" "One brief")
    (with-cell-stop ((long-conversations-start "data_dir" (handoff-test-folder) "keep_days" 1 "pending" nil))
      (let ((left (handoff-answer (handoff-tool "handoff_list" (nlk:json-object)))))
        (is (= 1 (nlk:json-value left :integer "count")) "a brief inside keep_days survives a restart")))))

;;; --- the delivery ----------------------------------------------------------------

(deftest long-conversations-cell-a-brief-reaches-another-session-once ()
  (handoff-empty)
  (with-temp-store ()
   (with-cell-stop ((long-conversations-start "data_dir" (handoff-test-folder)))
    (let* ((id (handoff-save "topic" "Cut the release" "next_step" "tag it"))
           (writer (handoff-session "handoff-writer"))
           (reader (handoff-session "handoff-reader")))
      (declare (ignore writer))
      ;; the marker names the session that saved it, so another session's
      ;; first request is what reads the brief
      (nodecode-long-conversations::write-pending id "handoff-writer")
      (let ((turn (handoff-turn reader)))
        (nodecode-long-conversations::deliver (list :session-id reader :turn turn :round 0))
        (nlk:complete-turn turn nil))
      (let ((told (handoff-told reader)))
        (is (= 1 (length told)) "the next session is told once")
        (is-carrying (text (format nil "~{~a~%~}" told))
                     "Cut the release"
                     "tag it"
                     "Continuation brief"))
      ;; the marker is gone, so the session after it is told nothing
      (let ((turn (handoff-turn reader)))
        (nodecode-long-conversations::deliver (list :session-id reader :turn turn :round 0))
        (nlk:complete-turn turn nil))
      (is (= 1 (length (handoff-told reader))) "a brief is delivered once")
      ;; and the session that saved it is never told its own brief
      (handoff-save "topic" "Second brief")
      (nodecode-long-conversations::write-pending (handoff-save "topic" "Saved here") reader)
      (let ((turn (handoff-turn reader)))
        (nodecode-long-conversations::deliver (list :session-id reader :turn turn :round 0))
        (nlk:complete-turn turn nil))
      (is (= 1 (length (handoff-told reader))) "the session that saved it reads nothing back")))))

;;; --- /long-conversations --------------------------------------------------------------------

(deftest long-conversations-cell-the-command-lists-shows-and-saves ()
  (handoff-empty)
  (with-cell-stop ((long-conversations-start "data_dir" (handoff-test-folder)))
    (let* ((id (handoff-save "topic" "Rotate the keys" "decisions_made" (vector "monthly")))
           (bare (multiple-value-list (nodecode-long-conversations::run-command "" "s1")))
           (shown (nodecode-long-conversations::run-command (format nil "show ~a" id) "s1"))
           (latest (nodecode-long-conversations::run-command "latest" "s1"))
           (saved (nodecode-long-conversations::run-command "save Winter planning" "s1")))
      (is-carrying (head (first bare)) "long-conversations: 1 brief" "Rotate the keys")
      (is (not (null (second bare))) "bare /long-conversations offers the briefs as rows")
      (is-carrying shown "Rotate the keys" "monthly" "Continuation brief")
      (is-carrying latest "Rotate the keys")
      (is-carrying saved "Winter planning" "handoff_save")
      (is (= 2 (length (nodecode-long-conversations::brief-records))) "a saved line becomes a brief")))
  (handoff-empty))
