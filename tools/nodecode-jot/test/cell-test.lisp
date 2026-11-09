;;;; cell-test.lisp --- the lifecycle, the six tools, /jot, and the library file.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved: the cell starts and stops, each tool answers through the
;;;; seam the model calls it by, a stale revision is a repair and not a write,
;;;; an edit that matches nothing changes nothing, one checklist marker moves
;;;; without touching the rest, a trashed note leaves the list, the AI switch
;;;; refuses while it is off, and the library outlives a restart because the
;;;; file in the folder holds it. Every call runs against a library file in
;;;; the test's own home, so nothing here reads the operator's notes.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defun jot-test-folder ()
  "The folder one test's library is kept in, inside the test's own home."
  (namestring (nlk:home "jot-test/")))

(defun jot-library-file ()
  (merge-pathnames "library.json" (nlk:home "jot-test/")))

(defun jot-empty-library ()
  "One test's library: the file removed, so the test starts from none."
  (when (probe-file (jot-library-file)) (delete-file (jot-library-file))))

(defun jot-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun jot-answer (text)
  "TEXT -- a tool's answer -- decoded, as the model reads it."
  (nlk:decode-json text))

(defun jot-markdown (id)
  "The Markdown of note ID, as jot_read answers it."
  (nlk:json-value (jot-answer (jot-tool "jot_read" (nlk:json-object "id" id)))
                  :string "markdown"))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "jot" "JOT-CELL-" :start nodecode-jot:start-cell)

(define-cell-lifecycle-tests "jot"
  (:config "data_dir" (jot-test-folder))
  (:help :jot)
  (:command "jot")
  (:running (is (nle::find-tool "jot_list") "jot_list is registered")
            (is (nle::find-tool "jot_read"))
            (is (nle::find-tool "jot_create"))
            (is (nle::find-tool "jot_update"))
            (is (nle::find-tool "jot_set_task"))
            (is (nle::find-tool "jot_delete"))
            (is (pathnamep (nodecode-jot::library-file)) "the library file is resolved")
            (is (eq t (nodecode-jot::setting :ai)) "collaboration is on unless the section says otherwise"))
  (:stopped (is (null (nle::find-tool "jot_list")) "stop unregisters the tools"))
  (:refused ("ai" t))
  (:idle nodecode-jot:jot-error (nodecode-jot::run-command "" nil)))

(deftest jot-cell-a-missing-data-dir-refuses-by-name ()
  (is-carrying (refusal (refusal-text nlk:config-refusal (jot-start "ai" t)))
               "data_dir"
               "the folder the notes are kept in")
  (is (null (nle::find-tool "jot_list")) "a refused start installed nothing"))

;;; --- the tools ------------------------------------------------------------------

(deftest jot-cell-a-created-note-is-readable-and-listed ()
  (jot-empty-library)
  (with-cell-stop ((jot-start "data_dir" (jot-test-folder)))
    (let* ((created (jot-answer
                      (jot-tool "jot_create"
                                 (nlk:json-object
                                  "folderId" "Work"
                                  "title" "Release checklist"
                                  "text" (format nil "Ship it~%- [ ] tag the release~%- [x] write the notes")))))
           (id (nlk:json-value created :string "id")))
      (is (stringp id) "the create answers the note's id")
      (is (= 1 (nlk:json-value created :integer "revision")) "a new note is revision 1")
      (is (search "tag the release" (nlk:json-value created :string "excerpt")))
      (let ((read (jot-answer (jot-tool "jot_read" (nlk:json-object "id" id)))))
        (is (search "Ship it" (nlk:json-value read :string "markdown")) "the note comes back as Markdown")
        (is (eq t (nlk:json-value read :boolean "replaceKeepsFormatting")) "Markdown holds it whole")
        (let ((tasks (nlk:json-value read :array "tasks")))
          (is (= 2 (length tasks)) "both checklist items are numbered")
          (is (equal "tag the release" (nlk:json-value (aref tasks 0) :string "text")))
          (is (eq nil (nlk:json-value (aref tasks 0) :boolean "checked")))
          (is (eq t (nlk:json-value (aref tasks 1) :boolean "checked")))))
      (let ((listed (jot-answer (jot-tool "jot_list" (nlk:json-object "query" "release")))))
        (is (= 1 (nlk:json-value listed :integer "total")) "the search finds it by title")
        (is (equal id (nlk:json-value (aref (nlk:json-value listed :array "notes") 0) :string "id"))))
      (let ((folders (nlk:json-value (jot-answer (jot-tool "jot_list" (nlk:json-object))) :array "folders")))
        (is (= 1 (length folders)) "an invented folder name is reported back")))))

(deftest jot-cell-edits-apply-all-or-none-against-the-revision-read ()
  (jot-empty-library)
  (with-cell-stop ((jot-start "data_dir" (jot-test-folder)))
    (let* ((created (jot-answer (jot-tool "jot_create"
                                           (nlk:json-object "title" "Plan"
                                                             "text" "Tuesday: walk the dog"))))
           (id (nlk:json-value created :string "id")))
      (let ((updated (jot-answer
                       (jot-tool "jot_update"
                                  (nlk:json-object
                                   "id" id "revision" 1
                                   "edits" (vector (nlk:json-object "find" "walk the dog"
                                                                     "replace" "walk the cat")))))))
        (is (= 2 (nlk:json-value updated :integer "revision")) "an edit moves the revision on")
        (is (= 1 (nlk:json-value updated :integer "edited")) "and says how many edits it applied"))
      (is (equal "Tuesday: walk the cat" (jot-markdown id)) "the words the edit named changed")
      (is-carrying (answer (jot-tool "jot_update"
                                      (nlk:json-object "id" id "revision" 1
                                                        "text" "gone" "allowFormattingLoss" t)))
                   "ERROR: jot:"
                   "Note changed; expected revision 1, current revision 2"
                   (:absent "gone" "a stale revision wrote nothing"))
      (is-carrying (answer (jot-tool "jot_update"
                                      (nlk:json-object
                                       "id" id "revision" 2
                                       "edits" (vector (nlk:json-object "find" "walk the horse"
                                                                         "replace" "walk the cow")))))
                   "ERROR: jot:"
                   "Edit 1: text not found"
                   "Match the note's visible text")
      (is (equal "Tuesday: walk the cat" (jot-markdown id)) "an edit that matched nothing changed nothing"))))

(deftest jot-cell-a-task-toggles-and-a-note-goes-to-trash ()
  (jot-empty-library)
  (with-cell-stop ((jot-start "data_dir" (jot-test-folder)))
    (let* ((created (jot-answer (jot-tool "jot_create"
                                           (nlk:json-object "title" "Shopping"
                                                             "text" (format nil "- [ ] milk~%- [ ] bread")))))
           (id (nlk:json-value created :string "id")))
      (let ((ticked (jot-answer (jot-tool "jot_set_task"
                                           (nlk:json-object "id" id "revision" 1
                                                             "index" 2 "checked" t)))))
        (let ((task (nlk:json-value ticked :object "task")))
          (is (eq t (nlk:json-value task :boolean "checked")) "it answers the item it changed")))
      (is (equal (format nil "- [ ] milk~%- [x] bread") (jot-markdown id)) "one marker changed, nothing else")
      (is-carrying (answer (jot-tool "jot_set_task"
                                      (nlk:json-object "id" id "revision" 2 "index" 9 "checked" t)))
                   "Task 9 was not found"
                   "2 checklist items")
      (let ((trashed (jot-answer (jot-tool "jot_delete" (nlk:json-object "id" id "revision" 2)))))
        (is (stringp (nlk:json-value trashed :string "deletedAt")) "the delete says when it went"))
      (is-carrying (answer (jot-tool "jot_read" (nlk:json-object "id" id))) "in Trash")
      (let ((listed (jot-answer (jot-tool "jot_list" (nlk:json-object)))))
        (is (= 0 (nlk:json-value listed :integer "total")) "a trashed note is not listed")))))

;;; --- /jot and the switch ----------------------------------------------------------

(deftest jot-cell-the-switch-refuses-and-the-command-shows-the-library ()
  (jot-empty-library)
  (with-cell-stop ((jot-start "data_dir" (jot-test-folder)))
    (jot-tool "jot_create" (nlk:json-object "title" "Ideas" "text" "a note about tea"))
    (is-carrying (answer (cell-entry "nodecode-jot" "jot" ""))
                 "Jot: AI collaboration on"
                 "1 live note"
                 "Recent: Ideas")
    (is-carrying (answer (cell-entry "nodecode-jot" "jot" "ai off")) "AI collaboration is off")
    (is-carrying (answer (jot-tool "jot_list" (nlk:json-object)))
                 "ERROR: jot:"
                 "AI collaboration is off"
                 "/jot ai on")
    (is-carrying (answer (jot-tool "jot_create" (nlk:json-object "title" "Late" "text" "x")))
                 ("AI collaboration is off" "a create is refused too"))
    (is-carrying (answer (cell-entry "nodecode-jot" "jot" "ai on")) "AI collaboration is on")
    (is-carrying (answer (cell-entry "nodecode-jot" "jot" "tea"))
                 ("1 note for \"tea\"" "the search counts the one match")
                 "Ideas")))

(deftest jot-cell-the-library-outlives-a-restart ()
  (jot-empty-library)
  (with-cell-stop ((jot-start "data_dir" (jot-test-folder)))
    (jot-tool "jot_create" (nlk:json-object "title" "Kept" "text" "still here")))
  (with-cell-stop ((jot-start "data_dir" (jot-test-folder)))
    (is-carrying (answer (jot-tool "jot_list" (nlk:json-object))) "Kept" "still here")))

