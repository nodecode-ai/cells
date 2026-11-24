;;;; cell-test.lisp --- the lifecycle, the four tools, /fieldnotes, and the store.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved: the cell starts and stops, each tool answers through the
;;;; seam the model calls it by, a note is a file in the store and the index
;;;; follows it, a patch check reads the code tree and says OK, MISSING or
;;;; UPSTREAMED by the signs alone, the doctor counts what is there, a patch
;;;; note without checks is refused, the AI switch refuses the tools without
;;;; changing what the command shows, and the notes outlive a restart because
;;;; the files in the store hold them. Every call runs against a store and a
;;;; code tree in the test's own home, so nothing here reads the operator's.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defun field-notes-test-store ()
  "The folder one test's notes are kept in, inside the test's own home."
  (namestring (nlk:home "field-notes-test/")))

(defun field-notes-test-core ()
  "The code tree one test's patch checks read, inside the test's own home."
  (namestring (nlk:home "field-notes-test-core/")))

(defun field-notes-reset ()
  "The test's store and code tree removed, so the test starts from none."
  (dolist (folder (list (field-notes-test-store) (field-notes-test-core)))
    (let ((path (uiop:ensure-directory-pathname folder)))
      (when (uiop:directory-exists-p path)
        (uiop:delete-directory-tree path :validate t)))))

(defmacro field-notes-fixture (&body body)
  `(progn (field-notes-reset) ,@body))

(defun field-notes-start* (&rest pairs)
  "START-CELL with the test's store and code tree, and PAIRS after them."
  (apply #'field-notes-start
         (append (list "store_dir" (field-notes-test-store)
                       "core_root" (field-notes-test-core))
                 pairs)))

(defun field-notes-core-file (name text)
  "Write TEXT as NAME inside the test's code tree."
  (let ((file (merge-pathnames name (uiop:ensure-directory-pathname (field-notes-test-core)))))
    (ensure-directories-exist file)
    (with-open-file (out file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string text out))
    file))

(defun field-notes-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun field-notes-json (text)
  "TEXT -- a tool's answer -- decoded, as the model reads it."
  (nlk:decode-json text))

(defun field-notes-checks (marker upstream)
  "A checks object: one edition with MARKER's target and UPSTREAM's fix."
  (nlk:json-object
   "editions"
   (vector (nlk:json-object
            "when" (nlk:json-object "min_version" "0.1.0")
            "targets" (vector (nlk:json-object
                               "file" "core.py"
                               "contains" (vector marker)
                               "not_contains" (vector "old_line()")))
            "upstream" (vector (nlk:json-object
                                "file" "core.py"
                                "contains" (vector upstream)))))))

(defun field-notes-patch-status (answer id)
  "The status the check ANSWER gives patch ID."
  (loop for patch across (nlk:json-value answer :array "patches")
        when (equal id (nlk:json-value patch :string "id"))
          return (nlk:json-value patch :string "status")))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "field-notes" "FIELD-NOTES-CELL-" :start nodecode-field-notes:start-cell)

(define-cell-lifecycle-tests "field-notes"
  (:config "store_dir" (field-notes-test-store)
           "core_root" (field-notes-test-core))
  (:help :field-notes)
  (:command "fieldnotes")
  (:fixture field-notes-fixture)
  (:running (is (nle::find-tool "fieldnote_search") "fieldnote_search is registered")
            (is (nle::find-tool "fieldnote_new"))
            (is (nle::find-tool "fieldnote_check"))
            (is (nle::find-tool "fieldnote_doctor"))
            (is (pathnamep (nodecode-field-notes::notes-folder)) "the notes folder is resolved")
            (is (probe-file (nodecode-field-notes::index-path)) "the index is written at start"))
  (:stopped (is (null (nle::find-tool "fieldnote_search")) "stop unregisters the tools"))
  (:refused ())
  (:idle nodecode-field-notes:field-notes-error (nodecode-field-notes::run-command "" nil)))

(deftest field-notes-cell-a-missing-store-dir-refuses-by-name ()
  (field-notes-reset)
  (is-carrying (refusal (refusal-text nlk:config-refusal (field-notes-start "core_root" "")))
               "store_dir"
               "the folder the notes are kept in")
  (is (null (nle::find-tool "fieldnote_search")) "a refused start installed nothing"))

;;; --- the notes ------------------------------------------------------------------

(deftest field-notes-cell-a-note-is-a-file-and-the-index-follows ()
  (with-cell-stop ((field-notes-start*))
    (let* ((answer (field-notes-json
                    (field-notes-tool
                     "fieldnote_new"
                     (nlk:json-object
                      "title" "A cron job fires twice after an update"
                      "summary" "The scheduler keeps the old unit and the new one; disable the stale unit by name."
                      "type" "pitfall"
                      "area" "cron"
                      "tags" "cron scheduler update"
                      "body" "Symptom: two runs. Root cause: two units. Fix: disable the stale one."))))
           (id (nlk:json-value answer :string "id")))
      (is (stringp id) "the create answers the note's id")
      (is (probe-file (nodecode-field-notes::note-path id)) "the note is a file in the store")
      (is (search "A cron job fires twice" (uiop:read-file-string (nodecode-field-notes::index-path))) "the index carries the note")
      (let ((found (field-notes-json
                    (field-notes-tool "fieldnote_search" (nlk:json-object "query" "scheduler")))))
        (is (= 1 (nlk:json-value found :integer "total")) "the search finds it by a body word")
        (is (equal id (nlk:json-value (aref (nlk:json-value found :array "notes") 0) :string "id"))))
      (let ((notes (field-notes-json
                    (field-notes-tool "fieldnote_search" (nlk:json-object)))))
        (is (= 1 (nlk:json-value notes :integer "total")) "an empty query lists the store")))))

(defun field-notes-carrying (text &rest needles)
  "T when TEXT carries every NEEDLE."
  (every (lambda (needle) (and (search needle text) t)) needles))

(deftest field-notes-cell-a-note-needs-a-title-and-a-summary ()
  (with-cell-stop ((field-notes-start*))
    (is (field-notes-carrying
         (field-notes-tool "fieldnote_new" (nlk:json-object "summary" "one line"))
         "ERROR: field-notes:" "title is the note's claim"))
    (is (field-notes-carrying
         (field-notes-tool "fieldnote_new" (nlk:json-object "title" "A claim"))
         "summary is one dense line"))
    (is (field-notes-carrying
         (field-notes-tool "fieldnote_new"
                            (nlk:json-object "title" "A claim" "summary" "one line"
                                              "type" "patch"))
         "a patch note needs patch_kind"))
    (is (= 0 (nlk:json-value (field-notes-json
                              (field-notes-tool "fieldnote_search" (nlk:json-object)))
                             :integer "total")) "a refused note wrote nothing")))

(deftest field-notes-cell-the-store-outlives-a-restart ()
  (with-cell-stop ((field-notes-start*))
    (field-notes-tool "fieldnote_new"
                       (nlk:json-object "title" "Kept across a restart"
                                        "summary" "The notes are files, so a restart changes nothing."
                                        "type" "observation")))
  (with-cell-stop ((field-notes-start*))
    (is (= 1 (nlk:json-value (field-notes-json
                              (field-notes-tool "fieldnote_search"
                                                (nlk:json-object "query" "restart")))
                             :integer "total")))))

;;; --- the patch check ------------------------------------------------------------

(deftest field-notes-cell-a-patch-check-reads-the-signs ()
  (with-cell-stop ((field-notes-start*))
    (field-notes-core-file "VERSION" "0.2.0
")
    (field-notes-core-file "core.py" "old_line()
")
    (let ((id (nlk:json-value
               (field-notes-json
                (field-notes-tool "fieldnote_new"
                                   (nlk:json-object
                                    "title" "The gateway answers a quota error as an auth failure"
                                    "summary" "The error table is walked top to bottom and the auth row matches first."
                                    "type" "patch"
                                    "area" "gateway"
                                    "patch_kind" "bugfix"
                                    "patch_short" "order the error rows"
                                    "checks" (field-notes-checks "PATCH_MARKER" "upstream_fix()"))))
               :string "id")))
      (is (stringp id) "a patch note with checks is written")
      (is (probe-file (nodecode-field-notes::checks-path id)) "its checks file is beside it")
      (let ((answer (field-notes-json (field-notes-tool "fieldnote_check" (nlk:json-object)))))
        (is (equal "MISSING" (field-notes-patch-status answer id)) "the unpatched tree is MISSING")
        (is (= 1 (nlk:json-value answer :integer "missing")))
        (is (search "MISSING" (nlk:json-value answer :string "verdict"))))
      (field-notes-core-file "core.py" "upstream_fix()
")
      (let ((answer (field-notes-json (field-notes-tool "fieldnote_check" (nlk:json-object)))))
        (is (equal "UPSTREAMED" (field-notes-patch-status answer id)) "upstream's signs read as UPSTREAMED"))
      (field-notes-core-file "core.py" "PATCH_MARKER
new_line()
")
      (let ((answer (field-notes-json
                     (field-notes-tool "fieldnote_check" (nlk:json-object "id" id)))))
        (is (equal "OK" (field-notes-patch-status answer id)) "the patched tree is OK")
        (is (= 1 (nlk:json-value answer :integer "ok")))
        (is (stringp (nlk:json-value answer :string "verdict")))))))

(deftest field-notes-cell-the-doctor-counts-and-verdicts ()
  (with-cell-stop ((field-notes-start*))
    (field-notes-tool "fieldnote_new"
                       (nlk:json-object "title" "A pitfall worth keeping"
                                        "summary" "A tool answered exit 0 and nothing."
                                        "type" "pitfall"))
    (field-notes-core-file "VERSION" "0.2.0
")
    (field-notes-core-file "core.py" "old_line()
")
    (let ((id (nlk:json-value
               (field-notes-json
                (field-notes-tool "fieldnote_new"
                                   (nlk:json-object
                                    "title" "A patch that is gone" "summary" "Its signs are absent."
                                    "type" "patch" "patch_kind" "bugfix"
                                    "checks" (field-notes-checks "PATCH_MARKER" "upstream_fix()"))))
               :string "id")))
      (is (stringp id))
      (let ((doctor (field-notes-json (field-notes-tool "fieldnote_doctor" (nlk:json-object)))))
        (is (= 2 (nlk:json-value doctor :integer "notes")) "the doctor counts both notes")
        (is (= 1 (nlk:json-value doctor :integer "pitfalls")))
        (is (= 1 (nlk:json-value doctor :integer "patches")))
        (is (= 1 (nlk:json-value doctor :integer "missing")))
        (is (search "Re-apply a missing patch" (nlk:json-value doctor :string "verdict")))))))

;;; --- /fieldnotes and the switch ---------------------------------------------------

(deftest field-notes-cell-the-command-shows-searches-and-switches ()
  (with-cell-stop ((field-notes-start*))
    (field-notes-tool "fieldnote_new"
                       (nlk:json-object "title" "Cron fires twice"
                                        "summary" "Two units, one stale."
                                        "type" "pitfall"))
    (is (field-notes-carrying (cell-entry "nodecode-field-notes" "fieldnotes" "")
                               "Field notes: 1 note in" "1 pitfall"
                               "No patch note is registered yet"))
    (is (field-notes-carrying (cell-entry "nodecode-field-notes" "fieldnotes" "cron")
                               "1 note for \"cron\"" "Cron fires twice"))
    (is (field-notes-carrying (cell-entry "nodecode-field-notes" "fieldnotes" "nothing-like-this")
                               "0 matches"))
    (is (field-notes-carrying (cell-entry "nodecode-field-notes" "fieldnotes" "ai off")
                               "AI access is off"))
    (is (field-notes-carrying (field-notes-tool "fieldnote_search" (nlk:json-object "query" "cron"))
                               "ERROR: field-notes:" "AI access is off"))
    (is (field-notes-carrying (field-notes-tool "fieldnote_new"
                                                 (nlk:json-object "title" "Late" "summary" "x"))
                               "AI access is off"))
    (is (field-notes-carrying (cell-entry "nodecode-field-notes" "fieldnotes" "ai on")
                               "AI access is on"))
    (is (= 1 (nlk:json-value (field-notes-json
                              (field-notes-tool "fieldnote_search" (nlk:json-object "query" "cron")))
                             :integer "total")))))
