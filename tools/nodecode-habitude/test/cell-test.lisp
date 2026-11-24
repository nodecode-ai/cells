;;;; cell-test.lisp --- the habitude cell against a vault this file writes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every vault here is a temp directory of the test's own, so the slice needs
;;;; no key, no network and no note of the operator's: a search that cites,
;;;; the excerpt a citation names, a miss that says it is a miss, an index that
;;;; notices a new note, and a reference that outlives its note. What is proved
;;;; is what a model reads back, through the seam the engine calls a tool by.

(in-package #:nodecode.test)

;;; --- fixtures ---------------------------------------------------------------------

(defvar *habitude-vaults* 0
  "How many scratch vaults this file has made, so two tests never share one.")

(defun habitude-scratch-vault ()
  "A fresh empty directory to be a vault. => its directory pathname."
  (let ((root (uiop:ensure-directory-pathname
               (merge-pathnames (format nil "habitude-~d-~d/" (get-universal-time)
                                         (incf *habitude-vaults*))
                                (uiop:temporary-directory)))))
    (ensure-directories-exist (merge-pathnames "placeholder" root))
    root))

(defun habitude-write (root relative text)
  "Write TEXT at RELATIVE under ROOT, making its directories. => the file."
  (let ((file (merge-pathnames relative (uiop:ensure-directory-pathname root))))
    (ensure-directories-exist file)
    (with-open-file (out file :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (write-string text out))
    file))

(defparameter *habitude-vault* (habitude-scratch-vault)
  "The vault the lifecycle tests start the cell on: an empty folder is enough.")

(defun habitude-arguments (&rest pairs)
  "PAIRS as the JSON object string a tool call carries."
  (nlk:encode-json-object (apply #'nlk:make-json-object pairs)))

(defun habitude-tool (name &rest pairs)
  "The registered tool NAME called with PAIRS the way the engine calls it:
the arguments as JSON, the record materialized at the body."
  (let* ((entry (nle::find-tool name))
         (schema (second entry))
         (arguments (nlk:decode-json (apply #'habitude-arguments pairs))))
    (assert entry () "~a is not registered" name)
    (funcall (third entry)
             (if (nlk:contract-p schema)
                 (nlk:contract-decode schema arguments)
                 arguments))))

(defun habitude-reference (answer)
  "The first reference ANSWER cites."
  (let ((at (search "reference=\"" answer)))
    (when at
      (let* ((start (+ at (length "reference=\"")))
             (end (position #\" answer :start start)))
        (and end (subseq answer start end))))))

(define-test-slice "habitude" "HABITUDE-CELL-" :start nodecode-habitude:start-cell)

(define-cell-lifecycle-tests "habitude"
  (:config "vault" (namestring *habitude-vault*))
  (:help :habitude)
  (:command "habitude")
  (:running (is (nle::find-tool "search_journal") "the search tool is registered")
            (is (nle::find-tool "read_excerpt") "and the excerpt tool")
            (is (nle::find-tool "index_status") "and the status tool")
            (is (nle::find-tool "refresh_index") "and the refresh tool")
            (is (= 8 (nodecode-habitude::setting :max-results)) "max_results defaults to eight")
            (is (= 60 (nodecode-habitude::setting :refresh-seconds)) "and the index to a minute"))
  (:stopped (is (null (nle::find-tool "search_journal")) "stop unregisters the tools"))
  (:refused () ("max_results" 0) ("vault" 7))
  (:idle nodecode-habitude:habitude-error (habitude:search-journal "writing routine")))

;;; --- a search that cites ------------------------------------------------------------

(deftest habitude-cell-a-search-cites-the-note-it-found ()
  (let ((vault (habitude-scratch-vault)))
    (habitude-write vault "journal/2026-01-02.md"
                     "# Morning pages

My writing routine was slow this week; the draft stalled after one page.
")
    (habitude-write vault "journal/2026-01-05.md"
                     "# Morning pages

Walked to the river and read for an hour.
")
    (with-cell-stop ((habitude-start "vault" (namestring vault)))
      (let ((answer (habitude-tool "search_journal" "query" "writing routine draft")))
        (is-carrying answer
                     ("journal/2026-01-02.md" "the note is cited")
                     ("date=2026-01-02" "with the date its path names")
                     ("untrusted reference data" "and framed as data, not instructions")
                     (:absent "Walked to the river" "a note without the words is not cited"))
        (let ((reference (habitude-reference answer)))
          (is (stringp reference) "the search issues a reference")
          (is-carrying (text (habitude-tool "read_excerpt" "reference" reference))
                       ("journal/2026-01-02.md:1-" "the excerpt names its lines")
                       ("the draft stalled" "and carries the passage itself")))))))

(deftest habitude-cell-a-miss-says-it-is-a-miss ()
  (let ((vault (habitude-scratch-vault)))
    (habitude-write vault "notes.md" "# Notes

Nothing about music here.
")
    (with-cell-stop ((habitude-start "vault" (namestring vault)))
      (is-carrying (text (habitude-tool "search_journal" "query" "xylophone orchestra"))
                   ("No local excerpt carries" "the answer says nothing matched")
                   ("not proof" "and does not claim a note is absent")))))

(deftest habitude-cell-the-index-sees-a-note-written-later ()
  (let ((vault (habitude-scratch-vault)))
    (habitude-write vault "notes.md" "# Notes

A first entry about the garden.
")
    (with-cell-stop ((habitude-start "vault" (namestring vault)))
      (is-carrying (text (habitude-tool "index_status"))
                   ("1 note" "one note is indexed")
                   ("built " "and it says when"))
      (habitude-write vault "2026-02-01.md" "# Later

The greenhouse was glazed on a cold Tuesday.
")
      (is-carrying (text (habitude-tool "refresh_index"))
                   ("2 notes" "a refresh reads the new note"))
      (is-carrying (text (habitude-tool "search_journal" "query" "greenhouse glazed"))
                   ("2026-02-01.md" "and the new note is findable")))))

(deftest habitude-cell-a-reference-goes-stale-with-its-note ()
  (let ((vault (habitude-scratch-vault)))
    (habitude-write vault "day.md" "# Day

The kettle boiled while I wrote about patience.
")
    (with-cell-stop ((habitude-start "vault" (namestring vault)))
      (let* ((answer (habitude-tool "search_journal" "query" "patience kettle"))
             (reference (habitude-reference answer)))
        (is (stringp reference) "the search issues a reference")
        (habitude-write vault "day.md" "# Day

The kettle boiled again, and the note was rewritten.
")
        (is-carrying (text (habitude-tool "read_excerpt" "reference" reference))
                     ("ERROR: HABITUDE-ERROR" "a changed note is refused")
                     ("search again" "with the repair said out loud"))
        (is-carrying (text (habitude-tool "read_excerpt" "reference" "made-up"))
                     ("ERROR: HABITUDE-ERROR" "a reference no search issued is refused"))))))

(deftest habitude-cell-the-command-answers-without-a-session ()
  (let ((vault (habitude-scratch-vault)))
    (habitude-write vault "week.md" "# Week

The writing routine held for three mornings.
")
    (with-cell-stop ((habitude-start "vault" (namestring vault)))
      (is-carrying (text (nodecode-habitude::run-slash "" "s1"))
                   ("Habitude index" "typed alone it reports the index"))
      (is-carrying (text (nodecode-habitude::run-slash "writing routine" "s1"))
                   ("week.md" "typed with words it searches")))))
