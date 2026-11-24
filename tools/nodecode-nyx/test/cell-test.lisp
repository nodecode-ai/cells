;;;; cell-test.lisp --- lifecycle, the four verbs, /nyx, and the refuse.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring on a scratch folder: the
;;;; section (a healthy start, an integer under its floor refused, a data_dir
;;;; it cannot write refused), the four tools through the seam the model calls
;;;; them by, /nyx with and without a query, the secret filter, and a
;;;; forgotten entry going to the quarantine and coming back. Nothing here
;;;; dials anything: the store is a folder under the temporary directory.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *nyx-folder* nil
  "The empty folder one test's store lives in.")

(defmacro with-nyx-store (&body body)
  "BODY with *NYX-FOLDER* a fresh, empty folder: what data_dir names."
  `(let ((*nyx-folder* (merge-pathnames (format nil "nyx-test-~d-~d/"
                                                (get-universal-time) (random 1000000))
                                        (uiop:temporary-directory))))
     (unwind-protect (progn ,@body)
       (when (probe-file *nyx-folder*)
         (ignore-errors (uiop:delete-directory-tree *nyx-folder* :validate t))))))

(defun nyx-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun nyx-id-of (answer)
  "The entry id ANSWER -- a tool's `Remembered [m_...] at ...' -- names."
  (let ((start (search "[m_" answer)))
    (and start (subseq answer (1+ start) (+ start 15)))))

(defun nyx-journal-lines ()
  "The store's journal, one entry a line."
  (let ((path (merge-pathnames "journal.txt" *nyx-folder*)))
    (when (probe-file path)
      (with-open-file (in path :external-format :utf-8)
        (loop for line = (read-line in nil nil)
              while line collect line)))))

(defun nyx-quarantine-rows ()
  "The store's quarantine, one JSON row a line."
  (let ((path (merge-pathnames "quarantine.jsonl" *nyx-folder*)))
    (when (probe-file path)
      (with-open-file (in path :external-format :utf-8)
        (loop for line = (read-line in nil nil)
              while line
              for text = (string-trim '(#\Space #\Tab) line)
              when (plusp (length text))
                collect (nlk:decode-json text))))))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "nyx" "NYX-CELL-" :start nodecode-nyx:start-cell)

(define-cell-lifecycle-tests "nyx"
  (:fixture with-nyx-store)
  (:config "data_dir" (namestring *nyx-folder*))
  (:help :nyx)
  (:command "nyx")
  (:running
   (is (nle::find-tool "nyx_remember") "nyx_remember is registered")
   (is (nle::find-tool "nyx_recall"))
   (is (nle::find-tool "nyx_forget"))
   (is (nle::find-tool "nyx_restore"))
   (is (equal (namestring *nyx-folder*)
              (namestring (nodecode-nyx::data-directory))) "the section's data_dir is where memory lives")
   (is (= 8 (nodecode-nyx::setting :recall-limit)) "the default recall budget")
   (is (= 7 (nodecode-nyx::setting :forget-days)) "the default quarantine window"))
  (:stopped (is (null (nle::find-tool "nyx_remember")) "stop unregisters the tools"))
  (:refused ("recall_limit" 0) ("forget_days" 0))
  (:idle nodecode-nyx:nyx-error (nodecode-nyx::run-command "" nil)))

;;; --- the tools ------------------------------------------------------------------

(deftest nyx-cell-remember-filters-secrets-and-recall-dates-them ()
  (with-nyx-store
    (with-cell-stop ((nyx-start "data_dir" (namestring *nyx-folder*)))
      (let ((answer (nyx-tool "nyx_remember"
                               (nlk:json-object
                                "text" "the deploy token is ghp_0123456789abcdef0123456789abcdef0123 and I drink oolong"))))
        (is (search "Remembered [m_" answer) "the answer names the entry")
        (is (search "placeholder" answer) "and says a secret was replaced"))
      (let ((lines (nyx-journal-lines)))
        (is (= 1 (length lines)) "one entry, one line")
        (is (search "[hidden: GitHub token]" (first lines))
            (format nil "the token became its label: ~a" (first lines)))
        (is (null (search "ghp_0123456789abcdef0123456789abcdef0123" (first lines))) "the token itself never reaches the disk")
        (is (search "I drink oolong" (first lines)) "the rest is written verbatim")
        (is (search " | user | " (first lines)) "with its source and its stamp"))
      (let ((answer (nyx-tool "nyx_recall" (nlk:json-object "query" "oolong"))))
        (is (search "1 memory for \"oolong\":" answer))
        (is (search "I drink oolong" answer))
        (is (search " | user | " answer) "each hit is dated"))
      (is (search "No memory matched \"coffee\"."
                  (nyx-tool "nyx_recall" (nlk:json-object "query" "coffee"))) "a miss says so"))))

(deftest nyx-cell-forget-quarantines-and-restore-returns-it ()
  (with-nyx-store
    (with-cell-stop ((nyx-start "data_dir" (namestring *nyx-folder*)))
      (nyx-tool "nyx_remember" (nlk:json-object "text" "lives in Oslo"))
      (let* ((full (nyx-tool "nyx_remember" (nlk:json-object "text" "drinks green tea, never coffee")))
             (id (nyx-id-of full)))
        (is (= 2 (length (nyx-journal-lines))) "two entries")
        (let ((forgotten (nyx-tool "nyx_forget" (nlk:json-object "mem_id" id))))
          (is (search "Forgotten 1 memory, restorable for 7 days" forgotten))
          (is (search id forgotten) "the id is named, to restore from"))
        (is (= 1 (length (nyx-journal-lines))) "the journal lost it")
        (is (= 1 (length (nyx-quarantine-rows))) "the quarantine holds it")
        (is (search "never coffee" (nlk:encode-json-object (first (nyx-quarantine-rows)))) "with its text, whole")
        (is (search "No memory matched \"coffee\"."
                    (nyx-tool "nyx_recall" (nlk:json-object "query" "coffee"))) "and recall no longer finds it")
        (is (equal (format nil "Restored ~a." id)
                   (nyx-tool "nyx_restore" (nlk:json-object "mem_id" id))))
        (is (= 2 (length (nyx-journal-lines))) "it is back in the journal")
        (is (null (nyx-quarantine-rows)) "and out of the quarantine")
        (is (search "never coffee" (nyx-tool "nyx_recall" (nlk:json-object "query" "coffee"))) "recall finds it again")
        (is (search "Forgotten 1 memory"
                    (nyx-tool "nyx_forget" (nlk:json-object "contains" "Oslo"))) "contains names an entry too")))))

(deftest nyx-cell-forget-needs-something-to-match ()
  (with-nyx-store
    (with-cell-stop ((nyx-start "data_dir" (namestring *nyx-folder*)))
      (is-carrying (answer (nyx-tool "nyx_forget" (nlk:json-object)))
                   ("ERROR: nyx:" "give mem_id or contains"))
      (nyx-tool "nyx_remember" (nlk:json-object "text" "lives in Oslo"))
      (is-carrying (answer (nyx-tool "nyx_forget" (nlk:json-object "contains" "Lisbon")))
                   ("ERROR: nyx:" "no memory matched"))
      (is-carrying (answer (nyx-tool "nyx_restore" (nlk:json-object "mem_id" "m_000000000000")))
                   ("ERROR: nyx:" "is not in the quarantine"))
      (is (= 1 (length (nyx-journal-lines))) "a failed forget takes nothing"))))

(deftest nyx-cell-forgetting-without-secrets-keeps-the-text ()
  (with-nyx-store
    (with-cell-stop ((nyx-start "data_dir" (namestring *nyx-folder*) "filter_secrets" nil))
      (nyx-tool "nyx_remember" (nlk:json-object "text" "the key is sk-abcdefghijklmnopqrstuvwx"))
      (is (search "sk-abcdefghijklmnopqrstuvwx" (first (nyx-journal-lines))) "filter_secrets off writes the text as it came"))))

;;; --- /nyx ---------------------------------------------------------------------

(deftest nyx-cell-the-command-shows-the-store-or-searches-it ()
  (with-nyx-store
    (with-cell-stop ((nyx-start "data_dir" (namestring *nyx-folder*)))
      (let ((answer (cell-entry "nodecode-nyx" "nyx" "")))
        (is (search "Nyx memory: 0 entries in " answer) "an empty store says so")
        (is (search (namestring *nyx-folder*) answer) "and where it is"))
      (nyx-tool "nyx_remember" (nlk:json-object "text" "lives in Oslo"))
      (is (search "Nyx memory: 1 entry in " (cell-entry "nodecode-nyx" "nyx" "")))
      (is (search "lives in Oslo" (cell-entry "nodecode-nyx" "nyx" "Oslo")) "with an argument it searches"))))

;;; --- a data_dir it cannot write ----------------------------------------------------

(deftest nyx-cell-a-data-dir-it-cannot-write-refuses ()
  (with-nyx-store
    (ensure-directories-exist *nyx-folder*)
    (let ((file (merge-pathnames "a-file" *nyx-folder*)))
      (with-open-file (out file :direction :output :if-exists :supersede
                                 :if-does-not-exist :create :external-format :utf-8)
        (write-string "not a folder" out))
      (is (search "data_dir"
                  (refusal-text nlk:config-refusal
                                 (nyx-start "data_dir" (namestring (merge-pathnames
                                                                     "inner/"
                                                                     (uiop:ensure-directory-pathname file)))))) "the refusal names the member the operator fixes")
      (is (null (nle::find-tool "nyx_remember")) "and a refused start installed nothing"))))
