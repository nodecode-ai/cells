;;;; cell-test.lisp --- the cell against a scratch store.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The cell's I/O is one file, so the stub is a home: every test starts the
;;;; cell on a fresh store under the scratch home and nothing reads or writes
;;;; the operator's own. What is proved is the wiring a user meets: the
;;;; section (a healthy start, a value out of range refused, a store that
;;;; cannot be written refused), the three tools through the dispatch the
;;;; model calls them by, the manifest that never drops an entry, the lexical
;;;; prefetch and its budget, /nachos, and the manual.

(in-package #:nodecode.test)

(define-test-slice "nachos" "NACHOS-CELL-" :start nodecode-nachos:start-cell)

(defvar *fixture-count* 0 "How many scratch stores this run has made, so two never share one.")

(defvar *nachos-store-file* nil "The store file the fixture started the cell on.")

(defun nachos-store ()
  "A fresh store path under the scratch home."
  (nlk:home (format nil "nachos-test/~d-~d/memory.md" (get-universal-time) (incf *fixture-count*))))

(defmacro with-nachos ((&rest pairs) &body body)
  "The cell started on a scratch store with PAIRS beside, stopped on unwind."
  `(let ((*nachos-store-file* (namestring (nachos-store))))
     (with-cell-stop ((nachos-start "store_file" *nachos-store-file* ,@pairs))
       ,@body)))

(defun nachos-call (name &rest pairs)
  "NAME called the way the model calls it. => (values TEXT DECODED)."
  (let* ((call (nle::execute-tool-call
                (wire-tool-call "call-1" name (apply #'nlk:make-json-object pairs))
                (compiled-context '())))
         (text (princ-to-string (nle::message-content call))))
    (values text (ignore-errors (nlk:decode-json text)))))

(defun nachos-json (name &rest pairs)
  "NAME called with PAIRS, its answer decoded."
  (nth-value 1 (apply #'nachos-call name pairs)))

(defun nachos-answer (name &rest pairs)
  "NAME called with PAIRS, the text the model reads."
  (nth-value 0 (apply #'nachos-call name pairs)))

(defun nachos-write (title body &rest pairs)
  "Put TITLE and BODY into the running store. => the key it stored."
  (nlk:json-value (apply #'nachos-json "nachos_memory_put" "title" title "body" body pairs) :string "stored"))

;;; --- the declaration, the start and the stop -------------------------------------

(define-cell-lifecycle-tests "nachos"
  (:config "store_file" (namestring (nachos-store)))
  (:help :nachos)
  (:command "nachos")
  (:running
   (is (nle::find-tool "nachos_memory_put") "the write tool is registered")
   (is (nle::find-tool "nachos_memory_recall") "and the read tool")
   (is (nle::find-tool "nachos_memory_remove") "and the delete tool")
   (is (assoc "nachos" (nle::read-harness-sections "s1") :test #'equal) "and the manifest rides every request as its own section"))
  (:stopped
   (is (null (nle::find-tool "nachos_memory_put")) "stop takes the tools back")
   (is (null (assoc "nachos" (nle::read-harness-sections "s1") :test #'equal)) "and the section with them"))
  (:refused ("store_file" (namestring (nachos-store)) "prefetch_top_n" 0)))

(deftest nachos-cell-a-store-that-cannot-be-written-refuses ()
  (let* ((root (nachos-store))
         (plain (merge-pathnames "blocked.txt" root))
         (under (namestring (merge-pathnames "blocked.txt/memory.md" root))))
    (ensure-directories-exist plain)
    (nlk:write-file-atomically plain "a file, not a directory")
    (is (probe-file plain) "a plain file stands where a directory would go")
    (is (signals-error nlk:config-refusal (nodecode-nachos::open-store under)) "so a store beneath it refuses")
    (is (search "store_file" (princ-to-string (signals-error nlk:config-refusal (nodecode-nachos::open-store under)))) "and the refusal names the setting to fix")))

;;; --- the tools --------------------------------------------------------------------

(deftest nachos-cell-a-put-is-recalled-by-key-and-by-query ()
  (with-nachos ()
    (let ((key (nachos-write "Deploy notes" "Run the deploy script after the tests.")))
      (is (equal "deploy-notes" key) "the key is the slug of the title")
      (is (equal *nachos-store-file* (namestring (nodecode-nachos::store-file))) "and the section's store_file is the store that was written")
      (is (probe-file *nachos-store-file*) "which is a file on the disk")
      (is (search "Run the deploy script after the tests." (nachos-answer "nachos_memory_recall" "key" key)) "the whole body comes back by key")
      (let ((answer (nachos-json "nachos_memory_recall" "query" "deploy")))
        (is (= 1 (nlk:json-value answer :integer "count")) "a query finds the entry")
        (is (equal key (nlk:json-value (aref (nlk:json-value answer :array "results") 0) :string "key")) "and names its key")))))

(deftest nachos-cell-a-category-groups-the-manifest ()
  (with-nachos ()
    (let ((answer (nachos-json "nachos_memory_put" "title" "Deploy notes" "body" "Run it."
                                "category" "ops" "summary" "the release checklist")))
      (is (equal "ops" (nlk:json-value answer :string "category")) "the answer says which group it landed in"))
    (is (search "## ops" (nodecode-nachos::memory-section)) "and the manifest groups it there")
    (is (search "Deploy notes --- the release checklist" (nodecode-nachos::memory-section)) "with the summary the caller gave")))

(deftest nachos-cell-a-missing-entry-is-an-answer-not-a-crash ()
  (with-nachos ()
    (is (search "no entry with key" (nachos-answer "nachos_memory_recall" "key" "absent")) "recall says what is missing")
    (is (search "provide a key or a query" (nachos-answer "nachos_memory_recall")) "and asks for one of its two arguments")))

(deftest nachos-cell-a-remove-takes-the-entry-out ()
  (with-nachos ()
    (nachos-write "Coffee order" "Oat milk, no sugar.")
    (is (search "coffee-order" (nachos-answer "nachos_memory_remove" "title" "Coffee order")) "a title is slugified to its key")
    (is (zerop (length (nodecode-nachos::entries))) "and the store is empty after")
    (is (search "no entry with key" (nachos-answer "nachos_memory_recall" "key" "coffee-order")) "so recall cannot find it")))

(deftest nachos-cell-the-store-is-a-file-a-hand-edit-lands-in ()
  (with-nachos ()
    (nachos-write "Build habit" "make test before pushing." "category" "ops")
    (let* ((path (namestring (nodecode-nachos::store-file)))
           (text (uiop:read-file-string path)))
      (is (search "# ops" text) "the file groups entries by category")
      (is (search "## Build habit" text) "and heads each entry with its title")
      (nlk:write-file-atomically path (format nil "~a~%## Added by hand~%changed by hand.~%" (string-right-trim '(#\Newline) text)))
      (is (= 2 (length (nodecode-nachos::entries))) "a person's edit is read back as an entry")
      (is (search "changed by hand" (nachos-answer "nachos_memory_recall" "query" "changed")) "and is recallable like any other"))))

;;; --- the manifest and the prefetch ---------------------------------------------------

(deftest nachos-cell-the-manifest-keeps-every-entry-under-its-budget ()
  (with-nachos ("manifest_chars" 200)
    (nachos-write "Alpha habit" "the release checklist, whose summary is long enough to be cut when the budget is small")
    (nachos-write "Beta fact" "a short one")
    (nachos-write "Gamma rule" "another short one")
    (let* ((items (nodecode-nachos::entries))
           (whole (nodecode-nachos::render-manifest items))
           (budgeted (nodecode-nachos::render-manifest items :budget 200)))
      (dolist (title '("Alpha habit" "Beta fact" "Gamma rule"))
        (is (search title budgeted) (format nil "~a survives the budget" title)))
      (is (< (length budgeted) (length whole)) "the summaries gave way, never an entry")
      (is (search "Alpha habit" (nodecode-nachos::memory-section)) "the section carries the manifest"))))

(deftest nachos-cell-prefetch-ranks-the-turns-own-words ()
  (with-nachos ("prefetch_top_n" 1 "prefetch_chars" 500)
    (nachos-write "Deploy notes" "Run the deploy script after the tests.")
    (nachos-write "Coffee order" "Oat milk, no sugar.")
    (let ((block (nodecode-nachos::prefetch-text "how do I deploy this")))
      (is (search "Deploy notes" block) "the entry the turn's words rank first")
      (is (not (search "Coffee order" block)) "and no other body")
      (is (gethash "deploy-notes" nodecode-nachos::*prefetched*) "the inlined entry is marked")
      (is (search "► Deploy notes" (nodecode-nachos::memory-section)) "and its manifest line says so"))))

(deftest nachos-cell-a-budget-too-small-prefetches-nothing ()
  (with-nachos ("prefetch_top_n" 1 "prefetch_chars" 60)
    (nachos-write "Deploy notes" "Run the deploy script after the tests.")
    (is (null (nodecode-nachos::prefetch-text "deploy")) "the block does not fit, so it is not sent")
    (is (zerop (hash-table-count nodecode-nachos::*prefetched*)) "and nothing is marked")))

;;; --- the command ----------------------------------------------------------------------

(deftest nachos-cell-nachos-says-where-the-store-is ()
  (with-nachos ()
    (nachos-write "Deploy notes" "Run it.")
    (is (search "entries    : 1" (nodecode-nachos::run-command "status" nil)) "status counts what the store holds")
    (is (search (namestring (nodecode-nachos::store-file)) (nodecode-nachos::run-command "status" nil)) "and says where it is")
    (is (search "Deploy notes" (nodecode-nachos::run-command "list" nil)) "list renders the manifest")
    (is (search "Run it." (nodecode-nachos::run-command "recall deploy-notes" nil)) "recall reads one entry")
    (multiple-value-bind (text panel) (nodecode-nachos::run-command "" nil)
      (is (search "nachos memory" text) "bare says the status")
      (is (getf panel :rows) "and opens its panel"))))
