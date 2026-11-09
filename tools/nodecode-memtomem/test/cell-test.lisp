;;;; cell-test.lisp --- lifecycle, the five tools, and /memtomem.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, and a relative store refused), every tool through the seam the
;;;; model calls it by, the shape a saved entry takes on disk, and /memtomem
;;;; with and without a query. There is no network to stub: the cell's whole
;;;; world is a store directory of the test's own, under the run's scratch
;;;; home.

(in-package #:nodecode.test)

(define-test-slice "memtomem" "MEMTOMEM-CELL-" :start nodecode-memtomem:start-cell)

;;; --- fixtures -----------------------------------------------------------------

(defun memtomem-temp-root ()
  "A store directory of this test's own, empty, with its way there made."
  (let* ((text (format nil "~a/nodecode-memtomem-~d/"
                        (string-right-trim "/" (uiop:native-namestring (uiop:temporary-directory)))
                        (random 1000000)))
         (path (uiop:ensure-directory-pathname text)))
    (ensure-directories-exist path)
    text))

(defmacro with-memtomem-store ((&rest config) &body body)
  "BODY with the cell started on a store of its own, and the store deleted after.
The pairs CONFIG gives override the section's defaults."
  `(let ((root (memtomem-temp-root)))
     (unwind-protect
          (with-cell-stop ((memtomem-start "root" root ,@config))
            ,@body)
       (ignore-errors
         (uiop:delete-directory-tree (uiop:ensure-directory-pathname root) :validate t)))))

(defun memtomem-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun memtomem-note (root relative text)
  "Write TEXT as RELATIVE under ROOT, the way another agent sharing the store would."
  (let ((file (merge-pathnames relative (uiop:ensure-directory-pathname root))))
    (ensure-directories-exist file)
    (with-open-file (stream file :direction :output :if-exists :supersede
                                  :if-does-not-exist :create :external-format :utf-8)
      (write-string text stream))
    file))

(defun memtomem-notes-file (root relative)
  (merge-pathnames relative (uiop:ensure-directory-pathname root)))

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "memtomem"
  (:config "root" "~/.memtomem")
  (:help :memtomem)
  (:command "memtomem")
  (:running (is (nle::find-tool "mem_search") "mem_search is registered")
            (is (nle::find-tool "mem_recall"))
            (is (nle::find-tool "mem_add"))
            (is (nle::find-tool "mem_index"))
            (is (nle::find-tool "mem_status"))
            (is (= 8 (nodecode-memtomem::setting :max-results)) "the default page")
            (is (search "memories" (uiop:native-namestring
                                     (nodecode-memtomem::notes-directory))) "the notes default under the store"))
  (:refused ("root" "relative/store")))

(deftest memtomem-cell-a-notes-directory-may-be-named-aside ()
  (let ((root (memtomem-temp-root)))
    (unwind-protect
         (with-cell-stop ((memtomem-start "root" (concatenate 'string root "store/")
                                           "notes_dir" (concatenate 'string root "notes/")))
           (is (equal (uiop:ensure-directory-pathname (concatenate 'string root "notes/"))
                      (nodecode-memtomem::notes-directory)) "notes_dir is where the cell reads"))
      (ignore-errors
        (uiop:delete-directory-tree (uiop:ensure-directory-pathname root) :validate t)))))

;;; --- saving an entry -------------------------------------------------------------

(deftest memtomem-cell-add-writes-a-note-in-the-stores-own-shape ()
  (with-memtomem-store ()
    (let ((answer (memtomem-tool "mem_add"
                                  (nlk:json-object
                                   "content" "Deploys go out on Thursdays, never on Fridays."
                                   "title" "Deploy days"
                                   "tags" (vector "deploy" "operations")))))
      (is (search "Saved one entry to" answer) "the write is reported")
      (is (search "2 tags" answer) "with the tags it kept"))
    (let ((files (nodecode-memtomem::note-files
                  (nodecode-memtomem::notes-directory))))
      (is (= 1 (length files)) "one day file was written")
      (is-present (text (and files (uiop:read-file-string (first files))))
        "the store holds the note"
        (is (search "## Deploy days" text) "the heading memtomem indexes")
        (is (search "> created: " text) "the created stamp")
        (is (search "> tags: [\"deploy\", \"operations\"]" text) "the tags as memtomem parses them")
        (is (search "Deploys go out on Thursdays" text) "and the text itself")))))

(deftest memtomem-cell-add-needs-content ()
  (with-memtomem-store ()
    (is (search "ERROR: memtomem: content is required"
                (memtomem-tool "mem_add" (nlk:json-object))))))

;;; --- search ----------------------------------------------------------------------

(deftest memtomem-cell-search-finds-the-note-that-answers ()
  (with-memtomem-store ()
    (memtomem-tool "mem_add" (nlk:json-object "content"
                                                 "Deploys go out on Thursdays, never on Fridays."
                                                 "title" "Deploy days"))
    (memtomem-tool "mem_add" (nlk:json-object "content"
                                                 "The office espresso machine is descaled monthly."
                                                 "title" "Espresso"))
    (let ((answer (memtomem-tool "mem_search"
                                  (nlk:json-object "query" "when do deploys go out"))))
      (is (search "match" answer) "the search answers")
      (is (search "Deploy days" answer) "with the heading of the note that says it")
      (is (search "2026-" answer) "and the day file it lives in")
      (is (search "(score " answer) "and the score it ranked it by")
      (is (null (search "Espresso" answer)) "the unrelated note is not a match"))
    (is (search "No memtomem note matched"
                (memtomem-tool "mem_search" (nlk:json-object "query" "kubernetes cluster"))) "a topic the store has nothing on says so")))

(deftest memtomem-cell-search-reads-what-another-agent-wrote ()
  (with-memtomem-store ()
    (memtomem-note root "memories/2026-01-05.md"
                    (format nil "## Handoff~2%> created: 2026-01-05T09:00:00+00:00~2%The staging key rotates every Monday.~%"))
    (let ((answer (memtomem-tool "mem_search" (nlk:json-object "query" "staging key rotation"))))
      (is (search "Handoff" answer) "a note another agent left is found")
      (is (search "2026-01-05.md" answer) "under the file it lives in"))))

;;; --- recall -----------------------------------------------------------------------

(deftest memtomem-cell-recall-windows-a-date-range ()
  (with-memtomem-store ()
    (memtomem-note root "memories/2020-01-02.md"
                    (format nil "## Old decision~2%> created: 2020-01-02T09:00:00+00:00~2%We chose SQLite for the index.~%"))
    (memtomem-tool "mem_add" (nlk:json-object "content"
                                                 "Today we picked Postgres for the ledger."
                                                 "title" "Ledger decision"))
    (let ((windowed (memtomem-tool "mem_recall"
                                    (nlk:json-object "since" "2020-01-01" "until" "2020-12-31"))))
      (is (search "Old decision" windowed) "the memory inside the window is read")
      (is (null (search "Ledger decision" windowed)) "and the one outside it is not"))
    (let ((all (memtomem-tool "mem_recall" (nlk:json-object))))
      (is (search "newest first" all) "with no window it answers all of them")
      (multiple-value-bind (newer older)
          (values (search "Ledger decision" all) (search "Old decision" all))
        (is (and newer older (< newer older)) "newest first, as the original orders them")))))

;;; --- the store as a whole -----------------------------------------------------------

(deftest memtomem-cell-status-reports-the-store-it-reads ()
  (with-memtomem-store ()
    (is (search "no notes directory yet" (memtomem-tool "mem_status" (nlk:json-object))) "an empty store says what will happen, not that it is broken")
    (memtomem-tool "mem_add" (nlk:json-object "content" "Prefers dark mode."))
    (let ((answer (memtomem-tool "mem_status" (nlk:json-object))))
      (is (search "1 note file" answer) "the file count")
      (is (search "1 entry" answer) "the entry count")
      (is (search "BM25" answer) "and what search is doing")
      (is (search "no config.json" answer) "a fresh store needs no setup"))))

(deftest memtomem-cell-index-wants-an-explicit-absolute-path ()
  (with-memtomem-store ()
    (memtomem-tool "mem_add" (nlk:json-object "content" "Notes about the release train."))
    (let ((answer (memtomem-tool "mem_index"
                                  (nlk:json-object
                                   "path" (uiop:native-namestring
                                           (nodecode-memtomem::notes-directory))))))
      (is (search "Scanned 1 file" answer) "the directory the user named is read")
      (is (search "1 entry" answer) "and counted"))
    (is (search "must be absolute"
                (memtomem-tool "mem_index" (nlk:json-object "path" "notes/"))) "a relative path is refused, as the original's skills require")))

;;; --- /memtomem ---------------------------------------------------------------------

(deftest memtomem-cell-the-command-shows-the-store-or-searches-it ()
  (with-memtomem-store ()
    (let ((answer (cell-entry "nodecode-memtomem" "memtomem" "")))
      (is (search "memtomem: store" answer) "no argument answers the store")
      (is (search "0 note file" answer) "with how much it holds"))
    (memtomem-tool "mem_add" (nlk:json-object "content"
                                                 "The build cache lives on the shared volume."
                                                 "title" "Build cache"))
    (let ((answer (cell-entry "nodecode-memtomem" "memtomem" "where is the build cache")))
      (is (search "Build cache" answer) "a query searches the notes from the shell"))))
