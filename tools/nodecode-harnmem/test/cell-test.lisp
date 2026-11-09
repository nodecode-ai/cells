;;;; cell-test.lisp --- lifecycle, the five tools, /harnmem and the injected snapshot.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, floors refused, a start whose home is a file refused), the five
;;;; tools through the seam the model calls them by, /harnmem with and
;;;; without an argument, and the snapshot the standing harness section
;;;; carries. Offline: every test reads and writes a store of its own under
;;;; the scratch home, and nothing dials a network.

(in-package #:nodecode.test)

(define-test-slice "harnmem" "HARNMEM-" :start nodecode-harnmem:start-cell)

;;; --- fixtures ------------------------------------------------------------------

(defvar *scratch* 0
  "A counter so each test gets a store of its own under the scratch home.")

(defun harnmem-scratch-home ()
  "A fresh store directory for one test."
  (namestring (merge-pathnames (format nil "harnmem-store-~d-~d/" (get-universal-time) (incf *scratch*))
                                (user-homedir-pathname))))

(defun harnmem-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun harnmem-write (title description &key (type "user") (content "what the user said, in full."))
  (harnmem-tool "harnmem_write"
                (nlk:json-object "type" type "title" title
                                  "description" description "content" content)))

(defun harnmem-json (text)
  (nlk:decode-json text))

(defun harnmem-section ()
  "The block the standing harness section carries while the cell runs."
  (cdr (assoc "harnmem" (nle::read-harness-sections "session-1") :test #'string=)))

(defun harnmem-record-file (id)
  (nodecode-harnmem::record-path "user" id))

(defun harnmem-bullets (text)
  "The snapshot's one-memory-per-line bullets."
  (count-if (lambda (line) (uiop:string-prefix-p "- " line))
            (uiop:split-string (or text "") :separator '(#\Newline))))

;;; --- lifecycle ------------------------------------------------------------------

(define-cell-lifecycle-tests "harnmem"
  (:config)
  (:hooks 'nle::read-harness-sections)
  (:help :harnmem)
  (:command "harnmem")
  (:running
   (is (nle::find-tool "harnmem_search") "harnmem_search is registered")
   (is (nle::find-tool "harnmem_write"))
   (is (nle::find-tool "harnmem_read"))
   (is (nle::find-tool "harnmem_forget"))
   (is (nle::find-tool "harnmem_status"))
   (is (uiop:directory-exists-p (nodecode-harnmem::home)) "the store's home is there")
   (is (probe-file (merge-pathnames "VERSION" (nodecode-harnmem::home))) "with its VERSION file")
   (is (search "HarnMem store at" (nodecode-harnmem::run-command "" nil))))
  (:stopped
   (is (null (nle::find-tool "harnmem_search")) "stop unregisters the tools")
   (is (null (nodecode-harnmem::snapshot-section (lambda (session) (declare (ignore session)) nil) "s1"))
       "and the snapshot is gone from the standing sections"))
  (:refused ("max_inject_tokens" 0) ("hot_entries" 0) ("max_inject_chars" 5))
  (:idle nodecode-harnmem:harnmem-error (nodecode-harnmem::setting :home)))

(deftest harnmem-cell-a-home-that-is-a-file-refuses ()
  (let ((file (merge-pathnames "harnmem-not-a-store.txt" (user-homedir-pathname))))
    (with-open-file (out file :direction :output :if-exists :supersede
                               :if-does-not-exist :create)
      (write-string "not a store" out))
    (is-carrying (answer (refusal-text nodecode-harnmem:harnmem-error
                                       (harnmem-start "home" (namestring file))))
                 ("that is a file, not the store directory"))
    (is (null (nle::find-tool "harnmem_search")) "a refused start installed nothing")))

;;; --- the tools --------------------------------------------------------------------

(deftest harnmem-cell-write-then-search-and-read ()
  (with-cell-stop ((harnmem-start "home" (harnmem-scratch-home)))
    (let* ((answer (harnmem-json (harnmem-write "Drinks green tea" "prefers green tea to coffee")))
           (id (nlk:json-value answer :string "id")))
      (is (eq t (nlk:json-value answer :boolean "ok")))
      (is (equal "CREATE" (nlk:json-value answer :string "action")))
      (is (search "mem_" id) "an id of the original's shape")
      ;; the file is the original's: frontmatter, a blank line, the body
      (is (probe-file (harnmem-record-file id)) "one Markdown file per memory")
      (let ((file (uiop:read-file-string (harnmem-record-file id))))
        (is (uiop:string-prefix-p (format nil "---~%") file) "it opens with a frontmatter")
        (is (search "title: Drinks green tea" file))
        (is (search "type: user" file))
        (is (search "status: active" file))
        (is (search "what the user said, in full." file) "and the body below it"))
      ;; a second write of the same title merges instead of duplicating
      (let ((again (harnmem-json (harnmem-write "Drinks green tea" "prefers green tea, never coffee"))))
        (is (equal "MERGE" (nlk:json-value again :string "action")))
        (is (equal id (nlk:json-value again :string "id")) "the same record, updated"))
      ;; search finds it by its words
      (let* ((found (harnmem-json (harnmem-tool "harnmem_search"
                                                 (nlk:json-object "query" "what does the user drink"
                                                                  "limit" 5))))
             (results (nlk:json-value found :array "results"))
             (first-hit (and results (aref results 0))))
        (is (eq t (nlk:json-value found :boolean "ok")))
        (is (= 1 (nlk:json-value found :integer "total")) "one memory is in the store")
        (is (equal id (nlk:json-value first-hit :string "id")) "and it is the one that matched")
        (is (search "what the user said" (nlk:json-value first-hit :string "snippet")) "with its opening"))
      ;; an unrelated question abstains rather than inventing a hit
      (let ((miss (harnmem-json (harnmem-tool "harnmem_search"
                                               (nlk:json-object "query" "sourdough starter")))))
        (is (eq t (nlk:json-value miss :boolean "abstained")))
        (is (zerop (length (nlk:json-value miss :array "results")))))
      ;; read gives the record whole
      (let ((read (harnmem-json (harnmem-tool "harnmem_read" (nlk:json-object "id" id)))))
        (is (equal "Drinks green tea" (nlk:json-value read :string "record" "title")))
        (is (equal "what the user said, in full."
                   (nlk:json-value read :string "record" "content")))))))

(deftest harnmem-cell-forget-moves-to-trash-or-deletes ()
  (with-cell-stop ((harnmem-start "home" (harnmem-scratch-home)))
    (let* ((first (nlk:json-value (harnmem-json (harnmem-write "Lives in Oslo" "a fact")) :string "id"))
           (second (nlk:json-value (harnmem-json (harnmem-write "Works in Lisp" "another fact")) :string "id")))
      (let ((answer (harnmem-json (harnmem-tool "harnmem_forget" (nlk:json-object "id" first)))))
        (is (equal "trash" (nlk:json-value answer :string "mode")))
        (is (null (probe-file (harnmem-record-file first))) "gone from the shard")
        (is (plusp (length (directory (merge-pathnames "*.md" (merge-pathnames "trash/" (nodecode-harnmem::home))))))
            "and kept in trash"))
      (let ((answer (harnmem-json (harnmem-tool "harnmem_forget"
                                                 (nlk:json-object "id" second "hard" t)))))
        (is (equal "hard" (nlk:json-value answer :string "mode")))
        (is (null (probe-file (harnmem-record-file second))) "hard forget deletes it"))
      (is-carrying (answer (harnmem-tool "harnmem_forget" (nlk:json-object "id" "mem_missing")))
                   ("no memory mem_missing in the store")))))

(deftest harnmem-cell-a-missing-argument-and-a-read-only-store-refuse ()
  (with-cell-stop ((harnmem-start "home" (harnmem-scratch-home)))
    (is-carrying (answer (harnmem-tool "harnmem_write"
                                        (nlk:json-object "type" "user" "description" "d" "content" "c")))
                 ("title is required"))
    (is-carrying (answer (harnmem-tool "harnmem_write"
                                        (nlk:json-object "type" "gossip" "title" "t"
                                                         "description" "d" "content" "c")))
                 ("type must be one of")))
  (with-cell-stop ((harnmem-start "home" (harnmem-scratch-home) "read_only" t))
    (is-carrying (answer (harnmem-write "Any fact" "a fact"))
                 ("read-only" "read_only=true"))
    (is (zerop (length (nodecode-harnmem::shard-records "user"))) "and nothing was written")))

;;; --- what the model is handed -------------------------------------------------------

(deftest harnmem-cell-the-snapshot-is-stable-and-budgeted ()
  (with-cell-stop ((harnmem-start "home" (harnmem-scratch-home)
                                  "max_inject_tokens" 80 "hot_entries" 2))
    (harnmem-write "First fact" "one line about the first")
    (harnmem-write "Second fact" "one line about the second")
    (harnmem-write "Third fact" "one line about the third")
    (let ((section (harnmem-section)))
      (is-present section "the snapshot rides the standing sections")
      (is (uiop:string-prefix-p "## HarnMem memory (cross-harness; stable snapshot)" section))
      (is (search "Use harnmem_search for details" section) "it names the tool to reach for")
      (is (equal section (harnmem-section)) "two reads, the same bytes")
      (is (<= (harnmem-bullets section) 2)
          (format nil "hot_entries caps the lines (got ~d)" (harnmem-bullets section)))
      (is (< (length section) 1200) "a small store stays a small block"))))

(deftest harnmem-cell-the-snapshot-is-capped ()
  (with-cell-stop ((harnmem-start "home" (harnmem-scratch-home) "max_inject_chars" 240))
    (harnmem-write "A long one"
                   (format nil "~{~a~^ ~}" (make-list 6 :initial-element "a description long enough to overflow the cap on its own and keep going")))
    (let ((section (harnmem-section)))
      (is-present section "the snapshot is still there")
      (is (< (length section) 640) "a char cap cuts the block"))))

;;; --- /harnmem ----------------------------------------------------------------------

(deftest harnmem-cell-the-command-shows-the-store-or-searches-it ()
  (with-cell-stop ((harnmem-start "home" (harnmem-scratch-home)))
    (is (search "HarnMem store at" (cell-entry "nodecode-harnmem" "harnmem" ""))
        "no argument shows the store")
    (harnmem-write "Lives in Oslo" "says so when it comes up")
    (let ((answer (cell-entry "nodecode-harnmem" "harnmem" "oslo user lives")))
      (is (search "memories match" answer))
      (is (search "Lives in Oslo" answer)))
    (let* ((id (nlk:json-value (harnmem-json (harnmem-write "Works in Lisp" "a second fact")) :string "id"))
           (answer (cell-entry "nodecode-harnmem" "harnmem" (format nil "read ~a" id))))
      (is (search "[user] Works in Lisp" answer))
      (is (search "a second fact" answer)))))
