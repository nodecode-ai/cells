;;;; cell.lisp --- durable memory: a manifest every turn, prefetch, recall.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of Nachos' three-tier memory (see NOTICE). One markdown file
;;;; holds every entry, grouped by category and hand-editable:
;;;;
;;;;     # <category>
;;
;;;;     ## <entry title>
;;;;     <first line is the summary>
;;;;     <the rest of the body>
;;;;
;;;; Three tiers read it. The manifest is one line per entry, injected into
;;;; every request as the `nachos' harness section and never truncating an
;;;; entry away: over its budget summaries shorten, then go, and every title
;;;; stays, so the model always knows an entry exists. Prefetch runs at each
;;;; round (:ROUND), ranks entries against the turn's own words by hand-rolled
;;;; TF-IDF over titles and summaries -- no network, no LLM -- and hands the
;;;; best few bodies to the request inside a character budget, marking them in
;;;; the manifest. Recall is the nachos_memory_recall tool: a whole entry by
;;;; key, or a substring search, on demand.
;;;;
;;;; Settings are all optional: with no `nachos' section the cell keeps its
;;;; memory at nachos/memory.md under the Nodecode home. Nothing here opens a
;;;; socket; the only I/O is that one file.

(in-package #:nodecode-nachos)

;;; --- the manual ---------------------------------------------------------------

(defparameter +manual+
  "Nachos keeps durable memory for you: a local markdown file of entries, each a
title, a one-line summary and a body, filed under a category.
  nachos_memory_put     add or replace an entry: title (its key is the slug of
      it), body, and optionally summary and category. Write the body
      summary-first; an omitted summary is the body's first line.
  nachos_memory_recall  key: the whole entry. query: every entry whose title,
      summary or body contains it, up to ten.
  nachos_memory_remove  delete an entry by key or title.
Every request carries the manifest -- one line per entry, grouped by category;
a title marked ► has its body inlined under `Recalled memory' this turn.
Recall an entry by key rather than asking for the whole store; the manifest is
the index, the tools are the reading. /nachos says where the store is, lists
the manifest, or recalls and removes by key. The store is one file
(nachos.store_file), so it survives every session, and hand edits are read
back as they stand."
  "What (help :nachos) answers while the cell runs.")

(defparameter *contract*
  (format nil "Nachos is the durable memory for this machine: an entry is kept in ~
               a local markdown store (# ~~a), and the manifest below is its index. Write with ~
               nachos_memory_put, read a whole entry with nachos_memory_recall (by key, or by ~
               query), delete with nachos_memory_remove. Nothing here leaves the disk."
          "nachos.store_file")
  "The line every request carries above the manifest.")

;;; --- words, data over code ----------------------------------------------------------

(defparameter *manifest-head* "# Memory Manifest")
(defparameter *manifest-hint*
  "(Index only --- a whole entry comes from nachos_memory_recall. ► = prefetched this turn.)")
(defparameter *prefetched-mark* "► ")
(defparameter *ellipsis* "…")
(defparameter *summary-caps* '(120 90 60 40 24 0)
  "How the manifest shortens summaries, in turn, when it is over budget.")
(defparameter *recalled-head* "# Recalled memory (relevant to this turn)")
(defparameter *recall-limit* 10
  "How many entries one query answers with.")
(defparameter *line-chars* 200
  "How much of an entry's first line becomes its summary when none is given.")

;;; --- the store: one markdown file ----------------------------------------------

(defstruct (entry (:constructor make-entry (key title summary category body)))
  (key "" :type string)
  (title "" :type string)
  (summary "" :type string)
  (category "general" :type string)
  (body "" :type string))

(defun slugify (title)
  "TITLE as an entry key: lowercase letters and digits, runs of anything else
one dash, no dash at either end; `untitled' when nothing is left."
  (let ((text (string-downcase (nlk:trimmed (or title ""))))
        (out (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        (gap nil))
    (loop for char across text
          do (if (or (char<= #\a char #\z) (char<= #\0 char #\9))
                 (progn (when (and gap (plusp (fill-pointer out)))
                          (vector-push-extend #\- out))
                        (setf gap nil)
                        (vector-push-extend char out))
                 (setf gap t)))
    (let ((slug (coerce out 'string)))
      (if (plusp (length slug)) slug "untitled"))))

(defun heading (line level)
  "The title LINE carries as a heading of exactly LEVEL hashes, else NIL."
  (let ((start (1+ level)))
    (when (and (> (length line) start)
               (loop for index below level always (char= (char line index) #\#))
               (member (char line level) '(#\Space #\Tab) :test #'char=)
               (not (member (char line start) '(#\Space #\Tab #\Return) :test #'char=)))
      (string-trim '(#\Space #\Tab #\Return) (subseq line start)))))

(defun first-line (body)
  "BODY's first non-blank line, trimmed: an entry's summary."
  (loop for line in (nlk:lines (or body ""))
        for text = (nlk:trimmed line)
        when (plusp (length text)) return text))

(defun store-path (given)
  "The store file GIVEN names: an absolute path as it stands, a relative one
under the Nodecode home, an empty one nachos/memory.md there."
  (let ((text (nlk:trimmed (or given ""))))
    (if (plusp (length text))
        (let ((path (pathname text)))
          (if (uiop:absolute-pathname-p path) path (nlk:home text)))
        (nlk:home "nachos/memory.md"))))

(defun store-file ()
  "The markdown file the entries live in: nachos.store_file through STORE-PATH."
  (store-path (princ-to-string (setting :store-file))))

(defun entries (&optional (path (store-file)))
  "Every entry in the store at PATH, the last of two entries with one key
standing. A missing file is an empty store."
  (let ((found '()) (category "general") (title nil) (body '()))
    (labels ((flush ()
               (when title
                 (let* ((text (string-trim '(#\Space #\Tab #\Newline #\Return)
                                            (format nil "~{~a~^~%~}" (nreverse body))))
                        (key (slugify title)))
                   (setf found (cons (make-entry key title (first-line text) category text)
                                     (remove key found :key #'entry-key :test #'equal)))))))
      (when (probe-file path)
        (dolist (line (uiop:read-file-lines path))
          (cond ((heading line 1)
                 (flush)
                 (setf title nil body '() category (heading line 1)))
                ((heading line 2)
                 (flush)
                 (setf title (heading line 2) body '()))
                (title (push line body))))
        (flush)))
    (nreverse found)))

(defun write-entries (items &optional (path (store-file)))
  "Write ITEMS to PATH whole, grouped by category and titled alphabetically,
one entry after its `## ' heading. => PATH."
  (let ((by-category (make-hash-table :test #'equal)))
    (dolist (item items)
      (push item (gethash (entry-category item) by-category '())))
    (let ((out '()))
      (dolist (category (sort (loop for key being the hash-keys of by-category collect key)
                               #'string-lessp))
        (push (format nil "# ~a" category) out)
        (push "" out)
        (dolist (item (sort (copy-list (gethash category by-category))
                             (lambda (a b)
                               (string-lessp (string-downcase (entry-title a))
                                             (string-downcase (entry-title b))))))
          (push (format nil "## ~a" (entry-title item)) out)
          (let ((body (string-trim '(#\Space #\Tab #\Newline #\Return) (entry-body item))))
            (when (plusp (length body)) (push body out)))
          (push "" out)))
      (ensure-directories-exist path)
      (nlk:write-file-atomically
       path (format nil "~a~%" (string-right-trim '(#\Space #\Newline) (format nil "~{~a~^~%~}" (nreverse out))))
       :mode #o600 :directory-mode #o700)
      path)))

(defun store-entry (key &key title summary category body)
  "Put one entry, KEY's older version replaced. The body leads with SUMMARY so
a read-back shows what the manifest said; a heading inside a title, category
or body is refused, since it would be read as structure."
  (let ((title (nlk:trimmed (or title "")))
        (category (let ((text (nlk:trimmed (or category ""))))
                    (if (plusp (length text)) text "general")))
        (summary (nlk:trimmed (or summary "")))
        (body (string-trim '(#\Space #\Tab #\Newline #\Return) (or body ""))))
    (when (find #\Newline title) (fail "an entry title is one line: ~s" title))
    (when (find #\Newline category) (fail "an entry category is one line: ~s" category))
    (when (and (plusp (length summary)) (not (equal (first-line body) summary)))
      (setf body (format nil "~a~@[~%~%~a~]" summary (and (plusp (length body)) body))))
    (dolist (line (nlk:lines body))
      (when (or (heading line 1) (heading line 2))
        (fail "a heading line in a body would be read as structure: ~s" (nlk:trimmed line))))
    (write-entries (cons (make-entry key title (if (plusp (length summary)) summary (first-line body))
                                     category body)
                         (remove key (entries) :key #'entry-key :test #'equal)))
    key))

(defun search-keys (query &optional (items (entries)))
  "The keys of ITEMS whose title, summary or body holds QUERY, by category and
title. An empty query matches nothing."
  (let ((needle (string-downcase (nlk:trimmed (or query "")))))
    (when (plusp (length needle))
      (mapcar #'entry-key
              (sort (remove-if-not
                     (lambda (item)
                       (search needle (string-downcase
                                       (format nil "~a~%~a~%~a" (entry-title item)
                                               (entry-summary item) (entry-body item)))))
                     items)
                    #'entry-lessp)))))

(defun entry-lessp (a b)
  "Order entries by category, then title, case-insensitively."
  (let ((left (string-downcase (entry-category a)))
        (right (string-downcase (entry-category b))))
    (if (equal left right)
        (string-lessp (string-downcase (entry-title a)) (string-downcase (entry-title b)))
        (string-lessp left right))))

;;; --- tier 1: the manifest ---------------------------------------------------------

(defun manifest-lines (items caps)
  "ITEMS as manifest lines, summaries cut at CAPS characters (NIL: whole)."
  (let ((out (list *manifest-head* *manifest-hint*))
        (category nil))
    (dolist (item (sort (copy-list items) #'entry-lessp))
      (unless (equal category (entry-category item))
        (setf category (entry-category item))
        (push "" out)
        (push (format nil "## ~a" category) out))
      (let* ((summary (entry-summary item))
             (mark (if (gethash (entry-key item) *prefetched*) *prefetched-mark* "")))
        (when (and caps (> (length summary) caps))
          (setf summary (format nil "~a~a"
                                (string-right-trim '(#\Space)
                                                   (if (plusp caps) (subseq summary 0 (1- caps)) ""))
                                *ellipsis*)))
        (push (if (plusp (length summary))
                  (format nil "- ~a~a --- ~a" mark (entry-title item) summary)
                  (format nil "- ~a~a" mark (entry-title item)))
              out)))
    (nreverse out)))

(defun render-manifest (items &key (budget nil))
  "ITEMS as the whole manifest: every entry on one line, never dropped. Over
BUDGET the summaries shorten in turn, and at last go, title alone; the budget
never removes a line."
  (if (null items)
      ""
      (let ((whole (format nil "~{~a~^~%~}" (manifest-lines items nil))))
        (if (or (null budget) (<= (length whole) budget))
            whole
            (or (loop for caps in *summary-caps*
                      for text = (format nil "~{~a~^~%~}" (manifest-lines items caps))
                      when (<= (length text) budget) return text)
                (format nil "~{~a~^~%~}" (manifest-lines items 0)))))))

;;; --- tier 2: prefetch -------------------------------------------------------------

(defvar *prefetched* (make-hash-table :test #'equal)
  "The keys whose bodies the last round inlined; the manifest marks them.")

(defun tokens (text)
  "TEXT's lowercase runs of letters and digits."
  (let ((out '()) (start nil))
    (loop for index from 0 below (length text)
          for char = (char text index)
          for word = (or (char<= #\a char #\z) (char<= #\0 char #\9))
          do (cond ((and word (null start)) (setf start index))
                   ((and (not word) start)
                    (push (subseq text start index) out)
                    (setf start nil)))
          finally (when start (push (subseq text start) out)))
    (nreverse out)))

(defun rank (query candidates top-n)
  "CANDIDATES scored against QUERY by TF-IDF over title and summary, best
first, at most TOP-N keys; ties keep the store's own order."
  (let* ((terms (remove-duplicates (tokens (string-downcase (or query ""))) :test #'equal))
         (docs (mapcar (lambda (item)
                         (tokens (string-downcase (format nil "~a ~a"
                                                          (entry-title item) (entry-summary item)))))
                       candidates))
         (count (length docs)))
    (when (and terms (plusp count))
      (let* ((frequency (mapcar (lambda (term)
                                   (cons term (count-if (lambda (doc) (member term doc :test #'equal)) docs)))
                                 terms))
             (weight (mapcar (lambda (pair)
                               (cons (car pair) (+ 1.0d0 (log (/ (1+ count) (1+ (cdr pair)))))))
                             frequency))
             (scored '()))
        (loop for item in candidates
              for doc in docs
              for index from 0
              do (let ((size (length doc)))
                   (when (plusp size)
                     (let ((score 0.0d0))
                       (dolist (pair weight)
                         (let ((hits (count (car pair) doc :test #'equal)))
                           (when (plusp hits)
                             (incf score (* (/ hits size) (cdr pair))))))
                       (when (plusp score) (push (list score index (entry-key item)) scored))))))
        (mapcar #'third
                (loop for (score index key) in (sort scored (lambda (a b)
                                                               (if (= (first a) (first b))
                                                                   (< (second a) (second b))
                                                                   (> (first a) (first b)))))
                      collect (list score index key)))))))

(defun prefetch-text (query)
  "The bodies of the entries most relevant to QUERY, under nachos.prefetch_chars,
headed as recalled memory; NIL when nothing ranks. Sets *PREFETCHED*."
  (let* ((items (entries))
         (hits (search-keys query items))
         (candidates (if hits (remove-if-not (lambda (item) (member (entry-key item) hits :test #'equal))
                                             items)
                         items))
         (keys (rank query candidates (setting :prefetch-top-n)))
         (budget (setting :prefetch-chars))
         (used (length *recalled-head*))
         (blocks '()))
    (clrhash *prefetched*)
    (dolist (key keys)
      (let* ((item (find key items :key #'entry-key :test #'equal))
             (block (and item (format nil "~%## ~a~%~a" (entry-title item)
                                      (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                   (entry-body item))))))
        (when block
          (if (> (+ used (length block)) budget)
              (return)
              (progn (setf (gethash key *prefetched*) t)
                     (push block blocks)
                     (incf used (length block)))))))
    (when blocks
      (format nil "~a~{~a~}" *recalled-head* (nreverse blocks)))))

(defun turn-words (session-id turn-id)
  "The words TURN-ID was admitted with, or an empty string: the turn-started
fact's payload, whose `input' the harness wrote."
  (let ((payload (ignore-errors
                   (nlk:events :session-id session-id :turn-id turn-id
                               :kind nlk::+kind-turn-started+
                               :as :value :columns '("payload")))))
    (or (and payload (ignore-errors (nlk:json-value (nlk:decode-json payload) :string "input")))
        "")))

(defun prefetch-round (op)
  "At a round: the entries this turn's words rank highest, as one block in
the round's request."
  (let* ((session (getf op :session-id))
         (turn (getf op :turn))
         (turn-id (and turn (nlk:durable-turn-turn-id turn)))
         (words (and turn-id (turn-words session turn-id)))
         (text (and words (plusp (length (nlk:trimmed words))) (prefetch-text words))))
    (when (and text turn) (nlk:record-turn-context-prompt turn :text text))
    text))

;;; --- the standing section ---------------------------------------------------------

(defun memory-section ()
  "The `nachos' harness section: the contract and the whole manifest."
  (let ((items (handler-case (entries) (error () nil))))
    (format nil "~a~@[~%~%~a~]" *contract* (and items (render-manifest items :budget (setting :manifest-chars))))))

(defun standing-sections (next session-id)
  "The harness sections with `nachos' supplied whole, whatever a sibling says."
  (acons "nachos" (memory-section)
         (remove "nachos" (funcall next session-id) :key #'car :test #'equal)))

;;; --- tier 3: the tools ------------------------------------------------------------

(nlk:define-contract recall-call
  (:doc "Read durable memory: an exact entry by key, or every entry whose title, summary or body matches a query.")
  ("key" :text :doc "the entry's key, as the manifest shows it: the slug of its title")
  ("query" :text :doc "substring searched over titles, summaries and bodies"))

(nlk:define-contract put-call
  (:doc "Add or replace a durable memory entry. Its key is the slug of its title. Write the body summary-first: an omitted summary is the body's first line, and the category groups the entry in the manifest.")
  ("title" :text :required t :doc "a short label, one line; it becomes the key")
  ("body" :text :required t :doc "the whole entry; its first non-blank line is the summary when none is given")
  ("summary" :text :doc "one keyword-dense line for the manifest")
  ("category" :text :doc "the manifest group; `general' when absent"))

(nlk:define-contract remove-call
  (:doc "Delete a durable memory entry by its key, or by its title (slugified to one).")
  ("key" :text :doc "the entry's key")
  ("title" :text :doc "the entry's title, slugified to its key"))


(defmacro with-refusal ((format-control) &body body)
  "BODY's answer; a refusal or an error becomes an object the model reads."
  `(handler-case (progn ,@body)
     (error (condition)
       (nlk:encode-json-object (nlk:json-object "error" (format nil ,format-control condition))))))

(defun tool-recall (call)
  "nachos_memory_recall: an entry by key, or a query's matches."
  (with-refusal ("~a")
    (let ((key (recall-call-key call))
          (query (recall-call-query call)))
      (cond
        (key (let ((item (find key (entries) :key #'entry-key :test #'equal)))
               (if item
                   (nlk:encode-json-object (nlk:json-object "key" key "body" (entry-body item)))
                   (nlk:encode-json-object (nlk:json-object "error" (format nil "no entry with key ~s" key))))))
        (query
         (let ((items (entries)) (found '()))
           (dolist (key (search-keys query items))
             (when (>= (length found) *recall-limit*) (return))
             (let ((item (find key items :key #'entry-key :test #'equal)))
               (when item (push (nlk:json-object "key" key "body" (entry-body item)) found))))
           (setf found (nreverse found))
           (nlk:encode-json-object
            (nlk:json-object "query" query "count" (length found) "results" (coerce found 'vector)))))
        (t (nlk:encode-json-object (nlk:json-object "error" "provide a key or a query")))))))

(defun tool-put (call)
  "nachos_memory_put: store one entry under the slug of its title."
  (with-refusal ("~a")
    (let* ((title (nlk:trimmed (put-call-title call)))
           (category (let ((text (nlk:trimmed (or (put-call-category call) ""))))
                       (if (plusp (length text)) text "general")))
           (key (slugify title)))
      (store-entry key :title title :summary (put-call-summary call) :category category
                       :body (put-call-body call))
      (nlk:encode-json-object
       (nlk:json-object "stored" key "title" title "category" category)))))

(defun tool-remove (call)
  "nachos_memory_remove: delete one entry by key or title."
  (with-refusal ("~a")
    (let* ((key (let ((key (nlk:trimmed (or (remove-call-key call) ""))))
                  (if (plusp (length key)) key
                      (let ((title (nlk:trimmed (or (remove-call-title call) ""))))
                        (and (plusp (length title)) (slugify title))))))
           (items (entries)))
      (if (null key)
          (nlk:encode-json-object (nlk:json-object "error" "provide a key or a title"))
          (progn (write-entries (remove key items :key #'entry-key :test #'equal))
                 (nlk:encode-json-object (nlk:json-object "removed" key)))))))


;;; --- /nachos ----------------------------------------------------------------------

(defun status-text ()
  "The store, the settings and the last prefetch, in one line each; no file is
written."
  (let ((items (handler-case (entries) (error () nil))))
    (format nil "nachos memory~%  store      : ~a~%  entries    : ~d~%  prefetch   : ~d entries, ~d chars~%  manifest   : ~d chars~%  prefetched : ~a"
            (namestring (store-file)) (length items)
            (setting :prefetch-top-n) (setting :prefetch-chars) (setting :manifest-chars)
            (or (let ((keys '())) (maphash (lambda (key value) (declare (ignore value)) (push key keys)) *prefetched*)
                  (and keys (format nil "~{~a~^, ~}" (sort keys #'string-lessp))))
                "-"))))

(defun panel (summary)
  "The dialog bare /nachos answers: the manifest, and a key to recall."
  (nle:list-dialog
   :title "Nachos memory" :summary summary
   :rows (list (nle:run-row "List the manifest" "/nachos list"
                             :detail "one line an entry, grouped by category")
               (nle:ask-row "Recall an entry" "Key" "/nachos recall"
                            :placeholder "the key the manifest shows" :verb "recall"
                            :detail "read the whole entry"))))

(defun run-command (args session-id)
  "/nachos typed alone: the store's status and its panel; then status, list,
recall KEY or remove KEY."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (space (position-if (lambda (char) (member char '(#\Space #\Tab))) text))
         (head (string-downcase (subseq text 0 space)))
         (rest (string-trim '(#\Space #\Tab) (if space (subseq text space) ""))))
    (flet ((found (key)
             (find key (entries) :key #'entry-key :test #'equal)))
      (cond
        ((equal text "") (let ((status (status-text))) (values status (panel status))))
        ((equal head "status") (status-text))
        ((equal head "list") (or (memory-section) "nachos memory is empty"))
        ((equal head "recall")
         (let ((item (found (slugify rest))))
           (if item (entry-body item) (format nil "nachos: no entry with key ~a" (slugify rest)))))
        ((equal head "remove")
         (let* ((key (slugify rest)) (item (found key)))
           (if item
               (progn (write-entries (remove key (entries) :key #'entry-key :test #'equal))
                      (format nil "nachos: removed ~a" key))
               (format nil "nachos: no entry with key ~a" key))))
        (t (let ((status (status-text)))
             (values (format nil "nachos: ~a is not one of its choices" head) (panel status))))))))

;;; --- the declaration ----------------------------------------------------------------

(defun open-store (&optional (given (princ-to-string (setting :store-file))))
  "Make the store's directory for GIVEN and prove the file can be written, or
refuse naming nachos.store_file."
  (let ((path (store-path given)))
    (handler-case
        (let ((probe (make-pathname :name (format nil "~a.probe" (or (pathname-name path) "memory"))
                                    :type nil :defaults path)))
          (ensure-directories-exist path)
          (with-open-file (out probe :direction :output :if-exists :supersede
                                      :if-does-not-exist :create)
            (format out "~&"))
          (ignore-errors (delete-file probe))
          (when (probe-file path) (uiop:read-file-lines path)))
      (error (condition)
        (error 'nlk:config-refusal
               :detail (format nil "store_file ~a cannot hold the memory: ~a"
                               (namestring path) condition))))))

(nle:define-cell nachos
  (:section ("nachos")
    (:guide "no key: the memory is one markdown file on this disk. store_file is where (empty is nachos/memory.md under the Nodecode home, a relative path sits there too); prefetch_top_n, prefetch_chars and manifest_chars bound what one turn carries, and every entry stays in the manifest however small manifest_chars is")
    ("store_file" :path :default ""
     :doc "the markdown file holding every entry; empty is nachos/memory.md under the Nodecode home, and a relative path is relative to it")
    ("prefetch_top_n" :integer :default 5 :min 1
     :doc "how many entries one turn's prefetch may inline bodies for")
    ("prefetch_chars" :integer :default 1500 :min 0
     :doc "the characters one turn's prefetched bodies may spend, heading included")
    ("manifest_chars" :integer :default 1200 :min 0
     :doc "the manifest's soft budget: over it summaries shorten, and entries stay"))
  (:help :nachos "nachos_memory_put, nachos_memory_recall and nachos_memory_remove: durable memory in one local markdown file, a manifest every turn" +manual+)
  (:tool "nachos_memory_recall" (nlk:contract 'recall-call) #'tool-recall)
  (:tool "nachos_memory_put" (nlk:contract 'put-call) #'tool-put)
  (:tool "nachos_memory_remove" (nlk:contract 'remove-call) #'tool-remove)
  (:hook :round (nle:observer #'prefetch-round "nachos"))
  (:command "nachos" 'run-command
            :catches 'nachos-error
            :description "Nachos durable memory: its status, the manifest, recall and remove")
  (:start (lambda ()
            (open-store)
            (nle:hook 'nle::read-harness-sections "nodecode-nachos" #'standing-sections)
            (nle:on-stop (lambda ()
                           (nle:unhook 'nle::read-harness-sections "nodecode-nachos")
                           (clrhash *prefetched*))))))
