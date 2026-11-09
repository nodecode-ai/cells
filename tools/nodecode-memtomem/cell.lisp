;;;; cell.lisp --- memtomem: markdown notes the model can save, search and recall.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A rewrite of memtomem's Hermes plugin (see NOTICE). The original starts a
;;;; Python MCP server that indexes markdown notes into SQLite and serves five
;;;; tools over it; this cell reads the same notes straight off disk, scores
;;;; them the way the original's default does (BM25 over the note sections)
;;;; and writes new entries in the same on-disk shape, so a store keeps being
;;;; shared with the other agents that use it.
;;;;
;;;; The store is memtomem's own: ~/.memtomem by default, notes under its
;;;; memories/ directory, one markdown file per day, one "## " section per
;;;; entry, a "> created:" / "> tags:" blockquote header under each heading.
;;;; Nothing indexes ahead of time: a search reads the notes it needs.

(defpackage #:nodecode-memtomem
  (:use #:cl)
  (:export #:memtomem-error #:memtomem-error-detail))

(in-package #:nodecode-memtomem)

(nlk:define-peripheral memtomem :not-running t)

;;; --- the store on disk ----------------------------------------------------------

(defparameter +note-types+ '("md" "markdown")
  "What a memtomem store keeps its notes in.")

(defparameter +max-files+ 5000
  "How many note files one read walks before it stops.")

(defparameter +max-depth+ 12
  "How deep under the notes directory one read walks.")

(defun home-directory ()
  (or (uiop:getenv "HOME") (uiop:getenv "USERPROFILE") ""))

(defun expand-path (text)
  "TEXT with a leading ~ replaced by the home directory."
  (let ((text (or text "")))
    (cond ((string= text "~") (home-directory))
          ((and (> (length text) 1)
                (char= #\~ (char text 0))
                (member (char text 1) '(#\/ #\\) :test #'char=))
           (concatenate 'string (home-directory) (subseq text 1)))
          (t text))))

(defun expand-directory (text)
  "TEXT as a directory pathname named the way the operator typed it."
  (uiop:ensure-directory-pathname
   (uiop:parse-native-namestring (expand-path text))))

(defun root-directory ()
  "The store this cell reads: the section's root, ~/.memtomem unless set."
  (expand-directory (or (let ((given (setting :root))) (and given given)) "~/.memtomem")))

(defun notes-directory ()
  "Where the notes are: the section's notes_dir, else <root>/memories."
  (let ((given (setting :notes-dir)))
    (if (and given (plusp (length (string-trim '(#\Space #\Tab) given))))
        (expand-directory given)
        (merge-pathnames "memories/" (root-directory)))))

(defun check-config ()
  "Refuse a section whose store can never be found; name the member."
  (dolist (pair (list (cons "root" (root-directory)) (cons "notes_dir" (notes-directory))))
    (unless (uiop:absolute-pathname-p (cdr pair))
      (error 'nlk:config-refusal
             :detail (format nil "memtomem: \"~a\" must be an absolute path, or start with ~~: ~a"
                              (car pair) (uiop:native-namestring (cdr pair)))))))

(defun note-file-p (file)
  (member (string-downcase (or (pathname-type file) "")) +note-types+ :test #'equal))

(defun note-files (directory)
  "Every note file under DIRECTORY, recursively, in a stable order."
  (let ((files '()))
    (labels ((walk (dir depth)
               (when (and (< (length files) +max-files+) (< depth +max-depth+))
                 (dolist (file (ignore-errors (uiop:directory-files dir)))
                   (when (and (< (length files) +max-files+) (note-file-p file))
                     (push file files)))
                 (dolist (sub (ignore-errors (uiop:subdirectories dir)))
                   (walk sub (1+ depth))))))
      (walk (uiop:ensure-directory-pathname directory) 0))
    (sort (nreverse files) #'string< :key #'uiop:native-namestring)))

;;; --- reading the notes ------------------------------------------------------------

(defstruct (note-entry (:constructor make-note-entry (file heading created tags text)))
  "One section of one note file: where it lives, its heading, the stamp and tags
its blockquote header carries, and its prose."
  (file "" :read-only t)
  (heading "" :read-only t)
  (created "" :read-only t)
  (tags '() :read-only t)
  (text "" :read-only t))

(defun line-prefix-p (prefix line)
  (and (stringp line) (>= (length line) (length prefix))
       (string= prefix line :end2 (length prefix))))

(defun iso-utc (&optional (time (get-universal-time)))
  "TIME as memtomem stamps an entry: UTC, to the second, its own offset spelled out."
  (multiple-value-bind (second minute hour day month year) (decode-universal-time time 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d+00:00"
            year month day hour minute second)))

(defun metadata-value (line key)
  "LINE's value when it is the blockquote header line for KEY, else NIL."
  (let ((prefix (concatenate 'string "> " key ":")))
    (when (line-prefix-p prefix line)
      (string-trim '(#\Space #\Tab #\Return) (subseq line (length prefix))))))

(defun parse-tag-list (value)
  "VALUE — a JSON array of tags, or a comma-separated list — as a list of strings."
  (let ((text (string-trim '(#\Space #\Tab #\Return) (or value ""))))
    (when (and (>= (length text) 2)
               (char= (char text 0) #\[)
               (char= (char text (1- (length text))) #\]))
      (setf text (subseq text 1 (1- (length text)))))
    (let ((tags '()) (start 0))
      (loop for index from 0 to (length text)
            do (when (or (= index (length text)) (char= (char text index) #\,))
                 (let ((item (string-trim '(#\Space #\Tab #\" #\') (subseq text start index))))
                   (when (plusp (length item)) (push item tags)))
                 (setf start (1+ index))))
      (nreverse tags))))

(defun file-stamp (file)
  "FILE's write date as the ISO stamp an entry carries, or NIL when it cannot be read."
  (let ((date (ignore-errors (file-write-date file))))
    (and date (iso-utc date))))

(defun entries-of-file (file)
  "FILE's entries: one per \"## \" section, plus its preamble when it has text."
  (let ((lines (handler-case (uiop:split-string (uiop:read-file-string file)
                                                 :separator '(#\Newline))
                 (error () nil)))
        (heading nil)
        (created nil)
        (tags nil)
        (body '())
        (entries '()))
    (macrolet ((flush ()
                 '(progn
                    (let ((text (string-trim '(#\Space #\Tab #\Return)
                                              (format nil "~{~a~^ ~}" (nreverse body)))))
                      (when (or (and heading (plusp (length heading)))
                                (plusp (length text)))
                        (push (make-note-entry (uiop:native-namestring file)
                                               (or heading "")
                                               (or created (file-stamp file) "")
                                               (nreverse tags)
                                               text)
                              entries)))
                    (setf body '() heading nil created nil tags '()))))
      (dolist (raw lines)
        (let ((line (string-right-trim '(#\Return) raw)))
          (cond ((line-prefix-p "## " line)
                 (flush)
                 (setf heading (string-trim '(#\Space) (subseq line 3))))
                ((metadata-value line "created")
                 (setf created (metadata-value line "created")))
                ((metadata-value line "tags")
                 (setf tags (parse-tag-list (metadata-value line "tags"))))
                ((and (null heading) (null body) (line-prefix-p "# " line))
                 (setf heading (string-trim '(#\Space) (subseq line 2))))
                (t (push line body)))))
      (flush)
      (nreverse entries))))

(defun store-entries ()
  "Every entry in the store, bounded by the section's byte budget."
  (let ((entries '())
        (bytes 0)
        (budget (setting :max-bytes)))
    (dolist (file (note-files (notes-directory)))
      (let ((size (or (ignore-errors
                        (with-open-file (stream file :element-type '(unsigned-byte 8))
                          (file-length stream)))
                      0)))
        (when (<= (+ bytes size) budget)
          (incf bytes size)
          (setf entries (nconc entries (entries-of-file file))))))
    entries))

(defun display-file (file)
  "FILE as the model reads it: relative to the notes directory when it is under it."
  (let* ((native (uiop:native-namestring file))
         (notes (uiop:native-namestring (notes-directory))))
    (if (and (>= (length native) (length notes))
             (string= notes native :end2 (length notes)))
        (subseq native (length notes))
        native)))

(defun entry-source (entry)
  (display-file (note-entry-file entry)))

(defun entry-label (entry)
  "ENTRY's one-line address: its file, and its heading when it has one."
  (format nil "~a~@[ » ~a~]" (entry-source entry)
          (let ((heading (note-entry-heading entry)))
            (and heading (plusp (length heading)) heading))))

(defun snippet (text limit)
  "TEXT on one line, cut to LIMIT characters with an ellipsis when it was longer."
  (let* ((flat (string-trim '(#\Space)
                             (with-output-to-string (out)
                               (let ((space nil))
                                 (loop for character across (or text "")
                                       do (if (member character '(#\Space #\Tab #\Newline #\Return))
                                              (setf space t)
                                              (progn (when space (write-char #\Space out))
                                                     (setf space nil)
                                                     (write-char character out))))))))
         (flat (if (plusp (length flat)) flat "(no text)")))
    (if (> (length flat) limit)
        (concatenate 'string (subseq flat 0 limit) "…")
        flat)))

;;; --- search: BM25 over the sections -----------------------------------------------

(defparameter +k1+ 1.5
  "BM25's term-frequency saturation, as the original's default ranker uses.")

(defparameter +b+ 0.75
  "BM25's length normalisation.")

(defparameter +stops+
  '("the" "and" "for" "with" "that" "this" "from" "into" "was" "were" "has"
    "have" "had" "not" "but" "you" "your" "are" "its" "it" "of" "to" "in" "on"
    "is" "be" "as" "at" "by" "or" "an" "we" "they" "them" "their" "there" "when"
    "what" "which" "who" "how" "why" "all" "any" "can" "will" "would" "should"
    "could" "may" "might" "do" "does" "did" "so" "if" "then" "than" "too" "very"
    "also" "just" "only" "some" "more" "most" "much" "many" "one" "two" "out" "up"
    "down" "over" "under" "about" "after" "before" "between" "during" "while"
    "because" "both" "each" "other" "same" "such" "no" "nor" "own" "per" "via")
  "Words a query and a note both carry without saying anything.")

(defun text-tokens (text)
  "TEXT as lower-case word tokens of two characters or more, stopwords dropped."
  (let ((tokens '())
        (current (make-string-output-stream)))
    (flet ((flush ()
             (let ((token (get-output-stream-string current)))
               (when (and (>= (length token) 2)
                          (not (member token +stops+ :test #'equal)))
                 (push token tokens)))))
      (loop for character across (string-downcase (or text ""))
            do (if (alphanumericp character)
                   (write-char character current)
                   (flush)))
      (flush))
    (nreverse tokens)))

(defun document-text (entry)
  (format nil "~@[~a~] ~@[~a~]" (note-entry-heading entry) (note-entry-text entry)))

(defun bm25-scores (query documents)
  "DOCUMENTS — (ENTRY . TOKENS) pairs — scored against QUERY's tokens.
Answers ((SCORE . ENTRY) ...) with a positive score, best first."
  (let* ((terms (remove-duplicates (text-tokens query) :test #'equal))
         (count (length documents))
         (average (if (plusp count)
                      (/ (reduce #'+ documents :key (lambda (document) (length (cdr document))))
                         (max 1 count))
                      1))
         (scored '()))
    (when (and (plusp count) terms)
      (dolist (document documents)
        (let ((tokens (cdr document))
              (score 0.0))
          (dolist (term terms)
            (let ((frequency (count term tokens :test #'equal)))
              (when (plusp frequency)
                (let* ((holders (count-if (lambda (other) (member term (cdr other) :test #'equal))
                                          documents))
                       (idf (log (1+ (/ (- count holders -0.5) (+ holders 0.5)))))
                       (length-factor (if (plusp average) (/ (length tokens) average) 1)))
                  (incf score (* idf (/ (* frequency (+ +k1+ 1))
                                        (+ frequency (* +k1+ (+ (- 1 +b+) (* +b+ length-factor)))))))))))
          (when (plusp score) (push (cons score (car document)) scored))))
      (sort scored #'> :key #'car))))

(defun search-entries (query limit)
  "The LIMIT entries that best match QUERY, best first."
  (let ((scored (bm25-scores query
                              (mapcar (lambda (entry)
                                        (cons entry (text-tokens (document-text entry))))
                                      (store-entries)))))
    (subseq scored 0 (min limit (length scored)))))

;;; --- writing an entry --------------------------------------------------------------

(defun tags-json (tags)
  (format nil "[~{\"~a\"~^, ~}]" tags))

(defun entry-block (content &key title tags)
  "The block memtomem's own writer appends for one entry: a heading (its own when
CONTENT starts with one), the \"> created:\" / \"> tags:\" blockquote header, and
the body. The shape is the original's tools/memory_writer.py format_entry_block."
  (let ((now (iso-utc))
        (tag-line (if tags (format nil "~%> tags: ~a" (tags-json tags)) ""))
        (stripped (string-trim '(#\Space #\Tab #\Newline #\Return) content)))
    (if (line-prefix-p "## " stripped)
        (format nil "~%> created: ~a~a~%~%~a~%" now tag-line stripped)
        (format nil "~%## ~a~%~%> created: ~a~a~%~%~a~%"
                (or title (format nil "Entry ~a" now)) now tag-line stripped))))

(defun today-file ()
  "The day file memtomem's mem_add appends to when no file is named."
  (merge-pathnames (format nil "~a.md" (subseq (iso-utc) 0 10)) (notes-directory)))

(defun write-entry (content &key title tags)
  (let ((file (today-file)))
    (ensure-directories-exist file)
    (with-open-file (stream file :direction :output :if-exists :append
                                   :if-does-not-exist :create
                                   :external-format :utf-8)
      (write-string (entry-block content :title title :tags tags) stream))
    file))

(defun similar-entry (content)
  "An existing entry that says much the same as CONTENT, or NIL."
  (let ((wanted (remove-duplicates (text-tokens content) :test #'equal)))
    (when (>= (length wanted) 5)
      (loop for entry in (store-entries)
            for held = (remove-duplicates (text-tokens (document-text entry)) :test #'equal)
            when (and (>= (length held) 5)
                      (>= (length (intersection wanted held :test #'equal))
                          (* 0.6 (min (length wanted) (length held)))))
              return entry))))

;;; --- argument plumbing -------------------------------------------------------------

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the ERROR line the model repairs from."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: memtomem: ~a" condition)))))

(defun argument-text (args name &key required default)
  (let ((value (gethash name args)))
    (cond ((and (stringp value) (plusp (length (string-trim '(#\Space #\Tab) value))))
           (string-trim '(#\Space #\Tab) value))
          (required (fail "~a is required" name))
          (t default))))

(defun argument-integer (args name default)
  (let ((value (gethash name args)))
    (if (integerp value) (max 1 (min value 200)) default)))

(defun argument-boolean (args name default)
  (multiple-value-bind (value present) (gethash name args)
    (if present (and value t) default)))

(defun argument-tags (args name)
  (let ((value (gethash name args)))
    (cond ((vectorp value) (loop for item across value when (stringp item) collect item))
          ((stringp value) (parse-tag-list (format nil "[~a]" value)))
          (t '()))))

;;; --- the tools --------------------------------------------------------------------

(defun tool-search (args)
  (answering
    (let* ((query (argument-text args "query" :required t))
           (limit (argument-integer args "max_results" (setting :max-results)))
           (hits (search-entries query limit)))
      (if (null hits)
          (format nil "No memtomem note matched ~s. mem_status shows the store; mem_index a directory to check the notes are there."
                  query)
          (with-output-to-string (out)
            (format out "~d match~:p for ~s, best first:~%" (length hits) query)
            (dolist (hit hits)
              (format out "- ~a (score ~,2f): ~a~%"
                      (entry-label (cdr hit)) (car hit)
                      (snippet (note-entry-text (cdr hit)) 200))))))))

(defun tool-recall (args)
  (answering
    (let* ((since (argument-text args "since"))
           (until (argument-text args "until"))
           (source (argument-text args "source"))
           (limit (argument-integer args "max_results" (setting :max-results)))
           (matches (remove-if-not
                     (lambda (entry)
                       (let* ((created (note-entry-created entry))
                              (day (subseq created 0 (min 10 (length created)))))
                         (and (or (null since) (string>= day since))
                              (or (null until) (string<= day until))
                              (or (null source)
                                  (search source (entry-source entry) :test #'char-equal)))))
                     (store-entries)))
           (kept (subseq (sort matches #'string> :key #'note-entry-created)
                         0 (min limit (length matches)))))
      (if (null kept)
          (format nil "No memtomem memory in that window~@[ (since ~a)~]~@[ (until ~a)~]." since until)
          (with-output-to-string (out)
            (format out "~d memor~:@p, newest first:~%" (length kept))
            (dolist (entry kept)
              (format out "- ~a ~a: ~a~%"
                      (note-entry-created entry) (entry-label entry)
                      (snippet (note-entry-text entry) 200))))))))

(defun tool-add (args)
  (answering
    (let* ((content (argument-text args "content" :required t))
           (title (argument-text args "title"))
           (tags (argument-tags args "tags"))
           (similar (similar-entry content))
           (file (write-entry content :title title :tags tags)))
      (format nil "Saved one entry to ~a (~a, ~d tag~:p).~@[ A similar memory is already there: ~a~]~%mem_search finds it by topic; mem_recall reads it by date."
              (display-file file) (iso-utc) (length tags)
              (and similar (entry-label similar))))))

(defun tool-index (args)
  (answering
    (let* ((given (argument-text args "path" :required t))
           (path (expand-directory given)))
      (unless (uiop:absolute-pathname-p path)
        (fail "path must be absolute or start with ~~: ~s" given))
      (let* ((files (if (note-file-p path) (list path) (note-files path)))
             (entries (loop for file in files nconc (entries-of-file file))))
        (format nil "Scanned ~d file~:p under ~a: ~d entr~:@p.~%Search reads the notes themselves, so there is no separate index to refresh: a new note is found as soon as it is written."
                (length files) (uiop:native-namestring path) (length entries))))))

(defun tool-status (args)
  (declare (ignore args))
  (answering
    (let* ((root (root-directory))
           (notes (notes-directory))
           (files (note-files notes))
           (entries (loop for file in files nconc (entries-of-file file)))
           (config (merge-pathnames "config.json" root)))
      (with-output-to-string (out)
        (format out "memtomem store: ~a~%notes: ~a~%"
                (uiop:native-namestring root) (uiop:native-namestring notes))
        (if (probe-file notes)
            (format out "~d note file~:p, ~d entr~:@p; search is BM25 over the sections, no index or embeddings~%"
                    (length files) (length entries))
            (format out "no notes directory yet: the first mem_add creates it~%"))
        (format out "configuration: ~a; ~a~%"
                (if (probe-file config)
                    "config.json found, so this store is shared with the other agents that read it"
                    "no config.json, so the defaults are in force (BM25, notes under the store)")
                "a fresh store needs nothing else")))))

;;; --- the manual and /memtomem ---------------------------------------------------------

(defparameter +primer+
  "memtomem keeps markdown notes in one local store (~/.memtomem unless the memtomem section
names another), shared with the other coding agents that read the same store. Nothing is sent
anywhere: a search reads the notes off disk and scores them with BM25, the original's default.
  mem_search {query, max_results?}      what the notes say about a topic, best match first
  mem_recall {since?, until?, source?, max_results?}   what was saved in a date range, newest first
  mem_add {content, title?, tags?}      save one entry the user asked you to remember
  mem_index {path}                      check a directory of notes; absolute path or ~
  mem_status                            where the store is and how much it holds
Call mem_search before answering a question the notes may already settle, and present the
strongest matches with their file, heading and score. Save only what the user asked you to
persist, and leave title and tags to what the content itself makes clear. For mem_index give
the path the user named, absolute or starting with ~: a relative path is refused here."
  "What (help :memtomem) answers while the cell runs.")

(defun status-line ()
  (format nil "memtomem: store ~a, notes ~a, ~d note file~:p; /memtomem QUERY searches it, and the tools mem_search, mem_add, mem_recall, mem_index and mem_status do the rest."
          (uiop:native-namestring (root-directory))
          (uiop:native-namestring (notes-directory))
          (length (note-files (notes-directory)))))

(defun run-command (args session-id)
  "One /memtomem invocation: a query answers what the notes say, no argument the store."
  (declare (ignore session-id))
  (let ((query (nlk:trimmed (or args ""))))
    (if (plusp (length query))
        (tool-search (nlk:json-object "query" query))
        (status-line))))

;;; --- schemas ----------------------------------------------------------------

(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES — (NAME TYPE DOC REQUIRED-P) ... — as a tool schema."
  (let ((fields (loop for (name type doc) in properties
                      nconc (list name (nlk:json-object "type" type
                                                         "description" doc)))))
    (list :description description
          :parameters (nlk:json-object
                       "type" "object"
                       "properties" (if fields
                                          (apply #'nlk:make-json-object fields)
                                          (nlk:json-object))
                       "required" (coerce (loop for property in properties
                                                 when (fourth property)
                                                   collect (first property))
                                          'vector)))))

(defparameter *search-schema*
  (tool-schema
   "Search the memtomem notes by topic: past decisions, notes and context this or another coding agent saved in the shared store. Use it before answering anything the notes may settle. It answers the strongest sections with their file, heading and score (BM25, the store's default ranker)."
   '(("query" "string" "What to look for, in the user's words." t)
     ("max_results" "integer" "How many hits to answer with (default: the section's max_results)." nil)))
  "The mem_search tool, as the original's MCP server describes it.")

(defparameter *recall-schema*
  (tool-schema
   "Recall the memtomem memories created in a date range — for \"what did we decide last week\", not for what a memory says (that is mem_search). It answers newest first, with each entry's creation date, file and heading."
   '(("since" "string" "Earliest date wanted, YYYY-MM-DD (translate the user's \"last week\" first)." nil)
     ("until" "string" "Latest date wanted, YYYY-MM-DD." nil)
     ("source" "string" "Only memories whose file path contains this text." nil)
     ("max_results" "integer" "How many memories to answer with (default: the section's max_results)." nil)))
  "The mem_recall tool.")

(defparameter *add-schema*
  (tool-schema
   "Save one memory to the memtomem store the other coding agents share, as one markdown entry: the heading, a created stamp, the tags and the text. Call it only when the user clearly asks to remember, record or persist something, and leave force and scope alone — this writes the user-scope store."
   '(("content" "string" "The memory itself, in the user's words." t)
     ("title" "string" "A short heading; one is made from the content when left out." nil)
     ("tags" "array" "A few lowercase tags the content makes clear, at most six." nil)))
  "The mem_add tool.")

(defparameter *index-schema*
  (tool-schema
   "Check a directory (or one file) of notes in the memtomem store and report how many files and entries it holds. Search reads the notes live, so there is no index to build; call this after the user points at a notes directory, and give the path they named, absolute or starting with ~."
   '(("path" "string" "The file or directory to check: an absolute path, or one starting with ~." t)
     ("force" "boolean" "Accepted for the original's shape; a read here rebuilds nothing." nil)
     ("auto_tag" "boolean" "Accepted for the original's shape; no tags are added." nil)))
  "The mem_index tool.")

(defparameter *status-schema*
  (tool-schema
   "Where the memtomem store is, how many notes and entries it holds, and what search is doing (BM25 over the files, no embeddings, nothing sent anywhere). Use it when a search comes back empty or the user asks what memory holds."
   '())
  "The mem_status tool.")

;;; --- the cell ------------------------------------------------------------------

(nle:define-cell memtomem
  (:section ("memtomem")
    (:guide "root is memtomem's own store (~/.memtomem); notes_dir defaults to <root>/memories; a fresh store needs no setup; next step: /memtomem shows the store and how many notes it holds")
    ("root" :path :default "~/.memtomem"
     :doc "the memtomem store: its notes, its config.json and its index live here")
    ("notes_dir" :path
     :doc "the markdown notes this cell reads and writes; default <root>/memories")
    ("max_results" :integer :default 8 :min 1
     :doc "how many hits one search answers with unless the call asks otherwise")
    ("max_bytes" :integer :default 4000000 :min 1000
     :doc "how much of the notes one read takes in before it stops"))
  (:help :memtomem "memtomem: mem_search finds notes by topic, mem_add saves one, mem_recall reads a date range" +primer+)
  (:command "memtomem" 'run-command
            :description "Show the memtomem store, or search it"
            :argument-hint "QUERY")
  (:tool "mem_search" *search-schema* 'tool-search)
  (:tool "mem_recall" *recall-schema* 'tool-recall)
  (:tool "mem_add" *add-schema* 'tool-add)
  (:tool "mem_index" *index-schema* 'tool-index)
  (:tool "mem_status" *status-schema* 'tool-status)
  (:start (lambda ()
            (check-config)
            (nle:on-stop (lambda () nil)))))
