;;;; cell.lisp --- the tools, the command, the manual and the ambient hint.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Five tools answer the model -- recall_index, recall_search, recall_open,
;;;; recall_stats, recall_forget -- and /recall answers the operator. The
;;;; harness-section hook puts the likely file in front of the model before
;;;; it answers, as the original's pre_llm_call did: a path, a date and one
;;;; highlighted line each, never the whole file, so the model still has to
;;;; read it before quoting it.

(in-package #:nodecode-recall)

;;; --- what the model is told -----------------------------------------------

(defparameter +manual+
  "recall searches the operator's own documents on this machine, through an
index you build once and refresh: no network, no embedding service, nothing
leaves the disk.

  recall_index /path [\"/abs/dir\" ...] [rebuild]  -- build or refresh it.
      Incremental: only new or changed files are re-extracted, so a repeat is
      cheap. Without paths it indexes the section's roots (~/Documents alone
      by default). Credential stores, build folders and Nodecode's own home
      are never read, whatever the settings say.
  recall_search query [limit] [path] [ext]         -- ranked files, with the
      matched line in brackets. At most 25 results.
  recall_open path [offset] [max_chars]            -- the text of one file:
      PDF (through pdftotext), Office (through unzip), Markdown, code, CSV.
      Page a long one with offset instead of taking the first window.
  recall_stats                                     -- what is indexed, where.
  recall_forget path | all                         -- drop it from the index.
      Index rows only; the file on disk is never touched.

The rule the tools exist for: search, then open the best hit, then quote it
with its path. Never name a file you have not read, and never say a document
says something you have not opened. When the index answers nothing, say so and
offer to index the folder the operator means -- do not guess a file. A path
the policy refuses comes back as a refusal: report it, do not work around it.
The same five are callable as (recall:search \"query\") and its kin from eval."
  "What (help :recall) answers while the cell runs.")

;;; --- schemas -------------------------------------------------------------

(defparameter +index-schema+
  (list :description
        (concatenate 'string
                     "Build or refresh the local file index that recall_search and "
                     "recall_open read. Incremental: only new or changed files are re-extracted, "
                     "so repeat calls are cheap. Credential stores (.env, *.pem, id_rsa, .ssh, "
                     ".aws, browser and agent auth files), build directories and Nodecode's own "
                     "home are never read. Use it before searching a folder for the first time, "
                     "or when the operator says a file is missing from results.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties"
         (nlk:make-json-object
          "path" (nlk:make-json-object
                  "type" "array" "items" (nlk:make-json-object "type" "string")
                  "description" "Absolute folders to index. Defaults to the section's roots, ~/Documents unless the operator set others; ~/Downloads and ~/Desktop are indexed only when named here.")
          "rebuild" (nlk:make-json-object
                     "type" "boolean"
                     "description" "Re-extract every file even if unchanged.")
          "budget_s" (nlk:make-json-object
                      "type" "integer"
                      "description" "Wall-clock budget for this run, in seconds. A partial run resumes next call.")
          "max_files" (nlk:make-json-object
                       "type" "integer"
                       "description" "Stop after this many files this run."))))
  "The recall_index boundary.")

(defparameter +search-schema+
  (list :description
        (concatenate 'string
                     "Search the operator's own files by the words that carry meaning (BM25 over "
                     "the local index). Use for 'where is that document about X', 'find the "
                     "invoice from Acme', 'which file mentions the delivery date'. Returns path, "
                     "modified time, a bracketed snippet and a score. Follow up with recall_open "
                     "on the best hit before quoting anything.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties"
         (nlk:make-json-object
          "query" (nlk:make-json-object
                   "type" "string"
                   "description" "Keywords or a natural phrase describing the document.")
          "limit" (nlk:make-json-object
                   "type" "integer" "description" "Max results (default 8, max 25).")
          "path" (nlk:make-json-object
                  "type" "string"
                  "description" "Only return files whose path contains this substring.")
          "ext" (nlk:make-json-object
                 "type" "string" "description" "Only return this extension, e.g. pdf or md."))
         "required" (vector "query")))
  "The recall_search boundary.")

(defparameter +open-schema+
  (list :description
        (concatenate 'string
                     "Read a file that recall_search returned, or any local document, as text: "
                     "PDF, Word, Excel, PowerPoint, Markdown, code, CSV. offset and max_chars "
                     "page through a long one instead of truncating it silently. Refuses "
                     "credential stores, Nodecode's own home and virtual filesystems, and says "
                     "which rule refused the path.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties"
         (nlk:make-json-object
          "path" (nlk:make-json-object
                  "type" "string"
                  "description" "Absolute path, or a path recall_search returned.")
          "offset" (nlk:make-json-object
                    "type" "integer" "description" "Character offset to start at (default 0).")
          "max_chars" (nlk:make-json-object
                       "type" "integer"
                       "description" "Characters to return (default 24000, at most 120000)."))
         "required" (vector "path")))
  "The recall_open boundary.")

(defparameter +stats-schema+
  (list :description
        "Index health for the local recall index: file and chunk counts, the last run, the roots and what was skipped."
        :parameters (nlk:make-json-object "type" "object" "properties" (nlk:make-json-object)))
  "The recall_stats boundary.")

(defparameter +forget-schema+
  (list :description
        (concatenate 'string
                     "Remove a file or folder from the local search index, or clear the whole "
                     "index. Use it when the operator says something should not be searchable, or "
                     "to reset after changing the indexed roots. Index rows only: the files on "
                     "disk are never touched.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties"
         (nlk:make-json-object
          "path" (nlk:make-json-object
                  "type" "string"
                  "description" "A file path, or a folder prefix to drop from the index.")
          "all" (nlk:make-json-object
                 "type" "boolean" "description" "Clear every indexed file."))))
  "The recall_forget boundary.")

;;; --- the verbs -----------------------------------------------------------

(defun string-list (value)
  "VALUE -- a JSON array, a list or one string -- as a list of strings."
  (cond ((null value) '())
        ((stringp value) (let ((value (nlk:trimmed value)))
                            (if (plusp (length value)) (list value) '())))
        ((listp value) (remove nil (mapcar (lambda (item)
                                             (and (stringp item) (nlk:trimmed item)))
                                           value)))
        ((vectorp value) (loop for item across value
                               when (and (stringp item) (plusp (length (nlk:trimmed item))))
                                 collect (nlk:trimmed item)))
        (t '())))

(defun resolve-roots (configured)
  "CONFIGURED as folders this cell may index: existing, refused-free and not
repeated inside an earlier one."
  (let ((roots '()))
    (dolist (item (string-list configured))
      (let ((path (expand-path item)))
        (when (ignore-errors (uiop:directory-exists-p path))
          (multiple-value-bind (real reason) (guard-path path :directory t)
            (unless (plusp (length reason))
              (unless (some (lambda (root) (within-p real root)) roots)
                (push real roots)))))))
    (nreverse roots)))

(defun refused-roots (configured)
  "CONFIGURED roots that exist but may not be indexed, as a JSON object."
  (let ((pairs '()))
    (dolist (item (string-list configured))
      (let ((path (expand-path item)))
        (when (ignore-errors (uiop:directory-exists-p path))
          (multiple-value-bind (real reason) (guard-path path :directory t)
            (when (plusp (length reason))
              (push (cons (uiop:native-namestring real) reason) pairs))))))
    (apply #'nlk:make-json-object (loop for (key . value) in pairs append (list key value)))))

(defun index-files (&key paths rebuild (budget nil) (max-files nil))
  "Index the operator's folders => a summary object."
  (running-settings)
  (let* ((configured (or (string-list paths) (string-list (setting :roots))))
         (roots (resolve-roots configured)))
    (when (null roots)
      (fail "no indexable folders found: ~{~a~^, ~} is not a folder this cell may index"
            (string-list configured)))
    (index-roots roots
                  :excludes (string-list (setting :excludes))
                  :budget (or budget (setting :budget-s))
                  :max-files (or max-files (setting :max-files))
                  :max-file-mb (setting :max-file-mb)
                  :chunk-chars (setting :chunk-chars)
                  :force (and rebuild t))))

(defun stats-object ()
  "What the index holds, as a JSON object."
  (running-settings)
  (let* ((index (the-index))
         (files (hash-table-count (recall-index-files index)))
         (skipped (loop for file being the hash-values of (recall-index-files index)
                        count (plusp (length (or (remembered-skipped file) ""))))))
    (nlk:make-json-object
     "files" files
     "chunks" (recall-index-chunks index)
     "skipped_files" skipped
     "last_index" (or (gethash "last_index" (recall-index-meta index)) "")
     "roots" (or (gethash "roots" (recall-index-meta index)) "")
     "roots_configured" (coerce (string-list (setting :roots)) 'vector)
     "roots_refused" (refused-roots (setting :roots))
     "ambient" (and (setting :ambient) t)
     "refresh_on_start" (and (setting :refresh-on-start) t)
     "index_path" (uiop:native-namestring (index-path))
     "hint" (if (zerop files)
                 "nothing is indexed yet -- call recall_index on the folders that hold the operator's documents"
                 ""))))

(defun result-object (result)
  "One search RESULT as a JSON object."
  (nlk:make-json-object
   "path" (getf result :path)
   "name" (getf result :name)
   "score" (coerce (getf result :score) 'double-float)
   "snippet" (getf result :snippet)
   "chunk" (getf result :chunk)
   "modified" (getf result :modified)))

(defun open-document (path &key (offset 0) (max-chars *open-char-cap*))
  "One file's text, windowed => a JSON object."
  (running-settings)
  (let ((raw (nlk:trimmed (cond ((stringp path) path)
                                ((pathnamep path) (uiop:native-namestring path))
                                (t "")))))
    (when (zerop (length raw)) (fail "path is required"))
    (let* ((given (expand-path raw))
           (full (if (uiop:absolute-pathname-p given)
                     given
                     (merge-pathnames given (user-homedir-pathname)))))
      (multiple-value-bind (real reason) (guard-path full)
        (when (plusp (length reason)) (fail "~a" reason))
        (when (ignore-errors (uiop:directory-exists-p real))
          (fail "not a file: ~a" (uiop:native-namestring real)))
        (unless (ignore-errors (probe-file real))
          (fail "not found: ~a" (uiop:native-namestring real)))
        (multiple-value-bind (text note) (extract-text real (* 64 1024 1024))
          (when (zerop (length text))
            (fail "no readable text (~a): ~a" (if (plusp (length note)) note "unknown")
                  (uiop:native-namestring real)))
          (let* ((offset (max 0 (or offset 0)))
                 (cap (max 500 (min (or max-chars *open-char-cap*) 120000)))
                 (start (min offset (length text)))
                 (end (min (length text) (+ start cap))))
            (nlk:make-json-object
             "path" (uiop:native-namestring real)
             "chars_total" (length text)
             "offset" start
             "returned" (- end start)
             "truncated" (< end (length text))
             "note" note
             "text" (subseq text start end))))))))

(defun forget-files (&key path all)
  "Drop PATH, everything under it, or the whole index => a JSON object."
  (running-settings)
  (let ((index (the-index)))
    (if all
        (let ((before (hash-table-count (recall-index-files index))))
          (setf (recall-index-files index) (make-hash-table :test #'equal)
                (recall-index-meta index) (make-hash-table :test #'equal))
          (rebuild-tables index)
          (save-index index)
          (nlk:make-json-object "cleared" t "files_removed" before
                                 "files_remaining" 0))
        (let ((target (string-right-trim "/" (or path ""))))
          (when (zerop (length target)) (fail "pass path=<file or folder> or all=true"))
          (let ((removed (remove-matching index target)))
            (rebuild-tables index)
            (save-index index)
            (nlk:make-json-object "path" path "files_removed" removed
                                   "files_remaining" (hash-table-count
                                                       (recall-index-files index))))))))

(defun remove-matching (index target)
  "INDEX's files at TARGET or under it, removed => how many went."
  (let ((prefix (concatenate 'string target "/"))
        (gone '()))
    (maphash (lambda (key file)
               (declare (ignore file))
               (when (or (equal key target) (uiop:string-prefix-p prefix key))
                 (push key gone)))
             (recall-index-files index))
    (dolist (key gone) (remhash key (recall-index-files index)))
    (length gone)))

;;; --- ambient hint ---------------------------------------------------------

(defparameter *doc-cues*
  '("file" "files" "document" "documents" "doc" "docs" "pdf" "contract" "invoice"
    "invoices" "receipt" "receipts" "spreadsheet" "csv" "xlsx" "deck" "slides"
    "note" "notes" "letter" "resume" "cv" "report" "policy" "statement"
    "proposal" "quote" "spec" "lease" "agreement" "transcript" "archive")
  "A message that names a document at all.")

(defparameter *lookup-cues*
  '("where is" "where's" "find my" "find the" "which file" "pull up" "open the"
    "look at the" "search my" "that file" "the file" "send me the" "what did the"
    "what does the" "in my" "from my" "on my machine" "in my folder" "in my documents")
  "A message that asks for one.")

(defparameter *stopwords*
  '("the" "and" "for" "with" "that" "this" "from" "have" "has" "you" "your" "can"
    "could" "would" "should" "please" "about" "what" "when" "where" "which" "there"
    "their" "them" "then" "than" "into" "just" "like" "make" "made" "need" "want"
    "know" "tell" "give" "take" "get" "got" "let" "lets" "how" "why" "who" "all"
    "any" "some" "not" "but" "was" "were" "are" "been" "being" "did" "does"
    "doing" "done" "will" "now" "new" "old" "one" "two" "use" "using" "used" "it"
    "its" "as" "at" "by" "in" "on" "of" "or" "to" "is" "be" "if" "so" "we"
    "me" "my" "i" "a" "an" "do" "up" "out" "off" "run" "put" "see" "say" "says"
    "said")
  "Words that name no document.")

(defparameter *min-hint-score* 2.0
  "Below this a hit is a coincidence, not the file being discussed.")

(defparameter *hint-char-budget* 900
  "What one hint may add to a prompt: the hook runs every turn.")

(defun content-tokens (text)
  "TEXT's words a document search can use: no stopwords, no one- and
two-letter crumbs, in order, de-duplicated."
  (let ((seen '()))
    (dolist (token (tokenize text))
      (unless (or (< (length token) 3) (member token *stopwords* :test #'equal)
                  (member token seen :test #'equal))
        (push token seen)))
    (nreverse seen)))

(defun quoted-phrases (text)
  "The phrases TEXT puts in quotes, longest first."
  (let ((phrases '()))
    (loop for i from 0 below (length text)
          for character = (char text i)
          when (member character '(#\" #\' #\`) :test #'char=)
            do (let ((end (position character text :start (1+ i))))
                 (when (and end (>= (- end i) 4) (<= (- end i) 80))
                   (push (subseq text (1+ i) end) phrases))))
    phrases))

(defun hint-paths-shown (session-id paths)
  "The paths in PATHS this session has not been shown yet, remembering them."
  (when (> (hash-table-count *searched*) 400) (clrhash *searched*))
  (let* ((key (or session-id "-"))
         (seen (or (gethash key *searched*) (setf (gethash key *searched*)
                                                   (make-hash-table :test #'equal))))
         (fresh '()))
    (dolist (path paths)
      (unless (gethash path seen) (push path fresh))
      (setf (gethash path seen) t))
    (nreverse fresh)))

(defun ambient-block (text &key (session-id ""))
  "The candidate-file block for TEXT, or the empty string: only when the
message reads like it is about a document, only a few paths, and every line
bounded."
  (running-settings)
  (let ((text (or text "")))
    (when (or (< (length (nlk:trimmed text)) 12) (uiop:string-prefix-p "/" (nlk:trimmed text)))
      (return-from ambient-block ""))
    (let ((low (string-downcase text)))
      (when (some (lambda (marker) (cl:search marker low :test #'char=))
                  '("[important:" "[internal notification" "[out-of-band user message"
                    "[background process" "[recall]"))
        (return-from ambient-block ""))
      (let ((quoted (quoted-phrases text))
            (words (uiop:split-string low :separator " ")))
        (when (and (not (some (lambda (cue) (member cue words :test #'equal)) *doc-cues*))
                   (not (some (lambda (cue) (cl:search cue low :test #'char=)) *lookup-cues*))
                   (null quoted))
          (return-from ambient-block ""))
        (let ((tokens (content-tokens (if (and quoted (> (length (first quoted)) 4))
                                          (first quoted)
                                          text))))
          (when (< (length tokens) 2) (return-from ambient-block ""))
          (let* ((hits (search (format nil "~{~a~^ ~}" (subseq tokens 0 (min 8 (length tokens))))
                                :limit 6))
                 (good (remove-if (lambda (hit) (< (getf hit :score) *min-hint-score*)) hits)))
            (when (null good) (return-from ambient-block ""))
            (let* ((max-hints (max 1 (setting :max-hints)))
                   (fresh (hint-paths-shown session-id
                                            (mapcar (lambda (hit) (getf hit :path)) good)))
                   (picks (loop for hit in good
                                when (member (getf hit :path) fresh :test #'equal)
                                  collect hit into chosen
                                when (>= (length chosen) max-hints)
                                  return chosen
                                finally (return chosen))))
              (when (null picks) (return-from ambient-block ""))
              (let ((head "[recall] Local files in the operator's index that may be the document being discussed:")
                    (tail (concatenate 'string
                                       "Read one with recall_open before answering, and cite the path. "
                                       "If none of these is the right document, say so and ask -- do not invent a file."))
                    (budget (- *hint-char-budget* 300))
                    (lines '()))
                (dolist (hit picks)
                  (let ((line (format nil "- ~a (modified ~a) -- \"~a\""
                                       (getf hit :path)
                                       (if (plusp (length (getf hit :modified))) (getf hit :modified) "unknown")
                                       (subseq (getf hit :snippet) 0 (min 180 (length (getf hit :snippet)))))))
                    (when (>= (length line) budget) (return))
                    (push line lines)
                    (decf budget (1+ (length line)))))
                (when (null lines) (return-from ambient-block ""))
                (format nil "~a~%~{~a~%~}~a" head (nreverse lines) tail)))))))))

(defun message-text (content)
  "One message's CONTENT as text: a string, or the text parts of a vector."
  (cond ((stringp content) content)
        ((vectorp content)
         (format nil "~{~a~^ ~}"
                 (loop for part across content
                       when (and (hash-table-p part) (stringp (gethash "text" part)))
                         collect (gethash "text" part))))
        ((hash-table-p content) (or (gethash "text" content) ""))
        (t "")))

(defun last-user-text (session-id)
  "The operator's newest message in SESSION-ID, or NIL."
  (when (and session-id (nlk:store-open-p))
    (let ((turns (ignore-errors
                  (nle::history-turns-for-session (nle:intern-session session-id)))))
      (dolist (turn (reverse turns))
        (dolist (message (reverse (nle::history-turn-messages turn)))
          (when (equal "user" (gethash "role" message))
            (return-from last-user-text (message-text (gethash "content" message)))))))))

(defun harness-sections (next session-id)
  "The standing sections, with the ambient hint as one of our own while it
has something to say."
  (let* ((sections (funcall next session-id))
         (rest (remove "recall" sections :key #'car :test #'equal))
         (block (and *recall*
                     (ignore-errors
                      (and (setting :ambient)
                           (ambient-block (or (last-user-text session-id) "")
                                          :session-id (or session-id "")))))))
    (if (and block (plusp (length block)))
        (acons "recall" block rest)
        rest)))

;;; --- the background refresh -----------------------------------------------

(defun refresh-step ()
  "One bounded pass over what the last explicit run indexed: a file saved
yesterday is findable today without the operator asking."
  (handler-case
      (let* ((index (the-index))
             (indexed (string-list (gethash "roots_list" (recall-index-meta index)))))
        (when (plusp (hash-table-count (recall-index-files index)))
          (let ((roots (resolve-roots indexed)))
            (when roots
              (bt2:with-recursive-lock-held (*index-lock*)
                (index-roots roots
                              :excludes (string-list (setting :excludes))
                              :budget (setting :refresh-budget-s)
                              :max-files (setting :refresh-max-files)
                              :max-file-mb (setting :max-file-mb)
                              :chunk-chars (setting :chunk-chars))))))) 
    (error () nil))
  nil)

;;; --- the tools -----------------------------------------------------------

(defun tool-index (args)
  (nlk:encode-json-object
   (index-files :paths (nlk:json-value args :array "path")
                 :rebuild (nlk:json-value args :boolean "rebuild")
                 :budget (nlk:json-value args :integer "budget_s")
                 :max-files (nlk:json-value args :integer "max_files"))))

(defun tool-search (args)
  (let ((query (or (nlk:json-value args :string "query") "")))
    (when (zerop (length (nlk:trimmed query))) (fail "query is required"))
    (let* ((results (search query
                             :limit (or (nlk:json-value args :integer "limit") 8)
                             :path (or (nlk:json-value args :string "path") "")
                             :ext (or (nlk:json-value args :string "ext") "")))
           (total (hash-table-count (recall-index-files (the-index)))))
      (nlk:encode-json-object
       (nlk:make-json-object
        "query" query
        "count" (length results)
        "indexed_files" total
        "results" (coerce (mapcar #'result-object results) 'vector)
        "hint" (if (zerop total)
                    "the index is empty -- call recall_index once (it is incremental after that)"
                    ""))))))

(defun tool-open (args)
  (nlk:encode-json-object
   (open-document (or (nlk:json-value args :string "path") "")
                  :offset (or (nlk:json-value args :integer "offset") 0)
                  :max-chars (or (nlk:json-value args :integer "max_chars") *open-char-cap*))))

(defun tool-stats (args)
  (declare (ignore args))
  (nlk:encode-json-object (stats-object)))

(defun tool-forget (args)
  (nlk:encode-json-object
   (forget-files :path (nlk:json-value args :string "path")
                  :all (nlk:json-value args :boolean "all"))))

;;; --- the command ---------------------------------------------------------

(defun stats-line ()
  (let ((stats (stats-object)))
    (format nil "recall -- ~a files, ~a chunks~%last index: ~a -- roots: ~a~%ambient hints: ~a"
            (gethash "files" stats)
            (gethash "chunks" stats)
            (let ((stamp (gethash "last_index" stats)))
              (if (plusp (length stamp)) stamp "never"))
            (let ((roots (gethash "roots" stats)))
              (if (plusp (length roots)) roots
                  (format nil "~{~a~^, ~}" (string-list (setting :roots)))))
            (if (gethash "ambient" stats) "on" "off"))))

(defun command-search (query)
  (let ((results (search query :limit 6)))
    (if (null results)
        (format nil "no match for \"~a\"~@[ -- ~a~]"
                query
                (when (zerop (hash-table-count (recall-index-files (the-index))))
                  "the index is empty; /recall index builds it"))
        (with-output-to-string (out)
          (format out "recall -- ~d match(es) for \"~a\"~%" (length results) query)
          (dolist (result results)
            (format out "- ~a -- ~a -- score ~a~%  ...~a~%"
                    (getf result :path)
                    (let ((modified (getf result :modified)))
                      (if (plusp (length modified)) modified "?"))
                    (getf result :score)
                    (subseq (getf result :snippet) 0 (min 160 (length (getf result :snippet))))))
          (format out "read one with recall_open, or the model's recall tools")))))

(defun run-command (raw session-id)
  "One /recall invocation: a query, index [path], stats, or forget <path>|all."
  (declare (ignore session-id))
  (handler-case
      (let ((args (nlk:trimmed (or raw ""))))
        (cond
          ((or (zerop (length args)) (member args '("stats" "status") :test #'equal))
           (stats-line))
          ((uiop:string-prefix-p "index" args)
           (let* ((rest (nlk:trimmed (subseq args (length "index"))))
                  (rebuild (cl:search "--rebuild" rest :test #'char=))
                  (target (and (plusp (length rest))
                               (not (uiop:string-prefix-p "--" rest))
                               rest))
                  (summary (index-files :paths (and target (list target))
                                        :rebuild (and rebuild t))))
             (format nil "indexed ~a new/changed, ~a unchanged, ~a skipped, ~a removed in ~,1fs -- ~a files total"
                     (gethash "indexed" summary) (gethash "unchanged" summary)
                     (gethash "skipped" summary) (gethash "removed" summary)
                     (gethash "seconds" summary) (gethash "files" summary))))
          ((uiop:string-prefix-p "forget" args)
           (let ((target (nlk:trimmed (subseq args (length "forget")))))
             (cond ((member target '("all" "*") :test #'equal)
                    (let ((summary (forget-files :all t)))
                      (format nil "index cleared -- ~a files removed"
                              (gethash "files_removed" summary))))
                   ((zerop (length target))
                    "usage: /recall forget <path>   or   /recall forget all")
                   (t (let ((summary (forget-files :path target)))
                        (format nil "removed ~a file(s) from the index; ~a remain"
                                (gethash "files_removed" summary)
                                (gethash "files_remaining" summary))))))) 
          (t (command-search args))))
    (recall-error (condition) (format nil "recall: ~a" condition))
    (error (condition) (format nil "recall: ~a: ~a" (type-of condition) condition))))

;;; --- the cell ------------------------------------------------------------

(nle:define-cell recall
  (:section ("recall")
    (:guide "roots decides what is searched: ~/Documents alone unless you name others (~/Downloads and ~/Desktop are opt-in); then say \"index my documents\" or run /recall index; ambient puts the likely file in front of the model when a message reads like it is about a document")
    ("roots" :list
     :doc "folders to index; each is refused if it is inside Nodecode's own home or a credential store")
    ("excludes" :list
     :doc "extra glob patterns to skip, on top of the built-in secret, agent-state and build-directory denylist")
    ("ambient" :boolean :default t
     :doc "let the standing prompt carry candidate files when a message looks like it is about a document")
    ("max_hints" :integer :default 3 :min 1
     :doc "how many candidate files one hint may name")
    ("max_file_mb" :integer :default 8 :min 1
     :doc "skip files larger than this many megabytes")
    ("chunk_chars" :integer :default 1200 :min 200
     :doc "characters per indexed chunk")
    ("max_files" :integer :default 20000 :min 1
     :doc "hard cap on files touched by one index run")
    ("budget_s" :integer :default 90 :min 5
     :doc "wall-clock budget for one index run before it answers partial progress")
    ("refresh_on_start" :boolean :default t
     :doc "once an index exists, refresh it in the background when the cell starts")
    ("refresh_budget_s" :integer :default 25 :min 5
     :doc "wall-clock budget for one background refresh")
    ("refresh_max_files" :integer :default 5000 :min 1
     :doc "how many files a background refresh inspects"))
  (:tool "recall_index" +index-schema+ 'tool-index)
  (:tool "recall_search" +search-schema+ 'tool-search)
  (:tool "recall_open" +open-schema+ 'tool-open)
  (:tool "recall_stats" +stats-schema+ 'tool-stats)
  (:tool "recall_forget" +forget-schema+ 'tool-forget)
  (:help :recall "recall: search, read and cite the operator's own local files" +manual+)
  (:hook 'nle::read-harness-sections #'harness-sections)
  (:command "recall" 'run-command
            :description "Search or index your local files"
            :argument-hint "<query | index [path] | stats | forget all>")
  (:start (lambda ()
            (forget-index)
            (when (setting :refresh-on-start)
              (let ((worker (nlk:worker-start "recall-refresh" #'refresh-step
                                              :wake 3600 :prime t)))
                (nle:on-stop (lambda () (nlk:worker-stop worker))))))))
