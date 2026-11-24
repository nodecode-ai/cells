;;;; index.lisp --- the corpus, its postings, the walk that fills it and the
;;;; query that reads it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One JSON file under the organism's home holds every indexed file and its
;;;; chunks; the term postings and the chunk lengths are derived from those
;;;; chunks in memory, so a search is a hash lookup and never a scan of the
;;;; disk. Ranking is BM25 over the chunks, with the deterministic boosts the
;;;; original had: a path that carries a query word, a file name that does,
;;;; a phrase in order, and a gentle favour for a fresh file.

(in-package #:nodecode-recall)

;;; --- tokens ---------------------------------------------------------------

(defun token-char-p (character)
  "Whether CHARACTER is inside a word an index keeps: a letter, a digit or an
underscore."
  (or (alphanumericp character) (char= character #\_)))

(defun tokenize (text)
  "TEXT's words, downcased, in order: a run of letters, digits and
underscores, at least two long."
  (let ((tokens '())
        (start nil))
    (flet ((flush (end)
             (when start
               (let ((token (string-downcase (subseq text start end))))
                 (when (>= (length token) 2) (push token tokens)))
               (setf start nil))))
      (loop for i from 0 below (length text)
            for character = (char text i)
            do (if (token-char-p character)
                   (unless start (setf start i))
                   (flush i)))
      (flush (length text)))
    (nreverse tokens)))

(defun token-counts (text)
  "TEXT's words with how often each occurs, as a table."
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (token (tokenize text)) (incf (gethash token counts 0)))
    counts))

(defun query-tokens (query &key (limit 12))
  "QUERY's words, de-duplicated, at most LIMIT of them."
  (let ((seen '()))
    (dolist (token (tokenize (or query "")))
      (unless (member token seen :test #'equal) (push token seen))
      (when (>= (length seen) limit) (return)))
    (nreverse seen)))

;;; --- derived tables -------------------------------------------------------

(defun rebuild-tables (index)
  "INDEX's postings and chunk lengths, from the files it holds."
  (setf (recall-index-postings index) (make-hash-table :test #'equal)
        (recall-index-lengths index) (make-hash-table :test #'equal)
        (recall-index-chunks index) 0
        (recall-index-tokens index) 0)
  (maphash (lambda (path file)
             (declare (ignore path))
             (let* ((chunks (remembered-chunks file))
                    (lengths (make-array (length chunks) :initial-element 0)))
               (loop for chunk across chunks
                     for i from 0
                     do (let ((total 0))
                          (maphash (lambda (token count)
                                     (incf total count)
                                     (let ((by-path (or (gethash token (recall-index-postings index))
                                                        (setf (gethash token (recall-index-postings index))
                                                              (make-hash-table :test #'equal)))))
                                       (push (cons i count) (gethash (remembered-path file) by-path))))
                                   (token-counts chunk))
                          (setf (aref lengths i) total)
                          (incf (recall-index-chunks index))
                          (incf (recall-index-tokens index) total)))
               (setf (gethash (remembered-path file) (recall-index-lengths index)) lengths)))
           (recall-index-files index))
  index)

;;; --- the file on disk -----------------------------------------------------

(defun index-object (index)
  "INDEX as the JSON object it is written as."
  (let ((meta (nlk:make-json-object)))
    (maphash (lambda (key value) (setf (gethash key meta) value)) (recall-index-meta index))
    (nlk:make-json-object
     "version" 1
     "meta" meta
     "files" (coerce (loop for file being the hash-values of (recall-index-files index)
                            collect (nlk:make-json-object
                                     "path" (remembered-path file)
                                     "mtime" (remembered-mtime file)
                                     "size" (remembered-size file)
                                     "ext" (remembered-ext file)
                                     "skipped" (or (remembered-skipped file) "")
                                     "chunks" (coerce (remembered-chunks file) 'vector)))
                    'vector))))

(defun save-index (index)
  "INDEX written over the file it was read from, in one atomic replace."
  (let* ((target (index-path))
         (temporary (merge-pathnames (format nil "~a.tmp"
                                             (or (pathname-name target) "recall-index"))
                                     (uiop:pathname-directory-pathname target))))
    (ensure-directories-exist target)
    (nlk:with-output-file (out temporary)
      (write-string (nlk:encode-json-object (index-object index)) out))
    (nlk:replace-file temporary target)
    target))

(defun load-index ()
  "The index this home keeps, or an empty one when there is none yet."
  (let ((index (%make-recall-index)))
    (let ((text (ignore-errors (nlk:read-text (index-path) (* 512 1024 1024)))))
      (when text
        (let ((object (ignore-errors (nlk:decode-json text))))
          (when (hash-table-p object)
            (let ((meta (gethash "meta" object))
                  (files (gethash "files" object)))
              (when (hash-table-p meta)
                (maphash (lambda (key value) (setf (gethash key (recall-index-meta index)) value)) meta))
              (when (vectorp files)
                (loop for file across files
                      when (hash-table-p file)
                        do (let ((path (gethash "path" file)))
                             (when (and (stringp path) (plusp (length path)))
                               (setf (gethash path (recall-index-files index))
                                     (make-remembered
                                      path
                                      (gethash "mtime" file)
                                      (gethash "size" file)
                                      (or (gethash "ext" file) "")
                                      (or (gethash "skipped" file) "")
                                      (coerce (or (gethash "chunks" file) #()) 'vector))))))))))))
    (rebuild-tables index)))

(defun the-index ()
  "The loaded index, read once per process."
  (running-settings)
  (bt2:with-recursive-lock-held (*index-lock*)
    (or *index* (setf *index* (load-index)))))

(defun forget-index ()
  "Throw the loaded index away, so the next read is from disk."
  (bt2:with-recursive-lock-held (*index-lock*)
    (setf *index* nil)))

;;; --- the walk ------------------------------------------------------------

(defun file-attributes (path)
  "PATH's write date and size => (values MTIME SIZE)."
  (values (ignore-errors (file-write-date path))
          (ignore-errors (with-open-file (in path :element-type '(unsigned-byte 8))
                           (file-length in)))))

(defun name-chunk (path)
  "The words in a file's name, always indexed: a scanned PDF stays findable
by what it is called even when its text cannot be read."
  (format nil "file ~a ~a ~a"
          (or (pathname-name path) "")
          (or (car (last (pathname-directory path))) "")
          (file-extension path)))

(defun walk-files (root visit &key (depth 0))
  "Call VISIT on every readable file under ROOT, judged on the resolved path,
never descending into a credential store or a build directory."
  (when (> depth 24) (return-from walk-files nil))
  (dolist (child (ignore-errors (uiop:subdirectories (uiop:ensure-directory-pathname root))))
    (let ((name (car (last (pathname-directory child)))))
      (unless (and (stringp name) (skip-dir-p name))
        (let ((real (resolve-path child)))
          (multiple-value-bind (judged reason) (guard-path child :directory t)
            (declare (ignore judged))
            (when (and (zerop (length reason)) (within-p real root))
              (walk-files child visit :depth (1+ depth))))))))
  (dolist (file (ignore-errors (uiop:directory-files (uiop:ensure-directory-pathname root))))
    (funcall visit file)))

;;; --- one indexing run -----------------------------------------------------

(defun index-roots (roots &key excludes (budget 90) (max-files 20000) (max-file-mb 8)
                                  (chunk-chars 1200) force)
  "Index ROOTS into the loaded index => a summary object."
  (running-settings)
  (let* ((index (the-index))
         (started (get-internal-real-time))
         (deadline (+ started (* (max 5 budget) internal-time-units-per-second)))
         (max-bytes (max 1 (* 1024 1024 max-file-mb)))
         (touched 0) (unchanged 0) (skipped 0) (errors 0) (removed 0)
         (seen (make-hash-table :test #'equal))
         (reasons (make-hash-table :test #'equal)))
    (labels ((deadline-p () (> (get-internal-real-time) deadline))
             (note-reason (reason)
               (let ((key (car (uiop:split-string reason :separator ":"))))
                 (incf (gethash key reasons 0))))
             (visit (file)
               (when (or (deadline-p) (>= (+ touched unchanged skipped) max-files))
                 (throw 'bounded nil))
               (when (user-excluded-p file (or excludes '()))
                 (return-from visit nil))
               (multiple-value-bind (real reason) (guard-path file)
                 (when (plusp (length reason)) (return-from visit nil))
                 (let ((key (ignore-errors (uiop:native-namestring file))))
                   (when (or (null key) (zerop (length key))) (return-from visit nil))
                   (setf (gethash key seen) t)
                   (multiple-value-bind (mtime size) (file-attributes real)
                     (declare (ignore size))
                     (let ((existing (gethash key (recall-index-files index))))
                       (when (and existing (not force) (equal mtime (remembered-mtime existing)))
                         (incf unchanged)
                         (return-from visit nil))
                       (handler-case
                           (multiple-value-bind (text note) (extract-text real max-bytes)
                             (let* ((chunks (if (zerop (length text))
                                                (vector (name-chunk real))
                                                (coerce (cons (name-chunk real)
                                                              (chunk-text text chunk-chars
                                                                          *max-chunks-per-file*))
                                                        'vector)))
                                    (mtime (or mtime 0)))
                               (setf (gethash key (recall-index-files index))
                                     (make-remembered key mtime 0 (file-extension real) note chunks))
                               (if (zerop (length text))
                                   (progn (incf skipped) (note-reason note))
                                   (incf touched))))
                         (error () (incf errors) (incf (gethash "write" reasons 0))))))))))
      (catch 'bounded
        (dolist (root roots)
          (walk-files root #'visit)))
      ;; A file that vanished from a scanned root leaves the index with it.
      (let ((gone '()))
        (maphash (lambda (key file)
                   (declare (ignore file))
                   (unless (or (gethash key seen)
                               (some (lambda (root) (within-p key root)) roots)
                               (ignore-errors (probe-file key)))
                     (push key gone)))
                 (recall-index-files index))
        (dolist (key gone) (remhash key (recall-index-files index)) (incf removed)))
      (let ((elapsed (/ (- (get-internal-real-time) started)
                        (float internal-time-units-per-second))))
        (setf (gethash "last_index" (recall-index-meta index))
              (format-timestamp (get-universal-time))
              (gethash "roots" (recall-index-meta index))
              (format nil "~{~a~^, ~}" (mapcar (lambda (root) (uiop:native-namestring root)) roots))
              (gethash "index_seconds" (recall-index-meta index))
              (format nil "~,1f" elapsed))
        (rebuild-tables index)
        (save-index index)
        (nlk:make-json-object
         "indexed" touched
         "unchanged" unchanged
         "skipped" skipped
         "removed" removed
         "errors" errors
         "files" (hash-table-count (recall-index-files index))
         "chunks" (recall-index-chunks index)
         "seconds" (coerce elapsed 'double-float)
         "skip_reasons" (reasons-object reasons)
         "roots" (coerce (mapcar (lambda (root) (uiop:native-namestring root)) roots) 'vector))))))

(defun format-timestamp (universal)
  "UNIVERSAL as the one date spelling the model and the operator both read."
  (multiple-value-bind (second minute hour day month year) (decode-universal-time universal)
    (declare (ignore second))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" year month day hour minute)))

(defun reasons-object (table)
  "TABLE's first six entries, most frequent first, as a JSON object."
  (let ((pairs (loop for key being the hash-keys of table using (hash-value count)
                     collect (cons key count))))
    (setf pairs (subseq (sort pairs #'> :key #'cdr) 0 (min 6 (length pairs))))
    (apply #'nlk:make-json-object
           (loop for (key . count) in pairs append (list key count)))))

;;; --- search ---------------------------------------------------------------

(defun posting-tokens (index token)
  "The index terms TOKEN reads as: itself, and the terms it is a prefix of."
  (let ((keys (list (cons token 1.0))))
    (unless (gethash token (recall-index-postings index))
      (let ((extra '()))
        (maphash (lambda (key value)
                   (declare (ignore value))
                   (when (and (< (length extra) 40)
                              (> (length key) (length token))
                              (uiop:string-prefix-p token key))
                     (push (cons key 0.6) extra)))
                 (recall-index-postings index))
        (setf keys (append keys extra))))
    keys))

(defun snippet-of (chunk tokens &key (width 260))
  "The window of CHUNK around its first query word, every query word in it
wrapped in brackets."
  (let* ((lower (string-downcase chunk))
         (at (loop for token in tokens
                   for position = (cl:search token lower :test #'char=)
                   when position minimize position))
         (start (if at (max 0 (- at 80)) 0))
         (end (min (length chunk) (+ start width)))
         (piece (subseq chunk start end)))
    (with-output-to-string (out)
      (when (plusp start) (write-string "... " out))
      (loop with i = 0
            while (< i (length piece))
            do (let ((hit (loop for token in tokens
                               when (and (<= (+ i (length token)) (length piece))
                                         (string-equal token piece :start2 i
                                                             :end2 (+ i (length token))))
                                 return token)))
                 (if hit
                     (progn (write-char #\[ out)
                            (write-string (subseq piece i (+ i (length hit))) out)
                            (write-char #\] out)
                            (incf i (length hit)))
                     (progn (write-char (char piece i) out) (incf i)))))
      (when (< end (length chunk)) (write-string " ..." out)))))

(defun search (query &key (limit 8) (path "") (ext ""))
  "The files under QUERY, best first => a list of plists. A query word
matches a term and the terms it begins."
  (running-settings)
  (let* ((index (the-index))
         (tokens (query-tokens query))
         (filter (string-downcase (nlk:trimmed (or path ""))))
         (wanted (string-downcase (string-left-trim "." (or ext ""))))
         (limit (max 1 (min (or limit 8) 25))))
    (when (or (null tokens) (zerop (recall-index-chunks index))) (return-from search '()))
    (let ((scores (make-hash-table :test #'equal))
          (total (recall-index-chunks index))
          (average (max 1.0 (/ (recall-index-tokens index) (float (max 1 (recall-index-chunks index))))))
          (now (get-universal-time)))
      (dolist (token tokens)
        (dolist (pair (posting-tokens index token))
          (let* ((by-path (gethash (car pair) (recall-index-postings index)))
                 (df (if by-path (hash-table-count by-path) 0)))
            (when (plusp df)
              (let ((idf (* (cdr pair)
                            (log (1+ (/ (- total df 0.5) (+ df 0.5)))))))
                (maphash (lambda (path postings)
                           (let ((lengths (gethash path (recall-index-lengths index))))
                             (dolist (entry postings)
                               (let* ((which (car entry))
                                      (tf (cdr entry))
                                      (length (if (and lengths (< which (length lengths)))
                                                  (aref lengths which)
                                                  0))
                                      (score (* idf
                                                (/ (* tf 2.2)
                                                   (+ tf (* 1.2 (- 1 0.75 (* 0.75 (/ length average)))))))))
                                 (let ((best (gethash path scores)))
                                   (when (or (null best) (> score (car best)))
                                     (setf (gethash path scores) (cons score which))))))))
                         by-path))))))
      (let ((results '()))
        (maphash
         (lambda (path best)
           (let* ((file (gethash path (recall-index-files index)))
                  (name (string-downcase (or (pathname-name (pathname path)) "")))
                  (low (string-downcase path))
                  (chunk (and file (< (cdr best) (length (remembered-chunks file)))
                              (aref (remembered-chunks file) (cdr best))))
                  (score (car best)))
             (when (and file chunk
                        (or (zerop (length filter)) (cl:search filter low :test #'char=))
                        (or (zerop (length wanted))
                            (equal wanted (string-downcase (or (pathname-type (pathname path)) "")))))
               (dolist (token tokens)
                 (when (cl:search token low :test #'char=) (incf score 2.5))
                 (when (cl:search token name :test #'char=) (incf score 3.0)))
               (when (and (> (length tokens) 1)
                          (phrase-p (string-downcase chunk) tokens))
                 (incf score 3.0))
               (let ((age (if (realp (remembered-mtime file))
                              (max 0.0 (/ (- now (remembered-mtime file)) 86400.0))
                              3650.0)))
                 (setf score (* score (+ 1.0 (/ 0.35 (+ 1.0 (/ age 90.0)))))))
               (push (list :path path
                           :name (or (pathname-name (pathname path)) path)
                           :score (rationalize-score score)
                           :snippet (snippet-of chunk tokens)
                           :chunk (cdr best)
                           :modified (if (realp (remembered-mtime file))
                                         (format-timestamp (round (remembered-mtime file)))
                                         ""))
                     results))))
         scores)
        (subseq (sort results #'> :key (lambda (result) (getf result :score)))
                0 (min limit (length results)))))))

(defun phrase-p (text tokens)
  "Whether every one of TOKENS appears in TEXT and in that order."
  (let ((position 0))
    (dolist (token tokens t)
      (let ((at (cl:search token text :start2 position :test #'char=)))
        (unless at (return nil))
        (setf position (+ at (length token)))))))

(defun rationalize-score (score)
  "SCORE as the two decimals a reader is shown."
  (/ (round (* score 100.0)) 100.0))
