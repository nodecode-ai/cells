;;;; index.lisp --- what this machine keeps, as artifacts a question routes to.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One walk of the configured roots, one record per artifact, and one table
;;;; of every artifact's own words. Nothing here is a chunk index: a hit names
;;;; the whole file the agent should open next, which is what the original
;;;; did -- it routed to whole skills, scripts and runbooks, never to passages.
;;;;
;;;; Determinism is the property the search rests on: the walk is sorted, the
;;;; score is an integer, and a tie breaks on the artifact id, so the same
;;;; roots and the same question answer the same list every time.

(in-package #:nodecode-local-knowledge)

;;; --- what counts as an artifact -------------------------------------------

(defparameter *artifact-types* '("skill" "script" "runbook" "doc" "config")
  "The artifact types the scan produces, which is also the filter a search takes.")

(defparameter *text-types* '("md" "markdown" "org" "txt" "rst")
  "File types read as prose, and classified by the directory they sit in.")

(defparameter *script-types* '("sh" "bash" "zsh" "fish" "py" "rb" "pl" "lisp" "el"
                               "js" "mjs" "ts" "ps1" "rs" "go" "lua" "tcl")
  "File types run as a program, whatever directory holds them.")

(defparameter *config-types* '("json" "yaml" "yml" "toml" "ini" "conf" "cfg")
  "File types that describe a machine rather than a procedure.")

(defparameter *excluded-directories* '(".git" ".hg" ".svn" "node_modules" "__pycache__"
                                       ".venv" "venv" ".mypy_cache" ".pytest_cache"
                                       "target" "dist" "build" ".cache" ".tox" "htmlcov")
  "Directory names the walk never enters; the section's exclude_dir_names adds to these.")

(defparameter *read-limit* 65536
  "How much of one file the scan reads: enough for its headings and its words.")

(defparameter *term-limit* 1200
  "How many distinct words one artifact contributes; a long log stops here.")

;;; --- one artifact ---------------------------------------------------------

(defstruct (artifact (:constructor make-artifact))
  "One whole file a question routes to, and enough of its words to find it."
  id type title path relative summary headings terms title-terms path-terms size modified)

(defun file-bytes (path)
  "The size of PATH in bytes, or NIL when it cannot be read."
  (ignore-errors
   (with-open-file (in path :element-type '(unsigned-byte 8))
     (file-length in))))

(defun read-text (path &key (limit *read-limit*))
  "The first LIMIT characters of PATH, or NIL when it cannot be read as text."
  (handler-case
      (with-open-file (in path :element-type 'character :external-format :utf-8
                               :if-does-not-exist nil)
        (when in
          (let* ((size (ignore-errors (file-length in)))
                 (buffer (make-string (max 1 (min limit (or size limit)))))
                 (end (read-sequence buffer in)))
            (subseq buffer 0 end))))
    (error () nil)))

(defun words-of (text)
  "TEXT as lowercase words of two to twenty-four alphanumerics, in order."
  (let ((words '())
        (start nil)
        (length (length text)))
    (flet ((flush (end)
             (when start
               (let ((word (string-downcase (subseq text start end))))
                 (when (<= 2 (length word) 24) (push word words)))
               (setf start nil))))
      (dotimes (index length)
        (let ((character (char text index)))
          (if (or (alphanumericp character) (member character '(#\- #\_)))
              (unless start (setf start index))
              (flush index))))
      (flush length))
    (nreverse words)))

(defun term-table (texts)
  "The distinct words of TEXTS and how often each occurs, capped at *TERM-LIMIT*."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (text texts table)
      (when text
        (dolist (word (words-of (subseq text 0 (min (length text) *read-limit*))))
          (if (gethash word table)
              (incf (gethash word table))
              (when (< (hash-table-count table) *term-limit*)
                (setf (gethash word table) 1))))))))

;;; --- the paths a walk produces --------------------------------------------

(defun directory-name (path)
  "The last directory a path names, downcased, or \"\"."
  (string-downcase (or (pathname-name (uiop:ensure-directory-pathname path)) "")))

(defun file-extension (path)
  (string-downcase (or (pathname-type path) "")))

(defun file-kind (path)
  "The artifact type PATH is, or NIL for a file this index does not keep."
  (cond ((and (string-equal (or (pathname-name path) "") "skill")
              (member (file-extension path) *text-types* :test #'string=))
         "skill")
        ((member (file-extension path) *script-types* :test #'string=) "script")
        ((member (file-extension path) *text-types* :test #'string=) :prose)
        ((member (file-extension path) *config-types* :test #'string=) "config")
        (t nil)))

(defun prose-kind (path)
  "A prose file is a runbook when its name or a directory says so, else a doc."
  (let ((segments (uiop:split-string (namestring path) :separator "(/)")))
    (if (some (lambda (segment)
                (let ((segment (string-downcase segment)))
                  (or (search "runbook" segment)
                      (and (plusp (length segment))
                           (string= segment "runbooks")))))
              segments)
        "runbook"
        "doc")))

(defun relative-path (path root)
  "PATH as it reads under ROOT, or its own name when it is not under it."
  (let* ((path (or (ignore-errors (namestring (truename path))) (namestring path)))
         (root (let ((name (namestring (uiop:ensure-directory-pathname root))))
                 (or (ignore-errors (namestring (truename name))) name))))
    (if (and (<= (length root) (length path)) (string= root path :end2 (length root)))
        (subseq path (length root))
        (file-namestring path))))

(defun strip-extension (relative)
  "RELATIVE without its final extension: ops/backup.md reads ops/backup."
  (let* ((slash (position #\/ relative :from-end t))
         (dot (position #\. relative :from-end t)))
    (if (and dot (> dot (1+ (or slash -1))))
        (subseq relative 0 dot)
        relative)))

(defun make-id (kind relative)
  "The id a hit answers: the type, then the path under its root without extension."
  (let ((name (strip-extension relative)))
    (when (string= kind "skill")
      (let ((slash (position #\/ name :from-end t)))
        (when (and slash (string-equal (subseq name (1+ slash)) "skill"))
          (setf name (subseq name 0 slash)))
        (when (string-equal name "skill")
          (setf name (or (first (last (uiop:split-string relative :separator "(/)"))) name)))))
    (format nil "~a:~a" kind name)))

;;; --- titles, headings and the first paragraph -----------------------------

(defun bracketed (line prefix)
  "What LINE says after PREFIX, unquoted and trimmed; NIL when it says nothing."
  (let ((start (search prefix line :test #'char-equal)))
    (when (and start (zerop start))
      (let ((value (nlk:trimmed (subseq line (length prefix)))))
        (when (plusp (length value))
          (string-trim "\"'" (string-trim '(#\Space #\Tab) value)))))))

(defun frontmatter-name (text)
  "The name or title a leading --- block declares, or NIL."
  (let ((lines (nlk:lines (subseq text 0 (min (length text) 4000)))))
    (when (and lines (string= (nlk:trimmed (first lines)) "---"))
      (loop for line in (rest lines)
            until (string= (nlk:trimmed line) "---")
            thereis (or (bracketed (nlk:trimmed line) "name:")
                        (bracketed (nlk:trimmed line) "title:"))))))

(defun heading-lines (text)
  "Every #-heading of TEXT, without the marks, at most twelve."
  (let ((headings '()))
    (dolist (line (nlk:lines (subseq text 0 (min (length text) *read-limit*))) (nreverse headings))
      (let ((line (string-left-trim '(#\Space #\Tab) line)))
        (when (and (plusp (length line)) (char= (char line 0) #\#))
          (let ((text (nlk:trimmed (string-left-trim "#" line))))
            (when (and (plusp (length text)) (< (length headings) 12))
              (push text headings))))))))

(defun prose-text (text)
  "TEXT without its frontmatter, heading marks and fenced code, one line."
  (let ((kept '())
        (fence nil))
    (dolist (line (nlk:lines (subseq text 0 (min (length text) *read-limit*))))
      (let ((trimmed (nlk:trimmed line)))
        (cond ((and (>= (length trimmed) 3) (string= (subseq trimmed 0 3) "```")) (setf fence (not fence)))
              (fence)
              ((string= trimmed "---"))
              ((and (plusp (length trimmed)) (char= (char trimmed 0) #\#)))
              ((plusp (length trimmed)) (push trimmed kept)))))
    (nlk:trimmed (format nil "~{~a ~}" (nreverse kept)))))

(defun human-name (path)
  "A path's own name as a title: backup-restore reads backup restore."
  (let ((name (string-downcase (or (pathname-name path) ""))))
    (substitute #\Space #\- (substitute #\Space #\_ name))))

(defun pick-title (path text kind)
  "What the hit is called: the file's own name, else its heading, else its path."
  (or (and text (frontmatter-name text))
      (and text (first (heading-lines text)))
      (and (string= kind "skill") (human-name (uiop:pathname-parent-directory-pathname path)))
      (human-name path)))

;;; --- the walk -------------------------------------------------------------

(defun scan-root (root &key (max-depth 6) (max-files 4000) (excluded *excluded-directories*))
  "Every (PATH . KIND) under ROOT within MAX-DEPTH, sorted by path."
  (let ((found '())
        (stack (list (cons root 0))))
    (loop while (and stack (< (length found) max-files))
          do (let* ((entry (pop stack))
                    (directory (car entry))
                    (depth (cdr entry)))
               (unless (eq :prose (file-kind directory)))
               (when (< (1+ depth) max-depth)
                 (dolist (child (ignore-errors (uiop:subdirectories directory)))
                   (unless (member (directory-name child) excluded :test #'string=)
                     (push (cons child (1+ depth)) stack))))
               (dolist (file (ignore-errors (uiop:directory-files directory)))
                 (let ((kind (file-kind file)))
                   (cond ((stringp kind) (push (cons file kind) found))
                         ((eq kind :prose)
                          (unless (member (directory-name directory) '("logs") :test #'string=)
                            (push (cons file (prose-kind file)) found))))))))
    (sort found (lambda (left right) (string< (namestring (car left)) (namestring (car right)))))))

(defun scan-artifact (path kind root)
  "One artifact from PATH, read once; the words it contributes are capped."
  (let* ((relative (relative-path path root))
         (text (and (member kind '("skill" "runbook" "doc") :test #'string=) (read-text path)))
         (headings (and text (heading-lines text)))
         (summary (and text (nlk:clip (prose-text text) 240)))
         (title (pick-title path text kind))
         (title-terms (remove-duplicates (words-of title) :test #'equal))
         (path-terms (remove-duplicates (words-of relative) :test #'equal)))
    (make-artifact :id (make-id kind relative)
                   :type kind
                   :title title
                   :path (or (ignore-errors (namestring (truename path))) (namestring path))
                   :relative relative
                   :summary summary
                   :headings headings
                   :terms (term-table (list text title (format nil "~{~a ~}" headings)))
                   :title-terms title-terms
                   :path-terms path-terms
                   :size (file-bytes path)
                   :modified (ignore-errors (file-write-date path)))))

;;; --- the corpus ------------------------------------------------------------

(defstruct (corpus (:constructor make-corpus))
  "One scan's artifacts, its index by id, and every word to the artifacts holding it."
  artifacts by-id tokens roots missing built-at)

(defun build-corpus (roots &key (max-depth 6) (max-files 4000) excluded)
  "Scan ROOTS into a corpus: CO:ROOTS and CO:MISSING report what was walked."
  (let ((artifacts '())
        (real '())
        (missing '())
        (seen (make-hash-table :test #'equal)))
    (dolist (root roots)
      (let ((directory (uiop:ensure-directory-pathname (nlk:trimmed root))))
        (if (ignore-errors (uiop:directory-exists-p directory))
            (progn
              (push (or (ignore-errors (namestring (truename directory))) (namestring directory)) real)
              (dolist (entry (scan-root directory :max-depth max-depth :max-files max-files
                                                   :excluded excluded))
                (when (< (hash-table-count seen) max-files)
                  (let* ((path (car entry))
                         (artifact (ignore-errors (scan-artifact path (cdr entry) directory))))
                    (when (and artifact (not (gethash (artifact-id artifact) seen)))
                      (setf (gethash (artifact-id artifact) seen) t)
                      (push artifact artifacts))))))
            (push (namestring directory) missing))))
    (let ((artifacts (sort artifacts #'string< :key #'artifact-id))
          (tokens (make-hash-table :test #'equal))
          (by-id (make-hash-table :test #'equal)))
      (dolist (artifact artifacts)
        (setf (gethash (artifact-id artifact) by-id) artifact)
        (maphash (lambda (word count)
                   (declare (ignore count))
                   (push artifact (gethash word tokens)))
                 (artifact-terms artifact)))
      (make-corpus :artifacts artifacts :by-id by-id :tokens tokens
                   :roots (nreverse real) :missing (nreverse missing)
                   :built-at (get-universal-time)))))

;;; --- scoring ---------------------------------------------------------------

(defun title-hit (artifact token)
  "How well TOKEN names the artifact's title: exact 6, prefix 4, else 0."
  (let ((best 0))
    (dolist (word (artifact-title-terms artifact) best)
      (cond ((string= token word) (setf best 6))
            ((and (<= (length token) (length word))
                  (string= token word :end2 (length token)))
             (setf best (max best 4)))))))

(defun body-hit (artifact token corpus)
  "How well TOKEN names the artifact's own words: exact 2, prefix 1, else 0."
  (cond ((gethash token (artifact-terms artifact)) 2)
        ((loop for word being the hash-keys of (corpus-tokens corpus)
               thereis (and (<= (length token) (length word))
                            (string= token word :end2 (length token))
                            (member artifact (gethash word (corpus-tokens corpus)) :test #'eq)))
         1)
        (t 0)))

(defun artifact-score (artifact tokens phrase corpus)
  "The artifact's score for TOKENS: where a word was found decides its weight."
  (let ((score 0)
        (title (string-downcase (artifact-title artifact)))
        (relative (string-downcase (artifact-relative artifact))))
    (dolist (token tokens)
      (incf score (max (title-hit artifact token)
                        (if (member token (artifact-path-terms artifact) :test #'string=) 3 0)
                        (body-hit artifact token corpus))))
    (when (and (plusp (length phrase)) (search phrase title)) (incf score 6))
    (when (and (plusp (length phrase)) (search phrase relative)) (incf score 3))
    score))

(defun search-corpus (corpus query &key type limit)
  "The artifacts CORPUS holds that QUERY routes to, best first.
=> a list of (SCORE . ARTIFACT), at most LIMIT of them."
  (let* ((phrase (string-downcase (nlk:trimmed query)))
         (tokens (remove-duplicates (words-of phrase) :test #'equal))
         (wanted (and type (plusp (length type)) type))
         (hits '()))
    (dolist (artifact (corpus-artifacts corpus))
      (when (or (null wanted) (string= wanted (artifact-type artifact)))
        (let ((score (artifact-score artifact tokens phrase corpus)))
          (when (plusp score) (push (cons score artifact) hits)))))
    (setf hits (sort hits (lambda (first second)
                            (let ((left (car first))
                                  (right (car second)))
                              (cond ((/= left right) (> left right))
                                    (t (string< (artifact-id (cdr first))
                                                (artifact-id (cdr second)))))))))
    (if (and limit (< limit (length hits))) (subseq hits 0 limit) hits)))

(defun matched-words (artifact tokens)
  "The query's words that this artifact actually carries, for the hit's one line."
  (let ((found '()))
    (dolist (token tokens (nreverse found))
      (when (or (> (title-hit artifact token) 0)
                (member token (artifact-path-terms artifact) :test #'string=)
                (gethash token (artifact-terms artifact)))
        (push token found)))))

;;; --- neighbours ------------------------------------------------------------

(defun siblings (corpus artifact)
  "The artifacts beside ARTIFACT in the same directory, by id."
  (let ((directory (directory-of (artifact-relative artifact))))
    (remove-if-not (lambda (other)
                     (and (not (eq other artifact))
                          (string= directory (directory-of (artifact-relative other)))))
                   (corpus-artifacts corpus))))

(defun directory-of (relative)
  "The directory part of RELATIVE, or \"\" at the top."
  (let ((slash (position #\/ relative :from-end t)))
    (if slash (subseq relative 0 slash) "")))

(defun support-docs (corpus artifact)
  "The artifacts under a skill's own directory: its references, templates, scripts."
  (let ((directory (directory-of (artifact-relative artifact))))
    (remove-if-not (lambda (other)
                     (let ((relative (artifact-relative other)))
                       (and (not (eq other artifact))
                            (plusp (length directory))
                            (<= (length directory) (length relative))
                            (string= directory relative :end2 (length directory))
                            (< (length directory) (length relative)))))
                   (corpus-artifacts corpus))))

(defun mentioning (corpus artifact &key (limit 4))
  "Artifacts whose own words name ARTIFACT by its distinctive stem, at most LIMIT."
  (let ((stem (artifact-stem artifact))
        (found '()))
    (when (>= (length stem) 4)
      (dolist (other (corpus-artifacts corpus))
        (when (and (not (eq other artifact)) (< (length found) limit)
                   (gethash stem (artifact-terms other)))
          (push other found))))
    (nreverse found)))

(defun artifact-stem (artifact)
  "The one word a document would name this artifact by, downcased."
  (let* ((relative (strip-extension (artifact-relative artifact)))
         (slash (position #\/ relative :from-end t))
         (name (if slash (subseq relative (1+ slash)) relative)))
    (string-downcase name)))

(defun neighbors-of (corpus artifact &key (limit 12))
  "The artifacts beside ARTIFACT, each with the relation that put them there."
  (let ((edges '()))
    (flet ((add (others relation)
             (dolist (other others)
               (unless (assoc (artifact-id other) edges :test #'string=)
                 (push (cons (artifact-id other) (cons relation other)) edges)))))
      (when (string= (artifact-type artifact) "skill")
        (add (support-docs corpus artifact) "supports"))
      (add (siblings corpus artifact) "sibling")
      (add (mentioning corpus artifact) "mentions"))
    (let ((edges (sort edges #'string< :key #'car)))
      (if (and limit (< limit (length edges))) (subseq edges 0 limit) edges))))
