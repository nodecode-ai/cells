;;;; cell.lisp --- a markdown wiki as long-term memory: four tools, a recall, /hwiki.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of ottosulin/hwiki's Hermes memory-provider plugin (see NOTICE).
;;;; The store keeps the original's rules: the curated wiki is read-only to
;;;; the agent, automatic writes land only under memory_dir, untrusted_dirs
;;;; are never indexed, recalled or readable, and a secret-shaped note is
;;;; refused. Its SQLite FTS5 index becomes one this cell builds in memory
;;;; (BM25 over heading-scoped chunks), so nothing here needs Python or a
;;;; database; qmd's hybrid retrieval is left out with it.

(defpackage #:nodecode-hwiki
  (:use #:cl)
  (:export #:hwiki-error #:hwiki-error-detail))

(in-package #:nodecode-hwiki)

(nlk:define-peripheral hwiki :not-running t :lock "hwiki")

;;; --- the words that do not change ---------------------------------------------

(defparameter +skipped-dirs+ '(".git" ".obsidian" "node_modules" ".trash")
  "Directories never walked, wherever they sit: the original's DEFAULT_EXCLUDES.")

(defparameter +default-untrusted+ '("raw")
  "untrusted_dirs when the section names none: third-party documents live here.")

(defparameter +default-demote+ '("log.md" "wiki/memory/builtin-mirror.md")
  "Pages ranked after the curated ones when the section names none.")

(defparameter +chunk-chars+ 1200
  "How long one chunk may be before it is split on a paragraph, as the original's 1200.")

(defparameter +link-cap+ 8
  "Outbound wikilinks attached to one hit, as the original's _LINK_CAP.")

(defparameter +max-files+ 20000
  "The most pages one index walks; past this the wiki is read in the order found.")

(defparameter +max-candidates+ 2000
  "The most scored chunks one search ranks before the partition.")

(defparameter +stopwords+
  '("the" "a" "an" "and" "or" "of" "to" "in" "on" "for" "is" "are" "was" "were"
    "be" "do" "does" "did" "what" "which" "who" "how" "why" "when" "where"
    "our" "we" "you" "i" "it" "this" "that" "with" "about" "from" "have" "has"
    "can" "should" "would" "please")
  "Query terms too common to rank, as the original's _STOPWORDS.")

;;; --- what the cell runs on --------------------------------------------------------

(defvar *root* nil
  "The wiki root as a directory pathname while the cell runs, else NIL.")

(defvar *memory-dir* "wiki/memory"
  "The one wiki-relative folder the agent may write to.")

(defvar *untrusted* '("raw")
  "Wiki-relative folders never indexed, recalled or readable, each without a trailing slash.")

(defvar *demote* '()
  "Wiki-relative pages ranked after the rest.")

(defvar *index* nil
  "The built index (WIKI-INDEX), or NIL before the first one.")

(defvar *writes* 0
  "Notes this session filed, as the original's writes_this_session.")

;;; --- small text and path helpers ---------------------------------------------------

(defun starts-with-p (prefix text)
  "Whether TEXT begins with PREFIX."
  (let ((n (length prefix)))
    (and (<= n (length text)) (string= prefix text :end2 n))))

(defun ends-with-p (suffix text)
  "Whether TEXT ends with SUFFIX."
  (let ((n (length suffix)) (m (length text)))
    (and (<= n m) (string= suffix text :start2 (- m n)))))

(defun unquote (text)
  "TEXT with a matching pair of ' or \" taken off."
  (let ((text (string-trim '(#\Space #\Tab) (or text ""))))
    (if (and (>= (length text) 2)
             (member (char text 0) '(#\' #\"))
             (char= (char text 0) (char text (1- (length text)))))
        (subseq text 1 (1- (length text)))
        text)))

(defun collapse-whitespace (text)
  "TEXT with every run of whitespace as one space."
  (with-output-to-string (out)
    (let ((space nil))
      (loop for character across (or text "")
            do (if (member character '(#\Space #\Tab #\Newline #\Return #\Page))
                   (setf space t)
                   (progn (when space (write-char #\Space out) (setf space nil))
                          (write-char character out)))))))

(defun split-lines (text)
  "TEXT's lines."
  (let ((lines '()) (start 0) (n (length text)))
    (loop
      (let ((position (position #\Newline text :start start)))
        (push (string-right-trim '(#\Return) (subseq text start (or position n))) lines)
        (if position (setf start (1+ position)) (return))))
    (nreverse lines)))

(defun split-paragraphs (text)
  "TEXT's paragraphs, split on a blank line."
  (let ((paragraphs '()) (start 0) (separator (format nil "~%~%")))
    (loop
      (let ((position (search separator text :start2 start)))
        (push (subseq text start (or position (length text))) paragraphs)
        (if position (setf start (+ position 2)) (return))))
    (nreverse paragraphs)))

(defun expand-home (path)
  "PATH with a leading ~ replaced by the home directory."
  (if (and (plusp (length path)) (char= (char path 0) #\~))
      (namestring (merge-pathnames (if (> (length path) 1) (subseq path 2) "")
                                    (user-homedir-pathname)))
      path))

(defun root-pathname (path)
  "PATH as a directory pathname, or a refusal naming it."
  (handler-case (uiop:ensure-directory-pathname (uiop:parse-native-namestring path))
    (error () (fail "wiki_path ~s is no path this system can read" path))))

(defun relative-name (file root)
  "FILE's name relative to ROOT with / separators, or NIL when it is outside."
  (let ((name (ignore-errors (enough-namestring (truename file) root))))
    (when (and name (plusp (length name))
               (not (char= (char name 0) #\/))
               (not (starts-with-p ".." name)))
      name)))

(defun under-dir-p (relative dir)
  "Whether RELATIVE is DIR or sits under it."
  (or (string= relative dir)
      (starts-with-p (concatenate 'string dir "/") relative)))

(defun untrusted-path-p (relative)
  "Whether RELATIVE sits in one of the section's untrusted_dirs."
  (some (lambda (dir) (under-dir-p relative dir)) *untrusted*))

(defun demoted-path-p (relative)
  "Whether RELATIVE is one of the section's demote_paths, at the root or below."
  (some (lambda (path)
          (or (string= relative path)
              (ends-with-p (concatenate 'string "/" path) relative)))
        *demote*))

(defun slugify (value &key (fallback "note"))
  "VALUE as a filesystem-safe lowercase slug, as the original's SLUGIFY."
  (let ((out (make-string-output-stream))
        (dash t))
    (loop for character across (string-downcase (or value ""))
          do (cond ((alphanumericp character)
                    (write-char character out)
                    (setf dash nil))
                   ((member character '(#\Space #\_ #\- #\. #\/ #\: #\,))
                    (unless dash (write-char #\- out) (setf dash t)))))
    (let ((slug (string-trim "-" (get-output-stream-string out))))
      (if (plusp (length slug)) (subseq slug 0 (min 80 (length slug))) fallback))))

(defun title-case (slug)
  "SLUG as words, each capitalized."
  (string-capitalize (substitute #\Space #\- slug)))

(defun list-setting (name default)
  "The section's list NAME as a list of strings, or DEFAULT when it names none."
  (let* ((value (ignore-errors (setting name)))
         (items (cond ((vectorp value) (coerce value 'list))
                      ((listp value) value)
                      ((and (stringp value) (plusp (length value))) (list value))
                      (t '()))))
    (or items default)))

;;; --- frontmatter, headings, links, tokens ------------------------------------------

(defun parse-frontmatter-lines (lines)
  "LINES of a YAML-ish block as an alist of (KEY . VALUE), VALUE a string or a list."
  (let ((pairs '()) (key nil))
    (dolist (line lines)
      (let ((trimmed (string-trim '(#\Space #\Tab) line)))
        (cond
          ((zerop (length trimmed)))
          ((char= (char trimmed 0) #\#))
          ((and (>= (length trimmed) 2) (char= (char trimmed 0) #\-)
                (member (char trimmed 1) '(#\Space #\Tab)))
           (when key
             (let* ((item (unquote (string-trim '(#\Space #\Tab) (subseq trimmed 2))))
                    (entry (assoc key pairs :test #'string=)))
               (if (and entry (listp (cdr entry)))
                   (setf (cdr entry) (append (cdr entry) (list item)))
                   (push (cons key (list item)) pairs)))))
          ((find #\: trimmed)
           (let* ((colon (position #\: trimmed))
                  (name (string-downcase (string-trim '(#\Space #\Tab)
                                                      (subseq trimmed 0 colon))))
                  (value (string-trim '(#\Space #\Tab) (subseq trimmed (1+ colon)))))
             (setf key (and (plusp (length name)) name))
             (when key
               (cond ((zerop (length value)) (push (cons key nil) pairs))
                     ((and (>= (length value) 2)
                           (char= (char value 0) #\[)
                           (char= (char value (1- (length value))) #\]))
                      (push (cons key (mapcar #'unquote
                                              (uiop:split-string (subseq value 1 (1- (length value)))
                                                                 :separator ",")))
                            pairs))
                     (t (push (cons key (unquote value)) pairs))))))
          (t nil))))
    (nreverse pairs)))

(defun parse-frontmatter (text)
  "TEXT as (values FRONTMATTER BODY): the block between the two --- lines at
the head, parsed to an alist, and what follows."
  (let ((lines (split-lines text)))
    (if (and lines (string= "---" (string-trim '(#\Space #\Tab) (first lines))))
        (let ((end (position "---" (rest lines) :test #'string=
                              :key (lambda (line) (string-trim '(#\Space #\Tab) line)))))
          (if end
              (values (parse-frontmatter-lines (subseq (rest lines) 0 end))
                      (format nil "~{~a~^~%~}" (nthcdr (+ end 2) lines)))
              (values nil text)))
        (values nil text))))

(defun frontmatter-value (frontmatter key)
  "KEY's value in FRONTMATTER, as parsed."
  (cdr (assoc key frontmatter :test #'string=)))

(defun stringify (value)
  "VALUE as one line of text: a list joined by commas, a string as it is."
  (typecase value
    (null "")
    (list (format nil "~{~a~^, ~}" (mapcar #'stringify value)))
    (t (princ-to-string value))))

(defun heading-line (line)
  "LINE as (values LEVEL TITLE) when it is a markdown heading, else NIL."
  (let ((trimmed (string-left-trim '(#\Space #\Tab) line)))
    (when (plusp (length trimmed))
      (let ((level (position-if-not (lambda (character) (char= character #\#)) trimmed)))
        (when (and level (<= 1 level 6) (< level (length trimmed))
                   (member (char trimmed level) '(#\Space #\Tab)))
          (values level (string-trim '(#\Space #\Tab #\#) (subseq trimmed level))))))))

(defun first-heading (body)
  "BODY's first heading as a title, or NIL."
  (loop for line in (split-lines body)
        do (multiple-value-bind (level title) (heading-line line)
             (declare (ignore level))
             (when (and title (plusp (length title))) (return title)))))

(defun wikilinks (text)
  "TEXT's [[wikilink]] targets, de-duplicated in order, folder and anchor dropped."
  (let ((targets '()) (start 0))
    (loop
      (let* ((open (search "[[" text :start2 start)))
        (unless open (return))
        (let* ((close (search "]]" text :start2 (+ open 2))))
          (unless close (return))
          (let* ((inner (subseq text (+ open 2) close))
                 (bar (or (position #\| inner) (length inner)))
                 (anchor (or (position #\# inner) (length inner)))
                 (target (string-trim '(#\Space #\Tab) (subseq inner 0 (min bar anchor)))))
            (when (plusp (length target)) (pushnew target targets :test #'string=)))
          (setf start (+ close 2)))))
    (nreverse targets)))

(defun token-character-p (character)
  "Whether CHARACTER may sit inside a term, as FTS5's tokenizer allows."
  (or (alphanumericp character) (member character '(#\_ #\. #\-))))

(defun tokenize (text)
  "TEXT's terms, lowercased: runs of letters, digits, _ . and - beginning
with a letter, digit or underscore, trailing . and - trimmed, singletons
dropped -- the original's FTS query tokens."
  (let ((terms '()) (index 0) (length (length text)))
    (loop while (< index length) do
      (let ((character (char text index)))
        (if (and (token-character-p character) (or (alphanumericp character)
                                                   (char= character #\_)))
            (let ((start index))
              (loop while (and (< index length) (token-character-p (char text index)))
                    do (incf index))
              (let* ((raw (subseq text start index))
                     (end (position-if-not (lambda (c) (member c '(#\. #\-)))
                                            raw :from-end t))
                     (term (and end (string-downcase (subseq raw 0 (1+ end))))))
                (when (and term (> (length term) 1)) (push term terms))))
            (incf index))))
    (nreverse terms)))

;;; --- the secret guard ---------------------------------------------------------------

(defun upper-alphanumeric-p (character)
  (or (upper-case-p character) (digit-char-p character)))

(defun narrow-alphanumeric-p (character)
  (or (alphanumericp character) (member character '(#\_ #\-))))

(defun base64url-p (character)
  (or (alphanumericp character) (member character '(#\_ #\-))))

(defun run-at-p (text start prefix count allowed)
  "Whether PREFIX, then COUNT characters allowed by ALLOWED, sit at START of TEXT."
  (let ((begin (+ start (length prefix)))
        (end (+ start (length prefix) count)))
    (and (<= end (length text))
         (string= prefix text :start2 start :end2 (+ start (length prefix)))
         (loop for index from begin below end always (funcall allowed (char text index))))))

(defun run-length-at (text start allowed)
  "How many characters ALLOWED allows from START of TEXT."
  (let ((index start))
    (loop while (and (< index (length text)) (funcall allowed (char text index)))
          do (incf index))
    (- index start)))

(defun credential-words ()
  '("password" "passwd" "secret" "api_key" "api-key" "apikey" "access_token"
    "access-token" "client_secret" "client-secret"))

(defun inline-credential-at-p (text start word)
  "Whether a WORD assignment holding 8+ characters begins at START of TEXT."
  (let ((after (+ start (length word))))
    (when (and (<= after (length text))
               (string-equal word text :start2 start :end2 after))
      (let ((index after))
        (loop while (and (< index (length text)) (member (char text index) '(#\Space #\Tab)))
              do (incf index))
        (when (and (< index (length text)) (member (char text index) '(#\: #\=)))
          (incf index)
          (loop while (and (< index (length text)) (member (char text index) '(#\Space #\Tab)))
                do (incf index))
          (when (and (< index (length text)) (member (char text index) '(#\" #\')))
            (incf index))
          (let ((run (run-length-at text index
                                    (lambda (c) (not (member c '(#\Space #\Tab #\Newline
                                                                 #\Return #\" #\' #\, #\;)))))))
            (>= run 8)))))))

(defun jwt-at-p (text start)
  "Whether a JSON web token begins at START of TEXT."
  (and (<= (+ start 3) (length text))
       (string-equal "eyJ" text :start2 start :end2 (+ start 3))
       (>= (+ start 3) 0)
       (let ((first (run-length-at text (+ start 3) #'base64url-p)))
         (and (>= first 10)
                (let ((dot (+ start 3 first)))
                  (and (< dot (length text)) (char= (char text dot) #\.)
                       (let ((second (run-length-at text (1+ dot) #'base64url-p)))
                         (and (>= second 10)
                              (let ((dot2 (+ dot 1 second)))
                                (and (< dot2 (length text)) (char= (char text dot2) #\.)
                                     (>= (run-length-at text (1+ dot2) #'base64url-p) 10)))))))))))

(defun scan-for-secrets (text)
  "Labels of the secret-shaped material TEXT carries, as the original's scan does."
  (let ((found '())
        (length (length (or text ""))))
    (flet ((note (label) (pushnew label found :test #'string=)))
      (loop for index from 0 below length do
        (cond
          ((run-at-p text index "AKIA" 16 #'upper-alphanumeric-p) (note "AWS access key id"))
          ((run-at-p text index "ASIA" 16 #'upper-alphanumeric-p) (note "AWS access key id"))
          ((run-at-p text index "SCW" 11 #'upper-alphanumeric-p) (note "Scaleway secret key"))
          ((some (lambda (prefix) (run-at-p text index prefix 20 #'narrow-alphanumeric-p))
                 '("ghp_" "gho_" "ghu_" "ghs_" "ghr_"))
           (note "GitHub token"))
          ((some (lambda (prefix) (run-at-p text index prefix 10 #'narrow-alphanumeric-p))
                 '("xoxa-" "xoxb-" "xoxp-" "xoxr-" "xoxs-"))
           (note "Slack token"))
          ((run-at-p text index "AIza" 35 #'narrow-alphanumeric-p) (note "Google API key"))
          ((run-at-p text index "sk-" 20 #'narrow-alphanumeric-p) (note "OpenAI-style key"))
          ((and (< index (- length 26))
                (string= "-----BEGIN" text :start2 index :end2 (+ index 10))
                (search "PRIVATE KEY-----" text :start2 index))
           (note "private key block"))
          ((jwt-at-p text index) (note "JWT"))
          ((some (lambda (word) (inline-credential-at-p text index word)) (credential-words))
           (note "inline credential")))))
    found))

;;; --- pages, sections, the index -----------------------------------------------------

(defstruct (wiki-page (:constructor make-wiki-page (path title slug frontmatter links)))
  "One markdown page of the wiki, as the index knows it."
  path title slug frontmatter links)

(defstruct (wiki-chunk (:constructor make-wiki-chunk (path title heading spine text tokens)))
  "One heading-scoped slice of a page: the unit of retrieval."
  path title heading spine text tokens)

(defstruct (wiki-index (:constructor make-wiki-index (&key pages chunks postings lengths average built-at)))
  "The whole index: its pages, its chunks, and the term postings over them."
  pages chunks postings lengths average built-at)

(defstruct (wiki-hit (:constructor make-wiki-hit (path title heading snippet score links)))
  "One search result."
  path title heading snippet score links)

(defun walk-markdown (root)
  "Every markdown file under ROOT as (ABSPATH . RELATIVE), skipping +SKIPPED-DIRS+."
  (let ((files '()) (seen (make-hash-table :test #'equal)))
    (labels ((walk (directory depth)
               (when (<= depth 24)
                 (let ((key (ignore-errors (namestring (truename directory)))))
                   (unless (and key (gethash key seen))
                     (when key (setf (gethash key seen) t))
                     (dolist (file (ignore-errors (uiop:directory-files directory)))
                       (when (string-equal "md" (pathname-type file))
                         (let ((relative (relative-name file root)))
                           (when relative (push (cons file relative) files)))))
                     (dolist (sub (ignore-errors (uiop:subdirectories directory)))
                       (unless (member (string-downcase (or (car (last (pathname-directory sub))) ""))
                                       +skipped-dirs+ :test #'string=)
                         (walk sub (1+ depth)))))))))
      (walk root 0))
    (let ((files (nreverse files)))
      (if (> (length files) +max-files+) (subseq files 0 +max-files+) files))))

(defun load-page (file relative)
  "FILE (RELATIVE its wiki-relative name) as (values PAGE BODY), or NIL."
  (let ((text (ignore-errors (nlk:read-text (namestring file)))))
    (when text
      (multiple-value-bind (frontmatter body) (parse-frontmatter text)
        (let* ((named (stringify (frontmatter-value frontmatter "title")))
               (title (cond ((plusp (length named)) named)
                            ((first-heading body))
                            (t (pathname-name file))))
               (slug (pathname-name file)))
          (values (make-wiki-page relative title slug frontmatter (wikilinks body))
                  body))))))

(defun wrap-text (text max)
  "TEXT cut into pieces of at most MAX characters on paragraph boundaries."
  (if (<= (length text) max)
      (list text)
      (let ((pieces '()) (buffer ""))
        (flet ((flush ()
                 (when (plusp (length buffer)) (push buffer pieces) (setf buffer ""))))
          (dolist (paragraph (split-paragraphs text))
            (cond ((> (length paragraph) max)
                   (flush)
                   (loop for index from 0 below (length paragraph) by max
                         do (push (subseq paragraph index (min (length paragraph) (+ index max)))
                                  pieces)))
                  ((> (+ (length buffer) (length paragraph) 2) max)
                   (flush)
                   (setf buffer paragraph))
                  (t (setf buffer (if (plusp (length buffer))
                                      (concatenate 'string buffer (format nil "~%~%") paragraph)
                                      paragraph)))))
          (flush))
        (nreverse pieces))))

(defun page-sections (page body)
  "BODY split on its level 1-3 headings, as a list of (HEADING . TEXT) chunks."
  (let ((sections '()) (heading (wiki-page-title page)) (lines '()))
    (flet ((finish-section ()
             (let ((text (string-trim '(#\Space #\Tab #\Newline)
                                       (format nil "~{~a~^~%~}" (nreverse lines)))))
               (when (plusp (length text))
                 (dolist (piece (wrap-text text +chunk-chars+))
                   (push (cons heading piece) sections))))))
      (dolist (line (split-lines body))
        (multiple-value-bind (level title) (heading-line line)
          (if (and level (<= level 3))
              (progn (finish-section) (setf heading (or title "")) (setf lines '()))
              (push line lines))))
      (finish-section))
    (nreverse sections)))

(defun build-index ()
  "Walk the wiki and index every chunk of every page: (WIKI-INDEX)."
  (let ((pages '()) (chunks '()) (postings (make-hash-table :test #'equal))
        (lengths '()) (total 0))
    (dolist (pair (walk-markdown *root*))
      (unless (untrusted-path-p (cdr pair))
        (multiple-value-bind (page body) (load-page (car pair) (cdr pair))
          (when page
            (push page pages)
            (dolist (section (page-sections page body))
              (let* ((tokens (tokenize (concatenate 'string (car section) " " (cdr section))))
                     (position (length chunks)))
                (push (make-wiki-chunk (wiki-page-path page) (wiki-page-title page)
                                       (car section)
                                       (stringify (frontmatter-value
                                                   (wiki-page-frontmatter page) "spine"))
                                       (cdr section) tokens)
                      chunks)
                (incf total (length tokens))
                (push (length tokens) lengths)
                (dolist (token tokens)
                  (let ((table (or (gethash token postings)
                                   (setf (gethash token postings) (make-hash-table)))))
                    (incf (gethash position table 0))))))))))
    (let ((chunks (coerce (nreverse chunks) 'vector))
          (lengths (coerce (nreverse lengths) 'vector)))
      (make-wiki-index :pages (nreverse pages)
                       :chunks chunks
                       :postings postings
                       :lengths lengths
                       :average (if (plusp (length chunks)) (/ total (length chunks)) 0)
                       :built-at (get-universal-time)))))

(defun current-index ()
  "The index, rebuilt when there is none or the last is older than reindex_seconds."
  (with-hwiki-lock
    (let ((seconds (setting :reindex-seconds)))
      (if (and *index* seconds
               (< (- (get-universal-time) (wiki-index-built-at *index*))
                  (max 1 seconds)))
          *index*
          (setf *index* (build-index))))))

;;; --- search ------------------------------------------------------------------------

(defun query-terms (query)
  "QUERY's terms, stopwords and singletons dropped, at most twelve."
  (let* ((tokens (tokenize (or query "")))
         (meaningful (remove-if (lambda (term)
                                  (or (member term +stopwords+ :test #'string=)
                                      (<= (length term) 1)))
                                tokens))
         (chosen (or meaningful tokens)))
    (subseq chosen 0 (min 12 (length chosen)))))

(defun expand-term (postings term)
  "The postings of TERM: its own first, then at most 32 indexed terms that
begin with it -- the prefix wildcard the original's MATCH query gives."
  (let ((exact (gethash term postings))
        (others '()))
    (maphash (lambda (key table)
               (when (and (not (string= key term))
                          (starts-with-p term key)
                          (plusp (hash-table-count table)))
                 (push key others)))
             postings)
    (let* ((sorted (sort others (lambda (left right) (< (length left) (length right)))))
           (kept (subseq sorted 0 (min 32 (length sorted)))))
      (remove nil (cons (and exact (cons term exact))
                        (loop for key in kept collect (cons key (gethash key postings))))))))

(defun rank-chunks (index terms)
  "A hash of chunk position to BM25 score, for TERMS in INDEX."
  (let ((scores (make-hash-table))
        (postings (wiki-index-postings index))
        (count (length (wiki-index-chunks index)))
        (average (max 1 (wiki-index-average index))))
    (when (plusp count)
      (dolist (term terms)
        (dolist (expansion (expand-term postings term))
          (let* ((frequency (hash-table-count (cdr expansion)))
                 (inverse (log (+ 1.0 (/ (- count frequency 0.5) (+ frequency 0.5))))))
            (maphash (lambda (position occurrences)
                       (let* ((length (aref (wiki-index-lengths index) position))
                              (norm (+ 0.25 (* 0.75 (/ length average))))
                              (weight (* inverse (/ (* occurrences 2.2)
                                                    (+ occurrences (* 1.2 norm))))))
                         (incf (gethash position scores 0) weight)))
                     (cdr expansion))))))
    scores))

(defun first-term-position (terms text)
  "Where the earliest of TERMS begins in TEXT, case-insensitively, or NIL."
  (let ((best nil))
    (dolist (term terms best)
      (let ((position (search term text :test #'char-equal)))
        (when (and position (or (null best) (< position best))) (setf best position))))))

(defun snippet-of (chunk terms &key (max 320))
  "CHUNK as the one-line excerpt a hit carries, around the first term that matched."
  (let* ((text (collapse-whitespace (wiki-chunk-text chunk)))
         (position (first-term-position terms text))
         (start (if position (max 0 (- position 80)) 0))
         (piece (subseq text start (min (length text) (+ start max)))))
    (format nil "~@[…~]~a~@[ …~]"
            (and (plusp start)) piece
            (and (< (+ start (length piece)) (length text))))))

(defun partition-hits (hits limit)
  "HITS -- ranked excerpts -- capped to LIMIT with at most max_hits_per_page
headings a page, demote_paths filled last, as the original's partition does."
  (let ((seen (make-hash-table :test #'equal))
        (per (make-hash-table :test #'equal))
        (primary '()) (secondary '())
        (cap (max 1 (or (ignore-errors (setting :max-hits-per-page)) 2))))
    (dolist (hit hits)
      (let ((key (list (wiki-hit-path hit) (wiki-hit-heading hit))))
        (unless (gethash key seen)
          (setf (gethash key seen) t)
          (if (demoted-path-p (wiki-hit-path hit))
              (push hit secondary)
              (when (< (gethash (wiki-hit-path hit) per 0) cap)
                (incf (gethash (wiki-hit-path hit) per 0))
                (push hit primary))))))
    (let ((chosen (nreverse primary))
          (last-chance (nreverse secondary)))
      (when (< (length chosen) limit)
        (let ((extra '()))
          (dolist (hit last-chance)
            (when (and (< (+ (length chosen) (length extra)) limit)
                       (< (gethash (wiki-hit-path hit) per 0) cap))
              (incf (gethash (wiki-hit-path hit) per 0))
              (push hit extra)))
          (setf chosen (append chosen (nreverse extra)))))
      (subseq chosen 0 (min limit (length chosen))))))

(defun search-wiki (query &key (limit 6) spine)
  "The excerpts QUERY recalls from the wiki, ranked: a list of WIKI-HIT."
  (let* ((index (current-index))
         (terms (query-terms query)))
    (if (null terms)
        '()
        (let ((scores (rank-chunks index terms))
              (hits '()))
          (let ((ranked (sort (loop for position being the hash-keys of scores
                                      using (hash-value score)
                                    collect (cons position score))
                              #'> :key #'cdr)))
            (dolist (pair (subseq ranked 0 (min +max-candidates+ (length ranked))))
              (let ((chunk (aref (wiki-index-chunks index) (car pair))))
                (unless (untrusted-path-p (wiki-chunk-path chunk))
                  (when (or (zerop (length (or spine "")))
                            (string-equal spine (wiki-chunk-spine chunk)))
                    (push (make-wiki-hit (wiki-chunk-path chunk) (wiki-chunk-title chunk)
                                          (wiki-chunk-heading chunk)
                                          (snippet-of chunk terms)
                                          (cdr pair)
                                          (page-links-of index (wiki-chunk-path chunk)))
                          hits))))))
          (partition-hits (nreverse hits) limit)))))

;;; --- pages by reference, links, lint --------------------------------------------------

(defun page-links-of (index relative &aux (found '()))
  "RELATIVE's outbound wikilinks, capped, as the original attaches them to a hit."
  (let ((page (find relative (wiki-index-pages index) :key #'wiki-page-path :test #'string=)))
    (dolist (link (and page (wiki-page-links page)) (nreverse found))
      (when (< (length found) +link-cap+) (push link found)))))

(defun backlinks-of (index slug)
  "The pages of INDEX carrying a wikilink that resolves to SLUG, sorted."
  (let ((found '()))
    (dolist (page (wiki-index-pages index))
      (when (some (lambda (link) (string-equal slug (slugify link)))
                  (wiki-page-links page))
        (pushnew (wiki-page-slug page) found :test #'string=)))
    (sort found #'string<)))

(defun page-at-path (relative)
  "RELATIVE, a wiki-relative path, as (values PAGE BODY) or NIL; never outside the wiki."
  (when (and (plusp (length relative))
             (not (search ".." relative))
             (not (untrusted-path-p relative)))
    (let ((file (merge-pathnames relative *root*)))
      (when (ignore-errors (uiop:file-exists-p file))
        (let ((name (relative-name file *root*)))
          (when (and name (not (untrusted-path-p name)))
            (load-page file name)))))))

(defun reference-stem (reference)
  "REFERENCE's file name without a directory or .md."
  (let* ((slash (position #\/ reference :from-end t))
         (name (if slash (subseq reference (1+ slash)) reference)))
    (if (ends-with-p ".md" name) (subseq name 0 (- (length name) 3)) name)))

(defun resolve-page (reference)
  "REFERENCE -- a path, a slug, a [[wikilink]] target or a title -- as
(values PAGE BODY), or NIL when no one page answers it."
  (let* ((reference (string-trim '(#\Space #\Tab #\[ #\]) (or reference ""))))
    (when (plusp (length reference))
      (or (page-at-path reference)
          (page-at-path (concatenate 'string reference ".md"))
          (let* ((index (current-index))
                 (slug (slugify (reference-stem reference)))
                 (by-slug (remove-if-not (lambda (page) (string-equal slug (wiki-page-slug page)))
                                          (wiki-index-pages index)))
                 (by-title (remove-if-not (lambda (page)
                                            (string-equal slug (slugify (wiki-page-title page))))
                                          (wiki-index-pages index)))
                 (match (or (and (= 1 (length by-slug)) (first by-slug))
                            (and (= 1 (length by-title)) (first by-title)))))
            (and match (page-at-path (wiki-page-path match))))))))

;;; --- writing into the memory zone -----------------------------------------------------

(defun today ()
  "Today as YYYY-MM-DD."
  (multiple-value-bind (second minute hour day month year) (get-decoded-time)
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

(defun memory-directory ()
  "The memory zone as a directory pathname under the wiki root."
  (uiop:ensure-directory-pathname
   (merge-pathnames (concatenate 'string (string-right-trim "/" *memory-dir*) "/") *root*)))

(defun write-text-file (path text)
  "TEXT into PATH as UTF-8, its parents made."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                             :if-does-not-exist :create :external-format :utf-8)
    (write-string text out))
  path)

(defun append-note (content &key topic page tags source)
  "Append CONTENT as a dated bullet to the memory-zone page PAGE (or TOPIC,
or journal): (values RELATIVE CREATED DUPLICATE). Refuses a secret."
  (let* ((content (collapse-whitespace content))
         (tag-list (remove-if (lambda (tag) (zerop (length (string-trim '(#\Space) tag))))
                              (mapcar (lambda (tag) (if (stringp tag) tag (princ-to-string tag)))
                                      tags))))
    (when (zerop (length content)) (fail "content is empty"))
    (let ((secrets (scan-for-secrets content)))
      (when secrets
        (fail "refusing to write apparent secret material into the wiki (~{~a~^, ~}); store a pointer to where it lives instead"
              secrets)))
    (let* ((slug (slugify (or (and page (plusp (length page)) page)
                               (and topic (plusp (length topic)) topic)
                               "journal")))
           (title (or (and topic (plusp (length topic)) topic)
                      (title-case slug)))
           (target (merge-pathnames (concatenate 'string slug ".md") (memory-directory)))
           (relative (concatenate 'string (string-right-trim "/" *memory-dir*) "/" slug ".md"))
           (stamp (today))
           (suffix (with-output-to-string (out)
                     (dolist (tag tag-list)
                       (format out " #~a" (slugify tag)))
                     (when (and source (plusp (length source)))
                       (format out " _(source: ~a)_" source))))
           (entry (format nil "- **~a** — ~a~a" stamp content suffix))
           (existing (and (ignore-errors (uiop:file-exists-p target))
                          (nlk:read-text (namestring target)))))
      (cond
        ((null existing)
         (write-text-file
          target
          (format nil "---~%title: ~a~%spine: memory~%type: memory~%status: auto~%~
                       source: agent-memory~%updated: ~a~%---~%~%# ~a~%~%~
                       > Auto-maintained by the `hwiki` cell. Promote durable conclusions into the
> curated wiki rather than editing in place.~%~%## Log~%~%~a~%"
                  title stamp title entry))
         (values relative t nil))
        ((search (string-downcase content) (string-downcase existing))
         (values relative nil t))
        (t
         (let ((text (if (search "## Log" existing)
                         existing
                         (concatenate 'string (string-right-trim '(#\Newline) existing)
                                      (format nil "~%~%## Log~%")))))
           (write-text-file target (concatenate 'string (string-right-trim '(#\Newline) text)
                                                (format nil "~%~a~%" entry)))
           (values relative nil nil)))))))

;;; --- what the model reads ------------------------------------------------------------

(defun format-hit (hit)
  "HIT as one readable line with its page, heading and links."
  (with-output-to-string (out)
    (let ((heading (wiki-hit-heading hit))
          (title (wiki-hit-title hit)))
      (format out "- **~a**~@[ › ~a~] (`~a`): ~a"
              title
              (and (plusp (length heading)) (not (string-equal heading title)) heading)
              (wiki-hit-path hit) (wiki-hit-snippet hit)))
    (when (wiki-hit-links hit)
      (format out "~%  links: ~{[[~a]]~^ ~}" (wiki-hit-links hit)))))

(defun search-text (query &key (limit 6) spine)
  "QUERY's excerpts as the text the model reads, or a line saying nothing matched."
  (let* ((index (current-index))
         (hits (search-wiki query :limit limit :spine spine))
         (pages (length (wiki-index-pages index)))
         (chunks (length (wiki-index-chunks index))))
    (if (null hits)
        (format nil "No excerpt of the wiki matches ~s (pages=~d, chunks=~d)." query pages chunks)
        (with-output-to-string (out)
          (format out "~d excerpt~:p for ~s (pages=~d, chunks=~d):~%"
                  (length hits) query pages chunks)
          (dolist (hit hits) (write-string (format-hit hit) out) (terpri out))
          (format out "Call wiki_memory_read for the full page before relying on a snippet.")))))

(defun print-hit (hit stream &optional colon at)
  "FORMAT helper: HIT as one line."
  (declare (ignore colon at))
  (write-string (format-hit hit) stream))

(defun read-text (page body max-chars)
  "PAGE and BODY as the text the model reads, body cut at MAX-CHARS."
  (let* ((index (current-index))
         (truncated (> (length body) max-chars))
         (content (subseq body 0 (min (length body) max-chars))))
    (with-output-to-string (out)
      (format out "Page `~a` — ~a~%" (wiki-page-path page) (wiki-page-title page))
      (let ((frontmatter (remove-if (lambda (pair) (null (cdr pair)))
                                    (wiki-page-frontmatter page))))
        (when frontmatter
          (format out "frontmatter: ~{~a=~a~^, ~}~%"
                  (loop for pair in frontmatter append (list (car pair) (stringify (cdr pair)))))))
      (when (wiki-page-links page)
        (format out "outbound links: ~{[[~a]]~^ ~}~%" (wiki-page-links page)))
      (let ((backlinks (backlinks-of index (wiki-page-slug page))))
        (when backlinks
          (format out "backlinks: ~{~a~^, ~}~%" backlinks)))
      (format out "--- content ---~%~a" content)
      (when truncated
        (format out "~%[truncated at ~d characters]" max-chars)))))

(defun status-text (&key lint)
  "The wiki's health as text: what is indexed, what may be written, and with
LINT the dangling wikilinks and orphan pages."
  (let* ((index (current-index))
         (pages (wiki-index-pages index))
         (chunks (length (wiki-index-chunks index)))
         (memory-root (memory-directory))
         (memory-pages (sort (mapcar #'pathname-name
                                     (ignore-errors
                                       (directory (merge-pathnames "*.md" memory-root))))
                             #'string<)))
    (with-output-to-string (out)
      (format out "hwiki: wiki root ~a, backend local-bm25~%" (namestring *root*))
      (format out "indexed: ~d page~:p, ~d chunk~:p; memory zone ~a (~d page~:p)~%"
              (length pages) chunks *memory-dir* (length memory-pages))
      (format out "untrusted dirs (never indexed): ~{~a~^, ~}~%" *untrusted*)
      (format out "writes this session: ~d~%" *writes*)
      (when lint
        (multiple-value-bind (dangling orphans) (lint-report index)
          (format out "lint: ~d dangling wikilink~:p, ~d orphan page~:p~%"
                  (length dangling) (length orphans))
          (dolist (entry (subseq dangling 0 (min 20 (length dangling))))
            (format out "  dangling: ~a -> ~{[[~a]]~^ ~}~%" (car entry) (cdr entry)))
          (when orphans
            (format out "  orphans: ~{~a~^, ~}~%" (subseq orphans 0 (min 50 (length orphans))))))))))

(defun lint-report (index)
  "The wiki's (values DANGLING ORPHANS): wikilinks pointing nowhere, and pages
nothing links to, as the original's lint has them."
  (let ((known (make-hash-table :test #'equal))
        (backlinks (make-hash-table :test #'equal))
        (dangling '()))
    (dolist (page (wiki-index-pages index))
      (setf (gethash (slugify (wiki-page-slug page)) known) t)
      (setf (gethash (slugify (wiki-page-title page)) known) t))
    (dolist (page (wiki-index-pages index))
      (let ((missing '()))
        (dolist (link (wiki-page-links page))
          (let ((slug (slugify link)))
            (if (gethash slug known)
                (pushnew (wiki-page-slug page) (gethash slug backlinks) :test #'string=)
                (push link missing))))
        (when missing (push (cons (wiki-page-path page) (nreverse missing)) dangling))))
    (values (nreverse dangling)
            (loop for page in (wiki-index-pages index)
                  unless (or (gethash (slugify (wiki-page-slug page)) backlinks)
                             (string= "index" (wiki-page-slug page)))
                    collect (wiki-page-slug page)))))

;;; --- recall before a request -----------------------------------------------------------

(defun session-source (session-id)
  "What admitted SESSION-ID's turns: \"channel\", \"cron\", \"cli\", ... or NIL."
  (ignore-errors (and session-id (nlk:session-source-kind session-id))))

(defun turn-input (session-id)
  "The words SESSION-ID's newest turn was admitted with, or NIL."
  (ignore-errors
    (nlk:json-value (nlk:events :session-id session-id :kind "turn.started"
                                :order :newest :limit 1 :as :payload)
                    :string "input")))

(defun recall-text (query)
  "The wiki memory block QUERY recalls -- the original's _recall, budget and
framing included -- or \"\" when nothing matches or the query is too short."
  (let ((query (string-trim '(#\Space #\Tab #\Newline) (or query ""))))
    (if (< (length query) (or (ignore-errors (setting :min-query-chars)) 8))
        ""
        (let* ((budget (or (ignore-errors (setting :prefetch-max-chars)) 1400))
               (hits (search-wiki query :limit (or (ignore-errors (setting :prefetch-limit)) 4)))
               (used 0))
          (if (null hits)
              ""
              (with-output-to-string (out)
                (format out "## Wiki Memory (recalled)~%")
                (block lines
                  (dolist (hit hits)
                    (let* ((heading (wiki-hit-heading hit))
                           (label (format nil "~a~@[ › ~a~]" (wiki-hit-title hit)
                                          (and (plusp (length heading))
                                               (not (string-equal heading (wiki-hit-title hit)))
                                               heading)))
                           (room (- budget used))
                           (snippet (subseq (wiki-hit-snippet hit) 0
                                            (min (length (wiki-hit-snippet hit)) (max 0 room)))))
                      (when (zerop (length snippet)) (return-from lines))
                      (format out "- **~a** (`~a`): ~a~%" label (wiki-hit-path hit) snippet)
                      (incf used (length snippet))
                      (when (>= used budget) (return-from lines)))))
                (format out "_Use wiki_memory_read for the full page before relying on an excerpt._")))))))

(defun recall-at-round (op)
  "At OP, one model request about to be built: the excerpts this turn's
prompt recalls, filed as the turn's context so the very request carries
them. The first round only, and never for a scheduled run."
  (let ((session (getf op :session-id))
        (turn (getf op :turn))
        (round (getf op :round)))
    (when (and session turn (eql 0 round)
               (or (ignore-errors (setting :auto-recall)) nil)
               (not (equal "cron" (session-source session))))
      (let ((prompt (turn-input session)))
        (when prompt
          (let ((text (recall-text prompt)))
            (when (plusp (length text))
              (nlk:record-turn-context-prompt turn :text text))))))))

(defun standing-primer (next session-id)
  "Every request's harness sections, with hwiki's first: where the memory is
and when to reach for it, as the original's system_prompt_block says."
  (let ((pages (if *index* (length (wiki-index-pages *index*)) 0)))
    (acons "hwiki"
           (format nil "Long-term memory is a markdown wiki at ~a (~d pages indexed). ~
                        Call wiki_memory_search before answering anything durable notes may ~
                        cover, wiki_memory_read to open a page and follow its [[wikilinks]], ~
                        and wiki_memory_write to file a durable fact -- never a secret or a ~
                        task's progress. Curated pages are edited by hand; automatic writes ~
                        land in ~a."
                   (if *root* (namestring *root*) "wiki_path") pages *memory-dir*)
           (remove "hwiki" (funcall next session-id) :key #'car :test #'equal))))

;;; --- the tools --------------------------------------------------------------------

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the `ERROR:' line the model repairs from."
  `(handler-case (progn ,@body)
     (error (condition) (nle:failure (format nil "ERROR: hwiki: ~a" condition)))))

(defun argument-text (args name)
  "ARGS' NAME as a trimmed non-empty string, or a refusal naming it."
  (let ((value (gethash name args)))
    (if (and (stringp value) (plusp (length (string-trim '(#\Space #\Tab #\Newline) value))))
        (string-trim '(#\Space #\Tab #\Newline) value)
        (fail "~a is required" (substitute #\_ #\- name)))))

(defun optional-text (args name)
  "ARGS' NAME as a trimmed string, or \"\"."
  (let ((value (gethash name args)))
    (if (stringp value) (string-trim '(#\Space #\Tab #\Newline) value) "")))

(defun argument-integer (args name default)
  "ARGS' NAME as an integer, or DEFAULT."
  (let ((value (gethash name args)))
    (cond ((integerp value) value)
          ((floatp value) (round value))
          (t default))))

(defun argument-strings (args name)
  "ARGS' NAME as a list of strings: a JSON array, or a comma-separated string."
  (let ((value (gethash name args)))
    (cond ((vectorp value) (coerce value 'list))
          ((listp value) value)
          ((and (stringp value) (plusp (length value)))
           (remove-if (lambda (part) (zerop (length part)))
                      (mapcar (lambda (part) (string-trim '(#\Space) part))
                              (uiop:split-string value :separator ","))))
          (t '()))))

(defun argument-boolean (args name default)
  "ARGS' NAME as a boolean, or DEFAULT."
  (multiple-value-bind (value present) (gethash name args)
    (if present (and value t) default)))

(defun cron-turn-p ()
  "Whether the running turn was started by the cron cell: writes are read-only
there, as the original's non-primary contexts are."
  (let* ((turn (ignore-errors (nle:turn)))
         (session (and turn (getf turn :session-id))))
    (and session (equal "cron" (session-source session)))))

(defun tool-search (args)
  "wiki_memory_search: ranked excerpts of the wiki for a question or keywords."
  (answering
    (let ((query (argument-text args "query"))
          (limit (max 1 (min (argument-integer args "limit" 6) 20))))
      (search-text query :limit limit :spine (optional-text args "spine")))))

(defun tool-read (args)
  "wiki_memory_read: one page in full, with its frontmatter, links and backlinks."
  (answering
    (let* ((reference (argument-text args "page"))
           (max-chars (max 200 (min (argument-integer args "max_chars" 8000) 40000))))
      (multiple-value-bind (page body) (resolve-page reference)
        (if page
            (read-text page body max-chars)
            (fail "page not found: ~a (a path, slug, [[wikilink]] target or title of a page outside the untrusted dirs)"
                  reference))))))

(defun tool-write (args)
  "wiki_memory_write: a dated bullet appended to the memory zone of the wiki."
  (answering
    (when (cron-turn-p)
      (fail "wiki memory is read-only in a scheduled run: only an interactive turn may file a durable fact"))
    (let ((content (argument-text args "content")))
      (multiple-value-bind (relative created duplicate)
          (append-note content
                       :topic (optional-text args "topic")
                       :page (optional-text args "page")
                       :tags (argument-strings args "tags")
                       :source (optional-text args "source"))
        (incf *writes*)
        (setf *index* nil)  ; the next search walks the wiki with the note in it
        (cond (duplicate (format nil "An equivalent fact is already in `~a`; nothing appended." relative))
              (created (format nil "Filed to `~a` (page created)." relative))
              (t (format nil "Filed to `~a`." relative)))))))

(defun tool-status (args)
  "wiki_memory_status: what is indexed, where writes land, and with lint what
the knowledge graph is missing."
  (answering
    (status-text :lint (argument-boolean args "lint" nil))))

;;; --- the schemas ---------------------------------------------------------------------

(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES -- (NAME TYPE DOC REQUIRED-P) ... -- as a tool schema."
  (list :description description
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (apply #'nlk:make-json-object
                             (loop for (name type doc) in properties
                                   nconc (list name (nlk:json-object "type" type
                                                                     "description" doc))))
         "required" (coerce (loop for property in properties
                                   when (fourth property) collect (first property))
                            'vector))))

(defparameter *search-schema*
  (tool-schema
   "Search long-term memory: the markdown knowledge wiki. Use this BEFORE answering any question that durable notes may already cover (past decisions, people, systems, compliance posture, prior incidents, vendor choices, recurring answers). Returns ranked page excerpts with their wiki paths. Follow up with wiki_memory_read to load a full page when an excerpt looks relevant. Each result also lists that page's outbound [[wikilinks]] as follow-up hints -- read a linked page when the excerpt is partial."
   '(("query" "string" "Natural-language question or keywords." t)
     ("limit" "integer" "Max results (default 6, max 20)." nil)
     ("spine" "string" "Optional frontmatter 'spine' filter (e.g. compliance, product)." nil)))
  "The wiki_memory_search tool, as the original's WIKI_SEARCH_SCHEMA has it.")

(defparameter *read-schema*
  (tool-schema
   "Read a wiki page in full by path, slug, or [[wikilink]] name. Also returns the page's frontmatter, outbound links and backlinks so you can traverse the knowledge graph."
   '(("page" "string" "Page path ('wiki/concepts/le-chonk.md'), slug ('le-chonk'), or wikilink target." t)
     ("max_chars" "integer" "Truncate the body at this many characters (default 8000)." nil)))
  "The wiki_memory_read tool, as the original's WIKI_READ_SCHEMA has it.")

(defparameter *write-schema*
  (tool-schema
   "File a durable fact into long-term memory. Writes a dated bullet into the wiki's memory zone -- the curated wiki pages are never modified. WRITE when: the user states a preference or correction, a decision is made with a rationale, you learn a stable fact about the environment, or an interpretation should be reused next time. DO NOT write: secrets, tokens, credentials, personal data, task progress, or anything stale within a week. Group related facts by passing the same 'page'."
   '(("content" "string" "The fact, as one self-contained declarative sentence." t)
     ("page" "string" "Memory page to append to (e.g. 'decisions', 'vendors'). Defaults to 'journal'." nil)
     ("topic" "string" "Human-readable title used when the page is created." nil)
     ("tags" "array" "Optional tags appended as #hashtags." nil)
     ("source" "string" "Optional provenance (URL, ticket key, document id)." nil)))
  "The wiki_memory_write tool, as the original's WIKI_WRITE_SCHEMA has it.")

(defparameter *status-schema*
  (tool-schema
   "Inspect memory health: wiki root, active search backend, page/chunk counts, memory-zone pages, and knowledge-graph hygiene (dangling wikilinks, orphan pages). Use when asked what the agent remembers, or to debug recall."
   '(("lint" "boolean" "Include dangling-wikilink and orphan-page report." nil)))
  "The wiki_memory_status tool, as the original's WIKI_STATUS_SCHEMA has it.")

;;; --- /hwiki and the manual -------------------------------------------------------------

(defparameter +primer+
  "hwiki is a folder of markdown pages used as long-term memory: you and the agent edit the
same files, and the agent recalls from them before every request.
  wiki_memory_search {query, limit?, spine?}  ranked excerpts with their wiki paths
  wiki_memory_read {page, max_chars?}        one page in full, with its links and backlinks
  wiki_memory_write {content, page?, topic?, tags?, source?}  a dated bullet in the memory zone
  wiki_memory_status {lint?}                 what is indexed; with lint, dangling links and orphans
Curated pages anywhere under the wiki are read-only to the agent: automatic writes land only
in memory_dir as dated bullets you review and promote. The folders in untrusted_dirs (raw by
default) hold third-party documents; they are never indexed, recalled or read, so an injected
instruction in one cannot reach the model through this memory. Never file a secret: a
secret-shaped note is refused.
The hwiki section of config.jsonc names wiki_path (the only required member), memory_dir,
untrusted_dirs, demote_paths, auto_recall and the recall budget. /hwiki shows the index,
/hwiki QUERY searches it, /hwiki lint names dangling links and orphans, /hwiki index rebuilds."
  "What (help :hwiki) answers while the cell runs.")

(defun run-command (args session-id)
  "/hwiki: no argument shows the index, lint names the gaps, index rebuilds it, and
anything else searches the wiki."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (lower (string-downcase text)))
    (cond
      ((zerop (length text)) (status-text))
      ((string= lower "status") (status-text))
      ((string= lower "lint") (status-text :lint t))
      ((member lower '("index" "reindex") :test #'string=)
       (setf *index* (build-index))
       (format nil "Rebuilt: ~d pages, ~d chunks."
               (length (wiki-index-pages *index*)) (length (wiki-index-chunks *index*))))
      ((starts-with-p "search " lower) (search-text (subseq text 7)))
      (t (search-text text)))))

;;; --- the cell -----------------------------------------------------------------------

(defun check-config ()
  "Refuse a section that names no wiki, or one that is not there, naming the member."
  (let ((path (expand-home (stringify (setting :wiki-path)))))
    (when (zerop (length path))
      (error 'nlk:config-refusal
             :detail "hwiki: wiki_path is not set: name the folder of markdown pages this memory reads"))
    (unless (ignore-errors (uiop:directory-exists-p (root-pathname path)))
      (error 'nlk:config-refusal
             :detail (format nil "hwiki: wiki_path ~s is not a directory: create it, or point it at the folder of markdown pages (an Obsidian vault works), then (restart-cells \"nodecode-hwiki\")"
                             path)))))

(defun start ()
  "Read the section, index the wiki, and answer the stop thunk."
  (check-config)
  (let* ((path (expand-home (stringify (setting :wiki-path))))
         (memory (stringify (setting :memory-dir))))
    (setf *root* (root-pathname path)
          *memory-dir* (if (plusp (length memory)) memory "wiki/memory")
          *untrusted* (mapcar (lambda (dir) (string-right-trim "/" dir))
                               (list-setting :untrusted-dirs +default-untrusted+))
          *demote* (list-setting :demote-paths +default-demote+)
          *writes* 0
          *index* (build-index))
    (nle:on-stop (lambda ()
                   (setf *root* nil *index* nil *writes* 0)))))

(nle:define-cell hwiki
  (:section ("hwiki")
    (:guide "wiki_path is the only required member: a folder of markdown pages, an Obsidian vault included; memory_dir is the one folder the agent writes to; untrusted_dirs holds third-party documents that are never recalled; then /hwiki shows the index")
    ("wiki_path" :path :required t
     :doc "the wiki root: a folder of markdown pages, indexed and recalled")
    ("memory_dir" :string :default "wiki/memory"
     :doc "the wiki-relative folder the agent may write to; curated pages are never modified")
    ("untrusted_dirs" :list
     :doc "wiki-relative folders never indexed, recalled or readable (default: raw) -- where third-party documents go")
    ("demote_paths" :list
     :doc "wiki-relative pages ranked after the rest (default: log.md and wiki/memory/builtin-mirror.md)")
    ("auto_recall" :boolean :default t
     :doc "hand the model the excerpts a turn's prompt recalls; off leaves the tools and /hwiki")
    ("prefetch_limit" :integer :default 4 :min 1
     :doc "how many excerpts one turn recalls")
    ("prefetch_max_chars" :integer :default 1400 :min 200
     :doc "the longest recall one turn carries, in characters")
    ("min_query_chars" :integer :default 8 :min 0
     :doc "a shorter prompt recalls nothing")
    ("max_hits_per_page" :integer :default 2 :min 1
     :doc "how many excerpts one page may contribute to one search")
    ("reindex_seconds" :integer :default 60 :min 0
     :doc "how long an index is reused before the wiki is walked again"))
  (:help :hwiki "hwiki: a markdown wiki as long-term memory -- wiki_memory_search, wiki_memory_read, wiki_memory_write, /hwiki" +primer+)
  (:command "hwiki" 'run-command
            :catches 'hwiki-error
            :description "Show the wiki memory, search it, lint it, or rebuild its index"
            :argument-hint "QUERY | lint | index")
  (:tool "wiki_memory_search" *search-schema* 'tool-search)
  (:tool "wiki_memory_read" *read-schema* 'tool-read)
  (:tool "wiki_memory_write" *write-schema* 'tool-write)
  (:tool "wiki_memory_status" *status-schema* 'tool-status)
  (:hook :round (nle:observer #'recall-at-round "nodecode-hwiki"))
  (:hook 'nle::read-harness-sections #'standing-primer)
  (:start (lambda () (start))))
