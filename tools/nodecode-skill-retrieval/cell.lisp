;;;; cell.lisp --- BM25 skill retrieval: the section, the tool, /skill-search, the round.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the skill-retrieval plugin for Hermes Agent: a library of
;;;; SKILL.md skills is ranked by BM25 against what a turn is about, so the few
;;;; that fit are in front of the model without the whole library being read.
;;;;
;;;; What one turn costs here is the half the plugin keeps per turn:
;;;;   the round hook   the turn's own input is ranked and the best k skills
;;;;                    ride that very request, once per turn.
;;;;   skill_search     the same ranking on demand, with each skill's id, so
;;;;                    the model reads the steps of the one it needs.
;;;; Nothing leaves the machine: the index is built from the roots' own files,
;;;; in memory, and rebuilt only when a SKILL.md's mtime or size moves.

(defpackage #:nodecode-skill-retrieval
  (:use #:cl)
  (:export #:start-cell #:search-skills #:retrieve #:current-index #:indexed-count
           #:skill-retrieval-error))

(in-package #:nodecode-skill-retrieval)

(nlk:define-peripheral skill-retrieval :not-running t)

;;; --- the manual ---------------------------------------------------------------

(defparameter +primer+
  "Skill retrieval ranks a library of SKILL.md files by BM25 against a few words
and answers the best matches, each with the description and the id of the
folder holding its steps.

  skill_search          a tool, given a query (deploy a docker container) and
                        an optional top_k. Lines read `- **name** (id):
                        description', best first.
  /skill-search WORDS   the same ranking, typed by hand; alone it says what
                        the roots hold.

Every turn also injects the top-k skills that match its own input, under
`## Retrieved skills', once per turn. The roots section lists the folders
scanned for SKILL.md files; a file's name and description come from its
frontmatter, and its triggers and tags are indexed beside them. Nothing is
sent anywhere: the index lives in memory, built from the files under the roots."
  "What (help :skill-retrieval) answers while the cell runs.")

;;; --- reading a SKILL.md ---------------------------------------------------------

(defparameter +skip-dirs+
  '(".git" ".github" ".archive" ".hub" ".curator_backups" ".locks" ".venv"
    "venv" "node_modules" "site-packages" "__pycache__" ".pytest_cache")
  "Directories a walk never enters, as the original skips them.")

(defparameter +front-keys+ '("name" "description" "triggers" "tags")
  "The frontmatter members this cell indexes; the rest of the frontmatter is not read.")

(defun trim-line (line)
  (string-trim '(#\Space #\Tab #\Return) line))

(defun split-lines (text)
  "TEXT as its lines, without their newlines."
  (loop with start = 0
        for pos = (position #\Newline text :start start)
        collect (subseq text start (if pos pos (length text)))
        while pos
        do (setf start (1+ pos))))

(defun split-on (text separator)
  "TEXT split at SEPARATOR, each part trimmed."
  (loop with start = 0
        for pos = (position separator text :start start)
        collect (trim-line (subseq text start (if pos pos (length text))))
        while pos
        do (setf start (1+ pos))))

(defun unquote (text)
  (let ((text (trim-line text)))
    (if (and (>= (length text) 2)
             (member (char text 0) '(#\" #\'))
             (char= (char text 0) (char text (1- (length text)))))
        (subseq text 1 (1- (length text)))
        text)))

(defun one-line (text)
  "TEXT with every run of whitespace one space, trimmed."
  (with-output-to-string (out)
    (loop with pending = nil
          for char across (if (stringp text) text (princ-to-string (or text "")))
          do (if (member char '(#\Space #\Tab #\Newline #\Return))
                 (setf pending t)
                 (progn (when pending (write-char #\Space out))
                        (setf pending nil)
                        (write-char char out))))))

(defun fence-line-p (line)
  (string= (string-left-trim (list (code-char #xFEFF) #\Space #\Tab) line) "---"))

(defun frontmatter (text)
  "TEXT's frontmatter as a plist of :NAME :DESCRIPTION :TRIGGERS :TAGS, each a
string as the file wrote it, or NIL when TEXT does not open on a --- fence."
  (let ((lines (split-lines text)))
    (when (and lines (fence-line-p (first lines)))
      (let ((fields '())
            (key nil))
        (dolist (line (rest lines) fields)
          (let ((trimmed (trim-line line)))
            (cond ((fence-line-p line) (return fields))
                  ((zerop (length trimmed)) nil)
                  ((member (char line 0) '(#\Space #\Tab))
                   ;; a folded continuation, or a block-list item, of the last key
                   (when key
                     (let ((part (if (and (>= (length trimmed) 2)
                                          (string= (subseq trimmed 0 2) "- "))
                                     (subseq trimmed 2)
                                     trimmed)))
                       (setf (getf fields key)
                             (format nil "~@[~a ~]~a" (getf fields key) (trim-line part))))))
                  (t (let ((colon (position #\: trimmed)))
                       (when colon
                         (let* ((name (string-downcase (trim-line (subseq trimmed 0 colon))))
                                (value (trim-line (subseq trimmed (1+ colon))))
                                (member (and (member name +front-keys+ :test #'string=)
                                             (intern (string-upcase name) :keyword))))
                           (setf key member)
                           (when member
                             (setf (getf fields member) (unquote value))))))))))))))

(defun list-value (text)
  "A frontmatter list as a list of strings: `[a, b]', `a, b', or a block list
joined by the reader above."
  (when (and (stringp text) (plusp (length text)))
    (let* ((text (trim-line text))
           (inner (if (and (char= (char text 0) #\[)
                           (char= (char text (1- (length text))) #\]))
                      (subseq text 1 (1- (length text)))
                      text)))
      (loop for item in (split-on inner #\,)
            for cleaned = (unquote (string-left-trim "- " item))
            when (plusp (length cleaned)) collect cleaned))))

(defun body-lines (text)
  "TEXT's lines after its frontmatter fence, or all of them."
  (let ((lines (split-lines text)))
    (if (and lines (fence-line-p (first lines)))
        (let ((rest (rest lines)))
          (loop for line in rest
                for at from 0
                when (fence-line-p line)
                  return (nthcdr (1+ at) rest)
                finally (return rest)))
        lines)))

(defstruct (skill (:conc-name skill-))
  "One SKILL.md: its id (its folder below its root), name and description from
frontmatter, its triggers and tags, and the file that holds it."
  id name description triggers tags path)

(defun read-skill (path id)
  "PATH read as a SKILL, or NIL when it holds neither a name nor a description."
  (let* ((text (or (ignore-errors (uiop:read-file-string path)) ""))
         (fields (frontmatter text))
         (name (one-line (or (getf fields :name) id)))
         (description (one-line (or (getf fields :description)
                                    (find-if (lambda (line) (plusp (length (trim-line line))))
                                             (body-lines text))
                                    ""))))
    (when (or (plusp (length name)) (plusp (length description)))
      (make-skill :id (if (plusp (length id)) id (file-namestring path))
                  :name (if (plusp (length name)) name id)
                  :description description
                  :triggers (list-value (getf fields :triggers))
                  :tags (list-value (getf fields :tags))
                  :path (namestring path)))))

;;; --- the walk ------------------------------------------------------------------

(defun expand-home (path)
  (let ((text (and path (princ-to-string path))))
    (if (and text (plusp (length text)) (char= #\~ (char text 0)))
        (merge-pathnames (if (> (length text) 1) (subseq text 1) "")
                         (user-homedir-pathname))
        text)))

(defun skill-files (root)
  "Every (PATH . ID) under ROOT that is a SKILL.md, ID its folder below ROOT;
symlink cycles and the skipped directories are never entered."
  (let ((out '())
        (seen (make-hash-table :test #'equal)))
    (labels ((walk (directory id depth)
               (when (> depth 12) (return-from walk))
               (let ((key (ignore-errors (namestring (uiop:truename* directory)))))
                 (when (and key (gethash key seen)) (return-from walk))
                 (when key (setf (gethash key seen) t)))
               (dolist (file (ignore-errors (uiop:directory-files directory)))
                 (when (string-equal (file-namestring file) "SKILL.md")
                   (push (cons file id) out)))
               (dolist (sub (ignore-errors (uiop:subdirectories directory)))
                 (let ((name (car (last (pathname-directory sub)))))
                   (when (and (stringp name) (plusp (length name))
                              (not (char= #\. (char name 0)))
                              (not (member name +skip-dirs+ :test #'string-equal)))
                     (walk sub (if (plusp (length id)) (format nil "~a/~a" id name) name)
                           (1+ depth)))))))
    (walk (uiop:ensure-directory-pathname root) "" 0)
    (sort (nreverse out)
          (lambda (a b) (string< (cdr a) (cdr b)))))))

(defun roots ()
  "The section's roots as directory pathnames; refuses while the cell is not running."
  (mapcar (lambda (root) (uiop:ensure-directory-pathname (expand-home root)))
          (getf (running-settings) :roots)))

(defun manifest (directories)
  "A signature of every SKILL.md under DIRECTORIES: the index is rebuilt when it moves."
  (loop for root in directories
        append (loop for (path . id) in (skill-files root)
                     collect (list id (namestring path)
                                   (or (ignore-errors (file-write-date path)) 0)
                                   (or (ignore-errors
                                        (with-open-file (in path :element-type '(unsigned-byte 8))
                                          (file-length in)))
                                       0)))))

;;; --- BM25 -----------------------------------------------------------------------

(defparameter +k1+ 1.5)
(defparameter +b+ 0.75)

(defun tokenize (text)
  "TEXT's terms: lowercased, everything that is not a letter or a digit a break."
  (let ((out '())
        (word (make-string-output-stream)))
    (labels ((flush ()
               (let ((text (get-output-stream-string word)))
                 (when (plusp (length text)) (push text out)))))
      (loop for char across (if (stringp text) text (princ-to-string (or text "")))
            do (if (or (alpha-char-p char) (digit-char-p char))
                   (write-char (char-downcase char) word)
                   (flush)))
      (flush))
    (nreverse out)))

(defstruct (bm25 (:conc-name idx-))
  "One built index: SKILLS a vector, POSTINGS term -> ((DOC-INDEX . WEIGHT) ...)."
  skills postings count avgdl)

(defun document-text (skill triggers)
  (format nil "~a: ~a~@[ triggers: ~{~a~^ ~}~]~@[ tags: ~{~a~^ ~}~]"
          (skill-name skill)
          (skill-description skill)
          (and triggers (skill-triggers skill))
          (and triggers (skill-tags skill))))

(defun distinct-terms (tokens)
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (term tokens) (setf (gethash term seen) t))
    (loop for term being the hash-keys of seen collect term)))

(defun build-index (skills &key triggers)
  "SKILLS as a BM25 index over `name: description triggers: ... tags: ...',
with Lucene's IDF, so a term in every document still scores."
  (let* ((docs (coerce skills 'vector))
         (count (length docs))
         (tokenized (map 'vector (lambda (skill)
                                   (tokenize (document-text skill triggers)))
                         docs))
         (lengths (map 'vector #'length tokenized))
         (avgdl (if (plusp count)
                    (/ (float (max 1 (reduce #'+ lengths :initial-value 0))) count)
                    1.0))
         (df (make-hash-table :test #'equal))
         (postings (make-hash-table :test #'equal)))
    (loop for tokens across tokenized
          do (dolist (term (distinct-terms tokens)) (incf (gethash term df 0))))
    (loop for i from 0 below count
          for tokens = (aref tokenized i)
          for dl = (aref lengths i)
          when (plusp dl)
            do (let ((counts (make-hash-table :test #'equal))
                     (denominator (* +k1+ (+ (- 1.0 +b+) (* +b+ (/ (float dl) avgdl))))))
                 (dolist (term tokens) (incf (gethash term counts 0)))
                 (loop for term being the hash-keys of counts using (hash-value tf)
                       for document-frequency = (gethash term df 1)
                       for idf = (log (+ 1.0 (/ (+ (- count document-frequency) 0.5)
                                                (+ document-frequency 0.5))))
                       for weight = (* (/ (* tf (+ +k1+ 1.0)) (+ tf denominator)) idf)
                       do (push (cons i weight) (gethash term postings)))))
    (make-bm25 :skills docs :postings postings :count count :avgdl avgdl)))

(defun retrieve (query index top-k)
  "The best TOP-K (SKILL . SCORE) of INDEX for QUERY, best first, ties in
corpus order; a candidate that shares no term with QUERY is left out."
  (when (and index (stringp query) (plusp (length query)) (plusp (idx-count index)))
    (let ((scores (make-hash-table :test #'eql))
          (seen (make-hash-table :test #'equal)))
      (dolist (term (tokenize query))
        (unless (gethash term seen)
          (setf (gethash term seen) t)
          (dolist (posting (gethash term (idx-postings index)))
            (incf (gethash (car posting) scores 0) (cdr posting)))))
      (let ((ranked (sort (loop for i being the hash-keys of scores using (hash-value score)
                                when (plusp score) collect (cons i score))
                          (lambda (a b)
                            (or (> (cdr a) (cdr b))
                                (and (= (cdr a) (cdr b)) (< (car a) (car b))))))))
        (loop for (i . score) in ranked
              repeat (max 1 (or top-k 1))
              collect (cons (aref (idx-skills index) i) score))))))

;;; --- the running index ----------------------------------------------------------

(defvar *index-cache* nil
  "(MANIFEST . INDEX) of the last build; the manifest is what invalidates it.")

(defvar *injected* (make-hash-table :test #'equal)
  "Turn id -> the input its injection was built from, so a turn injects once.")

(defun current-index ()
  "The index over the section's roots, rebuilt only when a SKILL.md moved."
  (let* ((directories (roots))
         (signature (manifest directories)))
    (unless (and *index-cache* (equal (car *index-cache*) signature))
      (setf *index-cache*
            (cons signature
                  (build-index (loop for (id path) in signature
                                     for skill = (read-skill path id)
                                     when skill collect skill)
                               :triggers (setting :triggers)))))
    (cdr *index-cache*)))

(defun indexed-count ()
  (let ((index (current-index)))
    (if index (idx-count index) 0)))

;;; --- what a reader sees ---------------------------------------------------------

(defun clip (text limit)
  (let ((text (or text "")))
    (if (> (length text) limit)
        (concatenate 'string (subseq text 0 (max 0 (- limit 3))) "...")
        text)))

(defun described-lines (matches)
  "MATCHES as one line each: name, id, description."
  (with-output-to-string (out)
    (dolist (match matches)
      (let ((skill (car match)))
        (format out "- **~a** (~a): ~a~%"
                (skill-name skill) (skill-id skill)
                (clip (skill-description skill) 200))))))

(defun search-skills (query &key top-k)
  "The index's best skills for QUERY, as the model reads them."
  (let* ((index (current-index))
         (total (if index (idx-count index) 0))
         (matches (retrieve query index (or top-k (setting :top-k)))))
    (cond ((null matches)
           (format nil "~d skills indexed; ~:[no SKILL.md is under the roots~;none shares a word with ~s~]."
                   total (plusp total) query))
          (t
           (format nil "~d skills indexed; best ~d for ~s (BM25):~%~aRead the SKILL.md a line names when it fits: the id is its folder below a root."
                   total (length matches) query (described-lines matches))))))

;;; --- the round: the turn's own input, once ---------------------------------------

(defparameter +injection-head+
  "## Retrieved skills (BM25, best matches for this request)
Read the SKILL.md a line names when it fits: it holds the steps. /skill-search WORDS asks again."
  "What the round's block opens on.")

(defun turn-input (turn)
  "The words TURN was admitted with, or NIL."
  (nlk:json-value (nlk:events :turn-id (nlk:durable-turn-turn-id turn)
                               :kind nlk::+kind-turn-started+ :as :payload)
                  :string "input"))

(defun injection-block (input)
  "The block a turn admitted with INPUT carries, or NIL when nothing matches."
  (let* ((index (current-index))
         (matches (retrieve input index (setting :top-k))))
    (when matches
      (format nil "~a~%~a" +injection-head+ (described-lines matches)))))

(defun on-round (op next)
  "The :round point: a turn about to ask a model reads the skills its input
matches, injected into this very request, once per turn; NEXT runs the rest
of the chain."
  (let* ((session (getf op :session-id))
         (turn (getf op :turn))
         (input (and session turn (nlk:store-open-p) (setting :roots) (turn-input turn))))
    (when (and input (plusp (length (nlk:trimmed input))) (setting :inject))
      (let ((id (nlk:durable-turn-turn-id turn)))
        (unless (equal (gethash id *injected*) input)
          (when (> (hash-table-count *injected*) 500) (clrhash *injected*))
          (setf (gethash id *injected*) input)
          (nlk:when-let (block (handler-case (injection-block input)
                                 (skill-retrieval-error () nil)
                                 (error (condition)
                                   (warn "skill retrieval: ~a" condition)
                                   nil)))
            (nlk:record-turn-context-prompt turn :text block)))))
    (funcall next op)))

;;; --- the model's verb and the operator's -----------------------------------------

(nlk:define-contract skill-search
  (:doc "Rank a library of SKILL.md skills by BM25 against a few words and answer the best matches, each with its description and the id whose file holds the steps. Use it before a task whose procedure may already be written down.")
  ("query" :text :required t
           :doc "what the task is about, in a few words")
  ("top_k" :integer
           :doc "how many skills to answer with; the section's top_k when left out"))

(defun skill-search-tool (call)
  (handler-case (search-skills (skill-search-query call) :top-k (skill-search-top-k call))
    (skill-retrieval-error (condition)
      (nle:failure (format nil "ERROR: SKILL-RETRIEVAL-ERROR: ~a" condition)))
    (error (condition)
      (nle:failure (format nil "ERROR: SKILL-RETRIEVAL-ERROR: ~a" condition)))))

(defun search-command (args session-id)
  "One /skill-search invocation: WORDS ranked, or what the library holds."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (if (plusp (length text))
        (search-skills text)
        (format nil "~d skills indexed from ~d root~:p; /skill-search WORDS ranks them, and every turn injects the best ~d~:[~; of its own accord~].~%Roots: ~{~a~^, ~}"
                (indexed-count) (length (setting :roots)) (setting :top-k)
                (setting :inject)
                (mapcar #'expand-home (setting :roots))))))

;;; --- start ---------------------------------------------------------------------

(defun start ()
  "The section must name a root; everything else comes from its defaults."
  (unless (setting :roots)
    (error 'nlk:config-refusal
           :detail "skill-retrieval: roots is required: name at least one folder that holds SKILL.md files"))
  (setf *index-cache* nil)
  (clrhash *injected*)
  (nle:on-stop (lambda () (setf *index-cache* nil) (clrhash *injected*))))

(nle:define-cell skill-retrieval
  (:section ("skill-retrieval")
    (:guide "roots lists the folders that hold SKILL.md files (an imported skill library, for one); top_k caps one answer and one turn's injection; inject says whether a turn carries the matches at all; triggers indexes each skill's trigger phrases and tags beside its description")
    ("roots" :list :required t
             :doc "folders scanned for SKILL.md files, recursively")
    ("top_k" :integer :default 6 :min 1
             :doc "how many skills one search or one turn carries")
    ("inject" :boolean :default t
              :doc "inject the turn's best matches into its own request")
    ("triggers" :boolean :default t
                :doc "index trigger phrases and tags beside the name and description"))
  (:tool "skill_search" (nlk:contract 'skill-search) 'skill-search-tool)
  (:command "skill-search" 'search-command
            :description "Find skills by a few words"
            :argument-hint "WORDS")
  (:help :skill-retrieval
         "skill retrieval: BM25 over a SKILL.md library, injected per turn"
         +primer+)
  (:hook :round #'on-round)
  (:start #'start))
