;;;; cell.lisp --- a flat keyword index in front of the model, verbose topics on disk.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A rewrite of hermes-dynamic-memory's memory-shaper plugin. What the model
;;;; keeps is one line per subject -- "keyword,keyword,keyword -> file.md",
;;;; five to twenty comma-separated keywords and a Unicode arrow -- and the
;;;; sentences live in that file, read only when a keyword matches. The small
;;;; index rides every turn's first request; the detail costs nothing until it
;;;; is wanted.
;;;;
;;;; The original enforced this by intercepting another harness's memory tool
;;;; and every file write that aimed at its index. Here the index is the cell's
;;;; own: the memory_index tool validates one line and appends it, routes a
;;;; paragraph into a topic file, and lints what is there -- in the original's
;;;; words, against the same rules (Unicode arrow only, 5-20 keywords, a bare
;;;; .md target, prose refused by its comma-to-word ratio, a long line sent to
;;;; a topic file).

(defpackage #:nodecode-memory-shaper
  (:use #:cl))

(in-package #:nodecode-memory-shaper)

(nlk:define-peripheral memory-shaper :not-running t)

;;; --- the format, as data ----------------------------------------------------------

(defparameter *arrow* (string (code-char 8594))
  "The one arrow an index line may carry. The ASCII pair is refused with its
own message, because it is what someone types first.")

(defparameter *ascii-arrow* "->"
  "What the original refuses everywhere, so the model is told which arrow to use.")

(defparameter *entry-shape* (format nil "keyword,keyword,keyword ~a file.md" *arrow*)
  "The one shape an entry has, repeated by every refusal.")

(defparameter *index-marker*
  (format nil "[Keyword index ~a read the topic file when a line matches]" (string (code-char 8212)))
  "The line the resident index opens on, so the model reads the entries as a
lookup table rather than as conversation.")

(defparameter +actions+ '("add" "route" "lint" "show" "rewrite")
  "What the memory_index tool does, and the enum its schema advertises.")

(defparameter +primer+
  (format nil "~a

An index entry is one line: five to twenty comma-separated keywords, a space,
the Unicode arrow, a space, and a bare .md file name inside the topics folder:

  python,packaging,poetry,ruff,pytest,typing,imports ~a python.md

The memory_index tool is the way in. Add one validated line; route a paragraph
to its topic file and then add the pointer; lint what the index holds; show it;
rewrite the whole file (allowed only when the section says so). A malformed line
is refused with what to fix -- a wrong arrow, a keyword count outside the
section's range, a path where a bare file name belongs, or a sentence where a
keyword list belongs.

The index rides every turn's first request, so keep it short: anything past the
section's verbose_chars belongs in its topic file. The operator types
/memory-shaper to check the index, or /memory-shaper add <line>."
  *entry-shape* *arrow*)
  "What (help :memory-shaper) answers while the cell runs.")

;;; --- where the index and its topics live -------------------------------------------

(defun index-configured-p ()
  "T when the section says which file the index is."
  (let ((name (ignore-errors (setting :index-file))))
    (and (stringp name) (plusp (length (nlk:trimmed name))))))

(defun index-pathname ()
  "The index file the section names, or a refusal saying which setting to fill."
  (let ((name (and (index-configured-p) (nlk:trimmed (setting :index-file)))))
    (unless name
      (fail "memory-shaper.index_file is not set: name the keyword index file, e.g. memory/index.md under the home, in /settings (or the memory-shaper section of config.jsonc)"))
    (if (or (uiop:string-prefix-p "/" name) (uiop:string-prefix-p "~/" name))
        (nlk:expand-home name)
        (nlk:home name))))

(defun topics-dir ()
  "Where the topic files live: the section's topics_dir, else the index's own folder."
  (let ((name (nlk:trimmed (or (ignore-errors (setting :topics-dir)) ""))))
    (if (plusp (length name))
        (if (or (uiop:string-prefix-p "/" name) (uiop:string-prefix-p "~/" name))
            (nlk:expand-home name)
            (nlk:home name))
        (uiop:pathname-directory-pathname (index-pathname)))))

(defun topic-pathname (name)
  "The topic file NAME inside the topics folder."
  (merge-pathnames name (topics-dir)))

;;; --- one line, read back -------------------------------------------------------------

(defun flatten (text)
  "TEXT with every run of white space as one space, trimmed: an entry is one
line, so neither a newline nor a tab can split it."
  (let ((out (make-string-output-stream))
        (space nil)
        (written nil))
    (loop for character across (princ-to-string (or text ""))
          do (if (member character '(#\Space #\Tab #\Newline #\Return #\Page))
                 (setf space written)
                 (progn (when space (write-char #\Space out))
                        (setf space nil)
                        (setf written t)
                        (write-char character out))))
    (nlk:trimmed (get-output-stream-string out))))

(defun arrow-line-parts (line)
  "VALUES: the keywords and the target either side of LINE's first arrow, or
NIL when it carries none."
  (let ((at (search *arrow* line)))
    (when at
      (values (nlk:trimmed (subseq line 0 at))
              (nlk:trimmed (subseq line (+ at (length *arrow*))))))))

(defun keyword-list (text)
  "TEXT's comma-separated keywords, trimmed, empty ones dropped."
  (remove-if (lambda (keyword) (zerop (length keyword)))
             (mapcar #'nlk:trimmed (uiop:split-string text :separator ","))))

(defun word-count (text)
  "How many white-space separated words TEXT holds."
  (length (remove-if #'zerop
                      (mapcar #'length
                              (uiop:split-string text
                                                  :separator '(#\Space #\Tab #\Newline #\Return))))))

(defun comma-word-ratio (text)
  "TEXT's commas over its words, the original's test for prose: a keyword list
is commas-heavy, a sentence is not. One when there are no words."
  (let ((words (word-count text)))
    (if (zerop words) 1.0 (/ (count #\, text) words))))

;;; --- what an entry must be -----------------------------------------------------------

(defun ends-with-md-p (target)
  "T when TARGET names a .md file."
  (let ((n (length target)))
    (and (>= n 4) (string-equal ".md" (subseq target (- n 3))))))

(defun relative-target-p (target)
  "T when TARGET is a bare .md name, or a ../ path to one, and nothing else."
  (let* ((parts (uiop:split-string target :separator "/"))
         (last (car (last parts))))
    (and (plusp (length last))
         (ends-with-md-p last)
         (not (string-equal ".md" last))
         (every (lambda (part) (or (string= part "..") (plusp (length part))))
                (butlast parts)))))

(defun setting-keywords (name)
  "The section's keyword bound NAME, or NIL before the cell runs."
  (ignore-errors (setting name)))

(defun keyword-problem (text)
  "What is wrong with TEXT as an entry's keyword list, or NIL when nothing is."
  (let ((list (keyword-list text))
        (least (or (setting-keywords :min-keywords) 5))
        (most (or (setting-keywords :max-keywords) 20)))
    (cond ((null list)
           (format nil "[memory-shaper] the entry carries no keywords: ~a" *entry-shape*))
          ((< (length list) least)
           (format nil "[memory-shaper] ~d keyword~:p is too few: an entry carries ~d to ~d comma-separated keywords, as in ~a"
                   (length list) least most *entry-shape*))
          ((> (length list) most)
           (format nil "[memory-shaper] ~d keywords is too many: an entry carries ~d to ~d, as in ~a"
                   (length list) least most *entry-shape*))
          (t (let ((spaced (find-if (lambda (keyword) (find #\Space keyword)) list)))
               (when spaced
                 (format nil "[memory-shaper] a keyword carries no space: ~s is two words, and the words around it are separated by commas"
                         spaced)))))))

(defun target-problem (target)
  "What is wrong with TARGET as the file an entry points at, or NIL."
  (let ((folder (ignore-errors (namestring (topics-dir)))))
    (cond ((zerop (length target))
           (format nil "[memory-shaper] the entry names no file: ~a" *entry-shape*))
          ((member (char target 0) '(#\/ #\~))
           (format nil "[memory-shaper] ~s is an absolute path: an entry names a bare .md file inside ~a"
                   target folder))
          ((not (ends-with-md-p target))
           (format nil "[memory-shaper] the target ends in .md: ~s does not" target))
          ((find #\Space target)
           (format nil "[memory-shaper] the target carries no space: ~s" target))
          ((not (relative-target-p target))
           (format nil "[memory-shaper] ~s is no topic file name: one bare .md name inside ~a (or a ../ path to one), no folder prefix"
                   target folder)))))

(defun topic-slug (text)
  "The first few meaningful words of TEXT as a file name stem."
  (let* ((stop '("the" "a" "an" "is" "of" "and" "or" "to" "in" "on" "for"
                 "with" "this" "that" "see" "it" "as" "at" "by" "be"))
         (words (loop for word in (uiop:split-string (string-downcase (or text ""))
                                                     :separator '(#\Space #\Tab #\Newline #\Return
                                                                  #\, #\: #\; #\. #\( #\)))
                      for cleaned = (remove-if-not (lambda (character)
                                                     (or (alphanumericp character)
                                                         (char= character #\-)))
                                                   word)
                      when (and (plusp (length cleaned))
                                (not (member cleaned stop :test #'string=)))
                        collect cleaned))
         (kept (subseq words 0 (min 4 (length words))))
         (slug (nlk:trimmed (format nil "~{~a~^-~}" kept))))
    (cond ((zerop (length slug)) "note")
          ((> (length slug) 60) (subseq slug 0 60))
          (t slug))))

(defun route-message (line)
  "The refusal a too-long line gets: where its detail goes, and the pointer to
add afterwards."
  (let* ((name (or (multiple-value-bind (kws target) (arrow-line-parts line)
                      (declare (ignore kws))
                      (and target (plusp (length target)) (not (find #\/ target)) target))
                    (format nil "~a.md" (topic-slug line))))
         (path (namestring (topic-pathname name))))
    (format nil "[memory-shaper] too long for the index: ~d characters, and an entry carries at most ~d.~%Write the detail to ~a (memory_index action=route topic=~a text=...), then add one line: kw1,kw2,kw3,kw4,kw5 ~a ~a"
            (length line) (or (setting-keywords :verbose-chars) 140) path name *arrow* name)))

(defun wrong-arrow-message ()
  (format nil "[memory-shaper] wrong arrow: an entry carries the Unicode arrow ~a only, one space on each side:~%  ~a~%You wrote '->'."
          *arrow* *entry-shape*))

(defun no-arrow-message (line)
  "The refusal a line without an arrow gets, prose and shape told apart."
  (let ((ratio (comma-word-ratio line))
        (least 0.5))
    (if (< ratio least)
        (format nil "[memory-shaper] that reads as a sentence, not a keyword pointer: ~,2f commas per word, and a keyword list holds at least ~,1f.~%Write it to a topic file (memory_index action=route), then add one line: ~a"
                ratio least *entry-shape*)
        (format nil "[memory-shaper] that entry carries no arrow: one line per topic file, ~a"
                *entry-shape*))))

(defun index-line-problem (line)
  "The fix message LINE gets as an index entry, or NIL when it is well formed."
  (let ((line (flatten line))
        (most (or (setting-keywords :verbose-chars) 140)))
    (cond ((zerop (length line))
           (format nil "[memory-shaper] an empty line is no entry: every entry is one line, ~a"
                   *entry-shape*))
          ((> (length line) most) (route-message line))
          ((and (search *ascii-arrow* line) (not (search *arrow* line))) (wrong-arrow-message))
          ((not (search *arrow* line)) (no-arrow-message line))
          (t (multiple-value-bind (kws target) (arrow-line-parts line)
               (or (keyword-problem kws) (target-problem target)))))))

;;; --- the file on disk ----------------------------------------------------------------

(defun index-lines ()
  "Every line of the index file, or NIL when it is not there yet."
  (let ((path (ignore-errors (index-pathname))))
    (if (or (null path) (not (probe-file path)))
        '()
        (remove-if (lambda (line) (zerop (length line)))
                   (mapcar (lambda (line) (string-trim '(#\Space #\Tab #\Return) line))
                           (uiop:split-string (or (nlk:read-text path) "") :separator '(#\Newline)))))))

(defun ignored-line-p (line)
  "T for a line that is no entry and no error: a # header, a blank, a separator."
  (let ((line (nlk:trimmed line)))
    (or (zerop (length line))
        (char= (char line 0) #\#)
        (every (lambda (character)
                 (member character (list (code-char 167) (code-char 9552) #\=)))
               (coerce line 'list)))))

(defun entry-lines ()
  "The index's entries: every line that is meant to be one."
  (remove-if #'ignored-line-p (index-lines)))

(defun write-index (lines)
  "LINES as the whole index file: written beside it, then renamed over it."
  (let* ((path (index-pathname))
         (tmp (uiop:parse-native-namestring (concatenate 'string (namestring path) ".tmp"))))
    (ensure-directories-exist path)
    (with-open-file (out tmp :direction :output :if-exists :supersede
                             :external-format :utf-8)
      (dolist (line lines)
        (write-line line out)))
    (nlk:replace-file tmp path)))

;;; --- the verbs ---------------------------------------------------------------------

(defun add-entry (line)
  "Validate LINE as one entry and append it to the index."
  (running-settings)
  (let* ((line (flatten line))
         (problem (index-line-problem line)))
    (when problem
      (fail problem))
    (let ((existing (index-lines)))
      (when (member line existing :test #'string=)
        (return-from add-entry (format nil "Already in the index: ~a" line)))
      (write-index (append existing (list line)))
      (multiple-value-bind (kws target) (arrow-line-parts line)
        (declare (ignore target))
        (format nil "Added (~d keywords): ~a~%It rides every turn's first request; the detail stays in its topic file."
                (length (keyword-list kws)) line)))))

(defun lint-report ()
  "Every malformed entry with what to do about it, or the one line saying the
index is all well formed."
  (running-settings)
  (let* ((path (index-pathname))
         (entries (entry-lines))
         (problems (loop for line in entries
                         for problem = (index-line-problem line)
                         when problem collect (cons line problem))))
    (cond ((null entries)
           (format nil "No entries yet in ~a: add one with the memory_index tool, or /memory-shaper add <line>."
                   (namestring path)))
          ((null problems)
           (format nil "~d entr~:@p in ~a, every one well formed."
                   (length entries) (namestring path)))
          (t (with-output-to-string (out)
               (format out "~d of ~d entr~:@p in ~a need a fix:~%"
                       (length problems) (length entries) (namestring path))
               (dolist (pair problems)
                 (format out "  ~a~%    ~a~%" (car pair) (cdr pair))))))))

(defun index-show ()
  "The index as it stands, with where it is."
  (running-settings)
  (let ((lines (index-lines)))
    (if (null lines)
        (format nil "The index at ~a is empty: add the first entry with the memory_index tool."
                (namestring (index-pathname)))
        (with-output-to-string (out)
          (format out "~a~%~{~a~^~%}~}~%" (namestring (index-pathname)) lines)))))

(defun route-topic (topic text)
  "Write TEXT to the topic file TOPIC (default: a slug of TEXT's words) and
answer the pointer line to add afterwards."
  (running-settings)
  (index-pathname)
  (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return) (or text ""))))
    (when (zerop (length text))
      (fail "routing needs text: the detail the index should not carry"))
    (let* ((given (nlk:trimmed (or topic "")))
           (name (cond ((zerop (length given)) (format nil "~a.md" (topic-slug text)))
                       ((ends-with-md-p given) given)
                       (t (format nil "~a.md" given)))))
      (when (find #\/ name)
        (fail (format nil "a topic file is one bare name inside ~a, no folder: ~a"
                       (namestring (topics-dir)) name)))
      (let ((path (topic-pathname name)))
        (ensure-directories-exist path)
        (with-open-file (out path :direction :output :if-exists :supersede
                                 :external-format :utf-8)
          (write-string text out)
          (terpri out))
        (format nil "Wrote ~d characters to ~a.~%Now add the pointer the index carries: kw1,kw2,kw3,kw4,kw5 ~a ~a (memory_index action=add line=...)."
                (length text) (namestring path) *arrow* name)))))

(defun rewrite-index (text)
  "TEXT as the whole index, one entry a line, when the section allows it."
  (running-settings)
  (index-pathname)
  (unless (setting :allow-rewrite)
    (fail "rewriting the whole index needs approval: set memory-shaper.allow_rewrite to true in /settings, then retry"))
  (let* ((lines (remove-if (lambda (line) (zerop (length line)))
                            (mapcar #'flatten
                                    (mapcar (lambda (line)
                                              (string-trim '(#\Space #\Tab #\Return) line))
                                            (uiop:split-string (or text "") :separator '(#\Newline))))))
         (problems (loop for line in (remove-if #'ignored-line-p lines)
                         for problem = (index-line-problem line)
                         when problem collect (format nil "  ~a~%    ~a" line problem))))
    (when problems
      (fail (format nil "the rewrite is refused, nothing written: ~{~a~^~%}~}" problems)))
    (write-index lines)
    (format nil "Rewrote ~a: ~d line~:p." (namestring (index-pathname)) (length lines))))

(defun status-text ()
  "The index, linted: what the operator reads when /memory-shaper is typed alone."
  (running-settings)
  (let ((path (index-pathname)))
    (format nil "~a~%~a" (namestring path) (lint-report))))

;;; --- the model's way in ---------------------------------------------------------------

(nlk:define-contract memory-index-call
  (:doc "Keep the keyword index the way it is read: one line per subject, five to twenty comma-separated keywords and the Unicode arrow pointing at a bare .md topic file. add appends one validated entry; route writes a paragraph to its topic file and answers the pointer line; lint reports every malformed entry; show reads the index; rewrite replaces the whole file. A malformed line is refused with what to fix, and the detail belongs in the topic file, not the index.")
  ("action" :text :default "add" :options +actions+
            :doc "add appends one validated entry; route writes text to a topic file; lint reports every malformed entry; show reads the index; rewrite replaces the whole file")
  ("line" :text :doc "the entry for add, one line: kw1,kw2,kw3,kw4,kw5 → file.md, with the Unicode arrow")
  ("topic" :text :doc "route's file name, bare and ending in .md; default: a name taken from the text's first words")
  ("text" :text :doc "route's paragraph, or every line of a rewrite"))

(defun memory-tool (call)
  "The memory_index tool: the action CALL names, against the index file."
  (let ((action (string-downcase (or (memory-index-call-action call) "add"))))
    (cond ((string= action "add") (add-entry (memory-index-call-line call)))
          ((string= action "route") (route-topic (memory-index-call-topic call)
                                                  (memory-index-call-text call)))
          ((string= action "lint") (lint-report))
          ((string= action "show") (index-show))
          ((string= action "rewrite") (rewrite-index (memory-index-call-text call)))
          (t (fail "~s is no memory_index action: add, route, lint, show or rewrite" action)))))

;;; --- the operator's way in -------------------------------------------------------------

(defun run-slash (args session-id)
  "One /memory-shaper invocation: nothing lints the index, 'add LINE' appends,
'show' reads it, 'rewrite' takes the rest as the whole file."
  (declare (ignore session-id))
  (let* ((args (nlk:trimmed (or args "")))
         (lower (string-downcase args)))
    (cond ((zerop (length args)) (status-text))
          ((uiop:string-prefix-p "show" lower) (index-show))
          ((uiop:string-prefix-p "rewrite " lower) (rewrite-index (subseq args 8)))
          ((uiop:string-prefix-p "add " lower) (add-entry (subseq args 4)))
          (t (add-entry args)))))

;;; --- every turn ---------------------------------------------------------------------

(defun index-block ()
  "The resident index: every entry, packed into the section's index_chars, or
NIL when the index holds none."
  (let ((lines '())
        (room (or (setting-keywords :index-chars) 800)))
    (dolist (line (entry-lines))
      (let ((line (flatten line)))
        (when (search *arrow* line)
          (when (and lines (> (+ 1 (length line)) room))
            (return))
          (push line lines)
          (decf room (1+ (length line))))))
    (when (null lines)
      (return-from index-block nil))
    (format nil "~a~%~{~a~^~%}~}" *index-marker* (nreverse lines))))

(defun deliver (op next)
  "The :round point: at a turn's first round the index goes into the request
about to be built, so the model knows what it can look up."
  (prog1 (funcall next op)
    (handler-case
        (let ((turn (getf op :turn))
              (round (getf op :round)))
          (when (and turn (nlk:store-open-p) (eql 0 (or round 0)) (index-configured-p))
            (let ((block (index-block)))
              (when block
                (nlk:record-turn-context-prompt turn :text block)))))
      (error (condition) (warn "memory-shaper: ~a" condition)))))

;;; --- the declaration ----------------------------------------------------------------

(nle:define-cell memory-shaper
  (:section ("memory-shaper")
    (:guide "index_file is the keyword index the model reads every turn (memory/index.md under the home is the usual answer); topics_dir is where the detail files go, beside the index when it is empty; - /cells shows whether the folder started")
    ("index_file" :path :required t
     :doc "the flat keyword index the model reads every turn; the memory_index tool and /memory-shaper refuse until it is set")
    ("topics_dir" :path
     :doc "where the verbose topic files live; empty: the folder the index is in")
    ("min_keywords" :integer :default 5 :min 1
     :doc "the fewest comma-separated keywords one entry may carry")
    ("max_keywords" :integer :default 20 :min 2
     :doc "the most comma-separated keywords one entry may carry")
    ("verbose_chars" :integer :default 140 :min 20
     :doc "the longest an index line may be before its detail belongs in a topic file")
    ("index_chars" :integer :default 800 :min 40
     :doc "how much of the index rides the first request of every turn")
    ("allow_rewrite" :boolean :default nil
     :doc "t when memory_index may replace the whole index file at once"))
  (:tool "memory_index" (nlk:contract 'memory-index-call) 'memory-tool)
  (:hook :round #'deliver)
  (:command "memory-shaper" 'run-slash
            :catches 'memory-shaper-error
            :argument-hint "[add LINE | show | rewrite LINE...]"
            :description "The keyword index: lint it, or add one entry")
  (:help :memory-shaper
         "memory-shaper: a flat keyword index in front of the model, the detail in topic files; /memory-shaper"
         +primer+))
