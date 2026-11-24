;;;; cell.lisp --- Digital Marketing Pro's skills, searched and then read.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original registers 207 skills with Hermes, which lists every name in
;;;; the prompt of every request. A Nodecode cell does the same job the other
;;;; way round: one manual line and two tools, so the library costs a line
;;;; until it is wanted, and the skill the conversation actually needs is read
;;;; whole (or one heading of it) at the moment it is needed.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "marketing": {"groups": ["strategy", "content", "social"],
;;;;                 "max_chars": 6000}
;;;; Nothing is required: skills/ ships with the cell. A vetoed section
;;;; ("enabled": false) installs nothing.
;;;;
;;;; Ported from indranilbanerjee's Hermes Agent plugin (see NOTICE): the
;;;; three groups and their 207 skills, the names the SKILL.md frontmatter
;;;; gives them, and the prose of every skill and of the guides beside it.
;;;; The Python the skills call is left out (see NOTICE).

(in-package #:nodecode-marketing)

(nlk:define-peripheral marketing :not-running t)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Digital Marketing Pro is a library of 207 marketing skills, available through the
nodecode-marketing cell: two tools, which a person can also call from eval in the
marketing package.
  marketing_skills  {query, group, limit}
      Finds the skills that answer a question, by words in a skill's name, its folder or
      its one-sentence description: \"technical seo\", \"ab test sample size\", \"linkedin
      carousel\", \"press release\", \"pricing\", \"churn\". Every word must appear. The three
      groups are strategy (positioning, SEO/AEO/GEO, paid media, CRM, analytics, pricing,
      launches), content (long-form articles, fact-checking, style guides, humanizing,
      editing) and social (a content calendar, captions, carousels, images and video).
      Search before answering any marketing question: a skill is usually a whole
      procedure, with the checklists, templates and numbers the answer needs.
  marketing_skill  {name, section, max_chars}
      Reads one skill whole, or one part of it: section takes words from a heading
      (\"quality gates\", \"output format\"), which is how a long skill is taken a step at a
      time. A read is clipped at max_chars (6000 by default) and a clipped answer lists
      the headings further down, so the next read can name one.
Work in the order the library is built for: search, read the skill that fits, then follow
it — a skill says what to produce and how to check it. Many strategy skills name a script
under their group's scripts/ folder (an A/B sample-size calculator, a forecast); this cell
carries the prose, not the Python, so either do that step by hand with the formula the
skill gives, or tell the person the script is what runs it. The person can also run the
original plugin's scripts if they have it installed.
Every request's help section carries one line naming this manual; (help :marketing) reads
it again."
  "What (help :marketing) answers while the cell runs.")

;;; --- the corpus ----------------------------------------------------------------

(defparameter +groups+ '("strategy" "content" "social")
  "The three groups, in the order the original lists them.")

(defstruct (skill (:constructor make-skill (name group folder description file label))
                  (:copier nil)
                  (:predicate skillp))
  name        ; the frontmatter's name, which is what a reader asks for
  group       ; "strategy", "content" or "social"
  folder      ; the skill's own folder name, the frontmatter's fallback
  description ; one sentence: what it does and when to use it
  file        ; the SKILL.md to read
  label)      ; file, as the folder tree names it: "strategy/ab-test-plan/SKILL.md"

(defvar *catalog* nil
  "Every skill, scanned once per start: the tree does not change under a
running cell, and scanning it reads 207 files.")

(defun skills-root ()
  "The tree to read: skills_dir when the section names one, else the one that
ships in this folder."
  (let ((set (getf (running-settings) :skills-dir)))
    (if (and set (plusp (length set)))
        (uiop:ensure-directory-pathname set)
        (asdf:system-relative-pathname "nodecode-marketing" "skills/"))))

(defun wanted-groups ()
  (let ((groups (getf (running-settings) :groups)))
    (if groups groups +groups+)))

(defun last-name (thing)
  "The last component of a pathname's directory, as a string."
  (let ((parts (pathname-directory (uiop:ensure-directory-pathname thing))))
    (or (and parts (car (last parts))) "")))

(defun read-text (file)
  "FILE whole, as UTF-8, or NIL when it is gone."
  (ignore-errors (uiop:read-file-string file :external-format :utf-8)))

(defun split-lines (text)
  (let ((out '()) (start 0))
    (loop for position = (position #\Newline text :start start)
          do (push (subseq text start (or position (length text))) out)
          while position
          do (setf start (1+ position)))
    (nreverse out)))

(defun bare-line (line)
  "LINE without its line ending or surrounding blanks."
  (string-trim '(#\Space #\Tab #\Return #\Newline) line))

(defun frontmatter (lines)
  "The lines of the leading --- block of LINES, or NIL."
  (when (and lines (string= (bare-line (first lines)) "---"))
    (loop for tail on (rest lines)
          when (string= (bare-line (first tail)) "---")
            return (ldiff (rest lines) tail))))

(defun body-of (lines)
  "LINES without its frontmatter block."
  (if (and lines (string= (bare-line (first lines)) "---"))
      (loop for tail on (rest lines)
            when (string= (bare-line (first tail)) "---")
              return (rest tail))
      lines))

(defun unquoted (value)
  "VALUE as a frontmatter string: the quotes off, \\\" and \\\\ put back."
  (let* ((end (if (and (> (length value) 1) (char= (char value (1- (length value))) #\"))
                  (1- (length value))
                  (length value)))
         (raw (subseq value 1 end))
         (out (make-string-output-stream)))
    (loop for index from 0 below (length raw)
          for character = (char raw index)
          do (cond ((and (char= character #\\) (< (1+ index) (length raw)))
                    (write-char (char raw (1+ index)) out)
                    (incf index))
                   (t (write-char character out))))
    (get-output-stream-string out)))

(defun folded (lines)
  "A YAML folded or literal block: its indented lines, joined by a space."
  (let ((out '()))
    (dolist (line lines)
      (unless (or (string= (bare-line line) "")
                  (member (char line 0) '(#\Space #\Tab)))
        (return))
      (let ((text (bare-line line)))
        (when (plusp (length text)) (push text out))))
    (format nil "~{~a~^ ~}" (nreverse out))))

(defun frontmatter-field (lines key)
  "KEY's value in the frontmatter LINES, or NIL."
  (let ((prefix (concatenate 'string key ":")))
    (loop for tail on lines
          for line = (first tail)
          when (and (>= (length line) (length prefix))
                    (string= prefix (subseq line 0 (length prefix))))
            do (return (let ((value (bare-line (subseq line (length prefix)))))
                         (cond ((member value '(">" "|" ">-" "|-" ">+" "|+") :test #'string=)
                                (folded (rest tail)))
                               ((and (plusp (length value)) (char= (char value 0) #\"))
                                (unquoted value))
                               (t value)))))))

(defun scan-group (root group)
  "Every skill under ROOT/GROUP, in folder order."
  (let ((directory (merge-pathnames (format nil "~a/" group) root))
        (out '()))
    (when (uiop:directory-exists-p directory)
      (dolist (folder (uiop:subdirectories directory) (nreverse out))
        (let ((file (merge-pathnames "SKILL.md" folder)))
          (when (probe-file file)
            (let* ((lines (split-lines (or (read-text file) "")))
                   (front (frontmatter lines))
                   (folder-name (last-name folder)))
              (push (make-skill (or (frontmatter-field front "name") folder-name)
                                 group folder-name
                                 (frontmatter-field front "description")
                                 file
                                 (format nil "~a/~a/SKILL.md" group folder-name))
                    out))))))))

(defun scan-skills ()
  (let ((root (skills-root)))
    (loop for group in (wanted-groups)
          nconc (scan-group root group))))

(defun catalog ()
  "Every skill this cell can read, scanned once and kept until it stops."
  (or *catalog* (setf *catalog* (scan-skills))))

;;; --- finding one ---------------------------------------------------------------

(defun query-words (query)
  (let ((text (string-downcase (or query "")))
        (out '())
        (start 0))
    (loop for index from 0 to (length text)
          for character = (and (< index (length text)) (char text index))
          do (cond ((or (null character) (member character '(#\Space #\Tab #\Newline #\,)))
                    (when (> index start) (push (subseq text start index) out))
                    (setf start (1+ index)))
                   (t nil)))
    (nreverse out)))

(defun skill-score (skill words)
  "How well SKILL matches WORDS, or NIL when one of them is nowhere in it."
  (let ((name (string-downcase (skill-name skill)))
        (folder (string-downcase (skill-folder skill)))
        (description (string-downcase (or (skill-description skill) "")))
        (group (skill-group skill))
        (score 0))
    (dolist (word words score)
      (cond ((search word name) (incf score 6))
            ((search word folder) (incf score 4))
            ((search word description) (incf score 2))
            ((search word group) (incf score 1))
            (t (return-from skill-score nil))))))

(defun find-skills (query &key group limit)
  "The skills matching QUERY, best first, at most LIMIT of them; the second
value is how many matched."
  (let* ((words (query-words query))
         (want (and group (plusp (length (nlk:trimmed group))) (string-downcase (nlk:trimmed group))))
         (scored '()))
    (dolist (skill (catalog))
      (when (or (null want) (string= want (skill-group skill)))
        (let ((score (skill-score skill words)))
          (when score (push (cons score skill) scored)))))
    (setf scored (stable-sort (nreverse scored) #'> :key #'car))
    (values (if (and limit (< limit (length scored)))
                (mapcar #'cdr (subseq scored 0 limit))
                (mapcar #'cdr scored))
            (length scored))))

(defun resolve-skill (name)
  "The one skill NAME means, or NIL: the frontmatter name, the folder, a
group-qualified name, or a unique part of one of those."
  (let* ((want (string-downcase (nlk:trimmed (or name ""))))
         (all (catalog)))
    (or (find want all :test #'string= :key (lambda (skill) (string-downcase (skill-name skill))))
        (find want all :test #'string= :key (lambda (skill) (string-downcase (skill-folder skill))))
        (let ((slash (position #\/ want)))
          (and slash
               (let ((group (subseq want 0 slash))
                     (rest-name (subseq want (1+ slash))))
                 (find-if (lambda (skill)
                            (and (string= group (skill-group skill))
                                 (or (string= rest-name (string-downcase (skill-name skill)))
                                     (string= rest-name (string-downcase (skill-folder skill))))))
                          all))))
        (let ((hits (remove-if-not (lambda (skill)
                                      (or (search want (string-downcase (skill-name skill)))
                                          (search want (string-downcase (skill-folder skill)))))
                                    all)))
          (and (= 1 (length hits)) (first hits))))))

;;; --- reading one ---------------------------------------------------------------

(defun heading-level (line)
  "LINE's heading level (1 to 6), or NIL when it is not a heading."
  (let ((count 0))
    (loop while (and (< count (length line)) (< count 6) (char= (char line count) #\#))
          do (incf count))
    (when (and (plusp count) (< count (length line))
               (member (char line count) '(#\Space #\Tab)))
      count)))

(defun heading-text (line level)
  (bare-line (subseq line (min level (length line)))))

(defun headings (text &key (depth 3) (limit 24))
  (let ((out '()))
    (dolist (line (split-lines text) (nreverse out))
      (let ((level (heading-level line)))
        (when (and level (<= level depth) (< (length out) limit))
          (push (heading-text line level) out))))))

(defun section-of (text query)
  "The part of TEXT under the first heading that holds QUERY, or NIL."
  (let ((needle (string-downcase query)))
    (loop for tail on (split-lines text)
          for level = (heading-level (first tail))
          when (and level (search needle (string-downcase (first tail))))
            do (return (let ((out (list (first tail))))
                         (dolist (line (rest tail))
                           (let ((inner (heading-level line)))
                             (when (and inner (<= inner level)) (return))
                             (push line out)))
                         (format nil "~{~a~^~%~}" (nreverse out)))))))

(defun skill-body (skill)
  (or (let ((text (read-text (skill-file skill))))
        (and text (format nil "~{~a~^~%~}" (body-of (split-lines text)))))
      "")) 

(defun clipped (skill text max)
  "TEXT, cut at MAX characters, with a note on what is left and how to get it."
  (if (<= (length text) max)
      text
      (let ((rest-headings (headings (subseq text (min max (length text))))))
        (format nil "~a~%~%[~d of ~d characters~@[; headings further down: ~{~a~^ / ~}~]. Read one with marketing_skill {name: ~s, section: \"...\"}, or raise max_chars.]"
                (string-right-trim " " (nlk:clip text max :ellipsis "..."))
                max (length text) rest-headings (skill-name skill)))))

(defun read-skill-text (skill max section)
  (let ((body (skill-body skill)))
    (if (and (plusp (length (nlk:trimmed section)))
             (null (section-of body section)))
        (format nil "~a (~a) has no heading holding ~s. Its headings: ~{~a~^ / ~}. Read it whole with marketing_skill {name: ~s}, or name one of these as section."
                (skill-name skill) (skill-group skill) section
                (headings body) (skill-name skill))
        (clipped skill (or (and (plusp (length (nlk:trimmed section)))
                                (section-of body section))
                           body)
                 max))))

;;; --- what a call answers -------------------------------------------------------

(defun catalogue-note ()
  (format nil "~d skills in ~{~a~^, ~}, shipped with the cell."
          (length (catalog)) (wanted-groups)))

(defun search-answer (query &key group limit (empty-line "Nothing matches"))
  (multiple-value-bind (matches total) (find-skills query :group group :limit limit)
    (cond
      ((null (catalog))
       (format nil "No skill found under ~a: set the marketing section's skills_dir to a folder holding <group>/<skill>/SKILL.md, or remove it to use the tree that ships with the cell."
               (namestring (skills-root))))
      ((null matches)
       (format nil "~a ~s~@[ in ~a~]: ~a Try fewer or broader words — \"seo\", \"carousel\", \"pricing\", \"press release\" — or call marketing_skills with an empty query to list the library."
               empty-line query group (catalogue-note)))
      (t (with-output-to-string (out)
           (format out "~d of ~d skills~@[ matching ~s~]~@[ in ~a~]:~%"
                   (length matches) (length (catalog))
                   (and (plusp (length (nlk:trimmed (or query "")))) query) group)
           (dolist (skill matches)
             (format out "  ~a [~a] ~a~%" (skill-name skill) (skill-group skill)
                     (nlk:clip (or (skill-description skill) "") 150)))
           (when (> total (length matches))
             (format out "~%~d more match: narrow the words, or raise limit.~%" (- total (length matches))))
           (format out "~%Read one with marketing_skill {name: \"~a\"}."
                   (skill-name (first matches))))))))

(defun tool-search (record)
  (let ((limit (skill-search-limit record)))
    (search-answer (skill-search-query record)
                   :group (skill-search-group record)
                   :limit (if (and limit (plusp limit)) limit 20))))

(defun nearest-names (name &key (limit 6))
  "Skills whose name shares a word with NAME, for an answer that must suggest."
  (let ((want (string-downcase (nlk:trimmed (or name ""))))
        (hits '()))
    (when (plusp (length want))
      (dolist (skill (catalog) (nreverse hits))
        (let ((candidate (string-downcase (skill-name skill))))
          (when (and (< (length hits) limit)
                     (or (search want candidate) (search candidate want)))
            (push (skill-name skill) hits)))))))

(defun tool-read (record)
  (let* ((name (skill-read-name record))
         (section (or (skill-read-section record) ""))
         (asked (skill-read-max-chars record))
         (default (max 500 (or (getf (running-settings) :max-chars) 6000)))
         (max (if (and asked (plusp asked)) asked default))
         (skill (resolve-skill name)))
    (cond
      ((null (catalog))
       (format nil "No skill found under ~a: set the marketing section's skills_dir to a folder holding <group>/<skill>/SKILL.md, or remove it to use the tree that ships with the cell."
               (namestring (skills-root))))
      ((null skill)
       (format nil "No skill is named ~s.~@[ The nearest are ~{~a~^, ~}.~] Call marketing_skills {query: ...} to find one; an empty query lists the library."
               name (nearest-names name)))
      (t (format nil "~a (~a), from skills/~a~%~%~a"
                 (skill-name skill) (skill-group skill) (skill-label skill)
                 (read-skill-text skill max section))))))

(defun overview ()
  (let ((counts (make-hash-table :test #'equal))
        (names (make-hash-table :test #'equal)))
    (dolist (skill (catalog))
      (incf (gethash (skill-group skill) counts 0))
      (push (skill-name skill) (gethash (skill-group skill) names)))
    (with-output-to-string (out)
      (format out "~d marketing skills, in three groups:~%" (length (catalog)))
      (dolist (group (wanted-groups))
        (let ((listed (reverse (gethash group names))))
          (format out "  ~a (~d)~@[: ~{~a~^, ~}~]~%"
                  group (gethash group counts 0)
                  (subseq listed 0 (min 6 (length listed))))))
      (format out "~%Ask with words: /marketing technical seo, /marketing ab test sample size, /marketing linkedin carousel.~%Then the model reads the skill that fits: marketing_skills finds it, marketing_skill reads it.~%"))))
;;; --- the command --------------------------------------------------------------

(defun run-slash (args session-id)
  "The /marketing command: words find skills, no words say what is in the library."
  (declare (ignore session-id))
  (let ((args (nlk:trimmed (or args ""))))
    (if (plusp (length args))
        (search-answer args :limit 25)
        (overview))))

;;; --- the cell -----------------------------------------------------------------

(nle:define-cell marketing
  (:section ("marketing")
    (:guide "nothing is required: the 207 skills ship with the cell. groups picks which of strategy, content and social are read; max_chars is how much of one skill a read hands over at a time; skills_dir points at another <group>/<skill>/SKILL.md tree")
    ("skills_dir" :path
     :doc "a folder holding <group>/<skill>/SKILL.md trees, when not the one this cell ships")
    ("groups" :list
     :doc "which groups to read: strategy, content, social (default all three)")
    ("max_chars" :integer :default 6000 :min 500
     :doc "how much of one skill a read hands over at a time"))
  (:help :marketing
         "marketing: 207 marketing skills - marketing_skills {query} finds one, marketing_skill {name} reads it"
         *manual*)
  (:tool "marketing_skills" (nlk:contract 'skill-search) #'tool-search)
  (:tool "marketing_skill" (nlk:contract 'skill-read) #'tool-read)
  (:command "marketing" 'run-slash
            :description "Find a marketing skill in the library"
            :argument-hint "WORDS")
  (:start (lambda ()
            (setf *catalog* nil)
            (nle:on-stop (lambda () (setf *catalog* nil))))))
