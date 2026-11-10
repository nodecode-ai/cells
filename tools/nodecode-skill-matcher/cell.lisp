;;;; cell.lisp --- route a task to the installed cells and skills that fit it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of xXLODXx's hermes-skill-router (see NOTICE): the same
;;;; evidence-first idea, read against this organism's own inventory. The
;;;; original scored a Hermes skill index on every turn and pushed the best
;;;; three into the prompt; here the catalog is the cell folders under this
;;;; home and any SKILL.md under them, and the model asks for a routing with
;;;; one tool call, or the operator with /skill-matcher.
;;;;
;;;; The weights are the original's: a word shared with a name counts 5, a
;;;; tag 3, a description 1, and a stem or compound overlap is supporting
;;;; evidence that can never carry a match alone. Confidence is the score
;;;; over ten, plus a little for agreeing evidence, capped at 0.99. A match
;;;; needs exact evidence and the section's min_score; the rest ride as
;;;; rejections rather than crowding the answer.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "skill-matcher": {"dirs": [], "max_matches": 3, "min_score": 3, "log": true}
;;;; A vetoed section ("enabled": false) installs nothing.

(defpackage #:nodecode-skill-matcher
  (:use #:cl)
  (:export #:catalog-entry-name #:catalog-entry-category #:catalog-entry-description
           #:catalog-entry-tags #:catalog-entry-source
           #:match-entry #:match-decision #:match-confidence #:match-evidence
           #:decision-matches #:decision-rejected #:decision-fallback #:decision-task
           #:scan-catalog #:route #:render-routing #:entry-label
           #:route-skills #:run-command #:start #:log-path #:record-decision
           #:unchanged-p #:*last-signature*))

(in-package #:nodecode-skill-matcher)

(nlk:define-peripheral skill-matcher :not-running t)

;;; --- what a router reads ----------------------------------------------------

(defstruct (catalog-entry (:constructor make-catalog-entry
                                       (name &key category description tags source)))
  "One thing the organism could route to: a cell folder or a skill file."
  name category description tags source)

(defstruct (routing-match (:conc-name match-)
                           (:constructor make-routing-match
                               (entry decision confidence evidence score)))
  entry decision confidence evidence score)

(defstruct (routing-decision (:conc-name decision-)
                              (:constructor make-routing-decision
                                  (&key task matches rejected fallback)))
  task matches rejected fallback)

(defparameter +stopwords+
  '("the" "and" "und" "der" "die" "das" "ein" "eine" "einen" "für" "fur" "ist"
    "sind" "with" "from" "this" "that" "you" "your" "use" "using" "make" "need"
    "want" "should" "would" "can" "how" "what" "when" "why" "who" "all" "any"
    "for" "but" "not" "now" "then" "than" "into" "out" "per" "via" "please"
    "task" "tasks" "skill" "skills" "cell" "cells" "nodecode" "help" "some")
  "Words that describe every entry here, so they route to nothing.")

(defparameter +min-word-length+ 3
  "Shorter words are noise: a router ranks on words that can distinguish.")

(defparameter +min-subword-length+ 4
  "A stem or compound overlap shorter than this is a coincidence.")

(defun task-words (text)
  "The routing words of TEXT, downcased and distinct, in first-seen order."
  (let ((words '())
        (text (or text ""))
        (start nil))
    (flet ((cut (end)
             (when start
               (let ((word (string-downcase (subseq text start end))))
                 (unless (or (< (length word) +min-word-length+)
                             (member word +stopwords+ :test #'string=))
                   (push word words)))
               (setf start nil))))
      (loop for char across text
            for index from 0
            do (if (alphanumericp char)
                   (unless start (setf start index))
                   (cut index)))
      (cut (length text)))
    (nreverse (remove-duplicates (nreverse words) :test #'string=))))

(defun subword-related-p (left right)
  "Whether LEFT and RIGHT are one stem or one compound of the other.
Language-neutral on purpose: the original learned this from real use, and a
dictionary would only work for one language."
  (when (and (string/= left right)
             (>= (min (length left) (length right)) +min-subword-length+))
    (let* ((short (if (<= (length left) (length right)) left right))
           (long (if (eq short left) right left))
           (prefix 0))
      (or (search short long)
          (progn (loop for a across short
                       for b across long
                       while (char= a b)
                       do (incf prefix))
                 (>= prefix (max +min-subword-length+ (floor (length short) 2))))))))

;;; --- reading the inventory --------------------------------------------------

(defun read-file-text (path)
  "PATH's text, or NIL when it cannot be read."
  (ignore-errors (uiop:read-file-string path)))

(defun quoted-after (text key)
  "The double-quoted value that follows KEY in TEXT, or NIL."
  (let ((position (and text (search key text))))
    (when position
      (let* ((open (position #\" text :start (+ position (length key))))
             (close (and open (position #\" text :start (1+ open)))))
        (and close (subseq text (1+ open) close))))))

(defun entry-from-asd (path)
  "The cell folder whose .asd is PATH: its system name and description."
  (let ((text (read-file-text path)))
    ;; A cell folder is named by itself; a skill inside it is filed under it.
    (make-catalog-entry (string-downcase (or (pathname-name path) "cell"))
                        :category ""
                        :description (or (quoted-after text ":description") "")
                        :tags '()
                        :source (namestring path))))

(defun split-tags (text)
  "TEXT as the tag list it is written as: `[a, b]', `a, b' or empty."
  (let ((text (string-trim " []" (or text ""))))
    (when (plusp (length text))
      (remove ""
              (mapcar (lambda (item) (string-downcase (string-trim " \"'" item)))
                      (uiop:split-string text :separator '(#\,)))
              :test #'string=))))

(defun frontmatter (text)
  "A SKILL.md's frontmatter as an alist of (key . value), or NIL."
  (let ((lines (and text (uiop:split-string text :separator '(#\Newline)))))
    (when (and lines (string= "---" (string-trim " " (first lines))))
      (loop for line in (rest lines)
            for trimmed = (string-trim " " line)
            until (string= "---" trimmed)
            collect (let ((colon (position #\: trimmed)))
                      (and colon
                           (cons (string-downcase (string-trim " " (subseq trimmed 0 colon)))
                                 (string-trim " \"'" (subseq trimmed (1+ colon))))))))))

(defun entry-from-skill (path dir)
  "The skill whose SKILL.md is PATH, filed under its folder in DIR."
  (let* ((text (read-file-text path))
         (front (frontmatter text))
         (name (or (cdr (assoc "name" front :test #'string=))
                   (pathname-name (uiop:pathname-parent-directory-pathname path))))
         (tags (split-tags (cdr (assoc "tags" front :test #'string=)))))
    (make-catalog-entry (string-downcase (if (stringp name) name "skill"))
                        :category (or (relative-head path dir) "skill")
                        :description (or (cdr (assoc "description" front :test #'string=)) "")
                        :tags (or tags '())
                        :source (namestring path))))

(defun relative-head (path dir)
  "The first folder under DIR that holds PATH: which cell the skill is in."
  (let* ((dir (uiop:ensure-directory-pathname dir))
         (path (uiop:ensure-directory-pathname
                (uiop:pathname-parent-directory-pathname path)))
         (rest (enough-namestring path dir)))
    (first (remove "" (uiop:split-string rest :separator '(#\/ #\\))
                   :test #'string=))))

(defun catalog-dirs ()
  "The folders the router reads: the section's dirs, else this home's cells."
  (let ((configured (ignore-errors (setting :dirs))))
    (if configured
        (mapcar #'uiop:ensure-directory-pathname configured)
        (list (nlk:home "cells/")))))

(defun scan-catalog (&key dirs)
  "Every entry the router can name: one per cell folder under each of DIRS,
and one per SKILL.md found under them. The first entry of a name wins, so a
folder is never offered twice."
  (let ((entries '())
        (seen (make-hash-table :test #'equal)))
    (flet ((keep (entry)
             (when (and entry (catalog-entry-name entry)
                        (not (gethash (catalog-entry-name entry) seen)))
               (setf (gethash (catalog-entry-name entry) seen) t)
               (push entry entries))))
      (dolist (dir (or dirs (catalog-dirs)))
        (let ((dir (uiop:ensure-directory-pathname dir)))
          (when (uiop:directory-exists-p dir)
            (dolist (folder (uiop:subdirectories dir))
              (let ((asd (first (directory (merge-pathnames "*.asd" folder)))))
                (when asd (keep (entry-from-asd asd)))))
            (dolist (skill (directory (merge-pathnames "**/SKILL.md" dir)))
              (keep (entry-from-skill skill dir)))))))
    (nreverse entries)))

;;; --- the decision ----------------------------------------------------------

(defun evidence-for (words entry)
  "WORDS against ENTRY => a list of (KIND WEIGHT WORD). The first kind
(:name) outranks (:tag), which outranks (:description); :subword is support."
  (let ((name-words (task-words (catalog-entry-name entry)))
        (tag-words (task-words (format nil "~{~a~^ ~}" (catalog-entry-tags entry))))
        (description-words (task-words (catalog-entry-description entry)))
        (evidence '()))
    (dolist (word words)
      (let ((exact nil))
        (when (member word name-words :test #'string=)
          (push (list :name 5.0 word) evidence) (setf exact t))
        (when (member word tag-words :test #'string=)
          (push (list :tag 3.0 word) evidence) (setf exact t))
        (when (member word description-words :test #'string=)
          (push (list :description 1.0 word) evidence) (setf exact t))
        (unless exact
          (loop for (kind field weight) in (list (list :subword name-words 3.0)
                                                 (list :subword tag-words 3.0)
                                                 (list :subword description-words 2.0))
                when (some (lambda (value) (subword-related-p word value)) field)
                  do (push (list kind weight word) evidence) (return)))))
    (nreverse evidence)))

(defun exact-score (match)
  "The evidence MATCH rests on, stem overlap excluded: the original ranked two
matches that reach the same confidence by this, so a name or tag hit outranks
a compound that only looks like one."
  (reduce #'+ (match-evidence match)
          :key (lambda (evidence) (if (eq (first evidence) :subword) 0 (second evidence)))
          :initial-value 0))

(defun route (task &key entries (max-matches 3) (min-score 3))
  "TASK against ENTRIES => a ROUTING-DECISION: at most MAX-MATCHES matches,
each with its evidence, and the rest rejected with the reason."
  (let ((words (task-words task))
        (matches '())
        (rejected '()))
    (dolist (entry entries)
      (let* ((evidence (evidence-for words entry))
             (score (reduce #'+ evidence :key #'second :initial-value 0))
             (kinds (remove-duplicates (mapcar #'first evidence)))
             (exact (intersection kinds '(:name :tag :description))))
        (cond ((or (null evidence) (< score min-score) (null exact))
               (push (make-routing-match entry :reject (min 0.99 (/ score 10)) evidence score)
                     rejected))
              (t (let ((confidence (min 0.99 (+ (/ score 10)
                                                 (if (> (length kinds) 1) 0.12 0.0)))))
                   (push (make-routing-match entry
                                              (if (member :name kinds) :required :recommended)
                                              confidence evidence score)
                         matches))))))
    (setf matches (sort matches (lambda (a b)
                                  (cond ((/= (match-confidence a) (match-confidence b))
                                         (> (match-confidence a) (match-confidence b)))
                                        ((/= (exact-score a) (exact-score b))
                                         (> (exact-score a) (exact-score b)))
                                        (t (string< (catalog-entry-name (match-entry a))
                                                    (catalog-entry-name (match-entry b))))))))
    (let ((over (and (> (length matches) max-matches) (nthcdr max-matches matches))))
      (dolist (match over)
        (push (make-routing-match (match-entry match) :reject
                                  (match-confidence match) (match-evidence match)
                                  (match-score match))
              rejected))
      (make-routing-decision :task task
                             :matches (if over (subseq matches 0 max-matches) matches)
                             :rejected rejected
                             :fallback (if matches nil "no_high_confidence_match")))))

(defun entry-label (entry)
  "ENTRY as one line names it: its folder and, when it has one, its category."
  (let ((category (catalog-entry-category entry)))
    (if (and category (plusp (length category)))
        (format nil "~a/~a" category (catalog-entry-name entry))
        (catalog-entry-name entry))))

(defun render-routing (decision)
  "DECISION as the compact block the model reads, or the fallback line."
  (let ((matches (decision-matches decision)))
    (if (null matches)
        "No high-confidence match. Continue with the normal index."
        (with-output-to-string (out)
          (format out "## Skill routing - ~d ~a for this task~%"
                  (length matches) (if (= 1 (length matches)) "match" "matches"))
          (dolist (match matches)
            (format out "- ~a (~a; confidence ~,2f; evidence: ~{~a~^, ~})~%"
                    (entry-label (match-entry match))
                    (string-downcase (symbol-name (match-decision match)))
                    (match-confidence match)
                    (sort (remove-duplicates
                           (mapcar (lambda (evidence)
                                     (string-downcase (symbol-name (first evidence))))
                                   (match-evidence match))
                           :test #'string=)
                          #'string<)))
          (format out "~%Load one with (help 'name).")))))

;;; --- the local record -------------------------------------------------------

(defvar *last-signature* (make-hash-table :test #'equal)
  "Session id -> the names the last routing named, so a repeat says so.")

(defvar *last-task* (make-hash-table :test #'equal)
  "Session id -> the last task routed here, for a bare /skill-matcher.")

(defun log-path ()
  "Where the routing decisions this home made are appended."
  (nlk:home "skill-matcher/decisions.jsonl"))

(defun signature (decision)
  "What DECISION would say, as the value two decisions compare by."
  (mapcar (lambda (match) (catalog-entry-name (match-entry match)))
          (decision-matches decision)))

(defun unchanged-p (session-id decision)
  "Whether SESSION-ID was last shown this same answer. Registers DECISION as
the answer either way: the original's topic-change test, kept because one
task routed twice should cost one block, not two."
  (let* ((signature (signature decision))
         (previous (gethash session-id *last-signature*)))
    (setf (gethash session-id *last-signature*) signature)
    (and previous (equal previous signature))))

(defun record-decision (decision &key session-id (path (log-path)))
  "Append DECISION to PATH as one JSON line: the names it matched, how many
it rejected, the highest confidence and whether it fell back -- never the
task's words."
  ;; One line per routing, and no word of the task in it: the answer is what
  ;; a later reading needs, and the ask is the operator's.
  (when (ignore-errors (setting :log))
    (let ((object (nlk:json-object
                   "at" (get-universal-time)
                   "matches" (coerce (signature decision) 'vector)
                   "rejected" (count :reject (decision-rejected decision)
                                      :key #'match-decision)
                   "fallback" (or (decision-fallback decision) ""))))
      (when (decision-matches decision)
        (setf (gethash "confidence" object)
              (reduce #'max (decision-matches decision) :key #'match-confidence)))
      (ignore-errors (nlk:append-json-line path object)))
    (decision-task decision)))

;;; --- the tool and the command ----------------------------------------------

(nlk:define-contract route-request
  (:doc "Ask which of this organism's installed cells and skills fit a task,
with the evidence for each match. Call it when you are unsure which skill or
cell a task wants, instead of guessing from the index.")
  ("task" :text :required t :doc "the task in the operator's own words")
  ("max_matches" :integer :doc "how many matches to name; the section's max_matches when omitted"))

(defun route-for (task &key (session-id nil) (max-matches (setting :max-matches)))
  "TASK routed against the live catalog, logged, answered as a DECISION."
  (let ((decision (route task
                         :entries (scan-catalog)
                         :max-matches (or max-matches (setting :max-matches))
                         :min-score (setting :min-score))))
    (when session-id (setf (gethash session-id *last-task*) task))
    (record-decision decision :session-id session-id)
    decision))

(defun route-skills (request)
  "The match_skills tool: the routing block, and one line on what it means
when nothing matched."
  (let* ((session-id (getf (nle:turn) :session-id))
         (decision (route-for (route-request-task request)
                              :session-id session-id
                              :max-matches (route-request-max-matches request)))
         (text (render-routing decision)))
    (if (unchanged-p session-id decision)
        (format nil "~a~%unchanged since your last routing in this session." text)
        text)))

(defun run-command (args session-id)
  "One /skill-matcher invocation: with words, route them; bare, route the last
task this session routed. => the answer's text, and a dialog asking for a task."
  (let* ((task (nlk:trimmed (or args "")))
         (bare (zerop (length task)))
         (task (if bare (gethash session-id *last-task*) task)))
    (if (null task)
        (values "No task routed in this session yet: /skill-matcher <the task's words>."
                (nle:list-dialog
                 :title "Skill Matcher"
                 :summary "Which of the installed cells and skills fit the task you are about to do?"
                 :rows (list (nle:ask-row "Route a task" "Task" "/skill-matcher"
                                           :placeholder "what you are about to do"
                                           :verb "route"))))
        (let* ((decision (route-for task :session-id session-id))
               (text (format nil "~@[task: ~a~%~]~a" (and bare task) (render-routing decision))))
          (values text
                  (nle:list-dialog
                   :title "Skill Matcher"
                   :summary text
                   :rows (list (nle:ask-row "Route another task" "Task" "/skill-matcher"
                                             :placeholder "what you are about to do"
                                             :verb "route"))))))))

(defun start ()
  "A fresh start forgets the last routing of every session; a stop, the same."
  (clrhash *last-signature*)
  (clrhash *last-task*)
  (nle:on-stop (lambda () (clrhash *last-signature*) (clrhash *last-task*))))

(defparameter +primer+
  "Skill routing is available through the nodecode-skill-matcher cell: one tool,
match_skills, and one command, /skill-matcher.
  match_skills {\"task\": \"...\", \"max_matches\": 3}
      It reads every cell folder under this home and every SKILL.md under them,
      scores the task's words against names (5), tags (3) and descriptions (1),
      and names at most max_matches of them with the evidence each rests on.
      A word shared with a name is the strongest signal; a stem or compound
      overlap is support and never carries a match alone. With no match it answers
      the fallback line, and you continue with the normal index.
Use it before guessing from the index which skill a task wants. After it names
one, read that one with (help 'name) and follow it. Section skill-matcher sets
dirs (extra folders to scan), max_matches, min_score and log."
  "What (help :skill-matcher) answers while the cell runs.")

(nle:define-cell skill-matcher
  (:section ("skill-matcher")
    (:guide "nothing here is required; max_matches caps the answer, min_score the evidence a match needs, and a bare /skill-matcher re-routes this session's last task")
    ("dirs" :list :doc "extra folders to scan for cell .asd files and SKILL.md files; empty reads this home's cells folder")
    ("max_matches" :integer :default 3 :min 1
     :doc "how many matches one routing names; the rest are reported as rejected")
    ("min_score" :integer :default 3 :min 1
     :doc "evidence a match needs before it is named: a tag alone is 3, a description alone is not enough")
    ("log" :boolean :default t
     :doc "append each routing to skill-matcher/decisions.jsonl under this home, never the task's words"))
  (:start #'start)
  (:help :skill-matcher "skill-matcher: route a task to the installed cells and skills that fit it, with the evidence" +primer+)
  (:tool "match_skills" (nlk:contract 'route-request) 'route-skills)
  (:command "skill-matcher" 'run-command
            :description "Which installed cells and skills fit this task"
            :argument-hint "TASK WORDS"))
