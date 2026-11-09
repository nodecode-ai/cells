;;;; cell.lisp --- the section, the three tools, /dream, the manual, the night worker.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is a skill and a set of scripts that run in a scheduler: a
;;;; deterministic pass reads a fact store and the conversations, counts how
;;;; many separate days a fact came back on, scores what it finds, and hands
;;;; the model a small dossier -- the few facts worth keeping, the memory
;;;; entries nobody has said in a while, the near-duplicates, how full memory
;;;; is. The model decides and writes; the pass never rewrites memory, and it
;;;; invents nothing: a fact said on one day only waits.
;;;;
;;;; This cell is that pass, native. The facts it corroborates are its own
;;;; sightings (DREAM_SIGHT, one line per day a fact was said) and the memory
;;;; it reads is the organism's own knowledge cell -- the (help 'name) entries
;;;; the index already lists. Nothing is deleted: a proposal is text, and the
;;;; model keeps, patches or forgets with the eval it already has.

(in-package #:nodecode-dreaming)

(nlk:define-peripheral dreaming :not-running t)

;;; --- the words ---------------------------------------------------------------------

(defparameter +memory-types+ '("user" "feedback" "project" "reference" "lesson")
  "The kinds a sighting may carry, as define-memory's :type names them.")

(defparameter +diary-name+ "DREAMS.md"
  "The night's log, beside the state file.")

(defparameter +min-score+ 0.55d0
  "Below this a fact is not proposed even when it was said on two days.")

(defparameter +shown-days+ 14
  "A proposal answered once waits this long before it is raised again.")

(defparameter +duplicate-overlap+ 0.62d0
  "Token overlap at which a sighting counts as already kept.")

(defparameter +stopwords+
  '("the" "and" "for" "with" "that" "this" "these" "those" "are" "was" "were"
    "not" "you" "your" "yours" "his" "her" "hers" "she" "him" "they" "them"
    "their" "from" "have" "has" "had" "but" "all" "any" "can" "could" "will"
    "would" "should" "there" "its" "into" "out" "about" "when" "what" "which"
    "who" "how" "why" "than" "then" "also" "just" "only" "very" "more" "most"
    "some" "such" "each" "other" "own" "same" "one" "two" "use" "used" "using"
    "get" "got" "make" "made" "does" "did" "doing" "been" "being" "of" "to"
    "in" "on" "at" "as" "by" "or" "an" "be" "if" "so" "no" "do" "it" "we"
    "us" "my" "me" "he" "is" "always" "never" "every")
  "Words that carry no fact: a rule is what is left.")

(defparameter +primer+
  (format nil "~{~a~^ ~}"
          '("Dreaming consolidates memory the way sleep does: a fact is not kept the"
            "first time it is said. When the person states something worth keeping --"
            "a preference, who they are, a standing rule, a fact about a project --"
            "record it once with dream_sight; when they say it again on another day,"
            "dream_report proposes it, and you keep it with a define-memory eval."
            "The pass also raises the memories nobody has said in a while (ask, then"
            "forget with (unintern 'name) if the person agrees), the memories that"
            "say nearly the same thing, and how full memory is. Nothing is written"
            "for you and nothing is deleted: the proposal is text, the decision is"
            "the person's. Dreaming never invents a candidate, so one quiet day is"
            "not a failure; a fact said once is simply still waiting."))
  "What (help :dreaming) answers, and the line every request carries.")

(defvar *state-dir* nil
  "Bound at start to the section's state_dir, or the home's dreaming/.")

(defvar *memory-source* nil
  "A thunk answering the memory rows the pass reads, or NIL for the real
knowledge cell. The one seam a test stubs.")

;;; --- days --------------------------------------------------------------------------

(defun today (&optional (now (get-universal-time)))
  "NOW's local day as YYYY-MM-DD."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time now)
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

(defun day-time (day)
  "DAY, \"YYYY-MM-DD\", as universal time at its noon; NIL when it is not one."
  (when (and (stringp day) (>= (length day) 10))
    (handler-case
        (multiple-value-bind (year month date)
            (values (parse-integer day :start 0 :end 4)
                    (parse-integer day :start 5 :end 7)
                    (parse-integer day :start 8 :end 10))
          (encode-universal-time 0 0 12 date month year))
      (error () nil))))

(defun days-since (day &optional (now (get-universal-time)))
  "Whole days from DAY to NOW; NIL when DAY is not a day."
  (let ((at (day-time day)))
    (and at (floor (- now at) 86400))))

;;; --- words -------------------------------------------------------------------------

(defun normalize (text)
  "TEXT lowercased, every run of non-alphanumerics one space, trimmed."
  (let ((out (make-string-output-stream))
        (space t))
    (loop for character across (string-downcase (or text ""))
          do (if (alphanumericp character)
                 (progn (write-char character out) (setf space nil))
                 (unless space (write-char #\Space out) (setf space t))))
    (string-trim " " (get-output-stream-string out))))

(defun fingerprint (text)
  "TEXT's key in the ledger: its normalized form, cut to 200 characters."
  (let ((normal (normalize text)))
    (subseq normal 0 (min 200 (length normal)))))

(defun words-of (text)
  (uiop:split-string (normalize text) :separator '(#\Space)))

(defun tokens (text)
  "The words a fact is made of: normalized, three letters and up, no stopword."
  (let ((words '()))
    (dolist (word (words-of text) (nreverse words))
      (when (and (>= (length word) 3)
                 (not (member word +stopwords+ :test #'string=)))
        (pushnew word words :test #'string=)))))

(defun overlap (one other)
  "Jaccard overlap of two token lists, 0..1."
  (let ((left (make-hash-table :test 'equal))
        (shared 0))
    (dolist (word one) (setf (gethash word left) t))
    (dolist (word other) (when (gethash word left) (incf shared)))
    (let ((union (+ (length one) (length other) (- shared))))
      (if (plusp union) (/ (float shared) union) 0.0))))

(defun plural (count singular &optional (plural (concatenate 'string singular "s")))
  (format nil "~d ~a" count (if (= count 1) singular plural)))

;;; --- the state ---------------------------------------------------------------------

(defun state-dir ()
  "The folder this cell keeps its state in."
  (let ((named (or *state-dir* (setting :state-dir))))
    (uiop:ensure-directory-pathname
     (if (and named (stringp named) (string/= (string-trim " " named) ""))
         named
         (nlk:home "dreaming")))))

(defun state-path () (merge-pathnames "state.json" (state-dir)))
(defun diary-path () (merge-pathnames +diary-name+ (state-dir)))

(defun empty-state ()
  (nlk:json-object "format" 1
                   "sights" (nlk:make-json-object)
                   "rejected" (nlk:make-json-object)
                   "shown" (nlk:make-json-object)
                   "last_run" ""))

(defun read-state ()
  "The ledger, or an empty one when there is none yet or it does not read."
  (let ((path (state-path)))
    (or (and (probe-file path)
             (handler-case
                 (let ((state (nlk:decode-json (uiop:read-file-string path))))
                   (and (hash-table-p state)
                        (hash-table-p (nlk:json-value state :object "sights"))
                        state))
               (error () nil)))
        (empty-state))))

(defun write-state (state)
  (ensure-directories-exist (state-path))
  (with-open-file (out (state-path) :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create
                                     :external-format :utf-8)
    (write-string (nlk:encode-json-object state) out)
    (terpri out))
  state)

(defun table-of (state key)
  (let ((object (nlk:json-value state :object key)))
    (or object
        (setf (gethash key state) (nlk:make-json-object)))))

(defun day-list (entry)
  (coerce (nlk:json-value entry :array "days") 'list))

;;; --- a sighting --------------------------------------------------------------------

(defun sight-entry (state key)
  "KEY's sighting, made empty when it is the first."
  (let ((sights (table-of state "sights")))
    (or (gethash key sights)
        (setf (gethash key sights)
              (nlk:json-object "text" "" "type" "user" "days" #() "mentions" 0
                                "inferred" 0 "first" "" "last" "")))))

(defun record-sight (state text type day inferred)
  "One sighting of TEXT on DAY: a second one the same day is a mention, not a new day."
  (let* ((key (fingerprint text))
         (entry (sight-entry state key)))
    (setf (gethash "text" entry) text
          (gethash "type" entry) type)
    (setf (gethash "mentions" entry) (1+ (or (nlk:json-value entry :integer "mentions") 0)))
    (when inferred
      (setf (gethash "inferred" entry) (1+ (or (nlk:json-value entry :integer "inferred") 0))))
    (let ((days (day-list entry)))
      (unless (member day days :test #'string=)
        (setf (gethash "days" entry) (coerce (sort (append days (list day)) #'string<) 'vector))))
    (let ((days (day-list entry)))
      (setf (gethash "first" entry) (first days)
            (gethash "last" entry) (car (last days))))
    entry))

(defun sight-summary (entry)
  (let ((days (day-list entry))
        (mentions (or (nlk:json-value entry :integer "mentions") 0)))
    (format nil "~a, last on ~a (~a)"
            (if (<= (length days) 1)
                "said on one day only"
                (format nil "said on ~a" (plural (length days) "day")))
            (or (car (last days)) "no day")
            (plural mentions "mention"))))

;;; --- the score ---------------------------------------------------------------------
;;; The original's six signals, over the facts this cell can see: how much of
;;; a fact the person said rather than the model inferred, how often it came
;;; back, on how many separate days, how recently, and how far apart those
;;; days are. A single fresh sighting never reaches the gate.

(defun sight-score (entry &optional (now (get-universal-time)))
  "ENTRY's 0..1 score and the sentence that says what carried it."
  (let* ((mentions (max 1 (or (nlk:json-value entry :integer "mentions") 0)))
         (inferred (or (nlk:json-value entry :integer "inferred") 0))
         (said (max 0 (- mentions inferred)))
         (days (day-list entry))
         (reference-days (max 1 (length days)))
         (age (or (days-since (nlk:json-value entry :string "last") now) 365))
         (first (day-time (nlk:json-value entry :string "first")))
         (last (day-time (nlk:json-value entry :string "last")))
         (span (if (and first last) (abs (/ (- last first) 86400.0)) 0.0))
         (words (tokens (nlk:json-value entry :string "text")))
         (relevance (+ (* 0.6 (/ said (float mentions)))
                        (* 0.4 (min (/ mentions 3.0) 1.0))))
         (frequency (min (/ mentions 6.0) 1.0))
         (diversity (min (/ reference-days 3.0) 1.0))
         (recency (exp (- (/ (max 0 age) 30.0))))
         (consolidation (min 1.0 (max (/ span 14.0) (/ (1- reference-days) 3.0))))
         (richness (min (/ (length words) 3.0) 1.0))
         (score (+ (* 0.30 relevance) (* 0.24 frequency) (* 0.15 diversity)
                   (* 0.15 recency) (* 0.10 consolidation) (* 0.06 richness))))
    (values score
            (format nil "score ~,2f: ~a, ~a~@[ -- last said ~a day~:p ago~]"
                    score
                    (plural reference-days "separate day")
                    (plural mentions "mention")
                    (and (plusp age) age)))))

(defun corroborated-p (entry)
  "The gate beside the score: separate days, the person saying it again."
  (>= (length (day-list entry)) (setting :min-days)))

;;; --- the memory it reads -------------------------------------------------------------

(defun knowledge-day (entry)
  (let ((written (ignore-errors (nlk::knowledge-entry-written entry))))
    (and written (integerp written) (plusp written) (today written))))

(defun knowledge-memory-p (entry)
  "A memory, never a skill or a run definition, and one the index would list."
  (and (string= (or (ignore-errors (nlk::knowledge-entry-operator entry)) "") "DEFINE-MEMORY")
       (ignore-errors (nlk::knowledge-entry-description entry))))

(defun real-memory-rows ()
  "Every memory of the organism's knowledge cells, as (:name :text :type :written)."
  (loop for entry in (or (ignore-errors (nlk:knowledge-entries)) '())
        when (knowledge-memory-p entry)
          collect (list :name (nlk::knowledge-entry-name entry)
                        :text (or (nlk::knowledge-entry-description entry) "")
                        :type (string-downcase (string (or (nlk::knowledge-entry-type entry) :user)))
                        :written (knowledge-day entry))))

(defun memory-rows ()
  (if *memory-source* (funcall *memory-source*) (real-memory-rows)))

;;; --- the pass ----------------------------------------------------------------------

(defun sight-rows (state)
  "Every sighting as a plist, newest day first."
  (let ((rows '()))
    (maphash (lambda (key entry)
               (push (list :key key
                           :text (or (nlk:json-value entry :string "text") "")
                           :type (or (nlk:json-value entry :string "type") "user")
                           :mentions (or (nlk:json-value entry :integer "mentions") 0)
                           :days (day-list entry)
                           :last (or (nlk:json-value entry :string "last") ""))
                     rows))
             (table-of state "sights"))
    (sort rows #'string> :key (lambda (row) (getf row :last)))))

(defun duplicate-pairs (rows)
  "The pairs of memories that say nearly the same thing, most alike first."
  (let ((pairs '()))
    (loop for (one . rest) on rows
          do (loop for other in rest
                   for near = (overlap (tokens (getf one :text)) (tokens (getf other :text)))
                   when (>= near +duplicate-overlap+)
                     do (push (list :score near :one one :other other) pairs)))
    (sort pairs #'>= :key (lambda (pair) (getf pair :score)))))

(defun written-before-p (row days &optional (now (get-universal-time)))
  (let ((age (days-since (getf row :written) now)))
    (and age (>= age days))))

(defun memory-pressure (rows)
  (let* ((chars (reduce #'+ rows :key (lambda (row) (length (getf row :text))) :initial-value 0))
         (budget (max 1 (setting :budget-chars))))
    (values chars budget (round (* 100 (/ chars (float budget)))))))

(defun pass-state (state rows &key (now (get-universal-time)))
  "What the night found, as a plist; nothing is written."
  (let ((ready '()) (waiting '()) (kept '()) (rejected 0)
        (rejected-table (table-of state "rejected"))
        (shown-table (table-of state "shown"))
        (sights '()))
    (dolist (row (sight-rows state))
      (push row sights)
      (let ((key (getf row :key)))
        (cond
          ((gethash key rejected-table) (incf rejected))
          ((gethash key shown-table) nil)
          (t
           (let ((entry (sight-entry state key)))
             (multiple-value-bind (score why)
                 (sight-score entry now)
               (let* ((words (tokens (getf row :text)))
                      (nearest (loop for memory in rows
                                     for near = (overlap words (tokens (getf memory :text)))
                                     when (>= near +duplicate-overlap+)
                                       return (list :score near :memory memory))))
                 (cond
                   (nearest (push (list :row row :nearest nearest) kept))
                   ((and (corroborated-p entry) (>= score +min-score+))
                    (push (list :row row :score score :why why) ready))
                   (t (push (list :row row :score score :why why) waiting))))))))))
    (list :ready (nreverse ready)
          :waiting (nreverse waiting)
          :kept (nreverse kept)
          :stale (remove-if-not (lambda (row) (written-before-p row (setting :forget-days) now)) rows)
          :duplicates (duplicate-pairs rows)
          :rejected rejected
          :sights (length sights)
          :rows rows
          :memories (length rows)
          :now now)))

(defun mark-shown (state findings)
  "Every proposal shown tonight waits +SHOWN-DAYS+ before it is shown again."
  (let ((shown (table-of state "shown")))
    (dolist (proposal (getf findings :ready))
      (setf (gethash (getf (getf proposal :row) :key) shown) (today)))
    state))

(defun expire-shown (state &optional (now (get-universal-time)))
  (let ((shown (table-of state "shown"))
        (gone '()))
    (maphash (lambda (key day)
               (let ((age (days-since day now)))
                 (when (and age (>= age +shown-days+)) (push key gone))))
             shown)
    (dolist (key gone) (remhash key shown))
    state))

(defun report-text (findings)
  (with-output-to-string (out)
    (multiple-value-bind (chars budget percent) (memory-pressure (getf findings :rows))
      (format out "dreaming: ~a ready, ~a waiting, ~a already kept, ~d in memory, ~d% full~@[, ~a rejected~]~%"
              (length (getf findings :ready))
              (length (getf findings :waiting))
              (length (getf findings :kept))
              (getf findings :memories)
              percent
              (and (plusp (getf findings :rejected)) (getf findings :rejected)))
      (when (>= percent 80)
        (format out "~&memory is filling: ~a characters of a ~a budget (budget_chars).~%"
                chars budget)))
    (when (getf findings :ready)
      (format out "~&ready -- said on separate days: keep with (define-memory name \"one sentence\" :type :~a)~%"
              (or (getf (getf (first (getf findings :ready)) :row) :type) "user"))
      (dolist (proposal (getf findings :ready))
        (format out "  ~a~%    ~a~%" (getf (getf proposal :row) :text) (getf proposal :why))))
    (when (getf findings :waiting)
      (format out "~&waiting -- one day is not a fact yet~%")
      (dolist (proposal (getf findings :waiting))
        (let ((row (getf proposal :row)))
          (format out "  ~a~%    ~a, ~a~%" (getf row :text)
                  (plural (length (getf row :days)) "day")
                  (plural (getf row :mentions) "mention")))))
    (when (getf findings :kept)
      (format out "~&already in memory -- no proposal~%")
      (dolist (proposal (getf findings :kept))
        (format out "  ~a~%    reads as ~a~%" (getf (getf proposal :row) :text)
                (getf (getf (getf proposal :nearest) :memory) :name))))
    (when (getf findings :duplicates)
      (format out "~&near-duplicates in memory -- the model decides, nothing is merged here~%")
      (loop for pair in (getf findings :duplicates) for i from 1 to 5
            do (format out "  ~,2f  ~a / ~a~%" (getf pair :score)
                       (getf (getf pair :one) :name) (getf (getf pair :other) :name))))
    (when (getf findings :stale)
      (format out "~&still relevant? -- nothing said or written in ~a days~%" (setting :forget-days))
      (loop for row in (getf findings :stale) for i from 1 to 8
            do (format out "  ~a (~a)~%" (getf row :name) (or (getf row :written) "no date"))))
    (format out "~&nothing here was written by the pass: the model keeps, patches or forgets. /dream diary reads the night's log.")))

(defun pass (&key (write-diary t) (mark t))
  "Run the night: read, score, answer the proposals, mark them shown, write the diary."
  (let* ((state (read-state))
         (findings (pass-state state (memory-rows))))
    (expire-shown state)
    (when mark (mark-shown state findings))
    (setf (gethash "last_run" state) (today))
    (write-state state)
    (when write-diary (append-diary findings))
    (values findings (report-text findings))))

(defun append-diary (findings)
  "One section a night, kept small: the count, then what was proposed."
  (ensure-directories-exist (diary-path))
  (with-open-file (out (diary-path) :direction :output
                                     :if-exists :append
                                     :if-does-not-exist :create
                                     :external-format :utf-8)
    (format out "~&## ~a~%" (today))
    (multiple-value-bind (chars budget percent) (memory-pressure (getf findings :rows))
      (declare (ignore chars))
      (format out "- ~a proposed, ~a waiting, ~d in memory, ~d% of a ~a budget~%"
              (length (getf findings :ready))
              (length (getf findings :waiting))
              (getf findings :memories) percent budget))
    (dolist (proposal (getf findings :ready))
      (format out "- ready: ~a~%" (getf (getf proposal :row) :text)))
    (loop for row in (getf findings :stale) for i from 1 to 5
          do (format out "- still relevant? ~a~%" (getf row :name)))
    (when (getf findings :duplicates)
      (format out "- ~a near-duplicate ~a in memory~%"
              (length (getf findings :duplicates))
              (if (= 1 (length (getf findings :duplicates))) "pair" "pairs")))
    (diary-path)))

(defun diary-tail (&optional (lines 14))
  "The newest LINES of the night's log, or a line saying there is none yet."
  (let ((path (diary-path)))
    (if (probe-file path)
        (let ((all (uiop:split-string (uiop:read-file-string path)
                                       :separator '(#\Newline))))
          (format nil "~{~a~^~%~}" (last all (min lines (length all)))))
        "no night has run yet: /dream pass runs one, and the worker runs one at the configured hour")))

;;; --- the verbs the model and the operator call ---------------------------------------

(defun sight (text &optional (type "user") (day "") (inferred nil))
  "Record that TEXT was said on DAY -- today when DAY is empty."
  (let* ((day (if (and (stringp day) (string/= (string-trim " " day) "")) day (today)))
         (type (string-downcase (or type "user"))))
    (unless (day-time day)
      (fail "~s is not a day: write it YYYY-MM-DD" day))
    (unless (member type +memory-types+ :test #'string=)
      (fail "~s is not a memory type: ~{~a~^, ~}" type +memory-types+))
    (let* ((text (string-trim " " (or text "")))
           (state (read-state))
           (entry (record-sight state text type day inferred)))
      (when (string= "" text)
        (fail "a sighting needs the fact's words"))
      (write-state state)
      (format nil "~a: ~a~%~a"
              (sight-summary entry)
              (if (corroborated-p entry)
                  "corroborated -- it came back on separate days; dream_report proposes it"
                  (format nil "waiting -- it needs ~a on separate days (min_days)"
                          (plural (max 1 (- (setting :min-days) (length (day-list entry)))) "more day")))
              (let ((nearest (loop for row in (memory-rows)
                                   for near = (overlap (tokens text) (tokens (getf row :text)))
                                   when (>= near +duplicate-overlap+) return row)))
                (if nearest
                    (format nil "already in memory as ~a -- no proposal" (getf nearest :name))
                    ""))))))

(defun report-now (&optional (write-diary t))
  "The night's pass, now: the proposals in words, and the diary line."
  (multiple-value-bind (findings text) (pass :write-diary write-diary)
    (when (getf findings :ready)
      (nle:notice (format nil "dreaming: ~a ready to keep" (length (getf findings :ready)))
                  :level :info :key "nodecode-dreaming"))
    text))

(defun reject (text &optional reason)
  "Close TEXT: the pass stops proposing it."
  (when (string= "" (string-trim " " (or text "")))
    (fail "a rejection needs the fact's words"))
  (let* ((state (read-state))
         (table (table-of state "rejected"))
         (key (fingerprint text)))
    (setf (gethash key table) (nlk:json-object "text" text "day" (today)
                                                "reason" (or reason "")))
    (write-state state)
    (format nil "closed, and not proposed again: ~a~@[ (~a)~]" text reason)))

(defun run-slash (args session-id)
  "One /dream invocation."
  (declare (ignore session-id))
  (let* ((text (string-trim " " (or args "")))
         (head (first (uiop:split-string text :separator '(#\Space))))
         (rest (if head (string-trim " " (subseq text (min (length text) (length head)))) "")))
    (cond
      ((or (string= text "") (string-equal head "pass")) (report-now t))
      ((string-equal head "diary") (diary-tail))
      ((string-equal head "sight") (sight rest))
      ((string-equal head "reject") (reject rest))
      ((string-equal head "forget")
       (let ((findings (pass-state (read-state) (memory-rows))))
         (if (getf findings :stale)
             (format nil "still relevant? ~{~a~^, ~}~&ask, then forget with (unintern 'name)"
                     (mapcar (lambda (row) (getf row :name)) (getf findings :stale)))
             "nothing is stale: every memory was written inside forget_days")))
      (t (format nil "~a~&dreaming: /dream runs the pass, /dream diary reads the night's log, ~
/dream sight <fact> records one, /dream reject <fact> closes one" text)))))

;;; --- the night -----------------------------------------------------------------------

(defun night-due-p (state &optional (now (get-universal-time)))
  "True at the configured hour, once a day."
  (and (= (nth-value 2 (decode-universal-time now)) (setting :hour))
       (string/= (or (nlk:json-value state :string "last_run") "") (today now))))

(defun nightly-step ()
  "What the worker runs: the pass at the hour, a notice when it found something."
  (let ((state (read-state)))
    (when (night-due-p state)
      (multiple-value-bind (findings text)
          (pass :write-diary t)
        (declare (ignore text))
        (when (or (getf findings :ready) (getf findings :stale))
          (nle:notice (format nil "dreaming: ~a to keep, ~a still relevant?"
                              (length (getf findings :ready))
                              (length (getf findings :stale)))
                      :level :info :key "nodecode-dreaming"))))))

;;; --- the boundary --------------------------------------------------------------------

(nlk:define-contract dream-sight
  (:doc "Record that a fact was said, so the night can propose it once it comes back on separate days. One call per fact per day is enough; a second the same day is a mention. A fact already in memory, or one said on a single day, is never proposed -- the night waits for the person to say it again.")
  ("text" :text :required t
          :doc "the fact in one declarative sentence, as the person said it")
  ("type" :text :default "user" :options +memory-types+ :slot memory-type
          :doc "what kind of memory it would be: who the person is, a preference or correction, a fact about a project, a reference, a lesson")
  ("day" :text :default ""
         :doc "the day it was said, YYYY-MM-DD; today unless you know another")
  ("inferred" :boolean :default nil
              :doc "true when this is the model's conclusion rather than the person's words"))

(nlk:define-contract dream-report
  (:doc "Run the consolidation pass now and read what it found: the facts said on separate days that are worth keeping, the ones waiting for a second day, the memories nothing has touched in a while, the near-duplicates, and how full memory is. The pass writes only its ledger and the diary; keep, patch or forget with the eval you already have.")
  ("write_diary" :boolean :default t
                 :doc "append tonight's section to the diary"))

(nlk:define-contract dream-reject
  (:doc "Close a proposal for good: the pass stops showing it. Reject what the person did not confirm, not what you are unsure about.")
  ("text" :text :required t :doc "the fact's words, as the proposal showed them")
  ("reason" :text :default "" :doc "one short reason, which the ledger keeps"))

(defun sight-tool (call)
  (sight (dream-sight-text call) (dream-sight-memory-type call)
         (dream-sight-day call) (dream-sight-inferred call)))

(defun report-tool (call)
  (report-now (dream-report-write-diary call)))

(defun reject-tool (call)
  (reject (dream-reject-text call) (dream-reject-reason call)))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell dreaming
  (:section ("dreaming")
    (:guide "A fact is kept once it comes back on separate days. state_dir holds the ledger and the diary; the worker runs the pass at hour; min_days and budget_chars say when a fact is a fact and when memory is full")
    ("state_dir" :path
                 :doc "where the ledger and the diary live; empty means the home's dreaming/")
    ("min_days" :integer :default 2 :min 2
                :doc "separate days a fact must be said on before it is proposed")
    ("forget_days" :integer :default 60 :min 1
                   :doc "days without a word after which a memory is raised as still relevant?")
    ("budget_chars" :integer :default 12000 :min 1000
                    :doc "the characters of memory at which the night says memory is filling")
    ("hour" :integer :default 3 :min 0
            :doc "the hour the night runs, 0-23"))
  (:tool "dream_sight" (nlk:contract 'dream-sight) 'sight-tool)
  (:tool "dream_report" (nlk:contract 'dream-report) 'report-tool)
  (:tool "dream_reject" (nlk:contract 'dream-reject) 'reject-tool)
  (:help :dreaming "dreaming: a fact is kept once it comes back on separate days; dream_sight, dream_report, dream_reject, /dream" +primer+)
  (:command "dream" 'run-slash
            :catches 'dreaming-error
            :description "The night's consolidation pass: proposals, the diary, a sighting")
  (:start (lambda ()
            (setf *state-dir* (setting :state-dir))
            (let ((worker (nlk:worker-start "dreaming-night" #'nightly-step :wake 300)))
              (nle:on-stop (lambda ()
                             (nlk:worker-stop worker)
                             (setf *state-dir* nil)))))))
