;;;; tools.lisp --- the three tools, what each sends, and what it answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin offered Hermes three tools -- penny_profile, penny_search and
;;;; penny_remember -- over Penny's tools penny_read and penny_write. This cell
;;;; keeps the three names, the arguments the plugin advertised and the
;;;; bounds it enforced, and answers text the model reads: memory lines, the
;;;; notes a search found, and the receipt a save answered with.
;;;;
;;;; Two differences in shape, both because a cell is not a provider: the
;;;; prompt the plugin prefetched memory into is gone (the model calls
;;;; penny_profile and penny_search when it needs them), and a saved note
;;;; names its source as "nodecode" rather than the plugin's runtime.

(in-package #:nodecode-penny)

(defparameter +max-query-chars+ 4000 "One search query's ceiling, as the plugin had it.")
(defparameter +max-note-chars+ 20000 "One note's ceiling, as the plugin had it.")
(defparameter +max-question-chars+ 500)
(defparameter +max-tag-chars+ 100)
(defparameter +max-tags+ 10)

(defun array-p (value)
  "Whether VALUE is a JSON array (a vector that is not a string)."
  (and (vectorp value) (not (stringp value))))

(defun as-list (value)
  "VALUE -- a JSON array -- as a list; anything else as the empty list."
  (and (array-p value) (coerce value 'list)))

(defun clip (text limit)
  "TEXT cut to LIMIT characters, saying how much was left."
  (cond ((null text) nil)
        ((<= (length text) limit) text)
        (t (format nil "~a ... (~d more characters)"
                   (subseq text 0 limit) (- (length text) limit)))))

(defun scalar-text (value)
  "VALUE -- a JSON scalar -- as one line."
  (cond ((stringp value) value)
        ((null value) "false")
        ((eq value :null) "null")
        ((eq t value) "true")
        ((integerp value) (format nil "~d" value))
        ((numberp value) (format nil "~f" value))
        (t (format nil "~a" value))))

(defun entry-field (entry &rest names)
  "The first of NAMES ENTRY carries as a non-empty string, else NIL."
  (and (hash-table-p entry)
       (loop for name in names
             for value = (nlk:json-value entry :string name)
             when (and value (plusp (length (nlk:trimmed value))))
               return value)))

(defun entries (value)
  "VALUE -- whatever Penny answered -- as a list of memory entries: an array
of them, the array a wrapper object keeps under a name this cell knows, or
the object itself."
  (cond ((hash-table-p value)
         (or (loop for key in '("results" "memories" "notes" "items" "global" "profile" "data")
                   for found = (gethash key value)
                   when (array-p found) return (coerce found 'list))
             (list value)))
        ((array-p value) (coerce value 'list))
        ((or (null value) (eq value :null)) '())
        (t (list value))))

(defun entry-text (entry chars)
  "ENTRY -- one memory entry -- as the lines a reader gets: its own words,
then what it is tagged and how sure it is."
  (cond
    ((stringp entry) (clip (nlk:trimmed entry) chars))
    ((hash-table-p entry)
     (let ((body (entry-field entry "content" "text" "memory" "summary" "body" "value" "name" "title"))
           (tags (as-list (nlk:json-value entry :array "tags")))
           (confidence (nlk:json-value entry :number "confidence"))
           (scope (nlk:json-value entry :object "scope")))
       (format nil "~a~@[~%   tags: ~{~a~^, ~}~]~@[  (confidence ~,2f)~]~@[  [~a]~]"
               (or (clip body chars) "(no text in this entry)")
               (and tags (mapcar (lambda (tag) (scalar-text tag)) tags))
               confidence
               (and scope (nlk:json-value scope :string "kind")))))
    (t (clip (scalar-text entry) chars))))

(defun render (value &key (limit (setting :max-items)) (chars (setting :max-chars))
                      (empty "nothing"))
  "VALUE laid out as a numbered list of memory entries, at most LIMIT of them."
  (let ((items (entries value)))
    (if (null items)
        empty
        (with-output-to-string (stream)
          (loop for item in items
                for index from 1
                do (if (> index limit)
                       (progn (format stream "   ... and ~d more~%" (- (length items) limit))
                              (loop-finish))
                       (format stream "~d. ~a~%" index (entry-text item chars))))))))

;;; --- penny_profile ---------------------------------------------------------------

(defparameter +profile-schema+
  (list :description "Read the Penny core memory and assistant persona kept for this account, before
answering from the operator's preferences or history. Reads memory only; takes no arguments."
        :parameters (nlk:json-object))
  "penny_profile takes no arguments, so its schema advertises the empty object.")

(define-verb penny-profile (arguments)
  "penny_profile: the core memory and persona Penny keeps for this account.
Answers text, one numbered memory per line. Sends no arguments."
  (let ((value (call "penny_read" (nlk:make-json-object "target" "profile"))))
    (declare (ignore arguments))
    (format nil "Penny core memory:~%~a"
            (render value :empty "Penny holds no core memory for this account yet."))))

;;; --- penny_search ----------------------------------------------------------------

(nlk:define-contract search-call
  (:doc "Search your private Penny notes for past decisions, work and context. The query is sent
to Penny's cloud. Answers the notes it found, nearest first, with their tags and confidence.")
  ("query" :text :required t
   :doc "what to look for, in the words of the question you are answering, at most 4,000 characters")
  ("limit" :integer
   :doc "how many notes to ask for, 1 to 10; five unless the call says"))

(define-verb penny-search (call)
  "penny_search: Penny's private notes, by query."
  (let* ((query (nonempty (search-call-query call)))
         (limit (or (search-call-limit call) (setting :default-limit))))
    (unless query
      (fail "penny_search needs a query: what to look for, in the words of the question"))
    (when (> (length query) +max-query-chars+)
      (fail "penny_search: the query is ~d characters; Penny takes at most ~d"
            (length query) +max-query-chars+))
    (unless (and (integerp limit) (<= 1 limit 10))
      (fail "penny_search: limit is 1 to 10, the call said ~a" limit))
    (let ((value (call "penny_read"
                       (nlk:make-json-object "target" "search" "query" query "limit" limit))))
      (format nil "Penny notes for ~s:~%~a"
              (clip query 200)
              (render value :limit limit
                          :empty "no note matched; Penny may hold none on this, which proves no absence")))))

;;; --- penny_remember --------------------------------------------------------------

(nlk:define-contract remember-call
  (:doc "Save one durable memory to your private Penny notes, and say exactly three natural
questions it would answer. Save facts, decisions and findings, with uncertainty stated; never
passwords, credentials or raw transcripts. This sends the note to Penny's cloud. A save counts
only when Penny answers with its receipt.")
  ("content" :text :required t
   :doc "the memory itself, one or two sentences, at most 20,000 characters")
  ("sample_questions" :array :of :string :required t
   :doc "exactly three questions this note would answer, as a person would ask them")
  ("confidence" :number :default 0.7
   :doc "how sure the note is, 0 to 1")
  ("tags" :array :of :string
   :doc "up to ten short labels, each at most 100 characters"))

(defun receipt-text (value)
  "VALUE -- Penny's write answer -- as the line a save answers with, or a
refusal when it did not confirm exactly one saved note."
  (let* ((ok (as-list (nlk:json-value value :array "ok")))
         (failed (as-list (nlk:json-value value :array "failed")))
         (receipts (nlk:json-value value :object "receipts")))
    (unless (and (hash-table-p value)
                 (= 1 (length ok))
                 (stringp (first ok))
                 (plusp (length (nlk:trimmed (first ok))))
                 (null failed))
      (fail "penny: Penny did not confirm the save~@[: ~a~]; read the notes before retrying"
            (and (hash-table-p value) (json-line value 300))))
    (let* ((id (first ok))
           (receipt (and receipts (gethash id receipts)))
           (status (and (hash-table-p receipt) (nlk:json-value receipt :string "status"))))
      (format nil "saved to Penny private notes: ~a~@[ (~a)~]~@[~%  ~d other note(s) were refused~]"
              id (or status "no receipt label") (and failed (length failed))))))

(define-verb penny-remember (call)
  "penny_remember: one note saved to Penny's private notes."
  (let* ((content (nonempty (remember-call-content call)))
         (questions (as-list (remember-call-sample-questions call)))
         (tags (as-list (remember-call-tags call)))
         (confidence (remember-call-confidence call)))
    (unless content
      (fail "penny_remember needs content: the fact, decision or finding to keep"))
    (when (> (length content) +max-note-chars+)
      (fail "penny_remember: the note is ~d characters; Penny takes at most ~d"
            (length content) +max-note-chars+))
    (unless (and (= 3 (length questions))
                  (every (lambda (question) (and (stringp question)
                                                 (plusp (length (nlk:trimmed question)))
                                                 (<= (length question) +max-question-chars+)))
                         questions))
      (fail "penny_remember needs exactly three sample questions, each a non-empty line of at most ~d characters; the call gave ~d"
            +max-question-chars+ (length questions)))
    (when (> (length tags) +max-tags+)
      (fail "penny_remember takes at most ~d tags, the call gave ~d" +max-tags+ (length tags)))
    (unless (every (lambda (tag) (and (stringp tag)
                                      (plusp (length (nlk:trimmed tag)))
                                      (<= (length tag) +max-tag-chars+)))
                   tags)
      (fail "penny_remember: each tag is a non-empty label of at most ~d characters" +max-tag-chars+))
    (unless (and (numberp confidence) (<= 0 confidence 1))
      (fail "penny_remember: confidence is 0 to 1, the call said ~a" confidence))
    (receipt-text
     (call "penny_write"
           (nlk:make-json-object
            "entityType" "note"
            "notes" (vector (nlk:make-json-object
                             "content" content
                             "tags" (coerce tags 'vector)
                             "confidence" confidence
                             "sampleQuestions" (coerce questions 'vector)
                             "source" "nodecode")))))))
