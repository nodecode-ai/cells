;;;; cell.lisp --- mem-os memory: durable facts in one local file, six tools, /memos.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of Markgatcha/memos's Hermes plugin (see NOTICE). The original
;;;; keeps memories in SQLite behind a Node MCP server, embeds them with a
;;;; local model and hands the host six memos_* tools plus a skill and a
;;;; SessionStart summary. This cell keeps the same six verbs over one
;;;; JSON-lines file of facts: store, search, a token-budgeted pack,
;;;; supersede, forget, history. Nothing leaves the machine and no runtime
;;;; beyond this image is needed, so the facts are readable with a text
;;;; editor and a rebuild of the host cannot lose them.
;;;;
;;;; The log is append-only: every store, supersede and forget is one line,
;;;; and what a caller reads is the fold of those lines. A correction never
;;;; rewrites an old sentence, so a later question can still ask what was
;;;; believed then -- the original's version chain, kept as data.

(defpackage #:nodecode-memos
  (:use #:cl)
  (:nicknames #:memos)
  (:export #:memos-error #:memos-error-detail
           #:store #:recall #:context-pack #:forget #:supersede-fact #:history
           #:live-memories #:store-path))

(in-package #:nodecode-memos)

(nlk:define-peripheral memos :not-running t :lock "memos")

;;; --- the words the cell runs on ------------------------------------------------

(defparameter +types+ '("preference" "fact" "context" "entity" "relationship")
  "What a memory may be: the original's five types, unchanged. A preference is
how the operator wants things, a fact is a claim about the world, context is a
detail of a project or an environment, entity is a person, place or thing, and
relationship is how two entities stand.")

(defparameter +standard-namespace+ "default"
  "The namespace a memory lands in when the call names none: what a search that
names no namespace reads.")

(defparameter +chars-per-token+ 4
  "What a pack budgets per token: English prose runs near four characters, and
a pack is a budget rather than a bill, so the estimate may be rough.")

(defparameter +max-text+ 4000
  "The longest one memory keeps, in characters: a fact is a sentence or a
paragraph, not a document; a document goes in a file and the fact names it.")

(defparameter +stopwords+
  '("the" "a" "an" "and" "or" "of" "to" "in" "on" "at" "for" "with" "from" "by"
    "is" "are" "was" "were" "be" "been" "it" "its" "this" "that" "these" "those"
    "i" "we" "you" "he" "she" "they" "my" "our" "your" "me" "us" "do" "does"
    "did" "what" "which" "who" "whom" "how" "when" "where" "why" "not" "no"
    "if" "then" "than" "so" "as" "but" "about" "into" "over" "there")
  "Words a query never searches by: a fact is matched on what it says, not on
the grammar a question carries.")

;;; --- where the facts are, and what the file says ------------------------------

(defun store-path ()
  "The file the facts live in: the section's, else <home>/memos/memories.jsonl."
  (let ((given (setting :file)))
    (if (and given (plusp (length (nlk:trimmed (princ-to-string given)))))
        (pathname (princ-to-string given))
        (nlk:home "memos/memories.jsonl"))))

(defun fact-lines ()
  "Every line of the file, decoded, oldest first. A line that is not a JSON
object is skipped, so one damaged line costs one fact and not the file."
  (let ((path (store-path)))
    (if (probe-file path)
        (with-open-file (in path :direction :input :external-format :utf-8)
          (loop for line = (read-line in nil nil)
                while line
                for object = (and (plusp (length (nlk:trimmed line)))
                                  (ignore-errors (nlk:decode-json line)))
                when (hash-table-p object) collect object))
        '())))

(defstruct (memory (:constructor make-memory) (:copier nil))
  "One memory as the file's facts describe it now."
  (id "" :read-only t)
  (text "" :read-only t)
  (type "fact" :read-only t)
  (namespace +standard-namespace+ :read-only t)
  (tags '() :read-only t)
  (at "" :read-only t)
  (superseded-by nil)
  (forgotten nil))

(defun tags-of (fact)
  (remove-if-not #'stringp (coerce (or (nlk:json-value fact :array "tags") #()) 'list)))

(defun all-memories ()
  "Every memory this file ever kept, oldest store first, each in its latest
state: superseded and forgotten memories included, in the order they were first
stored."
  (let ((table (make-hash-table :test #'equal))
        (order '()))
    (dolist (fact (fact-lines))
      (let ((id (nlk:json-value fact :string "id"))
            (op (or (nlk:json-value fact :string "op") "")))
        (when id
          (cond
            ((string= op "store")
             (unless (gethash id table) (push id order))
             (setf (gethash id table)
                   (make-memory :id id
                                :text (or (nlk:json-value fact :string "text") "")
                                :type (or (nlk:json-value fact :string "type") "fact")
                                :namespace (or (nlk:json-value fact :string "namespace")
                                               +standard-namespace+)
                                :tags (tags-of fact)
                                :at (or (nlk:json-value fact :string "at") ""))))
            ((string= op "supersede")
             (nlk:when-let (memory (gethash id table))
               (setf (memory-superseded-by memory) (nlk:json-value fact :string "by"))))
            ((string= op "forget")
             (nlk:when-let (memory (gethash id table))
               (setf (memory-forgotten memory) t)))))))
    (loop for id in (nreverse order) collect (gethash id table))))

(defun live-memories (&key namespace memory-type)
  "The memories a search may answer: neither forgotten nor superseded, newest
first, narrowed to NAMESPACE and MEMORY-TYPE when they are given."
  (reverse
   (remove-if (lambda (memory)
                (or (memory-forgotten memory)
                    (memory-superseded-by memory)
                    (and namespace (not (string-equal namespace (memory-namespace memory))))
                    (and memory-type (not (string-equal memory-type (memory-type memory))))))
              (all-memories))))

(defun find-memory (id &key (otherwise t))
  "ID's memory, whatever its state, or a refusal naming it."
  (let ((memory (find (nlk:trimmed (princ-to-string (or id "")))
                       (all-memories) :key #'memory-id :test #'string-equal)))
    (or memory
        (and otherwise (fail "no memory ~s: take an id from a search line, or /memos recent" id)))))

(defun append-fact (object)
  "One line at the end of the file, its directory made first. => the path."
  (let ((path (store-path)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :append
                              :if-does-not-exist :create :external-format :utf-8)
      (write-line (nlk:encode-json-object object) out))
    path))

(defun fresh-id (text)
  "A new memory's id: eight hex characters of a hash of TEXT and its moment,
unique among the ids the file already holds."
  (let ((known (make-hash-table :test #'equal)))
    (dolist (memory (all-memories)) (setf (gethash (memory-id memory) known) t))
    (loop for salt from 0
          for id = (format nil "mem:~a"
                            (subseq (nlk:sha256-text
                                     (format nil "~a|~a|~a" text (nlk:iso-now) salt))
                                    7 15))
          unless (gethash id known) return id)))

(defun day (at)
  "AT's date, for a line a model reads."
  (if (and (stringp at) (>= (length at) 10)) (subseq at 0 10) "?"))

;;; --- writing a memory ---------------------------------------------------------

(defun normalize-type (type)
  (let ((type (string-downcase (nlk:trimmed (princ-to-string (or type "fact"))))))
    (if (member type +types+ :test #'string=)
        type
        (fail "~s is no memory type: ~{~a~^, ~}" type +types+))))

(defun normalize-namespace (namespace)
  (let ((namespace (string-downcase (nlk:trimmed (princ-to-string (or namespace ""))))))
    (cond ((zerop (length namespace)) +standard-namespace+)
          ((> (length namespace) 64) (fail "a namespace is at most 64 characters"))
          ((not (every (lambda (character)
                         (or (alphanumericp character) (find character "-._/")))
                       namespace))
           (fail "~s is no namespace: letters, digits and - . _ /" namespace))
          (t namespace))))

(defun require-text (text what)
  (let ((text (nlk:trimmed (princ-to-string (or text "")))))
    (when (zerop (length text)) (fail "~a needs words" what))
    (when (> (length text) +max-text+) (fail "~a is at most ~d characters" what +max-text+))
    text))

(defun clean-tags (tags)
  (let ((tags (loop for tag in (coerce (or tags '()) 'list)
                    for text = (and tag (nlk:trimmed (princ-to-string tag)))
                    when (and text (plusp (length text))) collect text)))
    (remove-duplicates (mapcar (lambda (tag) (subseq tag 0 (min 64 (length tag)))) tags)
                       :test #'string-equal)))

(defun store (text &key (type "fact") (namespace +standard-namespace+) tags)
  "Keep TEXT as one memory. => the line the model reads, its id first."
  (let ((text (require-text text "a memory"))
        (type (normalize-type type))
        (namespace (normalize-namespace namespace))
        (tags (clean-tags tags)))
    (with-memos-lock
      (let ((id (fresh-id text)))
        (append-fact (nlk:make-json-object
                      "op" "store" "id" id "text" text "type" type
                      "namespace" namespace "tags" (coerce tags 'vector)
                      "at" (nlk:iso-now)))
        (format nil "remembered [~a] ~a~@[/~a~]: ~a"
                id type (and (string/= namespace +standard-namespace+) namespace)
                (nlk:clip (nlk:one-line text) 200))))))

(defun supersede-fact (id text)
  "Keep TEXT as ID's correction: a new memory, and ID marked superseded by it.
The old wording stays in the file, so the history still reads. => both ids."
  (let* ((old (find-memory id))
         (text (require-text text "a correction")))
    (when (memory-forgotten old)
      (fail "memory ~a was forgotten: keep the correction as a new memory" (memory-id old)))
    (when (memory-superseded-by old)
      (fail "memory ~a is already superseded by ~a: correct that one"
                (memory-id old) (memory-superseded-by old)))
    (with-memos-lock
      (let ((new (fresh-id text)))
        (append-fact (nlk:make-json-object
                      "op" "store" "id" new "text" text
                      "type" (memory-type old) "namespace" (memory-namespace old)
                      "tags" (coerce (memory-tags old) 'vector) "at" (nlk:iso-now)))
        (append-fact (nlk:make-json-object
                      "op" "supersede" "id" (memory-id old) "by" new
                      "at" (nlk:iso-now)))
        (format nil "superseded [~a] with [~a]: ~a"
                (memory-id old) new (nlk:clip (nlk:one-line text) 200))))))

(defun forget (id)
  "Mark ID forgotten: no search answers it, and the file still holds it."
  (let ((memory (find-memory id)))
    (when (memory-forgotten memory)
      (fail "memory ~a was forgotten already" (memory-id memory)))
    (with-memos-lock
      (append-fact (nlk:make-json-object "op" "forget" "id" (memory-id memory)
                                          "at" (nlk:iso-now))))
    (format nil "forgot [~a]: ~a" (memory-id memory)
            (nlk:clip (nlk:one-line (memory-text memory)) 200))))

(defun history (id)
  "How ID came to read as it does: its text and every correction after it."
  (let ((memory (find-memory id)))
    (with-output-to-string (out)
      (format out "memory [~a] ~a~@[/~a~], ~a~%  ~a~@[~%  (forgotten)~]"
              (memory-id memory) (memory-type memory)
              (and (string/= (memory-namespace memory) +standard-namespace+)
                   (memory-namespace memory))
              (day (memory-at memory)) (memory-text memory)
              (memory-forgotten memory))
      (loop for next = (and (memory-superseded-by memory)
                            (find-memory (memory-superseded-by memory) :otherwise nil))
            then (and (memory-superseded-by next)
                      (find-memory (memory-superseded-by next) :otherwise nil))
            while next
            do (format out "~%superseded by [~a] ~a~%  ~a"
                       (memory-id next) (day (memory-at next)) (memory-text next))
               (setf memory next))
      (format out "~%  ~d line~:p in ~a" (length (fact-lines)) (namestring (store-path))))))

;;; --- finding, and packing ------------------------------------------------------

(defun terms (text)
  "TEXT's search terms: lowercase runs of letters and digits, two characters or
longer, stopwords out."
  (let ((text (string-downcase (princ-to-string (or text ""))))
        (start nil)
        (found '()))
    (loop for character across text
          for index from 0
          do (if (alphanumericp character)
                 (unless start (setf start index))
                 (when start
                   (push (subseq text start index) found)
                   (setf start nil))))
    (when start (push (subseq text start) found))
    (remove-duplicates
     (remove-if (lambda (term)
                  (or (< (length term) 2) (member term +stopwords+ :test #'equal)))
                (nreverse found))
     :test #'equal :from-end t)))

(defun occurrences (needle haystack)
  "How many times NEEDLE occurs in HAYSTACK, case-insensitively. An empty
NEEDLE is no occurrence, so a query of nothing adds nothing."
  (if (zerop (length needle))
      0
      (loop with step = (length needle)
            for at = (search needle haystack :test #'char-equal)
            then (search needle haystack :start2 (min (+ at step) (length haystack))
                                        :test #'char-equal)
            while at
            count 1)))

(defun score-of (memory terms phrase)
  "How well MEMORY answers TERMS: its words count, its tags count double, its
type and namespace a little, and the whole PHRASE more than any of its words."
  (let ((text (memory-text memory))
        (score 0.0))
    (dolist (term terms)
      (incf score (occurrences term text))
      (when (some (lambda (tag) (search term tag :test #'char-equal)) (memory-tags memory))
        (incf score 2.0))
      (when (search term (memory-type memory) :test #'char-equal) (incf score 0.5))
      (when (search term (memory-namespace memory) :test #'char-equal) (incf score 0.5)))
    (when (plusp (occurrences phrase text)) (incf score 3.0))
    score))

(defun ranked (query &key namespace memory-type limit)
  "The memories nearest QUERY, best first, at most LIMIT of them -- the
section's limit when the call names none. A query of nothing but grammar
answers the newest, which is what an empty /recall asks for."
  (let* ((query (nlk:trimmed (princ-to-string (or query ""))))
         (terms (terms query))
         (phrase (string-downcase query))
         (candidates (live-memories :namespace namespace :memory-type memory-type))
         (scored (mapcar (lambda (memory) (cons (score-of memory terms phrase) memory))
                         candidates))
         (hits (stable-sort (if terms
                                (remove-if (lambda (pair) (not (plusp (car pair)))) scored)
                                scored)
                            #'> :key #'car))
         (limit (or limit (setting :limit))))
    (if (and limit (> (length hits) limit)) (subseq hits 0 limit) hits)))

(defun hit-line (pair)
  "One hit as a line the model reads: id, type, namespace, words, date."
  (let ((score (car pair))
        (memory (cdr pair)))
    (format nil "- [~a] ~a~@[/~a~]: ~a~@[ (~,1f)~]  (~a)"
            (memory-id memory) (memory-type memory)
            (and (string/= (memory-namespace memory) +standard-namespace+)
                 (memory-namespace memory))
            (nlk:clip (memory-text memory) 400)
            (and (plusp score) score)
            (day (memory-at memory)))))

(defun recall (query &key limit namespace memory-type)
  "The memories nearest QUERY, as the lines the model reads."
  (let* ((query (nlk:trimmed (princ-to-string (or query ""))))
         (hits (ranked query :namespace namespace :memory-type memory-type :limit limit)))
    (if (null hits)
        (format nil "no memory matches ~s~@[ in namespace ~a~]" query namespace)
        (format nil "~d memor~:@p for ~s~@[ in namespace ~a~]:~%~{~a~^~%~}"
                (length hits) query namespace (mapcar #'hit-line hits)))))

(defun context-pack (query &key (tokens 2000) namespace)
  "The memories nearest QUERY that fit TOKENS tokens, best first, and how many
did not fit. The best one is always kept, whatever the budget."
  (let* ((query (nlk:trimmed (princ-to-string (or query ""))))
         (budget (max 1 (* +chars-per-token+ (max 1 tokens))))
         (hits (ranked query :namespace namespace :limit most-positive-fixnum))
         (kept '())
         (used 0))
    (dolist (pair hits)
      (let ((line (hit-line pair)))
        (cond ((<= (+ used (length line) 1) budget)
               (push line kept)
               (incf used (1+ (length line))))
              (t
               (unless kept (push (subseq line 0 (min (length line) budget)) kept))
               (return)))))
    (let ((left (- (length hits) (length kept))))
      (if (null hits)
          (format nil "no memory matches ~s~@[ in namespace ~a~]" query namespace)
          (format nil "~d memor~:@p~@[ for ~s~] (~d-token budget)~@[; ~d more did not fit~]:~%~{~a~^~%~}"
                  (length kept) (and (plusp (length query)) query) tokens
                  (and (plusp left) left)
                  (nreverse kept))))))

;;; --- the manual, the state line, and /memos ------------------------------------

(defparameter +primer+
  "MemOS is this machine's own memory: durable facts in one local file, found by their
words, each with a history of corrections. Nothing leaves the machine and no server is
needed. Tools, each called with JSON arguments:
  memos_store {text, memory_type?, namespace?, tags?}     keep one durable fact; answers its id
  memos_search {query, limit?, namespace?, memory_type?}  the facts nearest a query, with ids
  memos_context_pack {query, tokens?, namespace?}         the same, packed into a token budget
  memos_supersede {memory_id, text}                       correct a fact; the old wording stays
  memos_forget {memory_id}                                stop answering it; the file keeps it
  memos_history {memory_id}                               how a fact came to read as it does
Store the moment the operator states a lasting preference, a correction or a decision, and
search before answering anything that may depend on what was said earlier; for a multi-part
question search again in other words. Take an id from a search line before you correct or
forget one. In eval the same verbs are (memos:store \"...\"), (memos:recall \"...\"),
(memos:context-pack \"...\"), (memos:history \"m-...\"). /memos QUERY packs what is relevant,
/memos store TEXT remembers one, /memos recent lists the newest. The memos section of
config.jsonc names the file, how many a search answers, and standing: how many characters of
the newest facts ride every prompt (0, the default, keeps them for the tools alone)."
  "What (help :memos) answers while the cell runs.")

(defun status-line ()
  (let* ((all (all-memories))
         (live (live-memories))
         (forgotten (count-if #'memory-forgotten all))
         (superseded (count-if (lambda (memory)
                                  (and (not (memory-forgotten memory))
                                       (memory-superseded-by memory)))
                                all)))
    (format nil "MemOS memory: ~d live fact~:p in ~a~@[; ~a~].~%~
                 memos_search finds them; /memos QUERY packs them, /memos store TEXT remembers one, /memos recent lists the newest."
            (length live) (namestring (store-path))
            (and (or (plusp superseded) (plusp forgotten))
                 (format nil "~d superseded, ~d forgotten" superseded forgotten)))))

(defun panel ()
  (nle:list-dialog
   :title "Memos" :summary (format nil "~d live facts" (length (live-memories)))
   :rows (list (nle:ask-row "Remember a fact" "Fact" "/memos store"
                             :placeholder "one sentence that stays true"
                             :verb "store"
                             :detail "kept in the memos file, found by its words")
               (nle:run-row "Show the newest" "/memos recent"
                             :detail "the last ten live facts, with their ids"))))

(defun run-command (args session-id)
  "One /memos invocation: nothing the state of the store, an action, or a query
to pack."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (princ-to-string (or args ""))))
         (space (position #\Space text))
         (head (string-downcase (if space (subseq text 0 space) text)))
         (rest (if space (nlk:trimmed (subseq text space)) "")))
    (cond ((zerop (length text)) (values (status-line) (panel)))
          ((string= head "store")
           (values (if (zerop (length rest))
                       "memos: say what to remember, as /memos store TEXT"
                       (store rest))
                   nil))
          ((string= head "recent") (values (recall "" :limit 10) nil))
          (t (values (context-pack text) nil)))))

;;; --- standing: the newest facts in every prompt, when the section asks ----------

(defun standing-text ()
  "The newest live facts, up to the section's standing characters, as one
prompt section; NIL when the section asks for none or there are none."
  (let ((budget (setting :standing))
        (memories (live-memories)))
    (when (and (plusp budget) (plusp (length memories)))
      (let ((kept '())
            (used 0))
        (dolist (memory memories)
          (let ((line (hit-line (cons 0.0 memory))))
            (when (<= (+ used (length line) 1) budget)
              (push line kept)
              (incf used (1+ (length line))))))
        (when kept
          (format nil "~d fact~:p from this machine's MemOS memory (memos_search finds more; memos_store keeps one):~%~{~a~^~%~}"
                  (length kept) (nreverse kept)))))))

(defun add-standing (next session-id)
  "The newest facts the section asks to stand in every request, as one harness
section; the chain's own sections untouched."
  (let ((facts (standing-text)))
    (if facts
        (acons "memos" facts
               (remove "memos" (funcall next session-id) :key #'car :test #'equal))
        (funcall next session-id))))

;;; --- the tools: the boundary is a contract, once ------------------------------

(nlk:define-contract memos-store
  (:doc "Keep one durable fact in this machine's local memory: a preference, a correction, a decision, an environment detail, or who someone is. Call it the moment the operator states something worth recalling in a later session; skip transient chit-chat and what you stored already. Answers the memory's id, which a later supersede or forget takes.")
  ("text" :text :required t
   :doc "the fact itself, one sentence, in the words that will still be true later")
  ("memory_type" :string :default "fact" :options +types+
   :doc "preference (how the operator wants things), fact (a claim about the world), context (a detail of a project or environment), entity (a person, place or thing), relationship (how two entities stand)")
  ("namespace" :string :default "default"
   :doc "what the fact is about -- a project, a repository, a topic; labels: a search may narrow to one")
  ("tags" :array :of :string
   :doc "a few words naming the fact's subject, matched more strongly than its text"))

(nlk:define-contract memos-search
  (:doc "Find stored memories by their words: the facts nearest a query, best first, each with its id, type, namespace and date. Search before answering anything that may depend on what was said earlier.")
  ("query" :text :required t
   :doc "what to find; the question's own words find more than a single keyword")
  ("limit" :integer :default 10
   :doc "how many to answer at most")
  ("namespace" :string :default ""
   :doc "only facts kept in this namespace; empty for every namespace")
  ("memory_type" :string :default "" :options '("" "preference" "fact" "context" "entity" "relationship")
   :doc "only facts of this type; empty for every type"))

(nlk:define-contract memos-context-pack
  (:doc "The stored memories nearest a question, packed into a token budget and ready to read before answering: each line carries its id, type, words and date, and the answer says how many more did not fit.")
  ("query" :text :required t
   :doc "what the answer must be about")
  ("tokens" :integer :default 2000
   :doc "the budget in tokens, about four characters each; 2000 suits most answers")
  ("namespace" :string :default ""
   :doc "only facts kept in this namespace; empty for every namespace"))

(nlk:define-contract memos-supersede
  (:doc "Correct a stored fact: keep the corrected text as a new memory and mark the old one superseded, so a later search answers the correction and the history still reads the old wording.")
  ("memory_id" :text :required t
   :doc "the id of the fact that is now wrong, as a search line carries it")
  ("text" :text :required t
   :doc "the corrected fact, whole, as it should read from now on"))

(nlk:define-contract memos-forget
  (:doc "Forget a stored memory by its id: it stops answering a search and a pack, and stays in the file's history. Prefer memos_supersede when the fact merely changed.")
  ("memory_id" :text :required t
   :doc "the id a search or a pack line carries"))

(nlk:define-contract memos-history
  (:doc "How a stored fact came to read as it does: its text, every correction after it, and whether it was forgotten.")
  ("memory_id" :text :required t
   :doc "the id a search or a pack line carries"))

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the model's one-line repair:
a tool call that refuses is a result, not a crashed turn."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: memos: ~a" condition)))))

(defun given (text)
  "TEXT as a non-empty string, or NIL: an empty member means the call named none."
  (let ((text (nlk:trimmed (princ-to-string (or text "")))))
    (and (plusp (length text)) text)))

(defun tool-store (request)
  (answering
    (store (memos-store-text request)
           :type (memos-store-memory-type request)
           :namespace (memos-store-namespace request)
           :tags (memos-store-tags request))))

(defun tool-search (request)
  (answering
    (recall (memos-search-query request)
            :limit (memos-search-limit request)
            :namespace (given (memos-search-namespace request))
            :memory-type (given (memos-search-memory-type request)))))

(defun tool-context-pack (request)
  (answering
    (context-pack (memos-context-pack-query request)
                  :tokens (memos-context-pack-tokens request)
                  :namespace (given (memos-context-pack-namespace request)))))

(defun tool-supersede (request)
  (answering
    (supersede-fact (memos-supersede-memory-id request)
                    (memos-supersede-text request))))

(defun tool-forget (request)
  (answering (forget (memos-forget-memory-id request))))

(defun tool-history (request)
  (answering (history (memos-history-memory-id request))))

;;; --- the cell -----------------------------------------------------------------

(nle:define-cell memos
  (:section ("memos")
    (:guide "nothing here is required: the facts live in a file under the home and the tools keep and find them; standing is how many characters of the newest facts ride every prompt, 0 for none")
    ("file" :path
     :doc "where the facts are kept: <home>/memos/memories.jsonl unless set")
    ("limit" :integer :default 10 :min 1
     :doc "how many memories one search answers unless the call says otherwise")
    ("standing" :integer :default 0 :min 0
     :doc "characters of the newest facts put in every prompt; 0 for none, so only the tools recall them"))
  (:help :memos "memos: memos_store keeps a durable fact, memos_search and memos_context_pack find it, memos_supersede corrects it" +primer+)
  (:command "memos" 'run-command
            :description "Recall from this machine's memory, or remember one fact"
            :argument-hint "QUERY | store TEXT | recent")
  (:tool "memos_store" (nlk:contract 'memos-store) 'tool-store)
  (:tool "memos_search" (nlk:contract 'memos-search) 'tool-search)
  (:tool "memos_context_pack" (nlk:contract 'memos-context-pack) 'tool-context-pack)
  (:tool "memos_supersede" (nlk:contract 'memos-supersede) 'tool-supersede)
  (:tool "memos_forget" (nlk:contract 'memos-forget) 'tool-forget)
  (:tool "memos_history" (nlk:contract 'memos-history) 'tool-history)
  (:hook 'nle::read-harness-sections #'add-standing)
  (:start (lambda ()
            ;; The directory is made now, so a first failure names a path
            ;; rather than surfacing at the first store.
            (ensure-directories-exist (store-path)))))
