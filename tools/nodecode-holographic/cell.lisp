;;;; cell.lisp --- the section, the two tools, /holographic, the round.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Holographic memory is a fact store that stays on this machine: one file
;;;; of Lisp data, no key, no network, no model call. What it keeps is short
;;;; standalone sentences the person said or the model concluded, each with
;;;; the words someone would ask for it by, a trust score and its dates. A
;;;; change keeps the text it replaced, so 'what did it say before' has an
;;;; answer, and fact_feedback moves a fact up or down by how it did.
;;;;
;;;; Two halves, as the original provider has them:
;;;;   the two tools   fact_store (add, search, list, history, change, end,
;;;;                    forget, stats) and fact_feedback (helpful, unhelpful,
;;;;                    confirmed), the same verbs the plugin gives a model.
;;;;   the round        at each model request the turn's own input is
;;;;                    searched and the facts it recalls ride that request
;;;;                    under '# Holographic memory', so a fact learned in one
;;;;                    session is in front of the model in the next.

(in-package #:nodecode-holographic)

;; The condition every refusal is (HOLOGRAPHIC-ERROR), FAIL, the settings the
;; cell runs on and RUNNING-SETTINGS, which SETTING reads: NLE:DEFINE-CELL
;; below fills and clears them.
(nlk:define-peripheral holographic :not-running t)

;;; --- the manual ---------------------------------------------------------------

(defparameter +primer+
  "Holographic memory is long-term facts kept on this machine: one file of
Lisp data, no key, no network. One fact is one short standalone sentence -
what the person said, or what you concluded - with the words someone would
ask for it by, a trust score and the dates it held.

The model has two tools:
  fact_store {action, content, query, fact_id, keywords, category, origin,
              valid_from, valid_to, as_of, min_trust, limit}
      add {content, origin, keywords, valid_from}   store one sentence;
          the answer names the facts it looks like, so a conflict is seen
      search {query, as_of}   facts by words; ask again with a synonym
      list {as_of} - history {fact_id} - stats
      change {fact_id, content, keywords, valid_from}   it changed
      end {fact_id, valid_to}   no longer true, nothing replaces it
      forget {fact_id}   the person asks to forget it: gone, history included
  fact_feedback {action, fact_id}   helpful - unhelpful - confirmed

From eval the same verbs are Lisp: (holographic:recall \"query\"),
(holographic:remember \"one sentence\" :keywords '(\"synonym\")),
(holographic:forget 3), (holographic:facts-text (holographic:recall \"q\")).
Recall is on: each turn's own message is searched and the facts it recalls
ride that request under '# Holographic memory', each by its id. Pass that id
to fact_feedback after using a fact. /holographic shows the store and asks
by hand. A refusal is ERROR: HOLOGRAPHIC-ERROR and says what was wrong."
  "What (help :holographic) answers while the cell runs.")

;;; --- the two tools ------------------------------------------------------------

(nlk:define-contract fact-store-call
  (:doc "Long-term facts kept on this machine, by words and by date. One fact is one short standalone sentence. search finds them; add stores one; change, end and forget keep the history straight when what was true stops being true.")
  ("action" :text :required t
            :options '("add" "search" "list" "history" "change" "end" "forget" "stats")
            :doc "what to do: add (store one sentence), search (find by words), list, history (a fact's past versions), change (it changed), end (no longer true, nothing replaces it), forget (delete for good, history included), stats")
  ("content" :string
             :doc "add/change: one short standalone sentence, the fact itself")
  ("query" :string
           :doc "search: the words to look for; a synonym or a translation when nothing fits")
  ("fact_id" :integer
             :doc "the id a recall line shows as #N: history, change, end and forget take one")
  ("keywords" :array :of :string
              :doc "add/change: 3-8 words someone might ask for this fact by that are NOT in its text - synonyms, other word forms, other languages. At most 12, each up to 40 characters")
  ("category" :text
              :options '("user_pref" "project" "tool" "general")
              :doc "what kind of fact it is")
  ("origin" :text :options '("said" "confirmed" "inferred")
            :doc "who stands behind it: said (the person said it), confirmed (your conclusion, and they agreed), inferred (your own conclusion, nobody confirmed it)")
  ("valid_from" :string :doc "add/change: the date (YYYY-MM-DD) it started to hold, only when the message gives one")
  ("valid_to" :string :doc "end: the date (YYYY-MM-DD) from which it no longer held, only when the message gives one")
  ("as_of" :string :doc "search/list: a date (YYYY-MM-DD); what held then")
  ("min_trust" :string :doc "search: the lowest trust to return, as a number from 0 to 1")
  ("limit" :integer :doc "the most facts to answer with (default 10, at most 100)"))

(nlk:define-contract fact-feedback-call
  (:doc "Say how a fact did. helpful if it was accurate, unhelpful if it was outdated, confirmed when the person said it again or agreed - a conclusion of yours then becomes confirmed.")
  ("action" :text :required t :options '("helpful" "unhelpful" "confirmed")
            :doc "how the fact did")
  ("fact_id" :integer :required t :doc "the id the fact was recalled by"))

;;; --- the store's own verbs, for the model and for eval -------------------------

(defun limit-of (limit &key (default 10))
  "LIMIT bounded to 1..100, DEFAULT when it is absent."
  (max 1 (min 100 (or limit default))))

(defun status-line ()
  "One line: how many facts, where they live, and what recall does."
  (ensure-store)
  (let ((current (count-if (lambda (fact) (equal (fact-status fact) "current")) *facts*)))
    (format nil "holographic: ~d facts (~d current) in ~a; recall ~a, up to ~d a turn"
            (length *facts*) current (store-path) (setting :recall) (setting :recall-facts))))

(defun guarded (body)
  "Run BODY, a function answering the tool's string; a refusal becomes
ERROR: HOLOGRAPHIC-ERROR: ..., so the model reads what went wrong."
  (handler-case (funcall body)
    (holographic-error (condition)
      (nle:failure (format nil "ERROR: HOLOGRAPHIC-ERROR: ~a" condition)))
    (error (condition)
      (nle:failure (format nil "ERROR: HOLOGRAPHIC-ERROR: ~a" condition)))))

(defun add-report (call)
  "Store the call's fact and answer with the facts it looks like."
  (let ((fact (remember (fact-store-call-content call)
                        :keywords (coerce (or (fact-store-call-keywords call) #()) 'list)
                        :category (fact-store-call-category call)
                        :origin (fact-store-call-origin call)
                        :valid-from (fact-store-call-valid-from call))))
    (with-output-to-string (out)
      (format out "stored #~d: ~a" (fact-id fact) (fact-content fact))
      (when (plusp (length (fact-keywords fact)))
        (format out "~%keywords: ~{~a~^, ~}" (fact-keywords fact)))
      (let ((similar (similar-facts fact)))
        (when similar
          (format out "~%similar, check for a conflict:~%~a" (facts-text similar)))))))

(defun list-facts (&key as-of (limit 50))
  "The facts the store holds, newest first, or AS-OF the ones that held then."
  (ensure-store)
  (let ((held (loop for fact in (reverse *facts*)
                    when (held-at-p fact as-of)
                      collect fact into kept
                    finally (return kept))))
    (if held
        (format nil "~{~a~^~%~}" (mapcar #'fact-line (subseq held 0 (min limit (length held)))))
        "The store holds no facts yet.")))

(defun stats-text ()
  "What the store holds, in a few lines."
  (ensure-store)
  (let ((by-category (make-hash-table :test #'equal)))
    (dolist (fact *facts*)
      (incf (gethash (fact-category fact) by-category 0)))
    (format nil "~d facts, ~d current, ~d ended~%file: ~a~%trust: ~,2f average (recall keeps ~a and up)~%categories: ~{~a~^, ~}"
            (length *facts*)
            (count-if (lambda (fact) (equal (fact-status fact) "current")) *facts*)
            (count-if (lambda (fact) (equal (fact-status fact) "ended")) *facts*)
            (store-path)
            (if *facts* (/ (reduce #'+ *facts* :key #'fact-trust) (length *facts*)) 0)
            (setting :min-trust)
            (loop for category being the hash-keys of by-category using (hash-value count)
                  collect (format nil "~a ~d" category count)))))

(defun fact-store-tool (call)
  "The fact_store tool's body: one action of the memory, as a string."
  (guarded
   (lambda ()
     (let ((action (fact-store-call-action call))
           (limit (limit-of (fact-store-call-limit call))))
       (cond
         ((equal action "add") (add-report call))
         ((equal action "search")
          (let ((query (fact-store-call-query call)))
            (when (zerop (length (nlk:trimmed (or query ""))))
              (refuse "search needs a query"))
            (facts-text (recall query
                                 :limit limit
                                 :min-trust (number-or (fact-store-call-min-trust call) nil)
                                 :as-of (fact-store-call-as-of call)))))
         ((equal action "list") (list-facts :as-of (fact-store-call-as-of call) :limit limit))
         ((equal action "history")
          (format nil "~{~a~^~%~}" (history-of (fact-store-call-fact-id call))))
         ((equal action "change")
          (let ((fact (change-fact (fact-store-call-fact-id call)
                                   (fact-store-call-content call)
                                   :keywords (let ((given (fact-store-call-keywords call)))
                                               (and given (coerce given 'list)))
                                   :valid-from (fact-store-call-valid-from call)
                                   :origin (fact-store-call-origin call))))
            (format nil "changed: ~a" (fact-line fact))))
         ((equal action "end")
          (format nil "ended: ~a" (fact-line (end-fact (fact-store-call-fact-id call)
                                                        :valid-to (fact-store-call-valid-to call)))))
         ((equal action "forget")
          (format nil "forgot #~d: ~a" (fact-store-call-fact-id call)
                  (forget (fact-store-call-fact-id call))))
         ((equal action "stats") (stats-text))
         (t (refuse "~a is not one of fact_store's actions" action)))))))

(defun fact-feedback-tool (call)
  "The fact_feedback tool's body: trust moves, and the number is said back."
  (guarded
   (lambda ()
     (let ((fact (feedback (fact-feedback-call-fact-id call)
                           (fact-feedback-call-action call))))
       (format nil "#~d ~a: trust now ~,2f~@[; origin ~a~]"
               (fact-id fact) (fact-feedback-call-action call) (fact-trust fact)
               (let ((origin (fact-origin fact)))
                 (and (not (equal origin "unknown")) origin)))))))

;;; --- the round: recall before the request --------------------------------------

(defun turn-input (turn)
  "The words TURN was admitted with, or NIL."
  (nlk:json-value (nlk:events :turn-id (nlk:durable-turn-turn-id turn)
                               :kind nlk::+kind-turn-started+ :as :payload)
                  :string "input"))

(defun memory-block (query)
  "The block the facts QUERY recalls ride the request as, or NIL when it
recalls nothing. Each line is one fact as FACT-LINE draws it."
  (let ((facts (recall query :limit (ignore-errors (setting :recall-facts)))))
    (when facts
      (let ((budget (or (ignore-errors (setting :recall-max-chars)) 800))
            (header "# Holographic memory (long-term facts kept on this machine; #N is the id fact_feedback takes)"))
        (with-output-to-string (out)
          (format out "~a~%" header)
          (let ((used (length header)))
            (dolist (fact facts)
              (let ((line (fact-line fact)))
                (when (<= (+ used (length line) 3) budget)
                  (format out "- ~a~%" line)
                  (incf used (+ (length line) 3)))))))))))

(defun on-round (op)
  "The :round point: the turn is about to ask a model. The turn's own input
is searched and the facts it recalls are put in that very request."
  (let* ((session (getf op :session-id))
         (turn (getf op :turn))
         (input (and session turn (nlk:store-open-p) (turn-input turn))))
    (when (and input
               (string-equal "on" (setting :recall))
               (plusp (length (nlk:trimmed input))))
      (nlk:when-let (block (memory-block input))
        (nlk:record-turn-context-prompt turn :text block)))))

;;; --- /holographic ---------------------------------------------------------------

(defun split-verb (args)
  "ARGS as (values HEAD REST): the first word, and the trimmed remainder."
  (let* ((text (nlk:trimmed (or args "")))
         (space (position #\Space text)))
    (values (if space (subseq text 0 space) text)
            (if space (nlk:trimmed (subseq text (1+ space))) ""))))

(defun panel (line)
  "The dialog bare /holographic answers."
  (nle:list-dialog
   :title "Holographic memory" :summary line
   :rows (list (nle:ask-row "Search the facts" "Query" "/holographic search"
                             :placeholder "words to look for" :verb "search")
               (nle:ask-row "Remember something" "Fact" "/holographic add"
                             :placeholder "one short standalone sentence" :verb "add"))))

(defun run-slash (args session-id)
  "One /holographic invocation => the answer's text, and typed alone the
dialog of what it does."
  (declare (ignore session-id))
  (multiple-value-bind (head rest) (split-verb args)
    (cond
      ((zerop (length head))
       (let ((line (status-line)))
         (values line (panel line))))
      ((string-equal head "status") (values (status-line) (panel (status-line))))
      ((string-equal head "search")
       (values (if (zerop (length rest))
                   "holographic: search for what? /holographic search QUERY"
                   (facts-text (recall rest)))
               (panel (status-line))))
      ((string-equal head "add")
       (values (if (zerop (length rest))
                   "holographic: add what? /holographic add ONE SENTENCE"
                   (format nil "stored #~d" (fact-id (remember rest :origin "said"))))
               (panel (status-line))))
      (t (values (format nil "holographic: ~a is not one of its choices (search, add, status)" head)
                 (panel (status-line)))))))

;;; --- start ----------------------------------------------------------------------

(defun start ()
  "A start reads the file the section names; a stop leaves nothing loaded."
  (setf *loaded-from* nil *facts* '() *next-id* 1 *path-override* nil)
  (ensure-store)
  (nle:on-stop (lambda ()
                 (setf *loaded-from* nil *facts* '() *next-id* 1))))

;;; --- the one declaration ---------------------------------------------------------

(nle:define-cell holographic
  (:section ("holographic")
    (:guide "nothing is required: the facts live in one file under this home, and recall is on. db_path names another file; recall off stops the injection; min_trust is the lowest trust a recalled fact may have")
    ("db_path" :string
               :doc "the file the facts live in; empty keeps one under this home")
    ("default_trust" :string :default "0.5"
                     :doc "the trust a new fact starts with, as a number from 0 to 1")
    ("min_trust" :string :default "0.3"
                 :doc "facts below this trust are not recalled, as a number from 0 to 1")
    ("recall" :choice :options '("on" "off") :default "on"
              :doc "search each turn's message and put the facts it recalls into that request")
    ("recall_facts" :integer :default 5 :min 1
                    :doc "the most facts one turn's recall injects")
    ("recall_max_chars" :integer :default 800 :min 100
                        :doc "the most characters the injected block may take"))
  (:tool "fact_store" (nlk:contract 'fact-store-call) 'fact-store-tool)
  (:tool "fact_feedback" (nlk:contract 'fact-feedback-call) 'fact-feedback-tool)
  (:command "holographic" 'run-slash
            :description "Facts kept on this machine: search, add, status"
            :argument-hint "[search|add] TEXT")
  (:help :holographic "holographic: facts kept on this machine - fact_store, fact_feedback, holographic:recall" +primer+)
  (:hook :round (nle:observer #'on-round "holographic"))
  (:start #'start))
