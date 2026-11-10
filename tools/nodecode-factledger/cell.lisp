;;;; cell.lisp --- the tool, /factledger, the manual, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The ledger's verbs live in ledger.lisp; this file is the surface the
;;;; model and the operator reach them by: one tool, one action a verb, whose
;;;; contract is the original's nine tools collapsed into one payload (its
;;;; members are theirs: scope, subject, predicate, value, source,
;;;; confidence, valid_from, as_of, term, scopes, budget, days,
;;;; include_history); /factledger for the operator; (help :factledger) for
;;;; the manual; and the section the ledger's own settings come from.
;;;;
;;;; The original registered a skill that tells the model when to write and
;;;; when to look up. A Nodecode request carries every manual topic's one
;;;; line, so the summary below is where that rule lives, with the full text
;;;; behind (help :factledger) -- nothing is injected into every round.

(in-package #:nodecode-factledger)

(defparameter +primer+
  "Factledger: one durable ledger of typed facts. A fact is a row of scope
(the namespace, e.g. market), subject (the entity, e.g. competitor.x),
predicate (the property, e.g. price), value, and source -- where the value
comes from, in its own words, which every fact must carry. For one
(scope, subject, predicate) exactly one row holds: recording a new value
closes the row it replaces (valid_to, superseded_by) instead of deleting it,
so two contradictory versions can never stand as equals and the history is
the audit trail. The file is TSV: read it, diff it, grep it.
The tool is `factledger', one action a verb:
  action=add        scope, subject, predicate, value, source (required),
                    and optionally confidence (stated, derived or assumed;
                    derived is computed, assumed a guess) and valid_from
                    (an ISO date, today by default). A value identical to
                    the one already holding changes nothing.
  action=show       the truth holding today, or as_of an ISO date, for a
                    scope and/or subject.
  action=search     rows whose subject, value, predicate or source carries
                    term; scope narrows it.
  action=history    every row for a subject, oldest first: what changed and
                    when, each with its source.
  action=compile    the truth holding today as compact lines inside budget
                    characters, for a prompt; scopes narrows it.
  action=stale      rows holding today whose valid_from is days old or
                    older: what wants re-checking.
  action=export     the rows as JSON; include_history adds the closed ones.
  action=check      the ledger's integrity: an empty problems list is
                    healthy.
  action=stats      the totals: rows, active, closed, scopes.
Never quote a stored value without its valid_from: a number without its
date is a wrong number. From eval the same verbs are
(factledger:add-fact ...), active-facts, search-facts, fact-history,
compile-view, stale-facts, export-facts, check-ledger and ledger-stats; the
operator's /factledger reads the same ledger."
  "What (help :factledger) answers while the cell runs.")

;;; --- the tool -------------------------------------------------------------------

(nlk:define-contract factledger-call
  (:doc "A durable ledger of typed facts: for one scope, subject and predicate exactly one row holds, and recording a new value closes the row it replaces instead of deleting it, so the history stays. action=add records a fact and needs source (where it comes from); show reads the truth holding today or as_of; search finds a term; history is what changed and when; compile a compact view within budget characters; stale the facts older than days; export JSON; check the ledger's integrity; stats the totals.")
  ("action" :text :required t
            :options '("add" "show" "search" "history" "compile" "stale" "export" "check" "stats")
            :doc "what to do")
  ("scope" :string :doc "the namespace a fact belongs to, e.g. market")
  ("subject" :string :doc "the entity the fact is about, e.g. competitor.x")
  ("predicate" :string :doc "the property, e.g. price or status")
  ("value" :string :doc "add: the fact's value, verbatim")
  ("source" :string :doc "add: where the value comes from, verbatim -- required")
  ("confidence" :text :options '("stated" "derived" "assumed") :default "stated"
                :doc "how firm the value is: stated is first-party, derived computed, assumed a guess")
  ("valid_from" :string :doc "add: the ISO date (YYYY-MM-DD) the value holds from; today by default")
  ("as_of" :string :doc "show: the ISO date to read the truth at; today by default")
  ("term" :string :doc "search: the term to look for")
  ("scopes" :string :doc "compile, stale and export: comma-separated scopes to keep")
  ("budget" :integer :doc "compile: the view's character budget")
  ("days" :integer :doc "stale: how many days old an active fact must be")
  ("include_history" :boolean :doc "export: include the closed rows too"))

(defun row-json (row)
  "ROW as the JSON object every action answers a row in."
  (apply #'nlk:make-json-object
         (loop for name in +fields+
               append (list name (row-field row name)))))

(defun rows-json (rows)
  (coerce (mapcar #'row-json rows) 'vector))

(defun ok-json (data)
  "DATA as the answer every action gives: {\"ok\": true, \"data\": DATA}."
  (nlk:encode-json-object (nlk:make-json-object "ok" :true "data" data)))

(defun added-json (result)
  "The add action's data: what happened, which row, and which it closed."
  (nlk:make-json-object
   "status" (getf result :status)
   "id" (getf result :id)
   "closed" (coerce (getf result :closed) 'vector)
   "row" (row-json (getf result :row))))

(defun scope-list (text)
  "TEXT's comma-separated scopes, or NIL when it names none."
  (when text
    (let ((scopes (remove-if (lambda (scope) (zerop (length scope)))
                              (mapcar (lambda (scope) (string-trim '(#\Space #\Tab) scope))
                                      (nlk:split-words (substitute #\Space #\, text))))))
      scopes)))

(defun tool-answer (function)
  "FUNCTION's answer, or its refusal as the ERROR line the model repairs from."
  (handler-case (funcall function)
    (factledger-error (condition) (nle:failure (format nil "ERROR: factledger: ~a" condition)))
    (error (condition) (nle:failure (format nil "ERROR: factledger: ~a" condition)))))

(defun run-tool (call)
  "The factledger tool: one action on the ledger."
  (tool-answer
   (lambda ()
     (let ((action (factledger-call-action call)))
       (cond ((string= action "add")
              (ok-json (added-json
                        (add-fact (or (factledger-call-scope call) "")
                                  (or (factledger-call-subject call) "")
                                  (or (factledger-call-predicate call) "")
                                  (or (factledger-call-value call) "")
                                  (or (factledger-call-source call) "")
                                  :confidence (factledger-call-confidence call)
                                  :valid-from (factledger-call-valid-from call)))))
             ((string= action "show")
              (ok-json (rows-json (active-facts :scope (factledger-call-scope call)
                                                 :subject (factledger-call-subject call)
                                                 :as-of (factledger-call-as-of call)))))
             ((string= action "search")
              (ok-json (rows-json (search-facts (or (factledger-call-term call)
                                                     (fail "search needs term: what to look for"))
                                                 :scope (factledger-call-scope call)))))
             ((string= action "history")
              (ok-json (rows-json (fact-history (or (factledger-call-subject call)
                                                     (fail "history needs subject: whose history"))
                                                 :scope (factledger-call-scope call)
                                                 :predicate (factledger-call-predicate call)))))
             ((string= action "compile")
              (ok-json (nlk:make-json-object
                        "view" (compile-view :scopes (scope-list (factledger-call-scopes call))
                                              :budget (factledger-call-budget call)))))
             ((string= action "stale")
              (ok-json (coerce (loop for pair in (stale-facts :days (factledger-call-days call)
                                                               :scopes (scope-list (factledger-call-scopes call)))
                                      collect (nlk:make-json-object "row" (row-json (car pair))
                                                                     "age_days" (cdr pair)))
                                'vector)))
             ((string= action "export")
              (ok-json (rows-json (export-facts :scopes (scope-list (factledger-call-scopes call))
                                                 :include-history (factledger-call-include-history call)))))
             ((string= action "check")
              (let ((problems (check-ledger)))
                (ok-json (nlk:make-json-object "healthy" (if problems :false :true)
                                                "problems" (coerce problems 'vector)))))
             ((string= action "stats")
              (let ((stats (ledger-stats)))
                (ok-json (nlk:make-json-object "total" (getf stats :total)
                                                "active" (getf stats :active)
                                                "closed" (getf stats :closed)
                                                "scopes" (coerce (getf stats :scopes) 'vector)))))
             (t (fail "unknown action ~s: add, show, search, history, compile, stale, export, check or stats"
                      action)))))))

;;; --- /factledger ------------------------------------------------------------------

(defun fact-line (row)
  "One row as a shell reads it."
  (format nil "~a  ~a/~a . ~a = ~a  [~a] ~a~@[ -> ~a~]  <- ~a"
          (row-field row "id") (row-field row "scope") (row-field row "subject")
          (row-field row "predicate") (row-field row "value")
          (row-field row "confidence") (row-field row "valid_from")
          (let ((closed (row-field row "valid_to")))
            (and (plusp (length closed)) closed))
          (row-field row "source")))

(defun facts-text (rows &optional (empty "none"))
  "ROWS as lines, or EMPTY when there are none."
  (if rows (format nil "~{~a~^~%~}" (mapcar #'fact-line rows)) empty))

(defun summary ()
  "The ledger as /factledger typed alone answers it: its totals and its scopes."
  (let* ((stats (ledger-stats))
         (scopes (getf stats :scopes)))
    (format nil "factledger: ~d fact~:p, ~d active, ~d closed~@[, in ~{~a~^, ~}~]; the ledger is ~a.~%~
                 /factledger show, search TERM, history SUBJECT, stale, check, stats, compile, ~
                 or add scope | subject | predicate | value | source."
            (getf stats :total) (getf stats :active) (getf stats :closed)
            (and scopes scopes) (ledger-file))))

(defun panel ()
  "What bare /factledger carries: the ledger's verbs as rows."
  (nle:list-dialog
   :title "Factledger" :summary "The facts this agent keeps"
   :rows (list (nle:run-row "What holds today" "/factledger show"
                             :detail "every active fact, with its source")
               (nle:run-row "What needs re-checking" "/factledger stale"
                             :detail "active facts older than stale_days")
               (nle:run-row "Integrity check" "/factledger check"
                             :detail "one truth a key, no open row under a closed one")
               (nle:run-row "Totals" "/factledger stats" :detail "rows, active, closed, scopes")
               (nle:ask-row "Record a fact" "Fact" "/factledger add" :verb "add"
                            :placeholder "scope | subject | predicate | value | source"
                            :detail "a fact is not written without its source"))))

(defun add-parts (text)
  "TEXT as five parts split by |: scope, subject, predicate, value and source."
  (let ((parts (mapcar (lambda (part) (string-trim '(#\Space #\Tab) part))
                        (loop with start = 0
                              for at = (position #\| text :start start)
                              collect (subseq text start at)
                              while at
                              do (setf start (1+ at))))))
    (unless (= 5 (length parts))
      (fail "a fact is five parts split by |: scope | subject | predicate | value | source"))
    (values-list parts)))

(defun split-head (text)
  "TEXT as (VALUES HEAD REST): its first word, downcased, and what follows it."
  (let ((space (position-if (lambda (character) (member character '(#\Space #\Tab)))
                            text)))
    (values (string-downcase (subseq text 0 space))
            (if space (nlk:trimmed (subseq text space)) ""))))

(defun run-command (args session-id)
  "Bare /factledger: the ledger's totals and a panel. `show [SCOPE [SUBJECT]]',
`search TERM', `history SUBJECT', `stale', `check', `stats', `compile' and
`add scope | subject | predicate | value | source' answer the rest."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (if (zerop (length text))
        (values (summary) (panel))
        (multiple-value-bind (head rest) (split-head text)
          (let ((words (nlk:split-words rest)))
            (cond ((string= head "show")
                   (format nil "factledger: ~a"
                           (facts-text (active-facts :scope (first words)
                                                      :subject (second words))
                                       "no fact holds there")))
                  ((string= head "search")
                   (format nil "factledger: ~a"
                           (facts-text (search-facts rest) "no row carries that term")))
                  ((string= head "history")
                   (format nil "factledger: ~a"
                           (facts-text (fact-history (or (first words)
                                                         (fail "history needs a subject")))
                                       "nothing is recorded for that subject")))
                  ((string= head "stale")
                   (format nil "factledger: ~a"
                           (facts-text (mapcar #'car (stale-facts))
                                       "no active fact is older than the setting")))
                  ((string= head "check")
                   (let ((problems (check-ledger)))
                     (if problems
                         (format nil "factledger: ~d problem~:p~%~{~a~^~%~}"
                                 (length problems) problems)
                         "factledger: healthy: one truth a key, every row dated, every successor there")))
                  ((string= head "stats")
                   (let ((stats (ledger-stats)))
                     (format nil "factledger: ~d fact~:p, ~d active, ~d closed; scopes: ~{~a~^, ~}"
                             (getf stats :total) (getf stats :active)
                             (getf stats :closed) (getf stats :scopes))))
                  ((string= head "compile")
                   (compile-view :scopes (and (plusp (length rest)) (scope-list rest))))
                  ((string= head "add")
                   (multiple-value-bind (scope subject predicate value source)
                       (add-parts rest)
                     (let ((result (add-fact scope subject predicate value source)))
                       (ecase (intern (string-upcase (getf result :status)) :keyword)
                         (:unchanged (format nil "factledger: nothing changed (~a holds these words)"
                                             (getf result :id)))
                         (:added (format nil "factledger: recorded ~a" (getf result :id)))
                         (:closed (format nil "factledger: recorded ~a, closing ~{~a~^, ~}"
                                           (getf result :id) (getf result :closed)))))))
                  (t (fail "~s names no /factledger verb: show, search, history, stale, check, stats, compile or add"
                           head))))))))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell factledger
  (:section ("factledger")
    (:guide "nothing here is required: the ledger is a TSV file under the home; stale_days is how old a fact may be before stale names it; next step: /factledger shows what holds")
    ("path" :path :doc "the ledger file, when it is not the one under the home")
    ("aliases" :path :doc "a TSV file mapping other predicate spellings onto one canonical one")
    ("stale_days" :integer :default 30 :min 1
                  :doc "an active fact older than this many days is what stale names")
    ("budget" :integer :default 800 :min 100
              :doc "the compiled view's character budget when a call names none")
    ("order" :choice :options '("confidence" "recency") :default "confidence"
             :doc "which facts a compiled view keeps when the budget runs out"))
  (:help :factledger "factledger: a durable ledger of typed facts, superseded with history - action=add|show|search|history|compile|stale|export|check|stats"
         +primer+)
  (:command "factledger" 'run-command
            :description "The fact ledger: what holds today, what changed, and what needs re-checking"
            :argument-hint "show · search TERM · add scope | subject | predicate | value | source")
  (:tool "factledger" (nlk:contract 'factledger-call) 'run-tool))
