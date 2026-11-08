;;;; cell.lisp --- the section, the three tools, /hindsight, the round.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Hindsight is a memory service: it extracts facts from what it is given,
;;;; resolves entities into a knowledge graph, and answers recall and reflect
;;;; over them. This cell is the client half of that, against the same API
;;;; the original Hermes plugin talks to through hindsight-client.
;;;;
;;;; Three tools, the same three the plugin gives a model:
;;;;   hindsight_retain   store one memory, with a context label and tags
;;;;   hindsight_recall   the memories a query recalls, numbered
;;;;   hindsight_reflect  a reasoned answer synthesized across the bank
;;;;
;;;; Two automatic halves, off in one switch each:
;;;;   auto_recall  at every model request, the current turn's input is
;;;;                recalled and the memories ride that request as a
;;;;                <system-reminder>; one dial per distinct query per
;;;;                conversation (the plugin's recall_sync shape).
;;;;   auto_retain  the operator's own words, once per turn, into the bank.
;;;; A turn's half of the exchange is the operator's; the assistant's half
;;;; is what hindsight_retain is for.
;;;;
;;;; Nothing here runs while the cell is off, and a section without a key
;;;; answers every verb with one sentence naming what to set.

(in-package #:nodecode-hindsight)

(defparameter +preamble+
  "# Hindsight memory (persistent, across sessions)
Use this to answer questions about the operator and earlier sessions; do not
call a tool to look up what is already here."
  "The first lines of the block auto-recall injects.")

(defparameter *recall-reuse-seconds* 600
  "How long one conversation's identical query reuses the block it already
injected, so a turn's later rounds and a prompt re-sent do not dial again.")

(defvar *seen* (make-hash-table :test #'equal)
  "Session id -> (QUERY . UNIVERSAL-TIME) it last recalled, while the cell runs.")

(defvar *retained* (make-hash-table :test #'equal)
  "Turn id -> T once auto-retain has written it, so a turn is stored once.")

(defvar *recalled* 0
  "How many memories this cell has injected since it started, for /hindsight.")

;;; --- the manual ---------------------------------------------------------------

(defparameter +primer+
  "Hindsight is long-term memory across sessions: what it stores, it extracts
into facts and a knowledge graph, and it answers recall and reflect over them,
for every session that uses the same bank.

(hindsight:recall \"query\")    the memories a query recalls, numbered.
(hindsight:reflect \"question\") a reasoned answer synthesized across the whole
    bank -- for \"what have we settled about X\", not for one fact.
(hindsight:remember \"text\" &key context tags occurred-at)   store one memory.
The model also has the same three as tools: hindsight_recall,
hindsight_reflect and hindsight_retain. Forget them for a look-up that the
injected memory block already answers.

Auto-recall is on: at each model request the turn's own input is recalled and
the best memories ride that request under `# Hindsight memory'. Auto-retain is
on too, writing the operator's words once per turn. /hindsight shows the bank,
the endpoint and the switches; /hindsight recall QUERY, /hindsight reflect
QUESTION and /hindsight retain TEXT ask by hand. A refusal is
ERROR: HINDSIGHT-ERROR and says what to set."
  "What (help :hindsight) answers while the cell runs.")

;;; --- the three tools ----------------------------------------------------------

(nlk:define-contract retain-call
  (:doc "Store information in long-term memory. Hindsight extracts structured facts, resolves entities and indexes them for retrieval in later sessions.")
  ("content" :text :required t
             :doc "the information to store, in full sentences")
  ("context" :text
             :doc "a short label: 'operator preference', 'project decision', 'build setup'")
  ("tags" :array :of :string
          :doc "tags to file this memory under, merged with the section's own")
  ("occurred_at" :text
                 :doc "when the remembered event happened, ISO-8601 ('2026-08-20' or '2026-08-20T14:30:00Z') when the memory is about a time; leave it out for a timeless fact"))

(nlk:define-contract recall-call
  (:doc "Search long-term memory. Answers memories ranked by relevance, from semantic search, keyword matching, entity-graph traversal and reranking.")
  ("query" :text :required t :doc "what to search for, as a question or a phrase"))

(nlk:define-contract reflect-call
  (:doc "Synthesize a reasoned answer from long-term memories. Unlike recall, which lists them, this reasons across the whole bank to answer a question about what is known.")
  ("query" :text :required t :doc "the question to reflect on"))

(defun tags-for (extra)
  "The section's default tags, then EXTRA, without repeats."
  (remove-duplicates (append (mapcar #'princ-to-string (setting :tags))
                              (coerce (or extra #()) 'list))
                     :test #'equal))

(defun guarded (body)
  "Run BODY, answering its string; a refusal or a dial that failed becomes
the tool answer ERROR: ..., so the model reads what went wrong and the
transcript draws the call failed."
  (handler-case (funcall body)
    (hindsight-error (condition)
      (setf *last-error* (format nil "~a" condition))
      (nle:failure (format nil "ERROR: HINDSIGHT-ERROR: ~a" condition)))
    (error (condition)
      (setf *last-error* (format nil "~a" condition))
      (nle:failure (format nil "ERROR: HINDSIGHT-ERROR: ~a" condition)))))

(defun remember (content &key context tags occurred-at)
  "Store CONTENT as one memory => a sentence for the caller."
  (guarded
   (lambda ()
     (remember-once content
                    :context (and context (plusp (length context)) context)
                    :tags (tags-for tags)
                    :occurred-at (and occurred-at (plusp (length occurred-at)) occurred-at))
     (setf *last-error* nil)
     "Memory stored successfully.")))

(defun recall (query)
  "The memories QUERY recalls, numbered; one sentence when there are none."
  (guarded
   (lambda ()
     (let ((texts (recall-texts query)))
       (setf *last-error* nil)
       (if texts
           (format nil "~{~{~d. ~a~}~^~%~}" (loop for text in texts for i from 1 collect (list i text)))
           "No relevant memories found.")))))

(defun reflect (query)
  "A reasoned answer to QUERY from the whole bank."
  (guarded
   (lambda ()
     (let ((text (reflect-text query)))
       (setf *last-error* nil)
       (or text "No relevant memories found.")))))

;;; --- the model's side of them ---------------------------------------------------

(defun retain-tool (call)
  (remember (retain-call-content call)
            :context (retain-call-context call)
            :tags (retain-call-tags call)
            :occurred-at (retain-call-occurred-at call)))

(defun recall-tool (call)
  (recall (recall-call-query call)))

(defun reflect-tool (call)
  (reflect (reflect-call-query call)))

;;; --- the round: auto-recall and auto-retain --------------------------------------

(defun turn-key (turn)
  "TURN's id, whether the point handed over the turn or its id."
  (if (stringp turn) turn (nlk:durable-turn-turn-id turn)))

(defun turn-input (turn)
  "The words TURN was admitted with, or NIL."
  (nlk:json-value (nlk:events :turn-id (turn-key turn) :kind nlk::+kind-turn-started+
                               :as :payload)
                  :string "input"))

(defun remembered-block (session query)
  "The block to inject for QUERY in SESSION, dialing only when this
conversation has not recalled this exact query in the last
*RECALL-REUSE-SECONDS*; NIL when there is nothing to inject."
  (let ((last (gethash session *seen*)))
    (when (and last (equal (car last) query)
               (< (- (get-universal-time) (cdr last)) *recall-reuse-seconds*))
      (return-from remembered-block nil))
    (setf (gethash session *seen*) (cons query (get-universal-time)))
    (let ((texts (recall-texts query)))
      (when texts
        (incf *recalled* (length texts))
        (with-output-to-string (out)
          (format out "~a~%~%" +preamble+)
          (dolist (text texts) (format out "- ~a~%" text)))))))

(defun retain-turn (turn session input)
  "Write TURN's operator input to the bank once, asynchronously, tagged with
the session; a failure is remembered for /hindsight and never stops a turn."
  (unless (gethash (turn-key turn) *retained*)
    (when (> (hash-table-count *retained*) 500)
      (clrhash *retained*))
    (setf (gethash (turn-key turn) *retained*) t)
    (handler-case
        (remember-once (format nil "User: ~a" input)
                       :context "conversation between the operator and Nodecode"
                       :tags (tags-for (vector (format nil "session:~a" session)))
                       :document-id (format nil "nodecode-~a" session)
                       :async t)
      (error (condition) (setf *last-error* (format nil "~a" condition))))))

(defun on-round (op)
  "The :round point: TURN about to ask a model. Auto-retain writes the
operator's words, auto-recall injects what they recall -- into this very
request, before the history folds."
  (let* ((session (getf op :session-id))
         (turn (getf op :turn))
         (input (and session turn (nlk:store-open-p) (configured-p) (turn-input turn))))
    (when (and input (plusp (length (nlk:trimmed input))))
      (when (setting :auto-retain) (retain-turn turn session input))
      (nlk:when-let (block (and (setting :auto-recall) (remembered-block session input)))
        (nlk:record-turn-context-prompt turn :text block)))))
