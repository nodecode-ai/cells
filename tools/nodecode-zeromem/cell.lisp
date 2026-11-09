;;;; cell.lisp --- the section, the three tools, /zeromem, the round hook.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What the original Hermes plugin does for a user, kept: every user and
;;;; assistant line of every session is kept verbatim, with its provenance,
;;;; in one local file; a model asks for past words with a tool and pays only
;;;; for what comes back; a recall of past sessions reaches the turn that
;;;; needs it without anyone asking; a session can be forgotten for good.
;;;;
;;;; The tools keep the original's names -- zeromem_recall, zeromem_stats,
;;;; zeromem_forget_session -- because a model that read about the plugin
;;;; will call them so. /zeromem is the operator's own read, and the
;;;; analog of `hermes zeromem import` on a long-lived home.
;;;;
;;;; Config, a top-level key:
;;;;   "zeromem": {"store": "zeromem/", "recall": true, "top_k": 5, "chars": 700}

(in-package #:nodecode-zeromem)

(defparameter +usage+ "/zeromem [stats | import [yes] | forget SESSION]")

(defparameter +manual+
  "zeromem keeps every turn of every session verbatim, with its provenance,
in one local file under the home; nothing leaves this machine and nothing in
the memory path calls a model. Verbs, in eval, every one a string, every
refusal ERROR: ZEROMEM-ERROR:
  (zeromem:recall \"what to look for\" &key top-k)   turns of past sessions, verbatim, each with its session, turn, speaker and day; the current session is never among them, its words are already in the window
  (zeromem:stats)   the counters: turns, sessions, the oldest and newest day kept
  (zeromem:forget-session \"s-XXXX\")   drop one past session for good (its turns, and it is not read in again); the running session refuses
  (zeromem:import-turns &key dry-run)   read the sessions this home already had into the store, or say how many there are
The model's own tools are zeromem_recall, zeromem_stats and
zeromem_forget_session; /zeromem shows the same counters, imports and
forgets. At each turn's first round, up to `top_k' turns of past sessions
that answer its input ride into it as one reminder, each clipped to `chars'
characters, unless `recall' is false."
  "What (help :zeromem) answers while the cell runs.")

(defparameter +stats-schema+
  (list :description "Memory store counters: turns, sessions, the oldest and newest day kept."
        :parameters (nlk:json-object "type" "object" "properties" (nlk:json-object)))
  "zeromem_stats takes no arguments, so its schema is the one written by hand:
an empty properties object.")

(defvar *recalled* (make-hash-table :test #'equal)
  "session id -> the turn a recall was put into, so one turn is told once.")

;;; --- the model's tools -------------------------------------------------------

(nlk:define-contract zeromem-recall
  (:doc "Recall evidence from past conversations: verbatim turns with provenance (session, turn, speaker, day), never summaries. Returns JSON {query, evidence[]}.")
  ("query" :text :required t :doc "what to look for")
  ("top_k" :integer :default 5
           :doc "most evidence turns to answer; the cell's own top_k when absent"))

(nlk:define-contract zeromem-forget-call
  (:doc "Permanently delete one past session from memory: its turns, and it is not read in again. Irreversible; only on explicit user request. Refuses the session that is running now.")
  ("session_id" :text :required t :doc "the session to delete, as a recall answered it"))

(defun active-session ()
  "The session this call is made for: the running turn's, or the one an eval
form runs under, or NIL."
  (or (getf (nle:turn) :session-id) nlk:*scribe-session-id*))

(defun recall-tool (call)
  "zeromem_recall: past sessions' words, each with where and when it was said."
  (let* ((query (zeromem-recall-query call))
         (top-k (or (zeromem-recall-top-k call) (setting :top-k)))
         (evidence (recall-turns (running-store) query :top-k top-k :exclude (active-session))))
    (json-text (list :query query
                     :evidence (map 'vector
                                    (lambda (turn) (json-answer (evidence-json turn)))
                                    evidence)))))

(defun stats-tool (arguments)
  "zeromem_stats: the store's counters."
  (declare (ignore arguments))
  (json-text (store-stats (running-store))))

(defun forget-tool (call)
  "zeromem_forget_session: drop one past session, never the running one."
  (let ((session (zeromem-forget-call-session-id call))
        (active (active-session)))
    (if (and active (equal session active))
        (json-text (list :session_id session :error "refusing to delete the active session"))
        (json-text (list :session_id session
                         :deleted_turns (forget-turns (running-store) session))))))

;;; --- the verbs, for eval and for the command --------------------------------

(defun store-folder ()
  "The folder the store lives in: the section's, or the home's zeromem/."
  (or (setting :store) (nlk:home "zeromem/")))

(defun execute (query top-k)
  "QUERY's evidence in the running store, as the lines a reader is shown."
  (let ((evidence (recall-turns (running-store) query :top-k (or top-k (setting :top-k))
                                                   :exclude (active-session))))
    (if evidence
        (format nil "~{~a~^~%~}" (mapcar (lambda (turn)
                                            (evidence-line turn (setting :chars)))
                                          evidence))
        "no past turn answers that")))

(define-verb recall (query &key top-k)
  "Past sessions' words that answer QUERY, verbatim, one line each: session,
turn, speaker, day, then the words clipped to the section's chars."
  (execute query top-k))

(define-verb stats ()
  "The counters this store holds: turns, sessions, the oldest and newest day."
  (let ((counters (store-stats (running-store))))
    (format nil "zeromem: ~a turns from ~a sessions~@[, ~a to ~a~]"
            (getf counters :turns) (getf counters :sessions)
            (and (plusp (getf counters :turns)) (getf counters :oldest))
            (getf counters :newest))))

(define-verb forget-session (session)
  "Drop SESSION's turns for good and stop reading it in; the running session
refuses. => one line saying how many turns went."
  (let ((session (and session (nlk:trimmed (string session)))))
    (unless (and session (plusp (length session)))
      (fail "forget names a session: (zeromem:forget-session \"s-XXXX\")"))
    (when (equal session (active-session))
      (fail "refusing to delete the active session"))
    (let ((gone (forget-turns (running-store) session)))
      (if (plusp gone)
          (format nil "zeromem: forgot ~a turns of ~a" gone session)
          (format nil "zeromem: no turn of ~a is stored" session)))))

(define-verb import-turns (&key dry-run)
  "Read the sessions this home already had into the store -- the original's
`import` -- or, with DRY-RUN, say how many turns there would be."
  (let* ((store (running-store))
         (sessions (or (ignore-errors (nlk:list-sessions)) '()))
         (turns 0) (count 0))
    (dolist (entry sessions)
      (let ((rows (ingest-session store (getf entry :id) :dry-run dry-run)))
        (when rows (incf count) (incf turns (length rows)))))
    (cond ((zerop turns) "zeromem: nothing new to import")
          (dry-run (format nil "zeromem: would import ~a turns from ~a sessions; ~
                                (zeromem:import-turns :dry-run nil) writes them"
                            turns count))
          (t (format nil "zeromem: imported ~a turns from ~a sessions" turns count)))))

;;; --- /zeromem -----------------------------------------------------------------

(defun show ()
  "One line for /zeromem typed alone."
  (let ((counters (store-stats (running-store))))
    (format nil "zeromem: ~a turns from ~a sessions kept locally~@[, ~a to ~a~]; ~
                 the zeromem_ tools recall them"
            (getf counters :turns) (getf counters :sessions)
            (and (plusp (getf counters :turns)) (getf counters :oldest))
            (getf counters :newest))))

(defun import-command (confirm)
  "One /zeromem import [yes]."
  (let* ((store (running-store))
         (dry-run (not (member confirm '("yes" "y") :test #'string-equal)))
         (sessions (or (ignore-errors (nlk:list-sessions)) '()))
         (turns 0) (count 0))
    (dolist (entry sessions)
      (let ((rows (ingest-session store (getf entry :id) :dry-run dry-run)))
        (when rows (incf count) (incf turns (length rows)))))
    (cond ((zerop turns) "zeromem: nothing new to import")
          (dry-run (format nil "zeromem: would import ~a turns from ~a sessions; /zeromem import yes writes them"
                            turns count))
          (t (format nil "zeromem: imported ~a turns from ~a sessions" turns count)))))

(defun forget-command (session)
  "One /zeromem forget SESSION: never the session that is running now."
  (unless session
    (fail "forget names a session: /zeromem forget s-XXXX"))
  (when (equal session (active-session))
    (fail "refusing to delete the active session"))
  (let ((gone (forget-turns (running-store) session)))
    (if (plusp gone)
        (format nil "zeromem: forgot ~a turns of ~a" gone session)
        (format nil "zeromem: no turn of ~a is stored" session))))

(defun run-slash (args session-id)
  "The answer to one /zeromem invocation."
  (declare (ignore session-id))
  (let* ((words (nlk:split-words (or args "")))
         (head (first words)))
    (nlk:with-handlers ((zeromem-error (condition) (format nil "zeromem: ~a" condition)))
      (cond ((null head) (show))
            ((string-equal head "stats") (show))
            ((string-equal head "import") (import-command (second words)))
            ((string-equal head "forget") (forget-command (second words)))
            (t (format nil "zeromem: unknown subcommand ~a; usage ~a" head +usage+))))))

;;; --- the round: keep what was said, and tell a turn what it should know -------

(defun turn-input (turn)
  "The words TURN was started with, or NIL when the log has none."
  (let ((payload (nlk:events :turn-id (nlk:durable-turn-turn-id turn)
                              :kind "turn.input_committed" :as :payload)))
    (nlk:json-value payload :string "message")))

(defun recall-into-turn (session turn)
  "Put what past sessions answer this turn's own input into it, once."
  (let* ((query (turn-input turn))
         (evidence (and query
                        (recall-turns *store* query :top-k (setting :top-k) :exclude session))))
    (when evidence
      (nlk:record-turn-context-prompt turn
                                       :text (recall-block evidence (setting :chars)))
      (length evidence))))

(defun observe-round (op next)
  "The :ROUND point: every round of every turn. Keep what the session said
since the last round; on a turn's first sight, put past sessions' answers
into it. NEXT builds the round and answers NIL."
  (let ((session (getf op :session-id))
        (turn (getf op :turn)))
    (when (and *store* session)
      (ignore-errors (ingest-session *store* session))
      (when (and turn (setting :recall))
        (let ((turn-id (nlk:durable-turn-turn-id turn)))
          (unless (equal (gethash session *recalled*) turn-id)
            (setf (gethash session *recalled*) turn-id)
            (ignore-errors (recall-into-turn session turn)))))))
  (funcall next op))

;;; --- the entry: the one declaration ----------------------------------------

(nle:define-cell zeromem
  (:section ("zeromem")
    (:guide "nothing here is required: every turn of every session is kept verbatim in one file under the home, and the zeromem_ tools recall past sessions by keyword; store names the folder, top_k how many turns one recall answers, chars how much of one turn a line carries, and recall false stops the block that puts them into a turn")
    ("store" :directory :default "zeromem/"
             :doc "the folder holding turns.jsonl and progress.jsonl")
    ("recall" :boolean :default t
              :doc "put what past sessions answered into a turn's first request")
    ("top_k" :integer :default 5 :min 1
             :doc "how many turns one recall answers")
    ("chars" :integer :default 700 :min 50
             :doc "the most of one turn's words a recall line carries"))
  (:tool "zeromem_recall" (nlk:contract 'zeromem-recall) 'recall-tool)
  (:tool "zeromem_stats" +stats-schema+ 'stats-tool)
  (:tool "zeromem_forget_session" (nlk:contract 'zeromem-forget-call) 'forget-tool)
  (:help :zeromem "zeromem: past sessions' words, verbatim - (zeromem:recall \"what to look for\")" +manual+)
  (:hook :round #'observe-round)
  (:command "zeromem" 'run-slash
            :description "What this store holds: counters, import, forget one session"
            :argument-hint "stats | import [yes] | forget SESSION")
  (:start (lambda ()
            (clrhash *recalled*)
            (open-store (store-folder))
            (nle:on-stop #'close-store))))
