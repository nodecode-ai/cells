;;;; cell.lisp --- GBrain pointers: a background search, and the pointers it found.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of praggybuilds/hermes-gbrain-pointer (see NOTICE).
;;;; The original is a Hermes memory provider: after each non-trivial turn it
;;;; searches a GBrain MCP server on a daemon thread and injects up to N compact
;;;; pointers into the next turn. The harness seams are Nodecode's: a turn's
;;;; input is read from the store, the search runs on NLK:WORKER-START, and
;;;; what it found rides the next request through
;;;; NLK:RECORD-TURN-CONTEXT-PROMPT at the :ROUND point.
;;;;
;;;; Three things the original does not have, because a Nodecode cell can:
;;;;   gbrain_pointer {query, limit?}   search now, as a tool the model calls
;;;;   /gbrain-pointer [QUERY]          the endpoint, the switches, and a search
;;;;   (help :gbrain-pointer)           the primer below
;;;; It still never writes to GBrain: the cell is read-only, and a turn only
;;;; ever costs a search.

(in-package #:nodecode-gbrain-pointer)

(nlk:define-peripheral gbrain-pointer :not-running t :lock "gbrain-pointer")

;;; --- the model's side ----------------------------------------------------------

(nlk:define-contract search-call
  (:doc "Search the operator's GBrain knowledge base. Answers up to LIMIT compact
pointers -- source id, page slug and title -- to pages that look relevant to
QUERY, and never the page text: open the cited page in GBrain to read it. Read
only; the same search runs in the background before each turn, so ask this when
the operators words need an answer now.")
  ("query" :text :required t :doc "what to look for, as the phrase or question to search the brain with")
  ("limit" :integer :doc "how many pointers to answer, 1 to 10; the section's limit when left out"))

(defun search-answer (query &optional how-many)
  "What the tool and /gbrain-pointer answer for QUERY: the rendered pointers,
one sentence when nothing matched, or ERROR: ... when the brain refused."
  (handler-case
      (let ((lines (search-now query how-many)))
        (setf *last-error* nil)
        (if lines
            (render-pointers lines)
            "No matching pointers in GBrain."))
    (gbrain-pointer-error (condition)
      (setf *last-error* (princ-to-string condition))
      (format nil "ERROR: GBRAIN-POINTER-ERROR: ~a" condition))
    (error (condition)
      (setf *last-error* (format nil "~a: ~a" (type-of condition) condition))
      (format nil "ERROR: GBRAIN-POINTER-ERROR: ~a" *last-error*))))

(defun search-tool (call)
  "One gbrain_pointer call: the pointers, or a failure the transcript draws failed."
  (let* ((how-many (search-call-limit call))
         (answer (search-answer (search-call-query call) how-many)))
    (if (uiop:string-prefix-p "ERROR: " answer)
        (nle:failure answer)
        answer)))

(defun recall (query &optional how-many)
  "The verb an eval snippet calls: QUERY's pointers as text, a refusal included."
  (search-answer query how-many))

;;; --- the operator's side ---------------------------------------------------------

(defparameter +primer+
  "GBrain pointers are short references -- [source:slug] title -- to pages in the
operator's own GBrain knowledge base that looked relevant to what they last asked.
They arrive in the request under `Retrieved reference pointers' as fallible
background context: cite them, but open the page in GBrain before relying on what
it says. The cell never writes to GBrain.
The search runs in the background, so the pointers in a request answer the
previous turn and the words of this one are searched for the next. For an answer
now, call the tool: gbrain_pointer {query, limit?}. /gbrain-pointer shows the
endpoint and the switches; /gbrain-pointer QUERY asks by hand. A refusal is
ERROR: GBRAIN-POINTER-ERROR and says what to set."
  "What (help :gbrain-pointer) answers while the cell runs.")

(defun status-line ()
  "One line: where the brain is, how the cell asks, what it has injected, and
the last failure while one stands."
  (format nil "gbrain-pointer: ~a at ~a~@[ (source ~a)~]; auto ~:[off~;on~], limit ~d; ~
               ~d pointers injected this session~@[; last error: ~a~]"
          (auth-label) (mcp-url) (source) (eq t (setting :auto)) (limit) *injected*
          (and *last-error* (nlk:clip *last-error* 200))))

(defun panel (line)
  "The dialog bare /gbrain-pointer answers: ask the brain a question, then the
line as it stands."
  (nle:list-dialog
   :title "GBrain pointers" :summary line
   :rows (list (nle:ask-row "Search the brain" "Query" "/gbrain-pointer"
                             :placeholder "what to look for" :verb "search"))))

(defun run-slash (args session-id)
  "One /gbrain-pointer invocation => the answer's text, and typed alone the
dialog of what it does."
  (declare (ignore session-id))
  (let ((query (nlk:trimmed (or args ""))))
    (if (plusp (length query))
        (values (search-answer query) (panel (status-line)))
        (let ((line (status-line)))
          (values line (panel line))))))

;;; --- a turn, and the cache its pointers wait in -----------------------------------

(defparameter +queued-turns+ 8
  "Searches waiting for the worker: past this the oldest are dropped, as the
original keeps a bounded queue and skips a turn rather than queue it forever.")

(defparameter +acknowledgements+
  '("ok" "okay" "k" "thanks" "thank you" "ty" "yes" "no" "yep" "nope" "sure"
    "got it" "cool" "nice" "great" "hi" "hello" "hey" "bye" "cheers")
  "What the original skips as a trivial prompt: a greeting, an acknowledgement.")

(defvar *pointers* (make-hash-table :test #'equal)
  "Session id -> the pointer lines the last search for it found, handed out once.")

(defvar *pending* (make-hash-table :test #'equal)
  "Session id -> the query its next search will ask.")

(defvar *worker* nil
  "The prefetch worker (NLK:WORKER-START) while the cell runs, or NIL.")

(defun turn-key (turn)
  "TURN's id, whether the point handed over the turn or its id."
  (if (stringp turn) turn (nlk:durable-turn-turn-id turn)))

(defun turn-input (turn)
  "The words TURN was admitted with, or NIL."
  (nlk:json-value (nlk:events :turn-id (turn-key turn) :kind nlk::+kind-turn-started+
                               :as :payload)
                  :string "input"))

(defun trivial-prompt-p (text)
  "Whether TEXT is too thin to search for: a slash command, a greeting, an
acknowledgement, or fewer than three words."
  (let* ((text (nlk:trimmed text))
         (words (remove "" (uiop:split-string (string-downcase text)
                                               :separator '(#\Space #\Tab #\Newline))
                        :test #'equal)))
    (or (and (plusp (length text)) (char= (char text 0) #\/))
        (< (length words) 3)
        (member (format nil "~{~a~^ ~}" words) +acknowledgements+ :test #'equal))))

(defun take-pointers (session)
  "The pointer lines waiting for SESSION, and an empty slot: each result is
handed out once, as the original hands it to one turn."
  (with-gbrain-pointer-lock
    (prog1 (gethash session *pointers*)
      (remhash session *pointers*))))

(defun queue-search (session query)
  "Ask for QUERY to be searched for SESSION, and wake the worker."
  (with-gbrain-pointer-lock
    (when (> (hash-table-count *pending*) +queued-turns+)
      (clrhash *pending*))
    (setf (gethash session *pending*) query))
  (nlk:worker-poke *worker*))

(defun drain-pending ()
  "The queued (SESSION . QUERY) pairs, and an empty queue."
  (let ((jobs '()))
    (with-gbrain-pointer-lock
      (maphash (lambda (session query) (push (cons session query) jobs)) *pending*)
      (clrhash *pending*))
    (nreverse jobs)))

(defun prefetch-step ()
  "One worker lap: search every queued query and keep what each found.
A failure keeps nothing and costs the turn nothing."
  (dolist (job (drain-pending))
    (let ((lines (fetch-pointers (cdr job))))
      (when lines
        (with-gbrain-pointer-lock
          (setf (gethash (car job) *pointers*) lines)))))
  nil)

(defun on-round (op)
  "The :round point: TURN about to ask a model. Hand over what the last search
for this conversation found, into this very request, then queue the search that
this turn's own words will answer on the next one."
  (let* ((session (getf op :session-id))
         (turn (getf op :turn))
         (input (and session turn (nlk:store-open-p) (usable-p) (turn-input turn)))
         (query (and input (nlk:trimmed input))))
    (when (and query (plusp (length query)) (eq t (setting :auto)) (not (trivial-prompt-p query)))
      (let ((lines (take-pointers session)))
        (when lines
          (nlk:record-turn-context-prompt turn :text (render-pointers lines))
          (incf *injected* (length lines))))
      (queue-search session query))))

(defun check-config ()
  "Refuse a section that could never reach a brain, naming the member."
  (when (url-trouble-now)
    (error 'nlk:config-refusal
           :detail (format nil "gbrain-pointer: mcp_url ~a" (url-trouble-now))))
  (unless (auth-mode)
    (error 'nlk:config-refusal :detail (unconfigured-text))))

;;; --- the one declaration ---------------------------------------------------------

(nle:define-cell gbrain-pointer
  (:section ("gbrain-pointer")
    (:guide "run gbrain serve --http where the brain lives; mcp_url is its MCP endpoint, loopback by default (GBRAIN_MCP_URL answers too); grant this client read scope and put client_id and client_secret here (GBRAIN_MCP_CLIENT_ID and GBRAIN_MCP_CLIENT_SECRET answer too), or a pre-minted access_token; next step: /gbrain-pointer shows the endpoint and searches the brain")
    ("mcp_url" :string
               :doc "GBrain's MCP endpoint; http://127.0.0.1:3131/mcp when empty")
    ("access_token" :secret
                    :doc "a pre-minted bearer token; it wins over the client pair and is never refreshed")
    ("client_id" :string
                 :doc "the OAuth client id a client_credentials grant sends")
    ("client_secret" :secret
                      :doc "that client's secret")
    ("source" :string
              :doc "restrict every search to one GBrain source id; every source when empty")
    ("limit" :integer :default 3 :min 1
             :doc "pointers one turn carries, 1 to 10")
    ("auto" :boolean :default t
            :doc "search in the background each turn and inject what the last search found")
    ("allow_http" :boolean :default nil
                  :doc "allow plain http to a host that is not loopback")
    ("timeout_seconds" :integer :default 12 :min 1
                        :doc "one search's ceiling"))
  (:help :gbrain-pointer
         "gbrain-pointer: a few GBrain recall pointers, injected as background context before a turn"
         +primer+)
  (:command "gbrain-pointer" 'run-slash
            :description "Show the GBrain endpoint, or search the brain"
            :argument-hint "QUERY")
  (:tool "gbrain_pointer" (nlk:contract 'search-call) 'search-tool)
  (:hook :round (nle:observer #'on-round "gbrain-pointer"))
  (:start (lambda ()
            (check-config)
            (setf *pointers* (make-hash-table :test #'equal)
                  *pending* (make-hash-table :test #'equal)
                  *minted* nil
                  *injected* 0
                  *last-error* nil)
            (setf *worker* (nlk:worker-start "gbrain-pointer" #'prefetch-step :wake 60))
            (nle:on-stop (lambda ()
                           (nlk:worker-stop *worker*)
                           (setf *worker* nil
                                 *minted* nil
                                 *pointers* (make-hash-table :test #'equal)
                                 *pending* (make-hash-table :test #'equal)))))))
