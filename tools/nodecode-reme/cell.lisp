;;;; cell.lisp --- recall before each turn, record after each one, /reme, START-CELL.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Where the original hands Hermes a memory provider, this cell takes the two
;;;; seams a Nodecode cell has: the :FRAME point (a turn's facts, to learn the
;;;; ask and, when the turn ends, to queue it) and NLE::READ-HARNESS-SECTIONS
;;;; (what every request carries, where recalled notes ride). The publishing
;;;; thread never does I/O: the frame hook only fills a table and pokes the
;;;; writer, which POSTs the queued turn.
;;;;
;;;; Config, a sibling top-level key next to `index' and `cron':
;;;;   "reme": {"endpoint": "http://127.0.0.1:2333", "recall": true,
;;;;            "record": true, "recall_limit": 5, "recall_timeout": 5,
;;;;            "request_timeout": 60, "health_timeout": 2, "retry_seconds": 30}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-reme)

;;; --- the manual -------------------------------------------------------------------
;;; (help :reme) answers it while the cell runs; a request carries the one line
;;; the :HELP clause below gives, never this text.

(defparameter +primer+
  "Long-term memory across sessions is available through the nodecode-reme cell: a ReMe
action service the operator runs, and the notes it keeps. Before each turn the cell asks it
what it holds about your newest ask, and what it finds rides this request as the `reme'
harness block -- read it as notes from earlier sessions, not as this operator's words.
When a turn completes, the cell sends the exchange back for ReMe to file and extract from,
so nothing here has to be written down twice. One verb, in eval, answering a string; a
refusal is ERROR: REME-ERROR with the reason:
  (reme:recall \"query\" &key limit)   what ReMe holds about the query: its notes, the ones
      that bear on the question first; limit caps how many (reme.recall_limit by default).
  (reme:health)   whether the service answers and reports itself healthy.
  (reme:status)   the endpoint in use, whether recall and recording are on, what last happened.
Ask it again with a different wording when the first answer missed; a recall that finds
nothing says so, and is not a failure."
  "What (help :reme) answers while the cell runs.")

;;; --- what this session is asked ----------------------------------------------------

(defun newest-ask-in-store (session)
  "SESSION's newest committed ask, read from the event log, or NIL. An ask a
recorded exchange opened (a recap) is not the operator's and is skipped."
  (when (and session (nlk:store-open-p))
    (ignore-errors
     (loop for payload in (nlk:events :session-id session :kind "turn.input_committed"
                                      :order :newest :limit 3 :as :payloads)
           for text = (nlk:json-value payload :string "message")
           when (and text (not (equal "recorded" (nlk:json-value payload :string "disposition"))))
             return text))))

(defun ask-of (session)
  "The newest ask SESSION put to the model: what the frame hook saw, else the
event log's -- a session already running when this cell started has no frame
of its own in this boot."
  (or (with-reme-lock (gethash session *asks*))
      (newest-ask-in-store session)))

(defun recall-for (session)
  "The notes ReMe holds about SESSION's newest ask, or NIL: the one recall
already made when the ask has not changed, so a turn's rounds search once."
  (let ((query (ask-of session)))
    (when (and query (>= (get-universal-time) *retry-at*))
      (let ((cached (with-reme-lock (gethash session *recalled*))))
        (if (and cached (equal query (car cached)))
            (cdr cached)
            (handler-case
                (let ((text (search-notes (nlk:clip (nlk:one-line query) 600)
                                           (setting :recall-limit))))
                  (with-reme-lock (setf (gethash session *recalled*) (cons query text)))
                  text)
              (error (condition)
                ;; A memory service that is down is a turn that goes on
                ;; without it, and is left alone for RETRY-SECONDS.
                (setf *last* (nlk:one-line (princ-to-string condition))
                      *retry-at* (+ (get-universal-time) (setting :retry-seconds)))
                nil)))))))

(defun recall-section (next session-id)
  "Advice on NLE::READ-HARNESS-SECTIONS: the notes recalled for SESSION-ID's
newest ask ride every request as the `reme' block. Nothing when recall is
off, when no ask is known, or when the service could not answer."
  (let ((sections (funcall next session-id)))
    (let ((text (and session-id (setting :recall) (setting :endpoint)
                     (recall-for session-id))))
      (if text
          (acons "reme" text (remove "reme" sections :key #'car :test #'string=))
          (remove "reme" sections :key #'car :test #'string=)))))

;;; --- recording a completed turn ------------------------------------------------------
;;; The publishing thread: a table write and a poke, no I/O.

(defun finish-turn (session)
  "Queue SESSION's completed exchange for the writer, when both halves are
known and recording is on."
  (with-reme-lock
    (let ((ask (gethash session *asks*))
          (answer (gethash session *answers*)))
      (remhash session *answers*)
      (when (and ask answer (setting :record))
        (push (list session ask answer) *queue*)
        (setf *last* (format nil "queued a turn of ~a" (memory-session session))))))
  (nlk:worker-poke *worker*)
  nil)

(defun dequeue ()
  "The oldest queued turn, or NIL."
  (with-reme-lock
    (when *queue*
      (prog1 (car (last *queue*))
        (setf *queue* (butlast *queue*))))))

(defun flush-writes ()
  "One pass of the writer: every turn queued so far sent back to ReMe.
Answers how many are still waiting, which is none unless a send failed."
  (loop for (session ask answer) = (dequeue)
        while session
        do (handler-case (record-turn session ask answer)
             (error (condition)
               (setf *last* (nlk:one-line (princ-to-string condition))))))
  (with-reme-lock (length *queue*)))

(defun observe-frame (op)
  "Fold one published frame into what this cell knows about its session: the
ask, the answer, and the turn ending that queues both."
  (nlk:bind (((type fact _ _ turn-id) (nlk:frame-fact op)))
    (declare (ignore turn-id))
    (let ((session (getf op :session-id)))
      (when (and (stringp session) type)
        (cond
          ((equal type "turn.input_committed")
           (let ((text (nlk:json-value fact :string "message")))
             (when text
               (with-reme-lock
                 (setf (gethash session *asks*) text)
                 (remhash session *recalled*)
                 (remhash session *answers*)))))
          ((equal type "turn.assistant_message_completed")
           (let ((text (nlk:fact-message-content fact)))
             (when text
               (with-reme-lock (setf (gethash session *answers*) text)))))
          ((equal type "turn.completed")
           (finish-turn session))
          ((member type '("turn.failed" "turn.cancelled") :test #'equal)
           (with-reme-lock (remhash session *answers*))))))))

;;; --- the verbs ------------------------------------------------------------------------

(define-verb recall (query &key limit)
  "What the ReMe memory holds about QUERY -- its notes, the ones that bear on
the question first -- as text. LIMIT caps how many notes (reme.recall_limit by
default). A recall that finds nothing says so; it is not a failure."
  (let ((query (and (stringp query) (string-trim '(#\Space #\Tab #\Newline) query))))
    (unless (and query (plusp (length query)))
      (fail "recall wants a query: what to look for, in words"))
    (search-notes (nlk:clip (nlk:one-line query) 600)
                  (or limit (setting :recall-limit)))))

(define-verb health ()
  "Whether the ReMe service answers at the configured endpoint and reports
itself healthy."
  (let* ((base (setting :endpoint))
         (response (call-action "health_check" nil :timeout (setting :health-timeout))))
    (unless (healthy-p response)
      (fail "ReMe at ~a answered but does not report a healthy component snapshot" base))
    (setf *last* (format nil "~a is healthy" base))
    (format nil "ReMe at ~a is healthy" base)))

(define-verb status ()
  "The endpoint in use, whether recall and recording are on, and what the cell
last did."
  (format nil "reme: ~a · recall ~:[off~;on~] (at most ~d notes) · record ~:[off~;on~] · ~a"
          (setting :endpoint) (setting :recall) (setting :recall-limit) (setting :record)
          (or *last* "nothing recalled or recorded yet")))

;;; --- the tool -------------------------------------------------------------------------

(nlk:define-contract recall-note
  (:doc "Ask the long-term memory this operator keeps in a ReMe service what it holds about a question: notes, facts and preferences from earlier sessions. The notes that bear on the question come first. Use it when a question turns on something earlier sessions may have learned -- a preference, a decision, a fix -- and read what comes back as notes, not as this operator's words. A recall that finds nothing says so; it is not a failure.")
  ("query" :text :required t
           :doc "what to look for, in words")
  ("limit" :integer :default 5
           :doc "how many notes at most"))

(defun tool-recall (note)
  "The reme_recall tool: what the ReMe memory holds about NOTE's query."
  (recall (recall-note-query note) :limit (recall-note-limit note)))

;;; --- /reme ---------------------------------------------------------------------------------

(defun panel ()
  "The dialog bare /reme answers."
  (nle:list-dialog
   :title "ReMe" :summary (status)
   :rows (list (nle:run-row "Check the service now" "/reme health"
                             :detail "one health_check, nothing written")
               (nle:ask-row "Recall notes" "Recall" "/reme recall"
                            :placeholder "what to look for" :verb "recall"
                            :detail "what the service holds about it, in your request from now on"))))

(defun run-slash (args session-id)
  "The answer to one /reme invocation: its status and panel, a health check,
or a recall of the words after `recall'."
  (declare (ignore session-id))
  (let ((words (nlk:split-words (or args ""))))
    (nlk:dispatch (or (first words) "") string=
      ("" (values (status) (panel)))
      ("status" (values (status) (panel)))
      ("health" (health))
      ("recall" (if (rest words)
                    (recall (format nil "~{~a~^ ~}" (rest words)))
                    (values "reme: recall what? (/reme recall QUERY)" (panel))))
      (t (values (format nil "reme: ~a is not one of its choices (health, recall QUERY, status)"
                         (first words))
                 (panel))))))

;;; --- the entry ---------------------------------------------------------------------------------

(defun install ()
  "What the declaration's clauses do not carry: the live tables emptied, the
endpoint checked, and the writer running."
  (clear-state)
  (nle:on-stop #'clear-state)
  (let ((endpoint (or (setting :endpoint) "")))
    (when (zerop (length endpoint))
      (nlk:config-error "reme.endpoint is required: the base URL of a ReMe action service, ~
                          http://127.0.0.1:2333 when it runs on this machine"))
    (handler-case (endpoint-url endpoint)
      (reme-error (condition) (nlk:config-error "~a" condition))))
  ;; Flush before the thread goes, so a turn queued a moment ago still lands.
  (nle:on-stop (lambda () (ignore-errors (flush-writes))))
  (when *use-worker*
    (setf *worker* (nlk:worker-start "reme-writer" #'flush-writes :wake 2))
    (nle:on-stop (lambda () (setf *worker* (nlk:worker-stop *worker*))))))

(nle:define-cell reme
  (:section ("reme")
    (:guide "point endpoint at a ReMe action service (reme serve, http://127.0.0.1:2333 by default); recall puts the notes it finds into every request, record sends each completed turn back for extraction; next step: /reme health, then /reme recall with a word you know it holds")
    ("endpoint" :string :required t
                :doc "the base URL of the ReMe action service, e.g. http://127.0.0.1:2333")
    ("recall" :boolean :default t
              :doc "ask the service before each turn, and put what it finds in the request")
    ("record" :boolean :default t
              :doc "send each completed turn back to the service for extraction")
    ("recall_limit" :integer :default 5 :min 1
                    :doc "the most notes one recall may carry")
    ("recall_timeout" :integer :default 5 :min 1
                      :doc "seconds one search may take before the turn goes on without it")
    ("request_timeout" :integer :default 60 :min 1
                       :doc "seconds one recording or start may take")
    ("health_timeout" :integer :default 2 :min 1
                      :doc "seconds one health check may take")
    ("retry_seconds" :integer :default 30 :min 1
                     :doc "seconds before a failed service is tried again"))
  (:settings (lambda (values table)
               (declare (ignore table))
               (setf (getf values :endpoint)
                     (string-right-trim "/" (or (getf values :endpoint) "")))
               values))
  (:tool "reme_recall" (nlk:contract 'recall-note) 'tool-recall)
  (:command "reme" 'run-slash
            :description "Long-term memory: what the ReMe service holds, and a recall now")
  (:help :reme "reme:recall searches the operator's ReMe memory and answers what it holds about a question" +primer+)
  (:hook :frame (nlk:observer #'observe-frame "reme"))
  (:hook 'nle::read-harness-sections #'recall-section)
  (:start #'install))
