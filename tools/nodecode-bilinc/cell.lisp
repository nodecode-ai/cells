;;;; cell.lisp --- Bilinc memory: four tools, /bilinc, recall before a turn.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of ReARCLabs's bilinc Hermes memory-provider plugin (see
;;;; NOTICE). Every call goes over HTTPS to Bilinc Cloud under one API key;
;;;; nothing is stored on this machine and nothing else is dialled. The
;;;; original's four tools keep their names, descriptions and parameters:
;;;;
;;;;   bilinc_recall    the memories nearest a query, with their keys
;;;;   bilinc_remember  store a new fact under a key; never overwrites
;;;;   bilinc_revise    change an existing memory; the old value is kept
;;;;   bilinc_forget    drop a memory from recall, with the reason kept
;;;;
;;;; Two automatic halves, as the original has:
;;;;   auto_recall    at every model request the turn's input is recalled
;;;;                  and the memories ride that request as a block
;;;;   capture_turns  each finished turn stored as a short-lived episodic
;;;;                  record (off by default)
;;;;
;;;; Secret-shaped strings are replaced with [REDACTED] before anything is
;;;; sent, a failed recall pauses recall for a minute, and a rejected key
;;;; never stops a turn: the tool answers what the API said.

(defpackage #:nodecode-bilinc
  (:use #:cl)
  (:export #:bilinc-error #:bilinc-error-detail))

(in-package #:nodecode-bilinc)

(nlk:define-peripheral bilinc :not-running t :lock "bilinc")

(define-condition bilinc-api-error (bilinc-error)
  ((code :initarg :code :initform nil :reader bilinc-api-error-code)
   (status :initarg :status :initform nil :reader bilinc-api-error-status))
  (:documentation "A failed Bilinc Cloud call; CODE is the API's own error
code when the server sent one, and STATUS the HTTP status behind it."))

(defparameter +version+ "0.1.0"
  "What this rewrite calls itself in the client headers.")

(defparameter +default-base-url+ "https://bilinc.space"
  "Bilinc Cloud, where every call goes unless the section names another
https endpoint.")

(defparameter +loopback-hosts+ '("localhost" "127.0.0.1" "::1" "[::1]")
  "Hosts plain http is accepted for, as the original's _LOCAL_HOSTS does.")

(defparameter +preamble+
  "[Bilinc memory - recalled for this turn; may be outdated, verify before relying on it]"
  "The first line of the block an automatic recall injects.")

(defparameter +max-value-chars+ 6000
  "How much of one memory's value is sent or shown; the original's
_MAX_VALUE_CHARS.")

(defparameter +max-line-chars+ 320
  "How much of one value rides an injected line; the original's
_MAX_PREFETCH_LINE.")

(defparameter +max-key-chars+ 512
  "The longest key bilinc_remember will take from the model.")

(defparameter +recall-pause-seconds+ 60
  "How long a failed automatic recall waits before it dials again: an outage
costs one short wait a minute, not one per turn.")

(defparameter +recall-reuse-seconds+ 60
  "How long one conversation's identical query reuses the answer it already
had, so a prompt re-sent does not dial again.")

(defparameter +trivial-prompts+
  '("hi" "hey" "hello" "yo" "thanks" "thank you" "thx" "ok" "okay"
    "yes" "no" "sure" "continue" "go" "go on" "next" "nice")
  "Prompts that ask nothing of memory, as the original's is_trivial_prompt
skips; anything shorter than four characters is skipped too.")

(defparameter +secret-prefixes+
  '(("bearer " . 12) ("sk-" . 20) ("pypi-" . 20) ("bil_live_" . 12)
    ("ghp_" . 20) ("gho_" . 20) ("ghu_" . 20) ("ghs_" . 20) ("ghr_" . 20)
    ("xoxb-" . 10) ("xoxp-" . 10) ("xoxa-" . 10) ("xoxr-" . 10)
    ("akia" . 16))
  "Prefixes of well-known secrets, each with how many token characters must
follow it before it is redacted: the original's _SECRETISH alternation.")

(defparameter +secret-keys+ '("api_key" "api-key" "apikey" "token" "password")
  "Names whose `name = value' form is redacted whole, as the original's
api[_-]?key / token / password branches do.")

(defvar *paused-until* 0
  "Universal time automatic recall may dial again after a failure.")

(defvar *recalled* 0
  "How many memories this cell has injected since it started, for /bilinc.")

(defvar *seen* (make-hash-table :test #'equal)
  "Session id -> (QUERY . UNIVERSAL-TIME) it last recalled, while the cell
runs.")

(defvar *turns* (make-hash-table :test #'equal)
  "Turn id -> its captured turn's plist of :SESSION, :USER and :ANSWER, until
it completes.")

(defvar *queued* '()
  "Finished turns waiting for capture, newest first: (TURN-ID . PLIST).")

(defvar *worker* nil
  "The capture worker (NLK:WORKER-START) while the cell runs, or NIL.")

;;; --- the section, read back --------------------------------------------------

(defun api-key ()
  "The key the section names, inline or by environment variable, else
BILINC_API_KEY; a leading `Bearer ' is stripped, as the original's client does."
  (let* ((inline (setting :api-key))
         (variable (setting :api-key-env))
         (key (or (and inline (plusp (length inline)) inline)
                  (and variable (plusp (length variable)) (uiop:getenv variable))
                  (uiop:getenv "BILINC_API_KEY")
                  ""))
         (key (string-trim '(#\Space #\Tab #\Newline #\Return) key)))
    (if (and (>= (length key) 7) (string-equal "bearer " key :end2 7))
        (string-trim '(#\Space) (subseq key 7))
        key)))

(defun configured-p () (plusp (length (api-key))))

(defun root ()
  "The API root this cell talks to, without a trailing slash."
  (let ((given (string-right-trim "/" (or (setting :base-url) ""))))
    (if (plusp (length given)) given +default-base-url+)))

(defun https-p (url)
  "URL's scheme is https, or http to a loopback host -- the original's
_resolve_base_url rule."
  (let* ((mark (search "://" url))
         (scheme (and mark (string-downcase (subseq url 0 mark))))
         (rest (and mark (subseq url (+ mark 3))))
         (end (and rest (or (position-if (lambda (character)
                                           (member character '(#\/ #\: #\? #\#)))
                                         rest)
                            (length rest))))
         (host (and rest (string-downcase (subseq rest 0 end)))))
    (or (equal scheme "https")
        (and (equal scheme "http") (member host +loopback-hosts+ :test #'equal)))))

(defun check-config ()
  "Refuse a section that could never reach Bilinc, naming the member."
  (unless (configured-p)
    (error 'nlk:config-refusal
           :detail (concatenate 'string
                                 "bilinc: no API key: set \"api_key\" (or \"api_key_env\") in the "
                                 "bilinc section, or put BILINC_API_KEY in the environment; "
                                 "a free key comes from https://bilinc.space")))
  (unless (https-p (root))
    (error 'nlk:config-refusal
           :detail (format nil "bilinc: base_url ~s must be https, or http to localhost" (root)))))

;;; --- what never leaves in the clear ------------------------------------------

(defun token-character-p (character)
  "CHARACTER is one a token or key is made of."
  (or (alphanumericp character)
      (member character '(#\_ #\. #\-) :test #'char=)))

(defun match-prefixed-secret (text start)
  "When TEXT at START begins a well-known secret, the index just past it."
  (loop for (prefix . minimum) in +secret-prefixes+
        do (let ((end (+ start (length prefix))))
             (when (and (<= end (length text))
                        (string-equal prefix text :start2 start :end2 end)
                        (or (zerop start) (not (token-character-p (char text (1- start))))))
               (let ((stop end))
                 (loop while (and (< stop (length text))
                                  (token-character-p (char text stop)))
                       do (incf stop))
                 (when (>= (- stop end) minimum)
                   (return-from match-prefixed-secret stop))))))
  nil)

(defun match-keyed-secret (text start)
  "When TEXT at START begins `name: value' or `name=value' for a secret's
name, the index just past the value."
  (dolist (key +secret-keys+ nil)
    (let ((end (+ start (length key))))
      (when (and (<= end (length text))
                 (string-equal key text :start2 start :end2 end)
                 (or (zerop start) (not (token-character-p (char text (1- start))))))
        (let ((mark end))
          (loop while (and (< mark (length text)) (member (char text mark) '(#\Space #\Tab)))
                do (incf mark))
          (when (and (< mark (length text)) (member (char text mark) '(#\: #\=) :test #'char=))
            (incf mark)
            (loop while (and (< mark (length text)) (member (char text mark) '(#\Space #\Tab)))
                  do (incf mark))
            (when (< mark (length text))
              (loop while (and (< mark (length text))
                               (not (member (char text mark) '(#\Space #\Tab #\Newline #\Return))))
                    do (incf mark))
              (return mark))))))))

(defun redact (text)
  "TEXT with every secret-shaped substring replaced by [REDACTED]: the
original's _redact safety net, not a guarantee."
  (let* ((text (if (stringp text) text (princ-to-string (or text ""))))
         (out (make-string-output-stream)))
    (loop with index = 0
          while (< index (length text))
          do (let ((end (or (match-prefixed-secret text index)
                            (match-keyed-secret text index))))
               (cond (end (write-string "[REDACTED]" out) (setf index end))
                     (t (write-char (char text index) out) (incf index)))))
    (get-output-stream-string out)))

(defun clip (text &optional (limit +max-value-chars+))
  "TEXT, redacted and cut at LIMIT characters; the original's _clip."
  (let ((text (redact text)))
    (if (<= (length text) limit)
        text
        (concatenate 'string (subseq text 0 limit) "...[truncated]"))))

(defun compact (text &optional (limit +max-line-chars+))
  "TEXT flattened to one line of at most LIMIT characters; the original's
_compact."
  (let* ((flat (string-trim '(#\Space #\Tab #\Newline #\Return)
                             (remove-if (lambda (character)
                                          (member character '(#\Newline #\Return #\Tab)))
                                        (substitute #\Space #\Newline
                                                    (substitute #\Space #\Return text)))))
         (flat (if (> (length flat) limit)
                   (concatenate 'string (subseq flat 0 (1- limit)) "...")
                   flat)))
    flat))

(defun digest (text &optional (limit 16))
  "TEXT's digest, in hex, at most LIMIT characters: a key's content half."
  (let ((hex (format nil "~(~36,'0X~)" (sxhash text))))
    (subseq hex 0 (min limit (length hex)))))

(defun safe-part (value &optional (limit 64))
  "VALUE as one key segment: every character outside [A-Za-z0-9_.:-] becomes
an underscore; the original's _safe_part."
  (let* ((raw (or value ""))
         (clean (map 'string (lambda (character)
                               (if (or (alphanumericp character)
                                       (member character '(#\_ #\. #\: #\-) :test #'char=))
                                   character
                                   #\_))
                             raw))
         (trimmed (string-trim "_" clean))
         (kept (if (plusp (length trimmed)) trimmed "default")))
    (subseq kept 0 (min limit (length kept)))))

(defun request-id (prefix)
  "A fresh Idempotency-Key: Bilinc replays the stored response for a repeated
key, so a key derived from content would turn remember then forget then
remember of one fact into a silent no-op -- the original's _request_id."
  (format nil "~a-~(~32,'0X~)" prefix (random (expt 2 128))))

;;; --- the wire -----------------------------------------------------------------

(defun object (&rest pairs)
  "A JSON object from alternating NAME VALUE; a NIL value is left out."
  (apply #'nlk:make-json-object
         (loop for (name value) on pairs by #'cddr
               when value append (list name value))))

(defun decode-body (answer)
  "ANSWER -- an HTTP body -- as a decoded object; an empty body is {}."
  (if (and (stringp answer)
           (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) answer))))
      (handler-case (let ((decoded (nlk:decode-json answer)))
                      (if (hash-table-p decoded) decoded (nlk:json-object "result" decoded)))
        (error () (nlk:json-object "message" (subseq answer 0 (min 200 (length answer))))))
      (nlk:json-object)))

(defun describe-failure (condition url)
  "What a dial that never answered says, for the one line the model reads."
  (handler-case (nle:transport-failure-label condition url)
    (error () (princ-to-string condition))))

(defun api-fail (code status control &rest arguments)
  "Signal BILINC-API-ERROR with CODE, STATUS and a formatted detail."
  (error 'bilinc-api-error :code code :status status
                            :detail (apply #'format nil control arguments)))

(defun headers (idempotency)
  "The request headers every call carries, as the original's _headers has them."
  (append (list (cons "authorization" (format nil "Bearer ~a" (api-key)))
                (cons "content-type" "application/json")
                (cons "user-agent" (format nil "bilinc-nodecode/~a" +version+))
                (cons "x-bilinc-client" "nodecode-plugin")
                (cons "x-bilinc-client-version" +version+))
          (when idempotency (list (cons "idempotency-key" idempotency)))))

(defun body-message (body answer)
  "What a failed answer says, for the one line the model reads."
  (or (and (hash-table-p body) (nlk:json-value body :string "message"))
      (and (hash-table-p body) (nlk:json-value body :string "error"))
      (and (stringp answer) (subseq answer 0 (min 200 (length answer))))))

(defun call (method path &key body idempotency timeout)
  "METHOD PATH on the configured root with BODY, a JSON object, => the decoded
answer. A dial that failed, or a refused status, signals BILINC-API-ERROR."
  (let ((url (concatenate 'string (root) path))
        (limit (or timeout (setting :timeout-seconds)))
        (answer nil)
        (status nil))
    (handler-case
        (multiple-value-setq (answer status)
          (nlk:http method url
                    :headers (headers idempotency)
                    :content (and body (nlk:encode-json-object body))
                    :timeout limit
                    :connect-timeout (min 10 limit)))
      ((or error sb-sys:deadline-timeout) (condition)
        (api-fail "connection_failed" nil "~a ~a: ~a" (string-downcase (string method)) path
                  (describe-failure condition url))))
    (if (or (null status) (and (integerp status) (< status 300)))
        (decode-body answer)
        (let ((body (decode-body answer)))
          (api-fail (and (hash-table-p body) (nlk:json-value body :string "error"))
                    status "~a ~a: HTTP ~d~@[ - ~a~]"
                    (string-upcase (string method)) path status (body-message body answer))))))

(defun results-of (body)
  "BODY's result objects: the \"results\" array of an object, or the array itself."
  (coerce (or (cond ((hash-table-p body) (nlk:json-value body :array "results"))
                    ((vectorp body) body))
               #())
          'list))

(defun recall-results (query &key limit profile timeout)
  "The memories nearest QUERY for this workspace, as the API's own objects."
  (results-of (call :post "/api/cloud/memory/recall"
                     :body (object "query" (clip query 4096)
                                   "profile" (or profile "balanced")
                                   "limit" limit)
                     :timeout timeout)))

(defun commit (key value &key (memory-type "semantic") (importance 0.7) metadata
                              (source "nodecode") session-id canonical priority ttl
                              if-absent idempotency-key)
  "Store VALUE under KEY, always fresh: an existing key is refused unless
IF-ABSENT is off and the server overwrites."
  (let ((pairs (list "key" key "value" value "memoryType" memory-type
                     "importance" importance "metadata" (or metadata (nlk:json-object))
                     "source" source
                     ;; NIL encodes as JSON false, so the flag always rides.
                     "canonical" (and canonical t))))
    (when session-id
      (setf pairs (nconc pairs (list "sessionId"
                                      (subseq session-id 0 (min 256 (length session-id)))))))
    (when priority (setf pairs (nconc pairs (list "priority" priority))))
    (when ttl (setf pairs (nconc pairs (list "ttl" ttl))))
    (when if-absent (setf pairs (nconc pairs (list "ifAbsent" t))))
    (call :post "/api/cloud/memory/commit" :body (apply #'nlk:make-json-object pairs)
          :idempotency idempotency-key)))

(defun revise (key value &key reason idempotency-key)
  "Replace KEY's value, keeping the old one in its history."
  (call :post "/api/cloud/memory/revise"
        :body (object "key" key "value" value "strategy" "recency"
                      "reason" (and reason (plusp (length reason)) reason))
        :idempotency idempotency-key))

(defun forget (key &key reason idempotency-key)
  "Drop KEY from recall; REASON is written to its history."
  (call :post "/api/cloud/memory/forget"
        :body (object "key" key "reason" reason)
        :idempotency idempotency-key))

;;; --- what the model reads ------------------------------------------------------

(defun value-text (value)
  "VALUE as text: a string as itself, anything else as its JSON."
  (cond ((stringp value) value)
        ((null value) "")
        (t (handler-case (nlk:encode-json-object value)
             (error () (princ-to-string value))))))

(defun entry-version (item)
  "ITEM's version: a write names it entryVersion, a recall item entry_version."
  (and (hash-table-p item)
       (or (gethash "entryVersion" item) (gethash "entry_version" item)
           (gethash "version" item))))

(defun format-results (query items)
  "ITEMS as the lines the model reads: one per memory, its key -- what
bilinc_revise and bilinc_forget take -- its value and its score."
  (let ((items (remove-if-not #'hash-table-p items)))
    (if (null items)
        (format nil "No memory matched ~s." query)
        (with-output-to-string (out)
          (format out "~d ~a for ~s:"
                  (length items) (if (= 1 (length items)) "memory" "memories") query)
          (dolist (item items)
            (let ((score (gethash "score" item)))
              (format out "~% - [~a] ~a~@[ (score ~,2f)~]~@[, v~a~]"
                      (or (gethash "key" item) "memory")
                      (compact (value-text (gethash "value" item)))
                      (and (numberp score) score)
                      (entry-version item))))))))

(defun format-block (items)
  "ITEMS as the block an automatic recall injects, or NIL when nothing matched."
  (let ((lines (loop for item in (remove-if-not #'hash-table-p items)
                      for value = (compact (value-text (gethash "value" item)))
                      when (plusp (length value))
                        collect (format nil " - ~a: ~a"
                                        (or (gethash "key" item) "memory") value))))
    (when lines
      (format nil "~a~%~{~a~%~}" +preamble+ lines))))

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the `ERROR:' line the model
repairs from."
  `(handler-case (progn ,@body)
     (bilinc-api-error (condition)
       (nle:failure (format nil "ERROR: bilinc: ~a~@[~a~]" condition
                             (if (equal (bilinc-api-error-code condition) "memory_exists")
                                 " Use bilinc_revise to change an existing key."
                                 ""))))
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: bilinc: ~a" condition)))))

;;; --- the four tools ------------------------------------------------------------

(nlk:define-contract recall-call
  (:doc "Search long-term memory in Bilinc. Returns matching memories with their keys, so they can be revised or forgotten.")
  ("query" :text :required t :doc "What to look for, as a question or a phrase.")
  ("limit" :integer :doc "Maximum results, 1-20 (default: the section's recall_limit)."))

(nlk:define-contract remember-call
  (:doc "Store a new durable fact, decision or preference in Bilinc. Never overwrites: if the key already exists, use bilinc_revise instead.")
  ("content" :text :required t :doc "The fact to remember, self-contained.")
  ("key" :text :doc "Optional stable key such as 'pref:editor'. Generated if omitted.")
  ("memory_type" :text :options '("semantic" "episodic" "procedural") :default "semantic"
                 :doc "semantic (facts, the default), episodic (events), procedural (how-to)."))

(nlk:define-contract revise-call
  (:doc "Replace the value of an existing Bilinc memory. Bilinc keeps the previous version in its history.")
  ("key" :text :required t :doc "Key of the memory to change (from bilinc_recall).")
  ("content" :text :required t :doc "The new value.")
  ("reason" :text :doc "Why it changed (kept in the audit history)."))

(nlk:define-contract forget-call
  (:doc "Remove a Bilinc memory from recall. The reason is written to the audit history.")
  ("key" :text :required t :doc "Key of the memory to forget (from bilinc_recall).")
  ("reason" :text :required t :doc "Why it should be forgotten."))

(defun note-metadata () (object "kind" "note" "plugin" "bilinc-nodecode"))

(defun recall-tool (call)
  (answering
    (let* ((query (recall-call-query call))
           (asked (recall-call-limit call))
           (limit (min 20 (max 1 (or asked (setting :recall-limit))))))
      (format-results query (recall-results query :limit limit :profile "balanced")))))

(defun remember-tool (call)
  (answering
    (let* ((content (clip (remember-call-content call)))
           (given (nlk:trimmed (or (remember-call-key call) "")))
           (key (if (plusp (length given))
                    (subseq given 0 (min +max-key-chars+ (length given)))
                    (format nil "nodecode_note::~a" (digest content))))
           (answer (commit key content
                           :memory-type (or (remember-call-memory-type call) "semantic")
                           :importance 0.7 :canonical t :priority 0.6 :if-absent t
                           :metadata (note-metadata)
                           :idempotency-key (request-id "nodecode-note"))))
      (multiple-value-bind (stored present) (gethash "success" answer)
        (cond ((and present (null stored))
               (format nil "Key ~a already exists; use bilinc_revise to change it." key))
              (t (format nil "Stored ~a~@[, version ~a~]." key (entry-version answer))))))))

(defun revise-tool (call)
  (answering
    (let* ((key (nlk:trimmed (revise-call-key call)))
           (content (clip (revise-call-content call)))
           (answer (revise key content
                           :reason (clip (or (revise-call-reason call) "") 512)
                           :idempotency-key (request-id "nodecode-revise"))))
      (format nil "Revised ~a~@[, version ~a~]." key (entry-version answer)))))

(defun forget-tool (call)
  (answering
    (let ((key (nlk:trimmed (forget-call-key call))))
      (forget key :reason (clip (forget-call-reason call) 512)
              :idempotency-key (request-id "nodecode-forget"))
      (format nil "Forgot ~a." key))))

;;; --- recall before a turn -------------------------------------------------------

(defun turn-key (turn)
  "TURN's id, whether the point handed over the turn or its id."
  (if (stringp turn) turn (nlk:durable-turn-turn-id turn)))

(defun turn-input (turn)
  "The words TURN was admitted with, or NIL."
  (nlk:json-value (nlk:events :turn-id (turn-key turn) :kind nlk::+kind-turn-started+
                               :as :payload)
                  :string "input"))

(defun trivial-prompt-p (text)
  "TEXT asks nothing of memory: too short, or one bare word of greeting."
  (let ((flat (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return #\. #\! #\?)
                                             text))))
    (or (< (length flat) 4) (member flat +trivial-prompts+ :test #'equal))))

(defun recall-block (query &key (profile "fast") timeout)
  "The block to inject for QUERY and how many memories it carries."
  (let ((items (recall-results query :limit (min 20 (setting :recall-limit))
                                      :profile profile :timeout timeout)))
    (values (format-block items) (length items))))

(defun on-round (op)
  "The :round point: TURN about to ask a model, its input recalled into this
very request. A failure pauses recall for a minute and never stops the turn."
  (let* ((turn (getf op :turn))
         (session (or (getf op :session-id) ""))
         (input (and turn (setting :auto-recall) (configured-p)
                     (ignore-errors (turn-input turn)))))
    (when (and (stringp input) (plusp (length (nlk:trimmed input)))
               (not (trivial-prompt-p input))
               (>= (get-universal-time) *paused-until*)
               (not (let ((last (gethash session *seen*)))
                      (and last (equal (car last) input)
                           (< (- (get-universal-time) (cdr last)) +recall-reuse-seconds+)))))
      (setf (gethash session *seen*) (cons input (get-universal-time)))
      (handler-case
          (multiple-value-bind (block count)
              (recall-block input :timeout (min 4 (setting :timeout-seconds)))
            (when block
              (incf *recalled* count)
              (nlk:record-turn-context-prompt turn :text block)))
        (error () (setf *paused-until* (+ (get-universal-time) +recall-pause-seconds+)))))))

;;; --- a finished turn, captured (off by default) ----------------------------------

(defun turn-record (turn)
  "TURN's plist in *TURNS*, made when it is first seen."
  (or (gethash turn *turns*) (setf (gethash turn *turns*) (list :turn turn))))

(defun remember-turn (turn property text session)
  "Keep TEXT, one half of TURN, until the turn completes."
  (when (and (stringp turn) (stringp text) (plusp (length text)))
    (with-bilinc-lock
      (let ((record (turn-record turn)))
        (setf (getf record :session) (or session (getf record :session)))
        (setf (getf record property) text)
        ;; a key the record did not carry prepends a cons, so the table takes the new head
        (setf (gethash turn *turns*) record)))))

(defun take-turn (turn)
  "TURN's record, removed from *TURNS*."
  (with-bilinc-lock (prog1 (gethash turn *turns*) (remhash turn *turns*))))

(defun enqueue-turn (turn record)
  "Queue TURN's record for capture; the newest eight are kept."
  (when (and (getf record :user) (getf record :answer))
    (with-bilinc-lock
      (setf *queued* (cons (cons turn record) *queued*))
      (when (> (length *queued*) 8) (setf *queued* (subseq *queued* 0 8))))
    (nlk:worker-poke *worker*)))

(defun pop-queued (&aux (entry nil))
  (with-bilinc-lock
    (when *queued* (setf entry (first *queued*) *queued* (rest *queued*))))
  entry)

(defun capture-one (turn record)
  "Send TURN's two texts to Bilinc as one short-lived episodic record."
  (let* ((session (or (getf record :session) ""))
         (key (format nil "nodecode_turn::~a::~a" (safe-part session)
                       (digest (format nil "~a~c~a~c~a" session (code-char 31)
                                       (getf record :user) (code-char 31)
                                       (getf record :answer)))))
         (ttl (* 86400 (setting :capture-ttl-days))))
    (commit key (object "user" (clip (getf record :user) 3000)
                        "assistant" (clip (getf record :answer) 3000))
            :memory-type "episodic" :importance 0.3 :source "nodecode_session_summary"
            :session-id session :canonical nil :priority 0.2 :ttl ttl
            :metadata (object "kind" "turn" "plugin" "bilinc-nodecode")
            :idempotency-key (format nil "nodecode-turn-~a"
                                      (digest (format nil "~a~c1" key (code-char 31)) 32)))))

(defun capture-step ()
  "The worker's step: send every queued turn."
  (loop for entry = (pop-queued)
        while entry
        do (handler-case (capture-one (car entry) (cdr entry))
             (error (condition) (warn "bilinc: a turn was not captured: ~a" condition))))
  nil)

(defun observe-frame (op)
  "Fold one published frame: a turn's two texts kept, a finished turn queued."
  (multiple-value-bind (type payload metadata turn) (nlk:frame-fact op)
    (declare (ignore metadata))
    (when (and type turn *bilinc* (setting :capture-turns))
      (cond ((equal type "turn.input_committed")
             (remember-turn turn :user (nlk:fact-message-content payload) (getf op :session-id)))
            ((equal type "turn.assistant_message_completed")
             (remember-turn turn :answer (nlk:fact-message-content payload) (getf op :session-id)))
            ((equal type "turn.completed")
             (let ((record (take-turn turn)))
               (when record (enqueue-turn turn record))))))))

;;; --- the manual and /bilinc -------------------------------------------------------

(defparameter +primer+
  "Bilinc keeps long-term memories server-side, versioned: every change keeps the previous
value and the reason it was given. (help :bilinc) is this page; the tools are the four.
  bilinc_recall {query, limit?}                 the memories nearest a question, with their keys
  bilinc_remember {content, key?, memory_type?} a new durable fact; never overwrites a key
  bilinc_revise {key, content, reason?}         replace a memory's value, keeping its history
  bilinc_forget {key, reason}                   drop a memory from recall
Store only what will matter in later sessions, and never secrets. Take a key from a
bilinc_recall result before you revise or forget. With auto_recall on (the default) each
turn's input is recalled and the memories ride that request; capture_turns additionally
stores each finished turn as a short-lived episodic record.
The bilinc section of config.jsonc names the key (api_key, api_key_env, or BILINC_API_KEY
in the environment) and the endpoint (base_url: https, or http only to localhost)."
  "What (help :bilinc) answers while the cell runs.")

(defun status-line ()
  (format nil "Bilinc memory: ~a at ~a; auto recall ~:[off~;on~], turn capture ~:[off~;on~], ~
               ~d memories recalled this session.~%/~
               /bilinc QUERY searches; the tools bilinc_recall, bilinc_remember, bilinc_revise and ~
               bilinc_forget do the rest."
          (if (configured-p) "key set" "no key")
          (root) (setting :auto-recall) (setting :capture-turns) *recalled*))

(defun run-command (arguments session-id)
  "One /bilinc invocation: a query answers what Bilinc holds, no argument the
state of the cell."
  (declare (ignore session-id))
  (let ((query (nlk:trimmed (or arguments ""))))
    (if (plusp (length query))
        (answering (format-results query (recall-results query :profile "balanced")))
        (status-line))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell bilinc
  (:section ("bilinc")
    (:guide "paste a key from bilinc.space into api_key (or export BILINC_API_KEY); base_url is the endpoint and must be https; auto_recall recalls before each turn; capture_turns stores each finished turn; next step: /bilinc shows the state")
    ("api_key" :secret :doc "the Bilinc Cloud API key; BILINC_API_KEY answers when this is empty")
    ("api_key_env" :env :default "BILINC_API_KEY"
                   :doc "the environment variable holding the key, when it is not written here")
    ("base_url" :string :default "https://bilinc.space"
                :doc "the Bilinc Cloud endpoint: https, or http only to a localhost one")
    ("auto_recall" :boolean :default t
                   :doc "recall the turn's input and inject the memories into that request")
    ("recall_limit" :integer :default 6 :min 1
                     :doc "how many memories one automatic recall injects (at most 20)")
    ("capture_turns" :boolean :default nil
                      :doc "store each finished turn as a short-lived episodic record")
    ("capture_ttl_days" :integer :default 30 :min 1
                         :doc "how many days a captured turn is kept")
    ("timeout_seconds" :integer :default 10 :min 1
                        :doc "one request's ceiling, in seconds"))
  (:help :bilinc "bilinc: bilinc_recall, bilinc_remember, bilinc_revise and bilinc_forget keep versioned facts on Bilinc Cloud" +primer+)
  (:command "bilinc" 'run-command
            :description "Show the Bilinc memory state, or search it"
            :argument-hint "QUERY")
  (:tool "bilinc_recall" (nlk:contract 'recall-call) 'recall-tool)
  (:tool "bilinc_remember" (nlk:contract 'remember-call) 'remember-tool)
  (:tool "bilinc_revise" (nlk:contract 'revise-call) 'revise-tool)
  (:tool "bilinc_forget" (nlk:contract 'forget-call) 'forget-tool)
  (:hook :round (nle:observer #'on-round "nodecode-bilinc"))
  (:hook :frame (nlk:observer #'observe-frame "nodecode-bilinc"))
  (:start (lambda ()
            (check-config)
            (setf *turns* (make-hash-table :test #'equal)
                  *seen* (make-hash-table :test #'equal)
                  *queued* '()
                  *recalled* 0
                  *paused-until* 0)
            (setf *worker* (nlk:worker-start "bilinc-capture" #'capture-step :wake 60))
            (nle:on-stop (lambda ()
                           (nlk:worker-stop *worker*)
                           (setf *worker* nil
                                 *queued* '()
                                 *turns* (make-hash-table :test #'equal)))))))
