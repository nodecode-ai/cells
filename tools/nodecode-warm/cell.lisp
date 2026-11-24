;;;; cell.lisp --- keep an OpenAI-compatible prompt cache warm, from Nodecode.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of mojomast/hermes-session-warmer. After a turn ends,
;;;; the request that turn sent is kept; once the session has been quiet for
;;;; the interval, that exact body is sent again as a one-token,
;;;; tool-free request, so the provider's prompt cache is still warm when the
;;;; next question arrives. Bounded: an allowlist of models, a ceiling on
;;;; requests per idle stretch, a session cap, a payload cap and one timeout.
;;;; Nothing but numeric usage is kept, and the only credential read is the
;;;; variable the section names.

(defpackage #:nodecode-warm (:use #:cl))

(in-package #:nodecode-warm)

(nlk:define-peripheral warm :not-running t)

;;; --- reading the section ------------------------------------------------------

(defun nonblank (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun url-parts (url)
  "(values SCHEME HOST) of an absolute URL, both lowercased, or NILs."
  (let ((mark (and (stringp url) (search "://" url))))
    (when (and mark (plusp mark))
      (let* ((scheme (string-downcase (subseq url 0 mark)))
             (rest (subseq url (+ mark 3)))
             (end (or (position-if (lambda (character) (find character "/?#")) rest)
                      (length rest)))
             (authority (subseq rest 0 end))
             (at (position #\@ authority :from-end t))
             (hostport (if at (subseq authority (1+ at)) authority))
             (host (cond ((and (plusp (length hostport)) (char= #\[ (char hostport 0)))
                          (subseq hostport 0 (1+ (or (position #\] hostport)
                                                      (1- (length hostport))))))
                         (t (subseq hostport 0 (or (position #\: hostport)
                                                    (length hostport)))))))
        (values scheme (string-downcase host))))))

(defun loopback-host-p (host)
  "True for the hosts a plain-HTTP endpoint may live at: literal loopback, or
a name only this machine resolves."
  (let ((host (string-downcase (or host ""))))
    (or (member host '("localhost" "localhost.localdomain" "::1" "[::1]") :test #'string=)
        (uiop:string-prefix-p "127." host)
        (uiop:string-prefix-p "[::1]" host))))

(defun check-endpoint (url)
  "URL, without its trailing slash, or a CONFIG-REFUSAL saying what is wrong.
Only HTTPS, or plain HTTP on loopback, so a conversation is never re-sent in
clear text across the internet."
  (let ((url (nlk:trimmed (or url ""))))
    (when (zerop (length url))
      (nlk:config-error "warm.base_url is empty: give the OpenAI-compatible endpoint whose prompt cache you want kept warm, such as https://api.example.com/v1"))
    (multiple-value-bind (scheme host) (url-parts url)
      (unless scheme
        (nlk:config-error "warm.base_url is ~s; give the whole address, such as https://api.example.com/v1" url))
      (unless (member scheme '("http" "https") :test #'string=)
        (nlk:config-error "warm.base_url scheme ~s is not http or https" scheme))
      (when (and (string= scheme "http") (not (loopback-host-p host)))
        (nlk:config-error "warm.base_url is ~s over plain http, so a conversation would cross the public internet in clear text: use https://" url))
      (string-right-trim "/" url))))

(defun check-models (models)
  "The model ids MODELS names, or a CONFIG-REFUSAL when it names none."
  (let ((models (remove-duplicates
                 (remove-if-not #'nonblank
                                (mapcar (lambda (model) (and (stringp model) (nlk:trimmed model)))
                                        (coerce (or models '()) 'list)))
                 :test #'equal)))
    (unless models
      (nlk:config-error "warm.models is empty: name the model ids whose cache is to be kept warm, and every other model is left alone"))
    models))

(defun read-settings (values table)
  "The declared VALUES, with the endpoint and the model allowlist checked."
  (declare (ignore table))
  (let ((plist (copy-list values)))
    (setf (getf plist :base-url) (check-endpoint (getf values :base-url))
          (getf plist :models) (check-models (getf values :models)))
    plist))

(defun current-settings ()
  "The settings this start runs on: what the chunk, the tick and /warm read.
Signals the cell's own error while it is not running."
  (list :base-url (setting :base-url)
        :models (setting :models)
        :api-key-env (setting :api-key-env)
        :interval-seconds (setting :interval-seconds)
        :idle-window-seconds (setting :idle-window-seconds)
        :max-requests (setting :max-requests)
        :timeout-seconds (setting :timeout-seconds)
        :max-sessions (setting :max-sessions)
        :max-payload-bytes (setting :max-payload-bytes)))

;;; --- the warming request --------------------------------------------------------

(defvar *body-fn* #'nlk:last-request-body
  "The function answering a session's newest request body, as octets. The
kernel keeps the exact bytes of every model call; a test stubs this.")

(defvar *transport* nil
  "The function sending one warming request => a usage plist, or NIL for
NLK:HTTP. A test stubs this; it is read on whichever thread ticks.")

(defun body-text (body)
  "BODY, a session's kept request, as the JSON text it was sent as."
  (cond ((stringp body) body)
        ((and (vectorp body) (plusp (length body)))
         (sb-ext:octets-to-string body :external-format :utf-8))
        (t nil)))

(defun body-model (text)
  "The model id the request TEXT names, or NIL."
  (ignore-errors (nlk:json-value (nlk:decode-json text) :string "model")))

(defun warming-payload (text)
  "TEXT, the body a session sent, as the body this cell sends: the same
history, one completion token, no tools and no stream."
  (let ((object (nlk:decode-json text)))
    (unless (hash-table-p object)
      (fail "the kept request body is not a JSON object"))
    (let ((copy (nlk:copy-json-object object)))
      (dolist (key '("tools" "tool_choice" "stream" "stream_options" "n"
                     "parallel_tool_calls" "logprobs" "top_logprobs"))
        (remhash key copy))
      ;; A reasoning model that took max_completion_tokens keeps taking it.
      (cond ((gethash "max_completion_tokens" copy)
             (setf (gethash "max_completion_tokens" copy) 1))
            (t (setf (gethash "max_tokens" copy) 1)))
      (nlk:encode-json-object copy))))

(defun number-of (object key)
  "OBJECT's KEY as a count, or 0."
  (let ((value (and (hash-table-p object) (gethash key object))))
    (if (and (realp value) (not (minusp value))) (round value) 0)))

(defun usage-of (text)
  "The numeric usage answer TEXT carries, as a plist; zeros for what it omits.
The reply's own text is read and dropped here, never kept."
  (let* ((answer (ignore-errors (nlk:decode-json text)))
         (usage (and (hash-table-p answer) (gethash "usage" answer)))
         (details (and (hash-table-p usage) (gethash "prompt_tokens_details" usage))))
    (list :prompt-tokens (number-of usage "prompt_tokens")
          :completion-tokens (number-of usage "completion_tokens")
          :cached-tokens (number-of details "cached_tokens"))))

(defun api-key (settings)
  "The key the endpoint's requests carry, read from the variable the section
names and from nowhere else."
  (let ((name (nonblank (getf settings :api-key-env))))
    (and name (let ((value (nle::credential-env name)))
                (and (stringp value) (plusp (length value)) value)))))

(defun warming-url (settings)
  (concatenate 'string (getf settings :base-url) "/chat/completions"))

(defun http-transport (settings body)
  "Send BODY, the warming request, to the endpoint SETTINGS names => a usage
plist. Signals on a failed status; the tick counts it and moves on."
  (let* ((key (api-key settings))
         (headers (cons '("content-type" . "application/json")
                        (and key (list (cons "authorization"
                                              (format nil "Bearer ~a" key)))))))
    (multiple-value-bind (text status)
        (nlk:http :post (warming-url settings)
                  :headers headers
                  :content body
                  :timeout (getf settings :timeout-seconds))
      (unless (and (integerp status) (<= 200 status 299))
        (fail "the warming request answered ~a" status))
      (usage-of text))))

(defun send (settings body)
  (funcall (or *transport* #'http-transport) settings body))

;;; --- what is kept, and when it is sent -------------------------------------------

(defstruct (warm-session (:constructor make-warm-session (body due)))
  "One session's warming state: the body to resend, when it is next due, how
many times it has gone, and when the session was last seen."
  (body "" :read-only t)
  (due 0)
  (touched 0)
  (count 0))

(defstruct (warmer (:constructor make-warmer ()))
  (lock (bt:make-lock "nodecode-warm"))
  (sessions (make-hash-table :test #'equal))
  (stopped-p nil)
  (attempts 0) (successes 0) (errors 0) (skipped 0)
  (prompt-tokens 0) (completion-tokens 0) (cached-tokens 0))

(defvar *warmer* nil
  "The running warmer, or NIL while the cell is not started.")

(defmacro with-warmer-lock ((warmer) &body body)
  `(bt:with-lock-held ((warmer-lock ,warmer)) ,@body))

(defun note-usage (warmer usage)
  (incf (warmer-prompt-tokens warmer) (getf usage :prompt-tokens 0))
  (incf (warmer-completion-tokens warmer) (getf usage :completion-tokens 0))
  (incf (warmer-cached-tokens warmer) (getf usage :cached-tokens 0)))

(defun forget-session (session-id)
  "Drop SESSION-ID: its turn has started, so the request it sent before it is
no longer the one to keep warm."
  (when (and *warmer* (stringp session-id))
    (with-warmer-lock (*warmer*)
      (remhash session-id (warmer-sessions *warmer*)))))

(defun capture-session (session-id)
  "Keep SESSION-ID's newest request, when this cell would warm it. Never
signals: a warmer that breaks a turn is worse than one that missed one."
  (let ((settings (ignore-errors (current-settings)))
        (warmer *warmer*))
    (when (and settings warmer (not (warmer-stopped-p warmer)) (stringp session-id))
      (let* ((text (ignore-errors (body-text (funcall *body-fn* session-id))))
             (model (and text (body-model text)))
             (wanted (and model (member model (getf settings :models) :test #'equal))))
        (with-warmer-lock (warmer)
          (let ((sessions (warmer-sessions warmer)))
            (cond
              ((or (null text) (null wanted)
                   (> (length text) (getf settings :max-payload-bytes)))
               (incf (warmer-skipped warmer))
               (remhash session-id sessions))
              ((and (null (gethash session-id sessions))
                    (>= (hash-table-count sessions) (getf settings :max-sessions)))
               (incf (warmer-skipped warmer)))
              (t
               (let ((now (get-universal-time)))
                 (setf (gethash session-id sessions)
                       (make-warm-session (warming-payload text)
                                           (+ now (getf settings :interval-seconds)))
                       (warm-session-touched (gethash session-id sessions)) now))))))))))

(defun note-fact (op)
  "One durable frame: a turn that ended readies its session, one that started
forgets it."
  (multiple-value-bind (type payload metadata turn) (nlk:frame-fact op)
    (declare (ignore payload metadata turn))
    (let ((session-id (getf op :session-id)))
      (when (and (stringp type) (stringp session-id) (plusp (length session-id)))
        (cond ((equal type "turn.completed") (capture-session session-id))
              ((equal type "turn.started") (forget-session session-id)))))))

(defun observe-frame (op next)
  "The :FRAME point: every frame passes untouched; a turn's end readies one
more warming request."
  (let ((answer (funcall next op)))
    (when (getf op :durable-p)
      (nlk:with-handlers ((error () nil))
        (note-fact op)))
    answer))

(defun claim-session (warmer settings)
  "Under the lock: drop every session past its idle window or its request
ceiling, then take the first one due => (values SESSION SESSION-ID)."
  (let ((now (get-universal-time)))
    (with-warmer-lock (warmer)
      (let ((sessions (warmer-sessions warmer))
            (dead '())
            (chosen nil)
            (chosen-id nil))
        (maphash (lambda (session-id session)
                   (cond ((>= (- now (warm-session-touched session))
                              (getf settings :idle-window-seconds))
                          (push session-id dead))
                         ((>= (warm-session-count session) (getf settings :max-requests))
                          (push session-id dead))
                         ((and (null chosen) (<= (warm-session-due session) now))
                          (setf chosen session chosen-id session-id))))
                 sessions)
        (dolist (session-id dead)
          (remhash session-id sessions))
        (when chosen
          ;; In flight: no second thread takes it, and no retry after an error.
          (setf (warm-session-due chosen) most-positive-fixnum)
          (incf (warm-session-count chosen))
          (incf (warmer-attempts warmer)))
        (values chosen chosen-id)))))

(defun require-running ()
  "The running warmer, or the refusal an operator reads: the cell is declared
but its section has not started it in this process."
  (or *warmer*
      (fail "warm is not running: it starts with its section at boot, or with a restart of this cell")))

(defun tick ()
  "One pass: send the one session that is due, and count what came back."
  (let ((warmer (require-running))
        (settings (ignore-errors (current-settings))))
    (when (and warmer settings (not (warmer-stopped-p warmer)))
      (multiple-value-bind (session session-id) (claim-session warmer settings)
        (when session
          (let ((usage (handler-case (send settings (warm-session-body session))
                         (error () nil))))
            (with-warmer-lock (warmer)
              (if usage
                  (let ((now (get-universal-time)))
                    (incf (warmer-successes warmer))
                    (note-usage warmer usage)
                    (setf (warm-session-due session)
                          (+ now (getf settings :interval-seconds))
                          (warm-session-touched session) now))
                  (progn
                    (incf (warmer-errors warmer))
                    (remhash session-id (warmer-sessions warmer)))))))))))

;;; --- what an operator reads --------------------------------------------------------

(defun status-text ()
  "What /warm says: the counters, and what the next request will be."
  (let* ((settings (current-settings))
         (warmer *warmer*))
    (with-warmer-lock (warmer)
      (format nil "warm: ~d sent (~d answered, ~d failed), ~d skipped, ~d session~:p idle · cached ~d of ~d prompt tokens · ~a, models ~{~a~^, ~} · every ~ds, at most ~d per session in ~ds"
              (warmer-attempts warmer)
              (warmer-successes warmer)
              (warmer-errors warmer)
              (warmer-skipped warmer)
              (hash-table-count (warmer-sessions warmer))
              (warmer-cached-tokens warmer)
              (warmer-prompt-tokens warmer)
              (getf settings :base-url)
              (getf settings :models)
              (getf settings :interval-seconds)
              (getf settings :max-requests)
              (getf settings :idle-window-seconds)))))

(defun stop-warming ()
  "Stop sending for this process, and forget every session."
  (let ((warmer *warmer*))
    (when warmer
      (with-warmer-lock (warmer)
        (setf (warmer-stopped-p warmer) t)
        (clrhash (warmer-sessions warmer))))))

(defun run-command (args session-id)
  "The /warm verb: nothing for status, `stop' to end the warming here."
  (declare (ignore session-id))
  (require-running)
  (let ((word (string-downcase (nlk:trimmed (or args "")))))
    (cond ((equal word "stop")
           (stop-warming)
           "warm: stopped. No session is re-sent until this cell is restarted.")
          ((member word '("" "status") :test #'equal)
           (status-text))
          (t "Usage: /warm [status|stop]"))))

(defparameter *primer*
  "warm: keeps a provider's prompt cache warm. When a turn ends, the request that turn sent is kept, and once the session has been quiet for the configured interval it is sent once more with a single completion token and no tools, so the next question still finds its prefix cached and cheaper. Only the models the section lists are ever re-sent, to the endpoint the section names; nothing is sent while a session is taking turns, at most a fixed number of times per idle stretch, and nothing is kept but numeric usage. /warm answers the counters, /warm stop ends it for this process. Each re-send is billed as a request, though a cache hit costs a small fraction of the first one."
  "What the manual says about this cell.")

(nle:define-cell warm
  (:section ("warm")
    (:guide "base_url is the OpenAI-compatible endpoint whose cache is kept warm (e.g. the base URL of the provider you already use), and models are the model ids it may re-send; every other field has a default. Each re-send is a request the endpoint bills; a one-token request against a cached prefix is the cheapest one there is")
    ("base_url" :string :doc "the endpoint whose prompt cache is kept warm, such as https://api.example.com/v1")
    ("models" :list :doc "the model ids to warm; a session sending any other model is left alone")
    ("api_key_env" :env :doc "environment variable holding that endpoint's key, when it needs one")
    ("interval_seconds" :integer :default 240 :min 30
     :doc "how long a session is idle before its request is sent again")
    ("idle_window_seconds" :integer :default 1800 :min 60
     :doc "how long a session keeps being warmed after its last turn")
    ("max_requests" :integer :default 7 :min 1
     :doc "how many times one idle session is re-sent at most")
    ("timeout_seconds" :integer :default 15 :min 1
     :doc "one warming request's ceiling")
    ("max_sessions" :integer :default 4 :min 1
     :doc "how many sessions are kept warm at once")
    ("max_payload_bytes" :integer :default 524288 :min 1024
     :doc "a request larger than this is not warmed"))
  (:settings #'read-settings)
  (:help :warm
   "warm: keep a provider's prompt cache warm between turns - /warm [status|stop]"
   *primer*)
  (:command "warm" 'run-command
            :description "Prompt cache warming: the counters, or stop"
            :argument-hint "[status|stop]")
  (:hook :frame #'observe-frame)
  (:start (lambda ()
            (setf *warmer* (make-warmer))
            (let ((worker (nlk:worker-start "nodecode-warm" #'tick :wake 5)))
              (nle:on-stop (lambda ()
                             (nlk:worker-stop worker)
                             (setf *warmer* nil)))))))
