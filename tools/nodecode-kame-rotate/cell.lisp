;;;; cell.lisp --- KAME's key pool as Nodecode tools and one command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is the provider half of KAME: it hands every one of
;;;; Hermes' provider profiles a client that rotates that provider's keys,
;;;; so one refused request is served by the next key instead of failing
;;;; the turn. Nodecode's provider lanes build their own clients, so this
;;;; rewrite keeps the pool and its one promise as verbs the model and the
;;;; operator call: KAME_CHAT sends one OpenAI-compatible chat completion
;;;; through the pool, KAME_KEYS and /kame say what each key has done, and a
;;;; key told 429, 401, 403 or 5xx sits out for a while instead of being
;;;; asked again.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "kame-rotate": {"base_url": "https://api.example/v1",
;;;;             "key_envs": ["KAME_KEY_1", "KAME_KEY_2"],
;;;;             "model": "...", "cooldown_seconds": 60}
;;;; The keys themselves stay in the environment; the section names them.

(in-package #:nodecode-kame-rotate)

(defparameter +primer+
  "KAME rotation is available through the nodecode-kame-rotate cell: a pool of API keys under one
OpenAI-compatible endpoint, where a key that answers 429, 401, 403 or a 5xx sits out instead of
being asked again.
  kame_chat  {prompt, system?, model?} - one chat completion through the pool: the healthiest
      ready key first, then the next key when one refuses. The answer says which key served it.
  kame_keys  {} - every key's state: ready or cooling and for how long, how many calls it served,
      and the status it last answered.
kame_chat is for a model or an endpoint the operator keeps a key pool for, not the provider
serving this session. Keys stay in the environment variables the `kame-rotate' section names; they are
never printed. A refusal no key can outlast answers ERROR: KAME-ROTATE-ERROR with what the
endpoint said. /kame shows the pool, /kame reset forgets every key's record."
  "What (help :kame) answers while the cell runs.")

;;; --- the pool ---------------------------------------------------------------

(defparameter +rejected-seconds+ 900
  "How long a key told 401 or 403 sits out: it is wrong, not busy.")

(defparameter +clip+ 300
  "How much of a body a refusal quotes.")

(defvar *health* (make-hash-table :test #'equal)
  "Every key's record: :cool-until, :served, :failed, :status, :said.")

(defvar *last-served* nil
  "The key the last successful call used, so the next call starts after it.")

(defun reset-pool ()
  "Forget every key's record: what /kame reset does."
  (setf *health* (make-hash-table :test #'equal)
        *last-served* nil)
  "kame: forgot every key's record.")

(defun pool ()
  "The keys the section's key_envs name, in order, without repeats or empties."
  (remove-duplicates
   (remove "" (mapcar (lambda (name) (or (uiop:getenv name) "")) (setting :key-envs))
           :test #'string=)
   :test #'string=))

(defun state-of (key)
  "KEY's record, made empty on first sight."
  (or (gethash key *health*)
      (setf (gethash key *health*)
            (list :cool-until 0 :served 0 :failed 0 :status nil :said ""))))

(defun cooling-p (key &optional (at (get-universal-time)))
  "Whether KEY sits out at AT."
  (> (getf (state-of key) :cool-until) at))

(defun note-served (key status &aux (state (state-of key)))
  "Record that KEY answered STATUS."
  (setf (getf state :served) (1+ (getf state :served))
        (getf state :status) status
        (getf state :said) ""
        (getf state :cool-until) 0
        *last-served* key)
  key)

(defun note-refused (key status seconds said &aux (state (state-of key)))
  "Record that KEY answered STATUS and sits out SECONDS, saying SAID."
  (setf (getf state :failed) (1+ (getf state :failed))
        (getf state :status) status
        (getf state :said) said
        (getf state :cool-until) (+ (get-universal-time) seconds))
  key)

(defun ordered-keys ()
  "The pool as the next call tries it: the ready keys round-robin after the one
last served; when none is ready, the key whose cooldown ends soonest first."
  (let* ((keys (pool))
         (ready (remove-if #'cooling-p keys)))
    (if (null ready)
        (stable-sort (copy-list keys) #'<
                     :key (lambda (key) (getf (state-of key) :cool-until)))
        (let* ((start (or (position *last-served* keys :test #'string=) -1))
               (count (length keys)))
          (loop for step from 0 below count
                for key = (nth (mod (+ start 1 step) count) keys)
                when (member key ready :test #'string=) collect key)))))

;;; --- the wire ---------------------------------------------------------------

(defun base-url ()
  "The section's endpoint, trimmed."
  (string-right-trim "/" (or (setting :base-url) "")))

(defun endpoint ()
  "The chat-completions URL of the section's base URL."
  (let ((base (base-url)))
    (if (or (search "/chat/completions" base :from-end t)
            (search "/completions" base :from-end t))
        base
        (format nil "~a/chat/completions" base))))

(defun body-string (body)
  "BODY, dex's string or octets, as text."
  (typecase body
    (string body)
    ((vector (unsigned-byte 8)) (sb-ext:octets-to-string body :external-format :utf-8))
    (null "")
    (t (princ-to-string body))))

(defun header (headers name)
  "The value HEADERS carries for NAME, a hash table or an alist."
  (cond ((hash-table-p headers)
         (or (gethash name headers)
             (gethash (string-capitalize name) headers)
             (loop for key being the hash-keys of headers using (hash-value value)
                   when (string-equal key name) return value)))
        ((listp headers)
         (cdr (assoc name headers :test #'string-equal)))))

(defun exchange (key content seconds)
  "One POST to the endpoint with KEY: (values TEXT STATUS HEADERS); a status is a
value, a transport failure reads TEXT NIL and STATUS :TRANSPORT."
  (handler-case
      (sb-sys:with-deadline (:seconds seconds)
        (multiple-value-bind (body status headers)
            (handler-case
                (dex:post (endpoint)
                          :headers (list (cons "authorization" (format nil "Bearer ~a" key))
                                         (cons "content-type" "application/json"))
                          :content content
                          :connect-timeout seconds :read-timeout seconds
                          :use-connection-pool nil)
              (dex:http-request-failed (condition)
                (values (dex:response-body condition)
                        (dex:response-status condition)
                        (dex:response-headers condition))))
          (values (body-string body) status headers)))
    ((or error sb-sys:deadline-timeout) (condition)
      (declare (ignore condition))
      (values nil :transport nil))))

(defun refusal-said (text)
  "The endpoint's own words on a refusal: error.message, else its body, clipped."
  (let ((json (ignore-errors (nlk:decode-json text))))
    (or (nlk:json-value json :string "error" "message")
        (nlk:json-value json :string "message")
        (nlk:json-value json :string "error")
        (subseq (body-string text) 0 (min (length (body-string text)) +clip+)))))

(defun answer-text (text)
  "The assistant's content in a chat-completions body, else the body clipped."
  (let* ((json (ignore-errors (nlk:decode-json text)))
         (choices (nlk:json-value json :array "choices"))
         (first (and choices (plusp (length choices)) (aref choices 0)))
         (content (and first (nlk:json-value first :string "message" "content"))))
    (if (and content (plusp (length content)))
        content
        (format nil "~a" (subseq (body-string text) 0 (min (length (body-string text)) +clip+))))))

(defun ok-p (status)
  "Whether STATUS is a 2xx."
  (and (integerp status) (<= 200 status 299)))

(defun retry-after (headers fallback)
  "The seconds HEADERS name in Retry-After, else FALLBACK."
  (let ((value (header headers "retry-after")))
    (or (and value (parse-integer value :junk-allowed t)) fallback)))

(defun request-content (model system prompt)
  "The chat-completions request body for PROMPT."
  (nlk:encode-json-object
   (nlk:make-json-object
    "model" model
    "messages" (coerce (list (nlk:make-json-object "role" "system" "content" system)
                               (nlk:make-json-object "role" "user" "content" prompt))
                         'vector))))

;;; --- the call ---------------------------------------------------------------

(defun rotate (prompt system model)
  "One chat completion through the pool: the first key that answers is the
answer; a key that refuses sits out and the next one is asked."
  (let ((url (base-url)))
    (when (zerop (length url))
      (fail "base_url is not set: name the OpenAI-compatible endpoint in the kame-rotate section (Set it up in /settings)"))
    (let ((keys (ordered-keys)))
      (when (null keys)
        (fail "no key: key_envs names no environment variable holding one, or none of them is set"))
      (let ((content (request-content (or model (setting :model)) system prompt))
            (seconds (setting :timeout-seconds))
            (cooldown (setting :cooldown-seconds))
            (tried '())
            (last-said ""))
        (block attempt
          (loop for key in keys
                for index from 1
                do (multiple-value-bind (text status headers) (exchange key content seconds)
                     (cond
                       ((null text)
                        (note-refused key :transport cooldown "the endpoint did not answer")
                        (setf last-said "the endpoint did not answer")
                        (push (format nil "key ~d: no answer" index) tried))
                       ((ok-p status)
                        (note-served key status)
                        (return-from attempt
                          (format nil "~a~%~%(kame: key ~d of ~d served this call)"
                                  (answer-text text) index (length keys))))
                       ((= status 429)
                        (let ((wait (retry-after headers cooldown)))
                          (note-refused key status wait (refusal-said text))
                          (setf last-said (refusal-said text))
                          (push (format nil "key ~d: 429, sits out ~ds" index wait) tried)))
                       ((member status '(401 403))
                        (note-refused key status +rejected-seconds+ (refusal-said text))
                        (setf last-said (refusal-said text))
                        (push (format nil "key ~d: ~d, sits out ~ds" index status +rejected-seconds+)
                              tried))
                       ((and (integerp status) (<= 500 status 599))
                        (note-refused key status cooldown (refusal-said text))
                        (setf last-said (refusal-said text))
                        (push (format nil "key ~d: ~d, sits out ~ds" index status cooldown) tried))
                       (t
                        (fail "~a refused ~@[~a: ~]~a"
                              (endpoint)
                              status
                              (refusal-said text))))))
          (fail "every key refused (~a): ~a" (format nil "~{~a~^, ~}" (nreverse tried)) last-said))))))

;;; --- the operator's and the model's reads ------------------------------------

(defun status ()
  "One line per key: its state, what it served and what it last answered."
  (let ((keys (pool))
        (at (get-universal-time)))
    (with-output-to-string (out)
      (if (null keys)
          (format out "kame: no key — an environment variable key_envs names is not set.~%")
          (format out "kame: ~d key~:p for ~a~%" (length keys) (endpoint)))
      (loop for key in keys
            for index from 1
            for state = (state-of key)
            for left = (- (getf state :cool-until) at)
            for said = (getf state :said)
            do (format out "  ~d. ~a~@[ (~ds)~], served ~d, failed ~d, last ~@[~a~]~@[: ~a~]~%"
                       index
                       (if (plusp left) "cooling" "ready")
                       (and (plusp left) left)
                       (getf state :served)
                       (getf state :failed)
                       (getf state :status)
                       (and (stringp said) (plusp (length said))
                            (subseq said 0 (min (length said) 120))))))))

(defun run-command (args session-id)
  "One /kame invocation: status, or reset."
  (declare (ignore session-id))
  (let ((words (string-downcase (nlk:trimmed (or args "")))))
    (cond ((member words '("" "status") :test #'equal) (status))
          ((equal words "reset") (reset-pool))
          (t (format nil "kame: /kame status shows the pool, /kame reset forgets every key's record — not ~s"
                     words)))))

(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES -- (NAME TYPE DOC REQUIRED-P) ... -- as a tool schema."
  (list :description description
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties" (apply #'nlk:make-json-object
                              (loop for (name type doc) in properties
                                    nconc (list name (nlk:make-json-object "type" type
                                                                            "description" doc))))
         "required" (coerce (loop for property in properties
                                    when (fourth property) collect (first property))
                              'vector))))

(defparameter *chat-schema*
  (tool-schema
   "One chat completion (OpenAI-compatible chat completions) through KAME's pool of API keys: the healthiest ready key first, the next key when one is refused instead of failing the call. Use it for a model whose endpoint the `kame-rotate' section names."
   '(("prompt" "string" "What to ask, as one user message." t)
     ("system" "string" "A system message to put before it.")
     ("model" "string" "The model id; empty asks the one the section names.")))
  "KAME_CHAT.")

(defparameter *keys-schema*
  (tool-schema
   "Every key in KAME's pool: ready or cooling and for how long, calls served, calls failed, and the status it last answered. Read-only."
   '())
  "KAME_KEYS.")

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the `ERROR:' line the model repairs from."
  `(handler-case (progn ,@body)
     (kame-rotate-error (condition) (nle:failure (format nil "ERROR: ~a" condition)))
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: kame: ~a" condition)))))

(defun tool-chat (args)
  "The KAME_CHAT tool: ARGS is the decoded arguments."
  (answering
    (let ((prompt (nlk:json-value args :string "prompt")))
      (when (or (null prompt) (zerop (length (nlk:trimmed prompt))))
        (fail "prompt is empty: say what to ask"))
      (rotate prompt
              (or (nlk:json-value args :string "system") "")
              (nlk:json-value args :string "model")))))

(defun tool-keys (args)
  "The KAME_KEYS tool: ARGS is ignored."
  (declare (ignore args))
  (answering (status)))

(defun start ()
  "A fresh start sees no key's history: what the last run learned is not this one's."
  (reset-pool)
  (nle:on-stop (lambda () (clrhash *health*) (setf *last-served* nil))))

(nle:define-cell kame-rotate
  (:section ("kame-rotate")
    (:guide "base_url is the OpenAI-compatible endpoint you hold keys for; key_envs names the environment variables that hold the keys, in preference order; a key told 429, 401, 403 or a 5xx sits out cooldown_seconds before it is asked again")
    ("base_url" :string :required t
     :doc "the OpenAI-compatible base URL, .../v1; /chat/completions is appended unless it is already there")
    ("key_envs" :list :required t
     :doc "environment variables holding the API keys, in preference order")
    ("model" :string :default "gpt-4o-mini"
     :doc "the model a call asks for when it names none")
    ("cooldown_seconds" :integer :default 60 :min 1
     :doc "how long a key that was refused sits out (Retry-After outranks it)")
    ("timeout_seconds" :integer :default 60 :min 1
     :doc "one key's call ceiling"))
  (:tool "kame_chat" *chat-schema* 'tool-chat)
  (:tool "kame_keys" *keys-schema* 'tool-keys)
  (:help :kame "kame: one chat completion through a rotating pool of API keys" +primer+)
  (:command "kame" 'run-command
            :description "KAME's key pool: status, or reset"
            :argument-hint "status|reset")
  (:start #'start))
