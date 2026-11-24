;;;; wire.lisp --- the one network seam, and the reading of what comes back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every byte this cell moves goes through GHOST-CHAT, so a test stubs the
;;;; one name and never touches the network. The original talked to three
;;;; OpenAI endpoints (realtime transcription, chat, TTS) from its bridge and
;;;; to Twilio's REST API for the phone legs; what a text-only coach needs is
;;;; the one chat call: a hint line, and the post-call summary.

(in-package #:nodecode-ghost-call)

(defparameter +base-url+ "https://api.openai.com/v1"
  "OpenAI's API, where every call goes unless the section names another root.")

(defparameter +default-model+ "gpt-4.1-mini"
  "The original's default fast model for hints and the summary.")

(defparameter +hint-timeout-s+ 5
  "One hint call's ceiling. The original budgeted 2.5 s mid-call; a typed line
can wait a little longer, and a slow hint is skipped rather than queued.")

(defun nonempty (value)
  "VALUE when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value)))
       (nlk:trimmed value)))

(defun api-key ()
  "The key the section names inline or by variable, else OPENAI_API_KEY."
  (or (nonempty (setting :api-key))
      (let ((variable (nonempty (setting :api-key-env))))
        (and variable (nonempty (uiop:getenv variable))))
      (nonempty (uiop:getenv "OPENAI_API_KEY"))))

(defun check-config ()
  "Refuse a section that would reach the model with no key, naming the member."
  (unless (api-key)
    (error 'nlk:config-refusal
           :detail (concatenate 'string
                                "ghost-call: this cell needs a key: set OPENAI_API_KEY, or "
                                "\"api_key\" (or \"api_key_env\") in the ghost-call section; "
                                "a key is made at platform.openai.com/api-keys"))))

(defun root ()
  "The API root this cell talks to, without a trailing slash."
  (let ((given (string-right-trim "/" (or (setting :base-url) ""))))
    (if (plusp (length given)) given +base-url+)))

(defun auth-headers ()
  "A bearer key, and the JSON content type every call here sends."
  (list (cons "authorization" (format nil "Bearer ~a" (or (api-key) "")))
        (cons "content-type" "application/json")))

(defun decode-body (answer)
  "ANSWER — an HTTP body — as a decoded value; an empty body is an empty object."
  (cond ((stringp answer)
         (if (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) answer)))
             (handler-case (nlk:decode-json answer) (error () (nlk:json-object)))
             (nlk:json-object)))
        ((hash-table-p answer) answer)
        (t (nlk:json-object))))

(defun body-error-text (answer)
  "What a refused answer says, in the one line the model reads."
  (let ((body (and (stringp answer) (ignore-errors (nlk:decode-json answer)))))
    (or (nlk:json-value (nlk:json-value body :object "error") :text "message")
        (nlk:json-value body :text "message")
        (nlk:json-value body :text "error")
        (and (stringp answer) (subseq answer 0 (min 200 (length answer)))))))

(defun message (role content)
  "One chat message, as the wire carries it."
  (nlk:make-json-object "role" role "content" content))

(defun choices-content (body)
  "BODY — a /chat/completions answer — as the first choice's text, or NIL."
  (let* ((choices (nlk:json-value body :array "choices"))
         (first (and choices (plusp (length choices)) (aref choices 0))))
    (and first (nlk:json-value (nlk:json-value first :object "message") :text "content"))))

(defun ghost-chat (messages &key (max-tokens 60) (temperature 0.2)
                              (timeout +hint-timeout-s+) json-mode)
  "POST MESSAGES — a list of MESSAGE objects — to the chat endpoint => the
first choice's text. A transport failure or a refused status signals
GHOST-CALL-ERROR, which is what the model repairs from."
  (let* ((url (concatenate 'string (root) "/chat/completions"))
         (body (apply #'nlk:make-json-object
                      (append (list "model" (or (setting :model) +default-model+)
                                    "messages" (coerce messages 'vector)
                                    "max_completion_tokens" max-tokens)
                              (unless (search "o1" (or (setting :model) "") :test #'char-equal)
                                (list "temperature" temperature))
                              (when json-mode
                                (list "response_format"
                                      (nlk:make-json-object "type" "json_object"))))))
         (answer nil)
         (status nil))
    (handler-case
        (multiple-value-setq (answer status)
          (nlk:http :post url
                    :headers (auth-headers)
                    :content (nlk:encode-json-object body)
                    :timeout timeout
                    :connect-timeout (min 10 timeout)))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "ghost-call: could not reach ~a: ~a" (root)
              (nle:transport-failure-label condition url))))
    (cond ((and (integerp status) (< status 300))
           (or (choices-content (decode-body answer))
               (fail "ghost-call: ~a answered no text" (root))))
          ((and (integerp status) (= status 401))
           (fail "ghost-call: HTTP 401, the key was refused; check OPENAI_API_KEY or the \
ghost-call section's api_key"))
          ((and (integerp status) (= status 429))
           (fail "ghost-call: HTTP 429, rate or quota; wait before the next call"))
          (t (fail "ghost-call: HTTP ~a~@[: ~a~]" status (body-error-text answer))))))
