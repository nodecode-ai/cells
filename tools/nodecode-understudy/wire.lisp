;;;; wire.lisp --- the one network seam, and the reading of what comes back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every byte this cell moves goes through POST-CHAT, so a test stubs
;;;; NLK:HTTP and never touches the network. The endpoint is any
;;;; OpenAI-compatible chat-completions API, so one key serves the candidates
;;;; and the judge alike. A refusal is UNDERSTUDY-ERROR with the service's own
;;;; message in it, which is what the model repairs from.

(in-package #:nodecode-understudy)

(defparameter +default-base-url+ "https://openrouter.ai/api/v1"
  "An OpenAI-compatible endpoint, used when the section names none.")

(defun nonempty (value)
  "VALUE when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) value))

(defun setting-api-key ()
  "The key the section names inline or by variable, else the endpoint's own."
  (or (nonempty (setting :api-key))
      (let ((variable (nonempty (setting :api-key-env))))
        (and variable (nonempty (uiop:getenv variable))))
      (nonempty (uiop:getenv "OPENROUTER_API_KEY"))))

(defun judge-model ()
  "The model that judges a replay, or NIL when the section names none."
  (nonempty (setting :judge-model)))

(defun check-config ()
  "Refuse a section this cell could not run on, naming the member to fix."
  (unless (judge-model)
    (error 'nlk:config-refusal
           :detail (concatenate
                    'string
                    "understudy: set \"judge_model\" in the understudy section to the model "
                    "id that judges a replay, e.g. \"anthropic/claude-sonnet-5\"; it is "
                    "served by the same base_url as the candidates")))
  (unless (setting-api-key)
    (error 'nlk:config-refusal
           :detail (concatenate
                    'string
                    "understudy: this cell needs an API key: set \"api_key\", or name the "
                    "environment variable holding it in \"api_key_env\", or export "
                    "OPENROUTER_API_KEY"))))

(defun root ()
  "The endpoint this cell talks to, without a trailing slash."
  (let ((given (string-right-trim "/" (or (setting :base-url) ""))))
    (if (plusp (length given)) given +default-base-url+)))

(defun auth-headers ()
  "The one header every request carries."
  (list (cons "authorization" (format nil "Bearer ~a" (or (setting-api-key) "")))))

(defun bolt (&rest pairs)
  "A JSON object from PAIRS (KEY VALUE ...), leaving out every NIL VALUE."
  (loop for (key value) on pairs by #'cddr
        when value append (list key value) into kept
        finally (return (apply #'nlk:make-json-object kept))))

(defun wire-messages (messages)
  "MESSAGES — a list of (ROLE . TEXT) pairs — as a JSON array of message objects."
  (coerce (loop for (role . text) in messages
                collect (nlk:make-json-object "role" role "content" text))
          'vector))

(defun decode-body (answer)
  "ANSWER — an HTTP body — as a decoded value; an empty body is an empty object."
  (cond ((stringp answer)
         (if (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) answer)))
             (handler-case (nlk:decode-json answer) (error () (nlk:json-object)))
             (nlk:json-object)))
        ((vectorp answer) answer)
        (t (nlk:json-object))))

(defun body-error-text (answer)
  "What a refused answer says, in the one line the model reads."
  (let ((body (and (stringp answer) (ignore-errors (nlk:decode-json answer)))))
    (or (nlk:json-value body :text "error" "message")
        (nlk:json-value body :text "detail")
        (nlk:json-value body :text "message")
        (and (stringp answer) (subseq answer 0 (min 200 (length answer))))
        "no body")))

(defun answer-text (body)
  "BODY's first choice's message content, or the empty string."
  (let* ((choices (nlk:json-value body :array "choices"))
         (first (and choices (plusp (length choices)) (aref choices 0))))
    (or (nlk:json-value first :text "message" "content") "")))

(defun post-chat (model messages)
  "One chat-completions call to MODEL => (values TEXT USAGE), USAGE a decoded
object or NIL. A transport failure or a refused status signals UNDERSTUDY-ERROR."
  (let* ((url (concatenate 'string (root) "/chat/completions"))
         (timeout (setting :timeout-seconds))
         (body (bolt "model" model
                     "messages" (wire-messages messages)
                     "temperature" 0))
         (answer nil)
         (status nil))
    (handler-case
        (multiple-value-setq (answer status)
          (nlk:http :post url
                    :headers (cons '("content-type" . "application/json") (auth-headers))
                    :content (nlk:encode-json-object body)
                    :timeout timeout
                    :connect-timeout (min 10 timeout)))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "understudy: ~a: ~a" model (nle:transport-failure-label condition url))))
    (unless (and (integerp status) (< status 300))
      (fail "understudy: ~a: HTTP ~d~@[: ~a~]" model status (body-error-text answer)))
    (let ((decoded (decode-body answer)))
      (values (answer-text decoded)
              (nlk:json-value decoded :object "usage")))))

(defun usage-count (usage key)
  "USAGE's integer KEY, or 0."
  (or (nlk:json-value usage :integer key) 0))

(defun usage-cost (usage)
  "The cost USAGE reports, when the endpoint reports one as a number."
  (let ((cost (nlk:json-value usage :any "cost")))
    (and (numberp cost) (float cost 1d0))))
