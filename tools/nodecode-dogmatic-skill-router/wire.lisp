;;;; wire.lisp --- one typed question set, three ways to ask it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; TypeSafe's System One answers a map of named questions about one state
;;;; with a distribution each. Hermes's plugin asked it directly, through
;;;; OpenRouter's Decisions API, or through the Vercel AI Gateway; the same
;;;; three routes are here, picked by `backend` (auto: the first with a key).
;;;;
;;;;   typesafe    POST {base}/v1/systemone        noul / choice / score
;;;;   openrouter  POST {base}/decisions           noul / choice / score
;;;;   gateway     POST {base}/evaluation-model    boolean / choice / score
;;;;
;;;; A yes/no question is a boolean here and the wire's noul on the two
;;;; direct routes; an answer is read for either spelling, so one reader
;;;; serves all three. Every failure returns NIL (fail-open): a router that
;;;; cannot answer must never fail a turn.

(in-package #:nodecode-dogmatic-skill-router)

(defparameter +typesafe-base+ "https://api.typesafe.ai"
  "TypeSafe's public API root (DEFAULT_TYPESAFE_BASE_URL).")

(defparameter +typesafe-model+ "jev-latest"
  "The judgment model the direct route asks (DEFAULT_TYPESAFE_MODEL).")

(defparameter +openrouter-base+ "https://openrouter.ai/api/alpha"
  "OpenRouter's Decisions API root (DEFAULT_OPENROUTER_BASE_URL).")

(defparameter +openrouter-model+ "~typesafe/jev-latest"
  "The model the OpenRouter route names (DEFAULT_OPENROUTER_MODEL).")

(defparameter +gateway-base+ "https://ai-gateway.vercel.sh/v4/ai"
  "The Vercel AI Gateway root (DEFAULT_BASE_URL).")

(defparameter +gateway-model+ "typesafe-ai/jev"
  "The gateway's prefixed model id (DEFAULT_MODEL).")

(defparameter +key-page+ "https://console.typesafe.ai/"
  "Where a key is made.")

(defparameter +protocol+ "0.0.1"
  "The gateway's protocol version header (PROTOCOL_VERSION).")

(defparameter +spec+ "4"
  "The evaluation-model specification version header (SPEC_VERSION).")

(defparameter +max-payload+ 65536
  "The largest request body the cell sends (MAX_PAYLOAD_BYTES); past it, silence.")

(defparameter +none+ "none_of_these"
  "The option a chunk nominates when nothing in it fits (NONE_OPTION).")

(defparameter +none-pressure+ 0.5d0
  "A chunk whose P(none) reaches this nominates nobody (NONE_THRESHOLD).")

;;; --- where, and with which key ------------------------------------------------

(defun nonblank (value)
  "VALUE trimmed when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) (nlk:trimmed value)))

(defun env (name)
  "NAME's value from the environment (NLE:CREDENTIAL-ENV), or NIL."
  (nonblank (ignore-errors (nle::credential-env name))))

(defun saved-key (provider)
  "The key auth.json keeps under api_keys.PROVIDER, or NIL; an unreadable
store keeps none."
  (handler-case (nonblank (nle::auth-api-key (nle::read-auth-file nle::*auth-file-path*) provider))
    (error () nil)))

(defun api-key (backend)
  "The key BACKEND is asked with: the saved one over the environment, as a
chat provider's saved key outranks its variable."
  (case backend
    (:typesafe (or (saved-key "typesafe") (env "TYPESAFE_API_KEY")))
    (:openrouter (or (saved-key "openrouter") (env "OPENROUTER_API_KEY")))
    (:gateway (env "AI_GATEWAY_API_KEY"))))

(defun keys-in-hand ()
  "Every key this cell would send: what a refusal's text is cleaned of."
  (remove nil (list (saved-key "typesafe") (env "TYPESAFE_API_KEY")
                    (saved-key "openrouter") (env "OPENROUTER_API_KEY")
                    (env "AI_GATEWAY_API_KEY"))))

(defun resolve-backend ()
  "The backend a decision goes to, or NIL when the wanted one has no key:
:auto takes the first of typesafe, openrouter, gateway that has one."
  (flet ((has (backend) (and (api-key backend) t)))
    (let ((want (setting :backend)))
      (cond ((equal want "typesafe") (and (has :typesafe) :typesafe))
            ((equal want "openrouter") (and (has :openrouter) :openrouter))
            ((equal want "gateway") (and (has :gateway) :gateway))
            ((has :typesafe) :typesafe)
            ((has :openrouter) :openrouter)
            ((has :gateway) :gateway)))))

(defun defaulted (key variable fallback)
  "The section's KEY when it is set, else VARIABLE, else FALLBACK."
  (or (nonblank (setting key)) (env variable) fallback))

(defun typesafe-base () (string-right-trim "/" (defaulted :base-url "TYPESAFE_BASE_URL" +typesafe-base+)))
(defun typesafe-model () (defaulted :model "TYPESAFE_DEFAULT_MODEL" +typesafe-model+))
(defun openrouter-base () (string-right-trim "/" (defaulted :openrouter-base-url "OPENROUTER_BASE_URL" +openrouter-base+)))
(defun openrouter-model () (defaulted :openrouter-model "OPENROUTER_MODEL" +openrouter-model+))
(defun gateway-base () (string-right-trim "/" (defaulted :gateway-base-url "AI_GATEWAY_BASE_URL" +gateway-base+)))
(defun gateway-model () (defaulted :gateway-model "AI_GATEWAY_MODEL" +gateway-model+))

(defun endpoint (backend)
  "The address BACKEND's questions are asked at."
  (ecase backend
    (:typesafe (format nil "~a/v1/systemone" (typesafe-base)))
    (:openrouter (format nil "~a/decisions" (openrouter-base)))
    (:gateway (format nil "~a/evaluation-model" (gateway-base)))))

(defun headers (backend key)
  "What a request to BACKEND carries."
  `(("authorization" . ,(format nil "Bearer ~a" key))
    ("accept" . "application/json")
    ("content-type" . "application/json")
    ,@(when (eq backend :gateway)
        `(("ai-gateway-protocol-version" . ,+protocol+)
          ("ai-evaluation-model-specification-version" . ,+spec+)
          ("ai-model-id" . ,(gateway-model))))))

;;; --- staying out of the way when the far side is unwell ------------------------

(defparameter +breaker-threshold+ 3
  "Consecutive failures before the cell goes silent (BREAKER_THRESHOLD).")

(defparameter +breaker-cooldown+ 120
  "Seconds of silence once the breaker opens (BREAKER_COOLDOWN_S).")

(defvar *breaker* (cons 0 0.0)
  "(FAILURES . OPEN-UNTIL) — one process's record; a whole turn is not worth a stall.")

(defvar *breaker-lock* (bt2:make-lock :name "dogmatic-skill-router breaker"))

(defun breaker-open-p ()
  "Whether the last +BREAKER-THRESHOLD+ calls all failed and the cooldown has not passed."
  (bt2:with-lock-held (*breaker-lock*)
    (and (>= (car *breaker*) +breaker-threshold+)
         (< (get-internal-real-time) (cdr *breaker*)))))

(defun note-failure ()
  "Count one failed call; enough in a row opens the breaker for the cooldown."
  (bt2:with-lock-held (*breaker-lock*)
    (when (>= (incf (car *breaker*)) +breaker-threshold+)
      (setf (cdr *breaker*) (+ (get-internal-real-time) (* +breaker-cooldown+ internal-time-units-per-second))))))

(defun note-answer ()
  "Clear the failure count: the far side answered."
  (bt2:with-lock-held (*breaker-lock*)
    (setf (car *breaker*) 0 (cdr *breaker*) 0.0)))

;;; --- the question set, as the wire's ------------------------------------------

(defun wire-question (question backend)
  "QUESTION, this cell's plist, as the wire's object for BACKEND."
  (if (eq (getf question :type) :choice)
      (nlk:json-object "type" "choice"
                        "instructions" (getf question :instructions)
                        "criteria" (or (getf question :criteria) (nlk:make-json-object)))
      (nlk:json-object "type" (if (eq backend :gateway) "boolean" "noul")
                        "instructions" (getf question :instructions))))

(defun payload (backend text questions)
  "The request body for BACKEND: the same state and question set every route asks."
  (let ((wire (make-hash-table :test #'equal))
        (state (nlk:json-object "request" text "recent_context" "")))
    (maphash (lambda (id question) (setf (gethash id wire) (wire-question question backend)))
             questions)
    (case backend
      (:typesafe (nlk:json-object "state" state "model" (typesafe-model) "questions" wire))
      (:openrouter (nlk:json-object "model" (openrouter-model) "state" state "questions" wire))
      (t (nlk:json-object "state" state "questions" wire)))))

;;; --- one exchange ---------------------------------------------------------------

(defun evaluate (text questions)
  "Ask TEXT's questions of the resolved backend. => (:answers HASH) or NIL;
never signals, never waits past the section's timeout."
  (when (and (not (breaker-open-p)) (ignore-errors (resolve-backend)))
    (handler-case
        (let* ((backend (resolve-backend))
               (key (api-key backend))
               (body (nlk:encode-json-object (payload backend text questions)))
               (timeout (setting :timeout)))
          (cond ((null key) nil)
                ((> (length body) +max-payload+) nil)
                (t (multiple-value-bind (answer status)
                       (nlk:http :post (endpoint backend) :headers (headers backend key)
                                 :content body :timeout timeout :connect-timeout timeout)
                     (if (and (integerp status) (<= 200 status 299))
                         (progn
                           (note-answer)
                           (let ((decoded (ignore-errors (nlk:decode-json (nlk:body-text answer)))))
                             (and (hash-table-p decoded)
                                  (list :answers (nlk:json-value decoded :object "answers")))))
                         (progn (note-failure) nil))))))
      (error () (note-failure) nil))))

;;; --- reading an answer -----------------------------------------------------------

(defun answer-at (answers id)
  "ANSWERS' entry for ID, or NIL."
  (and (hash-table-p answers) (gethash id answers)))

(defun number-at (object key)
  "OBJECT's KEY as a float, JSON true as 1 and false as 0, else NIL."
  (let ((value (nlk:json-value object :any key)))
    (cond ((eq value t) 1d0)
          ((eq value :null) nil)
          ((realp value) (float value 1d0))
          (t nil))))

(defun answer-probability (answer)
  "A yes/no ANSWER as its probability of yes: the gateway's `probability',
the two direct routes' `noul'."
  (or (number-at answer "probability") (number-at answer "noul")))

(defun answer-choice (answer)
  "A choice ANSWER's most probable option, or NIL."
  (nlk:json-value answer :string "choice"))

(defun answer-probabilities (answer)
  "A choice ANSWER's option -> probability object, or NIL."
  (nlk:json-value answer :object "probabilities"))

(defun probe-answer ()
  "One judgment asked and read: what /dogmatic-skill-router check reports, or NIL."
  (let ((questions (make-hash-table :test #'equal)))
    (setf (gethash "up" questions)
          (list :type :boolean
                :instructions "Is this a health check of a skill router?"))
    (let* ((result (evaluate "A health check of the skill router." questions))
           (probability (and result (answer-probability (answer-at (getf result :answers) "up")))))
      probability)))
