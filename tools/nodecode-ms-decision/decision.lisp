;;;; decision.lisp --- Microsoft-Decision-1 on Azure AI Foundry.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of ms-decision-provider's provider.py (see NOTICE). One POST
;;;; {base}/providers/microsoft/v1/systemone asks a map of named, typed
;;;; questions about one state and answers each with a distribution:
;;;;
;;;;   {"model": "ms-decision-1", "state": ...,
;;;;    "questions": {"<id>": {"type": "noul"|"choice"|"score",
;;;;                           "instructions": "...", "criteria": ...}}}
;;;;   => {"answers": {"<id>": {"type": ..., "noul" | choice+probabilities
;;;;       | score+probabilities, "confidence"?}}, "usage": {...}}
;;;;
;;;; A noul question answers the probability of yes; a choice its most
;;;; probable option and every option's probability; a score a
;;;; probability-weighted level over the levels given, lowest first. The whole
;;;; distribution is carried back as the answer, never a bare label: the caller
;;;; owns the threshold. The key is Azure Foundry's api-key header, and the
;;;; endpoint is the operator's own resource: nothing is sent until both are
;;;; set, and a base that is not an https Azure OpenAI resource host is refused
;;;; before a credential or a state could go anywhere else.

(in-package #:nodecode-ms-decision)

(defparameter +route+ "/providers/microsoft/v1/systemone"
  "The System One route under the resource host (provider.py:4).")

(defparameter +model-env+ "MS_DECISION_MODEL"
  "The environment variable a deployment name is read from.")

(defparameter +default-model+ "ms-decision-1"
  "The deployment name when nothing names one (provider.py:34).")

(defparameter +key-env+ "AZURE_FOUNDRY_API_KEY"
  "The environment variable the Foundry key is read from.")

(defparameter +base-env+ "AZURE_FOUNDRY_BASE_URL"
  "The environment variable the resource host is read from.")

(defparameter +key-page+ "https://ai.azure.com"
  "Where the key and the endpoint are made.")

(defparameter +host-suffix+ ".openai.azure.com"
  "The only host shape a request goes to (provider.py:99-104).")

;;; --- where, which, with what ---------------------------------------------------

(defun nonblank (value)
  "VALUE trimmed when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) (nlk:trimmed value)))

(defun azure-host (raw)
  "RAW as https://<resource>.openai.azure.com, or NIL when it is anything else: a scheme that is not https, a domain the operator does not own, userinfo, a port, a query, a fragment or a path beyond /openai/v1 would send the key and the state somewhere the plugin never did."
  (let* ((prefix "https://")
         (rest (and (stringp raw) (uiop:string-prefix-p prefix raw) (subseq raw (length prefix))))
         (slash (and rest (position #\/ rest)))
         (host (and rest (string-downcase (if slash (subseq rest 0 slash) rest))))
         (path (if slash (subseq rest slash) "")))
    (when (and rest (plusp (length host))
               (uiop:string-suffix-p host +host-suffix+)
               (not (find #\@ rest)) (not (find #\: rest))
               (not (find #\? rest)) (not (find #\# rest))
               (member (string-right-trim "/" path) '("" "/openai" "/openai/v1") :test #'equal))
      (let ((label (subseq host 0 (- (length host) (length +host-suffix+)))))
        (when (and (plusp (length label))
                   (every (lambda (c)
                            (and (< (char-code c) 128)
                                 (or (alphanumericp c) (char= c #\-))))
                          label))
          (format nil "~a~a" prefix host))))))

(defun base-url ()
  "The section's base_url, else AZURE_FOUNDRY_BASE_URL, else NIL: the resource host a request goes to, already validated."
  (azure-host (or (nonblank (setting :base-url))
                   (nonblank (nle::credential-env +base-env+))
                   "")))

(defun model ()
  "The section's model, else MS_DECISION_MODEL, else ms-decision-1."
  (or (nonblank (setting :model))
      (nonblank (nle::credential-env +model-env+))
      +default-model+))

(defun timeout ()
  "Seconds one decision may take: the section's timeout_seconds."
  (or (ignore-errors (setting :timeout-seconds)) 20))

(defun stored-key (&optional (path nle::*auth-file-path*))
  "The key auth.json keeps under api_keys.ms-decision, or NIL; a store that cannot be read keeps none."
  (nle::auth-api-key (ignore-errors (nle::read-auth-file path)) +provider+))

(defun api-key (&optional (path nle::*auth-file-path*))
  "(values KEY SOURCE): the saved key over AZURE_FOUNDRY_API_KEY, as a chat provider's saved key outranks its variable; NIL when neither holds one."
  (alexandria:if-let (key (stored-key path))
    (values key :auth)
    (alexandria:when-let (key (nle::credential-env +key-env+))
      (values key :env))))

;;; --- one exchange ---------------------------------------------------------------

(defun http (method url &rest arguments)
  "The cell's one network call, NLK:HTTP; tests stub DEX:REQUEST under it."
  (apply #'nlk:http method url arguments))

(defun error-json (text)
  "TEXT as the answer of a call that produced no decision: the plugin answered {\"error\": ...} and sent nothing (provider.py:121-147)."
  (nlk:encode-json-object (nlk:json-object "error" text)))

(defun call (state questions)
  "POST QUESTIONS about STATE to the resource; the answer's JSON text, or a JSON error object naming what is missing or what the endpoint said. Nothing is sent when the endpoint or the key is unset."
  (let ((base (base-url)) (key (api-key)))
    (cond ((null base)
           (error-json (format nil "set base_url to https://<resource>~a (or ~a), and ~a"
                               +host-suffix+ +base-env+ +key-env+)))
          ((null key)
           (error-json (format nil "no Foundry key: make one at ~a, then set ~a or save it in auth.json under api_keys.~a"
                               +key-page+ +key-env+ +provider+)))
          (t
           (handler-case
               (multiple-value-bind (body status)
                   (http :post (format nil "~a~a" base +route+)
                         :headers `(("api-key" . ,key)
                                    ("accept" . "application/json")
                                    ("content-type" . "application/json"))
                         :content (nlk:encode-json-object
                                   (nlk:json-object "model" (model) "state" state
                                                    "questions" questions))
                         :timeout (timeout) :connect-timeout (timeout))
                 (let ((text (and body (nlk:body-text body))))
                   (cond ((not (<= 200 status 299)) (error-json (format nil "HTTP ~d" status)))
                         ((and text (plusp (length text))) text)
                         (t (error-json "the endpoint answered with nothing")))))
             (error (condition)
               (error-json (format nil "the endpoint could not be reached: ~a"
                                    (type-of condition)))))))))

;;; --- the questions and the four verbs ---------------------------------------------

(defun as-labels (values)
  "VALUES, a vector or a list of strings, as a list."
  (cond ((null values) '())
        ((vectorp values) (coerce values 'list))
        (t (copy-list values))))

(defun criteria-object (pairs)
  "PAIRS, (label . rubric) as an object; a list of labels alone becomes {label label}, as the plugin defaulted it (provider.py:167)."
  (let ((object (nlk:json-object)))
    (dolist (pair pairs object)
      (if (consp pair)
          (setf (gethash (car pair) object) (or (cdr pair) (car pair)))
          (setf (gethash pair object) pair)))))

(defun typed-question (type instructions criteria)
  "One typed question as the wire's object."
  (nlk:json-object "type" type "instructions" instructions "criteria" criteria))

(defun one-question (state id question)
  "STATE with the single question QUESTION keyed ID."
  (let ((questions (nlk:json-object)))
    (setf (gethash id questions) question)
    (call state questions)))

(defun noul (state question &key (true-label "true") (false-label "false"))
  "The probability, 0 to 1, that QUESTION's yes/no condition holds for STATE, with TRUE-LABEL and FALSE-LABEL saying what each side means: the endpoint's JSON text, its answers object carrying one noul probability."
  (one-question state "q"
                (typed-question "noul" question
                                (nlk:json-object "true" true-label "false" false-label))))

(defun choice (state question options &key criteria)
  "The most probable of OPTIONS for STATE, with every option's probability and a confidence: the endpoint's JSON text, its answers object carrying the choice and the whole distribution."
  (one-question state "q"
                (typed-question "choice" question
                                (if (and (hash-table-p criteria) (plusp (hash-table-count criteria)))
                                    criteria
                                    (criteria-object (as-labels options))))))

(defun score (state question levels)
  "STATE's place on LEVELS, ordered lowest first, as a probability-weighted number with the whole distribution behind it."
  (one-question state "q"
                (typed-question "score" question (coerce (as-labels levels) 'vector))))

(defun decide (state questions)
  "Several typed QUESTIONS about one STATE in one call: QUESTIONS an object of id to a typed question, as (ms-decision:decide STATE (nlk:json-object \"a\" (nlk:json-object \"type\" \"noul\" \"instructions\" \"...\")))."
  (call state questions))

;;; --- what an answer says -------------------------------------------------------

(defun answers (text)
  "TEXT, a response body, as its answers object, or NIL."
  (let ((decoded (ignore-errors (nlk:decode-json text))))
    (nlk:json-value decoded :object "answers")))

(defun probability (text id)
  "The probability answer ID carries in TEXT, or NIL: what the gate reads."
  (let ((value (nlk:json-value (answers text) :number id "noul")))
    (and (realp value) (<= 0 value 1) value)))
