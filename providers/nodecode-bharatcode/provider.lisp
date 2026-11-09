;;;; provider.lisp --- what BharatCode is: its address, its key, its models.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of thenabbu's bharatcode-hermes (see NOTICE): its provider
;;;; profile -- the OpenAI-compatible base at https://bharatcode.ai/api/model/v1,
;;;; API-key auth, the two fallback models it names, and its
;;;; supports_vision=True claim.
;;;;
;;;; BharatCode speaks the OpenAI chat wire at that base with a bearer key made
;;;; at https://bharatcode.ai (free for students with an .ac.in or .edu.in
;;;; address). Its GET /models names the models an account may use; this folder
;;;; reads it once at start, keeps the last good answer, and asks again every
;;;; +REFRESH-SECONDS+. Until the provider has named anything, the picker
;;;; offers the original's two fallback models.

(in-package #:nodecode-bharatcode)

(defparameter +base+ "https://bharatcode.ai/api/model/v1"
  "Where BharatCode is served: the base the chat lane appends
/chat/completions to.")

(defparameter +env+ '("BHARATCODE_API_KEY")
  "The environment variables a BharatCode key is read from, in order.")

(defparameter +signup+ "https://bharatcode.ai"
  "Where a key is made: the free tier is for .ac.in and .edu.in students.")

(defparameter +refresh-seconds+ 600
  "How long the provider's model list may stand before the worker asks again.")

(defparameter +seed-timeout+ 5
  "One catalog read's ceiling, in seconds: a provider that is down must not
hold a start.")

(defparameter +fallback-models+ '("deepseek-v4.1-flash" "qwen-3.8-27b")
  "The picker's list while the provider answers nothing: the original's
fallback_models, in its order.")

;;; --- the provider's model list ---------------------------------------------------

(defvar *live* nil
  "(FETCHED-AT . IDS) of the last model list the provider answered, IDS the
ids it named. A time with no ids means it has named none yet.")

(defvar *live-lock* (bt2:make-lock :name "bharatcode-catalog")
  "Held while the list is read or replaced, so a turn never reads half of it.")

(defun parse-live-ids (items)
  "ITEMS, the provider's /models payload, as the ids it names: its `data'
array, or a bare array of ids."
  (let ((ids '()))
    (loop for item across (if (vectorp items) items #())
          for id = (if (hash-table-p item)
                        (nlk:json-value item :text "id")
                        (and (stringp item) item))
          when id do (pushnew id ids :test #'equal))
    (nreverse ids)))

(defun fetch-live-items (base api-key timeout)
  "The model array of {BASE}/models; NIL on any failure, never signals."
  (handler-case
      (multiple-value-bind (body status)
          (nlk:http :get (concatenate 'string (string-right-trim "/" base) "/models")
                    :headers (list (cons "authorization" (format nil "Bearer ~a" api-key))
                                   (cons "accept" "application/json"))
                    :timeout timeout)
        (when (and (stringp body) (integerp status) (< status 300))
          (let ((payload (nlk:decode-json body)))
            (if (vectorp payload) payload (nlk:json-value payload :array "data")))))
    (error () nil)))

(defun car-safe (value)
  "VALUE's car when it is a cons, else NIL."
  (and (consp value) (car value)))

(defun refresh-live ()
  "Ask the provider for its model list and keep it. A failed read keeps the
list already held, so a provider that is down costs nothing and serves what it
last said."
  (let* ((base (setting :base-url))
         (items (fetch-live-items base (or (env-key) "") +seed-timeout+))
         (ids (and items (parse-live-ids items))))
    (when ids
      (bt2:with-lock-held (*live-lock*)
        (setf *live* (cons (get-universal-time) ids))))
    (car-safe *live*)))

(defun forget-live ()
  "Drop the list, so a start with another base asks again."
  (bt2:with-lock-held (*live-lock*)
    (setf *live* nil)))

(defun live-ids ()
  "The ids the provider last named, or NIL when it has named none."
  (bt2:with-lock-held (*live-lock*)
    (cdr *live*)))

;;; --- the catalog row -------------------------------------------------------------

(defun catalog-model (id)
  "ID as the catalog keeps a model (NLE::MAKE-CATALOG-MODEL's fields). The
original declares the provider vision-capable (supports_vision=True), so every
row this folder offers takes images; no context, output or reasoning figure is
claimed for a model the provider alone names."
  (nle::make-catalog-model id nil nil #("text" "image") #("text") nil nil nil t nil))

(defun catalog-models ()
  "The models the picker offers: every id the provider last named, or the
fallback list while it has named none."
  (mapcar #'catalog-model (or (live-ids) +fallback-models+)))

(defun catalog-row (&optional prior)
  "BharatCode as a models.dev provider: the chat lane's package, this
section's base, the key variables, and the models the provider last named over
PRIOR's (the row models.dev itself published, when it did)."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for model in (catalog-models)
          do (setf (gethash (nle::catalog-model-name model) models) model))
    (nlk:json-object "name" "BharatCode"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (setting :base-url)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))
