;;;; provider.lisp --- what Crowbot is: its address, its key, its models, its ladder.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Rewritten from AgileIndustrialComplex's hermes-crowbot (see NOTICE):
;;;; plugin.yaml and __init__.py. crowbot.sh serves an OpenAI-compatible relay
;;;; at https://api.crowbot.sh/v1, billed to a 16-digit account number that is
;;;; sent as the bearer key. Its models take the whole OpenAI-compat
;;;; reasoning-effort vocabulary, and the relay's own default for a reasoning
;;;; model is its ceiling (max), so the original pins medium where the agent
;;;; picked nothing; that rule rides here as the section's default_effort.

(in-package #:nodecode-crowbot)

(defparameter +base+ "https://api.crowbot.sh/v1"
  "Where Crowbot is served: the base the chat lane appends /chat/completions
to.")

(defparameter +env+ '("CROWBOT_API_KEY" "HERMES_CUSTOM_API_CROWBOT_SH_API_KEY")
  "The environment variables a Crowbot account number is read from, in order:
this client's own, then the variable an existing Hermes install already holds.
Both name the same service, so no other provider's key is ever read here.")

(defparameter +key-page+ "https://crowbot.sh/"
  "Where an account is made and its account number found.")

(defparameter +models+ '("kimi-k3")
  "The model the original names when the relay has not been read: its fallback
list, which /models offers until /crowbot reads the live one.")

(defparameter +efforts+ '("minimal" "low" "medium" "high" "xhigh" "max")
  "The reasoning rungs a Crowbot model is offered, weakest first: the widest
OpenAI-compat wire set the original declares, less its `none', which is the
core's `off'.")

(defparameter +names+ '(("kimi-k3" . "Kimi K3"))
  "The display names of the models the original names; any other id stands as
the relay spells it.")

(defvar *discovered* nil
  "The model ids /crowbot last read from GET /models, in the order the relay
listed them; NIL until it is read.")

;;; --- the address, the key ------------------------------------------------------

(defun base ()
  "The base this section names, without a trailing slash."
  (string-right-trim "/" (setting :base-url)))

(defun env-key ()
  "The first account number one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))

(defun catalog-key ()
  "The key a model list is asked with, or NIL: the one /connect saved, else
the variable. A listing needs no key to answer, but a saved one is sent when
it is there."
  (handler-case
      (let ((key (nle:credential-key (nle::resolve-provider-credential +provider+))))
        (and key (not (equal key "public")) key))
    (error () nil)))

;;; --- the models -----------------------------------------------------------------

(defun display-name (id)
  "ID as a picker shows it: the original's own name for the model it names,
else the id as the relay spells it."
  (or (cdr (assoc id +names+ :test #'equal)) id))

(defun efforts ()
  "The ladder in rank order, as the catalog keeps it: the original's wire
vocabulary, so a pick of `max' is offered as `max' and not demoted."
  (sort (remove-if-not #'nle::effort-rank (copy-list +efforts+))
        #'< :key #'nle::effort-rank))

(defun model-row (id &optional context)
  "ID as the catalog keeps a model: text in, text out, the widest ladder, and
CONTEXT seconds the section's context_tokens where the relay published none."
  (nle::make-catalog-model (display-name id)
                            (or context (setting :context-tokens))
                            nil
                            #("text") #("text")
                            (efforts)
                            t nil t nil))

(defun catalog-row (&optional prior)
  "Crowbot as a models.dev provider: the chat lane's package, this section's
base, the key variables, and the models the original names plus the ones
/crowbot last read, over PRIOR's (the row models.dev itself published, when it
did)."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let ((published (nlk:json-value prior :object "models")))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (dolist (id (append +models+ *discovered*))
      (setf (gethash id models) (model-row id)))
    (nlk:json-object "name" +display-name+
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (setting :base-url)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun model-ids (document)
  "The model ids DOCUMENT, a decoded GET /models answer, lists: the OpenAI
`data' array, else a `models' one. NIL when neither is there."
  (let ((rows (or (nlk:json-value document :array "data")
                  (nlk:json-value document :array "models"))))
    (loop for row across (or rows #())
          for id = (and (hash-table-p row) (nlk:json-value row :string "id"))
          when id collect id)))

(defun fetch-models (&optional (timeout 15))
  "GET <base>/models: the ids the relay lists and NIL; or NIL and the reason
it could not be read."
  (handler-case
      (multiple-value-bind (body status)
          (nlk:http :get (format nil "~a/models" (base))
                    :headers (append '(("accept" . "application/json"))
                                     (alexandria:when-let ((key (catalog-key)))
                                       `(("authorization" . ,(format nil "Bearer ~a" key)))))
                    :timeout timeout)
        (if (and (integerp status) (= status 200))
            (let* ((document (handler-case (nlk:decode-json body) (error () nil)))
                   (ids (and document (model-ids document))))
              (if ids
                  (values ids nil)
                  (values nil "the answer carries no model list")))
            (values nil (format nil "HTTP ~a" status))))
    (error (condition)
      (values nil (princ-to-string condition)))))

(defun run-command (args session-id)
  "The /crowbot command: read the relay's model list and list it, so /models
offers what the account can reach; ARGS narrows the list by substring."
  (declare (ignore session-id))
  (multiple-value-bind (ids why) (fetch-models)
    (if (null ids)
        (format nil "Crowbot: the model list at ~a/models did not answer (~a)." (base) why)
        (progn
          (setf *discovered* ids)
          (forget-catalog)
          (let* ((needle (string-downcase (or args "")))
                 (shown (remove-if-not (lambda (id) (search needle (string-downcase id)))
                                       ids)))
            (if (null shown)
                (format nil "Crowbot: ~a model~:p listed; none matches ~s." (length ids) args)
                (with-output-to-string (out)
                  (format out "Crowbot: ~a model~:p listed, ~a shown; /models offers them now:~%"
                          (length ids) (length shown))
                  (dolist (id (subseq shown 0 (min 40 (length shown))))
                    (format out "  ~a~%" id))
                  (when (> (length shown) 40)
                    (format out "  ... and ~a more~%" (- (length shown) 40))))))))))
