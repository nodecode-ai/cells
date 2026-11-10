;;;; provider.lisp --- what Lightning AI is: its address, its key, its models,
;;;; and the list of them the gateway publishes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of dinosauria123's hermes-lightningai-provider (see NOTICE):
;;;; __init__.py's ProviderProfile and plugin.yaml.
;;;;
;;;; Lightning AI serves an OpenAI-compatible gateway behind a bearer key made
;;;; on lightning.ai. What the original gives a user, and this keeps:
;;;;
;;;;   - the provider id `lightningai', displayed as Lightning AI, with the
;;;;     two further spellings the profile's aliases answer to (lightning,
;;;;     lightning-ai)
;;;;   - the base the profile names, https://lightning.ai/api/v1 -- which now
;;;;     answers {"error": "moved"} naming https://platform.lightning.ai/api/v1,
;;;;     so that address is the default here and the profile's is one base_url
;;;;     away
;;;;   - the profile's env_vars: LIGHTNING_API_KEY and LIGHTNING_AI_API_KEY
;;;;     for the key, LIGHTNING_BASE_URL standing in for the base
;;;;   - the live model list from GET <base>/models, which Hermes fetched with
;;;;     its own client and this cell reads on start and on /lightning-ai
;;;;
;;;; The original bundles no model ids: its picker is whatever /models
;;;; answers, and so is this one.

(in-package #:nodecode-lightning-ai)

(defparameter +base+ "https://platform.lightning.ai/api/v1"
  "Where Lightning AI serves its gateway: the base the chat lane appends
/chat/completions to.")

(defparameter +base-env+ "LIGHTNING_BASE_URL"
  "The environment variable that, when set, stands in for the section's
base_url, which is how the original's env_vars list spells a base override.")

(defparameter +env+ '("LIGHTNING_API_KEY" "LIGHTNING_AI_API_KEY")
  "The environment variables a key is read from, in order: the profile's own
name for it, then the one the profile also accepts.")

(defparameter +key-page+ "https://lightning.ai"
  "Where a key is made and where the original sends a new user: the profile's
signup_url.")

(defparameter +seed-timeout+ 8
  "One /models read's ceiling, in seconds.")

;;; --- the key ---------------------------------------------------------------------

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))

(defun credential-key ()
  "The key a model-list read carries: the first environment variable, else the
key /connect saved, else NIL."
  (or (env-key)
      (let ((credential (ignore-errors (nle::resolve-provider-credential +provider+ :probe t))))
        (and credential
             (not (eq :public (nle:credential-source credential)))
             (nle:credential-key credential)))))

;;; --- the section -----------------------------------------------------------------

(defun env-base ()
  "LIGHTNING_BASE_URL, without a trailing slash, or NIL when it is unset or
blank."
  (let* ((value (nle::credential-env +base-env+))
         (trimmed (and value (string-right-trim "/" value))))
    (and trimmed (plusp (length trimmed)) trimmed)))

(defun read-settings (values table)
  "The settings the cell runs on: the base the declaration derived, unless
LIGHTNING_BASE_URL names one, as the original's env_vars list allows."
  (declare (ignore table))
  (list :base-url (or (env-base) (getf values :base-url) +base+)))

(defun base ()
  "The base this section names, without a trailing slash."
  (string-right-trim "/" (setting :base-url)))

;;; --- the model list the gateway publishes ----------------------------------------

(defun model-row (id &key context)
  "ID as the catalog keeps a model (NLE::MAKE-CATALOG-MODEL's fields): the
context window the list published when it published one, text in and out, and
a turn to be had. The list publishes no display name, so the id serves."
  (nle::make-catalog-model id context nil #("text") #("text") nil nil nil t nil))

(defun live-entry-p (entry)
  "Whether ENTRY, one row of the gateway's own model list, names a model."
  (and (hash-table-p entry)
       (let ((id (nlk:json-value entry :text "id")))
         (and id (plusp (length id)) t))))

(defun live-model (entry)
  "ENTRY, one row of GET /models, as the catalog keeps a model."
  (let* ((id (nlk:json-value entry :text "id"))
         (window (or (nlk:json-value entry :integer "context_window")
                     (nlk:json-value entry :integer "context_length"))))
    (model-row id :context (and window (plusp window) window))))

(defun parse-models (items)
  "The `data' array of a /models answer as catalog models, in the order the
gateway listed them; NIL when the payload names nothing."
  (loop for entry across (if (vectorp items) items #())
        when (live-entry-p entry) collect (live-model entry)))

(defvar *live* nil
  "(FETCHED-AT . MODELS) of the last /models list the gateway answered,
MODELS as the catalog keeps them; NIL before the first read.")

(defvar *live-lock* (bt2:make-lock :name "lightning-ai-models")
  "Held while the list is read or replaced, so a reader never sees half of
one.")

(defun read-models (base &optional api-key (timeout +seed-timeout+))
  "GET <BASE>/models: the models it publishes and NIL; or NIL and the reason
the list could not be read. Never signals."
  (handler-case
      (multiple-value-bind (body status)
          (nlk:http :get (format nil "~a/models" base)
                    :headers (append (list (cons "accept" "application/json"))
                                     (and api-key
                                          (list (cons "authorization"
                                                      (format nil "Bearer ~a" api-key)))))
                    :timeout timeout)
        (if (and (stringp body) (integerp status) (< status 300))
            (let ((payload (handler-case (nlk:decode-json body) (error () nil))))
              (values (or (parse-models (and payload (nlk:json-value payload :array "data")))
                          '())
                      nil))
            (values nil (format nil "HTTP ~a" status))))
    (error (condition)
      (values nil (princ-to-string condition)))))

(defun live-models ()
  "The models the last successful read kept, or NIL."
  (bt2:with-lock-held (*live-lock*)
    (cdr *live*)))

(defun live-stamp ()
  "When the list now held was read, or NIL: what the merged catalog memo
compares, so a refresh is seen."
  (bt2:with-lock-held (*live-lock*)
    (car *live*)))

(defun forget-live ()
  "Drop the list, so a start against another base asks again."
  (bt2:with-lock-held (*live-lock*)
    (setf *live* nil)))

(defun refresh-live ()
  "Ask the gateway for its model list and keep it; a read with no key, or one
that fails, keeps the list already held."
  (let ((key (credential-key)))
    (when key
      (multiple-value-bind (models why) (read-models (base) key)
        (declare (ignore why))
        (when models
          (bt2:with-lock-held (*live-lock*)
            (setf *live* (cons (get-universal-time) models)))))))
  (live-models))

;;; --- the catalog row -------------------------------------------------------------

(defun catalog-row (&optional prior)
  "Lightning AI as a models.dev provider: its id and name, the chat lane's
package, this section's base, the profile's key variables, and the models the
gateway last listed - over PRIOR's (the row models.dev itself published, when
it did)."
  (let ((models (make-hash-table :test 'equal)))
    (when (hash-table-p prior)
      (let ((published (nlk:json-value prior :object "models")))
        (when (hash-table-p published)
          (maphash (lambda (id model) (setf (gethash id models) model)) published))))
    (dolist (model (live-models))
      (setf (gethash (nle::catalog-model-name model) models) model))
    (nlk:json-object "name" "Lightning AI"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (base)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defvar *catalog* nil
  "(CATALOG STAMP . MERGED): the last root the core answered, the /models read
the row was built from, and that catalog with Lightning AI's row in it.")

(defun catalog (next)
  "MODELS-CATALOG-TABLE advice: the catalog with Lightning AI's row, made once
per catalog the core reads and once more when the model list changes."
  (let ((root (funcall next)) (stamp (live-stamp)) (memo *catalog*))
    (if (and memo (eq (first memo) root) (eql (second memo) stamp))
        (third memo)
        (let ((merged (make-hash-table :test 'equal)))
          (when (hash-table-p root)
            (maphash (lambda (id provider) (setf (gethash id merged) provider)) root))
          (setf (gethash +provider+ merged)
                (catalog-row (and (hash-table-p root) (gethash +provider+ root))))
          (setf *catalog* (list root stamp merged))
          merged))))

(defun forget-catalog ()
  "Drop the merged catalog and the model list, so a restart with another base
builds both again."
  (setf *catalog* nil)
  (forget-live))

;;; --- the key hook ----------------------------------------------------------------

(defun credential (op next)
  "The :CREDENTIAL answer for lightningai: LIGHTNING_API_KEY, else
LIGHTNING_AI_API_KEY, else none. A key /connect saved in auth.json answers
before this point."
  ;; Never NEXT for lightningai: the ladder behind this point falls back to the
  ;; chat family's default variable, and would send OPENAI_API_KEY to Lightning
  ;; AI.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (env-key))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

;;; --- the command -----------------------------------------------------------------

(defun run-command (args session-id)
  "The /lightning-ai command: read the model list the gateway publishes and
list it, so /models offers what the key serves; ARGS narrows the listing by
substring."
  (declare (ignore session-id))
  (let ((key (credential-key)))
    (multiple-value-bind (models why) (read-models (base) key)
      (cond
        (why
         (format nil "Lightning AI: the model list at ~a/models did not answer (~a)~@[; make a key at ~a~]~@[; one is set~]."
                 (base) why (and (null key) +key-page+) key))
        (t
         (bt2:with-lock-held (*live-lock*)
           (setf *live* (cons (get-universal-time) models)))
         (setf *catalog* nil)
         (let* ((needle (string-downcase (or args "")))
                (shown (remove-if-not
                        (lambda (model)
                          (search needle (string-downcase (nle::catalog-model-name model))))
                        models)))
           (if (null shown)
               (format nil "Lightning AI: ~a model~:p read; none matches ~s." (length models) args)
               (with-output-to-string (out)
                 (format out "Lightning AI ~a model~:p read, ~a listed; /models offers them now:~%"
                         (length models) (length shown))
                 (dolist (model (subseq shown 0 (min 25 (length shown))))
                   (format out "  ~a~%" (nle::catalog-model-name model)))
                 (when (> (length shown) 25)
                   (format out "  ... and ~a more~%" (- (length shown) 25)))))))))))
