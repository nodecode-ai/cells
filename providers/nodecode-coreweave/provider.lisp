;;;; provider.lisp --- what CoreWeave Inference is: its address, its key, its models.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Rewritten from coreweave/serverless-inference-for-hermes (see NOTICE):
;;;; __init__.py's provider profile, plugin.yaml and README.md.
;;;;
;;;; CoreWeave Serverless Inference (served by W&B at
;;;; https://api.inference.wandb.ai/v1) speaks the OpenAI chat wire with a
;;;; bearer key made at https://wandb.ai/authorize. What the original adds:
;;;;
;;;;   - the provider's name, endpoint and variable (COREWEAVE_API_KEY) so a
;;;;     user does not type them in: here that is the catalog row the cell
;;;;     puts in front of models.dev, which is what makes /models list
;;;;     CoreWeave and a turn resolve its lane, its address and its key
;;;;   - live model discovery: GET <base>/models with the key, so the picker
;;;;     offers what the account can actually use. /coreweave reads it on
;;;;     demand; nothing invents a fallback model list, as the original
;;;;     deliberately ships none

(in-package #:nodecode-coreweave)

(defparameter +base+ "https://api.inference.wandb.ai/v1"
  "Where CoreWeave Serverless Inference is served: the base the chat lane
appends /chat/completions to.")

(defparameter +env+ '("COREWEAVE_API_KEY")
  "The environment variables a CoreWeave key is read from, in order.")

(defparameter +key-page+ "https://wandb.ai/authorize"
  "Where an inference-enabled W&B API key is made.")

(defun base ()
  "The base this section names, without a trailing slash."
  (string-right-trim "/" (setting :base-url)))

;;; --- the models ----------------------------------------------------------------

(defun row-model (id &key context)
  "ID as the catalog keeps a model (NLE::MAKE-CATALOG-MODEL's fields). A model
CoreWeave lists takes tool calls and answers in text, which is what a turn
needs; its context window and price are left unsaid rather than invented."
  (nle::make-catalog-model id context nil #("text") #("text") nil nil nil t nil))

(defvar *live* nil
  "The model ids /coreweave last read from the endpoint; NIL before it runs.")

(defun catalog-row (&optional prior)
  "CoreWeave Inference as a models.dev provider: its name, the chat lane's
package, this section's base, the key variable, and the models /coreweave read,
over PRIOR's (when models.dev published a coreweave row of its own)."
  (let ((models (make-hash-table :test 'equal)))
    (when (hash-table-p prior)
      (let ((published (nlk:json-value prior :object "models")))
        (when (hash-table-p published)
          (maphash (lambda (id model) (setf (gethash id models) model)) published))))
    (dolist (id *live*)
      (setf (gethash id models) (row-model id)))
    (nlk:json-object "name" "CoreWeave Inference"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (base)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defvar *catalog* nil
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
CoreWeave's row in it; NIL before the first read.")

(defun catalog (next &aux (base (funcall next)))
  "MODELS-CATALOG-TABLE advice: the catalog with CoreWeave's row, made once per
catalog the core reads and again after /coreweave read a new model list."
  (if (and *catalog* (eq (car *catalog*) base))
      (cdr *catalog*)
      (let ((merged (make-hash-table :test 'equal)))
        (when (hash-table-p base)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) base))
        (setf (gethash +provider+ merged)
              (catalog-row (and (hash-table-p base) (gethash +provider+ base))))
        (setf *catalog* (cons base merged))
        merged)))

(defun forget-catalog ()
  "Drop the merged catalog and the model list /coreweave read, so a restart
with another base builds both again."
  (setf *catalog* nil *live* nil))

;;; --- the key -------------------------------------------------------------------

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))

(defun credential (op next)
  "The :CREDENTIAL answer for coreweave: COREWEAVE_API_KEY, else none. A key
/connect saved in auth.json answers before this point."
  ;; Never NEXT for coreweave: the ladder behind this point falls back to the
  ;; chat family's default variable, and would send OPENAI_API_KEY to CoreWeave.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (env-key))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

;;; --- the command ---------------------------------------------------------------

(defun catalog-key ()
  "The key the catalog fetch carries, or NIL: the one /connect saved, else
COREWEAVE_API_KEY."
  (handler-case
      (let ((key (nle:credential-key (nle::resolve-provider-credential +provider+))))
        (and key (not (equal key "public")) key))
    (error () nil)))

(defun entry-id (entry)
  "ENTRY, one row of GET /models, as a model id, or NIL."
  (or (and (stringp entry) entry)
      (nlk:json-value entry :text "id")))

(defun fetch-models (&optional (timeout 15))
  "GET <base>/models: the model ids this key may use, how many rows the answer
held, and NIL; or NIL, zero and the reason it could not be read."
  (handler-case
      (multiple-value-bind (body status)
          (nlk:http :get (format nil "~a/models" (base))
                    :headers (append '(("accept" . "application/json"))
                                     (alexandria:when-let ((key (catalog-key)))
                                       `(("authorization" . ,(format nil "Bearer ~a" key)))))
                    :timeout timeout)
        (if (and (integerp status) (= status 200))
            (let* ((document (handler-case (nlk:decode-json body) (error () nil)))
                   (data (if (hash-table-p document)
                             (nlk:json-value document :array "data")
                             document))
                   (entries (and (vectorp data) (coerce data 'list)))
                   (ids (remove-duplicates
                         (remove nil (mapcar #'entry-id entries)) :test #'equal)))
              (if (null entries)
                  (values nil 0 "an unexpected catalog shape")
                  (values ids (length entries) nil)))
            (values nil 0 (format nil "HTTP ~a" status))))
    (error (condition)
      (values nil 0 (princ-to-string condition)))))

(defun run-command (args session-id)
  "The /coreweave command: read GET /models and list the models this key can
use, so /models offers them; ARGS narrows the list by substring. Without a key
it says where to make one."
  (declare (ignore session-id))
  (if (null (catalog-key))
      (format nil "CoreWeave Inference: no key yet. Make an inference-enabled W&B API key at ~
~a, save it with /connect (provider coreweave) or set COREWEAVE_API_KEY, then ~
/coreweave lists the models your account can use. Endpoint: ~a"
              +key-page+ (base))
      (multiple-value-bind (ids total why) (fetch-models)
        (cond
          ((null ids)
           (format nil "CoreWeave Inference: the catalog at ~a did not answer (~a)." (base) why))
          (t
           (setf *catalog* nil)
           (setf *live* ids)
           (let* ((needle (string-downcase (or args "")))
                  (shown (remove-if-not (lambda (id) (search needle (string-downcase id))) ids)))
             (if (null shown)
                 (format nil "CoreWeave Inference: ~a model~:p at ~a; none matches ~s."
                         (length ids) (base) args)
                 (with-output-to-string (out)
                   (format out "CoreWeave Inference: ~a of ~a model~:p listed, ~a shown; ~
/models offers them now:~%"
                           (length ids) total (length shown))
                   (dolist (id (subseq shown 0 (min 25 (length shown))))
                     (format out "  ~a~%" id))
                   (when (> (length shown) 25)
                     (format out "  ... and ~a more~%" (- (length shown) 25)))))))))))
