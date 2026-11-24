;;;; provider.lisp --- what CloudGPU is: its address, its key, its models, its wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Rewritten from mrhy7176/hermes-cloudgpu (see NOTICE): __init__.py's
;;;; profile and plugin.yaml.
;;;;
;;;; CloudGPU serves DeepSeek, Kimi, GLM and MiniMax models over the OpenAI
;;;; chat wire at https://cloudgpu.app/v1, behind a bearer key from
;;;; cloudgpu.app/api. What the original gives a user, and this keeps:
;;;;
;;;;   - the profile's fallback models, listed until the live catalog is
;;;;     read, deepseek-v4.1-flash among them
;;;;   - the model list the API publishes at GET /v1/models, which the
;;;;     original documents as needing no key: /cloudgpu reads it on demand
;;;;     and puts it in /models
;;;;   - CLOUDGPU_BASE_URL, the base override the profile's env_vars list
;;;;     names beside the key

(in-package #:nodecode-cloudgpu)

(defparameter +base+ "https://cloudgpu.app/v1"
  "Where CloudGPU is served: the base the chat lane appends /chat/completions to.")

(defparameter +base-env+ "CLOUDGPU_BASE_URL"
  "The environment variable that, when set, stands in for the section's base_url,
which is how the original's env_vars list spells a base override.")

(defparameter +env+ '("CLOUDGPU_API_KEY")
  "The environment variables a key is read from, in order.")

(defparameter +key-page+ "https://cloudgpu.app/api"
  "Where a key is made, and where the original sends a new user.")

(defparameter +models+
  '(("deepseek-v4.1-flash" . "DeepSeek V4.1 Flash")
    ("deepseek-v4-pro" . "DeepSeek V4 Pro")
    ("kimi-k3" . "Kimi K3")
    ("kimi-k2.7-code" . "Kimi K2.7 Code")
    ("glm-5.2" . "GLM 5.2")
    ("minimax-m3" . "MiniMax M3"))
  "The profile's fallback_models, as (ID . NAME), shown until /cloudgpu read
what the API publishes. The original's setup default, deepseek-v4.1-flash,
leads.")

;;; --- the models ----------------------------------------------------------------

(defun row-model (name &key context)
  "A model as the catalog keeps one (NLE::MAKE-CATALOG-MODEL's fields): NAME,
the context window the API published when it published one, text in and out,
and a turn to be had."
  (nle::make-catalog-model name context nil #("text") #("text") nil nil nil t nil))

(defun live-entry-p (entry)
  "Whether ENTRY, one row of the API's own model list, names a model."
  (and (hash-table-p entry)
       (let ((id (nlk:json-value entry :text "id")))
         (and id (plusp (length id)) t))))

(defun live-model (entry)
  "ENTRY, one row of GET /models, as the catalog keeps a model. The list
publishes an id and, sometimes, a window; no display name, so the id serves."
  (let* ((id (nlk:json-value entry :text "id"))
         (window (or (nlk:json-value entry :integer "context_window")
                     (nlk:json-value entry :integer "context_length"))))
    (row-model id :context (and window (plusp window) window))))

(defvar *live* nil
  "The rows /cloudgpu last read from the API, as its entries; NIL before the
command runs.")

(defun catalog-row (&optional prior)
  "CloudGPU as a models.dev provider: its id and name, the chat lane's
package, this section's base, the key variable, and the rows /cloudgpu read
over the bundled ones, over PRIOR's (when models.dev published a cloudgpu row
of its own)."
  (let ((models (make-hash-table :test 'equal)))
    (when (hash-table-p prior)
      (let ((published (nlk:json-value prior :object "models")))
        (when (hash-table-p published)
          (maphash (lambda (id model) (setf (gethash id models) model)) published))))
    (dolist (row +models+)
      (setf (gethash (car row) models) (row-model (cdr row))))
    (dolist (entry *live*)
      (setf (gethash (nlk:json-value entry :text "id") models) (live-model entry)))
    (nlk:json-object "name" "CloudGPU"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (base)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defvar *catalog* nil
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
CloudGPU's row in it; NIL before the first read.")

(defun catalog (next &aux (root (funcall next)))
  "MODELS-CATALOG-TABLE advice: the catalog with CloudGPU's row, made once per
catalog the core reads and again after /cloudgpu read a new model list."
  (if (and *catalog* (eq (car *catalog*) root))
      (cdr *catalog*)
      (let ((merged (make-hash-table :test 'equal)))
        (when (hash-table-p root)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) root))
        (setf (gethash +provider+ merged)
              (catalog-row (and (hash-table-p root) (gethash +provider+ root))))
        (setf *catalog* (cons root merged))
        merged)))

(defun forget-catalog ()
  "Drop the merged catalog and the model list /cloudgpu read, so a restart with
another base builds both again."
  (setf *catalog* nil *live* nil))

;;; --- the key -------------------------------------------------------------------

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))

(defun credential (op next)
  "The :CREDENTIAL answer for cloudgpu: CLOUDGPU_API_KEY, else none. A key
/connect saved in auth.json answers before this point."
  ;; Never NEXT for cloudgpu: the ladder behind this point falls back to the
  ;; chat family's default variable, and would send OPENAI_API_KEY to CloudGPU.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (env-key))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

;;; --- the section ----------------------------------------------------------------

(defun env-base ()
  "CLOUDGPU_BASE_URL, without a trailing slash, or NIL when it is unset or
blank."
  (let* ((value (nle::credential-env +base-env+))
         (trimmed (and value (string-right-trim "/" value))))
    (and trimmed (plusp (length trimmed)) trimmed)))

(defun read-settings (values table)
  "The settings the cell runs on: the base the declaration derived, unless
CLOUDGPU_BASE_URL names one, as the original's env_vars list allows."
  (declare (ignore table))
  (list :base-url (or (env-base) (getf values :base-url) +base+)))

(defun base ()
  "The base this section names, without a trailing slash."
  (string-right-trim "/" (setting :base-url)))

;;; --- the public catalog ----------------------------------------------------------

(defun fetch-models (&optional (timeout 15))
  "GET <base>/models: the rows the API publishes, and NIL; or NIL and the
reason it could not be read. The listing is public; the key rides along when
one is set."
  (handler-case
      (multiple-value-bind (body status)
          (nlk:http :get (format nil "~a/models" (base))
                    :headers (append (list (cons "accept" "application/json"))
                                     (alexandria:when-let (key (env-key))
                                       (list (cons "authorization" (format nil "Bearer ~a" key)))))
                    :timeout timeout)
        (if (and (integerp status) (= status 200))
            (let* ((document (handler-case (nlk:decode-json body) (error () nil)))
                   (data (and document (nlk:json-value document :array "data")))
                   (rows (remove-if-not #'live-entry-p (coerce (or data #()) 'list))))
              (values rows nil))
            (values nil (format nil "HTTP ~a" status))))
    (error (condition)
      (values nil (princ-to-string condition)))))

(defun run-command (args session-id)
  "The /cloudgpu command: read the model list the API publishes and list it,
so /models offers what this deployment serves; ARGS narrows the list by
substring."
  (declare (ignore session-id))
  (multiple-value-bind (rows why) (fetch-models)
    (cond
      ((null rows)
       (format nil "CloudGPU: the model list at ~a did not answer (~a)~@[; make a key at ~a~];
the bundled models stand."
               (base) why (and (null (env-key)) +key-page+)))
      (t
       (setf *catalog* nil)
       (setf *live* rows)
       (let* ((needle (string-downcase (or args "")))
              (shown (remove-if-not
                      (lambda (entry)
                        (search needle (string-downcase (nlk:json-value entry :text "id"))))
                      rows)))
         (if (null shown)
             (format nil "CloudGPU: ~a model~:p read; none matches ~s." (length rows) args)
             (with-output-to-string (out)
               (format out "CloudGPU ~a model~:p read, ~a listed; /models offers them now:~%"
                       (length rows) (length shown))
               (dolist (entry (subseq shown 0 (min 25 (length shown))))
                 (format out "  ~a~%" (nlk:json-value entry :text "id")))
               (when (> (length shown) 25)
                 (format out "  ... and ~a more~%" (- (length shown) 25))))))))))
