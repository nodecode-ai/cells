;;;; provider.lisp --- what Gondola is: its address, its key, its catalog, its models.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of Gondola's Hermes provider plugin (see NOTICE). The plugin's
;;;; one idea, kept here: Gondola's GET /v1/models is a mixed catalog -- text,
;;;; image and video models side by side -- so the model picker should carry
;;;; only the ids a chat turn can run on. The original decides an entry's kind
;;;; by its `endpoint' field, falling back to `modality', and keeps an entry
;;;; that says neither, so an unfamiliar catalog shape degrades to the stock
;;;; picker rather than emptying it.
;;;;
;;;; Two tiers, as in the original: the live catalog when it answers, and the
;;;; profile's curated fallback models when it does not.

(in-package #:nodecode-gondola)

(defparameter +base+ "https://api.gondola-ai.com/v1"
  "Where Gondola is served: the base the chat lane appends /chat/completions to.")

(defparameter +api-key-env+ "GONDOLA_API_KEY"
  "The environment variable a Gondola key is read from; /connect's saved key answers first.")

(defparameter +timeout+ 8
  "Seconds one catalog fetch may take, as the original's fetch_models allows.")

(defparameter +chat-endpoint+ "/v1/chat/completions"
  "The `endpoint' a catalog entry names when a chat turn can run on it.")

(defparameter +fallback-ids+
  '("deepseek-v4-flash"
    "claude-sonnet-5"
    "kimi-k2-7-code"
    "qwen3-coder-480b-a35b-instruct-turbo"
    "venice-uncensored-1-2")
  "The picker when the live catalog is unreachable: the original profile's
fallback models, each a tool-calling chat route.")

;;; --- the catalog -------------------------------------------------------------

(defun chat-model-p (entry)
  "Whether catalog ENTRY is a model a chat turn can run on, as the original
decides it: an `endpoint' string decides when there is one, else a `modality'
string, else the entry is kept."
  (let ((endpoint (nlk:json-value entry :string "endpoint")))
    (if endpoint
        (equal endpoint +chat-endpoint+)
        (let ((modality (nlk:json-value entry :string "modality")))
          (if modality (equal modality "text") t)))))

(defun entry-id (entry)
  "ENTRY's model id, or NIL when it names none."
  (nlk:json-value entry :string "id"))

(defun entry-name (entry)
  "ENTRY's display name when the catalog publishes one, else its id."
  (or (nlk:json-value entry :string "name") (entry-id entry)))

(defun entry-context (entry)
  "The context window ENTRY publishes, when it publishes one."
  (or (nlk:json-value entry :integer "context_length")
      (nlk:json-value entry :integer "context_window")))

(defun catalog-rows (payload)
  "The chat-capable rows of a model catalog PAYLOAD, in catalog order, each a
plist of :ID, :NAME and :CONTEXT: an id the catalog repeats is kept once."
  (let ((entries (if (hash-table-p payload)
                     (nlk:json-value payload :array "data")
                     payload))
        (rows '())
        (seen (make-hash-table :test 'equal)))
    (when (vectorp entries)
      (loop for entry across entries
            do (let ((id (and (hash-table-p entry) (chat-model-p entry) (entry-id entry))))
                 (when (and id (plusp (length id)) (not (gethash id seen)))
                   (setf (gethash id seen) t)
                   (push (list :id id
                               :name (entry-name entry)
                               :context (entry-context entry))
                         rows)))))
    (nreverse rows)))

(defun fallback-rows ()
  "The curated chat rows: what the picker keeps when the catalog is unreachable."
  (loop for id in +fallback-ids+ collect (list :id id :name id :context nil)))

(defvar *live-rows* nil
  "The rows the live catalog answered, or NIL while it is unfetched or
unreachable; NIL is what makes the curated list answer instead.")

(defun models-url (base)
  "BASE without a trailing slash, and /models: the address the original
resolves its catalog to."
  (concatenate 'string (string-right-trim "/" base) "/models"))

(defun fetch-rows (base &aux (answer nil) (status nil))
  "One fetch of the model catalog under BASE => its chat-capable rows, or NIL
when the catalog is unreachable or names no chat model."
  (handler-case
      (multiple-value-setq (answer status)
        (nlk:http :get (models-url base)
                  :headers (let ((key (nle::credential-env +api-key-env+)))
                             (append (when key
                                       (list (cons "authorization" (format nil "Bearer ~a" key))))
                                     (list (cons "accept" "application/json"))))
                  :timeout +timeout+
                  :connect-timeout (min 5 +timeout+)))
    ((or error sb-sys:deadline-timeout) () (return-from fetch-rows nil)))
  (when (and (integerp status) (< status 300))
    (let ((rows (handler-case (catalog-rows (nlk:decode-json answer))
                  (error () nil))))
      (and rows (plusp (length rows)) rows))))

(defun base ()
  "This section's base, without a trailing slash."
  (string-right-trim "/" (setting :base-url)))

(defun refresh (base)
  "One step of the catalog worker: fetch the live catalog from BASE, keep its
rows, and stop the worker once they have landed. Answers NIL to be woken again
after the worker's wait, so an unreachable catalog is retried rather than
given up on."
  (when (setf *live-rows* (fetch-rows base))
    (forget-catalog)
    (nle:notice (format nil "gondola: ~d chat models from the live catalog"
                        (length *live-rows*)))
    :stop))

;;; --- the provider row --------------------------------------------------------

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Gondola's row in it.")

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base or other models
builds a new one."
  (setf *catalog* (cons nil nil)))

(defun catalog-model (row)
  "ROW, a plist from the catalog or the curated list, as a CATALOG-MODEL."
  (destructuring-bind (&key id name context) row
    (declare (ignore id))
    ;; every row here is chat-capable by construction, takes tool calls, and
    ;; takes text in and answers text out: Gondola's chat models relay Venice's
    (nle::make-catalog-model name context nil #("text") #("text")
                              nil nil nil t nil)))

(defun catalog-row (&optional prior)
  "Gondola as a models.dev provider: the chat lane's package, this section's
base, the key variable, and the chat-capable models over PRIOR's (the row
models.dev itself published, when it did)."
  (let ((models (make-hash-table :test 'equal)))
    (when (hash-table-p prior)
      (let ((published (nlk:json-value prior :object "models")))
        (when (hash-table-p published)
          (maphash (lambda (id model) (setf (gethash id models) model)) published))))
    (dolist (row (or *live-rows* (fallback-rows)))
      (setf (gethash (getf row :id) models) (catalog-model row)))
    (nlk:json-object "name" "Gondola"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (base)
                     "env" (vector +api-key-env+)
                     "models" models)))

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Gondola's row, made once per
catalog the core reads."
  (if (and (car memo) (eq (car memo) base))
      (cdr memo)
      (let ((merged (make-hash-table :test 'equal)))
        (when (hash-table-p base)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) base))
        (setf (gethash +provider+ merged)
              (catalog-row (and (hash-table-p base) (gethash +provider+ base))))
        (setf *catalog* (cons base merged))
        merged)))
