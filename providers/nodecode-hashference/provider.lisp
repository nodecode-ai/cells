;;;; provider.lisp --- what Hashference is: its address, its key, its catalog, its models.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of 0xalank's Hermes provider plugin (see NOTICE). The plugin's
;;;; one idea, kept here: a turn must be sized by what a model actually
;;;; accepts, not by what its window suggests. Hashference's hosted models
;;;; advertise a 163,840-token window but take 98,304 input tokens, so a
;;;; session sized from the window comes back refused as oversized input, and
;;;; their output cap is the model's own default (8,192 there). Both numbers
;;;; are in Hashference's public catalog, so the rows below carry them.
;;;;
;;;; The second thing the plugin did, kept: the picker lists only the models
;;;; an agent can run on -- no Anthropic Messages aliases (the same models
;;;; for another wire), nothing the catalog marks unable to call tools, and
;;;; nothing that takes less than 64K tokens of input.
;;;;
;;;; Two tiers, as in the original: the live catalog when it answers, and a
;;;; curated list of tool-capable models when it does not, so an offline
;;;; start still fills the picker.

(in-package #:nodecode-hashference)

(defparameter +base+ "https://hashference.com/v1"
  "Where Hashference is served: the base the chat lane appends /chat/completions to.")

(defparameter +api-key-env+ "HASHFERENCE_API_KEY"
  "The environment variable a Hashference key is read from; /connect's saved key answers first.")

(defparameter +catalog-path+ "/models"
  "The public catalog, at the API base: no credentials, and no account to read it.")

(defparameter +timeout+ 8
  "Seconds one catalog fetch may take, connect included.")

(defparameter +min-context+ 65536
  "The smallest input a model may take and still hold a working session: 64K.")

(defparameter +fallback-ids+
  '("hashference-qwen3.8-27b"
    "anthropic-claude-sonnet-5.5"
    "openai-gpt-6.1-sol"
    "z-ai-glm-5.3"
    "moonshotai-kimi-k3.1-pro"
    "deepseek-deepseek-v4.1-flash")
  "The picker when the live catalog is unreachable: the original plugin's
curated tool-capable models, in its order.")

;;; --- the catalog -------------------------------------------------------------

(defun ends-with (suffix text)
  "Whether TEXT ends with SUFFIX."
  (let ((n (length text)) (m (length suffix)))
    (and (>= n m) (string= suffix text :start2 (- n m)))))

(defun integer-field (entry key)
  "ENTRY's KEY as a positive integer, or NIL: a JSON null, a string or an
absent member is no number here, as the original read it."
  (let ((value (and (hash-table-p entry) (gethash key entry))))
    (and (integerp value) (plusp value) value)))

(defun false-field-p (entry key)
  "Whether ENTRY states KEY as JSON false. Absent and null are not false: here
they read the same, and neither is a refusal."
  (and (hash-table-p entry)
       (multiple-value-bind (value present) (gethash key entry)
         (and present (null value) t))))

(defun context-limit (entry)
  "The most input ENTRY accepts: `max_input_tokens' when stated, else its
context window. NIL where the catalog states neither."
  (or (integer-field entry "max_input_tokens")
      (integer-field entry "context_length")))

(defun output-limit (entry)
  "The output cap ENTRY asks to be sent when the user set none."
  (integer-field entry "default_max_output_tokens"))

(defun runs-a-turn-p (entry)
  "Whether catalog ENTRY belongs in the picker: a model with an id, on the
chat wire, that calls tools when the catalog says so, and that takes at least
+MIN-CONTEXT+ tokens of input (an unstated limit is not a refusal)."
  (let ((id (and (hash-table-p entry) (nlk:json-value entry :string "id"))))
    (and id
         (plusp (length id))
         (not (equal (nlk:json-value entry :string "protocol") "messages"))
         (not (ends-with "-messages" id))
         (not (false-field-p entry "supports_tools"))
         (let ((limit (context-limit entry)))
           (or (null limit) (>= limit +min-context+))))))

(defun catalog-rows (payload)
  "The rows of the public catalog PAYLOAD a turn can run on, in catalog order."
  (let ((entries (if (hash-table-p payload)
                     (nlk:json-array payload "data")
                     payload))
        (rows '())
        (seen (make-hash-table :test 'equal)))
    (when (vectorp entries)
      (loop for entry across entries
            do (let ((id (and (hash-table-p entry) (runs-a-turn-p entry)
                              (nlk:json-value entry :string "id"))))
                 (when (and id (not (gethash id seen)))
                   (setf (gethash id seen) t)
                   (push (list :id id
                               :name (or (nlk:json-value entry :string "name") id)
                               :context (context-limit entry)
                               :output (output-limit entry))
                         rows)))))
    (nreverse rows)))

(defun fallback-rows ()
  "The curated rows: what the picker keeps when the catalog is unreachable."
  (loop for id in +fallback-ids+ collect (list :id id :name id :context nil :output nil)))

(defvar *live-rows* nil
  "The rows the live catalog answered, or NIL while it is unfetched or
unreachable; NIL is what makes the curated list answer instead.")

(defun catalog-url (base)
  "The public catalog's address under BASE: one trailing slash, then /models."
  (concatenate 'string (string-right-trim "/" base) +catalog-path+))

(defun fetch-rows (&optional (base +base+) &aux (answer nil) (status nil))
  "One fetch of the public catalog under BASE => its runnable rows, or NIL.
Sent without credentials: anybody may read it, as the original sends it."
  (handler-case
      (multiple-value-setq (answer status)
        (nlk:http :get (catalog-url base)
                  :headers '(("accept" . "application/json"))
                  :timeout +timeout+
                  :connect-timeout (min 5 +timeout+)))
    ((or error sb-sys:deadline-timeout) () (return-from fetch-rows nil)))
  (when (and (integerp status) (< status 300))
    (let ((rows (handler-case (catalog-rows (nlk:decode-json answer))
                  (error () nil))))
      (and rows (plusp (length rows)) rows))))

(defun refresh (base)
  "One step of the catalog worker: fetch the catalog under BASE, keep its rows,
and stop the worker once they have landed. Answers NIL to be woken again after
the worker's wait, so an unreachable catalog is retried rather than given up on."
  (when (setf *live-rows* (fetch-rows base))
    (forget-catalog)
    (nle:notice (format nil "hashference: ~d models from the live catalog"
                        (length *live-rows*)))
    :stop))

;;; --- the provider row --------------------------------------------------------

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Hashference's row in it.")

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base or other models
builds a new one."
  (setf *catalog* (cons nil nil)))

(defun catalog-model (row)
  "ROW, a plist from the catalog or the curated list, as a CATALOG-MODEL."
  (destructuring-bind (&key id name context output) row
    (declare (ignore id))
    ;; every row here calls tools by construction, and a turn answers in text:
    ;; the catalog states no modalities, so text is what is claimed
    (nle::make-catalog-model name context output #("text") #("text")
                              nil nil nil t nil)))

(defun catalog-row ()
  "Hashference as a models.dev provider: the chat lane's package, this
section's base, the key variable, and the rows a turn can run on."
  (let ((models (make-hash-table :test 'equal)))
    (dolist (row (or *live-rows* (fallback-rows)))
      (setf (gethash (getf row :id) models) (catalog-model row)))
    (nlk:json-object "name" "Hashference"
                      "npm" "@ai-sdk/openai-compatible"
                      "api" (setting :base-url)
                      "env" (vector +api-key-env+)
                      "models" models)))

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Hashference's row, made once
per catalog the core reads."
  (if (and (car memo) (eq (car memo) base))
      (cdr memo)
      (let ((merged (make-hash-table :test 'equal)))
        (when (hash-table-p base)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) base))
        (setf (gethash +provider+ merged) (catalog-row))
        (setf *catalog* (cons base merged))
        merged)))
