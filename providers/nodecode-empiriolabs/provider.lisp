;;;; provider.lisp --- what EmpirioLabs AI is: its address, its key, its catalog.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of EmpirioLabs-AI's Hermes plugin (see NOTICE):
;;;; __init__.py, its catalog reader (_fetch_rows, _is_chat_with_tools,
;;;; _supports_vision, _supports_reasoning, _context_length, _base_price,
;;;; _reasoning_options), its auxiliary pick (resolve_aux_model) and its
;;;; reasoning rule (_clamp_effort, _requested_effort).
;;;;
;;;; EmpirioLabs AI speaks the OpenAI chat wire at
;;;; https://api.empiriolabs.ai/v1 with a bearer key from its dashboard, and
;;;; publishes its catalog at GET /v1/models. Two things below the plain
;;;; OpenAI-compatible surface come from that catalog, not from the wire:
;;;;
;;;;   - the model list is the catalog's chat models that take tool calls
;;;;     (Hermes sends tools with every request, so the original left the
;;;;     others out; so does this cell)
;;;;   - a round asks for reasoning_effort only when the model declares it,
;;;;     at the nearest level it offers that is not dearer than the one
;;;;     chosen
;;;;
;;;; The catalog is kept in this cell's memory (CATALOG-RECORDS), filled by
;;;; the listing hook the picker and /connect already call, and read back
;;;; into the catalog row so context window, image input, reasoning and price
;;;; reach the picker and the turn. Nothing is written to disk.

(in-package #:nodecode-empiriolabs)

(defparameter +base+ "https://api.empiriolabs.ai/v1"
  "Where EmpirioLabs AI is served: the base the chat lane appends
/chat/completions to.")

(defparameter +env+ '("EMPIRIOLABS_API_KEY")
  "The environment variable a key is read from, as the original reads it.")

(defparameter +key-page+ "https://platform.empiriolabs.ai/dashboard/api-keys"
  "Where a key is made.")

(defparameter +models-path+ "/models"
  "The catalog, relative to the base.")

(defparameter +chat-endpoint+ "POST /v1/chat/completions"
  "The endpoint a catalog row names to say it is a chat model.")

(defparameter +tool-feature+ "function_calling"
  "The catalog feature that says a model takes tool calls.")

(defparameter +effort-ladder+ '("none" "minimal" "low" "medium" "high" "xhigh" "max" "ultra")
  "Nodecode's reasoning ladder, weakest to strongest, as the original's
EFFORT_LADDER has it.")

(defparameter +fallback-ids+ '("glm-5-3" "qwen3-7-flash")
  "The two ids the original offers when its catalog cannot be fetched
(OFFLINE_FALLBACK_MODELS), bare: the catalog is where their details live.")

(defparameter +timeout+ 8
  "Seconds one catalog GET may take, as the original's fetch.")

(defparameter +max-bytes+ (* 8 1024 1024)
  "The catalog body's cap, as the original's MAX_CATALOG_BYTES, in a size
that suits a listing of every model the service serves.")

(defparameter +user-agent+ "nodecode-empiriolabs/0.1.0"
  "What the catalog GET calls itself.")

;;; --- the section ---------------------------------------------------------------

(defun present (value)
  "VALUE trimmed when it is a non-empty string, else NIL."
  (and (stringp value)
       (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
         (and (plusp (length trimmed)) trimmed))))

(defun base-url ()
  "The API base: the section's base_url, else the original's default."
  (or (present (setting :base-url)) +base+))

(defun catalog-url ()
  "The catalog's address: <base>/models."
  (uiop:strcat (string-right-trim "/" (base-url)) +models-path+))

(defun env-key ()
  "The key EMPIRIOLABS_API_KEY holds, or NIL."
  (some #'nle::credential-env +env+))

;;; --- the catalog, as the service publishes it ------------------------------------

(defun string-list (row key)
  "ROW's KEY as a list of strings, or NIL when it is not an array."
  (let ((value (nlk:json-value row :array key)))
    (and (vectorp value) (coerce value 'list))))

(defun chat-with-tools-p (row)
  "Whether ROW is a chat-completions model with tool calling and not
deprecated, the original's _is_chat_with_tools."
  (and (member +chat-endpoint+ (string-list row "supported_endpoints") :test #'equal)
       (member +tool-feature+ (string-list row "features") :test #'equal)
       (not (nlk:json-value row :boolean "deprecated"))))

(defun catalog-metadata-p (rows)
  "Whether ROWS carry the service's own catalog fields, or are a plain
OpenAI-style list from another endpoint (the original's
_has_catalog_metadata)."
  (some (lambda (row) (nth-value 1 (nlk:json-value row :any "supported_endpoints")))
        rows))

(defun supports-vision-p (row)
  "Whether ROW takes images in, as the original reads it."
  (not (null (or (member "image" (string-list row "input_modalities") :test #'equal)
                 (member "vision" (string-list row "features") :test #'equal)))))

(defun supports-reasoning-p (row)
  "Whether ROW reasons, as the original reads it."
  (not (null (or (nlk:json-value row :boolean "capabilities" "reasoning")
                 (member "reasoning" (string-list row "features") :test #'equal)))))

(defun context-length (row)
  "ROW's context window, under either name the catalog gives it."
  (let ((window (or (nlk:json-value row :integer "context_length")
                    (nlk:json-value row :integer "context_window"))))
    (and window (plusp window) window)))

(defun base-prices (row)
  "ROW's input and output price per token at the base tier, as (values INPUT
OUTPUT); NIL for both when it is not priced per token (the original's
_base_price)."
  (let ((pricing (nlk:json-value row :object "pricing")))
    (unless pricing
      (let ((tiers (nlk:json-value row :array "pricing")))
        (setf pricing (and (vectorp tiers) (plusp (length tiers))
                           (nlk:json-value (aref tiers 0) :object)))))
    (when pricing
      (let ((input (nlk:json-value pricing :number "prompt"))
            (output (nlk:json-value pricing :number "completion")))
        (when (and input output) (values input output))))))

(defun reasoning-options (row)
  "ROW's declared reasoning_effort values, lower-cased, or NIL when it
declares none (the original's _reasoning_options)."
  (loop for param across (or (nlk:json-value row :array "supported_parameters") #())
        when (and (hash-table-p param)
                  (equal "reasoning_effort" (nlk:json-value param :string "name")))
          return (loop for option across (or (nlk:json-value param :array "options") #())
                       for text = (and (stringp option)
                                       (string-downcase (string-trim '(#\Space) option)))
                       when (and text (plusp (length text)))
                         collect text)))

(defun record (row)
  "ROW as this cell keeps a catalog model: a plist of what a turn reads."
  (multiple-value-bind (input output) (base-prices row)
    (list :id (nlk:json-value row :text "id")
          :display (or (nlk:json-value row :text "display_name")
                       (nlk:json-value row :text "id"))
          :context (context-length row)
          :vision (supports-vision-p row)
          :reasoning (supports-reasoning-p row)
          :efforts (reasoning-options row)
          :input input
          :output output)))

(defun cartesian-rows (payload)
  "PAYLOAD's model rows: its data array, or itself when it is the array."
  (cond ((hash-table-p payload)
         (let ((data (nlk:json-value payload :array "data")))
           (and (vectorp data) (coerce data 'list))))
        ((vectorp payload) (coerce payload 'list))))

(defun parse-catalog (payload)
  "(values RECORDS ERROR): the chat models PAYLOAD lists, each a RECORD. A
plain OpenAI-style list is taken whole; the service's own is filtered to the
chat models that take tool calls, as the original did."
  (let ((rows (cartesian-rows payload)))
    (unless rows (return-from parse-catalog (values nil "the catalog carries no model list")))
    (let ((rows (remove-if-not (lambda (row)
                                  (and (hash-table-p row) (nlk:json-value row :text "id")))
                                rows))
          (metadata (catalog-metadata-p (remove-if-not #'hash-table-p rows))))
      (values (loop for row in rows
                    when (or (not metadata) (chat-with-tools-p row))
                      collect (record row))
              nil))))

;;; --- what a record says to the wire and to the menu -------------------------------

(defun ladder (options)
  "OPTIONS narrowed to the rungs Nodecode knows, weakest first. A model's own
list is what it takes; the rest of a vendor's vocabulary is not a rung."
  (sort (remove-duplicates (remove-if-not (lambda (option) (nle::effort-rank option))
                                           options)
                            :test #'equal)
        #'< :key #'nle::effort-rank))

(defun clamp-effort (requested options)
  "REQUESTED mapped onto OPTIONS without ever going dearer, the original's
_clamp_effort: the level itself when offered, else the nearest lower one,
else the weakest; NIL when the model declares no rung Nodecode knows, or the
request is outside the ladder and undeclared."
  (cond ((member requested options :test #'equal) requested)
        (t (let ((enabled (remove "none" (ladder options) :test #'equal)))
             (when (and enabled (member requested +effort-ladder+ :test #'equal))
               (let ((weakest (first enabled)))
                 (if (equal requested "none")
                     weakest
                     (let ((weaker (remove-if-not
                                    (lambda (option)
                                      (< (nle::effort-rank option) (nle::effort-rank requested)))
                                    enabled)))
                       (or (first (sort (copy-list weaker) #'> :key #'nle::effort-rank))
                           weakest)))))))))

(defun listing-row (entry)
  "ENTRY as a provider listing row: the id, its display name, its context."
  (list :id (getf entry :id)
        :display (getf entry :display)
        :context-window (getf entry :context)
        :kind "chat"))

(defun catalog-model (entry)
  "ENTRY as the catalog keeps a model (NLE::MAKE-CATALOG-MODEL's fields):
the details the original declared to Hermes, as Nodecode reads them."
  (let ((input (getf entry :input)) (output (getf entry :output)))
    (nle::make-catalog-model
     (getf entry :display)
     (getf entry :context)
     nil
     (if (getf entry :vision) #("text" "image") #("text"))
     #("text")
     (ladder (getf entry :efforts))
     (getf entry :reasoning)
     nil
     t
     (and input output (or (plusp input) (plusp output))
          (list input output nil nil)))))

(defun fallback-model (id)
  "The catalog model a bare fallback id is: it runs a turn, and nothing more
is known of it until the catalog answers."
  (nle::make-catalog-model id nil nil #("text") #("text") nil nil nil t nil))
