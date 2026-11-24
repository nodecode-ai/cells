;;;; provider.lisp --- what APInex is: its address, its key, its models, its two
;;;; wires.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Rewritten from APInex-AI's Hermes provider plugin (see NOTICE): the two
;;;; provider profiles in its apinex/__init__.py, their gateway addresses, the
;;;; per-model capability matrices, and the rule that splits the gateway's
;;;; model list between the two wires.
;;;;
;;;; APInex speaks two wires at one host: https://api.apinex.bond/v1/messages
;;;; takes the Claude family over the Anthropic Messages API, and
;;;; https://api.apinex.bond/v1/chat/completions takes the rest over the
;;;; OpenAI-compatible one. One key serves both. Nothing here dials the
;;;; gateway: the models the original fetched from /v1/models are bundled in
;;;; models.json beside this file, laid out the way that endpoint lists them,
;;;; and the split its fetch_models() applied is applied to them here.

(in-package #:nodecode-apinex)

(defparameter +base+ "https://api.apinex.bond/v1"
  "Where APInex is served: the base each lane appends its own path to, so the
Messages wire answers at /v1/messages and the chat one at /v1/chat/completions.")

(defparameter +models+
  (nlk:decode-json
   (uiop:read-file-string
    (asdf:system-relative-pathname "nodecode-apinex" "models.json")))
  "The gateway's model list, read when this file loads: a vector of objects,
the shape /v1/models answers, with the context window and the capability
flags the original's matrices carried beside it.")

;;; --- how the gateway's list splits ---------------------------------------------

(defparameter +anthropic-id-prefixes+ '("claude-" "free/claude-")
  "The ids that belong to the Claude family, and so to the Messages wire.")

(defparameter +media-model-tokens+ '("image" "banana" "seedance" "video" "embed"
                                     "whisper" "tts" "stt")
  "A model whose id carries one of these is not a chat model: the picker
leaves it out.")

(defparameter +non-tool-models+ '("drex-v2.0" "jev-1.13" "free/jev-1.13")
  "Roster entries the gateway lists with supportsTools false: a turn always
offers tools, so the picker leaves them out.")

(defun white-space-p (character)
  "Whether CHARACTER is whitespace, as the original's str.isspace said."
  (member character '(#\Space #\Tab #\Newline #\Return #\Page) :test #'char=))

(defun anthropic-model-p (model-id)
  "Whether MODEL-ID belongs to the Claude family on APInex."
  (some (lambda (prefix) (uiop:string-prefix-p prefix model-id))
        +anthropic-id-prefixes+))

(defun agentic-chat-model-p (model-id &key anthropic-wire)
  "Whether MODEL-ID belongs in the picker of the wire ANTHROPIC-WIRE names."
  (and (stringp model-id)
       (plusp (length model-id))
       (notany #'white-space-p model-id)
       (not (member model-id +non-tool-models+ :test #'equal))
       (notany (lambda (token) (search token model-id)) +media-model-tokens+)
       (eq (anthropic-model-p model-id) (and anthropic-wire t))))

(defun wire-models (anthropic-wire)
  "The bundled rows that belong to one wire, as a list in the order the file
lists them."
  (coerce (remove-if-not (lambda (row)
                            (agentic-chat-model-p (nlk:json-value row :string "id")
                                                  :anthropic-wire anthropic-wire))
                          +models+)
          'list))

;;; --- the rows the catalog carries ---------------------------------------------

(defun catalog-model (row)
  "ROW as the catalog keeps a model (NLE::MAKE-CATALOG-MODEL's fields)."
  (flet ((value (type key) (nlk:json-value row type key)))
    (nle::make-catalog-model
     (value :string "name")
     (value :integer "context")
     (value :integer "output")
     (if (value :boolean "vision") #("text" "image") #("text"))
     #("text")
     ;; no effort ladder is published for these; the reasoning flag is
     nil
     (value :boolean "reasoning")
     nil
     (value :boolean "tools")
     ;; no price: the gateway publishes none of its own
     nil)))

(defun catalog-row (anthropic-wire &optional prior)
  "APInex as a models.dev provider: the wire's lane package, this section's
base, the one key variable, and the bundled models over PRIOR's (the row
models.dev itself published, when it did)."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (and prior (nlk:json-value prior :object "models")))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for row in (wire-models anthropic-wire)
          do (setf (gethash (nlk:json-value row :string "id") models) (catalog-model row)))
    (nlk:json-object "name" (if anthropic-wire "APInex" "APInex (OpenAI-compatible)")
                     "npm" (if anthropic-wire "@ai-sdk/anthropic" "@ai-sdk/openai-compatible")
                     "api" (setting :base-url)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))
