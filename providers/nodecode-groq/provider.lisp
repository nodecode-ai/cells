;;;; provider.lisp --- what Groq is: its address, its key, its models, and
;;;; the reasoning_effort each of its models takes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of AhmetArif0's hermes-groq-provider (see NOTICE): its default
;;;; address, its key variable, its fallback models, the ids it leaves out of
;;;; the picker, and the per-model reasoning table it checked against the live
;;;; API.
;;;;
;;;; Groq speaks the OpenAI chat wire at https://api.groq.com/openai/v1 with a
;;;; bearer key made at console.groq.com. Two things set it apart from a plain
;;;; OpenAI-compatible endpoint, and both are why this folder exists:
;;;;
;;;;   * gpt-oss takes only low, medium and high -- Groq answers HTTP 400 for
;;;;     `default' and `none' -- qwen3.8 also takes none, and a model without
;;;;     reasoning rejects the field, so it is left out for those;
;;;;   * Groq's /models also names whisper (speech-to-text), orpheus
;;;;     (text-to-speech), prompt-guard (a 512-token classifier) and
;;;;     allam-2-7b (a 4096-token window), none of which can run a
;;;;     conversation.
;;;;
;;;; models.dev publishes Groq under the @ai-sdk/groq package, which no
;;;; Nodecode lane claims and whose row carries no address, so without this
;;;; folder the provider is a catalog row nobody can choose. This file is what
;;;; the row and the model list are made of.

(in-package #:nodecode-groq)

(defparameter +base+ "https://api.groq.com/openai/v1"
  "Where Groq is served: the base the chat lane appends /chat/completions to.")

(defparameter +env+ '("GROQ_API_KEY")
  "The environment variables a Groq key is read from, in order.")

(defparameter +key-page+ "https://console.groq.com/keys"
  "Where a key is made.")

(defparameter +docs-url+ "https://console.groq.com/docs/models"
  "Groq's own model list, what the picker's rows describe.")

(defparameter +fallback-models+
  '("openai/gpt-oss-120b" "openai/gpt-oss-20b" "qwen/qwen3.8-27b")
  "The picker's list while neither Groq nor models.dev has been read: the
original's fallback models, all chat models that take tools.")

(defparameter +excluded-markers+ '("whisper" "orpheus" "prompt-guard" "allam")
  "Id markers of the Groq models that cannot run a conversation: a
speech-to-text model with no chat endpoint, a text-to-speech model and a
classifier on a 4K window, and a 4096-token chat model.")

(defparameter +refresh-seconds+ 3600
  "How long the live /models list may stand before the worker asks again.")

(defparameter +seed-timeout+ 8
  "One /models read's ceiling, in seconds.")

;;; --- the reasoning table ---------------------------------------------------------

(defparameter +effort-ladder+
  '("none" "minimal" "low" "medium" "high" "xhigh" "max" "ultra")
  "The reasoning ladder the original ranked its picks on, weakest first; the
wire's `none' is Nodecode's `off'.")

(defparameter +reasoning-efforts+
  '(("openai/gpt-oss-" "low" "medium" "high")
    ("qwen/qwen3.8-" "none" "low" "medium" "high"))
  "The reasoning_effort values Groq accepts, by model-id prefix. Only models
whose values were checked are listed: any other model is sent no
reasoning_effort at all, and Groq uses its own default.")

(defun runs-in-nodecode-p (model-id)
  "Whether MODEL-ID belongs in the picker: a Groq model that can hold a
conversation."
  (let ((lowered (string-downcase (or model-id ""))))
    (notany (lambda (marker) (search marker lowered :test #'char-equal))
            +excluded-markers+)))

(defun effort-rank (level)
  "LEVEL's rank in +EFFORT-LADDER+, or NIL when it names no level; Nodecode's
`off' ranks as the wire's `none'."
  (and (stringp level)
       (position (if (string-equal level "off") "none" level)
                 +effort-ladder+ :test #'string-equal)))

(defun supported-efforts (model)
  "The reasoning_effort values Groq accepts for MODEL, weakest first, or NIL
when it takes none -- the plugin's table, read by model-id prefix."
  (let ((lowered (string-downcase (or model ""))))
    (cdr (assoc-if (lambda (prefix) (uiop:string-prefix-p prefix lowered))
                   +reasoning-efforts+))))

(defun floor-effort (levels)
  "The weakest level in LEVELS that is not the disabled one: what a model
that always reasons is sent when reasoning was turned off."
  (find-if-not (lambda (level) (string-equal level "none")) levels))

(defun nearest-effort (levels requested &aux (rank (effort-rank requested)))
  "The strongest level in LEVELS at or below REQUESTED that the model takes,
else its floor: a level is only ever lowered to one the model accepts, never
raised."
  (or (car (last (remove-if (lambda (level)
                              (or (string-equal level "none")
                                  (>= (effort-rank level) rank)))
                            levels)))
      (floor-effort levels)))

(defun reasoning-effort-for (model requested off-p)
  "The reasoning_effort to send for MODEL, or NIL to leave the field out.
REQUESTED is the level the lane built (NIL when it built none) and OFF-P
whether the round turned reasoning off: a level the model does not take
becomes the nearest weaker one it does, or its floor, and a model without
reasoning gets no field at all."
  (let ((levels (supported-efforts model)))
    (cond
      ;; a model Groq takes no reasoning_effort for: the field is left out
      ((null levels) nil)
      ;; reasoning turned off: `none' where the model has it, its floor otherwise
      ((or off-p (string-equal (or requested "") "none"))
       (or (find "none" levels :test #'string-equal) (floor-effort levels)))
      ((null requested) nil)
      ((find requested levels :test #'string-equal) requested)
      ((null (effort-rank requested)) nil)
      (t (nearest-effort levels requested)))))

(defun catalog-efforts (levels)
  "LEVELS as a catalog ladder: Nodecode's own rungs, the wire's `none'
spelled the way the catalog spells the disabled tier, `off'."
  (sort (remove-duplicates
         (loop for level in levels
               for rung = (if (string-equal level "none") "off" (string-downcase level))
               when (nle::effort-rank rung) collect rung)
         :test #'string-equal)
        #'< :key #'nle::effort-rank))

;;; --- Groq's model list -----------------------------------------------------------

(defvar *live* nil
  "(FETCHED-AT . IDS) of the last /models list Groq answered, IDS already
filtered to the models that can run a conversation.")

(defvar *live-lock* (bt2:make-lock :name "groq-models")
  "Held while the list is read or replaced, so a turn never reads half of
one.")

(defun parse-model-ids (items)
  "Groq's /models `data' array as the ids that can run a conversation; NIL
when the payload names nothing."
  (let ((ids '()))
    (loop for item across (if (vectorp items) items #())
          when (hash-table-p item)
            do (let ((id (nlk:json-value item :text "id")))
                 (when (and id (runs-in-nodecode-p id)) (push id ids))))
    (nreverse ids)))

(defun fetch-model-ids (base api-key timeout)
  "The `data' array of {BASE}/models, or NIL on any failure: never signals."
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

(defun credential-key ()
  "The key the catalog read carries: GROQ_API_KEY, else the key /connect
saved, else NIL."
  (or (env-key)
      (let ((credential (ignore-errors (nle::resolve-provider-credential +provider+ :probe t))))
        (and credential
             (not (eq :public (nle:credential-source credential)))
             (nle:credential-key credential)))))

(defun refresh-live (&aux (key (credential-key)))
  "Ask Groq for its model list and keep it; a read with no key, or one that
fails, keeps the list already held."
  (let ((ids (and key (parse-model-ids (fetch-model-ids (setting :base-url) key +seed-timeout+)))))
    (when ids
      (bt2:with-lock-held (*live-lock*)
        (setf *live* (cons (get-universal-time) ids))))
    (live-models)))

(defun live-models ()
  "The ids the last successful read kept, or NIL."
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

;;; --- the catalog row -------------------------------------------------------------

(defun catalog-model (model-id efforts)
  "MODEL-ID as the catalog keeps a model (NLE::MAKE-CATALOG-MODEL's fields),
with EFFORTS the values Groq takes for it, or NIL when it takes none."
  (nle::make-catalog-model model-id nil nil #("text") #("text")
                            (and efforts (catalog-efforts efforts))
                            (and efforts t)
                            nil t nil))

(defun catalog-models (&optional prior)
  "The models the picker offers: the live list, each id with whatever row
models.dev published for it, else models.dev's own Groq rows filtered to what
can run a conversation, else the fallback list."
  (let ((published (nlk:json-value prior :object "models"))
        (live (live-models)))
    (cond
      (live (loop for id in live
                  collect (or (and (hash-table-p published) (gethash id published))
                              (catalog-model id (supported-efforts id)))))
      ((and (hash-table-p published) (plusp (hash-table-count published)))
       (loop for id being the hash-keys of published using (hash-value row)
             when (runs-in-nodecode-p id) collect row))
      (t (mapcar (lambda (id) (catalog-model id (supported-efforts id)))
                 +fallback-models+)))))

(defun catalog-row (&optional prior)
  "Groq as a models.dev provider: the chat lane's package, this section's
base, the key variable, and its models over PRIOR's (the row models.dev
itself published, when it did)."
  (let ((models (make-hash-table :test 'equal)))
    (loop for model in (catalog-models prior)
          do (setf (gethash (nle::catalog-model-name model) models) model))
    (nlk:json-object "name" "Groq"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (setting :base-url)
                     "doc" +docs-url+
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))
