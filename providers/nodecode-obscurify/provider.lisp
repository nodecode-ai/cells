;;;; provider.lisp --- what Obscurify.ai is: its address, its key, its models,
;;;; their windows, and the reasoning_effort it takes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of Obscurify-ai/hermes-obscurify-provider's __init__.py (see
;;;; NOTICE): the provider id and its base, the key variable, the featured
;;;; models with the capabilities the plugin checked for them, and the two
;;;; readings of https://obscurify.ai/v1/models it made its own.
;;;;
;;;; Obscurify.ai speaks the OpenAI chat wire at https://obscurify.ai/v1 with a
;;;; bearer key from its account page. Two things set it apart from a plain
;;;; OpenAI-compatible endpoint, and both are why this folder exists:
;;;;
;;;;   * its public /v1/models catalog also lists decision models, which only
;;;;     answer structured questions (a chat request without their
;;;;     response_format is refused), and models whose context window is under
;;;;     64K. Both are left out of the picker.
;;;;   * the same catalog carries each model's context window, so a turn's
;;;;     budget is the API's own number rather than a guess.
;;;;
;;;; The catalog needs no key, so it is read unauthenticated, at most once an
;;;; hour, and the key rides only on chat turns. The plugin's 64K floor is no
;;;; Nodecode rule, so it is the section's min_context_tokens here.

(in-package #:nodecode-obscurify)

(defparameter +base+ "https://obscurify.ai/v1"
  "Where Obscurify.ai is served: the base the chat lane appends
/chat/completions to.")

(defparameter +env+ '("OBSCURIFY_API_KEY")
  "The environment variables an Obscurify.ai key is read from, in order.")

(defparameter +signup-url+ "https://obscurify.ai"
  "Where an account is made: a random account number, no email, no password.")

(defparameter +min-context+ 64000
  "The context floor the plugin dropped models under, in tokens: Hermes
refused to start a model with less. It is the section's default now.")

(defparameter +refresh-seconds+ 3600
  "How long the public catalog may stand before it is read again: the
plugin's CATALOG_TTL_SECONDS.")

(defparameter +seed-timeout+ 8
  "One catalog read's ceiling, in seconds: the plugin's own 8.0.")

(defparameter +featured-models+
  '(("deepseek/deepseek-v4-flash" 1048576 t)
    ("z-ai/glm-5.3" 1048576 t)
    ("moonshotai/kimi-k2-0905" 262144 nil)
    ("qwen/qwen3-coder" 262000 nil)
    ("meta-llama/llama-3.3-70b-instruct" 131072 nil))
  "The plugin's fallback_models, each with the context window and the
reasoning flag its model_capabilities named for it: what the picker lists
while the catalog has not answered.")

(defparameter +efforts+
  '(("off" . "none")
    ("minimal" . "minimal")
    ("low" . "low")
    ("medium" . "medium")
    ("high" . "high")
    ("xhigh" . "xhigh")
    ("max" . "max"))
  "Nodecode's rungs and the reasoning_effort the API takes for each: the
plugin's EFFORTS table. Its `ultra' was a top rung of its own, which this
vocabulary spells `max' already; Nodecode's `off' goes out as the API's
`none'.")

;;; --- the catalog's readings ------------------------------------------------------

(defun capability (model-id)
  "MODEL-ID's (WINDOW REASONING) in the plugin's capability table, or NIL."
  (cdr (assoc model-id +featured-models+ :test #'equal)))

(defun wire-effort (level)
  "The reasoning_effort the API accepts for Nodecode's LEVEL, or NIL to leave
the field as the lane built it."
  (and (stringp level) (cdr (assoc level +efforts+ :test #'string-equal))))

(defun decisions-p (entry)
  "Whether ENTRY, one catalog row, is a decision model: it answers structured
questions only, so a chat turn cannot use it."
  (and (nlk:json-value entry :any "decisions") t))

(defun entry-window (entry)
  "The context window ENTRY publishes, or NIL when it publishes none."
  (let ((window (nlk:json-value entry :integer "context_length")))
    (and window (plusp window) window)))

(defun clears-floor-p (entry)
  "Whether ENTRY clears the section's context floor: a row that publishes no
window is kept, as the plugin kept one whose context it could not read."
  (let ((window (entry-window entry)))
    (or (null window) (>= window (setting :min-context-tokens)))))

(defun runs-in-nodecode-p (entry)
  "Whether ENTRY belongs in the picker: it names an id, is no decision model,
and clears the section's context floor."
  (and (nlk:json-value entry :text "id")
       (not (decisions-p entry))
       (clears-floor-p entry)))

(defun stated-reasoning (entry)
  "(values FLAG PRESENT): ENTRY's own `reasoning' flag, as the catalog
published it."
  (if (hash-table-p entry)
      (gethash "reasoning" entry)
      (values nil nil)))

(defun entry-reasoning (model-id entry)
  "Whether the picker offers rungs for MODEL-ID: ENTRY's own reasoning flag
when it carries one, else the plugin's capability row for it, else T -- the
plugin sent reasoning_effort to every model, and the ones that do not reason
ignored it."
  (multiple-value-bind (flag present) (stated-reasoning entry)
    (cond (present (and flag (not (eq flag :null))))
          ((capability model-id) (third (capability model-id)))
          (t t))))

(defun catalog-rows (body)
  "The rows BODY, an Obscurify.ai /v1/models answer, carries for the models a
turn can run: (:id :window :reasoning) each, in the catalog's order, each id
once; NIL when BODY is not the catalog or names none such."
  (let ((payload (ignore-errors (nlk:decode-json body)))
        (seen (make-hash-table :test 'equal))
        (rows '()))
    (loop for entry across (or (nlk:json-value payload :array "data") #())
          for model-id = (nlk:json-value entry :text "id")
          when (and model-id (runs-in-nodecode-p entry) (not (gethash model-id seen)))
            do (setf (gethash model-id seen) t)
               (push (list :id model-id
                           :window (or (entry-window entry)
                                       (second (capability model-id)))
                           :reasoning (entry-reasoning model-id entry))
                     rows))
    (nreverse rows)))

;;; --- the catalog, held ----------------------------------------------------------

(defvar *live* nil
  "(GENERATION . ROWS) of the last catalog Obscurify.ai answered, ROWS
already filtered to the models a turn can run; the generation is what a
catalog read compares, so a refresh is seen.")

(defvar *generation* 0
  "Counts successful catalog reads: never reused, so two reads within one
second are still two.")

(defvar *live-lock* (bt2:make-lock :name "obscurify-models")
  "Held while the rows are read or replaced, so a reader never sees half of
one.")

(defun live-rows ()
  "The rows the last successful read kept, or NIL."
  (bt2:with-lock-held (*live-lock*)
    (cdr *live*)))

(defun live-stamp ()
  "The generation of the rows now held, or NIL."
  (bt2:with-lock-held (*live-lock*)
    (car *live*)))

(defun forget-live ()
  "Drop the rows, so a restart against another base asks again."
  (bt2:with-lock-held (*live-lock*)
    (setf *live* nil)))

(defun models-url ()
  "The catalog this cell reads: the section's base with /models on it."
  (uiop:strcat (string-right-trim "/" (setting :base-url)) "/models"))

(defun fetch-rows ()
  "The rows the public catalog names, or NIL when the read fails, names none,
or is not the catalog: never signals."
  ;; The catalog is public -- the plugin fetched it without credentials -- so
  ;; no key rides here, and the picker lists models before one is saved.
  (multiple-value-bind (body)
      (nle:http-fetch (models-url)
                      :headers '(("accept" . "application/json"))
                      :timeout (setting :timeout-seconds))
    (and (stringp body) (catalog-rows body))))

(defun refresh-live ()
  "Ask Obscurify.ai for its catalog and keep its rows; a read that answers
none keeps the rows already held."
  (let ((rows (fetch-rows)))
    (when rows
      (bt2:with-lock-held (*live-lock*)
        (setf *live* (cons (incf *generation*) rows))))
    (live-rows)))

;;; --- the catalog row -------------------------------------------------------------

(defun catalog-model (model-id window reasoning)
  "MODEL-ID as the catalog keeps a model (NLE::MAKE-CATALOG-MODEL's fields):
text in and out, WINDOW its context, and the API's rungs when REASONING."
  (nle::make-catalog-model model-id window nil #("text") #("text")
                            (and reasoning (coerce nle::+effort-ladder+ 'list))
                            reasoning nil t nil))

(defun listed-model (model-id window reasoning published)
  "MODEL-ID as the picker lists it: models.dev's own row when it has one with
a window, else this cell's row carrying what the catalog said."
  (let ((row (and (hash-table-p published) (gethash model-id published))))
    (if (and (typep row 'nle::catalog-model) (nle::catalog-model-context row))
        row
        (catalog-model model-id window reasoning))))

(defun catalog-models (&optional prior)
  "The models the picker offers: the rows the catalog last named, each with
whatever row models.dev published for it, else the plugin's featured list
while the catalog has not answered."
  (let ((published (nlk:json-value prior :object "models"))
        (live (live-rows)))
    (if live
        (loop for row in live
              collect (listed-model (getf row :id) (getf row :window)
                                    (getf row :reasoning) published))
        (loop for (model-id window reasoning) in +featured-models+
              collect (listed-model model-id window reasoning published)))))

(defun catalog-row (&optional prior)
  "Obscurify.ai as a models.dev provider: the chat lane's package, this
section's base, the key variable, and its models over PRIOR's (the row
models.dev itself published, when it did)."
  (let ((models (make-hash-table :test 'equal)))
    (loop for model in (catalog-models prior)
          do (setf (gethash (nle::catalog-model-name model) models) model))
    (nlk:json-object "name" "Obscurify.ai"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (setting :base-url)
                     "doc" +signup-url+
                     "env" (coerce +env+ 'vector)
                     "models" models)))

;;; --- the key --------------------------------------------------------------------

(defun env-key ()
  "The key OBSCURIFY_API_KEY holds, or NIL."
  (some #'nle::credential-env +env+))

(defun credential (op next)
  "The :CREDENTIAL answer for obscurify: OBSCURIFY_API_KEY, else none. A key
/connect saved in auth.json answers before this point does."
  ;; Never NEXT for obscurify: the ladder behind this point falls back to the
  ;; chat family's default variable, and would send OPENAI_API_KEY to
  ;; Obscurify.ai.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (env-key))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))
