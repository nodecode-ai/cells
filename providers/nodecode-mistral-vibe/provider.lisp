;;;; provider.lisp --- what Mistral Vibe is: its address, its key, its models, its wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Rewritten from nebneo's hermes-mistral-vibe (see NOTICE): vibe_provider.py
;;;; (the address, the key variable, the model discovery, the fallback model,
;;;; and the relay's reasoning-effort vocabularies) and plugin.yaml.
;;;;
;;;; The plugin is a chat-completions provider: the relay at
;;;; https://api.mistral.ai/v1 answers the OpenAI wire, a key made by the
;;;; browser sign-in (signin.lisp) or set as MISTRAL_VIBE_API_KEY is sent as a
;;;; bearer, and the account's own model list is read from GET /models. Two
;;;; things it adds over a plain compatible endpoint:
;;;;
;;;;   - the key is its own: the plugin never sends a MISTRAL_API_KEY, so a
;;;;     plain Mistral key cannot cross into a Vibe round by accident.
;;;;   - the relay's `reasoning_effort' vocabulary is per model and narrower
;;;;     than anyone else's (GLM-5.3/5/latest take low/high/max, GLM-5.2 adds
;;;;     medium; the relay cannot turn thinking off at all), so a configured
;;;;     effort is clamped down to what the model's relay accepts and an
;;;;     unverified model sends none, keeping the relay's own default.
;;;;
;;;; The plugin pins the origin, refuses a request carrying extra headers,
;;;; body or query, and disables redirects and environment proxies; here the
;;;; section's base_url is the address a turn dials, the way it is for any
;;;; Nodecode provider, and the default is the pinned one.

(in-package #:nodecode-mistral-vibe)

(defparameter +base+ "https://api.mistral.ai/v1"
  "Where the relay is served: the base the chat lane appends /chat/completions to.")

(defparameter +console+ "https://console.mistral.ai"
  "The console the browser sign-in talks to.")

(defparameter +env+ '("MISTRAL_VIBE_API_KEY")
  "The environment variables a Vibe key is read from, in order: the plugin's
own, and never MISTRAL_API_KEY.")

(defparameter +fallback-model+ "mistral-vibe-cli-latest"
  "The model the plugin always offers, whatever the account's listing says.")

(defparameter +models+
  '(("mistral-vibe-cli-latest" "Mistral Vibe CLI" nil nil)
    ("zai-glm-5-3" "GLM-5.3" ("low" "high" "max") t)
    ("zai-glm-5" "GLM-5" ("low" "high" "max") t)
    ("zai-glm-latest" "GLM latest" ("low" "high" "max") t)
    ("zai-glm-5-2" "GLM-5.2" ("low" "medium" "high" "max") t))
  "The rows this folder publishes before any listing: the fallback model and
the relay-verified GLM models, each with the efforts the relay takes for it
(taken from vibe_provider.py:21-24, live-verified 2026-10-03). What the
account actually sees comes from GET /models, which /models asks with the
key; these rows make the provider pickable and a first turn possible
without one.")

;;; --- the models ----------------------------------------------------------------

(defun catalog-model (name context efforts reasoning-p)
  "The catalog's model record (NLE::MAKE-CATALOG-MODEL's fields) for a chat
model of the relay."
  (nle::make-catalog-model
   name context nil #("text") #("text")
   (sort (copy-list efforts) #'< :key (lambda (level) (or (nle::effort-rank level) 0)))
   reasoning-p nil t nil))

(defun model-table ()
  "The rows above as a models.dev `models' object."
  (let ((models (make-hash-table :test 'equal)))
    (loop for (id name efforts reasoning) in +models+
          do (setf (gethash id models)
                   (catalog-model name nil (or efforts '()) (and reasoning t))))
    models))

(defun catalog-row (&optional prior)
  "Mistral Vibe as a models.dev provider: the chat lane's package, this
section's base, the key variable, and this folder's rows over PRIOR's (the
row models.dev itself published, when it did)."
  (let ((models (model-table)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model)
                 (unless (gethash id models) (setf (gethash id models) model)))
               published))
    (nlk:json-object "name" "Mistral Vibe"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (setting :base-url)
                     "env" (coerce (remove-duplicates
                                    (append +env+ (coerce (or (nlk:json-value prior :array "env") #()) 'list))
                                    :test #'equal :from-end t)
                                   'vector)
                     "models" models)))

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))

;;; --- the relay's reasoning vocabulary ---------------------------------------------

;; The relay validates `reasoning_effort' per model and answers HTTP 400 code
;; 3051 with the set it takes. Its sets are not z.ai's native ones, and
;; `none' cannot switch thinking off at all, so an unverified model is sent
;; no effort and keeps the relay's own default.
(defparameter +glm-efforts+ '("low" "high" "max"))

(defparameter +glm-52-efforts+ '("low" "medium" "high" "max"))

(defparameter +glm-53-tokens+ '("glm-5.3" "glm-5-3" "glm-5p3"))

(defparameter +glm-52-tokens+ '("glm-5.2" "glm-5-2" "glm-5p2"))

(defparameter +top-tier+ '("xhigh" . "max")
  "The vendor's top tier, and the relay's spelling of it.")

(defun relay-efforts (model)
  "The efforts the relay takes for MODEL, or NIL when its GLM version was not
verified there: what vibe_provider.py's _glm_effort_set answers."
  (let ((name (string-downcase (string-trim " " (or model "")))))
    (flet ((token-p (token) (not (null (search token name)))))
      (cond ((some #'token-p +glm-53-tokens+) +glm-efforts+)
            ((some #'token-p +glm-52-tokens+) +glm-52-efforts+)
            ;; verified on the 5 and latest rows; a future 5.x point release
            ;; lands here too, and the relay's refusal is then the word
            ((or (token-p "glm-5") (token-p "glm-latest")) +glm-efforts+)))))

(defun highest-of (levels)
  "The strongest rung of LEVELS."
  (reduce (lambda (a b) (if (> (or (nle::effort-rank b) -1) (or (nle::effort-rank a) -1)) b a))
          levels))

(defun lowest-of (levels)
  "The weakest rung of LEVELS."
  (reduce (lambda (a b) (if (< (or (nle::effort-rank b) 99) (or (nle::effort-rank a) 99)) b a))
          levels))

(defun clamp-relay-effort (effort levels)
  "EFFORT as the relay accepts it on LEVELS: the top tier reads its top rung,
any other rung the highest LEVELS holds at or below it (never an escalation),
and a rung LEVELS has nothing at or below the weakest. An effort outside the
shared ladder passes untouched, as the relay's own business."
  (cond ((member effort levels :test #'string-equal) effort)
        ((and (string-equal effort (car +top-tier+)) (member (cdr +top-tier+) levels :test #'equal))
         (cdr +top-tier+))
        ((null (nle::effort-rank effort)) effort)
        ((let* ((rank (nle::effort-rank effort))
                (below (remove-if (lambda (level)
                                    (or (null (nle::effort-rank level))
                                        (> (nle::effort-rank level) rank)))
                                  levels)))
           (and below (highest-of below))))
        (t (lowest-of (remove-if-not #'nle::effort-rank levels)))))

(defun shape-effort (body model)
  "BODY, a chat request for MODEL, with `reasoning_effort' as the relay takes
it: clamped for a verified model, absent for any other."
  ;; The key is written by the chat lane from the session's effort; here it is
  ;; rewritten, never invented, so a request that carried none keeps none.
  (let ((effort (nlk:json-value body :string "reasoning_effort"))
        (levels (relay-efforts model)))
    (when (and effort (hash-table-p body))
      (if levels
          (setf (gethash "reasoning_effort" body) (clamp-relay-effort effort levels))
          (remhash "reasoning_effort" body)))
    body))
