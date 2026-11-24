;;;; provider.lisp --- what LLM API is: its address, its key, its public model
;;;; feed, and the wire each of its models is sent on.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of llmapi-ai's hermes-provider-llmapi (see NOTICE): its default
;;;; address, its key variable, its public catalog, its per-model wire rules,
;;;; the attribution headers it adds and the credit balance it reads.
;;;;
;;;; LLM API (https://llmapi.ai) is one gateway with one key for Claude, GPT,
;;;; Gemini, Kimi, DeepSeek, GLM, Qwen and more, all served at
;;;; https://api.llmapi.ai/v1. Three things make it more than a plain
;;;; OpenAI-compatible endpoint, and they are why this folder exists:
;;;;
;;;;   * a model is sent on the wire it works best on -- claude-* on the
;;;;     Anthropic Messages API (where the lane adds its prompt-cache
;;;;     breakpoints), gpt-5 and later on the Responses API, everything else
;;;;     on Chat Completions -- all at the one address;
;;;;   * the picker, its context windows, image input, reasoning levels and
;;;;     list prices come from the gateway's own public catalog, GET
;;;;     /v1/models, which needs no key;
;;;;   * GET /v1/credits answers the account's balance, which /llmapi usage
;;;;     prints.
;;;;
;;;; models.dev publishes no row for this gateway, so without this folder the
;;;; provider is a catalog row nobody can choose. This file is what the row
;;;; and the model list are made of.

(in-package #:nodecode-llmapi)

(defparameter +base+ "https://api.llmapi.ai/v1"
  "Where LLM API is served: the base each lane appends its own path to
(/chat/completions, /messages, /responses).")

(defparameter +env+ '("LLMAPI_API_KEY")
  "The environment variables a key is read from, in order.")

(defparameter +key-page+ "https://app.llmapi.ai/dashboard/api-keys"
  "Where a key is made.")

(defparameter +docs-url+ "https://llmapi.ai"
  "What the gateway describes itself as, for the picker's row.")

(defparameter +user-agent+ "nodecode-llmapi/0.1.0"
  "What the catalog read names itself as. The gateway serves it unauthenticated.")

(defparameter +aux-model+ "gemini-3.5-flash-lite"
  "The original's cheap, image-reading model for side tasks; kept for the
README, since Nodecode names its own auxiliary model.")

(defparameter +min-context+ 64000
  "The floor the original put under a picker entry: a model with a smaller
window was left out, because Hermes refused to start a session below it. The
floor is kept here as the original set it; a model under it is still served
when it is named outright.")

(defparameter +fallback-models+
  '("claude-sonnet-5-5" "claude-opus-5-5" "claude-haiku-5-5" "gpt-5.5"
    "gpt-6.1-sol" "gemini-3.8-flash" "kimi-k3" "deepseek-v4-pro" "glm-5.3")
  "The picker while the gateway's catalog has not answered: the featured
models the original names, all chat models that take tools.")

(defparameter +refresh-seconds+ 3600
  "How long the live catalog may stand before it is read again.")

(defparameter +failed-retry-seconds+ 300
  "How long a failed catalog read is left alone before another is tried.")

(defparameter +seed-timeout+ 8
  "One catalog read's ceiling, in seconds.")

(defparameter +usage-timeout+ 5
  "One /credits read's ceiling, in seconds.")

(defparameter +attribution+
  '(("x-source" . "nodecode")
    ("x-llmapi-app-id" . "nodecode-llmapi"))
  "What every request names itself as to the gateway: the two headers the
original added to the first request of each turn. The app named is this one.")

;;; --- the wire each model rides ---------------------------------------------------

(defun base-name (model)
  "MODEL with any vendor prefix and any case taken off:
`deepseek/deepseek-v4-pro' -> `deepseek-v4-pro'."
  (let* ((name (nlk:trimmed (or model "")))
         (slash (position #\/ name :from-end t)))
    (string-downcase (if slash (subseq name (1+ slash)) name))))

(defun gpt-5-or-later-p (name)
  "Whether NAME is a GPT model of the 5th generation or later: gpt-5,
gpt-5.5, gpt-6.1-sol -- never gpt-4.x or gpt-oss, which have no version
after the dash."
  ;; The original's regular expression ^gpt-([5-9]|[1-9]\d)(\b|[.-]), read
  ;; without a regular-expression engine: the digits after `gpt-' are the
  ;; version, and only a version of five or more, followed by nothing or by a
  ;; dot or a dash, counts.
  (let ((prefix "gpt-"))
    (when (and (>= (length name) (length prefix))
               (string-equal prefix name :end2 (length prefix)))
      (let* ((rest (subseq name (length prefix)))
             (digits (position-if-not #'digit-char-p rest))
             (version (and digits (plusp digits)
                           (parse-integer rest :end digits :junk-allowed t)))
             (after (and digits (subseq rest digits))))
        (and version
             (>= version 5)
             (or (null after) (string= after "") (member (char after 0) '(#\. #\-))))))))

(defun lane-for (model)
  "The lane MODEL rides at LLM API: the Anthropic Messages wire for a Claude
model, the Responses wire for GPT-5 and later, Chat Completions for
everything else. Decided from the id alone, so it never waits on the catalog."
  (let ((name (base-name model)))
    (cond ((uiop:string-prefix-p "claude-" name) "anthropic")
          ((gpt-5-or-later-p name) "openai-responses")
          (t "openai-completions"))))

(defun lane-name-for (model)
  "LANE-FOR's answer as the API mode the original named it, for a reader who
knows that vocabulary: anthropic_messages, codex_responses, chat_completions."
  (ecase (intern (string-upcase (lane-for model)) :keyword)
    (:|ANTHROPIC| "anthropic_messages")
    (:|OPENAI-RESPONSES| "codex_responses")
    (:|OPENAI-COMPLETIONS| "chat_completions")))

;;; --- the model feed --------------------------------------------------------------

(defvar *feed* nil
  "(FETCHED-AT . MODELS): the last catalog the gateway answered, MODELS a
list of NLE::MAKE-CATALOG-MODEL rows in the order it named them.")

(defvar *feed-failed-at* nil
  "When a read last failed, so a gateway that is down is not asked every turn.")

(defvar *feed-lock* (bt2:make-lock :name "llmapi-models")
  "Held while the catalog is read or replaced, so a turn never reads half of
one.")

(defparameter +efl-1e6+ 1000000
  "What the feed's per-token prices are multiplied by: the catalog keeps
US dollars per million tokens.")

(defun priced (value)
  "VALUE, a per-token price from the feed, in the catalog's per-million
units, or NIL when it is not a number."
  (and (numberp value) (>= value 0) (* value +efl-1e6+)))

(defun modality-p (modalities kind)
  "Whether MODALITIES, a JSON array, names KIND."
  (and (vectorp modalities)
       (find kind modalities :test (lambda (a b) (and (stringp a) (string-equal a b))))))

(defun entry-tools-p (entry)
  "Whether any provider of ENTRY takes tool calls."
  (loop for provider across (or (nlk:json-value entry :array "providers") #())
        thereis (and (hash-table-p provider) (eq t (nlk:json-value provider :boolean "tools")))))

(defun entry-kind (entry &aux (architecture (nlk:json-value entry :object "architecture")))
  "The catalog kind of ENTRY; the gateway omits it when empty, so text in and
text out is a chat model."
  (or (nlk:json-value entry :string "kind")
      (let ((inputs (nlk:json-value architecture :array "input_modalities"))
            (outputs (nlk:json-value architecture :array "output_modalities")))
        (if (and (modality-p inputs "text")
                 (not (modality-p outputs "image"))
                 (not (modality-p outputs "audio")))
            "chat"
            "other"))))

(defun entry-efforts (entry)
  "ENTRY's reasoning rungs as the catalog spells them, weakest first: the
feed's `none' is the disabled rung, which the catalog names `off'."
  (sort (remove-duplicates
         (loop for level across (or (nlk:json-value entry :array "reasoning_levels") #())
               for rung = (and (stringp level)
                               (if (string-equal level "none") "off" (string-downcase level)))
               when (and rung (nle::effort-rank rung)) collect rung)
         :test #'string-equal)
        #'< :key #'nle::effort-rank))

(defun entry-price (entry)
  "ENTRY's list price in the catalog's shape (input output cache-read
cache-write), or NIL when it publishes none: the feed's per-token prices,
in the catalog's per-million units. A row priced at nothing is free, not a
price."
  (let* ((pricing (nlk:json-value entry :object "pricing"))
         (input (priced (nlk:json-value pricing :number "prompt")))
         (output (priced (nlk:json-value pricing :number "completion"))))
    (and input output (or (plusp input) (plusp output))
         (list input output
               (priced (nlk:json-value pricing :number "input_cache_read"))
               (priced (nlk:json-value pricing :number "input_cache_write"))))))

(defun parse-entry (entry)
  "ENTRY, one catalog model, as the catalog keeps it (NLE::MAKE-CATALOG-MODEL's
fields), or NIL when it names no id."
  (let ((id (and (hash-table-p entry) (nlk:json-value entry :text "id"))))
    (when id
      (let* ((architecture (nlk:json-value entry :object "architecture"))
             (context (nlk:json-value entry :integer "context_length"))
             (efforts (entry-efforts entry)))
        (nle::make-catalog-model
         id
         (and context (plusp context) context)
         nil
         (or (nlk:json-value architecture :array "input_modalities") #("text"))
         (or (nlk:json-value architecture :array "output_modalities") #("text"))
         efforts
         (and efforts t)
         nil
         (entry-tools-p entry)
         (entry-price entry))))))

(defun listed-p (model)
  "Whether MODEL belongs in the picker: a chat model that takes tool calls
and has a window the original's floor admits."
  (let ((context (nle::catalog-model-context model)))
    (and (nle::catalog-model-tool-call-p model)
         (nle::catalog-model-turn-p model)
         (integerp context)
         (>= context +min-context+))))

(defun fetch-entries (base timeout)
  "The `data' array of {BASE}/models?exclude_deprecated=true, which the
gateway serves without a key: a vector of entries, or NIL on any failure.
Never signals."
  (handler-case
      (multiple-value-bind (body status)
          (nlk:http :get (format nil "~a/models?exclude_deprecated=true"
                                  (string-right-trim "/" base))
                    :headers (list (cons "accept" "application/json")
                                   (cons "user-agent" +user-agent+))
                    :timeout timeout)
        (when (and (stringp body) (integerp status) (< status 300))
          (let ((payload (nlk:decode-json body)))
            (and (hash-table-p payload)
                 (coerce (or (nlk:json-value payload :array "data") #()) 'list)))))
    (error () nil)))

(defun store-entries (models)
  "Keep MODELS, parsed, as the catalog now held."
  (bt2:with-lock-held (*feed-lock*)
    (setf *feed* (cons (get-universal-time)
                       (remove nil (mapcar #'parse-entry models)))
          *feed-failed-at* nil)))

(defun refresh-feed (&optional force)
  "Ask the gateway for its catalog and keep it; a read with no answer, or one
that fails, keeps the list already held. Answers the models now held, and
whether this call read them."
  (let ((fresh (bt2:with-lock-held (*feed-lock*)
                 (and (not force) (car *feed*)
                      (< (- (get-universal-time) (car *feed*)) +refresh-seconds+))))
        (backing-off (bt2:with-lock-held (*feed-lock*)
                       (and (not force) *feed-failed-at*
                            (< (- (get-universal-time) *feed-failed-at*)
                               +failed-retry-seconds+)))))
    (values (feed-models)
            (if (or fresh backing-off)
                nil
                (let ((entries (fetch-entries (setting :base-url) +seed-timeout+)))
                  (if (and entries (plusp (length entries)))
                      (progn (store-entries entries) t)
                      (progn (bt2:with-lock-held (*feed-lock*)
                               (setf *feed-failed-at* (get-universal-time)))
                             nil)))))))

(defun feed-models ()
  "The catalog's rows as last read, in its order, or NIL."
  (bt2:with-lock-held (*feed-lock*)
    (cdr *feed*)))

(defun feed-stamp ()
  "When the catalog now held was read, or NIL: what the merged catalog memo
compares, so a refresh is seen."
  (bt2:with-lock-held (*feed-lock*)
    (car *feed*)))

(defun forget-feed ()
  "Drop the catalog and the back-off, so a start against another base asks
again."
  (bt2:with-lock-held (*feed-lock*)
    (setf *feed* nil *feed-failed-at* nil)))

;;; --- the catalog row --------------------------------------------------------------

(defun fallback-row (model-id)
  "MODEL-ID as a catalog row with nothing published about it but that it is a
chat model that takes tools."
  (nle::make-catalog-model model-id nil nil #("text") #("text") nil nil nil t nil))

(defun catalog-models ()
  "The models the picker offers: what the gateway's catalog names and the
original's floor admits, else the featured list while it has named nothing."
  (let ((live (remove-if-not #'listed-p (feed-models))))
    (or live (mapcar #'fallback-row +fallback-models+))))

(defun catalog-row (&optional prior)
  "LLM API as a models.dev provider: the chat lane's package (the lane a
turn's own model resolves overrides it), this section's base, the key
variable, and its models over PRIOR's, the row models.dev itself published
when it did."
  (let ((models (make-hash-table :test #'equal)))
    (when (hash-table-p (nlk:json-value prior :object "models"))
      (maphash (lambda (id row) (setf (gethash id models) row))
               (nlk:json-value prior :object "models")))
    (loop for model in (catalog-models)
          do (setf (gethash (nle::catalog-model-name model) models) model))
    (nlk:json-object "name" "LLM API"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (setting :base-url)
                     "doc" +docs-url+
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))

(defun saved-key ()
  "The key /connect saved for this provider, or NIL."
  (let ((credential (ignore-errors (nle::resolve-provider-credential +provider+ :probe t))))
    (and credential
         (not (eq :public (nle:credential-source credential)))
         (nle:credential-key credential))))

(defun credential-key ()
  "The key a catalog or credits read carries: the saved one first, then
LLMAPI_API_KEY, else NIL."
  (or (saved-key) (env-key)))

;;; --- the balance -----------------------------------------------------------------

(defun money (value)
  "VALUE in dollars, for a line of text."
  (if (minusp value)
      (format nil "-$~,2f" (- value))
      (format nil "$~,2f" value)))

(defun fetch-credits (base api-key timeout)
  "The gateway's GET /v1/credits answer, or NIL on any failure. The key goes
to the gateway it belongs to and nowhere else. Never signals."
  (handler-case
      (multiple-value-bind (body status)
          (nlk:http :get (format nil "~a/credits" (string-right-trim "/" base))
                    :headers (list (cons "authorization" (format nil "Bearer ~a" api-key))
                                   (cons "accept" "application/json")
                                   (cons "user-agent" +user-agent+))
                    :timeout timeout)
        (when (and (stringp body) (integerp status) (< status 300))
          (let ((payload (nlk:decode-json body)))
            (and (hash-table-p payload) payload))))
    (error () nil)))

(defun usage-lines (payload)
  "PAYLOAD, one /v1/credits answer, as the lines /llmapi usage prints: the
balance, what billing mode it is under, the monthly cap and the
organization."
  (let* ((total (nlk:json-value payload :number "total_credits"))
         (credits (nlk:json-value payload :number "credits"))
         (bonus (nlk:json-value payload :number "credits_bonus"))
         (mode (string-downcase (or (nlk:json-value payload :string "mode") "")))
         (cap (nlk:json-value payload :object "monthly_cap"))
         (organization (nlk:json-value payload :object "organization"))
         (lines '()))
    (cond (total (push (format nil "Credits balance: ~a~@[ (incl. ~a bonus)~]"
                               (money total)
                               (and bonus (plusp bonus) (money bonus)))
                       lines))
          (credits (push (format nil "Credits balance: ~a" (money credits)) lines)))
    (push (case (intern (string-upcase mode) :keyword)
            (:|PAYG| "Billing: pay as you go, invoiced after use")
            (:|BYOK| "Billing: your own provider keys (BYOK)")
            (:|SUBSCRIPTION| "Billing: subscription plan")
            (t (format nil "Billing~@[: ~a~]" (and (plusp (length mode)) mode))))
          lines)
    (let ((limit (nlk:json-value cap :number "limit"))
          (used (nlk:json-value cap :number "used")))
      (when (and limit (plusp limit) used)
        (push (format nil "Monthly cap: ~a of ~a used" (money used) (money limit)) lines)))
    (let ((name (nlk:json-value organization :string "name")))
      (when name (push (format nil "Organization: ~a" name) lines)))
    (nreverse lines)))
