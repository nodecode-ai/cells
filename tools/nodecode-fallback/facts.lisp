;;;; facts.lisp --- what a model id says about itself, and the catalog behind it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The heuristics run on facts about a model: parameter count, context window,
;;;; whether it can call tools, its price tier, its release date, its vendor.
;;;; Three readers answer them, in this order, each only when the one before it
;;;; said nothing:
;;;;
;;;;   1. the id, which publishes a lot -- `llama-3.3-70b' names 70B,
;;;;      `qwen3-coder:128k' a window, `:free' a price, `4o-mini' a tier;
;;;;   2. Nodecode's own models.dev catalog, when it holds a row for the id
;;;;      (read through the engine's reader, guarded: a release that renames it
;;;;      leaves this cell answering from 1 and 3 instead of failing);
;;;;   3. a small table of families whose ids publish no number -- claude at
;;;;      200k, gemini at 1M -- honest about being a table, and wrong alone.
;;;;
;;;; A number nobody published stays unknown rather than becoming zero: a
;;;; criterion that needs one leaves the model out, and the render says so.

(in-package #:nodecode-fallback)

;;; --- what an id says ---------------------------------------------------------

(defparameter +tiers+
  '(("nano" . 4.0) ("mini" . 8.0) ("small" . 12.0) ("lite" . 7.0)
    ("flash" . 20.0) ("haiku" . 12.0) ("sonnet" . 70.0) ("opus" . 150.0)
    ("pro" . 70.0) ("large" . 70.0) ("medium" . 30.0))
  "A tier word in an id, in billion parameters, for the families that publish
none: the coarsest reading, never a substitute for a number the id carries.")

(defparameter +flash-words+ '("flash" "haiku" "mini" "nano" "lite" "spark" "turbo" "speed")
  "Words that name a fast tier: the original's list, less the two that a
provider uses for a full-size model.")

(defparameter +free-markers+ '(":free" "/free" "-free" "free/")
  "What an id carries when the model rides a free tier.")

(defparameter +vendor-prefixes+
  '(("claude-" . "anthropic") ("gpt-" . "openai") ("o1-" . "openai")
    ("o3" . "openai") ("o4-" . "openai") ("gemini-" . "google")
    ("deepseek" . "deepseek") ("glm-" . "z-ai") ("kimi-" . "moonshotai")
    ("minimax" . "minimax") ("grok-" . "xai") ("qwen" . "qwen")
    ("llama" . "meta-llama") ("mistral" . "mistralai")
    ("mixtral" . "mistralai") ("nemotron" . "nvidia") ("mimo-" . "xiaomi"))
  "The vendor an id's own prefix names, when its provider is not the vendor.")

(defparameter +family-context+
  '(("claude" . 200000) ("gemini" . 1048576) ("gpt-5" . 400000)
    ("gpt-4.1" . 1047576) ("gpt-4o" . 128000) ("gpt-4" . 128000)
    ("o3" . 200000) ("o4" . 200000) ("deepseek" . 128000)
    ("llama" . 128000) ("qwen" . 131072) ("mixtral" . 32768)
    ("mistral" . 131072) ("grok" . 131072) ("glm-4" . 128000)
    ("kimi" . 131072) ("minimax" . 204800) ("nemotron" . 131072)
    ("phi-" . 131072) ("command" . 128000))
  "A window in tokens for the families whose ids publish none. The coarsest
reading there is, and the one a catalog row beats.")

(defun say-number (text)
  "TEXT as a number, or NIL: `70', `30.5', `1e3'."
  (when (plusp (length text))
    (let ((n (ignore-errors (read-from-string text))))
      (and (realp n) n))))

(defun param-size (model)
  "Parameter count in billions MODEL's id publishes, or NIL: `8x22b' is 176,
`a12b' is 12 (active), `70b' is 70, and a tier word a small table."
  (let ((id (string-downcase model)))
    (or (multiple-value-bind (match groups)
            (ppcre:scan-to-strings "([0-9]+)x([0-9]+(?:\\.[0-9]+)?)b" id)
          (when match
            (* (parse-integer (aref groups 0)) (or (say-number (aref groups 1)) 0))))
        (multiple-value-bind (match groups)
            (ppcre:scan-to-strings "(?:^|[^a-z0-9])a([0-9]+(?:\\.[0-9]+)?)b" id)
          (when match (say-number (aref groups 0))))
        (multiple-value-bind (match groups)
            (ppcre:scan-to-strings "([0-9]+(?:\\.[0-9]+)?)b(?:$|[^a-z0-9])" id)
          (when match (say-number (aref groups 0))))
        (tier-size id))))

(defun word-in-p (word id)
  "Whether ID carries WORD as a word of its own: `mini' is in `gpt-4o-mini' and
not in `gemini-2.5-pro'."
  (and (ppcre:scan (format nil "(?:^|[^a-z0-9])~a(?:$|[^a-z0-9])" word) id) t))

(defun tier-size (id)
  "The parameter count ID's tier word names, or NIL -- the last reading, for a
family that publishes no number."
  (loop for (word . size) in +tiers+
        when (word-in-p word id) return size))

(defparameter +k-power-of-two+
  '((4 . 4096) (8 . 8192) (16 . 16384) (32 . 32768) (64 . 65536)
    (128 . 131072) (256 . 262144) (512 . 524288))
  "A `128k' in a context window is 131072, as the catalogs write it; 100k is
100000. The original's table, kept because a window is compared, not printed.")

(defun context-in-id (model)
  "The context window MODEL's id publishes -- `128k', `1m' -- or NIL."
  (let ((id (string-downcase model)))
    (or (multiple-value-bind (match groups)
            (ppcre:scan-to-strings "([0-9]+(?:\\.[0-9]+)?)m(?:$|[^a-z0-9])" id)
          (when match
            (let ((value (say-number (aref groups 0))))
              (and value (<= 0.1 value 32.0) (round (* value 1000000))))))
        (multiple-value-bind (match groups)
            (ppcre:scan-to-strings "([0-9]+)k(?:$|[^a-z0-9])" id)
          (when match
            (let ((value (parse-integer (aref groups 0))))
              (or (cdr (assoc value +k-power-of-two+))
                  (and (<= 4 value 2048) (* value 1000)))))))))

(defun free-model-p (model)
  "Whether MODEL's id says it rides a free tier."
  (let ((id (string-downcase model)))
    (and (or (some (lambda (marker) (search marker id)) +free-markers+)
             (uiop:string-prefix-p "free-" id)
             (search "stealth/union-alpha" id))
         t)))

(defun fast-model-p (model)
  "Whether MODEL's id names a fast tier as a word of its own."
  (let ((id (string-downcase model)))
    (and (some (lambda (word) (word-in-p word id)) +flash-words+) t)))

(defun vendor-of (model)
  "The vendor MODEL's id names: its `vendor/' prefix, else a family prefix."
  (let ((id (string-downcase model)))
    (if (find #\/ id)
        (subseq id 0 (position #\/ id))
        (or (loop for (prefix . vendor) in +vendor-prefixes+
                  when (uiop:string-prefix-p prefix id) return vendor)
            ""))))

(defun release-in-id (model)
  "MODEL's release date as YYYYMMDD, when its id carries one, else 0."
  (multiple-value-bind (match groups)
      (ppcre:scan-to-strings "(20[0-9][0-9])-?([01][0-9])-?([0-3][0-9])(?:$|[^0-9])"
                              model)
    (declare (ignore match))
    (if groups
        (parse-integer (format nil "~a~a~a" (aref groups 0) (aref groups 1) (aref groups 2)))
        0)))

(defun version-in-id (model)
  "MODEL's version as MAJOR*1000+MINOR, for breaking a tie: `gpt-5.4' is 5004."
  (multiple-value-bind (match groups)
      (ppcre:scan-to-strings "([0-9]+)\\.([0-9]+)" model)
    (declare (ignore match))
    (if groups
        (+ (* 1000 (parse-integer (aref groups 0))) (parse-integer (aref groups 1)))
        0)))

;;; --- what a catalog says ------------------------------------------------------

(defun catalog-facts (provider model)
  "What Nodecode's own models.dev catalog holds for MODEL under PROVIDER, as a
plist of :CONTEXT and :TOOLS (`:no' when its row says no tool calls, `:unknown'
when it has no row), or NIL. Read through the engine's reader, guarded: a release
that renames it costs the cell this source, never its answer."
  (when (fboundp 'nlk::models-catalog-entry)
    (let ((entry (ignore-errors
                  (funcall (symbol-function 'nlk::models-catalog-entry) model provider))))
      (when entry
        (list :context (ignore-errors (nlk::catalog-model-context entry))
              :tools (if (ignore-errors (nlk::catalog-model-tool-call-p entry)) t :no))))))

(defvar *catalog* #'catalog-facts
  "How the cell reads a catalog row: (PROVIDER MODEL) => a plist of facts or NIL.
A test binds a stub, or NIL to read nothing but the id and the table.")

;;; --- one candidate -----------------------------------------------------------

(defun facts-of (provider model)
  "Everything known about MODEL under PROVIDER, as a plist. The id answers
what it publishes, the catalog what it holds, the family table the rest."
  (let* ((catalog (and *catalog* (funcall *catalog* provider model)))
         (context (or (context-in-id model) (getf catalog :context)
                      (loop for (family . tokens) in +family-context+
                            when (search family (string-downcase model)) return tokens))))
    (list :params (param-size model)
          :context context
          :tools (or (getf catalog :tools) :unknown)
          :free (free-model-p model)
          :flash (fast-model-p model)
          :vendor (vendor-of model)
          :date (release-in-id model)
          :version (version-in-id model))))

(defun candidate (provider model)
  "One candidate, a plist: the facts of MODEL under PROVIDER, and where it came from."
  (list* :provider provider :model model (facts-of provider model)))

(defun candidate-key (cand)
  "CAND as provider/model, the way the criteria and a reader name it."
  (format nil "~a/~a" (getf cand :provider) (getf cand :model)))

;;; --- where candidates come from ----------------------------------------------

(defun live-candidates ()
  "Every (PROVIDER . MODEL) Nodecode can name a model for, as conses: the
catalog the shell's /models picker shows, provider by provider. A provider whose
listing fails costs its models, never the answer."
  (loop for pair in (ignore-errors (nle:models))
        for provider = (car pair)
        append (let ((models (handler-case (nle:models (string provider)) (error () nil))))
                 (loop for model in models collect (cons (string provider) (string model))))))

(defvar *candidates* #'live-candidates
  "How the cell finds candidates: () => a list of (PROVIDER . MODEL) conses. A
test binds a stub, so every verb answers offline.")

(defun candidates (&key (source *candidates*))
  "The candidates SOURCE names, each with its facts, deduplicated by
provider/model in a stable order."
  (let ((seen (make-hash-table :test #'equal)) (out '()))
    (loop for (provider . model) in (funcall source)
          for key = (format nil "~a/~a" provider model)
          unless (gethash key seen)
            do (setf (gethash key seen) t)
               (push (candidate provider model) out))
    (nreverse out)))
