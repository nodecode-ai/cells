;;;; provider.lisp --- what ZeroSignal is: its address, its key, its models,
;;;; the reasoning rungs its network declares.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of TxnLab's hermes-zerosignal-provider (see NOTICE): its
;;;; default address, its placeholder key, its fallback model list, and the
;;;; per-model reasoning vocabulary read from the proxy's own /models catalog.
;;;;
;;;; ZeroSignal is a pay-per-request, privacy-preserving inference network
;;;; reached through `zs-proxy', a proxy on the same machine that speaks the
;;;; OpenAI chat wire at http://127.0.0.1:9376/v1. The proxy pays from a
;;;; funded wallet, so the Authorization header it receives is ignored: any
;;;; non-empty key works, and the placeholder below is the one this folder
;;;; sends when the operator has set none.
;;;;
;;;; The proxy's GET /models names, per model, whether it reasons at all
;;;; (`reasoning.supported') and, when the serving operator declared them,
;;;; the levels it accepts (`reasoning.allowed_efforts'). A node that
;;;; declares low/high/max answers 400 to Hermes' default `medium', so the
;;;; request's reasoning_effort is clamped onto the declared levels before
;;;; it goes on the wire -- never upward. The map is read once at start on
;;;; a worker, cached, and refreshed every +REFRESH-SECONDS+; a failed
;;;; refresh keeps serving the last map, and a model no map names passes
;;;; through untouched.

(in-package #:nodecode-zerosignal)

(defparameter +base+ "http://127.0.0.1:9376/v1"
  "Where the local proxy listens: the base the chat lane appends
/chat/completions to.")

(defparameter +env+ '("ZEROSIGNAL_API_KEY")
  "The environment variables a ZeroSignal key is read from, in order.")

(defparameter +placeholder-key+ "zerosignal-local"
  "The key sent when none is configured: the proxy ignores it, and Hermes
needed some non-empty value to light the provider, which is what this
rewrites. The wallet is the credential.")

(defparameter +docs-url+ "https://docs.zerosignal.ai/using-the-proxy/guides/hermes"
  "Where the proxy is installed from and funded.")

(defparameter +refresh-seconds+ 300
  "How long the proxy's catalog may stand before the worker asks again.")

(defparameter +seed-timeout+ 5
  "One catalog read's ceiling, in seconds: loopback answers in milliseconds,
and a proxy that is down must not hold a start.")

(defparameter +fallback-models+
  '("glm-5.3" "glm-5.3-flash" "glm-5.2" "kimi-k3" "grok-4.6" "grok-4.5"
    "grok-4.3" "google/gemini-3.7-flash" "google/gemini-3.8-flash"
    "openai/gpt-5.6-luna" "openai/gpt-5.6-terra" "openai/gpt-6-astra")
  "The picker's list while the proxy answers nothing: the original's
curated fallback, all tool-calling chat models.")

;;; --- reasoning ranks -----------------------------------------------------------

(defun rung-rank (level)
  "LEVEL's rank below Nodecode's ladder, or NIL when it names no level.
The wire's `none' -- Hermes' disabled spelling -- sits under `off'."
  (cond ((null level) nil)
        ((string-equal level "none") -1)
        (t (nle::effort-rank level))))

(defun declared-levels (levels)
  "LEVELS, the proxy's allowed_efforts, filtered to the spellings this
organism knows and sorted weakest first: Nodecode's own ladder plus the
wire's `none'."
  (sort (remove-duplicates
         (loop for level in levels
               when (and (stringp level) (rung-rank level))
                 collect (string-downcase level))
         :test #'string-equal)
        #'< :key #'rung-rank))

(defun catalog-efforts (levels)
  "LEVELS as a catalog ladder: Nodecode's own rungs, `none' spelled the way
the catalog spells the disabled tier, `off'."
  (sort (remove-duplicates
         (loop for level in levels
               for rung = (if (string-equal level "none") "off" (string-downcase level))
               when (nle::effort-rank rung) collect rung)
         :test #'string-equal)
        #'< :key #'nle::effort-rank))

;;; --- the proxy's catalog ---------------------------------------------------------

(defvar *live* nil
  "(FETCHED-AT . MAP) of the last catalog the proxy answered, MAP an alist
of (MODEL . LEVELS); LEVELS is the sorted list it declares, or :NONE when it
declares `reasoning.supported: false'. A model absent from MAP is unknown.")

(defvar *live-lock* (bt2:make-lock :name "zerosignal-catalog")
  "Held while the map is read or replaced, so a turn never reads a half map.")

(defun parse-live (items)
  "The proxy's /models `data' array as a MAP: (MODEL . LEVELS), LEVELS the
declared allowed_efforts or :NONE. NIL when the payload names nothing."
  (let ((map '()))
    (loop for item across (if (vectorp items) items #())
          when (hash-table-p item)
            do (let ((id (nlk:json-value item :text "id"))
                     (reasoning (nlk:json-value item :object "reasoning")))
                 (when id
                   (push (cons id
                               (cond ((null reasoning) nil)
                                     ((eq :false (gethash "supported" reasoning)) :none)
                                     (t (declared-levels
                                         (coerce (or (nlk:json-value reasoning :array
                                                                      "allowed_efforts")
                                                     #())
                                                 'list)))))
                         map))))
    map))

(defun fetch-live-items (base api-key timeout)
  "The `data' array of {BASE}/models; NIL on any failure, never signals."
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

(defun refresh-live ()
  "Ask the proxy for its catalog and keep it. A failed read keeps the map
already held, so a down proxy costs nothing and serves what it last said."
  (let* ((base (setting :base-url))
         (items (fetch-live-items base +placeholder-key+ +seed-timeout+))
         (map (and items (parse-live items))))
    (when map
      (bt2:with-lock-held (*live-lock*)
        (setf *live* (cons (get-universal-time) map))))
    (car-safe *live*)))

(defun car-safe (value)
  "VALUE's car when it is a cons, else NIL."
  (and (consp value) (car value)))

(defun forget-live ()
  "Drop the map, so a start with another base asks again."
  (bt2:with-lock-held (*live-lock*)
    (setf *live* nil)))

(defun live-map ()
  "The model map the last successful read kept, or NIL."
  (bt2:with-lock-held (*live-lock*)
    (cdr *live*)))

(defun live-levels (model)
  "MODEL's declared levels: a list, :NONE, or NIL when the map is cold or
does not name it."
  (cdr (assoc model (live-map) :test #'equal)))

;;; --- the catalog row -------------------------------------------------------------

(defun catalog-model (model levels)
  "MODEL as the catalog keeps one (NLE::MAKE-CATALOG-MODEL's fields), with
LEVELS the proxy's declaration for it, or NIL when unknown."
  (nle::make-catalog-model model nil nil #("text") #("text")
                            (and levels (listp levels) (catalog-efforts levels))
                            (if levels (not (eq levels :none)) t)
                            nil t nil))

(defun catalog-models ()
  "The models the picker offers: every id the proxy last named with the rungs
it declares, or the fallback list while the map is cold."
  (let ((map (live-map)))
    (if map
        (loop for (id . levels) in map collect (catalog-model id levels))
        (mapcar (lambda (id) (catalog-model id nil)) +fallback-models+))))

(defun catalog-row (&optional prior)
  "ZeroSignal as a models.dev provider: the chat lane's package, this
section's base, the key variables, and its models over PRIOR's (the row
models.dev itself published, when it did)."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for model in (catalog-models)
          do (setf (gethash (nle::catalog-model-name model) models) model))
    (nlk:json-object "name" "ZeroSignal"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (setting :base-url)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))
