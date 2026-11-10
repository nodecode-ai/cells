;;;; provider.lisp --- what Modular Cloud is: its address, its key, its models,
;;;; and the nested reasoning object each of them takes.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A rewrite of tommyjtl's hermes-modular-provider (see NOTICE): its default
;;;; address, its key variable, its model table (context window, tools, vision
;;;; and the reasoning controls measured against the live API) and the clamp
;;;; that never raises a level.
;;;;
;;;; Modular Cloud speaks the OpenAI chat wire at https://api.modular.com/v1
;;;; with a bearer key made from Modular's console. Two things set it apart
;;;; from a plain OpenAI-compatible endpoint, and both are why this folder
;;;; exists:
;;;;
;;;;   * the reasoning control is not OpenAI's top-level reasoning_effort but
;;;;     a nested `reasoning' object: {"effort": LEVEL} to think at a depth,
;;;;     {"enabled": false} to stop thinking, {"enabled": true} to just think;
;;;;   * each model takes its own set of levels -- deepseek-v4.1-flash takes
;;;;     low, high and max, MiniMax-M3.1-Flash takes low through max, and
;;;;     minimax-m3 and gemma-4-31b-it take a toggle and no level at all -- so
;;;;     a pick is lowered to the nearest level the model takes, never raised.
;;;;
;;;; models.dev publishes no row at Modular's own address, so without this
;;;; folder the provider is a catalog row nobody can dial. This file is what
;;;; the row and its models are made of.

(in-package #:nodecode-modular)

(defparameter +base+ "https://api.modular.com/v1"
  "Where Modular Cloud is served: the base the chat lane appends
/chat/completions to.")

(defparameter +env+ '("MODULAR_API_KEY")
  "The environment variables a Modular key is read from, in order.")

(defparameter +docs-url+ "https://console.modular.com/api/v1/docs/models"
  "Modular's own model catalog, where the rows' context windows and prices
are published.")

;;; --- the model table ---------------------------------------------------------------

(defparameter +models+
  '(("MiniMaxAI/MiniMax-M3.1-Flash" 1048576 ("low" "medium" "high" "xhigh" "max") nil nil)
    ("deepseek/deepseek-v4.1-flash-1017" 1048576 ("low" "high" "max") t "toggle")
    ("minimax/minimax-m3" 1048576 () t "toggle")
    ("google/gemma-4-31b-it" 262144 () t "toggle"))
  "The models this cell offers, as the original's table reads: (ID CONTEXT
EFFORTS TOGGLE OFF). EFFORTS are the levels the model takes on the wire,
weakest first; TOGGLE whether it takes an `enabled' switch; OFF what turning
thinking off sends -- `toggle' for {\"enabled\": false}, NIL where nothing on
this wire stops it. Every row takes tools, images and reasoning.")

(defparameter +effort-ladder+
  '("none" "minimal" "low" "medium" "high" "xhigh" "max")
  "The rungs the original ranked a pick against, weakest first: the wire's own
spelling, in which Nodecode's `off' is `none'.")

(defun model-row (model)
  "The row of MODEL, or NIL: a model this cell does not describe keeps
whatever the chat lane makes of it."
  (and (stringp model) (assoc model +models+ :test #'string-equal)))

(defun clamp-effort (effort supported)
  "EFFORT lowered to the nearest level in SUPPORTED at or below it, never
raised; the weakest supported level when none is weaker, or when EFFORT names
no rung at all."
  (let ((rank (position effort +effort-ladder+ :test #'string-equal)))
    (cond
      ((member effort supported :test #'string-equal) effort)
      ((null rank) (first supported))
      (t (or (car (last (remove-if (lambda (level)
                                     (>= (position level +effort-ladder+ :test #'string-equal)
                                         rank))
                                   supported)))
             (first supported))))))

(defun reasoning-object (efforts toggle off effort)
  "The nested `reasoning' object one request carries for a model, or NIL to
send none and let the model's own default stand. EFFORT is the round's pick:
a rung, `off' when thinking was turned off, or NIL when nothing was picked;
EFFORTS, TOGGLE and OFF are the model's own row."
  (cond
    ;; thinking turned off: the model's own way of stopping, and where nothing
    ;; stops it, its lightest level instead
    ((and effort (string-equal effort "off"))
     (cond ((equal off "toggle") (nlk:json-object "enabled" :false))
           ((equal off "effort") (nlk:json-object "effort" "none"))
           (efforts (nlk:json-object "effort" (first efforts)))
           (t nil)))
    ;; a level picked: the nearest one the model takes
    ((and effort efforts)
     (nlk:json-object "effort" (clamp-effort effort efforts)))
    ;; no pick: a model with a switch is told to think, as the original did,
    ;; and one without is sent nothing
    (toggle (nlk:json-object "enabled" t))
    (t nil)))

;;; --- the catalog row -----------------------------------------------------------------

(defun catalog-efforts (levels)
  "LEVELS as a catalog ladder, weakest first: Nodecode's own rungs, the
wire's `none' spelled the way the catalog spells the disabled tier, `off'."
  (sort (remove-duplicates
         (loop for level in levels
               for rung = (if (string-equal level "none") "off" (string-downcase level))
               when (nle::effort-rank rung) collect rung)
         :test #'string-equal)
        #'< :key #'nle::effort-rank))

(defun catalog-model (row)
  "ROW as the catalog keeps a model (NLE::MAKE-CATALOG-MODEL's fields): its
context window, the modalities it takes, and the ladder it declares."
  (destructuring-bind (id context efforts toggle off) row
    (declare (ignore toggle off))
    (nle::make-catalog-model id context nil #("text" "image") #("text")
                              (catalog-efforts efforts)
                              t nil t nil)))

(defun catalog-row ()
  "Modular Cloud as a models.dev provider: the chat lane's package, this
section's base, the key variable, and the models the original describes.
Nothing outside that table is offered, as the original's fetch_models narrowed
the live list to it."
  (let ((models (make-hash-table :test 'equal)))
    (loop for row in +models+
          do (setf (gethash (first row) models) (catalog-model row)))
    (nlk:json-object "name" "Modular"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (setting :base-url)
                     "doc" +docs-url+
                     "env" (coerce +env+ 'vector)
                     "models" models)))
