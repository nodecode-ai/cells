;;;; provider.lisp --- what Ace Data Cloud is: its address, its key, its models.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original (see NOTICE) is a provider profile and nothing else: one base
;;;; URL, one key variable, and the two models it verified for this wire. It
;;;; registers that profile with Hermes' provider registry; here the same facts
;;;; are a catalog row (CELL.LISP), which is what every reader in Nodecode
;;;; -- the picker, /models, /connect's key check, lane resolution -- reads a
;;;; provider from. Nothing else about the original runs anywhere: there are no
;;;; tools, no hooks and no telemetry to carry.
;;;;
;;;; Ace Data Cloud serves the OpenAI chat wire at
;;;; https://api.acedata.cloud/openai with a bearer key from its console. Its
;;;; account-wide /models answer also carries image-generation and
;;;; Messages-only ids, which the original filters out of its picker; the same
;;;; filter is LISTING-ROWS in CELL.LISP.

(in-package #:nodecode-acedatacloud)

(defparameter +base+ "https://api.acedata.cloud/openai"
  "Where Ace Data Cloud is served: the base the chat lane appends
/chat/completions to.")

(defparameter +env+ '("ACEDATACLOUD_API_KEY")
  "The environment variable an Ace Data Cloud key is read from, and the only
one this cell reads.")

(defparameter +key-page+ "https://platform.acedata.cloud/console/applications"
  "Where a key is made: the console's Applications page, a General
application's API Key.")

(defparameter +models+
  '(("gpt-4.1-mini" . "GPT-4.1 mini")
    ("gpt-4.1" . "GPT-4.1"))
  "The two models the original verified for this transport, one (id . display
name) each, in the order the picker offers them. They are the plugin's
fallback list and the only ids it lets through, so they are the row here.")

(defparameter +context+ 1047576
  "The context window both verified models publish.")

(defparameter +output+ 32768
  "The output ceiling both verified models publish.")

(defparameter +released+ "2025-04-14"
  "The date both were published: the GPT-4.1 family's.")

(defun model-row (id)
  "ID as the catalog keeps a model, or NIL when it is not one of the verified
two."
  (let ((display (cdr (assoc id +models+ :test #'equal))))
    (and display
         (nle::make-catalog-model display +context+ +output+
                                   #("text" "image") #("text")
                                   ;; no reasoning ladder: the original
                                   ;; declares supports_reasoning false
                                   nil nil +released+
                                   t
                                   ;; no price: Ace bills from its own
                                   ;; balance, and the original says Hermes' own
                                   ;; estimate may be wrong. None is invented.
                                   nil))))

(defun bundled-rows ()
  "The verified models as a listing answers them, for a catalog that answers
nothing: id, display name and the window the picker shows."
  (loop for (id . display) in +models+
        collect (list :id id :display display :context-window +context+)))

(defun catalog-row ()
  "Ace Data Cloud as a models.dev provider: the chat lane's package, this
section's base, the key variable, and the two verified models."
  ;; models.dev publishes no row for this vendor, and if it ever does the
  ;; account-wide one carries image and Messages-only ids the original filters
  ;; out -- so the row is built from the verified list, not merged over a
  ;; published one.
  (let ((models (make-hash-table :test 'equal)))
    (dolist (entry +models+)
      (setf (gethash (car entry) models) (model-row (car entry))))
    (nlk:json-object "name" "Ace Data Cloud"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (setting :base-url)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))

(defun listing-key ()
  "The key a listing asks with: the environment's, else the one /connect
saved, else NIL."
  (or (env-key)
      (ignore-errors (nle:credential-key (nle::resolve-provider-credential +provider+)))))
