;;;; provider.lisp --- what OpenPaths is: its address, its key, its models,
;;;; and the reasoning_effort it takes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of lee101's hermes-openpaths (see NOTICE): its default
;;;; address, its key variable, the task-tier routes and vendor models it
;;;; falls back to, and the reasoning_effort mapping its provider profile
;;;; works out.
;;;;
;;;; OpenPaths speaks the OpenAI chat wire at https://openpaths.io/v1 with a
;;;; bearer key. Two things set it apart from a plain OpenAI-compatible
;;;; endpoint, and both are why this folder exists:
;;;;
;;;;   * its reasoning_effort vocabulary is none/low/medium/high/auto, and
;;;;     `auto' is a value the vendor lanes do not take: a pick the route
;;;;     does not know is lowered to the nearest one it does, reasoning
;;;;     turned off goes out as `none', and an auto-think route with no pick
;;;;     at all goes out as `auto';
;;;;   * its roster is led by task-tier routes -- auto, auto-easy-task,
;;;;     auto-medium-task, auto-hard-task, auto-think -- which pick a vendor
;;;;     model per request; the rest are the vendor ids it fronts.
;;;;
;;;; models.dev publishes no row for OpenPaths, so without this folder the
;;;; provider is a name nobody can choose in /models or dial. This file is
;;;; what its catalog row and its model list are made of.

(in-package #:nodecode-openpaths)

(defparameter +base+ "https://openpaths.io/v1"
  "Where OpenPaths is served: the base the chat lane appends /chat/completions to.")

(defparameter +env+ '("OPENPATHS_API_KEY")
  "The environment variables an OpenPaths key is read from, in order.")

(defparameter +key-page+ "https://openpaths.io/"
  "Where a key is made.")

(defparameter +docs-url+ "https://openpaths.io/"
  "OpenPaths' own page, which the picker's row links to.")

(defparameter +fallback-models+
  '("auto-medium-task" "auto" "auto-easy-task" "auto-hard-task"
    "auto-think" "autothink"
    "gpt-5.5" "gpt-5.4" "gpt-5.4-mini" "gpt-5.4-nano"
    "claude-opus-4-6" "claude-sonnet-4-6" "claude-haiku-4-5-20251001"
    "gemini-3.1-pro-preview" "gemini-2.5-flash"
    "deepseek-v4-pro" "deepseek-v4-flash")
  "The picker's list while OpenPaths' /models has not been read: the
original's fallback models, its default `auto-medium-task' first.")

(defparameter +auto-think-routes+ '("auto-think" "auto-think-task" "autothink")
  "The routes that think by default: the original sends `auto' for one of
these when no effort is configured, and so does this folder.")

(defparameter +wire-levels+ '("none" "low" "medium" "high" "auto")
  "The reasoning_effort values OpenPaths takes, spelled as it spells them.")

(defparameter +catalog-efforts+ '("off" "low" "medium" "high")
  "The rungs /think offers for an OpenPaths route: the wire's `none' is
Nodecode's `off', and `auto' is what the cell sends for an auto-think route
with no pick, not a rung of its own.")

(defparameter +refresh-seconds+ 3600
  "How long the live /models list may stand before the worker asks again.")

(defparameter +seed-timeout+ 8
  "One /models read's ceiling, in seconds.")

;;; --- the reasoning_effort vocabulary --------------------------------------------

(defun effort-level (requested)
  "REQUESTED, a Nodecode rung, as the reasoning_effort OpenPaths takes; NIL
when the rung names none in its vocabulary. `off' is its `none', `minimal'
the `low' above it, and `xhigh' and `max' the `high' below them: a pick is
lowered to the nearest level it does take, never raised."
  (cond
    ((null requested) nil)
    ((string-equal requested "off") "none")
    ((find requested +wire-levels+ :test #'string-equal) (string-downcase requested))
    ((string-equal requested "minimal") "low")
    ((member requested '("xhigh" "max") :test #'string-equal) "high")))

(defun auto-think-p (model)
  "Whether MODEL is one of the routes that think by default."
  (and (stringp model)
       (find (string-trim " " model) +auto-think-routes+ :test #'string-equal)))

(defun reasoning-effort-for (model requested off-p)
  "The reasoning_effort to send for MODEL, or NIL to leave the field out.
REQUESTED is the level the lane built (NIL when it built none) and OFF-P
whether the round turned reasoning off. Off is sent as `none', an auto-think
route with no pick at all as `auto', and any other pick as itself or the
nearest level OpenPaths takes below it."
  (cond
    ((or off-p (and (stringp requested) (string-equal requested "off"))) "none")
    ((and (null requested) (auto-think-p model)) "auto")
    (t (effort-level requested))))

;;; --- OpenPaths' model list -----------------------------------------------------

(defvar *live* nil
  "(FETCHED-AT . IDS) of the last /models list OpenPaths answered.")

(defvar *live-lock* (bt2:make-lock :name "openpaths-models")
  "Held while the list is read or replaced, so a turn never reads half of
one.")

(defun parse-model-ids (items)
  "OpenPaths' /models `data' array as a list of ids; NIL when the payload
names nothing."
  (let ((ids '()))
    (loop for item across (if (vectorp items) items #())
          when (hash-table-p item)
            do (let ((id (nlk:json-value item :text "id")))
                 (when id (push id ids))))
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
  "The key the /models read carries: OPENPATHS_API_KEY, else the key
/connect saved, else NIL."
  (or (env-key)
      (let ((credential (ignore-errors (nle::resolve-provider-credential +provider+ :probe t))))
        (and credential
             (not (eq :public (nle:credential-source credential)))
             (nle:credential-key credential)))))

(defun refresh-live (&aux (key (credential-key)))
  "Ask OpenPaths for its model list and keep it; a read with no key, or one
that fails, keeps the list already held."
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

;;; --- the catalog row ------------------------------------------------------------

(defun catalog-model (model-id)
  "MODEL-ID as the catalog keeps a model (NLE::MAKE-CATALOG-MODEL's fields):
no window, no price -- the original published neither -- and the rungs every
OpenPaths route takes."
  (nle::make-catalog-model model-id nil nil #("text") #("text")
                            +catalog-efforts+ t nil t nil))

(defun catalog-models (&optional prior)
  "The models the picker offers: the live list, each id with whatever row
models.dev published for it, else models.dev's own OpenPaths rows, else the
fallback list."
  (let ((published (nlk:json-value prior :object "models"))
        (live (live-models)))
    (cond
      (live (loop for id in live
                  collect (or (and (hash-table-p published) (gethash id published))
                              (catalog-model id))))
      ((and (hash-table-p published) (plusp (hash-table-count published)))
       (loop for id being the hash-keys of published using (hash-value row)
             collect row))
      (t (mapcar #'catalog-model +fallback-models+)))))

(defun catalog-row (&optional prior)
  "OpenPaths as a models.dev provider: the chat lane's package, this
section's base, the key variable, and its models over PRIOR's (the row
models.dev itself published, when it did)."
  (let ((models (make-hash-table :test 'equal)))
    (loop for model in (catalog-models prior)
          do (setf (gethash (nle::catalog-model-name model) models) model))
    (nlk:json-object "name" "OpenPaths"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (setting :base-url)
                     "doc" +docs-url+
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))
