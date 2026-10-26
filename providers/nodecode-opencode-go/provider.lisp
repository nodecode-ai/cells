;;;; provider.lisp --- what OpenCode Go is: its address, its key, its wires, its session header.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Follows oh-my-pi (see NOTICE): packages/ai/src/providers/
;;;; inference-headers.ts (the session header), catalog/src/provider-models/
;;;; openai-compat.ts (createOpenCodeApiResolution: which wire a model rides)
;;;; and catalog/src/compat/rules/runtime/behavior.kdl (the opencode-go
;;;; api-routes pins).
;;;;
;;;; models.dev already carries OpenCode Go: its models, its key variable and
;;;; the chat wire's package. Two things keep the core from using it as it is:
;;;;
;;;; - Go refuses every request without an x-opencode-session header (HTTP
;;;;   400 MissingSessionID), before it looks at the key. Go asks for a stable
;;;;   id per conversation, which it routes and caches by.
;;;; - Go serves four wires, and models.dev says which one a model speaks per
;;;;   model (models[].provider.npm), a field the core's catalog does not
;;;;   keep. All four answer under one base, https://opencode.ai/inference/go/v1:
;;;;   /chat/completions, /responses, /messages and
;;;;   /models/<id>:streamGenerateContent.

(in-package #:nodecode-opencode-go)

(defparameter +base+ "https://opencode.ai/inference/go/v1"
  "Where OpenCode Go is served: the one base every lane appends its path to.")

(defparameter +env+ '("OPENCODE_API_KEY")
  "The environment variables a Go key is read from, in order: models.dev's
and omp's name.")

(defparameter +session-header+ "x-opencode-session"
  "The header Go refuses a request without: the conversation's id.")

;;; --- which wire a model rides ----------------------------------------------

(defparameter +routes+
  '(("google" () ("gemini-4-argon-contributor"))
    ("anthropic" () ("union-alpha"))
    ("openai-responses" ("muse-spark-") ("deepseek-v4-flash"))
    ("openai-completions" () ("minimax-m2.7" "minimax-m3" "minimax-m3-free"
                              "qwen3.5-plus" "qwen3.6-plus")))
  "omp's opencode-go api-routes pins, (LANE PREFIXES EXACT-IDS) each: models
models.dev's npm hint misroutes, checked against the gateway by omp.")

(defparameter +npm-lanes+
  '(("@ai-sdk/openai" . "openai-responses")
    ("@ai-sdk/anthropic" . "anthropic")
    ("@ai-sdk/google" . "google"))
  "models.dev's per-model package to the lane that speaks it; any other
model rides the chat lane.")

(defun hint-member-p (path &aux (keys (reverse path)) (depth (length keys)))
  "Whether the models.dev member at PATH, innermost key first, is on the way
to an opencode-go model's provider.npm."
  (and (<= depth 5)
       (equal (first keys) +provider+)
       (or (< depth 2) (equal (second keys) "models"))
       (or (< depth 4) (equal (fourth keys) "provider"))
       (or (< depth 5) (equal (fifth keys) "npm"))))

(defun read-hints (path &aux (table (make-hash-table :test 'equal)))
  "Model id -> models.dev's package for that model, read from the api.json
at PATH; an empty table when it does not read."
  (let ((root (ignore-errors
               (nlk:decode-json (alexandria:read-file-into-byte-vector path)
                                :keep #'hint-member-p))))
    (alexandria:when-let (models (nlk:json-value root :object +provider+ "models"))
      (maphash (lambda (id model)
                 (alexandria:when-let (npm (nlk:json-value model :string "provider" "npm"))
                   (setf (gethash id table) npm)))
               models))
    table))

(defvar *hints* (cons nil nil)
  "(STAMP . TABLE): the per-model packages last read, and the catalog file's
path and write date they were read at.")

(defun hints (&aux (path nle::*models-catalog-path*)
                   (date (and path (ignore-errors (file-write-date path))))
                   (stamp (and date (cons (namestring path) date)))
                   (memo *hints*))
  "The per-model packages of the catalog file the core reads, read again only
when that file changes."
  (cond ((null stamp) (make-hash-table :test 'equal))
        ((equal stamp (car memo)) (cdr memo))
        (t (let ((table (read-hints path)))
             (setf *hints* (cons stamp table))
             table))))

(defun pinned-lane (model)
  "The lane +ROUTES+ pins MODEL to, or NIL."
  (loop for (lane prefixes exact) in +routes+
        when (or (member model exact :test #'equal)
                 (some (lambda (prefix) (alexandria:starts-with-subseq prefix model)) prefixes))
          return lane))

(defun model-lane (model)
  "The lane MODEL rides on Go: its pin, else models.dev's package for it,
else the chat lane."
  (or (pinned-lane model)
      (cdr (assoc (gethash model (hints)) +npm-lanes+ :test #'equal))
      "openai-completions"))

;;; --- the catalog row and the key -------------------------------------------

(defun catalog-row (&optional prior)
  "OpenCode Go as a models.dev provider: the row models.dev published (PRIOR)
with this section's base, or a bare row with no models when it published
none, which the live listing then fills."
  (let ((row (make-hash-table :test 'equal)))
    (when (hash-table-p prior)
      (maphash (lambda (key value) (setf (gethash key row) value)) prior))
    (flet ((default (key value)
             (unless (nth-value 1 (gethash key row))
               (setf (gethash key row) value))))
      (default "name" "OpenCode Go")
      (default "npm" "@ai-sdk/openai-compatible")
      (default "env" (coerce +env+ 'vector))
      (default "models" (make-hash-table :test 'equal)))
    (setf (gethash "api" row) (setting :base-url))
    row))

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))

;;; --- the session header ----------------------------------------------------

(defvar *process-session* nil
  "The id a request outside any turn is sent under, made once per process.")

(defun process-session ()
  "This process's own session id, for a request no turn makes."
  (or *process-session*
      (setf *process-session*
            (format nil "nodecode-~(~32,'0x~)" (random (ash 1 128) (make-random-state t))))))

(defun session-value (&aux (id (getf (ignore-errors (nle:turn)) :session-id)))
  "The x-opencode-session value of a request made now: an opaque digest of the
running turn's session id, the same on every round of that session, else
this process's own id."
  ;; A digest, never the id itself: a channel session's id names the room.
  (if (stringp id)
      (format nil "nodecode-~a" (subseq (nlk::sha256-text id) 7 39))
      (process-session)))

(defun session-headers (headers)
  "HEADERS with the session header, unless they carry one already."
  (if (assoc +session-header+ headers :test #'string-equal)
      headers
      (append headers (list (cons +session-header+ (session-value))))))
