;;;; provider.lisp --- what GitLab Duo Agent is: its instance, its models, its lane.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; providers/gitlab-duo-agent.kdl (the env name, the fallback row),
;;;; catalog/src/discovery/gitlab-duo-workflow.ts (a discovered model as a
;;;; catalog row: its context window by family, no reasoning knob, no price),
;;;; and the bundled row of catalog/src/models.json, which models.json in this
;;;; folder carries (tools/omp-models.py wrote it).
;;;;
;;;; No Nodecode lane speaks the Duo Workflow Service, so the cell registers
;;;; one: `gitlab-duo-agent', whose stream function (workflow.lisp) runs a
;;;; workflow over GitLab's WebSocket and folds what it streams into the chat
;;;; shape every lane answers. The catalog row names the lane's package
;;;; (`nodecode-gitlab-duo-agent'), so lane resolution finds it the way it
;;;; finds any catalog provider's.

(in-package #:nodecode-gitlab-duo-agent)

(defparameter +gitlab-url+ "https://gitlab.com"
  "The GitLab instance: sign-in, REST setup, and the workflow socket.")

(defparameter +npm+ "nodecode-gitlab-duo-agent"
  "The package the catalog row names and the lane is registered with.")

(defparameter +env+ '("GITLAB_TOKEN")
  "The environment variables a GitLab token is read from.")

(defparameter +default-context-window+ 200000
  "The window of a model no rule names: the Duo Workflow Service's own fallback.")

(defparameter +context-window-rules+
  '(("(?i)claude[_-]?opus" . 1000000)
    ("(?i)claude[_-]?sonnet" . 1000000)
    ("(?i)claude[_-]?haiku" . 200000)
    ("(?i)gemini" . 1000000)
    ("(?i)gpt[_-]?5" . 400000))
  "omp's context windows by model ref: the catalog GraphQL publishes none.")

;;; --- the models ----------------------------------------------------------------

(defparameter +models+
  (nlk:decode-json
   (uiop:read-file-string
    (asdf:system-relative-pathname "nodecode-gitlab-duo-agent" "models.json")))
  "omp's bundled fallback row, read when this file loads: a vector of objects.")

(defvar *discovered* nil
  "((REF . NAME) ...): the models the account's namespace offers, as discovery
last found them, or NIL before it has. Discovery is authoritative: once it
answers, the fallback row is not listed.")

(defun context-window (ref)
  "The window omp gives the model REF."
  (or (cdr (find-if (lambda (rule) (ppcre:scan (car rule) ref)) +context-window-rules+))
      +default-context-window+))

(defun model-entry (name window)
  "A catalog model called NAME with WINDOW: text in, tools, no reasoning
knob (the Agent Platform fixes model parameters server-side), no price."
  (nle::make-catalog-model name window nil #("text") #("text") nil nil nil t nil))

(defun catalog-row ()
  "GitLab Duo Agent as a models.dev provider: this cell's lane package, the
instance as its base, the token variable, the discovered models (else omp's
fallback row)."
  (let ((models (make-hash-table :test 'equal)))
    (if *discovered*
        (loop for (ref . name) in *discovered*
              do (setf (gethash ref models) (model-entry name (context-window ref))))
        (loop for row across +models+
              for id = (nlk:json-value row :string "id")
              do (setf (gethash id models)
                       (model-entry (nlk:json-value row :string "name")
                                    (or (nlk:json-value row :integer "context") (context-window id))))))
    (nlk:json-object "name" "GitLab Duo Agent"
                     "npm" +npm+
                     "api" (setting :gitlab-url)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun env-key ()
  "The first token one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))

(defun configured (key &rest env)
  "The section's KEY when it is set, else the first of omp's ENV variables that is."
  (let ((value (setting key)))
    (if (and (stringp value) (plusp (length (nlk:trimmed value))))
        (nlk:trimmed value)
        (some #'nle::credential-env env))))

;;; --- the catalog -----------------------------------------------------------------

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
GitLab Duo Agent's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with GitLab Duo Agent's row,
made once per catalog the core reads (and again when discovery lands)."
  (if (and (car memo) (eq (car memo) base))
      (cdr memo)
      (let ((merged (make-hash-table :test 'equal)))
        (when (hash-table-p base)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) base))
        (setf (gethash +provider+ merged) (catalog-row))
        (setf *catalog* (cons base merged))
        merged)))

(defun forget-catalog ()
  "Drop the merged catalog, so the next read builds it again."
  (setf *catalog* (cons nil nil)))
