;;;; provider.lisp --- what ClinePass is: its address, its key, its models, its wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; auth/cline-pass.kdl and providers/cline-pass.kdl, catalog/src/wire/
;;;; cline-pass.ts (the client headers), catalog/src/cline-pass-model-id.ts
;;;; (the wire id), and the bundled rows of catalog/src/models.json, which
;;;; models.json in this folder carries (tools/omp-models.py wrote it).
;;;;
;;;; ClinePass speaks the OpenAI chat wire at https://api.cline.bot/api/v1
;;;; with a bearer key from the Cline dashboard. Two things set it apart from
;;;; a plain OpenAI-compatible endpoint: every request carries the Cline
;;;; CLI's client headers (the gateway gates some roster entries to Cline
;;;; clients), and a subscription model is named `cline-pass/<id>' on the
;;;; wire while its public id is bare. The free models (`cline-free/...')
;;;; go out as they are.

(in-package #:nodecode-cline-pass)

(defparameter +base+ "https://api.cline.bot/api/v1"
  "Where ClinePass is served: the base the chat lane appends /chat/completions to.")

(defparameter +env+ '("CLINE_API_KEY")
  "The environment variables a ClinePass key is read from, in order.")

(defparameter +key-page+ "https://app.cline.bot/dashboard/account"
  "Where a key is made: Settings, API Keys.")

(defparameter +client-version+ "3.0.58"
  "The Cline CLI version the client headers mirror.")

(defun platform ()
  "The platform as the Cline CLI names it: Node's process.platform."
  (case (uiop:operating-system)
    (:macosx "darwin")
    (:windows "win32")
    (t (string-downcase (uiop:operating-system)))))

(defun client-headers ()
  "The Cline CLI's client identity, which every ClinePass request carries."
  `(("HTTP-Referer" . "https://cline.bot")
    ("X-Title" . "Cline")
    ("X-IS-MULTIROOT" . "false")
    ("X-CLIENT-TYPE" . "cline-sdk")
    ("User-Agent" . ,(format nil "Cline/~a" +client-version+))
    ("X-CLIENT-VERSION" . ,+client-version+)
    ("X-PLATFORM" . ,(platform))
    ("X-PLATFORM-VERSION" . "3.0.54")
    ("X-CORE-VERSION" . "0.0.79")))

;;; --- the models ----------------------------------------------------------------

(defparameter +models+
  (nlk:decode-json
   (uiop:read-file-string
    (asdf:system-relative-pathname "nodecode-cline-pass" "models.json")))
  "omp's bundled ClinePass rows, read when this file loads: a vector of objects.")

(defun model-row (model-id)
  "The bundled row of MODEL-ID, or NIL."
  (find model-id +models+ :key (lambda (row) (nlk:json-value row :string "id")) :test #'equal))

(defun wire-id (model-id)
  "The id the gateway is asked for when the picker says MODEL-ID: a
subscription model's `cline-pass/' form, a free model's own."
  (or (nlk:json-value (model-row model-id) :string "wire_id") model-id))

(defun catalog-model (row)
  "ROW as the catalog keeps a model (NLE::MAKE-CATALOG-MODEL's fields)."
  (flet ((value (type key) (nlk:json-value row type key)))
    (let ((cost (value :object "cost")))
      (nle::make-catalog-model
       (value :string "name")
       (value :integer "context")
       (value :integer "output")
       (or (value :array "input") #("text"))
       #("text")
       (sort (remove-if-not #'nle::effort-rank (coerce (or (value :array "efforts") #()) 'list))
             #'< :key #'nle::effort-rank)
       (value :boolean "reasoning")
       nil
       t
       ;; CATALOG-PRICE's shape; a row priced at nothing is a plan's, not a price
       (let ((input (nlk:json-value cost :number "input"))
             (output (nlk:json-value cost :number "output")))
         (and input output (or (plusp input) (plusp output))
              (list input output
                    (nlk:json-value cost :number "cacheRead")
                    (nlk:json-value cost :number "cacheWrite"))))))))

(defun catalog-row (&optional prior)
  "ClinePass as a models.dev provider: the chat lane's package, this
section's base, the key variables, and the bundled models over PRIOR's (the
row models.dev itself published, when it did)."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for row across +models+
          do (setf (gethash (nlk:json-value row :string "id") models) (catalog-model row)))
    (nlk:json-object "name" "ClinePass"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (setting :base-url)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))
