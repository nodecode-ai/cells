;;;; provider.lisp --- what Muse Code is: its address, its models, its wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; providers/muse-code.kdl (the base and the wire rules), packages/ai/src/
;;;; registry/muse-code.ts (the transport: the minted key as the bearer, the
;;;; API version header), and the bundled rows of catalog/src/models.json,
;;;; which models.json in this folder carries (tools/omp-models.py wrote it).
;;;;
;;;; A Muse Code subscription signs in to Meta with a device code
;;;; (signin.lisp); the account token mints a Model API key, and that key is
;;;; what every model request spends, on Meta's Responses API at
;;;; https://api.meta.ai/v1, with x-api-version 1.0.0 beside it. Two rules of
;;;; Meta's endpoint reach the request: tool_choice is refused in every form
;;;; but auto (HTTP 400, verified by omp 2026-09-10), so it is never sent;
;;;; and results are not stored (store false), which is omp's default too,
;;;; storage there being an opt-in that keeps prompts on Meta's side.

(in-package #:nodecode-muse-code)

(defparameter +base+ "https://api.meta.ai/v1"
  "Where Muse Code is served: the base the Responses lane appends /responses to.")

(defparameter +api-version+ "1.0.0"
  "The x-api-version every Meta request carries, the sign-in's included.")

;;; --- the models ----------------------------------------------------------------

(defparameter +models+
  (nlk:decode-json
   (uiop:read-file-string
    (asdf:system-relative-pathname "nodecode-muse-code" "models.json")))
  "omp's bundled Muse Code rows, read when this file loads: a vector of objects.")

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
  "Muse Code as a models.dev provider: the Responses lane's package, this
section's base, no key variable (the sign-in is the only credential), and
the bundled models over PRIOR's (the row models.dev itself published)."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for row across +models+
          do (setf (gethash (nlk:json-value row :string "id") models) (catalog-model row)))
    (nlk:json-object "name" "Muse Code (Subscription)"
                     "npm" "@ai-sdk/openai"
                     "api" (setting :base-url)
                     "env" #()
                     "models" models)))

(defun list-models (key base)
  "(values ROWS ERROR): Meta's model listing at BASE, asked with the minted
KEY and the API version the way omp's discovery asks it, or NIL and why."
  (if (or (null key) (equal key "public"))
      (values nil "not signed in")
      (multiple-value-bind (body status)
          (nle::http-fetch (format nil "~a/models" (string-right-trim "/" base))
                           :headers `(("Authorization" . ,(format nil "Bearer ~a" key))
                                      ("x-api-version" . ,+api-version+))
                           :timeout nle::*provider-models-fetch-timeout-seconds*)
        (if (stringp status)
            (values nil status)
            (nle::parse-provider-listing (nlk:body-text body))))))

(defun muse-body (body)
  "BODY, the Responses lane's request, as Meta's endpoint takes it: no
tool_choice."
  (remhash "tool_choice" body)
  body)
