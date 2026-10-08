;;;; provider.lisp --- what Cloud Code Assist is: its address, its identity, its models.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; auth/google-gemini-cli.kdl and providers/google-gemini-cli.kdl, catalog/
;;;; src/wire/gemini-headers.ts (the Gemini CLI's identity), and the bundled
;;;; rows of catalog/src/models.json: models.json in this folder carries what
;;;; the catalog reads (tools/omp-models.py wrote it), wire.json what the wire
;;;; reads (wire-rows.py wrote it).
;;;;
;;;; Google Cloud Code Assist is the backend the Gemini CLI signs in to: a
;;;; Google account (a free tier, or a Google Cloud project) and Gemini models
;;;; at https://cloudcode-pa.googleapis.com. Its wire is the Gemini request
;;;; wrapped with the account's project, streamed back in an envelope of its
;;;; own (wire.lisp). Requests carry the Gemini CLI's identity, which unlocks
;;;; its rate limits.

(in-package #:nodecode-google-gemini-cli)

(defparameter +base+ "https://cloudcode-pa.googleapis.com"
  "Where Cloud Code Assist is served.")

(defparameter +cli-version+ "0.46.0"
  "The Gemini CLI version the user agent names (PI_AI_GEMINI_CLI_VERSION overrides).")

(defun node-platform ()
  "The platform as Node's process.platform names it."
  (case (uiop:operating-system)
    (:macosx "darwin")
    (:windows "win32")
    (t (string-downcase (uiop:operating-system)))))

(defun node-arch ()
  "The architecture as Node's process.arch names it."
  (let ((machine (string-downcase (machine-type))))
    (cond ((search "x86-64" machine) "x64")
          ((or (search "arm64" machine) (search "aarch64" machine)) "arm64")
          (t machine))))

(defun cli-user-agent (&optional (model-id "gemini-3.1-pro-preview"))
  "The Gemini CLI's user agent for MODEL-ID: GeminiCLI/VERSION/MODEL
(PLATFORM; ARCH; SURFACE), the format of the CLI since 0.35."
  (format nil "GeminiCLI/~a/~a (~a; ~a; terminal)"
          (or (nle::credential-env "PI_AI_GEMINI_CLI_VERSION") +cli-version+)
          model-id (node-platform) (node-arch)))

(defun cli-headers (&optional model-id)
  "The Gemini CLI's identity headers for a request about MODEL-ID."
  `(("User-Agent" . ,(if model-id (cli-user-agent model-id) (cli-user-agent)))
    ("Client-Metadata" . "ideType=IDE_UNSPECIFIED,platform=PLATFORM_UNSPECIFIED,pluginType=GEMINI")))

;;; --- the models ----------------------------------------------------------------

(defun bundled-rows (name)
  "The rows of the file NAME in this folder: a vector of objects."
  (nlk:decode-json
   (uiop:read-file-string (asdf:system-relative-pathname "nodecode-google-gemini-cli" name))))

(defparameter +models+ (bundled-rows "models.json")
  "omp's bundled Cloud Code Assist rows as the catalog reads them, read when
this file loads.")

(defparameter +wire-rows+ (bundled-rows "wire.json")
  "omp's bundled rows as the wire reads them: the requested id, the lineage,
how the model thinks, the compat flags.")

(defun find-row (rows model-id)
  "The row of ROWS whose id is MODEL-ID, or NIL."
  (find model-id rows :key (lambda (row) (nlk:json-value row :string "id")) :test #'equal))

(defun gemini-generation (model-id)
  "The major version of a gemini-X.Y MODEL-ID, or NIL."
  (ppcre:register-groups-bind ((#'parse-integer major)) ("^gemini-(\\d+)" (string-downcase model-id))
    major))

(defun inferred-facts (model-id)
  "The wire facts of a model the bundled rows do not carry, inferred from its
id the way omp's class rules would place it."
  (let* ((id (string-downcase model-id))
         (generation (gemini-generation id))
         (claude (search "claude" id)))
    (nlk:json-object
     "id" model-id
     "class" (cond (claude "anthropic") (generation "gemini") (t "unknown"))
     "reasoning" (and generation (>= generation 2) t)
     "images" t
     "tools" t
     "compat" (nlk:json-object
               "function_part_id" (and claude t)
               "legacy_parameters" (and claude t)
               "drop_unsigned_thinking" (and claude t)
               ;; the provider rule: Cloud Code Assist refuses an unsigned
               ;; first function call from Gemini 3 on
               "skip_signature_first_call" (and generation (>= generation 3) t)
               "multimodal_function_response" (and generation (>= generation 3) t)))))

(defun model-facts (model-id)
  "What the wire knows of MODEL-ID: its bundled row, else facts inferred from its id."
  (or (find-row +wire-rows+ model-id) (inferred-facts (or model-id ""))))

(defun compat (facts flag)
  "Whether FACTS's compat FLAG is set."
  (nlk:json-value facts :boolean "compat" flag))

(defun catalog-model (row facts)
  "ROW as the catalog keeps a model (NLE::MAKE-CATALOG-MODEL's fields); FACTS
say whether it takes tools at all (an image model does not)."
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
       (if (nth-value 1 (gethash "tools" facts)) (nlk:json-value facts :boolean "tools") t)
       ;; CATALOG-PRICE's shape; a row priced at nothing is a plan's, not a price
       (let ((input (nlk:json-value cost :number "input"))
             (output (nlk:json-value cost :number "output")))
         (and input output (or (plusp input) (plusp output))
              (list input output
                    (nlk:json-value cost :number "cacheRead")
                    (nlk:json-value cost :number "cacheWrite"))))))))

(defun catalog-row (&optional prior)
  "Cloud Code Assist as a models.dev provider: this cell's lane package, this
section's base, no key variable (the credential is a sign-in), and the bundled
models over PRIOR's (the row models.dev itself published, when it did)."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for row across +models+
          for id = (nlk:json-value row :string "id")
          do (setf (gethash id models) (catalog-model row (model-facts id))))
    (nlk:json-object "name" "Google Cloud Code Assist (Gemini CLI)"
                     "npm" "nodecode-google-gemini-cli"
                     "api" (setting :base-url)
                     "env" #()
                     "models" models)))
