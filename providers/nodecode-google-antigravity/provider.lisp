;;;; provider.lisp --- what Antigravity is: its addresses, its identity, its models.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; auth/google-antigravity.kdl and providers/google-antigravity.kdl, catalog/
;;;; src/wire/gemini-headers.ts (the Antigravity client's identity, its
;;;; version from the update manifest, the per-model wire profiles), ai/src/
;;;; providers/google-antigravity-forced-tool.md (forced-tool.md here), and
;;;; the bundled rows of catalog/src/models.json: models.json in this folder
;;;; carries what the catalog reads (tools/omp-models.py wrote it), wire.json
;;;; what the wire reads (wire-rows.py wrote it).
;;;;
;;;; Antigravity is Google's agent IDE. Its backend is the Cloud Code Assist
;;;; wire at https://daily-cloudcode-pa.googleapis.com (the sandbox host
;;;; daily-cloudcode-pa.sandbox.googleapis.com behind it), serving Gemini 3,
;;;; Claude and GPT-OSS to a Google account on Antigravity's free tier.
;;;; Requests carry the Antigravity client's user agent, whose version the
;;;; backend gates newer models on, so the version tracks Antigravity's
;;;; latest release.

(in-package #:nodecode-google-antigravity)

(defparameter +base+ "https://daily-cloudcode-pa.googleapis.com"
  "Where Antigravity's Cloud Code Assist is served.")

(defparameter +sandbox+ "https://daily-cloudcode-pa.sandbox.googleapis.com"
  "The sandbox host a round falls over to when the daily host fails before answering.")

(defparameter +default-version+ "2.19.1"
  "The Antigravity version the user agent names when the update manifest
cannot be read (PI_AI_ANTIGRAVITY_VERSION overrides both).")

(defparameter +manifest-url+
  "https://antigravity-hub-auto-updater-974169037036.us-central1.run.app/manifest/latest-arm64-mac.yml"
  "Antigravity's update manifest: its latest release's version.")

(defparameter +manifest-retry-seconds+ 600
  "How long a failed manifest read is not tried again.")

(defvar *discovered-version* nil
  "The version the update manifest named, once it was read.")

(defvar *manifest-failed-at* nil
  "When the last manifest read failed (universal time), or NIL.")

(defvar *manifest-lock* (bt2:make-lock :name "google-antigravity manifest")
  "Held across one manifest read, so concurrent rounds share it.")

(defun manifest-version (text)
  "The version an electron-builder update manifest TEXT names, or NIL."
  (dolist (line (uiop:split-string (or text "") :separator '(#\Newline)))
    (ppcre:register-groups-bind (quoted single bare)
        ("^\\s*version\\s*:\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s#]+))\\s*(?:#.*)?\\r?$" line)
      (let ((version (string-trim " " (or quoted single bare ""))))
        (return (and (ppcre:scan "^\\d+\\.\\d+\\.\\d+$" version) version))))))

(defun ensure-version ()
  "Read the latest Antigravity version from the update manifest, once per
process, a failure not retried for ten minutes; the pinned version stands
while it cannot be read."
  (unless (or (nle::credential-env "PI_AI_ANTIGRAVITY_VERSION") *discovered-version*
              (and *manifest-failed-at* (< (- (get-universal-time) *manifest-failed-at*) +manifest-retry-seconds+)))
    (bt2:with-lock-held (*manifest-lock*)
      (unless *discovered-version*
        (let ((version (ignore-errors
                        (multiple-value-bind (body status)
                            (dex:get +manifest-url+
                                     :headers '(("Cache-Control" . "no-cache") ("User-Agent" . "electron-builder"))
                                     :connect-timeout 5 :read-timeout 5)
                          (and (eql status 200) (manifest-version (nlk:body-text body)))))))
          (if version
              (setf *discovered-version* version *manifest-failed-at* nil)
              (setf *manifest-failed-at* (get-universal-time))))))))

(defun client-version ()
  "The Antigravity version requests name: the override, else the manifest's, else the pinned one."
  (or (nle::credential-env "PI_AI_ANTIGRAVITY_VERSION") *discovered-version* +default-version+))

(defun client-user-agent ()
  "The Antigravity client's user agent, as the real antigravity/hub client
sends it: os_type and arch pinned to the darwin/arm64 client the version is
read for (PI_AI_ANTIGRAVITY_CL, _OS and _ARCH override)."
  (format nil "antigravity/hub/~a (aidev_client; os_type=~a; arch=~a; cl=~a)"
          (client-version)
          (or (nle::credential-env "PI_AI_ANTIGRAVITY_OS") "darwin")
          (or (nle::credential-env "PI_AI_ANTIGRAVITY_ARCH") "arm64")
          (or (nle::credential-env "PI_AI_ANTIGRAVITY_CL") "963137146")))

(defparameter +wire-profiles+
  '(("gemini-3.5-flash-extra-low" "MODEL_PLACEHOLDER_M187" 65536)
    ("gemini-3.5-flash-low" "MODEL_PLACEHOLDER_M20" 65536)
    ("gemini-3-flash-agent" "MODEL_PLACEHOLDER_M132" 65536)
    ("gemini-3.1-pro-low" "MODEL_PLACEHOLDER_M36" 65535)
    ("gemini-pro-agent" "MODEL_PLACEHOLDER_M16" 65535)
    ;; Claude on daily-cloudcode-pa refuses maxOutputTokens over 64000
    ("claude-sonnet-4-6" nil 64000)
    ("claude-opus-4-6-thinking" nil 64000))
  "Per requested model id, the real client's labels.model_enum and its fixed
generationConfig.maxOutputTokens (ANTIGRAVITY_MODEL_WIRE_PROFILES).")

(defun wire-profile (model-id)
  "(values MODEL-ENUM MAX-OUTPUT-TOKENS) of the requested MODEL-ID, or NIL."
  (alexandria:when-let (profile (assoc model-id +wire-profiles+ :test #'equal))
    (values (second profile) (third profile))))

(defparameter +forced-tool-directive+
  (uiop:read-file-string (asdf:system-relative-pathname "nodecode-google-antigravity" "forced-tool.md"))
  "What a Gemini round that must call a tool is told: Antigravity's Gemini
routes drop the tool config, so the forced choice is restated in the transcript.")

;;; --- the models ----------------------------------------------------------------

(defun bundled-rows (name)
  "The rows of the file NAME in this folder: a vector of objects."
  (nlk:decode-json
   (uiop:read-file-string (asdf:system-relative-pathname "nodecode-google-antigravity" name))))

(defparameter +models+ (bundled-rows "models.json")
  "omp's bundled Antigravity rows as the catalog reads them, read when this file loads.")

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
         (claude (search "claude" id))
         (oss (search "gpt-oss" id)))
    (nlk:json-object
     "id" model-id
     "class" (cond (claude "anthropic") (oss "gpt-oss") (generation "gemini") (t "unknown"))
     "reasoning" (and (or claude oss (and generation (>= generation 2))) t)
     "images" (not oss)
     "tools" t
     "compat" (nlk:json-object
               "function_part_id" (and (or claude oss) t)
               "legacy_parameters" (and claude t)
               "drop_unsigned_thinking" (and claude t)
               "claude_thinking_beta" (and claude t)
               "claude_tool_mode" (and claude t)
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
  "Antigravity as a models.dev provider: this cell's lane package, this
section's base, no key variable (the credential is a sign-in), and the bundled
models over PRIOR's (the row models.dev itself published, when it did)."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for row across +models+
          for id = (nlk:json-value row :string "id")
          do (setf (gethash id models) (catalog-model row (model-facts id))))
    (nlk:json-object "name" "Antigravity (Gemini 3, Claude, GPT-OSS)"
                     "npm" "nodecode-google-antigravity"
                     "api" (setting :base-url)
                     "env" #()
                     "models" models)))
