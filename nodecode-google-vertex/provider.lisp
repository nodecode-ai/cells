;;;; provider.lisp --- what Vertex AI is: its hosts, its models, the address each wire takes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; auth/google-vertex.kdl (an API key or Application Default Credentials)
;;;; and providers/google-vertex.kdl (the default model, the request rules),
;;;; catalog/src/hosts.ts (resolveVertexEndpointHost), catalog/src/provider-
;;;; models/openai-compat.ts (each model's wire and URL template),
;;;; ai/src/providers/google-vertex.ts (the Gemini request: its address with a
;;;; key and with ADC, the safety settings), ai/src/stream.ts (the Claude and
;;;; OpenAI-compatible requests: the template filled from the environment,
;;;; the ADC bearer, the Claude body's anthropic_version), and the bundled
;;;; rows of catalog/src/models.json, which models.json in this folder
;;;; carries (tools/omp-models.py wrote it).
;;;;
;;;; Vertex AI serves three wires on one Google Cloud project:
;;;;
;;;;   Gemini        the GenAI wire, POST .../publishers/google/models/
;;;;                 <model>:streamGenerateContent?alt=sse
;;;;   Claude        the Messages wire, POST .../publishers/anthropic/models/
;;;;                 <model@version>:streamRawPredict, the model named by the
;;;;                 address and anthropic_version by the body
;;;;   partners      the chat wire, POST .../endpoints/openapi/chat/completions
;;;;
;;;; each under https://<host>/v1/projects/<project>/locations/<location>,
;;;; the host the location's (aiplatform.googleapis.com for global, the
;;;; multi-region REP hosts for eu and us, <location>-aiplatform otherwise).
;;;; Every request carries the ADC bearer, except a Gemini one sent with an
;;;; API key, which goes to the project-less express address with
;;;; x-goog-api-key. Each is the core's own wire, so this cell rides the
;;;; core's google, anthropic and chat lanes through their hooks.

(in-package #:nodecode-google-vertex)

(defparameter +api-key-env+ "GOOGLE_CLOUD_API_KEY"
  "The variable omp reads a Vertex API key from.")

(defparameter +project-env+ '("GOOGLE_CLOUD_PROJECT" "GCP_PROJECT" "GCLOUD_PROJECT")
  "Where omp reads the project, in order.")

(defparameter +location-env+ '("GOOGLE_VERTEX_LOCATION" "GOOGLE_CLOUD_LOCATION" "VERTEX_LOCATION")
  "Where omp reads the location, in order.")

(defparameter +template-host+ "https://{location}-aiplatform.googleapis.com"
  "The host every bundled row's template opens with, replaced by the location's own.")

(defparameter +anthropic-template+
  "https://{location}-aiplatform.googleapis.com/v1/projects/{project}/locations/{location}/publishers/anthropic/models/{model}:streamRawPredict"
  "The Claude address for a model off the roster (omp's GOOGLE_VERTEX_ANTHROPIC_BASE_URL).")

(defparameter +openapi-template+
  "https://{location}-aiplatform.googleapis.com/v1/projects/{project}/locations/{location}/endpoints/openapi"
  "The OpenAI-compatible base for a model off the roster (omp's GOOGLE_VERTEX_OPENAI_BASE_URL).")

(defparameter +vertex-anthropic-version+ "vertex-2023-10-16"
  "What a Claude body names as anthropic_version: Vertex takes it there, not in the address.")

(defparameter +safety-settings+
  '("HARM_CATEGORY_HATE_SPEECH" "HARM_CATEGORY_DANGEROUS_CONTENT"
    "HARM_CATEGORY_SEXUALLY_EXPLICIT" "HARM_CATEGORY_HARASSMENT")
  "The harm categories a Gemini request turns OFF when it names none.")

;;; --- the models ----------------------------------------------------------------

(defparameter +models+
  (nlk:decode-json
   (uiop:read-file-string
    (asdf:system-relative-pathname "nodecode-google-vertex" "models.json")))
  "omp's bundled Vertex rows, read when this file loads: a vector of objects.")

(defun model-row (model-id)
  "The bundled row of MODEL-ID, or NIL."
  (find model-id +models+ :key (lambda (row) (nlk:json-value row :string "id")) :test #'equal))

(defun model-lane (model-id)
  "The lane MODEL-ID rides: its row's wire; off the roster, the chat wire for
a namespaced id and Gemini's for any other, as omp's resolveGoogleVertexApi
reads models.dev, Claude's for a claude- id."
  (let ((api (nlk:json-value (model-row model-id) :string "api")))
    (cond ((equal api "anthropic-messages") "anthropic")
          ((equal api "openai-completions") "openai-completions")
          (api "google")
          ((find #\/ model-id) "openai-completions")
          ((uiop:string-prefix-p "claude-" model-id) "anthropic")
          (t "google"))))

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
       (let ((input (nlk:json-value cost :number "input"))
             (output (nlk:json-value cost :number "output")))
         (and input output (or (plusp input) (plusp output))
              (list input output
                    (nlk:json-value cost :number "cacheRead")
                    (nlk:json-value cost :number "cacheWrite"))))))))

;;; --- the project, the location, the host ----------------------------------------------

(defun present (value)
  "VALUE trimmed when it is a non-empty string, else NIL."
  (and (stringp value)
       (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
         (and (plusp (length trimmed)) trimmed))))

(defun project ()
  "The project the section names, else the first of omp's variables that does, or NIL."
  (or (present (setting :project)) (some #'nle::credential-env +project-env+)))

(defun ambient-location ()
  "The location omp's variables name, or NIL."
  (some #'nle::credential-env +location-env+))

(defun location ()
  "The location the section names, else omp's variables', or NIL."
  (or (present (setting :location)) (ambient-location)))

(defun vertex-host (location)
  "The host a request for LOCATION goes to (omp's resolveVertexEndpointHost):
the multi-regions are REP hosts, never <location>-aiplatform."
  (cond ((equal location "global") "aiplatform.googleapis.com")
        ((member location '("eu" "us") :test #'equal) (format nil "aiplatform.~a.rep.googleapis.com" location))
        (t (format nil "~a-aiplatform.googleapis.com" location))))

(defun require-project ()
  (or (project)
      (error 'nle::provider-config-error
             :detail "Vertex AI requires a project ID. Set google-vertex.project, or GOOGLE_CLOUD_PROJECT/GCP_PROJECT/GCLOUD_PROJECT.")))

(defun require-location ()
  (or (location)
      (error 'nle::provider-config-error
             :detail "Vertex AI requires a location. Set google-vertex.location, or GOOGLE_VERTEX_LOCATION/GOOGLE_CLOUD_LOCATION/VERTEX_LOCATION.")))

(defun replace-all (text needle value)
  "TEXT with every NEEDLE in it replaced by VALUE."
  (with-output-to-string (out)
    (loop with start = 0
          for at = (search needle text :start2 start)
          do (write-string text out :start start :end (or at (length text)))
             (if at
                 (progn (write-string value out) (setf start (+ at (length needle))))
                 (return)))))

(defun fill-template (template project location &optional model)
  "TEMPLATE, a row's address, with the location's host and the project,
location and MODEL filled in (omp's resolveVertexRequest)."
  (let* ((host (concatenate 'string "https://" (vertex-host location)))
         (text (if (uiop:string-prefix-p +template-host+ template)
                   (concatenate 'string host (subseq template (length +template-host+)))
                   template)))
    (flet ((put (text needle value)
             (if value (replace-all text needle value) text)))
      (put (put (put text "{project}" (quri:url-encode project))
                "{location}" (quri:url-encode location))
           "{model}" model))))

(defun gemini-endpoint (model project location)
  "The Gemini address of MODEL: under PROJECT and LOCATION, or the express
address when PROJECT is NIL (an API key's)."
  (if project
      (format nil "https://~a/v1/projects/~a/locations/~a/publishers/google/models/~a:streamGenerateContent?alt=sse"
              (vertex-host location) project location model)
      (format nil "https://~a/v1/publishers/google/models/~a:streamGenerateContent?alt=sse"
              (vertex-host location) model)))

(defun real-key-p (key)
  "Whether KEY is an API key, not a marker the credential stands in with
(omp's resolveApiKey: none starting <, not N/A)."
  (and (stringp key) (plusp (length key))
       (not (member key '("public" "vertex-adc" "N/A") :test #'equal))
       (char/= (char key 0) #\<)))

(defun catalog-row (&optional prior)
  "Vertex AI as a models.dev provider: the GenAI lane's package (each
model's own lane is RESOLVE-MODEL-LANE's answer), the Gemini base on the
section's project and location when they are known, no key variable the
core should read for it, and the bundled models over PRIOR's."
  (let ((models (make-hash-table :test 'equal))
        (project (project))
        (location (location)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for row across +models+
          do (setf (gethash (nlk:json-value row :string "id") models) (catalog-model row)))
    (nlk:json-object "name" "Google Vertex AI"
                     "npm" "@ai-sdk/google"
                     "api" (if (and project location)
                               (format nil "https://~a/v1/projects/~a/locations/~a/publishers/google"
                                       (vertex-host location) project location)
                               +template-host+)
                     "env" (vector +api-key-env+)
                     "models" models)))

(defun listing-rows ()
  "The roster as a provider listing answers it: the core's listing would ask
the template host, so the picker is answered from the roster without a request."
  (loop for row across +models+
        collect (list :id (nlk:json-value row :string "id")
                      :display (nlk:json-value row :string "name")
                      :context-window (nlk:json-value row :integer "context"))))
