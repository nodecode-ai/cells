;;;; cell.lisp --- the cell: Vertex AI among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Eight hooks, each declining for every provider but google-vertex:
;;;;
;;;;   MODELS-CATALOG-TABLE     the catalog carries Vertex's row: omp's bundled
;;;;                            Gemini, Claude and partner models over
;;;;                            whatever models.dev published
;;;;   LIST-PROVIDER-MODELS     the listing is the roster: the core's would ask
;;;;                            a template host
;;;;   RESOLVE-MODEL-LANE       Gemini rides the GenAI lane, Claude the
;;;;                            Messages lane, the partners the chat lane,
;;;;                            unless the operator's config pins one
;;;;   :CREDENTIAL              an API key from GOOGLE_CLOUD_API_KEY, else the
;;;;                            ADC marker the round turns into a bearer; a
;;;;                            key /connect saved answers before this point
;;;;   GOOGLE-REQUEST-BODY      a Gemini body turns the four harm categories
;;;;                            off when it names none
;;;;   ANTHROPIC-REQUEST-BODY   a Claude body names no model (the address
;;;;                            does), anthropic_version, and no output effort
;;;;   REQUEST-BODY             a partner body's cap as max_completion_tokens,
;;;;                            and no cache key
;;;;   WALK-PROVIDER-STREAM     each round goes to its wire's Vertex address,
;;;;                            with the ADC bearer, or a Gemini one with its
;;;;                            API key to the express address
;;;;
;;;; omp's login for Vertex is no login: an API key, or Application Default
;;;; Credentials resolved from the machine (adc.lisp). The project and the
;;;; location are the section's settings, else omp's variables.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "google-vertex": {"project": "my-project", "location": "us-central1"}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-google-vertex)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is a google-vertex round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Vertex's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Vertex's row, made once per
catalog the core reads."
  (if (and (car memo) (eq (car memo) base))
      (cdr memo)
      (let ((merged (make-hash-table :test 'equal)))
        (when (hash-table-p base)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) base))
        (setf (gethash +provider+ merged)
              (catalog-row (and (hash-table-p base) (gethash +provider+ base))))
        (setf *catalog* (cons base merged))
        merged)))

(defun listing (next provider &rest keys &key key &allow-other-keys)
  "LIST-PROVIDER-MODELS advice: Vertex's listing is its roster. Asked with
KEY, which only /connect's key check does, it says why the key was not
checked."
  ;; The roster asks Vertex nothing, and the check reads a NIL second value
  ;; as a key Vertex took: any key read `works'.
  (if (equal provider +provider+)
      (values (listing-rows)
              (and key "Vertex is asked nothing before a turn: its models come from the bundled roster, so the first turn tries the key"))
      (apply next provider keys)))

(defun pinned-p (provider model)
  "Whether the operator's config pins the wire PROVIDER's MODEL rides."
  (or (nle::trimmed-config-string (nle::configured-model-entry provider model) "sdk")
      (nle::trimmed-config-string (nle::configured-provider-entry provider) "sdk")))

(defun model-lane-advice (next provider model)
  "RESOLVE-MODEL-LANE advice: a Vertex model rides its wire's lane."
  (if (and (equal provider +provider+) (not (pinned-p provider model)))
      (model-lane model)
      (funcall next provider model)))

(defun credential (op next)
  "The :CREDENTIAL answer for google-vertex: the API key GOOGLE_CLOUD_API_KEY
holds, else the ADC marker a round turns into a bearer, else none."
  ;; Never NEXT for this provider: the ladder behind this point falls back to
  ;; the GenAI family's default variable, and would send GOOGLE_API_KEY, the
  ;; Gemini API's key, to Vertex. A probe (no endpoint) answers the marker
  ;; only when a source is named on this machine, and asks no network; a
  ;; round answers it always, since the metadata server may answer.
  (if (equal (getf op :provider) +provider+)
      (cond ((nle::credential-env +api-key-env+)
             (nle:make-credential (nle::credential-env +api-key-env+) :env))
            ((or (getf op :endpoint) (adc-source-p))
             (nle:make-credential "vertex-adc" :adc))
            (t (nle:make-credential "public" :public)))
      (funcall next op)))

(defun config-of (context)
  (nle::compiled-turn-context-provider-config context))

(defun gemini (next context &aux (body (funcall next context)) (config (config-of context)))
  "GOOGLE-REQUEST-BODY advice: a Vertex Gemini body turns the harm
categories off unless it names its own (omp's streamGoogleVertex)."
  (when (and (ours-p config) (hash-table-p body) (null (gethash "safetySettings" body)))
    (setf (gethash "safetySettings" body)
          (map 'vector (lambda (category) (nlk:json-object "category" category "threshold" "OFF"))
               +safety-settings+)))
  body)

(defun messages (next context)
  "ANTHROPIC-REQUEST-BODY advice: Vertex Claude takes the model in the
address and anthropic_version in the body, and refuses output_config.effort."
  (let ((answer (multiple-value-list (funcall next context)))
        (config (config-of context)))
    (when (and (ours-p config) (hash-table-p (first answer)))
      (let ((body (first answer)))
        (remhash "model" body)
        (remhash "output_config" body)
        (setf (gethash "anthropic_version" body) +vertex-anthropic-version+)))
    (values-list answer)))

(defun chat (next context &aux (body (funcall next context)) (config (config-of context)))
  "REQUEST-BODY advice: a Vertex partner round names its cap
max_completion_tokens and no cache key, as omp's chat compat does for a host
that is not OpenAI's own."
  (when (and (ours-p config) (hash-table-p body))
    (multiple-value-bind (cap present) (gethash "max_tokens" body)
      (when present
        (remhash "max_tokens" body)
        (setf (gethash "max_completion_tokens" body) cap)))
    (remhash "prompt_cache_key" body))
  body)

(defun row-template (model lane)
  "The address template MODEL's round on LANE fills: its row's, else the
lane's template for a model off the roster."
  (let ((row (model-row model)))
    (or (and row (not (equal lane "google")) (equal (model-lane model) lane) (nlk:json-value row :string "base"))
        (if (equal lane "anthropic") +anthropic-template+ +openapi-template+))))

(defun round-plan (config)
  "(values ENDPOINT HEADERS) of the round CONFIG freezes: its wire's Vertex
address and the credential header it carries."
  (let ((model (nle::effective-provider-config-model config))
        (lane (nle::effective-provider-config-lane config))
        (key (nle::effective-provider-config-api-key config)))
    (cond ((and (equal lane "google") (real-key-p key))
           ;; an explicit location is honoured, else the global host
           (values (gemini-endpoint model nil (or (location) "global"))
                   `(("x-goog-api-key" . ,key))))
          ((equal lane "google")
           (values (gemini-endpoint model (require-project) (require-location))
                   `(("Authorization" . ,(format nil "Bearer ~a" (access-token))))))
          (t
           (let ((filled (fill-template (row-template model lane) (require-project) (require-location) model)))
             (values (if (equal lane "anthropic")
                         ;; the SDK's /v1/messages suffix never reaches Vertex
                         filled
                         (concatenate 'string (string-right-trim "/" filled) "/chat/completions"))
                     `(("Authorization" . ,(format nil "Bearer ~a" (access-token))))))))))

(defun walk (next fold &rest keys &key config headers &allow-other-keys)
  "WALK-PROVIDER-STREAM advice: a Vertex round goes to its wire's address
with Vertex's credential in place of the lane's."
  (if (ours-p config)
      (multiple-value-bind (endpoint credential-headers) (round-plan config)
        (apply next fold
               :headers (append (remove-if (lambda (name)
                                             (member name '("authorization" "x-api-key" "x-goog-api-key")
                                                     :test #'string-equal))
                                           headers :key #'car)
                                credential-headers)
               :endpoint endpoint
               (alexandria:remove-from-plist keys :headers :endpoint)))
      (apply next fold keys)))

(defun start ()
  "Begin from a fresh catalog and no cached token; on stop drop both."
  (setf *catalog* (cons nil nil))
  (forget-tokens)
  (nle:on-stop (lambda () (setf *catalog* (cons nil nil)) (forget-tokens))))

(nle:define-cell google-vertex
  (:section ("google-vertex")
    (:guide "use Application Default Credentials (gcloud auth application-default login, or GOOGLE_APPLICATION_CREDENTIALS naming a service account file), or for Gemini an API key saved with /connect or GOOGLE_CLOUD_API_KEY; project and location name where Vertex serves you")
    ("project" :string :doc "the Google Cloud project (else GOOGLE_CLOUD_PROJECT, GCP_PROJECT or GCLOUD_PROJECT)")
    ("location" :string
     :doc "the Vertex location: global, us, eu or a region such as us-central1 (else GOOGLE_VERTEX_LOCATION, GOOGLE_CLOUD_LOCATION or VERTEX_LOCATION)"))
  (:start #'start)
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook 'nle::list-provider-models #'listing)
  (:hook 'nle::resolve-model-lane #'model-lane-advice)
  (:hook :credential #'credential)
  (:hook 'nle::google-request-body #'gemini)
  (:hook 'nle::anthropic-request-body #'messages)
  (:hook 'nle::request-body #'chat)
  (:hook 'nle::walk-provider-stream #'walk))
