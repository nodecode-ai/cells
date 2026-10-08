;;;; cell.lisp --- the cell: Factory Droid among the organism's providers, and its sign-in.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ten hooks, each declining for every provider but factory-droid, and one
;;;; command:
;;;;
;;;;   MODELS-CATALOG-TABLE     the catalog carries Factory's row: the roster
;;;;                            omp's discovery answers offline, so /models
;;;;                            lists it
;;;;   LIST-PROVIDER-MODELS     the listing is the roster: Factory has no
;;;;                            listing endpoint to ask
;;;;   RESOLVE-MODEL-LANE       each model rides its wire's lane (chat,
;;;;                            Responses, Messages or GenAI), unless the
;;;;                            operator's config pins one
;;;;   LANE-ENDPOINT            each lane's path on Factory's proxy
;;;;   :CREDENTIAL              the WorkOS token from auth.json, refreshed
;;;;                            first when it is about to expire, with the org
;;;;                            and the regions it was resolved with; a key
;;;;                            /connect saved answers before this point does.
;;;;                            There is no key variable: Factory's API keys
;;;;                            do not reach the LLM proxy
;;;;   REQUEST-BODY             each wire's body as the Factory CLI sends it
;;;;   RESPONSES-REQUEST-BODY   (wire.lisp), the round's route worked out
;;;;   ANTHROPIC-REQUEST-BODY   first (provider.lisp NOTE-ROUND)
;;;;   GOOGLE-REQUEST-BODY
;;;;   WALK-PROVIDER-STREAM     the region's host, and Factory's identity
;;;;                            headers in place of the lane's credential; a
;;;;                            refusal for the network's region says so
;;;;   /factory-droid           login (a WorkOS device code), logout, status
;;;;
;;;; Config, a sibling top-level key:
;;;;   "factory-droid": {"base_url": "https://api.factory.ai"}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-factory-droid)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is a factory-droid round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Factory's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Factory's row, made once per
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
  "LIST-PROVIDER-MODELS advice: Factory's listing is its roster. Asked with
KEY, which only /connect's key check does, it says why the key was not
checked."
  ;; The roster asks Factory nothing, and the check reads a NIL second value
  ;; as a key Factory took: any key read `works'.
  (if (equal provider +provider+)
      (values (listing-rows)
              (and key "Factory has no endpoint that checks a key, and its API keys do not reach the LLM proxy; /factory-droid login signs in"))
      (apply next provider keys)))

(defun pinned-p (provider model)
  "Whether the operator's config pins the wire PROVIDER's MODEL rides."
  (or (nle::trimmed-config-string (nle::configured-model-entry provider model) "sdk")
      (nle::trimmed-config-string (nle::configured-provider-entry provider) "sdk")))

(defun model-lane-advice (next provider model)
  "RESOLVE-MODEL-LANE advice: a Factory model rides its wire's lane."
  (if (and (equal provider +provider+) (not (pinned-p provider model)))
      (model-lane model)
      (funcall next provider model)))

(defun endpoint (next provider lane)
  "LANE-ENDPOINT advice: a Factory lane's path on the section's host."
  (if (equal provider +provider+)
      (let ((lane (or lane (nle::resolve-model-lane provider nle::*model*))))
        (concatenate 'string (string-right-trim "/" (setting :base-url)) (lane-path lane)))
      (funcall next provider lane)))

(defun credential (op next)
  "The :CREDENTIAL answer for factory-droid: the sign-in's token, else the
keyless placeholder, which the send refuses in words."
  (if (equal (getf op :provider) +provider+)
      (or (token-credential op) (nle:make-credential "public" :public))
      (funcall next op)))

(defun config-of (context)
  (nle::compiled-turn-context-provider-config context))

(defun chat (next context &aux (body (funcall next context)) (config (config-of context)))
  "REQUEST-BODY advice: a Factory chat round as the CLI sends it."
  (if (and (ours-p config) (hash-table-p body))
      (chat-body body (note-round context) config)
      body))

(defun responses (next context &aux (body (funcall next context)) (config (config-of context)))
  "RESPONSES-REQUEST-BODY advice: a Factory Responses round as the CLI sends it."
  (if (and (ours-p config) (hash-table-p body))
      (responses-body body (note-round context))
      body))

(defun messages (next context)
  "ANTHROPIC-REQUEST-BODY advice: a Factory Messages round as the CLI sends it."
  (let ((answer (multiple-value-list (funcall next context)))
        (config (config-of context)))
    (when (and (ours-p config) (hash-table-p (first answer)))
      (setf (second answer) (messages-body (first answer) (second answer) (note-round context))))
    (values-list answer)))

(defun gemini (next context &aux (body (funcall next context)) (config (config-of context)))
  "GOOGLE-REQUEST-BODY advice: a Factory GenAI round as the CLI sends it."
  (if (and (ours-p config) (hash-table-p body))
      (multiple-value-bind (body names) (gemini-body body (note-round context) config)
        (setf (getf (gethash config *rounds*) :names) names)
        body)
      body))

(defun walk (next fold &rest keys &key config headers &allow-other-keys)
  "WALK-PROVIDER-STREAM advice: a Factory round goes to its region's host
with Factory's identity, its tool names turned back on the GenAI wire."
  (if (ours-p config)
      (let ((lane (nle::effective-provider-config-lane config))
            (facts (or (round-facts config)
                       (list :request (random-uuid) :session (random-uuid)))))
        (when (equal "public" (nle::effective-provider-config-api-key config))
          (error 'nle::provider-config-error
                 :detail "No Factory Droid credentials found. Run /factory-droid login (WorkOS device code)."))
        (handler-bind ((nle::provider-error
                         (lambda (condition)
                           (note-region-refusal condition (nle::effective-provider-config-model config)))))
          (apply next (alexandria:if-let (names (getf facts :names))
                        (named-back-fold fold names)
                        fold)
                 :headers (request-headers headers config lane facts)
                 :endpoint (concatenate 'string (host (credential-region config)) (lane-path lane))
                 (alexandria:remove-from-plist keys :headers :endpoint))))
      (apply next fold keys)))

(defun note-region-refusal (condition model)
  "Say what a refusal of MODEL for the network's region means, on CONDITION's
note: the proxy's own words name no next step."
  (when (cl-ppcre:scan "(?i)not available in this region"
                       (format nil "~a ~a" (nle::provider-error-detail condition)
                               (or (nle::provider-error-evidence-body condition) "")))
    (setf (nle::provider-error-note condition)
          (format nil "~a is not served from your network's region. Choose another model." model))))

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base builds a new one."
  (setf *catalog* (cons nil nil)))

(defun start ()
  "Begin from a fresh catalog, and stop a sign-in in progress with the cell."
  (forget-catalog)
  (nle:on-stop #'forget-catalog)
  (nle:on-stop #'cancel-flow))

(nle:define-cell factory-droid
  (:section ("factory-droid")
    (:guide "sign in with /factory-droid login (a WorkOS device code: open the address, enter the code); base_url is Factory's host, an EU account moving to api.eu.factory.ai on its own")
    ("base_url" :string :default +host+
     :doc "the Factory host whose LLM proxy serves every wire (/api/llm/o, /a, /g)"))
  (:start #'start)
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook 'nle::list-provider-models #'listing)
  (:hook 'nle::resolve-model-lane #'model-lane-advice)
  (:hook 'nle::lane-endpoint #'endpoint)
  (:hook :credential #'credential)
  (:hook 'nle::request-body #'chat)
  (:hook 'nle::responses-request-body #'responses)
  (:hook 'nle::anthropic-request-body #'messages)
  (:hook 'nle::google-request-body #'gemini)
  (:hook 'nle::walk-provider-stream #'walk)
  (:command "factory-droid" 'run-command
            :description "Sign in to Factory Droid with a WorkOS device code"
            :argument-hint "login | logout | status"
            :session nil
            :complete 'complete-command))
