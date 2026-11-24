;;;; cell.lisp --- the cell: the ngrok AI Gateway among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Four hooks, each declining for every provider but ngrok:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries the gateway's row: the chat
;;;;                           lane's package, this section's endpoint, the
;;;;                           key variable, and no models at all -- the model
;;;;                           list is per-account gateway configuration, so
;;;;                           /models lists what the gateway's own /models
;;;;                           answers (the core's listing, against the same
;;;;                           base and the same key), and a gateway that does
;;;;                           not answer leaves the picker empty rather than
;;;;                           offering models the key cannot reach
;;;;   :CREDENTIAL             a key from NGROK_AI_GATEWAY_KEY, and no other
;;;;                           variable; a key /connect saved in auth.json
;;;;                           answers before this point does
;;;;   WALK-PROVIDER-STREAM   the round names this client, and refuses to go
;;;;                           without a key at all, in words naming the
;;;;                           variable and the page a key is made on
;;;;   PROVIDER-FAILURE-REASON  a gateway failure states its own verdict, so
;;;;                           an unrouted model, an access policy, a rejected
;;;;                           upstream key and an empty account are not all
;;;;                           reported as the operator's key being refused
;;;;
;;;; Config, a sibling top-level key:
;;;;   "ngrok-ai-gateway": {"base_url": "https://gateway.ngrok.ai/v1"}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-ngrok-ai-gateway)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is an ngrok round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog
with the gateway's row in it.")

(defun catalog-row ()
  "The gateway as a models.dev provider: the chat lane's package, this
section's endpoint, the key variable, and the models the gateway itself
lists."
  (nlk:json-object "name" "ngrok AI Gateway"
                   "npm" "@ai-sdk/openai-compatible"
                   "api" (base)
                   "env" (coerce +env+ 'vector)
                   "models" (make-hash-table :test 'equal)))

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with the gateway's row, made
once per catalog the core reads."
  (if (and (car memo) (eq (car memo) base))
      (cdr memo)
      (let ((merged (make-hash-table :test 'equal)))
        (when (hash-table-p base)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) base))
        (setf (gethash +provider+ merged) (catalog-row))
        (setf *catalog* (cons base merged))
        merged)))

(defun credential (op next)
  "The :CREDENTIAL answer for ngrok: the key NGROK_AI_GATEWAY_KEY holds,
else none."
  ;; Never NEXT for ngrok: the ladder behind this point falls back to the
  ;; chat family's default variable, and would send OPENAI_API_KEY to the
  ;; gateway as an access key.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (or (some #'nle::credential-env +env+)
                                   (nle::provider-env-key +provider+)))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

(defun key-or-refuse (config)
  "The access key CONFIG carries, or a refusal naming where a key is made."
  (let ((key (nle::effective-provider-config-api-key config)))
    (if (and (stringp key) (plusp (length key)) (string/= key "public"))
        key
        (error 'nle::provider-config-error
               :provider +provider+
               :detail (format nil "no ngrok AI Gateway access key: make one at ~a, ~
                                    then save it with /connect or set ~a"
                               +key-page+ (first +env+))))))

(defun walk (next fold &rest keys &key config headers &allow-other-keys)
  "WALK-PROVIDER-STREAM advice: an ngrok round names this client, and goes
nowhere without a key."
  (if (ours-p config)
      (progn
        (key-or-refuse config)
        (apply next fold :headers (append headers (client-headers))
               (alexandria:remove-from-plist keys :headers)))
      (apply next fold keys)))

(defun said (condition)
  "The gateway's own words for CONDITION: its detail and the body it kept."
  (format nil "~a ~a"
          (or (nle::provider-error-detail condition) "")
          (let ((body (nle::provider-error-evidence-body condition)))
            (if (stringp body) body ""))))

(defun reason (next condition &rest args)
  "PROVIDER-FAILURE-REASON advice: an ngrok round's failure says what the
gateway said it was, and anything else answers the core's own reason."
  (let ((label (apply next condition args)))
    (handler-case
        (if (and (typep condition 'nle::provider-error)
                 (equal (nle::provider-error-provider condition) +provider+))
            (or (gateway-label (nle::provider-error-status condition) (said condition))
                label)
            label)
      ;; A hook's own bug is not the turn's failure: the core's label stands.
      (serious-condition () label))))

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another endpoint builds a new one."
  (setf *catalog* (cons nil nil)))

(nle:define-cell ngrok-ai-gateway
  (:section ("ngrok-ai-gateway")
    (:guide "make an access key at https://app.ngrok.ai/keys; save it with /connect or set NGROK_AI_GATEWAY_KEY; base_url is the gateway endpoint (else NGROK_AI_GATEWAY_BASE_URL)")
    ("base_url" :string
     :doc "the gateway endpoint the chat lane appends /chat/completions to (default https://gateway.ngrok.ai/v1)"))
  (:start (lambda () (forget-catalog) (nle:on-stop #'forget-catalog)))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::walk-provider-stream #'walk)
  (:hook 'nle::provider-failure-reason #'reason))
