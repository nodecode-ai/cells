;;;; pi-test.lisp --- exact Pi accounts on the existing Codex cell wire.
;;;; SPDX-License-Identifier: MIT
(in-package #:nodecode.test)

(defun oc-pi-config (&key failover)
  (flet ((entry (source label)
           (nlk:json-object "sdk" "openai-responses" "pi_auth" source "display_name" label
                            "models" (vector (nlk:json-object "id" "gpt-6.1-sol")))))
    (nlk:encode-json-object
     (nlk:json-object
      "providers" (nlk:json-object "pi-main" (entry "openai-codex" "ChatGPT main")
                                   "pi-second" (entry "openai-codex-2" "ChatGPT second"))
      :when failover "failover"
      (nlk:json-object "on" (vector "rate_limit")
                       "chain" (vector (nlk:json-object "provider" "pi-second" "model" "gpt-6.1-sol")))))))

(defun oc-pi-answer (source)
  (nlk:json-object "type" "oauth" "key" (format nil "synthetic-~a" source)
                   "accountId" (format nil "account-~a" source)))

(defmacro with-oc-pi ((&key failover) &body body)
  `(with-cell-stop ((openai-codex-start))
     (with-temp-shared-config ((oc-pi-config :failover ,failover))
       (with-temp-auth (auth "{}")
         (let ((nle::*auth-file-path* auth) (nle::*api-key* nil) (nle::*endpoint* nil)
               (nle::*provider* "pi-main") (nle::*model* "gpt-6.1-sol")
               (nodecode-openai-codex::*threads* (make-hash-table :test 'equal :synchronized t)))
           (with-stubbed-fdefinition (nodecode-openai-codex::call-pi-helper (source entry probe)
                                      (oc-pi-answer source))
             ,@body))))))

(deftest openai-codex-cell-pi-source-names-are-exact ()
  ;; Numeric aliases identify stored accounts, not alternate provider protocols.
  (dolist (source '("openai-codex" "openai-codex-1" "openai-codex-2" "openai-codex-30"))
    (is (nodecode-openai-codex::pi-codex-source-p source)))
  (dolist (source '(nil "openai" "openai-codex-0" "openai-codex-02" "openai-codex-work"))
    (is (not (nodecode-openai-codex::pi-codex-source-p source)))))

(deftest openai-codex-cell-pi-inventory-never-reads-credentials ()
  (with-oc-pi ()
    (with-stubbed-fdefinition (nodecode-openai-codex::call-pi-helper (&rest args)
                               (error "inventory must not open Pi credentials"))
      (multiple-value-bind (ids models) (nle::configured-provider-inventory)
        (is (member "pi-main" ids :test #'equal))
        (is (member "pi-second" ids :test #'equal))
        (is (equal '("ChatGPT main" "ChatGPT second")
                   (sort (mapcar (lambda (row) (getf row :provider-display)) models) #'string<))))
      (is (null (nle::failover-chain))))))

(deftest openai-codex-cell-pi-freezes-only-the-selected-account ()
  (with-oc-pi ()
    (let ((asked '()))
      (with-stubbed-fdefinition (nodecode-openai-codex::call-pi-helper (source entry probe)
                                 (push source asked)
                                 (oc-pi-answer source))
        (dolist (pair '(("pi-main" "openai-codex") ("pi-second" "openai-codex-2")))
          (let* ((config (nle::snapshot-effective-provider-config (first pair) "gpt-6.1-sol"))
                 (headers (nle::credential-attribute config :headers)))
            (is (equal (format nil "synthetic-~a" (second pair)) (nle::effective-provider-config-api-key config)))
            (is (equal (format nil "account-~a" (second pair)) (oc-header headers "chatgpt-account-id")))
            (is (search (second pair) (nle::credential-attribute config :cache-key)))
            (is (equal "https://chatgpt.com/backend-api/codex/responses"
                       (nle::credential-attribute config :endpoint))))))
      (is (equal '("openai-codex-2" "openai-codex") asked)))))

(deftest openai-codex-cell-pi-missing-login-never-falls-back-even-in-probes ()
  (with-oc-pi ()
    (with-stubbed-fdefinitions
        ((nodecode-openai-codex::call-pi-helper (&rest args)
           (nle::credential-fail "synthetic missing account"))
         (nle::env-credential (&rest args) (error "must not borrow environment auth")))
      (is (signals-error nle:credential-error (nle::resolve-provider-credential "pi-second")))
      (is (eq :none (nle::provider-auth-state "pi-second"))))))

(deftest openai-codex-cell-pi-config-and-native-key-retain-precedence ()
  (with-oc-pi ()
    (with-stubbed-fdefinition (nodecode-openai-codex::call-pi-helper (&rest args)
                               (error "higher-priority key must prevent Pi reads"))
      (let ((nle::*api-key* "synthetic-explicit"))
        (is (equal "synthetic-explicit" (nle:credential-key (nle::resolve-provider-credential "pi-second")))))
      (with-temp-auth (native "{\"api_keys\":{\"pi-second\":{\"key\":\"synthetic-native\"}}}")
        (is (equal "synthetic-native"
                   (nle:credential-key (nle::resolve-provider-credential "pi-second" :auth-path native))))))))

(deftest openai-codex-cell-pi-does-not-list-models-with-a-private-login ()
  (with-oc-pi ()
    (with-stubbed-fdefinition (nle::resolve-provider-credential (&rest args)
                               (error "model listing must not resolve a Pi login"))
      (multiple-value-bind (rows reason) (nle::list-provider-models "pi-second" :base "https://untrusted.invalid")
        (is (find "gpt-6.1-sol" rows :key (lambda (row) (getf row :id)) :test #'equal))
        (is (null reason))))))

(deftest openai-codex-cell-pi-rejects-another-model-wire ()
  (with-oc-pi ()
    (with-stubbed-fdefinition (nle::resolve-model-lane (provider model) "anthropic")
      (is (signals-error nle:credential-error
            (nle::snapshot-effective-provider-config "pi-second" "gpt-6.1-sol"))))))

(deftest openai-codex-cell-pi-transport-errors-never-echo-child-output ()
  (with-stubbed-fdefinition (nlk:run-bounded (argv &rest keys)
                             (values "synthetic-secret-output" "synthetic-secret-error" 1))
    (let ((condition (signals-error nle:credential-error
                       (nodecode-openai-codex::call-pi-helper "openai-codex-2" (nlk:json-object) nil))))
      (is (not (search "synthetic-secret" (princ-to-string condition)))))))

(deftest openai-codex-cell-pi-invalid-path-never-opens-the-default-store ()
  (with-stubbed-fdefinition (nlk:run-bounded (&rest args)
                             (error "invalid path must not launch a credential reader"))
    (dolist (value '(5 "" nil))
      (is (signals-error nle:credential-error
            (nodecode-openai-codex::call-pi-helper
             "openai-codex-2" (nlk:json-object "pi_auth_file" value) nil))))))

(deftest openai-codex-cell-pi-malformed-source-never-borrows-native-auth ()
  (with-cell-stop ((openai-codex-start))
    (with-temp-shared-config ("{\"providers\":{\"openai-codex\":{\"pi_auth\":5}}}")
      (with-temp-auth (auth "{}")
        (let ((nle::*api-key* nil))
          (with-stubbed-fdefinition (nodecode-openai-codex::call-pi-helper (&rest args)
                                     (error "invalid source must not read credentials"))
            (is (signals-error nle:credential-error
                  (nle::resolve-provider-credential "openai-codex" :auth-path auth)))
            (is (eq :none (nle::provider-auth-state "openai-codex" :auth-path auth)))))))))

(deftest openai-codex-cell-pi-uses-the-full-codex-wire-and-keeps-account-state-separate ()
  (with-oc-pi ()
    (let ((requests '()))
      (with-stubbed-fdefinition
          (dex:post (url &rest keys)
            (let* ((headers (getf keys :headers))
                   (account (oc-header headers "chatgpt-account-id"))
                   (sticky (format nil "sticky-~a" account))
                   (response-headers (make-hash-table :test 'equal)))
              (setf (gethash "x-codex-turn-state" response-headers) sticky)
              (push (list url headers (nlk:decode-json (getf keys :content))) requests)
              (values (apply #'make-truncated-sse-stream +oc-stream+) 200 response-headers)))
        (dolist (provider '("pi-main" "pi-second" "pi-main"))
          (let ((nle::*provider* provider))
            (is (equal "ok" (gethash "content" (nle::call-responses-streaming (user-context "hi"))))))))
      (setf requests (nreverse requests))
      (let* ((first (first requests)) (second (second requests)) (third (third requests))
             (headers-1 (second first)) (headers-2 (second second)) (headers-3 (second third)))
        (is (every (lambda (request) (equal "https://chatgpt.com/backend-api/codex/responses" (first request))) requests))
        (is (equal "Bearer synthetic-openai-codex" (oc-header headers-1 "authorization")))
        (is (equal "Bearer synthetic-openai-codex-2" (oc-header headers-2 "authorization")))
        (is (not (equal (oc-header headers-1 "thread-id") (oc-header headers-2 "thread-id"))))
        (is (equal (oc-header headers-1 "thread-id") (oc-header headers-3 "thread-id")))
        (is (null (oc-header headers-2 "x-codex-turn-state")))
        (is (equal "sticky-account-openai-codex" (oc-header headers-3 "x-codex-turn-state")))
        (is (not (equal (gethash "prompt_cache_key" (third first)) (gethash "prompt_cache_key" (third second)))))
        (dolist (request requests)
          (let ((body (third request)))
            (is (null (gethash "max_output_tokens" body)))
            (is (null (gethash "temperature" body)))
            (is (gethash "client_metadata" body))
            (is (equalp #("reasoning.encrypted_content") (gethash "include" body)))))))))

(deftest openai-codex-cell-pi-rate-limit-failover-is-opt-in-and-keeps-the-primary
    (with-session-store ("oc-pi-fallback"))
  (with-oc-pi (:failover t)
    (with-durable-turn (turn "oc-pi-fallback" :command-id "oc-pi-cmd" :input "go" :current t :retry-delay 0)
      (let ((nle::*max-stream-retries* 0) (nle::*max-request-retries* 0) (nle::*failover* t)
            (calls '()) (count 0))
        (with-stubbed-fdefinition
            (dex:post (url &rest keys)
              (incf count)
              (push (oc-header (getf keys :headers) "chatgpt-account-id") calls)
              (if (= count 1)
                  (values (make-string-input-stream "{}") 429)
                  (values (apply #'make-truncated-sse-stream +oc-stream+) 200)))
          (is (equal "ok" (gethash "content" (nle::call-provider (user-context "hi")))))
          (is (equal "ok" (gethash "content" (nle::call-provider (user-context "next"))))))
        (is (equal '("account-openai-codex" "account-openai-codex-2" "account-openai-codex") (nreverse calls)))
        (is (equal "pi-main" nle::*provider*))
        (is (= 1 (length (provider-fallback-facts "oc-pi-fallback"))))))))

(deftest openai-codex-cell-pi-stop-removes-all-account-advice ()
  (let ((nodecode-openai-codex::*threads* (make-hash-table :test 'equal :synchronized t))
        (stop (openai-codex-start)))
    (setf (gethash '("session" "pi-account") nodecode-openai-codex::*threads*) :synthetic-pi
          (gethash "native-session" nodecode-openai-codex::*threads*) :synthetic-native)
    (funcall stop)
    (is (null (gethash '("session" "pi-account") nodecode-openai-codex::*threads*)))
    (is (eq :synthetic-native (gethash "native-session" nodecode-openai-codex::*threads*)))
    (dolist (point '(nle::resolve-provider-credential nle::configured-provider-lane nle::provider-base-url
                     nle::snapshot-effective-provider-config nle::configured-provider-inventory
                     nle::list-provider-models))
      (is (null (cell-hook "nodecode-openai-codex" point))))))

(deftest openai-codex-cell-pi-conflicting-token-and-metadata-identities-are-refused ()
  (with-oc-pi ()
    (with-stubbed-fdefinition
        (nodecode-openai-codex::call-pi-helper (source entry probe)
          (nlk:json-object "type" "oauth" "key" +oc-access+ "accountId" "different-account"))
      (is (signals-error nle:credential-error (nle::resolve-provider-credential "pi-second"))))))

(deftest openai-codex-cell-pi-malformed-selection-cannot-be-captured-by-legacy-oauth ()
  (dolist (legacy-first '(t nil))
    (let ((legacy (lambda (op next)
                    (if (and (eq (getf op :family) :openai)
                             (nlk:json-value (getf op :auth) :object "oauth_tokens" "pi-main"))
                        (nle:make-credential "synthetic-wrong-native-account" :oauth)
                        (funcall next op)))))
      (unwind-protect
           (progn
             (when legacy-first (nle:hook :credential "legacy-auth-specimen" legacy))
             (with-cell-stop ((openai-codex-start))
               (unless legacy-first (nle:hook :credential "legacy-auth-specimen" legacy))
               (with-temp-shared-config ("{\"providers\":{\"pi-main\":{\"sdk\":\"openai-responses\",\"pi_auth\":5}}}")
                 (with-temp-auth (auth "{\"oauth_tokens\":{\"pi-main\":{\"access_token\":\"synthetic-native\"}}}")
                   (let ((nle::*api-key* nil))
                     (is (signals-error nle:credential-error
                           (nle::resolve-provider-credential "pi-main" :auth-path auth)))
                     (is (eq :none (nle::provider-auth-state "pi-main" :auth-path auth))))))))
        (nle:unhook :credential "legacy-auth-specimen")))))
