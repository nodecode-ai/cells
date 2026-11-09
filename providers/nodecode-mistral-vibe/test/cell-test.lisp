;;;; cell-test.lisp --- the Mistral Vibe cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every wire a stubbed dex:post or
;;;; a stubbed NLK:HTTP: nothing touches the network, the environment or the
;;;; operator's files.

(in-package #:nodecode.test)

(define-test-slice "mistral-vibe" "MISTRAL-VIBE-CELL-" :start nodecode-mistral-vibe:start-cell)

(define-cell-lifecycle-tests "mistral-vibe"
  (:hooks 'nle::models-catalog-table :credential 'nle::request-body)
  (:command "mistral-vibe")
  (:refused ("base_url" 5)))

;;; --- the catalog --------------------------------------------------------------

(deftest mistral-vibe-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((mistral-vibe-start))
    (let* ((catalog (nle::models-catalog-table))
           (row (nlk:json-value catalog :object "mistral-vibe"))
           (env (coerce (or (nlk:json-value row :array "env") #()) 'list)))
      (is (equal "Mistral Vibe" (nlk:json-value row :string "name")))
      (is (equal "https://api.mistral.ai/v1" (nlk:json-value row :string "api")))
      (is (nlk:json-value row :any "models" "mistral-vibe-cli-latest") "the fallback model is offered")
      (is (member "MISTRAL_VIBE_API_KEY" env :test #'equal) "the plugin's own key variable")
      (is (not (member "MISTRAL_API_KEY" env :test #'equal)) "and never a plain Mistral key")
      (is (equal "openai-completions" (nle::configured-provider-lane "mistral-vibe")))
      (is (equal "https://api.mistral.ai/v1/chat/completions"
                 (nle::lane-endpoint "mistral-vibe" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "mistral-vibe")))))

(deftest mistral-vibe-cell-base-follows-the-section ()
  (with-cell-stop ((mistral-vibe-start "base_url" "https://relay.example/v1"))
    (is (equal "https://relay.example/v1"
               (nlk:json-value (nle::models-catalog-table) :string "mistral-vibe" "api")))))

;;; --- the key -----------------------------------------------------------------

(deftest mistral-vibe-cell-reads-its-own-variable ()
  (with-cell-stop ((mistral-vibe-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "MISTRAL_VIBE_API_KEY") "sk-vibe"))
        (let ((credential (nle::resolve-provider-credential "mistral-vibe" :auth-path auth :probe t)))
          (is (equal "sk-vibe" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential))))))))

(deftest mistral-vibe-cell-never-sends-another-familys-key ()
  (with-cell-stop ((mistral-vibe-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (member name '("MISTRAL_API_KEY" "OPENAI_API_KEY")
                                              :test #'equal)
                                      "sk-someone-else"))
        (let ((credential (nle::resolve-provider-credential "mistral-vibe" :auth-path auth :probe t)))
          (is (not (equal "sk-someone-else" (nle:credential-key credential))))
          (is (eq :public (nle:credential-source credential))))))))

(deftest mistral-vibe-cell-saved-key-outranks-the-variable ()
  (with-cell-stop ((mistral-vibe-start))
    (with-temp-auth (auth "{\"api_keys\":{\"mistral-vibe\":{\"provider\":\"mistral-vibe\",\"key\":\"sk-saved\"}}}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "MISTRAL_VIBE_API_KEY") "sk-vibe"))
        (is (equal "sk-saved"
                   (nle:credential-key
                    (nle::resolve-provider-credential "mistral-vibe" :auth-path auth :probe t))))))))

;;; --- the wire ------------------------------------------------------------------

(defmacro with-vibe-round ((url body) model &body forms)
  "FORMS with the cell started and one chat round for MODEL captured: URL and
BODY (the decoded request) as dex:post saw them, at a medium effort."
  `(with-cell-stop ((mistral-vibe-start))
     (let ((nle::*provider* "mistral-vibe") (nle::*model* ,model) (nle::*api-key* "sk-test")
           (nle::*endpoint* nil) (nle::*reasoning-effort* "medium")
           (,url nil) (,body nil))
       (declare (ignorable ,url ,body))
       (with-stubbed-fdefinition
           (dex:post (asked &rest args)
            (setf ,url asked ,body (nlk:decode-json (getf args :content)))
            (values (make-truncated-sse-stream
                     "{\"choices\":[{\"index\":0,\"delta\":{\"content\":\"ok\"}}]}"
                     "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}"
                     "[DONE]")
                    200))
         (nle::call-provider-streaming (user-context)))
       ,@forms)))

(deftest mistral-vibe-cell-clamps-a-glm-effort-down-to-the-relay-set ()
  (with-vibe-round (url body) "zai-glm-5-3"
    (is (equal "https://api.mistral.ai/v1/chat/completions" url))
    (is (equal "low" (nlk:json-value body :string "reasoning_effort")) "it clamps down")))

(deftest mistral-vibe-cell-keeps-medium-where-the-relay-takes-it ()
  (with-vibe-round (url body) "zai-glm-5-2"
    (is (equal "medium" (nlk:json-value body :string "reasoning_effort")))))

(deftest mistral-vibe-cell-sends-no-effort-to-an-unverified-model ()
  (with-vibe-round (url body) "mistral-vibe-cli-latest"
    (is (null (nth-value 1 (gethash "reasoning_effort" body))) "the relay's default answers")))

(deftest mistral-vibe-cell-clamp-keeps-the-top-tier-and-the-weakest ()
  (flet ((clamp (effort levels) (nodecode-mistral-vibe::clamp-relay-effort effort levels)))
    (is (equal "max" (clamp "xhigh" '("low" "high" "max"))) "the vendor top tier")
    (is (equal "high" (clamp "high" '("low" "high" "max"))))
    (is (equal "low" (clamp "minimal" '("low" "high"))))
    (is (equal "medium" (clamp "medium" '("low" "medium" "high" "max"))))))

;;; --- the sign-in ---------------------------------------------------------------

(defun vibe-console-answer (method url)
  "The console's answer to METHOD URL, as the plugin's flow expects it."
  (cond ((and (eq method :post) (search "/api/vibe/sign-in/" url))
         "{\"api_key\":\"sk-minted\"}")
        ((eq method :post)
         "{\"sign_in_url\":\"https://console.mistral.ai/vibe?process=p1\",\"poll_url\":\"https://console.mistral.ai/api/vibe/poll/p1\",\"process_id\":\"p1\",\"expires_at\":\"2030-01-01T00:00:00Z\"}")
        ((search "/api/vibe/poll/" url)
         "{\"status\":\"completed\",\"exchange_token\":\"tok\"}")
        ((search "/api/vibe/whoami" url) "{\"name\":\"vibe-user\"}")
        (t "{}")))

(deftest mistral-vibe-cell-signs-in-and-keeps-the-key ()
  (with-cell-stop ((mistral-vibe-start))
    (with-temp-auth (auth "{}")
      (let ((announced nil))
        (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                    (declare (ignore args))
                                    (values (vibe-console-answer method url) 200))
          (is (equal "vibe-user"
                     (nodecode-mistral-vibe::sign-in
                      :auth-path auth
                      :announce (lambda (url) (setf announced url)))))
          (is (equal "https://console.mistral.ai/vibe?process=p1" announced) "told where to sign in")
          (is (equal "sk-minted"
                     (nlk:json-value (nle::read-auth-file auth) :string
                                     "api_keys" "mistral-vibe" "key")) "it lands in auth.json"))))))

(deftest mistral-vibe-cell-challenge-is-the-rfc-7636-one ()
  ;; RFC 7636 B.1's verifier and its S256 challenge: PKCE checked without a
  ;; network.
  (is (equal "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
             (nodecode-mistral-vibe::pkce-challenge
              "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"))))

(deftest mistral-vibe-cell-refuses-a-sign-in-that-leaves-the-console ()
  (with-stubbed-fdefinition
      (nlk:http (method url &rest args)
       (declare (ignore method args url))
       (values "{\"sign_in_url\":\"https://elsewhere.example/x\",\"poll_url\":\"https://elsewhere.example/poll\",\"process_id\":\"p1\"}"
               200))
    (is (signals-error error (nodecode-mistral-vibe::start-process))))
  (is (not (nodecode-mistral-vibe::console-url-p "https://elsewhere.example/x")))
  (is (nodecode-mistral-vibe::console-url-p "https://console.mistral.ai/api/vibe/sign-in"))
  (is (not (nodecode-mistral-vibe::process-id-p "../etc"))))

;;; --- the command ---------------------------------------------------------------

(deftest mistral-vibe-cell-status-answers-with-no-key ()
  (with-cell-stop ((mistral-vibe-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (declare (ignore name))
                                 nil)
        (is (search "no key" (nodecode-mistral-vibe::status auth)))))))
