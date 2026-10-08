;;;; cell-test.lisp --- the kilo cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json, every key variable a stubbed
;;;; NLE::CREDENTIAL-ENV, every Kilo endpoint and every round a stubbed
;;;; dex:post or dex:get, every wait between two polls a stubbed PAUSE:
;;;; nothing reaches Kilo, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "kilo" "KILO-CELL-" :start nodecode-kilo:start-cell)

(define-cell-lifecycle-tests "kilo"
  (:hooks 'nle::models-catalog-table :credential 'nle::request-body)
  (:command "kilo")
  (:refused ("base_url" 5)))

;;; --- fixtures --------------------------------------------------------------------

(defun kilo-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun kilo-now () (nodecode-kilo::unix-seconds))

(defun kilo-store (&key (token "kilo-token") (expires-in 3600))
  "auth.json text holding one kilo sign-in."
  (format nil "{\"oauth_tokens\":{\"kilo\":{\"provider\":\"kilo\",\"access_token\":\"~a\",~
               \"refresh_token\":\"\",\"expires_at\":~d}}}"
          token (+ (kilo-now) expires-in)))

(defun kilo-await-login ()
  "Wait for the running sign-in's thread to finish."
  (alexandria:when-let (login nodecode-kilo::*login*)
    (bt2:join-thread (nodecode-kilo::login-thread login))))

(defmacro with-kilo-login ((auth answer asked notices &key (polls ''(202 200)) (approved "\"approved\""))
                           &body body)
  "BODY with the cell started and a device sign-in begun against a temp
auth.json AUTH, ANSWER what /kilo login said, ASKED the (METHOD URL HEADERS)
Kilo saw, oldest first, and NOTICES the (TEXT LEVEL KEY) said. The poll
answers the statuses POLLS in turn, a 200 with the status APPROVED and a token."
  `(with-cell-stop ((kilo-start))
     (with-temp-auth (,auth "{}")
       (let ((nle::*auth-file-path* ,auth) (,asked '()) (,notices '()) (polls ,polls))
         (with-stubbed-fdefinitions
             ((nodecode-kilo::pause (seconds) nil)
              (nle:notice (text &key level key) (push (list text level key) ,notices))
              (dex:post (url &rest args)
               (push (list :post url (getf args :headers)) ,asked)
               (values "{\"code\":\"ABC123\",\"verificationUrl\":\"https://kilo.ai/verify\",\"expiresIn\":300}" 200))
              (dex:get (url &rest args)
               (push (list :get url (getf args :headers)) ,asked)
               (let ((status (or (pop polls) 200)))
                 (if (= status 200)
                     (values (format nil "{\"status\":~a,\"token\":\"kilo-access-token\"}" ,approved) 200)
                     (values "" status)))))
           (let ((,answer (cell-entry "nodecode-kilo" "kilo" "login")))
             (declare (ignorable ,answer))
             (kilo-await-login)
             (setf ,asked (reverse ,asked) ,notices (reverse ,notices))
             ,@body))))))

;;; --- the catalog -----------------------------------------------------------------

(deftest kilo-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((kilo-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "kilo")))
      (is (equal "Kilo Gateway" (nlk:json-value row :string "name")))
      (is (equal "https://api.kilo.ai/api/gateway" (nlk:json-value row :string "api")))
      (is (gethash "anthropic/claude-opus-4.7" (nlk:json-value row :object "models"))
          "the bundled models are listed")
      (is (< 500 (hash-table-count (nlk:json-value row :object "models"))) "all six hundred of them")
      (is (equal "openai-completions" (nle::configured-provider-lane "kilo")) "the chat lane drives it")
      (is (equal "https://api.kilo.ai/api/gateway/chat/completions"
                 (nle::lane-endpoint "kilo" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "kilo"))
        "a stopped cell leaves the catalog as models.dev made it")))

(deftest kilo-cell-base-follows-the-section ()
  (with-cell-stop ((kilo-start "base_url" "https://relay.example/gateway"))
    (is (equal "https://relay.example/gateway"
               (nlk:json-value (nle::models-catalog-table) :string "kilo" "api")))))

;;; --- the sign-in -----------------------------------------------------------------

(deftest kilo-cell-signs-in-with-a-device-code ()
  (with-kilo-login (auth answer asked notices)
    (is (search "https://kilo.ai/verify" answer) "the page Kilo names")
    (is (search "ABC123" answer) "the code to approve")
    (is (= 3 (length asked)) "a code, a poll still waiting, a poll approved")
    (destructuring-bind (method url headers) (first asked)
      (is (eq :post method))
      (is (equal "https://api.kilo.ai/api/device-auth/codes" url))
      (is (equal "application/json" (kilo-header headers "content-type"))))
    (destructuring-bind (method url headers) (second asked)
      (declare (ignore headers))
      (is (eq :get method))
      (is (equal "https://api.kilo.ai/api/device-auth/codes/ABC123" url)))
    (let ((entry (nlk:json-value (nle::read-auth-file auth) :object "oauth_tokens" "kilo")))
      (is (equal "kilo-access-token" (nlk:json-value entry :string "access_token")))
      (is (equal "" (nlk:json-value entry :string "refresh_token")) "no refresh token, as omp keeps it")
      (is (<= (abs (- (nlk:json-value entry :integer "expires_at") (+ (kilo-now) (* 365 86400)))) 5)
          "good for a year"))
    (is (= #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (namestring auth))))))
    (is (find-if (lambda (notice) (and (search "kilo: signed in" (or (first notice) ""))
                                       (null (third notice))))
                 notices)
        "success is said once, under no key")
    (is (search "signed in" (cell-entry "nodecode-kilo" "kilo" "status")))))

(deftest kilo-cell-says-a-denied-sign-in ()
  (with-kilo-login (auth answer asked notices :polls '(202 403))
    (is (null (nlk:json-value (nle::read-auth-file auth) :object "oauth_tokens")) "nothing kept")
    (is (find-if (lambda (notice) (search "sign-in failed: Authorization was denied" (or (first notice) "")))
                 notices))))

(deftest kilo-cell-says-a-code-kilo-says-expired ()
  (with-kilo-login (auth answer asked notices :polls '(200) :approved "\"expired\"")
    (is (null (nlk:json-value (nle::read-auth-file auth) :object "oauth_tokens")))
    (is (find-if (lambda (notice) (search "Authorization code expired" (or (first notice) "")))
                 notices))))

(deftest kilo-cell-refuses-a-refused-initiation ()
  (with-cell-stop ((kilo-start))
    (with-temp-auth (auth "{}")
      (let ((nle::*auth-file-path* auth))
        (with-stubbed-fdefinition (dex:post (url &rest args) (values "" 429))
          (is (search "Too many pending authorization requests"
                      (cell-entry "nodecode-kilo" "kilo" "login")))
          (is (null nodecode-kilo::*login*)))
        (with-stubbed-fdefinition (dex:post (url &rest args) (values "{\"code\":\"X\"}" 200))
          (is (search "missing required fields" (cell-entry "nodecode-kilo" "kilo" "login"))))))))

(deftest kilo-cell-logs-out ()
  (with-cell-stop ((kilo-start))
    (with-temp-auth (auth (kilo-store))
      (let ((nle::*auth-file-path* auth))
        (is (search "signed out" (cell-entry "nodecode-kilo" "kilo" "logout")))
        (is (null (nlk:json-value (nle::read-auth-file auth) :object "oauth_tokens" "kilo")))
        (is (search "not signed in" (cell-entry "nodecode-kilo" "kilo" "status")))))))

;;; --- the credential --------------------------------------------------------------

(deftest kilo-cell-answers-the-saved-sign-in ()
  (with-cell-stop ((kilo-start))
    (with-temp-auth (auth (kilo-store))
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "KILO_API_KEY") "sk-env"))
        (let ((credential (nle::resolve-provider-credential "kilo" :auth-path auth :probe t)))
          (is (equal "kilo-token" (nle:credential-key credential)) "the sign-in outranks the variable")
          (is (eq :oauth (nle:credential-source credential))))))))

(deftest kilo-cell-reads-kilo-api-key ()
  (with-cell-stop ((kilo-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "KILO_API_KEY") "sk-kilo"))
        (let ((credential (nle::resolve-provider-credential "kilo" :auth-path auth :probe t)))
          (is (equal "sk-kilo" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential))))
        (is (not (equal "sk-kilo"
                        (nle:credential-key (nle::resolve-provider-credential "anthropic" :auth-path auth :probe t))))
            "another provider's ladder never reads KILO_API_KEY")))))

(deftest kilo-cell-never-sends-another-familys-key ()
  (with-cell-stop ((kilo-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "kilo" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential)))
              "the chat family's default variable never reaches Kilo")
          (is (eq :public (nle:credential-source credential))))))))

(deftest kilo-cell-saved-key-outranks-the-sign-in ()
  (with-cell-stop ((kilo-start))
    (with-temp-auth (auth (format nil "{\"api_keys\":{\"kilo\":{\"provider\":\"kilo\",\"key\":\"sk-saved\"}},~a"
                                  (subseq (kilo-store) 1)))
      (is (equal "sk-saved"
                 (nle:credential-key (nle::resolve-provider-credential "kilo" :auth-path auth :probe t)))))))

(deftest kilo-cell-refuses-a-round-on-an-expired-sign-in ()
  (with-cell-stop ((kilo-start))
    (with-temp-auth (auth (kilo-store :expires-in -60))
      (let ((notices '()))
        (with-stubbed-fdefinition (nle:notice (text &key level key) (push (list text level key) notices))
          (is (equal "kilo-token"
                     (nle:credential-key (nle::resolve-provider-credential "kilo" :auth-path auth :probe t)))
              "a probe reads the store as it is")
          (is (signals-error nle:credential-error
                (nle::resolve-provider-credential "kilo" :auth-path auth
                                                         :endpoint "https://api.kilo.ai/api/gateway/chat/completions")))
          (is (equal "nodecode-kilo" (third (first notices))) "the needed sign-in stands under the cell's key")
          (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "KILO_API_KEY") "sk-env"))
            (is (equal "sk-env"
                       (nle:credential-key
                        (nle::resolve-provider-credential "kilo" :auth-path auth
                                                                 :endpoint "https://api.kilo.ai/api/gateway/chat/completions")))
                "a key beside an expired sign-in answers")))))))

;;; --- one round -------------------------------------------------------------------

(defmacro with-kilo-round ((url headers body) (model &key effort) &body forms)
  "FORMS with the cell started, signed in, and one chat round for kilo MODEL
at EFFORT captured: URL, HEADERS and BODY (the decoded request) as dex:post saw them."
  `(with-cell-stop ((kilo-start))
     (with-temp-auth (auth (kilo-store))
       (let ((nle::*auth-file-path* auth) (nle::*provider* "kilo") (nle::*model* ,model)
             (nle::*api-key* nil) (nle::*endpoint* nil) (nle::*reasoning-effort* ,effort)
             (,url nil) (,headers nil) (,body nil))
         (declare (ignorable ,url ,headers ,body))
         (with-stubbed-fdefinition
             (dex:post (asked &rest args)
              (setf ,url asked ,headers (getf args :headers)
                    ,body (nlk:decode-json (getf args :content)))
              (values (make-truncated-sse-stream
                       "{\"choices\":[{\"index\":0,\"delta\":{\"content\":\"ok\"}}]}"
                       "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}"
                       "[DONE]")
                      200))
           (nle::call-provider-streaming (user-context)))
         ,@forms))))

(deftest kilo-cell-sends-a-round-with-the-signed-in-token ()
  (with-kilo-round (url headers body) ("anthropic/claude-opus-4.7" :effort "high")
    (is (equal "https://api.kilo.ai/api/gateway/chat/completions" url))
    (is (equal "Bearer kilo-token" (kilo-header headers "authorization")))
    (is (equal "anthropic/claude-opus-4.7" (nlk:json-value body :string "model")))
    (is (equal "high" (nlk:json-value body :string "reasoning_effort"))
        "a model outside the Qwen class keeps the chat lane's reasoning_effort")
    (is (null (nth-value 1 (gethash "enable_thinking" body))))))

(deftest kilo-cell-asks-a-qwen-model-to-think-the-qwen-way ()
  (with-kilo-round (url headers body) ("qwen/qwen3.7-max" :effort "high")
    (is (eq t (gethash "enable_thinking" body)))
    (is (null (nth-value 1 (gethash "reasoning_effort" body))) "the dialect sends no reasoning_effort"))
  (with-kilo-round (url headers body) ("qwen/qwen3.7-max" :effort "off")
    (is (member (gethash "enable_thinking" body) '(nil :false)))
    (is (nth-value 1 (gethash "enable_thinking" body)) "off is said, not left out"))
  (with-kilo-round (url headers body) ("qwen/qwen3.7-max")
    (is (null (nth-value 1 (gethash "enable_thinking" body))) "no effort: the provider's own default"))
  (with-kilo-round (url headers body) ("prism-ml/ternary-bonsai-2-27b" :effort "high")
    (is (eq t (gethash "enable_thinking" body)) "a Qwen-class model whose id says otherwise")))

(deftest kilo-cell-leaves-other-providers-alone ()
  (with-cell-stop ((kilo-start))
    (let ((nle::*provider* "openai-completions") (nle::*model* "qwen/qwen3.7-max") (nle::*api-key* "k")
          (nle::*endpoint* nil) (nle::*reasoning-effort* "high") (body nil))
      (with-stubbed-fdefinition
          (dex:post (asked &rest args)
           (setf body (nlk:decode-json (getf args :content)))
           (values (make-truncated-sse-stream
                    "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}" "[DONE]")
                   200))
        (nle::call-provider-streaming (user-context)))
      (is (equal "qwen/qwen3.7-max" (nlk:json-value body :string "model")))
      (is (null (nth-value 1 (gethash "enable_thinking" body)))))))
