;;;; cell-test.lisp --- the openrouter cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json, every key variable a stubbed
;;;; NLE::CREDENTIAL-ENV, every wire to OpenRouter a stubbed dex:post or
;;;; dex:get. The sign-in's callback server is real, on a loopback port of
;;;; the test's own, and is dialled from here: nothing leaves the machine.

(in-package #:nodecode.test)

(define-test-slice "openrouter" "OPENROUTER-CELL-" :start nodecode-openrouter:start-cell)

(define-cell-lifecycle-tests "openrouter"
  (:hooks 'nle::models-catalog-table :credential 'nle::request-body 'nle::walk-provider-stream)
  (:command "openrouter")
  (:refused ("base_url" 5) ("variant" "turbo")))

(defun openrouter-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun openrouter-stream ()
  "One whole chat answer, `ok'."
  (make-truncated-sse-stream
   "{\"choices\":[{\"index\":0,\"delta\":{\"content\":\"ok\"}}]}"
   "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}"
   "[DONE]"))

(defmacro with-openrouter-round ((url headers body) (model &key effort max-tokens (provider "openrouter"))
                                 (&rest section) &body forms)
  "FORMS after one chat round of PROVIDER's MODEL at EFFORT, the cell started
with SECTION: URL, HEADERS and BODY (decoded) as dex:post saw them."
  `(with-cell-stop ((openrouter-start ,@section))
     (let ((nle::*provider* ,provider) (nle::*model* ,model) (nle::*api-key* "sk-or-test")
           (nle::*endpoint* nil) (nle::*reasoning-effort* ,effort)
           (nle::*max-completion-tokens* ,max-tokens) (nle::*model-capability-memo* nil)
           (,url nil) (,headers nil) (,body nil))
       (declare (ignorable ,url ,headers ,body))
       (with-stubbed-fdefinition
           (dex:post (asked &rest args)
            (setf ,url asked ,headers (getf args :headers)
                  ,body (nlk:decode-json (getf args :content)))
            (values (openrouter-stream) 200))
         (nle::call-provider-streaming (user-context)))
       ,@forms)))

(defparameter *openrouter-models-dev*
  "{\"openrouter\":{\"id\":\"openrouter\",\"name\":\"OpenRouter\",\"npm\":\"@openrouter/ai-sdk-provider\",\"api\":\"https://openrouter.ai/api/v1\",\"env\":[\"OPENROUTER_API_KEY\"],\"models\":{\"sao10k/l3-lunaris-8b\":{\"id\":\"sao10k/l3-lunaris-8b\",\"name\":\"Lunaris 8B\",\"tool_call\":true,\"limit\":{\"context\":8192,\"output\":4096}}}}}"
  "A models.dev document holding OpenRouter as models.dev publishes it, with a
model omp bundles no row for.")

;;; --- the catalog, the lane, the address ----------------------------------------------

(deftest openrouter-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((openrouter-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "openrouter")))
      (is (equal "OpenRouter" (nlk:json-value row :string "name")))
      (is (equal "https://openrouter.ai/api/v1" (nlk:json-value row :string "api")))
      (is (gethash "openai/gpt-5.5" (nlk:json-value row :object "models")) "omp's bundled models are listed")
      (is (equal "openai-completions" (nle::configured-provider-lane "openrouter")) "the chat lane drives it")
      (is (equal "https://openrouter.ai/api/v1/chat/completions"
                 (nle::lane-endpoint "openrouter" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "openrouter"))
        "a stopped cell leaves the catalog as models.dev made it")))

(deftest openrouter-cell-keeps-what-models-dev-makes-of-it ()
  (with-catalog-fixture (catalog *openrouter-models-dev*)
    (with-temp-auth (auth "{\"api_keys\":{\"openrouter\":{\"provider\":\"openrouter\",\"key\":\"sk-or-saved\"}}}")
      (with-cell-stop ((openrouter-start))
        (let ((row (nlk:json-value (nle::models-catalog-table) :object "openrouter")))
          (is (gethash "sao10k/l3-lunaris-8b" (nlk:json-value row :object "models"))
              "models.dev's own models stay beside omp's")
          (is (gethash "openai/gpt-5.5" (nlk:json-value row :object "models")))
          (is (equal "@openrouter/ai-sdk-provider" (nlk:json-value row :string "npm"))))
        (let ((nle::*provider* "openrouter") (nle::*model* "sao10k/l3-lunaris-8b") (nle::*api-key* nil)
              (nle::*endpoint* nil) (nle::*auth-file-path* auth) (seen-url nil) (seen-headers nil) (seen-body nil))
          (with-stubbed-fdefinition (dex:post (asked &rest args)
                                     (setf seen-url asked seen-headers (getf args :headers)
                                           seen-body (nlk:decode-json (getf args :content)))
                                     (values (openrouter-stream) 200))
            (nle::call-provider-streaming (user-context)))
          (is (equal "https://openrouter.ai/api/v1/chat/completions" seen-url))
          (is (equal "Bearer sk-or-saved" (openrouter-header seen-headers "authorization"))
              "a key /connect saved still answers")
          (is (equal "sao10k/l3-lunaris-8b" (nlk:json-value seen-body :string "model"))))))))

(deftest openrouter-cell-reads-openrouter-api-key ()
  (with-cell-stop ((openrouter-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "OPENROUTER_API_KEY") "sk-or-env"))
        (let ((credential (nle::resolve-provider-credential "openrouter" :auth-path auth :probe t)))
          (is (equal "sk-or-env" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential))))))))

(deftest openrouter-cell-never-sends-another-familys-key ()
  (with-cell-stop ((openrouter-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "openrouter" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential)))
              "the chat family's default variable never reaches OpenRouter")
          (is (eq :public (nle:credential-source credential))))))))

(deftest openrouter-cell-saved-key-outranks-the-variable ()
  (with-cell-stop ((openrouter-start))
    (with-temp-auth (auth "{\"api_keys\":{\"openrouter\":{\"provider\":\"openrouter\",\"key\":\"sk-or-saved\"}}}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "OPENROUTER_API_KEY") "sk-or-env"))
        (is (equal "sk-or-saved"
                   (nle:credential-key (nle::resolve-provider-credential "openrouter" :auth-path auth :probe t))))))))

;;; --- one round -------------------------------------------------------------------------

(deftest openrouter-cell-sends-a-round-as-omp-does ()
  (with-openrouter-round (url headers body) ("openai/gpt-5.5" :effort "high")
      ("variant" "nitro" "only" #("anthropic" "openai") "order" #("openai"))
    (is (equal "https://openrouter.ai/api/v1/chat/completions" url))
    (is (equal "Bearer sk-or-test" (openrouter-header headers "authorization")))
    (is (equal "openai/gpt-5.5:nitro" (nlk:json-value body :string "model")) "the routing variant rides the id")
    (is (equal "high" (nlk:json-value body :string "reasoning" "effort")) "OpenRouter's reasoning object")
    (is (null (nlk:json-value body :string "reasoning_effort")) "never reasoning_effort")
    (is (null (nth-value 1 (gethash "max_tokens" body))) "no output cap the operator did not set")
    (is (equalp #("anthropic" "openai") (nlk:json-value body :array "provider" "only")))
    (is (equalp #("openai") (nlk:json-value body :array "provider" "order")))
    (is (equal "https://nodecode.ai" (openrouter-header headers "HTTP-Referer")))
    (is (equal "Nodecode" (openrouter-header headers "X-OpenRouter-Title")))
    (is (equal "cli-agent" (openrouter-header headers "X-OpenRouter-Categories")))
    (is (equal "true" (openrouter-header headers "X-OpenRouter-Cache")))
    (is (equal "3600" (openrouter-header headers "X-OpenRouter-Cache-TTL")))))

(deftest openrouter-cell-a-named-variant-is-kept ()
  (with-openrouter-round (url headers body) ("deepseek/deepseek-v3.1-terminus:exacto") ("variant" "nitro")
    (is (equal "deepseek/deepseek-v3.1-terminus:exacto" (nlk:json-value body :string "model"))
        "an id that names its variant is sent as it is")
    (is (null (nlk:json-value body :object "provider")) "no routing preferences, no provider field")))

(deftest openrouter-cell-thinking-off-and-an-operator-cap ()
  (with-openrouter-round (url headers body) ("openai/gpt-5.5" :effort "off" :max-tokens 2048) ()
    (is (equal "openai/gpt-5.5" (nlk:json-value body :string "model")) "the default variant adds nothing")
    (multiple-value-bind (enabled present) (nlk:json-value body :any "reasoning" "enabled")
      (is (and present (null enabled)) "off is {\"enabled\": false}"))
    (is (eql 2048 (nlk:json-value body :integer "max_tokens")) "a cap the operator set is sent")))

(deftest openrouter-cell-leaves-other-providers-alone ()
  (with-openrouter-round (url headers body) ("openai/gpt-5.5" :effort "high" :provider "openai-completions")
      ("variant" "nitro")
    (is (equal "openai/gpt-5.5" (nlk:json-value body :string "model")))
    (is (equal "high" (nlk:json-value body :string "reasoning_effort")))
    (is (null (openrouter-header headers "X-OpenRouter-Title")))))

;;; --- the sign-in -------------------------------------------------------------------------

(defun openrouter-notice ()
  "The newest notice said, or NIL."
  (first (first (nlk:notice-log :limit 1))))

(defun openrouter-local-get (port target)
  "The raw HTTP response 127.0.0.1:PORT answers GET TARGET with."
  (let* ((socket (usocket:socket-connect "127.0.0.1" port :element-type '(unsigned-byte 8)))
         (stream (usocket:socket-stream socket)))
    (unwind-protect
         (progn
           (write-sequence (sb-ext:string-to-octets
                            (format nil "GET ~a HTTP/1.1~c~cHost: localhost~c~c~c~c"
                                    target #\Return #\Newline #\Return #\Newline #\Return #\Newline)
                            :external-format :latin-1)
                           stream)
           (force-output stream)
           (let ((octets (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
             (loop for byte = (read-byte stream nil nil) while byte do (vector-push-extend byte octets))
             (sb-ext:octets-to-string (coerce octets '(vector (unsigned-byte 8))) :external-format :utf-8)))
      (usocket:socket-close socket))))

(defun openrouter-login-port (answer)
  "The callback port the login ANSWER sends the browser back to."
  (let* ((start (search "http://localhost:" answer))
         (digits (+ start (length "http://localhost:"))))
    (parse-integer answer :start digits :junk-allowed t)))

(deftest openrouter-cell-signs-in-through-the-callback ()
  (with-cell-stop ((openrouter-start))
    (with-temp-auth (auth "{\"api_keys\":{\"other\":{\"provider\":\"other\",\"key\":\"k\"}}}")
      (let ((nle::*auth-file-path* auth) (posts '())
            (nodecode-openrouter::*callback-port* 0))
        (with-stubbed-fdefinition (dex:post (url &rest args)
                                   (push (list url (getf args :headers) (nlk:decode-json (getf args :content))) posts)
                                   (values "{\"key\":\"sk-or-v1-minted\",\"user_id\":\"u1\"}" 200))
          (let* ((answer (nodecode-openrouter::run-command "login" "s1"))
                 (flow nodecode-openrouter::*flow*)
                 (port (openrouter-login-port answer))
                 (address (subseq answer (search "https://openrouter.ai/auth" answer)
                                  (position #\Newline answer :start (search "https://openrouter.ai/auth" answer)))))
            (is (integerp port) "the answer names the loopback callback")
            (is (equal (format nil "https://openrouter.ai/auth?callback_url=http%3A%2F%2Flocalhost%3A~d%2Fcallback&code_challenge=~a&code_challenge_method=S256"
                               port (nodecode-openrouter::flow-challenge flow))
                       address)
                "OpenRouter's own authorize parameters, no state")
            (is (= 128 (length (nodecode-openrouter::flow-verifier flow))) "96 random bytes, base64url")
            (is (equal (nodecode-openrouter::flow-challenge flow)
                       (nodecode-openrouter::base64url
                        (nodecode-openrouter::sha256-octets (nodecode-openrouter::flow-verifier flow))))
                "the S256 challenge of the verifier")
            (let ((launch (openrouter-local-get port "/launch")))
              (is (uiop:string-prefix-p "HTTP/1.1 302" launch) "/launch redirects")
              (is (search (format nil "Location: ~a" address) launch) "to the sign-in page"))
            (is (uiop:string-prefix-p "HTTP/1.1 404" (openrouter-local-get port "/elsewhere")))
            (let ((page (openrouter-local-get port "/callback?code=the-code")))
              (is (uiop:string-prefix-p "HTTP/1.1 200" page))
              (is (search "you can close this tab" page)))
            (is (await (:timeout 10) (search "signed in" (or (openrouter-notice) ""))) "the outcome is a notice")
            (let ((exchange (first posts)))
              (is (equal "https://openrouter.ai/api/v1/auth/keys" (first exchange)))
              (is (equal "application/json" (openrouter-header (second exchange) "Content-Type")))
              (is (equal "the-code" (nlk:json-value (third exchange) :string "code")))
              (is (equal (nodecode-openrouter::flow-verifier flow)
                         (nlk:json-value (third exchange) :string "code_verifier")))
              (is (equal "S256" (nlk:json-value (third exchange) :string "code_challenge_method"))))
            (let ((stored (nlk:decode-json (uiop:read-file-string auth))))
              (is (equal "sk-or-v1-minted" (nlk:json-value stored :string "api_keys" "openrouter" "key"))
                  "the key lands where /connect saves one")
              (is (equal "openrouter" (nlk:json-value stored :string "api_keys" "openrouter" "provider")))
              (is (equal "k" (nlk:json-value stored :string "api_keys" "other" "key")) "every other key is kept"))
            (is (equal "sk-or-v1-minted"
                       (nle:credential-key (nle::resolve-provider-credential "openrouter" :auth-path auth :probe t))))
            (is (await (:timeout 5) (null nodecode-openrouter::*flow*)) "the sign-in is over")
            (is (await (:timeout 5) (handler-case (progn (openrouter-local-get port "/launch") nil)
                                      (error () t)))
                "and its callback server is closed")
            (is (search "Signed out" (nodecode-openrouter::run-command "logout" "s1")))
            (is (null (nlk:json-value (nlk:decode-json (uiop:read-file-string auth)) :object "api_keys" "openrouter")))))))))

(deftest openrouter-cell-a-pasted-key-is-checked-and-saved ()
  (with-cell-stop ((openrouter-start))
    (with-temp-auth (auth "{}")
      (let ((nle::*auth-file-path* auth) (gets '()) (nodecode-openrouter::*callback-port* 0))
        (with-stubbed-fdefinitions
            ((dex:get (url &rest args) (push (list url (getf args :headers)) gets) (values "{\"data\":{}}" 200))
             (dex:post (url &rest args) (error "a pasted key is not exchanged")))
          (nodecode-openrouter::run-command "login" "s1")
          (is (search "Received" (nodecode-openrouter::run-command "code sk-or-v1-pasted" "s1")))
          (is (await (:timeout 10) (search "signed in" (or (openrouter-notice) ""))))
          (is (equal "https://openrouter.ai/api/v1/auth/key" (first (first gets))) "checked where keys authenticate")
          (is (equal "Bearer sk-or-v1-pasted" (openrouter-header (second (first gets)) "Authorization")))
          (is (equal "sk-or-v1-pasted"
                     (nlk:json-value (nlk:decode-json (uiop:read-file-string auth)) :string "api_keys" "openrouter" "key"))))))))

(deftest openrouter-cell-a-refusal-is-said ()
  (with-cell-stop ((openrouter-start))
    (with-temp-auth (auth "{}")
      (let ((nle::*auth-file-path* auth) (nodecode-openrouter::*callback-port* 0))
        (let ((port (openrouter-login-port (nodecode-openrouter::run-command "login" "s1"))))
          (is (uiop:string-prefix-p "HTTP/1.1 500"
                                    (openrouter-local-get port "/callback?error=access_denied")))
          (is (await (:timeout 10) (search "access_denied" (or (openrouter-notice) ""))))
          (is (search "failed" (openrouter-notice)))
          (is (equal "{}" (uiop:read-file-string auth)) "nothing is kept"))))))

(deftest openrouter-cell-without-a-sign-in-a-paste-says-so ()
  (with-cell-stop ((openrouter-start))
    (is (search "No sign-in" (nodecode-openrouter::run-command "code abc" "s1")))
    (is (search "Usage" (nodecode-openrouter::run-command "frobnicate" "s1")))))
