;;;; cell-test.lisp --- the google-gemini-cli cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json, every exchange with Google a stubbed
;;;; dex:post or dex:get, every stream a canned one. The one socket a test
;;;; opens is the sign-in's own loopback callback, dialled on 127.0.0.1:
;;;; nothing touches the network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "google-gemini-cli" "GOOGLE-GEMINI-CLI-CELL-" :start nodecode-google-gemini-cli:start-cell)

(define-cell-lifecycle-tests "google-gemini-cli"
  (:hooks 'nle::models-catalog-table :credential 'nle::google-request-body 'nle::walk-provider-stream
          'nle::list-provider-models)
  (:command "google-gemini-cli")
  (:running (is (nle::find-lane-by-name "google-gemini-cli" nil) "the lane is registered"))
  (:stopped (is (null (nle::find-lane-by-name "google-gemini-cli" nil)) "and taken back out"))
  (:refused ("base_url" 5)))

;;; --- fixtures -------------------------------------------------------------------

(defun gcli-said (fragment)
  "Whether a notice said lately carries FRAGMENT."
  (some (lambda (entry) (search fragment (first entry))) (nlk:notice-log :limit 50)))

(defun gcli-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun gcli-signed-in (&key (token "ya29.signed") (expires 4000000000) (project "proj-1")
                            (refresh "1//refresh") (email "me@example.com"))
  "An auth.json text holding a Cloud Code Assist sign-in."
  (shasht:write-json
   (nlk:json-object "oauth_tokens"
                    (nlk:json-object "google-gemini-cli"
                                     (nlk:json-object "access_token" token "refresh_token" refresh
                                                      "expires_at" expires "project_id" project
                                                      :opt "email" email)))
   nil))

(defun gcli-event (response)
  "One Cloud Code Assist event around the Gemini chunk RESPONSE (JSON text)."
  (format nil "{\"response\":~a,\"traceId\":\"trace-1\"}" response))

(defparameter +gcli-tool-stream+
  (list (gcli-event "{\"candidates\":[{\"content\":{\"role\":\"model\",\"parts\":[{\"text\":\"weighing it\",\"thought\":true,\"thoughtSignature\":\"c2lnLXRoaW5r\"}]}}],\"responseId\":\"resp-1\"}")
        (gcli-event "{\"candidates\":[{\"content\":{\"role\":\"model\",\"parts\":[{\"text\":\"Reading \"}]}}]}")
        (gcli-event "{\"candidates\":[{\"content\":{\"role\":\"model\",\"parts\":[{\"text\":\"the file.\",\"thoughtSignature\":\"c2lnLXRleHQ=\"}]}}]}")
        (gcli-event "{\"candidates\":[{\"content\":{\"role\":\"model\",\"parts\":[{\"functionCall\":{\"name\":\"eval\",\"args\":{\"form\":\"(+ 1 2)\"}},\"thoughtSignature\":\"c2lnLWNhbGw=\"}]},\"finishReason\":\"STOP\"}],\"usageMetadata\":{\"promptTokenCount\":12,\"candidatesTokenCount\":5,\"thoughtsTokenCount\":3,\"totalTokenCount\":20}}"))
  "A streamed thought, an answer in two chunks, and a call of the eval tool, each signed.")

(defun gcli-lane-round (context)
  "One round of CONTEXT on the lane its frozen config names, the way the turn loop runs it."
  (let ((config (nle::compiled-turn-context-provider-config context)))
    (funcall (nle::provider-lane-stream-symbol
              (nle::find-lane-by-name (nle::effective-provider-config-lane config)))
             context)))

(defmacro with-gcli-round ((message url headers body &key (model "gemini-3.1-pro-preview") effort
                                                          (frames '+gcli-tool-stream+)
                                                          (context '(user-context "hello cloud code")))
                           &body forms)
  "FORMS with the cell started and signed in, and one round of MODEL at
EFFORT streaming FRAMES: MESSAGE the lane's answer, URL, HEADERS and BODY (the
decoded request) as dex:post saw them."
  `(with-cell-stop ((google-gemini-cli-start))
     (with-temp-auth (auth (gcli-signed-in))
       (let ((nle::*provider* "google-gemini-cli") (nle::*model* ,model) (nle::*api-key* nil)
             (nle::*reasoning-effort* ,effort) (nle::*endpoint* nil) (nle::*auth-file-path* auth)
             (,url nil) (,headers nil) (,body nil) (,message nil))
         (declare (ignorable ,url ,headers ,body ,message))
         (with-stubbed-fdefinition
             (dex:post (asked &rest args)
              (setf ,url asked ,headers (getf args :headers)
                    ,body (nlk:decode-json (getf args :content)))
              (values (apply #'make-truncated-sse-stream ,frames) 200))
           (setf ,message (gcli-lane-round ,context)))
         ,@forms))))

(defun gcli-history (&optional answer)
  "A compiled context after one round that called eval, ANSWER (by default a
message no round of this lane made), and its result."
  (compiled-context
   (list (nlk:json-object "role" "user" "content" "hello cloud code")
         (or answer
             (nlk:json-object "role" "assistant" "content" "Reading the file."
                              "reasoning_content" "weighing it"
                              "tool_calls" (vector (nlk:json-object "id" "tool-0" "type" "function"
                                                                    "function" (nlk:json-object "name" "eval"
                                                                                                "arguments" "{\"form\":\"(+ 1 2)\"}")))
                              "thought_signatures" (nlk:json-object "tool-0" "c2lnLWNhbGw=")))
         (nlk:json-object "role" "tool" "tool_call_id" "tool-0" "content" "3"))))

(defun gcli-own-keys (object)
  "The keys of OBJECT that no core lane writes: what a cell would have added."
  (remove-if-not (lambda (key) (search "cca" key)) (alexandria:hash-table-keys object)))

;;; --- the catalog and the lane ---------------------------------------------------------

(deftest google-gemini-cli-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((google-gemini-cli-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "google-gemini-cli")))
      (is (equal "Google Cloud Code Assist (Gemini CLI)" (nlk:json-value row :string "name")))
      (is (equal "https://cloudcode-pa.googleapis.com" (nlk:json-value row :string "api")))
      (is (gethash "gemini-3.1-pro-preview" (nlk:json-value row :object "models")) "the bundled models are listed")
      (is (equal "google-gemini-cli" (nle::configured-provider-lane "google-gemini-cli")) "the cell's own lane drives it")
      (is (equal "https://cloudcode-pa.googleapis.com" (nle::lane-endpoint "google-gemini-cli" "google-gemini-cli"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "google-gemini-cli"))
        "a stopped cell leaves the catalog as models.dev made it")))

(deftest google-gemini-cli-cell-base-follows-the-section ()
  (with-cell-stop ((google-gemini-cli-start "base_url" "https://relay.example"))
    (is (equal "https://relay.example" (nlk:json-value (nle::models-catalog-table) :string "google-gemini-cli" "api")))))

;;; --- the credential --------------------------------------------------------------------

(deftest google-gemini-cli-cell-answers-the-kept-sign-in ()
  (with-cell-stop ((google-gemini-cli-start))
    (with-temp-auth (auth (gcli-signed-in))
      (let ((credential (nle::resolve-provider-credential "google-gemini-cli" :auth-path auth :probe t)))
        (is (equal "ya29.signed" (nle:credential-key credential)))
        (is (eq :oauth (nle:credential-source credential)))
        (is (equal "proj-1" (getf (nle:credential-attributes credential) :project-id)) "the project rides beside it")))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "GOOGLE_API_KEY") "AIza-env"))
        (is (eq :none (nle::provider-auth-state "google-gemini-cli" :auth-path auth))
            "GOOGLE_API_KEY is not a Cloud Code Assist credential")))))

(deftest google-gemini-cli-cell-never-sends-another-familys-key ()
  (with-cell-stop ((google-gemini-cli-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (member name '("GOOGLE_API_KEY" "google-gemini-cli_API_KEY" "OPENAI_API_KEY")
                                              :test #'string-equal)
                                      "AIza-other"))
        (let ((credential (nle::resolve-provider-credential "google-gemini-cli" :auth-path auth :probe t)))
          (is (not (equal "AIza-other" (nle:credential-key credential)))
              "the Google family's default variable never reaches Cloud Code Assist")
          (is (eq :public (nle:credential-source credential))))
        (let ((credential (nle::resolve-provider-credential "google-gemini-cli" :auth-path auth
                                                                   :endpoint "https://example.invalid")))
          (is (eq :public (nle:credential-source credential)) "nor on a round"))))))

(deftest google-gemini-cli-cell-says-a-refresh-that-failed ()
  (with-cell-stop ((google-gemini-cli-start))
    (with-temp-auth (auth (gcli-signed-in :expires (+ (nodecode-google-gemini-cli::unix-now) 10)))
      (with-stubbed-fdefinition (dex:post (url &rest args) (values "{\"error\":\"invalid_grant\"}" 400))
        (is (signals-error nle::credential-error
              (nle::resolve-provider-credential "google-gemini-cli" :auth-path auth :endpoint "https://example.invalid"))))
      (is (search "invalid_grant" (or (second (cell-notice "nodecode-google-gemini-cli")) ""))
          "the failed refresh stands, in Google's words")
      (is (search "/google-gemini-cli login" (or (second (cell-notice "nodecode-google-gemini-cli")) ""))))))

(deftest google-gemini-cli-cell-refreshes-a-sign-in-that-is-due ()
  (with-cell-stop ((google-gemini-cli-start))
    (let ((soon (+ (nodecode-google-gemini-cli::unix-now) 20)) (posted nil))
      (with-temp-auth (auth (gcli-signed-in :expires soon))
        (with-stubbed-fdefinition
            (dex:post (url &rest args)
             (setf posted (list url (getf args :content)))
             (values "{\"access_token\":\"ya29.fresh\",\"expires_in\":3599,\"token_type\":\"Bearer\"}" 200))
          (nle::resolve-provider-credential "google-gemini-cli" :auth-path auth :probe t)
          (is (null posted) "a probe refreshes nothing")
          (let ((credential (nle::resolve-provider-credential
                             "google-gemini-cli" :auth-path auth :endpoint "https://cloudcode-pa.googleapis.com")))
            (is (equal "ya29.fresh" (nle:credential-key credential)))))
        (is (equal "https://oauth2.googleapis.com/token" (first posted)))
        (let ((form (quri:url-decode-params (second posted))))
          (is (equal "refresh_token" (cdr (assoc "grant_type" form :test #'equal))))
          (is (equal "1//refresh" (cdr (assoc "refresh_token" form :test #'equal))))
          (is (uiop:string-prefix-p "681255809395-" (cdr (assoc "client_id" form :test #'equal)))
              "the Gemini CLI's client"))
        (let ((entry (nlk:json-value (nle::read-auth-file auth) :object "oauth_tokens" "google-gemini-cli")))
          (is (equal "ya29.fresh" (nlk:json-value entry :string "access_token")))
          (is (equal "1//refresh" (nlk:json-value entry :string "refresh_token")) "the refresh token kept")
          (is (equal "proj-1" (nlk:json-value entry :string "project_id")) "and the project")
          (is (equal "me@example.com" (nlk:json-value entry :string "email")))
          (is (<= (abs (- (nlk:json-value entry :integer "expires_at")
                          (+ (nodecode-google-gemini-cli::unix-now) 3599 -300)))
                  5)
              "Google's expiry less omp's five minutes"))))))

;;; --- the sign-in ------------------------------------------------------------------------

(defmacro with-google-sign-in ((posts gets &key (load "{\"currentTier\":{\"id\":\"free-tier\"},\"cloudaicompanionProject\":\"proj-1\"}")
                                                 (onboard "{\"done\":true,\"response\":{\"cloudaicompanionProject\":{\"id\":\"proj-new\"}}}")
                                                 (operation "{\"done\":true,\"response\":{\"cloudaicompanionProject\":{\"id\":\"proj-polled\"}}}"))
                               &body body)
  "BODY with Google's token, userinfo and Cloud Code Assist endpoints stubbed;
POSTS and GETS collect (URL HEADERS CONTENT); the loopback callback is dialled for real."
  `(let ((,posts '()) (,gets '()))
     (with-saved-globals ((nodecode-google-gemini-cli::*operation-poll-seconds* 0.01))
       (with-stubbed-fdefinitions
           ((dex:post (url &rest args)
             (push (list url (getf args :headers) (getf args :content)) ,posts)
             (cond ((search "oauth2.googleapis.com/token" url)
                    (values "{\"access_token\":\"ya29.new\",\"refresh_token\":\"1//new\",\"expires_in\":3599,\"token_type\":\"Bearer\"}" 200))
                   ((search ":loadCodeAssist" url) (values ,load 200))
                   ((search ":onboardUser" url) (values ,onboard 200))
                   (t (values "{}" 404))))
            (dex:get (url &rest args)
             (if (search "127.0.0.1" url)
                 (apply original url args)
                 (progn (push (list url (getf args :headers)) ,gets)
                        (cond ((search "userinfo" url) (values "{\"email\":\"me@example.com\"}" 200))
                              ((search "/v1internal/operations/" url) (values ,operation 200))
                              (t (values "{}" 404)))))))
         ,@body))))

(defun gcli-login (auth)
  "Run /google-gemini-cli login against AUTH: (values ANSWER REDIRECT-URI STATE)."
  (let ((answer (let ((nle::*auth-file-path* auth))
                  (cell-entry "nodecode-google-gemini-cli" "google-gemini-cli" "login"))))
    (values answer
            (ppcre:register-groups-bind (uri) ("listens at (http://\\S+)" answer) uri)
            (ppcre:register-groups-bind (state) ("[?&]state=([0-9a-f]+)" answer) state))))

(defun gcli-entry (auth)
  "The sign-in AUTH keeps, or NIL."
  (nlk:json-value (ignore-errors (nle::read-auth-file auth)) :object "oauth_tokens" "google-gemini-cli"))

(deftest google-gemini-cli-cell-signs-in-through-the-loopback-callback ()
  (with-cell-stop ((google-gemini-cli-start))
    (with-temp-auth (auth "{\"api_keys\":{\"other\":{\"provider\":\"other\",\"key\":\"k\"}}}")
      (with-google-sign-in (posts gets)
        (multiple-value-bind (answer redirect state) (gcli-login auth)
          (is (search "https://accounts.google.com/o/oauth2/v2/auth?" answer) "Google's consent page")
          (is (search "access_type=offline" answer))
          (is (search "prompt=consent" answer))
          (is (search "client_id=681255809395-" answer))
          (is (ppcre:scan "^http://127\\.0\\.0\\.1:\\d+/oauth2callback$" (or redirect "")) redirect)
          (is (= 32 (length (or state ""))) "a 16-byte hex state")
          (multiple-value-bind (page status) (dex:get (format nil "~a?state=~a&code=4%2Fcode-1&scope=x" redirect state))
            (is (eql 200 status))
            (is (search "Signed in" page) "the browser is told"))
          (is (await (:timeout 10) (gcli-said "google-gemini-cli: signed in as me@example.com, project proj-1"))
              "the outcome is said once")
          (is (null (cell-notice "nodecode-google-gemini-cli")) "and does not stand")
          (let ((entry (gcli-entry auth)))
            (is (equal "ya29.new" (nlk:json-value entry :string "access_token")))
            (is (equal "1//new" (nlk:json-value entry :string "refresh_token")))
            (is (equal "proj-1" (nlk:json-value entry :string "project_id")))
            (is (equal "me@example.com" (nlk:json-value entry :string "email")))
            (is (<= (abs (- (nlk:json-value entry :integer "expires_at")
                            (+ (nodecode-google-gemini-cli::unix-now) 3599 -300)))
                    5)))
          (is (equal "k" (nlk:json-value (nle::read-auth-file auth) :string "api_keys" "other" "key"))
              "every other field kept")
          (let* ((ordered (reverse posts))
                 (exchange (first ordered))
                 (form (quri:url-decode-params (third exchange)))
                 (load (second ordered)))
            (is (equal "https://oauth2.googleapis.com/token" (first exchange)))
            (is (equal "authorization_code" (cdr (assoc "grant_type" form :test #'equal))))
            (is (equal "4/code-1" (cdr (assoc "code" form :test #'equal))))
            (is (equal redirect (cdr (assoc "redirect_uri" form :test #'equal))) "the redirect the consent page named")
            (is (uiop:string-prefix-p "GOCSPX-" (cdr (assoc "client_secret" form :test #'equal))))
            (is (equal "https://cloudcode-pa.googleapis.com/v1internal:loadCodeAssist" (first load)))
            (is (equal "Bearer ya29.new" (gcli-header (second load) "Authorization")))
            (is (uiop:string-prefix-p "GeminiCLI/0.46.0/" (gcli-header (second load) "User-Agent")))
            (is (equal "GEMINI" (nlk:json-value (nlk:decode-json (third load)) :string "metadata" "pluginType"))))
          (is (equal "https://www.googleapis.com/oauth2/v1/userinfo?alt=json" (first (first gets)))))))))

(deftest google-gemini-cli-cell-provisions-the-free-tier-from-a-pasted-address ()
  (with-cell-stop ((google-gemini-cli-start))
    (with-temp-auth (auth "{}")
      (with-google-sign-in (posts gets :load "{\"allowedTiers\":[{\"id\":\"free-tier\",\"isDefault\":true}]}"
                                       :onboard "{\"name\":\"operations/op-1\",\"done\":false}")
        (multiple-value-bind (answer redirect state) (gcli-login auth)
          (declare (ignore answer))
          (is (search "not for the sign-in"
                      (or (cell-entry "nodecode-google-gemini-cli" "google-gemini-cli"
                                      (format nil "code ~a?state=deadbeef&code=4/x" redirect))
                          ""))
              "an address for another sign-in is refused")
          (is (search "code received"
                      (cell-entry "nodecode-google-gemini-cli" "google-gemini-cli"
                                  (format nil "code ~a?state=~a&code=4%2Fpasted" redirect state))))
          (is (await (:timeout 10) (nlk:json-value (gcli-entry auth) :string "project_id")))
          (is (equal "proj-polled" (nlk:json-value (gcli-entry auth) :string "project_id"))
              "the project the provisioning operation named")
          (let* ((ordered (reverse posts))
                 (onboard (find-if (lambda (post) (search ":onboardUser" (first post))) ordered)))
            (is (equal "4/pasted" (cdr (assoc "code" (quri:url-decode-params (third (first ordered))) :test #'equal))))
            (is (equal "free-tier" (nlk:json-value (nlk:decode-json (third onboard)) :string "tierId"))))
          (is (find "https://cloudcode-pa.googleapis.com/v1internal/operations/op-1" gets :key #'first :test #'equal)
              "the operation polled"))))))

(deftest google-gemini-cli-cell-says-a-sign-in-that-needs-a-project ()
  (with-cell-stop ((google-gemini-cli-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name) nil)
        (with-google-sign-in (posts gets :load "{\"allowedTiers\":[{\"id\":\"standard-tier\",\"isDefault\":true}]}")
          (multiple-value-bind (answer redirect state) (gcli-login auth)
            (declare (ignore answer redirect))
            (cell-entry "nodecode-google-gemini-cli" "google-gemini-cli" (format nil "code 4/x#~a" state))
            (is (await (:timeout 10)
                  (search "GOOGLE_CLOUD_PROJECT" (or (second (cell-notice "nodecode-google-gemini-cli")) "")))
                "a paid tier names its project through the environment")
            (is (null (gcli-entry auth)) "and nothing is kept")))))))

(deftest google-gemini-cli-cell-logout-forgets-the-sign-in ()
  (with-cell-stop ((google-gemini-cli-start))
    (with-temp-auth (auth (gcli-signed-in))
      (let ((nle::*auth-file-path* auth))
        (is (search "project proj-1" (cell-entry "nodecode-google-gemini-cli" "google-gemini-cli" "status")))
        (is (search "signed out" (cell-entry "nodecode-google-gemini-cli" "google-gemini-cli" "logout")))
        (is (null (gcli-entry auth)))))))

;;; --- a round ------------------------------------------------------------------------------

(deftest google-gemini-cli-cell-sends-a-cloud-code-assist-request ()
  (with-gcli-round (message url headers body)
    (is (equal "https://cloudcode-pa.googleapis.com/v1internal:streamGenerateContent?alt=sse" url))
    (is (equal "Bearer ya29.signed" (gcli-header headers "Authorization")))
    (is (null (gcli-header headers "x-goog-api-key")) "no API key header")
    (is (uiop:string-prefix-p "GeminiCLI/0.46.0/gemini-3.1-pro-preview (" (gcli-header headers "User-Agent")))
    (is (equal "ideType=IDE_UNSPECIFIED,platform=PLATFORM_UNSPECIFIED,pluginType=GEMINI"
               (gcli-header headers "Client-Metadata")))
    (is (equal "proj-1" (nlk:json-value body :string "project")) "the account's project")
    (is (equal "gemini-3.1-pro-preview" (nlk:json-value body :string "model")))
    (let ((request (nlk:json-value body :object "request")))
      (is (search "hello cloud code"
                  (nlk:json-value (aref (nlk:json-value (aref (nlk:json-array request "contents") 0) :array "parts") 0)
                                  :string "text")))
      (is (equal "user" (nlk:json-value (aref (nlk:json-array request "contents") 0) :string "role")))
      (is (plusp (length (nlk:json-value (aref (nlk:json-array request "systemInstruction" "parts") 0) :string "text")))
          "the system prompt as an instruction")
      (is (equal "LOW" (nlk:json-value request :string "generationConfig" "thinkingConfig" "thinkingLevel"))
          "a model that must think runs at its own lowest level")
      (is (eq t (nlk:json-value request :boolean "generationConfig" "thinkingConfig" "includeThoughts")))
      (let ((declaration (aref (nlk:json-array (aref (nlk:json-array request "tools") 0) "functionDeclarations") 0)))
        (is (equal "eval" (nlk:json-value declaration :string "name")))
        (is (nlk:json-value declaration :object "parametersJsonSchema") "Gemini reads the JSON-Schema field")
        (is (null (nlk:json-value declaration :any "parametersJsonSchema" "properties" "strings" "additionalProperties"))
            "with what Google has no field for left out")))))

(deftest google-gemini-cli-cell-folds-the-stream-into-text-and-a-call ()
  (with-gcli-round (message url headers body)
    (is (equal "Reading the file." (nlk:json-value message :string "content")))
    (is (equal "weighing it" (nlk:json-value message :string "reasoning_content")))
    (let ((call (aref (nlk:json-array message "tool_calls") 0)))
      (is (equal "eval" (nlk:json-value call :string "function" "name")))
      (is (equal "(+ 1 2)" (nlk:json-value (nlk:decode-json (nlk:json-value call :string "function" "arguments"))
                                           :string "form"))))
    (is (null (gcli-own-keys message)) "the message carries only the core's fields")
    (let ((signed (nodecode-google-gemini-cli::recall-signatures message)))
      (is (equal "c2lnLXRoaW5r" (getf signed :thinking)) "the thought's signature kept by the cell")
      (is (equal "c2lnLXRleHQ=" (getf signed :text)) "the answer's")
      (is (equal "google-gemini-cli/gemini-3.1-pro-preview" (getf signed :model)) "and the model that made them"))
    (is (equal '("c2lnLWNhbGw=") (alexandria:hash-table-values (nlk:json-value message :object "thought_signatures")))
        "the call's own, by the fold")))

(deftest google-gemini-cli-cell-replays-what-its-model-signed ()
  (let ((answer nil))
    (with-gcli-round (message url headers body) (setf answer message))
    (with-gcli-round (message url headers body :context (gcli-history answer))
    (let* ((contents (nlk:json-array body "request" "contents"))
           (model (find "model" contents :key (lambda (content) (nlk:json-value content :string "role")) :test #'equal))
           (parts (nlk:json-array model "parts"))
           (result (aref contents (1- (length contents)))))
      (is (eq t (nlk:json-value (aref parts 0) :boolean "thought")) "the thought, as a thought")
      (is (equal "c2lnLXRoaW5r" (nlk:json-value (aref parts 0) :string "thoughtSignature")))
      (is (equal "Reading the file." (nlk:json-value (aref parts 1) :string "text")))
      (is (equal "c2lnLXRleHQ=" (nlk:json-value (aref parts 1) :string "thoughtSignature")))
      (is (equal "eval" (nlk:json-value (aref parts 2) :string "functionCall" "name")))
      (is (equal "(+ 1 2)" (nlk:json-value (aref parts 2) :string "functionCall" "args" "form")))
      (is (equal "c2lnLWNhbGw=" (nlk:json-value (aref parts 2) :string "thoughtSignature")))
      (is (null (nlk:json-value (aref parts 2) :string "functionCall" "id")) "Gemini takes no call ids")
      (is (equal "user" (nlk:json-value result :string "role")))
      (is (equal "eval" (nlk:json-value (aref (nlk:json-array result "parts") 0) :string "functionResponse" "name")))
      (is (equal "3" (nlk:json-value (aref (nlk:json-array result "parts") 0) :string "functionResponse" "response" "output")))))))

(deftest google-gemini-cli-cell-replays-no-signature-to-another-model ()
  (let ((answer nil))
    (with-gcli-round (message url headers body) (setf answer message))
    (with-gcli-round (message url headers body :model "gemini-3-flash-preview" :context (gcli-history answer))
    (let* ((contents (nlk:json-array body "request" "contents"))
           (model (find "model" contents :key (lambda (content) (nlk:json-value content :string "role")) :test #'equal))
           (parts (nlk:json-array model "parts")))
      (is (equal (format nil "```thinking~%weighing it~%```") (nlk:json-value (aref parts 0) :string "text"))
          "another model's thought, as text")
      (is (null (nlk:json-value (aref parts 1) :string "thoughtSignature")))
      (is (equal "skip_thought_signature_validator" (nlk:json-value (aref parts 2) :string "thoughtSignature"))
          "Gemini 3's unsigned first call carries the bypass")))))

(deftest google-gemini-cli-cell-replays-no-signature-of-a-message-it-did-not-make ()
  (with-gcli-round (message url headers body :context (gcli-history))
    (let* ((contents (nlk:json-array body "request" "contents"))
           (model (find "model" contents :key (lambda (content) (nlk:json-value content :string "role")) :test #'equal))
           (parts (nlk:json-array model "parts")))
      (is (null (nlk:json-value (aref parts 0) :boolean "thought")) "the thought, as text")
      (is (equal "skip_thought_signature_validator" (nlk:json-value (aref parts 2) :string "thoughtSignature"))
          "the signature the fold kept is not replayed; the first call carries the bypass"))))

(deftest google-gemini-cli-cell-adds-no-field-another-lane-would-send ()
  (with-gcli-round (message url headers body)
    (let ((nle::*provider* "openai-completions") (nle::*model* "gpt-4.1") (nle::*api-key* "sk-test")
          (nle::*endpoint* nil) (sent nil))
      (with-stubbed-fdefinition
          (dex:post (asked &rest args)
           (setf sent (nlk:decode-json (getf args :content)))
           (values (make-truncated-sse-stream
                    "{\"choices\":[{\"index\":0,\"delta\":{\"content\":\"ok\"}}]}"
                    "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}"
                    "[DONE]")
                   200))
        (nle::call-provider-streaming (gcli-history message)))
      (let ((assistant (find "assistant" (nlk:json-array sent "messages")
                             :key (lambda (wire) (nlk:json-value wire :string "role")) :test #'equal)))
        (is assistant "the chat lane sends the round's answer")
        (is (equal "Reading the file." (nlk:json-value assistant :string "content")))
        (is (null (gcli-own-keys assistant)) "and no field this cell added")))))

(deftest google-gemini-cli-cell-spends-a-budget-model-s-effort ()
  (with-gcli-round (message url headers body :model "gemini-2.5-flash" :effort "high")
    (is (equal "gemini-2.5-flash-thinking" (nlk:json-value body :string "model")) "the effort's routed model")
    (is (= 16384 (nlk:json-value body :integer "request" "generationConfig" "thinkingConfig" "thinkingBudget")))
    (is (<= (nlk:json-value body :integer "request" "generationConfig" "maxOutputTokens") 65536)))
  (with-gcli-round (message url headers body :model "gemini-2.5-flash" :effort "off")
    (is (equal "gemini-2.5-flash" (nlk:json-value body :string "model")) "off routes to the plain model")
    (is (null (nlk:json-value body :object "request" "generationConfig" "thinkingConfig")))))

(deftest google-gemini-cli-cell-strips-a-flash-model-s-leaked-plan ()
  (with-gcli-round (message url headers body
                    :model "gemini-3-flash-preview"
                    :frames (list (gcli-event "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"{\\\"thou\"}]}}]}")
                                  (gcli-event "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"ght\\\": \\\"call eval next\\\"} The answer is 3.\"}]},\"finishReason\":\"STOP\"}]}")))
    (is (equal " The answer is 3." (nlk:json-value message :string "content")) "the plan held back and dropped")))

(deftest google-gemini-cli-cell-says-an-in-band-error ()
  (let ((refusal (signals-error nle::provider-error
                   (with-gcli-round (message url headers body
                                     :frames (list (gcli-event "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"par\"}]}}]}")
                                                   "{\"error\":{\"code\":429,\"message\":\"Resource has been exhausted\",\"status\":\"RESOURCE_EXHAUSTED\"}}"))))))
    (when (typep refusal 'nle::provider-error)
      (is (eql 429 (nle::provider-error-status refusal)) "the code as the status the retry policy reads")
      (is (search "Resource has been exhausted" (nle::provider-error-detail refusal))))))

(deftest google-gemini-cli-cell-refuses-a-stream-without-a-finish ()
  (is (signals-error nle::provider-stream-incomplete
        (with-gcli-round (message url headers body
                          :frames (list (gcli-event "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"half an\"}]}}]}")))))))

(deftest google-gemini-cli-cell-says-it-is-not-signed-in ()
  (with-cell-stop ((google-gemini-cli-start))
    (with-temp-auth (auth "{}")
      (let ((nle::*provider* "google-gemini-cli") (nle::*model* "gemini-2.5-pro") (nle::*api-key* nil)
            (nle::*endpoint* nil) (nle::*auth-file-path* auth) (posted nil))
        (with-stubbed-fdefinition (dex:post (url &rest args) (setf posted t) (values (make-truncated-sse-stream) 200))
          (let ((refusal (signals-error nle::provider-config-error (gcli-lane-round (user-context)))))
            (when (typep refusal 'nle::provider-error)
              (is (search "/google-gemini-cli login" (nle::provider-error-detail refusal))))))
        (is (null posted) "nothing is sent")))))

(deftest google-gemini-cli-cell-leaves-the-gemini-api-alone ()
  (with-cell-stop ((google-gemini-cli-start))
    (let ((nle::*provider* "google") (nle::*model* "gemini-2.5-pro") (nle::*api-key* "AIza-test")
          (nle::*endpoint* nil) (url nil) (headers nil) (body nil))
      (with-stubbed-fdefinition
          (dex:post (asked &rest args)
           (setf url asked headers (getf args :headers) body (nlk:decode-json (getf args :content)))
           (values (make-truncated-sse-stream
                    "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"ok\"}]},\"finishReason\":\"STOP\"}]}")
                   200))
        (nle::call-google-streaming (user-context)))
      (is (search "generativelanguage.googleapis.com" url))
      (is (equal "AIza-test" (gcli-header headers "x-goog-api-key")))
      (is (nlk:json-value body :array "contents") "the Gemini API's own body")
      (is (null (nlk:json-value body :string "project"))))))

(deftest google-gemini-cli-cell-lists-its-bundled-models-asking-no-one ()
  (with-cell-stop ((google-gemini-cli-start))
    (with-stubbed-fdefinitions ((dex:get (url &rest args) (error "no listing may be fetched: ~a" url))
                                (dex:post (url &rest args) (error "no listing may be fetched: ~a" url)))
      (multiple-value-bind (rows error) (nle::list-provider-models "google-gemini-cli")
        (is (null error) error)
        (is (= (count-if #'nodecode-google-gemini-cli::listed-p
                         (map 'list (lambda (row) (nlk:json-value row :string "id")) nodecode-google-gemini-cli::+models+))
               (length rows)))
        (is (plusp (length rows)))))))

(deftest google-gemini-cli-cell-checks-no-key-it-cannot-ask-about ()
  (with-cell-stop ((google-gemini-cli-start))
    (with-stubbed-fdefinitions ((dex:get (url &rest args) (error "no key check may dial: ~a" url))
                                (dex:post (url &rest args) (error "no key check may dial: ~a" url)))
      (multiple-value-bind (rows reason) (nle::list-provider-models "google-gemini-cli" :key "AIza-pasted")
        (is (plusp (length rows)) "the rows still")
        (is (search "/google-gemini-cli login" (or reason ""))
            "with why the key was not checked, never a NIL that /connect reads as a working key")))))
