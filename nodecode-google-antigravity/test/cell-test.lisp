;;;; cell-test.lisp --- the google-antigravity cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json, every exchange with Google (and the
;;;; update manifest) a stubbed dex:post or dex:get, every stream a canned
;;;; one. The one socket a test opens is the sign-in's own loopback callback,
;;;; dialled on 127.0.0.1: nothing touches the network, the environment or
;;;; the operator's files.

(in-package #:nodecode.test)

(define-test-slice "google-antigravity" "GOOGLE-ANTIGRAVITY-CELL-" :start nodecode-google-antigravity:start-cell)

(define-cell-lifecycle-tests "google-antigravity"
  (:hooks 'nle::models-catalog-table :credential 'nle::google-request-body 'nle::walk-provider-stream
          'nle::list-provider-models)
  (:command "google-antigravity")
  (:running (is (nle::find-lane-by-name "google-antigravity" nil) "the lane is registered"))
  (:stopped (is (null (nle::find-lane-by-name "google-antigravity" nil)) "and taken back out"))
  (:refused ("base_url" 5) ("endpoint_mode" "nightly")))

;;; --- fixtures -------------------------------------------------------------------

(defun ag-said (fragment)
  "Whether a notice said lately carries FRAGMENT."
  (some (lambda (entry) (search fragment (first entry))) (nlk:notice-log :limit 50)))

(defun ag-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun ag-signed-in (&key (token "ya29.ag") (expires 4000000000) (project "ag-proj"))
  "An auth.json text holding an Antigravity sign-in."
  (shasht:write-json
   (nlk:json-object "oauth_tokens"
                    (nlk:json-object "google-antigravity"
                                     (nlk:json-object "access_token" token "refresh_token" "1//ag"
                                                      "expires_at" expires "project_id" project
                                                      "email" "me@example.com")))
   nil))

(defun ag-event (response)
  "One Cloud Code Assist event around the Gemini chunk RESPONSE (JSON text)."
  (format nil "{\"response\":~a,\"traceId\":\"trace-ag\"}" response))

(defparameter +ag-claude-stream+
  (list (ag-event "{\"candidates\":[{\"content\":{\"role\":\"model\",\"parts\":[{\"text\":\"Let me add.\",\"thought\":true}]}}],\"responseId\":\"exec-1\"}")
        (ag-event "{\"candidates\":[{\"content\":{\"role\":\"model\",\"parts\":[{\"text\":\"\",\"thoughtSignature\":\"Y2xhdWRlLXNpZw==\"}]}}]}")
        (ag-event "{\"candidates\":[{\"content\":{\"role\":\"model\",\"parts\":[{\"functionCall\":{\"name\":\"eval\",\"args\":{\"form\":\"(+ 1 2)\"},\"id\":\"toolu_01\"}}]},\"finishReason\":\"STOP\"}],\"usageMetadata\":{\"candidatesTokenCount\":9,\"totalTokenCount\":40}}"))
  "A Claude round through Antigravity: a thought signed by an empty part, then a call.")

(defun ag-lane-round (context)
  "One round of CONTEXT on the lane its frozen config names, the way the turn loop runs it."
  (let ((config (nle::compiled-turn-context-provider-config context)))
    (funcall (nle::provider-lane-stream-symbol
              (nle::find-lane-by-name (nle::effective-provider-config-lane config)))
             context)))

(defmacro with-ag-round ((message posts &key (model "claude-sonnet-4-5") effort choice config
                                             (frames '+ag-claude-stream+) (context '(user-context "add one and two"))
                                             (answer '(lambda (url) (declare (ignore url)) nil)))
                         &body forms)
  "FORMS with the cell started on CONFIG (key/value pairs) and signed in, the
update manifest naming 2.20.0, and one round of MODEL at EFFORT streaming
FRAMES: MESSAGE the lane's answer, POSTS each (URL HEADERS BODY) dex:post saw,
newest first. ANSWER, given a URL, may answer (values BODY STATUS) instead."
  `(with-cell-stop ((google-antigravity-start ,@config))
     (with-temp-auth (auth (ag-signed-in))
       (with-saved-globals ((nodecode-google-antigravity::*discovered-version* nil)
                            (nodecode-google-antigravity::*manifest-failed-at* nil))
         (let ((nle::*provider* "google-antigravity") (nle::*model* ,model) (nle::*api-key* nil)
               (nle::*reasoning-effort* ,effort) (nle::*tool-choice* ,choice) (nle::*endpoint* nil)
               (nle::*auth-file-path* auth) (,posts '()) (,message nil))
           (declare (ignorable ,posts ,message))
           (with-stubbed-fdefinitions
               ((dex:get (url &rest args)
                 (if (search "manifest" url)
                     (values (format nil "path: Antigravity.zip~%version: 2.20.0~%sha512: x~%") 200)
                     (values "{}" 404)))
                (dex:post (asked &rest args)
                 (push (list asked (getf args :headers) (nlk:decode-json (getf args :content))) ,posts)
                 (multiple-value-bind (body status) (funcall ,answer asked)
                   (if status
                       (values body status)
                       (values (apply #'make-truncated-sse-stream ,frames) 200)))))
             (setf ,message (ag-lane-round ,context)))
           ,@forms)))))

(defun ag-body (posts)
  "The decoded body of the newest of POSTS."
  (third (first posts)))

;;; --- the catalog and the lane ---------------------------------------------------------

(deftest google-antigravity-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((google-antigravity-start))
    (let* ((row (nlk:json-value (nle::models-catalog-table) :object "google-antigravity"))
           (models (nlk:json-value row :object "models")))
      (is (equal "Antigravity (Gemini 3, Claude, GPT-OSS)" (nlk:json-value row :string "name")))
      (is (equal "https://daily-cloudcode-pa.googleapis.com" (nlk:json-value row :string "api")))
      (is (gethash "claude-sonnet-4-5" models) "Claude is listed")
      (is (gethash "gpt-oss-120b" models) "and GPT-OSS")
      (is (not (nle::catalog-model-tool-call-p (gethash "gemini-3-pro-image" models)))
          "an image model is no chat model")
      (is (equal "google-antigravity" (nle::configured-provider-lane "google-antigravity")) "the cell's own lane drives it"))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "google-antigravity")))))

;;; --- the credential --------------------------------------------------------------------

(deftest google-antigravity-cell-answers-the-kept-sign-in ()
  (with-cell-stop ((google-antigravity-start))
    (with-temp-auth (auth (ag-signed-in))
      (let ((credential (nle::resolve-provider-credential "google-antigravity" :auth-path auth :probe t)))
        (is (equal "ya29.ag" (nle:credential-key credential)))
        (is (eq :oauth (nle:credential-source credential)))
        (is (equal "ag-proj" (getf (nle:credential-attributes credential) :project-id)))))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "GOOGLE_API_KEY") "AIza-env"))
        (is (eq :none (nle::provider-auth-state "google-antigravity" :auth-path auth))
            "GOOGLE_API_KEY is not an Antigravity credential")))))

(deftest google-antigravity-cell-never-sends-another-familys-key ()
  (with-cell-stop ((google-antigravity-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (member name '("GOOGLE_API_KEY" "google-antigravity_API_KEY" "OPENAI_API_KEY")
                                              :test #'string-equal)
                                      "AIza-other"))
        (let ((credential (nle::resolve-provider-credential "google-antigravity" :auth-path auth :probe t)))
          (is (not (equal "AIza-other" (nle:credential-key credential)))
              "the Google family's default variable never reaches Cloud Code Assist")
          (is (eq :public (nle:credential-source credential))))
        (let ((credential (nle::resolve-provider-credential "google-antigravity" :auth-path auth
                                                                   :endpoint "https://example.invalid")))
          (is (eq :public (nle:credential-source credential)) "nor on a round"))))))

(deftest google-antigravity-cell-says-a-refresh-that-failed ()
  (with-cell-stop ((google-antigravity-start))
    (with-temp-auth (auth (ag-signed-in :expires (+ (nodecode-google-antigravity::unix-now) 10)))
      (with-stubbed-fdefinition (dex:post (url &rest args) (values "{\"error\":\"invalid_grant\"}" 400))
        (is (signals-error nle::credential-error
              (nle::resolve-provider-credential "google-antigravity" :auth-path auth :endpoint "https://example.invalid"))))
      (is (search "invalid_grant" (or (second (cell-notice "nodecode-google-antigravity")) ""))
          "the failed refresh stands, in Google's words")
      (is (search "/google-antigravity login" (or (second (cell-notice "nodecode-google-antigravity")) ""))))))

(deftest google-antigravity-cell-refreshes-with-its-own-client ()
  (with-cell-stop ((google-antigravity-start))
    (let ((posted nil))
      (with-temp-auth (auth (ag-signed-in :expires (+ (nodecode-google-antigravity::unix-now) 10)))
        (with-stubbed-fdefinition
            (dex:post (url &rest args)
             (setf posted (getf args :content))
             (values "{\"access_token\":\"ya29.fresh\",\"refresh_token\":\"1//rotated\",\"expires_in\":3599}" 200))
          (is (equal "ya29.fresh"
                     (nle:credential-key (nle::resolve-provider-credential
                                          "google-antigravity" :auth-path auth
                                                               :endpoint "https://daily-cloudcode-pa.googleapis.com")))))
        (is (uiop:string-prefix-p "1071006060591-"
                                  (cdr (assoc "client_id" (quri:url-decode-params posted) :test #'equal)))
            "Antigravity's client, not the Gemini CLI's")
        (let ((entry (nlk:json-value (nle::read-auth-file auth) :object "oauth_tokens" "google-antigravity")))
          (is (equal "1//rotated" (nlk:json-value entry :string "refresh_token")) "a rotated refresh token kept")
          (is (equal "ag-proj" (nlk:json-value entry :string "project_id"))))))))

;;; --- the sign-in ------------------------------------------------------------------------

(defmacro with-ag-sign-in ((posts gets &key (loads ''("{\"currentTier\":{\"id\":\"free-tier\"},\"allowedTiers\":[{\"id\":\"free-tier\"}],\"cloudaicompanionProject\":\"ag-proj\"}"))
                                            (onboard "{\"name\":\"operations/op-9\",\"done\":false}")
                                            (operation "{\"name\":\"operations/op-9\",\"done\":true,\"response\":{\"@type\":\"t\",\"cloudaicompanionProject\":\"ag-new\"}}"))
                           &body body)
  "BODY with Google's token and userinfo endpoints and Antigravity's control
endpoints stubbed: loadCodeAssist answers LOADS in turn (the last one again
once they run out); POSTS and GETS collect (URL HEADERS CONTENT)."
  `(let ((,posts '()) (,gets '()) (loads ,loads))
     (with-saved-globals ((nodecode-google-antigravity::*operation-poll-seconds* 0.01))
       (with-stubbed-fdefinitions
           ((dex:post (url &rest args)
             (push (list url (getf args :headers) (getf args :content)) ,posts)
             (cond ((search "oauth2.googleapis.com/token" url)
                    (values "{\"access_token\":\"ya29.new\",\"refresh_token\":\"1//new\",\"expires_in\":3599}" 200))
                   ((search ":loadCodeAssist" url)
                    (values (if (rest loads) (pop loads) (first loads)) 200))
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

(defun ag-login (auth)
  "Run /google-antigravity login against AUTH: (values ANSWER REDIRECT-URI STATE)."
  (let ((answer (let ((nle::*auth-file-path* auth))
                  (cell-entry "nodecode-google-antigravity" "google-antigravity" "login"))))
    (values answer
            (ppcre:register-groups-bind (uri) ("listens at (http://\\S+)" answer) uri)
            (ppcre:register-groups-bind (state) ("[?&]state=([0-9a-f]+)" answer) state))))

(defun ag-entry (auth)
  "The sign-in AUTH keeps, or NIL."
  (nlk:json-value (ignore-errors (nle::read-auth-file auth)) :object "oauth_tokens" "google-antigravity"))

(deftest google-antigravity-cell-signs-in-through-the-loopback-callback ()
  (with-cell-stop ((google-antigravity-start))
    (with-temp-auth (auth "{}")
      (with-ag-sign-in (posts gets)
        (multiple-value-bind (answer redirect state) (ag-login auth)
          (is (search "client_id=1071006060591-" answer) "Antigravity's own client")
          (is (search "experimentsandconfigs" answer) "and its scopes")
          (is (ppcre:scan "^http://127\\.0\\.0\\.1:\\d+/oauth-callback$" (or redirect "")) redirect)
          (multiple-value-bind (page status) (dex:get (format nil "~a?code=4%2Fag&state=~a" redirect state))
            (is (eql 200 status))
            (is (search "Signed in" page)))
          (is (await (:timeout 10) (ag-said "google-antigravity: signed in as me@example.com, project ag-proj"))
              "the outcome is said once")
          (is (null (cell-notice "nodecode-google-antigravity")) "and does not stand")
          (is (equal "ag-proj" (nlk:json-value (ag-entry auth) :string "project_id")))
          (is (equal "ya29.new" (nlk:json-value (ag-entry auth) :string "access_token")))
          (let ((loads (remove-if-not (lambda (post) (search ":loadCodeAssist" (first post))) (reverse posts))))
            (is (= 4 (length loads)) "asked twice, the second time with the project, and both once more after")
            (is (equal "https://daily-cloudcode-pa.googleapis.com/v1internal:loadCodeAssist" (first (first loads))))
            (is (uiop:string-prefix-p "antigravity/hub/" (ag-header (second (first loads)) "User-Agent")))
            (is (equal "ANTIGRAVITY" (nlk:json-value (nlk:decode-json (third (first loads))) :string "metadata" "ideType")))
            (is (null (nlk:json-value (nlk:decode-json (third (first loads))) :string "cloudaicompanionProject")))
            (is (equal "ag-proj" (nlk:json-value (nlk:decode-json (third (second loads))) :string "cloudaicompanionProject"))
                "the second ask names the project the first one named"))
          (is (null (find-if (lambda (post) (search ":onboardUser" (first post))) posts)) "a tiered account is not onboarded"))))))

(deftest google-antigravity-cell-onboards-the-free-tier ()
  (with-cell-stop ((google-antigravity-start))
    (with-temp-auth (auth "{}")
      (with-ag-sign-in (posts gets :loads '("{\"allowedTiers\":[{\"id\":\"free-tier\"}],\"paidTier\":null}"
                                            "{\"currentTier\":{\"id\":\"free-tier\"},\"paidTier\":{\"id\":\"p\"},\"cloudaicompanionProject\":\"ag-new\"}"))
        (multiple-value-bind (answer redirect state) (ag-login auth)
          (declare (ignore answer redirect))
          (is (search "code received"
                      (cell-entry "nodecode-google-antigravity" "google-antigravity" (format nil "code 4/ag#~a" state))))
          (is (await (:timeout 10) (nlk:json-value (ag-entry auth) :string "project_id")))
          (is (equal "ag-new" (nlk:json-value (ag-entry auth) :string "project_id")))
          (let ((onboard (find-if (lambda (post) (search ":onboardUser" (first post))) posts)))
            (is (equal "free-tier" (nlk:json-value (nlk:decode-json (third onboard)) :string "tierId")))
            (is (equal "ANTIGRAVITY" (nlk:json-value (nlk:decode-json (third onboard)) :string "metadata" "ideType"))))
          (is (find "https://daily-cloudcode-pa.googleapis.com/v1internal/operations/op-9" gets
                    :key #'first :test #'equal)
              "the operation polled"))))))

(deftest google-antigravity-cell-says-an-ineligible-account ()
  (with-cell-stop ((google-antigravity-start))
    (with-temp-auth (auth "{}")
      (with-ag-sign-in (posts gets :loads '("{\"ineligibleTiers\":[{\"tierId\":\"free-tier\",\"reasonMessage\":\"Not available for this account\",\"validationUrl\":\"https://verify.example/x\"}]}"))
        (multiple-value-bind (answer redirect state) (ag-login auth)
          (declare (ignore answer redirect))
          (cell-entry "nodecode-google-antigravity" "google-antigravity" (format nil "code 4/ag#~a" state))
          (is (await (:timeout 10)
                (let ((said (or (second (cell-notice "nodecode-google-antigravity")) "")))
                  (and (search "Not available for this account" said) (search "https://verify.example/x" said))))
              "Google's own reason and where to verify")
          (is (null (ag-entry auth))))))))

(deftest google-antigravity-cell-logout-forgets-the-sign-in ()
  (with-cell-stop ((google-antigravity-start))
    (with-temp-auth (auth (ag-signed-in))
      (let ((nle::*auth-file-path* auth))
        (is (search "project ag-proj" (cell-entry "nodecode-google-antigravity" "google-antigravity" "status")))
        (is (search "signed out" (cell-entry "nodecode-google-antigravity" "google-antigravity" "logout")))
        (is (null (ag-entry auth)))))))

;;; --- a round ------------------------------------------------------------------------------

(deftest google-antigravity-cell-sends-a-claude-round-in-antigravity-s-envelope ()
  (with-ag-round (message posts)
    (destructuring-bind (url headers body) (first posts)
      (is (equal "https://daily-cloudcode-pa.googleapis.com/v1internal:streamGenerateContent?alt=sse" url))
      (is (equal "Bearer ya29.ag" (ag-header headers "Authorization")))
      (is (equal "antigravity/hub/2.20.0 (aidev_client; os_type=darwin; arch=arm64; cl=963137146)"
                 (ag-header headers "User-Agent"))
          "the version the update manifest names")
      (is (equal "interleaved-thinking-2025-05-14" (ag-header headers "anthropic-beta")) "a thinking Claude's beta")
      (is (equal "ag-proj" (nlk:json-value body :string "project")))
      (is (equal "claude-sonnet-4-5" (nlk:json-value body :string "model")))
      (is (equal "antigravity" (nlk:json-value body :string "userAgent")))
      (is (equal "agent" (nlk:json-value body :string "requestType")))
      (is (ppcre:scan "^agent/[0-9a-f-]{36}/\\d+/[0-9a-f-]{36}/2$" (nlk:json-value body :string "requestId"))
          (nlk:json-value body :string "requestId"))
      (let ((request (nlk:json-value body :object "request")))
        (is (equal (nodecode-google-antigravity::derived-session-id "add one and two")
                   (nlk:json-value request :string "sessionId"))
            "outside a session, the id the first user text hashes to")
        (is (equal "user" (nlk:json-value request :string "systemInstruction" "role")) "the instruction as the user's")
        (is (equal "VALIDATED" (nlk:json-value request :string "toolConfig" "functionCallingConfig" "mode")))
        (is (equal "1" (nlk:json-value request :string "labels" "last_step_index")))
        (is (equal "true" (nlk:json-value request :string "labels" "used_claude")))
        (is (equal "true" (nlk:json-value request :string "labels" "used_claude_conservative")))
        (is (equal (fourth (uiop:split-string (nlk:json-value body :string "requestId") :separator "/"))
                   (nlk:json-value request :string "labels" "trajectory_id"))
            "the trajectory the request id names")
        (let ((declaration (aref (nlk:json-array (aref (nlk:json-array request "tools") 0) "functionDeclarations") 0)))
          (is (nlk:json-value declaration :object "parameters") "the legacy field, as Antigravity reads it")
          (is (null (gethash "parametersJsonSchema" declaration)))
          (is (null (nlk:json-value declaration :any "parameters" "properties" "strings" "additionalProperties"))))))
    (is (equal "Let me add." (nlk:json-value message :string "reasoning_content")))
    (is (null (remove-if-not (lambda (key) (search "cca" key)) (alexandria:hash-table-keys message)))
        "the message carries only the core's fields")
    (is (equal "Y2xhdWRlLXNpZw==" (getf (nodecode-google-antigravity::recall-signatures message) :thinking))
        "a signature on an empty part signs the thought before it")
    (is (equal "eval" (nlk:json-value (aref (nlk:json-array message "tool_calls") 0) :string "function" "name")))))

(defun ag-twice-history ()
  "A compiled context in which two rounds' calls reused one id, as the Gemini fold mints them."
  (flet ((call () (vector (nlk:json-object "id" "tool-0" "type" "function"
                                           "function" (nlk:json-object "name" "eval" "arguments" "{}")))))
    (compiled-context
     (list (nlk:json-object "role" "user" "content" "add twice")
           (nlk:json-object "role" "assistant" "content" :null "reasoning_content" "first" "tool_calls" (call))
           (nlk:json-object "role" "tool" "tool_call_id" "tool-0" "content" "1")
           (nlk:json-object "role" "assistant" "content" :null "tool_calls" (call))
           (nlk:json-object "role" "tool" "tool_call_id" "tool-0" "content" "2")))))

(deftest google-antigravity-cell-gives-claude-unique-call-ids ()
  (with-ag-round (message posts :context (ag-twice-history))
    (let* ((contents (nlk:json-array (ag-body posts) "request" "contents"))
           (parts (loop for content across contents nconc (coerce (nlk:json-array content "parts") 'list)))
           (calls (remove-if-not (lambda (part) (gethash "functionCall" part)) parts))
           (results (remove-if-not (lambda (part) (gethash "functionResponse" part)) parts)))
      (is (equal '("tool-0" "tool-0_2") (mapcar (lambda (part) (nlk:json-value part :string "functionCall" "id")) calls))
          "a reused id is made unique within the request")
      (is (equal '("tool-0" "tool-0_2") (mapcar (lambda (part) (nlk:json-value part :string "functionResponse" "id")) results))
          "and each result answers its own call")
      (is (notany (lambda (part) (equal "first" (nlk:json-value part :string "text"))) parts)
          "Claude's unsigned thinking is dropped, not replayed"))))

(deftest google-antigravity-cell-routes-a-gemini-effort-to-its-model ()
  (with-ag-round (message posts :model "gemini-3.1-pro" :effort "high"
                  :frames (list (ag-event "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"ok\"}]},\"finishReason\":\"STOP\"}]}")))
    (let ((body (ag-body posts)))
      (is (equal "gemini-pro-agent" (nlk:json-value body :string "model")) "the effort's routed model")
      (is (= 10001 (nlk:json-value body :integer "request" "generationConfig" "thinkingConfig" "thinkingBudget")))
      (is (= 65535 (nlk:json-value body :integer "request" "generationConfig" "maxOutputTokens"))
          "the real client's fixed cap for it")
      (is (equal "MODEL_PLACEHOLDER_M16" (nlk:json-value body :string "request" "labels" "model_enum")))
      (is (equal "false" (nlk:json-value body :string "request" "labels" "used_claude")))
      (is (null (ag-header (second (first posts)) "anthropic-beta")))))
  (with-ag-round (message posts :model "gemini-3.1-pro"
                  :frames (list (ag-event "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"ok\"}]},\"finishReason\":\"STOP\"}]}")))
    (let ((body (ag-body posts)))
      (is (equal "gemini-3.1-pro-low" (nlk:json-value body :string "model")) "thinking off routes to the low model")
      (is (eql 0 (nlk:json-value body :integer "request" "generationConfig" "thinkingConfig" "thinkingBudget"))
          "and says off, since the backend would think by default")
      (is (null (nlk:json-value body :boolean "request" "generationConfig" "thinkingConfig" "includeThoughts")))
      (is (equal "MODEL_PLACEHOLDER_M36" (nlk:json-value body :string "request" "labels" "model_enum"))))))

(deftest google-antigravity-cell-restates-a-forced-call-to-gemini ()
  (with-ag-round (message posts :model "gemini-3-flash" :choice "required"
                  :frames (list (ag-event "{\"candidates\":[{\"content\":{\"parts\":[{\"functionCall\":{\"name\":\"eval\",\"args\":{}}}]},\"finishReason\":\"STOP\"}]}")))
    (let* ((request (nlk:json-value (ag-body posts) :object "request"))
           (contents (nlk:json-array request "contents"))
           (last (aref contents (1- (length contents)))))
      (is (equal "ANY" (nlk:json-value request :string "toolConfig" "functionCallingConfig" "mode")))
      (is (equal "user" (nlk:json-value last :string "role")))
      (is (uiop:string-prefix-p "TOOL-ONLY TURN." (nlk:json-value (aref (nlk:json-array last "parts") 0) :string "text"))
          "the directive omp sends"))))

(deftest google-antigravity-cell-falls-over-to-the-sandbox-host ()
  (with-stubbed-fdefinition (nle:turn () (list :session-id "s-fallover"))
    (clrhash nodecode-google-antigravity::*sessions*)
    (with-ag-round (message posts :answer (lambda (url)
                                            (when (search "daily-cloudcode-pa.googleapis.com" url)
                                              (error 'dex:http-request-service-unavailable
                                                     :status 503 :body "{\"error\":{\"message\":\"unavailable\"}}"
                                                     :headers (make-hash-table :test #'equal) :uri (quri:uri url)
                                                     :method :post))))
      (is (= 2 (length posts)))
      (is (search "daily-cloudcode-pa.googleapis.com" (first (second posts))) "the daily host first")
      (is (equal "https://daily-cloudcode-pa.sandbox.googleapis.com/v1internal:streamGenerateContent?alt=sse"
                 (first (first posts)))
          "then the sandbox")
      (is (equalp (third (first posts)) (third (second posts))) "the same request to both")
      (is (equal "eval" (nlk:json-value (aref (nlk:json-array message "tool_calls") 0) :string "function" "name"))))
    (let ((session (gethash "s-fallover" nodecode-google-antigravity::*sessions*)))
      (is (equal "https://daily-cloudcode-pa.sandbox.googleapis.com"
                 (nodecode-google-antigravity::session-last-good session))
          "the host that answered is the session's first next time")
      (is (equal "exec-1" (nodecode-google-antigravity::session-last-execution session))))
    (with-ag-round (message posts)
      (is (= 1 (length posts)))
      (is (search "sandbox" (first (first posts))) "tried first")
      (let ((labels (nlk:json-value (ag-body posts) :object "request" "labels")))
        (is (equal "exec-1" (nlk:json-value labels :string "last_execution_id")) "the last answer's id")
        (is (equal "2" (nlk:json-value labels :string "last_step_index")) "the session's step counted on")))))

(deftest google-antigravity-cell-keeps-its-pinned-version-without-the-manifest ()
  (with-cell-stop ((google-antigravity-start))
    (with-saved-globals ((nodecode-google-antigravity::*discovered-version* nil)
                         (nodecode-google-antigravity::*manifest-failed-at* nil))
      (let ((asked 0))
        (with-stubbed-fdefinition (dex:get (url &rest args) (incf asked) (error "offline"))
          (nodecode-google-antigravity::ensure-version)
          (nodecode-google-antigravity::ensure-version))
        (is (= 1 asked) "a failed read is not tried again at once")
        (is (search "antigravity/hub/2.19.1 " (nodecode-google-antigravity::client-user-agent)))))))

(deftest google-antigravity-cell-says-it-is-not-signed-in ()
  (with-cell-stop ((google-antigravity-start))
    (with-temp-auth (auth "{}")
      (let ((nle::*provider* "google-antigravity") (nle::*model* "claude-sonnet-4-5") (nle::*api-key* nil)
            (nle::*endpoint* nil) (nle::*auth-file-path* auth) (posted nil))
        (with-stubbed-fdefinition (dex:post (url &rest args) (setf posted t) (values (make-truncated-sse-stream) 200))
          (let ((refusal (signals-error nle::provider-config-error (ag-lane-round (user-context)))))
            (when (typep refusal 'nle::provider-error)
              (is (search "/google-antigravity login" (nle::provider-error-detail refusal))))))
        (is (null posted) "nothing is sent")))))

(deftest google-antigravity-cell-leaves-the-gemini-api-alone ()
  (with-cell-stop ((google-antigravity-start))
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
      (is (equal "AIza-test" (ag-header headers "x-goog-api-key")))
      (is (null (nlk:json-value body :string "requestType"))))))

(deftest google-antigravity-cell-lists-its-bundled-models-asking-no-one ()
  (with-cell-stop ((google-antigravity-start))
    (with-stubbed-fdefinitions ((dex:get (url &rest args) (error "no listing may be fetched: ~a" url))
                                (dex:post (url &rest args) (error "no listing may be fetched: ~a" url)))
      (multiple-value-bind (rows error) (nle::list-provider-models "google-antigravity")
        (is (null error) error)
        (is (= (count-if #'nodecode-google-antigravity::listed-p
                         (map 'list (lambda (row) (nlk:json-value row :string "id")) nodecode-google-antigravity::+models+))
               (length rows)))
        (is (plusp (length rows)))))))

(deftest google-antigravity-cell-checks-no-key-it-cannot-ask-about ()
  (with-cell-stop ((google-antigravity-start))
    (with-stubbed-fdefinitions ((dex:get (url &rest args) (error "no key check may dial: ~a" url))
                                (dex:post (url &rest args) (error "no key check may dial: ~a" url)))
      (multiple-value-bind (rows reason) (nle::list-provider-models "google-antigravity" :key "AIza-pasted")
        (is (plusp (length rows)) "the rows still")
        (is (search "/google-antigravity login" (or reason ""))
            "with why the key was not checked, never a NIL that /connect reads as a working key")))))

(defun ag-history (answer)
  "A compiled context after one round whose answer was ANSWER, and its result."
  (compiled-context
   (list (nlk:json-object "role" "user" "content" "add one and two")
         answer
         (nlk:json-object "role" "tool" "tool_call_id" "tool-0" "content" "3"))))

(deftest google-antigravity-cell-replays-claude-s-signed-thought ()
  (let ((answer nil))
    (with-ag-round (message posts) (setf answer message))
    (with-ag-round (message posts :context (ag-history answer))
      (let* ((contents (nlk:json-array (ag-body posts) "request" "contents"))
             (model (find "model" contents :key (lambda (content) (nlk:json-value content :string "role")) :test #'equal))
             (parts (nlk:json-array model "parts")))
        (is (eq t (nlk:json-value (aref parts 0) :boolean "thought")) "the thought, as a thought")
        (is (equal "Let me add." (nlk:json-value (aref parts 0) :string "text")))
        (is (equal "Y2xhdWRlLXNpZw==" (nlk:json-value (aref parts 0) :string "thoughtSignature"))
            "under the signature the cell kept, not one on the message")
        (is (equal "eval" (nlk:json-value (aref parts 1) :string "functionCall" "name")))
        (is (equal "tool-0" (nlk:json-value (aref parts 1) :string "functionCall" "id")))))
    (with-ag-round (message posts :model "claude-opus-4-5" :context (ag-history answer))
      (let* ((contents (nlk:json-array (ag-body posts) "request" "contents"))
             (model (find "model" contents :key (lambda (content) (nlk:json-value content :string "role")) :test #'equal)))
        (is (notany (lambda (part) (nlk:json-value part :boolean "thought")) (nlk:json-array model "parts"))
            "another Claude model is handed no thought signed for this one")))))

(deftest google-antigravity-cell-adds-no-field-another-lane-would-send ()
  (with-ag-round (message posts)
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
        (nle::call-provider-streaming (ag-history message)))
      (let ((assistant (find "assistant" (nlk:json-array sent "messages")
                             :key (lambda (wire) (nlk:json-value wire :string "role")) :test #'equal)))
        (is assistant "the chat lane sends the round's answer")
        (is (null (remove-if-not (lambda (key) (search "cca" key)) (alexandria:hash-table-keys assistant)))
            "and no field this cell added")))))
