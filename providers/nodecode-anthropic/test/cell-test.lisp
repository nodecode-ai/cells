;;;; cell-test.lisp --- the anthropic cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json, every token endpoint, bootstrap and
;;;; round a stubbed dex:post or dex:get, every key variable a stubbed
;;;; NLE::CREDENTIAL-ENV. The sign-in's callback listens on a free loopback
;;;; port the test dials itself: nothing reaches Anthropic, the environment
;;;; or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "anthropic" "ANTHROPIC-CELL-" :start nodecode-anthropic:start-cell)

(define-cell-lifecycle-tests "anthropic"
  (:hooks 'nle::models-catalog-table :credential 'nle::anthropic-request-body 'nle::walk-provider-stream)
  (:command "anthropic"))

;;; --- fixtures --------------------------------------------------------------------

(defun an-now () (nodecode-anthropic::unix-seconds))

(defun an-store (&key (access "sk-ant-oat01-signed-in") (expires-in 3600) (extra ""))
  "auth.json text holding one anthropic sign-in, EXTRA spliced before it."
  (format nil "{~a\"oauth_tokens\":{\"anthropic\":{\"provider\":\"anthropic\",\"access_token\":\"~a\",~
               \"refresh_token\":\"rt-1\",\"expires_at\":~d,\"account_id\":\"acct-uuid\",\"email\":\"op@example.com\",~
               \"org_id\":\"org-uuid\",\"org_name\":\"Op's Org\",\"installation_id\":\"inst-1\"}}}"
          extra access (+ (an-now) expires-in)))

(defun an-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun an-entry (auth)
  "The anthropic entry of the auth.json at AUTH."
  (nlk:json-value (nle::read-auth-file auth) :object "oauth_tokens" "anthropic"))

(defun an-octets-text (content)
  "A request CONTENT, octets or text, as text."
  (if (stringp content) content (sb-ext:octets-to-string (coerce content '(vector (unsigned-byte 8))) :external-format :utf-8)))

(defparameter +an-catalog+
  "{\"anthropic\": {\"id\": \"anthropic\", \"name\": \"Anthropic\", \"npm\": \"@ai-sdk/anthropic\",
                    \"env\": [\"ANTHROPIC_API_KEY\"],
                    \"models\": {\"claude-haiku-4-5\": {\"name\": \"Haiku as models.dev says\", \"reasoning\": true,
                                                       \"tool_call\": true,
                                                       \"limit\": {\"context\": 200000, \"output\": 64000}}}}}"
  "A models.dev-shaped catalog whose anthropic row lists one model and no base.")

(defparameter +an-stream+
  (list "{\"type\":\"message_start\",\"message\":{\"id\":\"msg_1\",\"model\":\"claude-haiku-4-5\",\"usage\":{\"input_tokens\":10,\"output_tokens\":1}}}"
        "{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"tool_use\",\"id\":\"toolu_2\",\"name\":\"_eval\",\"input\":{}}}"
        "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"{\\\"code\\\":\\\"1\\\"}\"}}"
        "{\"type\":\"content_block_stop\",\"index\":0}"
        "{\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"tool_use\"},\"usage\":{\"output_tokens\":7}}"
        "{\"type\":\"message_stop\"}")
  "One streamed call of the eval tool, under its wire name.")

(defun an-round (context)
  "One anthropic round of CONTEXT, dex:post stubbed to stream an eval call:
(values MESSAGE URL HEADERS BODY-TEXT)."
  (let ((url nil) (headers nil) (content nil))
    (with-stubbed-fdefinition (dex:post (asked &rest args)
                               (setf url asked headers (getf args :headers) content (getf args :content))
                               (values (apply #'make-truncated-sse-stream +an-stream+) 200))
      (values (nle::call-anthropic-streaming context) url headers (an-octets-text content)))))

(defmacro with-an-round ((&key (store "{}") key (model "claude-haiku-4-5")) &body body)
  "BODY with the cell started over a temp auth.json holding STORE, the
environment's ANTHROPIC_API_KEY being KEY, and MODEL selected on anthropic."
  `(with-cell-stop ((anthropic-start))
     (with-temp-auth (auth ,store)
       (with-stubbed-fdefinition (nle::credential-env (name)
                                  (and (equal name "ANTHROPIC_API_KEY") ,key))
         (let ((nle::*auth-file-path* auth) (nle::*provider* "anthropic") (nle::*model* ,model)
               (nle::*api-key* nil) (nle::*endpoint* nil))
           ,@body)))))

(defun an-tool-history ()
  "A history in which the assistant called eval once and the user answered."
  (list (nle::message "user" "please evaluate one plus one")
        (nlk:json-object "role" "assistant" "content" ""
                         "tool_calls" (vector (nlk:json-object "id" "toolu_1" "type" "function"
                                                               "function" (nlk:json-object "name" "eval"
                                                                                           "arguments" "{}"))))
        (nlk:json-object "role" "tool" "tool_call_id" "toolu_1" "content" "2")))

;;; --- XXH64 and the billing header -------------------------------------------------

(deftest anthropic-cell-xxh64-is-the-reference ()
  (flet ((hash (text &optional (seed 0))
           (format nil "~(~16,'0x~)" (nodecode-anthropic::xxh64 (sb-ext:string-to-octets text :external-format :latin-1) seed)))
         (range ()
           (map 'string #'code-char (loop repeat 3 append (loop for i below 70 collect i)))))
    (is (equal "ef46db3751d8e999" (hash "")))
    (is (equal "44bc2cf5ad770999" (hash "abc")))
    (is (equal "fbcea83c8a378bf1" (hash "Nobody inspects the spammish repetition")))
    (is (equal "c4d9ec16bc042051" (hash (range))) "every stripe of a long input")
    (is (equal "b8b30e7de65b46c5" (hash "" nodecode-anthropic::+cch-seed+)))
    (is (equal "fc8d069f34aac4f8" (hash "Nobody inspects the spammish repetition" nodecode-anthropic::+cch-seed+)))
    (is (equal "1be949205c8402e5" (hash (range) nodecode-anthropic::+cch-seed+)))))

(deftest anthropic-cell-billing-header-is-claude-codes ()
  (let ((header (nodecode-anthropic::billing-header "please evaluate one plus one")))
    (is (ppcre:scan "^x-anthropic-billing-header: cc_version=2\\.1\\.280\\.[0-9a-f]{3}; cc_entrypoint=cli; cch=00000;$"
                    header))
    (is (equal header (nodecode-anthropic::billing-header "XXease evXluate one plus one"))
        "only the 5th, 8th and 21st characters count")
    (is (string/= header (nodecode-anthropic::billing-header "")) "and a short text pads them with 0")))

;;; --- the catalog -------------------------------------------------------------------

(deftest anthropic-cell-fills-the-catalog-row-and-changes-nothing-in-it ()
  (with-catalog-fixture (path +an-catalog+)
    (with-cell-stop ((anthropic-start))
      (let* ((row (nlk:json-value (nle::models-catalog-table) :object "anthropic"))
             (models (nlk:json-value row :object "models")))
        (is (equal "Haiku as models.dev says" (nle::catalog-model-name (gethash "claude-haiku-4-5" models)))
            "models.dev's own row stands")
        (is (gethash "claude-mythos-5-1" models) "omp's models are added")
        (is (equal "@ai-sdk/anthropic" (nlk:json-value row :string "npm")))
        (is (null (nth-value 1 (gethash "api" row))) "and no base: the lane keeps its own")
        (is (equal "https://api.anthropic.com/v1/messages" (nle::lane-endpoint "anthropic" "anthropic")))))))

;;; --- the credential --------------------------------------------------------------------

(deftest anthropic-cell-serves-the-sign-in-only-when-no-key-does ()
  (with-cell-stop ((anthropic-start))
    (with-temp-auth (auth (an-store))
      (with-stubbed-fdefinition (nle::credential-env (name) nil)
        (let ((credential (nle::resolve-provider-credential "anthropic" :auth-path auth
                                                                       :endpoint "https://api.anthropic.com/v1/messages")))
          (is (equal "sk-ant-oat01-signed-in" (nle:credential-key credential)))
          (is (eq :oauth (nle:credential-source credential)))
          (is (getf (nle:credential-attributes credential) :subscription))
          (is (equal "acct-uuid" (getf (nle:credential-attributes credential) :account-id))))
        (is (eq :oauth (nle::provider-auth-state "anthropic" :auth-path auth))))
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "ANTHROPIC_API_KEY") "sk-ant-api03-env"))
        (is (equal "sk-ant-api03-env" (nle:credential-key (nle::resolve-provider-credential "anthropic" :auth-path auth :probe t)))
            "a key in the environment outranks the sign-in")))
    (with-temp-auth (auth (an-store :extra "\"api_keys\":{\"anthropic\":{\"provider\":\"anthropic\",\"key\":\"sk-ant-api03-saved\"}},"))
      (with-stubbed-fdefinition (nle::credential-env (name) nil)
        (is (equal "sk-ant-api03-saved" (nle:credential-key (nle::resolve-provider-credential "anthropic" :auth-path auth :probe t)))
            "so does a key /connect saved")))))

(deftest anthropic-cell-refreshes-an-expiring-token ()
  (with-cell-stop ((anthropic-start))
    (with-temp-auth (auth (an-store :access "sk-ant-oat01-old" :expires-in 30))
      (let ((posts '()))
        (with-stubbed-fdefinitions ((nle::credential-env (name) nil)
                                    (dex:post (asked &rest args)
                                     (push (list asked (getf args :headers) (nlk:decode-json (getf args :content))) posts)
                                     (values "{\"access_token\":\"sk-ant-oat01-new\",\"refresh_token\":\"rt-2\",\"expires_in\":28800,\"account\":{\"uuid\":\"acct-uuid\",\"email_address\":\"op@example.com\"}}" 200)))
          (is (equal "sk-ant-oat01-old" (nle:credential-key (nle::resolve-provider-credential "anthropic" :auth-path auth :probe t)))
              "a probe reads the store as it is")
          (is (null posts) "and never dials")
          (is (equal "sk-ant-oat01-new" (nle:credential-key (nle::resolve-provider-credential
                                                             "anthropic" :auth-path auth
                                                                         :endpoint "https://api.anthropic.com/v1/messages")))))
        (is (= 1 (length posts)))
        (destructuring-bind (asked headers body) (first posts)
          (is (equal "https://api.anthropic.com/v1/oauth/token" asked))
          (is (equal "oauth-2025-04-20" (an-header headers "anthropic-beta")))
          (is (equal "anthropic-sdk-typescript/0.112.1 userOAuthProvider" (an-header headers "User-Agent")))
          (is (equal "application/json" (an-header headers "content-type")))
          (is (equal "refresh_token" (nlk:json-value body :string "grant_type")))
          (is (equal "rt-1" (nlk:json-value body :string "refresh_token")))
          (is (equal "9d1c250a-e61b-44d9-88ed-5944d1962f5e" (nlk:json-value body :string "client_id"))))
        (let ((entry (an-entry auth)))
          (is (equal "sk-ant-oat01-new" (nlk:json-value entry :string "access_token")))
          (is (equal "rt-2" (nlk:json-value entry :string "refresh_token")))
          (is (<= (abs (- (nlk:json-value entry :integer "expires_at") (+ (an-now) 28800 -300))) 5)
              "five minutes early, the rule's skew")
          (is (equal "org-uuid" (nlk:json-value entry :string "org_id")) "the sign-in's organization kept")
          (is (equal "inst-1" (nlk:json-value entry :string "installation_id"))))))))

;;; --- the key path is the core's ---------------------------------------------------------

(deftest anthropic-cell-leaves-the-key-path-as-it-was ()
  (with-catalog-fixture (path +an-catalog+)
    (flet ((key-round ()
             (with-temp-auth (auth (an-store))
               (with-stubbed-fdefinition (nle::credential-env (name)
                                          (and (equal name "ANTHROPIC_API_KEY") "sk-ant-api03-key"))
                 (let ((nle::*auth-file-path* auth) (nle::*provider* "anthropic")
                       (nle::*model* "claude-haiku-4-5") (nle::*api-key* nil) (nle::*endpoint* nil))
                   (multiple-value-list (an-round (compiled-context (an-tool-history)))))))))
      (destructuring-bind (message url headers body) (key-round)
        (declare (ignore message))
        (with-cell-stop ((anthropic-start))
          (destructuring-bind (message* url* headers* body*) (key-round)
            (is (equal "_eval" (nlk:json-value (aref (nlk:json-value message* :array "tool_calls") 0)
                                               :string "function" "name"))
                "a tool name the API sent comes back as it was sent")
            (is (equal url url*) "the same address")
            (is (equal "https://api.anthropic.com/v1/messages" url*))
            (is (equal headers headers*) "the same headers")
            (is (equal "sk-ant-api03-key" (an-header headers* "x-api-key")))
            (is (null (an-header headers* "authorization")))
            (is (equal body body*) "the same body, byte for byte")))))))

;;; --- a subscription round ---------------------------------------------------------------

(deftest anthropic-cell-sends-a-subscription-round-as-claude-code ()
  (with-an-round (:store (an-store))
    (multiple-value-bind (message url headers text)
        ;; thinking on: an agent request, Claude Code's fuller beta set
        (let ((nle::*reasoning-effort* "high")) (an-round (compiled-context (an-tool-history))))
      (let* ((body (nlk:decode-json text))
             (system (nlk:json-value body :array "system"))
             (billing (nlk:json-value (aref system 0) :string "text")))
        (is (equal "https://api.anthropic.com/v1/messages?beta=true" url))
        (is (equal "Bearer sk-ant-oat01-signed-in" (an-header headers "Authorization")))
        (is (null (an-header headers "x-api-key")) "never as a key")
        (is (= 1 (count "anthropic-beta" headers :key #'car :test #'string-equal)) "one beta header")
        (is (search "oauth-2025-04-20" (an-header headers "anthropic-beta")))
        (is (search "claude-code-20250219" (an-header headers "anthropic-beta")))
        (is (search "effort-2025-11-24" (an-header headers "anthropic-beta")) "thinking asks for effort")
        (is (null (search "extended-cache-ttl" (an-header headers "anthropic-beta"))) "Claude Code sends no ttl beta")
        (is (equal "claude-cli/2.1.280 (external, cli)" (an-header headers "User-Agent")))
        (is (equal "cli" (an-header headers "x-app")))
        (is (equal "js" (an-header headers "X-Stainless-Lang")))
        (is (equal "true" (an-header headers "anthropic-dangerous-direct-browser-access")))
        (is (an-header headers "X-Claude-Code-Session-Id"))
        (is (ppcre:scan "^x-anthropic-billing-header: cc_version=2\\.1\\.280\\.[0-9a-f]{3}; cc_entrypoint=cli; cch=[0-9a-f]{5};$" billing))
        (is (null (search "cch=00000" billing)) "the cch is attested")
        (let ((placeholder (ppcre:regex-replace "cch=[0-9a-f]{5}" text "cch=00000")))
          (is (equal (subseq billing (- (length billing) 6) (1- (length billing)))
                     (format nil "~(~5,'0x~)" (ldb (byte 20 0) (nodecode-anthropic::xxh64
                                                                (sb-ext:string-to-octets placeholder :external-format :utf-8)
                                                                nodecode-anthropic::+cch-seed+)))))
              "the low 20 bits of the body's XXH64, taken with the placeholder in")
        (is (equal "You are Claude Code, Anthropic's official CLI for Claude."
                   (nlk:json-value (aref system 1) :string "text")))
        (is (= 3 (length system)) "then the prompt Nodecode wrote")
        (is (<= (loop for block across system count (gethash "cache_control" block)) 1))
        (is (equal "1h" (nlk:json-value (aref system 2) :string "cache_control" "ttl")) "a seat's hour-long cache")
        (is (every (lambda (tool) (uiop:string-prefix-p "_" (nlk:json-value tool :string "name")))
                   (or (nlk:json-value body :array "tools") #()))
            "every tool under its wire name")
        (is (equal "_eval" (nlk:json-value (find "tool_use" (nlk:json-value (aref (nlk:json-value body :array "messages") 1)
                                                                            :array "content")
                                                 :key (lambda (block) (nlk:json-value block :string "type"))
                                                 :test #'equal)
                                           :string "name"))
            "a call in the history under its wire name")
        (let ((user (nlk:decode-json (nlk:json-value body :string "metadata" "user_id"))))
          (is (= 64 (length (nlk:json-value user :string "device_id"))))
          (is (equal "acct-uuid" (nlk:json-value user :string "account_uuid")))
          (is (equal (an-header headers "X-Claude-Code-Session-Id") (nlk:json-value user :string "session_id"))))
        (is (equal "eval" (nlk:json-value (aref (nlk:json-value message :array "tool_calls") 0)
                                          :string "function" "name"))
            "the answer's call comes back under Nodecode's name")))))

(deftest anthropic-cell-treats-a-subscription-key-as-omp-does ()
  (with-an-round (:key "sk-ant-oat01-from-the-environment")
    (multiple-value-bind (message url headers) (an-round (user-context "hello there, model"))
      (declare (ignore message))
      (is (equal "https://api.anthropic.com/v1/messages?beta=true" url))
      (is (equal "Bearer sk-ant-oat01-from-the-environment" (an-header headers "Authorization")))
      (is (null (an-header headers "x-api-key"))))))

;;; --- the sign-in -------------------------------------------------------------------

(defun an-get (port target)
  "GET TARGET from 127.0.0.1:PORT over a raw socket: the answer's status code."
  (let ((socket (usocket:socket-connect "127.0.0.1" port :element-type '(unsigned-byte 8))))
    (unwind-protect
         (let ((stream (usocket:socket-stream socket))
               (bytes (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
           (write-sequence (sb-ext:string-to-octets
                            (format nil "GET ~a HTTP/1.1~c~cHost: localhost~c~c~c~c"
                                    target #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed)
                            :external-format :latin-1)
                           stream)
           (finish-output stream)
           (loop for byte = (read-byte stream nil nil) while byte do (vector-push-extend byte bytes))
           (parse-integer (sb-ext:octets-to-string bytes :external-format :latin-1) :start 9 :end 12))
      (usocket:socket-close socket))))

(defun an-query (url name)
  "The query parameter NAME of URL."
  (cdr (assoc name (quri:uri-query-params (quri:uri url)) :test #'equal)))

(defun an-login-url (text)
  "The authorization address a /anthropic login answer names."
  (find-if (lambda (line) (uiop:string-prefix-p "https://" line))
           (uiop:split-string text :separator '(#\Newline))))

(defun an-port (url)
  "The port of the redirect URL names."
  (let ((redirect (an-query url "redirect_uri")))
    (parse-integer redirect :start 17 :end (position #\/ redirect :start 17))))

(defun an-await-login ()
  "Wait for the running sign-in's thread to finish."
  (alexandria:when-let (login nodecode-anthropic::*login*)
    (bt2:join-thread (nodecode-anthropic::login-thread login))))

(defparameter +an-token-answer+
  (concatenate 'string
               "{\"access_token\":\"sk-ant-oat01-fresh\",\"refresh_token\":\"rt-9\",\"expires_in\":28800,"
               "\"account\":{\"uuid\":\"acct-uuid\",\"email_address\":\"op@example.com\"},"
               "\"organization\":{\"uuid\":\"org-uuid\",\"name\":\"Op's Org\"}}")
  "The token endpoint's answer to a code: the token, its account, its organization.")

(defmacro with-an-login ((auth url posts gets &key (answer '+an-token-answer+)) &body body)
  "BODY with the cell started and a sign-in begun on a free port against a
temp auth.json AUTH; POSTS and GETS what the token and bootstrap endpoints saw."
  `(with-cell-stop ((anthropic-start))
     (with-temp-auth (,auth "{}")
       (let ((nle::*auth-file-path* ,auth)
             (nodecode-anthropic::*callback-port* 0)
             (,posts '())
             (,gets '()))
         (with-stubbed-fdefinitions ((dex:post (asked &rest args)
                                      (push (list asked (getf args :headers) (nlk:decode-json (getf args :content))) ,posts)
                                      (values ,answer 200))
                                     (dex:get (asked &rest args)
                                      (push (list asked (getf args :headers)) ,gets)
                                      (values "{\"oauth_account\":{\"account_uuid\":\"acct-b\",\"account_email\":\"b@example.com\",\"organization_uuid\":\"org-b\",\"organization_name\":\"B Org\"}}" 200)))
           (let ((,url (an-login-url (cell-entry "nodecode-anthropic" "anthropic" "login"))))
             (declare (ignorable ,url))
             ,@body))))))

(deftest anthropic-cell-signs-in-through-the-callback ()
  (with-an-login (auth url posts gets)
    (is (uiop:string-prefix-p "https://claude.ai/oauth/authorize?client_id=9d1c250a-e61b-44d9-88ed-5944d1962f5e&" url))
    (is (equal "true" (an-query url "code")) "claude.ai shows the code too")
    (is (equal "org:create_api_key user:profile user:inference user:sessions:claude_code user:mcp_servers user:file_upload"
               (an-query url "scope")))
    (is (equal "S256" (an-query url "code_challenge_method")))
    (is (= 500 (an-get (an-port url) "/callback?code=c-1&state=forged")) "a forged state is refused")
    (is (= 200 (an-get (an-port url) (format nil "/callback?code=c-1&state=~a" (an-query url "state")))))
    (an-await-login)
    (is (= 1 (length posts)))
    (destructuring-bind (asked headers body) (first posts)
      (is (equal "https://api.anthropic.com/v1/oauth/token" asked))
      (is (equal "application/json" (an-header headers "content-type")) "JSON, as the rule says")
      (is (equal "authorization_code" (nlk:json-value body :string "grant_type")))
      (is (equal "c-1" (nlk:json-value body :string "code")))
      (is (equal (an-query url "state") (nlk:json-value body :string "state")))
      (is (equal (an-query url "redirect_uri") (nlk:json-value body :string "redirect_uri")))
      (is (equal (an-query url "code_challenge")
                 (nodecode-anthropic::base64url (nodecode-anthropic::sha256 (nlk:json-value body :string "code_verifier"))))))
    (is (null gets) "a response naming its account needs no bootstrap")
    (let ((entry (an-entry auth)))
      (is (equal "sk-ant-oat01-fresh" (nlk:json-value entry :string "access_token")))
      (is (equal "rt-9" (nlk:json-value entry :string "refresh_token")))
      (is (<= (abs (- (nlk:json-value entry :integer "expires_at") (+ (an-now) 28800 -300))) 5))
      (is (equal "acct-uuid" (nlk:json-value entry :string "account_id")))
      (is (equal "op@example.com" (nlk:json-value entry :string "email")))
      (is (equal "org-uuid" (nlk:json-value entry :string "org_id")))
      (is (equal "Op's Org" (nlk:json-value entry :string "org_name")))
      (is (= 36 (length (nlk:json-value entry :string "installation_id")))))
    (is (= #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (namestring auth))))))
    (is (search "signed in as op@example.com (Op's Org)" (second (cell-notice "nodecode-anthropic"))))))

(deftest anthropic-cell-takes-a-pasted-code-and-asks-the-bootstrap ()
  (with-an-login (auth url posts gets :answer "{\"access_token\":\"sk-ant-oat01-fresh\",\"refresh_token\":\"rt-9\",\"expires_in\":28800}")
    (is (search "code received"
                (cell-entry "nodecode-anthropic" "anthropic" (format nil "code c-7#~a" (an-query url "state")))))
    (an-await-login)
    (destructuring-bind (asked headers body) (first posts)
      (declare (ignore asked headers))
      (is (equal "c-7" (nlk:json-value body :string "code")) "the code without its #state"))
    (is (= 1 (length gets)))
    (destructuring-bind (asked headers) (first gets)
      (is (equal "https://api.anthropic.com/api/claude_cli/bootstrap?entrypoint=cli&model=claude-opus-4-8" asked))
      (is (equal "Bearer sk-ant-oat01-fresh" (an-header headers "Authorization")))
      (is (equal "oauth-2025-04-20" (an-header headers "anthropic-beta"))))
    (let ((entry (an-entry auth)))
      (is (equal "acct-b" (nlk:json-value entry :string "account_id")) "the identity the token response left out")
      (is (equal "b@example.com" (nlk:json-value entry :string "email")))
      (is (equal "org-b" (nlk:json-value entry :string "org_id"))))))

(deftest anthropic-cell-falls-back-to-a-free-port ()
  (let ((held (usocket:socket-listen "127.0.0.1" 0 :reuse-address t)))
    (unwind-protect
         (with-cell-stop ((anthropic-start))
           (with-temp-auth (auth "{}")
             (let ((nle::*auth-file-path* auth)
                   (nodecode-anthropic::*callback-port* (usocket:get-local-port held)))
               (let ((url (an-login-url (cell-entry "nodecode-anthropic" "anthropic" "login"))))
                 (is url "the sign-in still starts")
                 (is (/= (usocket:get-local-port held) (an-port url)) "on another port")))))
      (nodecode-anthropic::cancel-login)
      (usocket:socket-close held))))

(deftest anthropic-cell-logs-out-and-says-its-status ()
  (with-cell-stop ((anthropic-start))
    (with-temp-auth (auth (an-store))
      (let ((nle::*auth-file-path* auth))
        (with-stubbed-fdefinition (nle::credential-env (name) nil)
          (is (search "signed in as op@example.com (Op's Org)" (cell-entry "nodecode-anthropic" "anthropic" "status")))
          (is (search "signed out" (cell-entry "nodecode-anthropic" "anthropic" "logout")))
          (is (null (an-entry auth)))
          (is (search "not signed in" (cell-entry "nodecode-anthropic" "anthropic" ""))))))))
