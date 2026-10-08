;;;; signin.lisp --- the browser sign-in: the code, the key it mints, where it is kept.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; auth/zai-coding-plan.kdl (the login rule), ai/src/registry/engine/
;;;; oauth-code.ts and common.ts (the authorization-code engine that rule
;;;; drives), ai/src/registry/oauth/callback-server.ts (the pasted-code parse)
;;;; and ai/src/registry/oauth/zai.ts (the key it mints).
;;;;
;;;; The flow is ZCode's "Individual Plan" sign-in, verbatim: an
;;;; authorization-code request with no PKCE against chat.z.ai, whose
;;;; redirect is ZCode's desktop scheme zcode://zai-auth/callback (Z.AI's
;;;; allowlist refuses every loopback redirect for this client), so the
;;;; operator pastes the address the browser was sent to, or the code in it.
;;;; The code buys a short-lived OAuth token at zcode.z.ai; that token buys a
;;;; business token at api.z.ai, which finds or creates a key named
;;;; `nodecode' in the account's default project and copies out its secret.
;;;; `<apiKey>.<secretKey>' is a durable Z.AI API key: it is what is kept, it
;;;; never expires, and it is what every request sends.
;;;;
;;;; Kept in auth.json under oauth_tokens.zai-coding-plan:
;;;;   {"access_token": "<id>.<secret>", "email": ..., "account_id": ...}
;;;; with no refresh_token and no expires_at, since the key does not expire.
;;;;
;;;; omp's environment overrides are honored as omp honors them:
;;;; ZAI_OAUTH_CLIENT_ID, ZAI_OAUTH_AUTHORIZE_URL, ZAI_OAUTH_REDIRECT_URI,
;;;; ZAI_OAUTH_TOKEN_URL, ZAI_BIZ_BASE, ZAI_BUSINESS_LOGIN_URL.

(in-package #:nodecode-zai-coding-plan)

(defparameter +client-id+ "client_P8X5CMWmlaRO9gyO-KSqtg"
  "ZCode's OAuth client at chat.z.ai.")

(defparameter +authorize-url+ "https://chat.z.ai/api/oauth/authorize")

(defparameter +redirect-uri+ "zcode://zai-auth/callback"
  "ZCode's desktop scheme: the one redirect Z.AI still allows this client.")

(defparameter +token-url+ "https://zcode.z.ai/api/v1/oauth/token")

(defparameter +biz-base+ "https://api.z.ai"
  "Z.AI's business API: the customer, its projects, their keys.")

(defparameter +business-login-url+ "https://api.z.ai/api/auth/z/login"
  "Where the OAuth token is exchanged for a business token.")

(defparameter +key-name+ "nodecode"
  "The name of the key the sign-in finds or creates: Nodecode's own, so a
sign-in never touches the key ZCode keeps (zcode-api-key) or omp's (oh-my-pi).")

(defparameter +login-seconds+ 300
  "How long a sign-in waits for its code: omp's callback timeout.")

(defun overridden (name default)
  "The environment variable NAME when it is set, else DEFAULT."
  (or (nle::credential-env name) default))

;;; --- words and bytes ------------------------------------------------------------

(defun random-hex (count)
  "COUNT random bytes as lowercase hex: omp's default state."
  (format nil "~(~{~2,'0x~}~)" (coerce (nlk:random-bytes count) 'list)))

(defun form-encode (text)
  "TEXT as application/x-www-form-urlencoded spells it, the way URLSearchParams does."
  (with-output-to-string (out)
    (loop for byte across (sb-ext:string-to-octets text :external-format :utf-8)
          for char = (code-char byte)
          do (cond ((or (char<= #\a char #\z) (char<= #\A char #\Z) (char<= #\0 char #\9)
                        (find char "*-._"))
                    (write-char char out))
                   ((= byte 32) (write-char #\+ out))
                   (t (format out "%~2,'0X" byte))))))

(defun form-decode (text)
  "TEXT, form-encoded, decoded: + is a space, %XX a byte."
  (let ((octets (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop with i = 0
          while (< i (length text))
          do (let ((char (char text i)))
               (cond ((char= char #\+) (vector-push-extend 32 octets) (incf i))
                     ((and (char= char #\%) (<= (+ i 3) (length text))
                           (digit-char-p (char text (+ i 1)) 16) (digit-char-p (char text (+ i 2)) 16))
                      (vector-push-extend (parse-integer text :start (1+ i) :end (+ i 3) :radix 16) octets)
                      (incf i 3))
                     (t (loop for byte across (sb-ext:string-to-octets (string char) :external-format :utf-8)
                              do (vector-push-extend byte octets))
                        (incf i)))))
    (sb-ext:octets-to-string (coerce octets '(vector (unsigned-byte 8))) :external-format :utf-8)))

(defun query-string (pairs)
  "PAIRS, an alist of strings, as a query string."
  (format nil "~{~a~^&~}"
          (loop for (key . value) in pairs
                collect (format nil "~a=~a" (form-encode key) (form-encode value)))))

(defun query-params (text)
  "The name/value alist of TEXT, a query string with or without its ? or #."
  (loop for pair in (uiop:split-string (string-left-trim "?#" text) :separator "&")
        for equals = (position #\= pair)
        when (plusp (length pair))
          collect (cons (form-decode (subseq pair 0 equals))
                        (if equals (form-decode (subseq pair (1+ equals))) ""))))

(defun param (params name)
  "NAME's value in PARAMS, the first one, or NIL."
  (cdr (assoc name params :test #'string=)))

(defun url-p (text)
  "Whether TEXT reads as an absolute URL: a scheme, then a colon."
  (let ((colon (position #\: text)))
    (and colon (plusp colon) (alpha-char-p (char text 0))
         (every (lambda (char) (or (alphanumericp char) (find char "+.-"))) (subseq text 0 colon)))))

(defun url-query (url)
  "The query of URL, between its ? and its #, or \"\"."
  (let* ((hash (position #\# url))
         (question (position #\? url :end hash)))
    (if question (subseq url (1+ question) hash) "")))

(defun parse-callback-input (input)
  "(values CODE STATE) out of what the operator pasted: the redirect address,
a query string, or the bare code with its state after a #. omp's
parseCallbackInput."
  (let ((value (nlk:trimmed (or input ""))))
    (cond ((zerop (length value)) (values nil nil))
          ((url-p value)
           (let ((params (query-params (url-query value))))
             (values (param params "code") (param params "state"))))
          ((search "code=" value)
           (let ((params (query-params value)))
             (values (param params "code") (param params "state"))))
          (t (let* ((hash (position #\# value))
                    (next-hash (and hash (position #\# value :start (1+ hash)))))
               (if hash
                   (values (subseq value 0 hash) (subseq value (1+ hash) next-hash))
                   (values value nil)))))))

;;; --- one HTTP exchange --------------------------------------------------------------

(defun body-string (body)
  "BODY, as dexador answered it, as a string."
  (typecase body
    (string body)
    ((vector (unsigned-byte 8)) (sb-ext:octets-to-string body :external-format :utf-8))
    (null "")
    (t (princ-to-string body))))

(defun http (method url &key headers content)
  "(values TEXT STATUS) of one request, a refusal's status and body included."
  (handler-case
      (multiple-value-bind (body status)
          (ecase method
            (:get (dex:get url :headers headers :connect-timeout 30 :read-timeout 30))
            (:post (dex:post url :headers headers :content content
                                 :connect-timeout 30 :read-timeout 30)))
        (values (body-string body) status))
    (dex:http-request-failed (condition)
      (values (body-string (dex:response-body condition)) (dex:response-status condition)))))

(defun json-or-nil (text)
  "TEXT decoded, or NIL when it is not JSON."
  (and (plusp (length text)) (ignore-errors (nlk:decode-json text))))

(defun ok-p (status)
  "Whether STATUS is a 2xx."
  (and (integerp status) (<= 200 status 299)))

(defun excerpt (text)
  "TEXT's first 500 characters, as omp quotes a body."
  (subseq text 0 (min 500 (length text))))

;;; --- the business API: the key the sign-in mints --------------------------------------

(defun success-code-p (code)
  "Whether CODE is a success: 0 at the OAuth endpoint, 200 at the business API."
  (or (null code) (eq code :null)
      (and (integerp code) (member code '(0 200)))
      (and (stringp code) (member code '("0" "200") :test #'string=))))

(defun unwrap (body operation)
  "BODY without Z.AI's {code, msg, data, success} envelope: its data, or
BODY itself when it carries none; an envelope saying failure signals with
its msg. omp's unwrapEnvelope."
  (if (and (hash-table-p body)
           (or (nth-value 1 (gethash "code" body)) (nth-value 1 (gethash "success" body))))
      (let ((code (gethash "code" body)))
        (when (or (eq (gethash "success" body :absent) nil) (not (success-code-p code)))
          (error "Z.ai ~a failed: ~a" operation
                 (or (nlk:json-value body :string "msg") (format nil "code ~a" code))))
        (multiple-value-bind (data present) (gethash "data" body)
          (if present data body)))
      body))

(defun biz-get (url token)
  "GET URL with the business TOKEN, decoded; a refusal signals."
  (multiple-value-bind (text status) (http :get url :headers `(("Authorization" . ,(format nil "Bearer ~a" token))))
    (unless (ok-p status)
      (error "HTTP request failed. status=~a; url=~a; body=~a" status url text))
    (json-or-nil text)))

(defun biz-post (url object &optional token)
  "POST OBJECT as JSON to URL, with the business TOKEN when given, decoded."
  (multiple-value-bind (text status)
      (http :post url :headers `(,@(when token `(("Authorization" . ,(format nil "Bearer ~a" token))))
                                 ("Content-Type" . "application/json"))
                      :content (nlk:encode-json-object object))
    (unless (ok-p status)
      (error "HTTP request failed. status=~a; url=~a; body=~a" status url text))
    (json-or-nil text)))

(defun text-field (object &rest keys)
  "The first of KEYS that is a non-blank string on OBJECT, trimmed."
  (loop for key in keys
        for value = (nlk:json-value object :string key)
        when (and value (plusp (length (nlk:trimmed value)))) return (nlk:trimmed value)))

(defun key-array (value)
  "An api_keys listing as a list: a bare array or a list/keys/apiKeys/records wrapper."
  (coerce (cond ((and (vectorp value) (not (stringp value))) value)
                ((hash-table-p value)
                 (or (some (lambda (field) (nlk:json-value value :array field))
                           '("list" "keys" "apiKeys" "records"))
                     #()))
                (t #()))
          'list))

(defun default-of (items)
  "The item of ITEMS marked isDefault, else the first."
  (or (find-if (lambda (item) (nlk:json-value item :boolean "isDefault")) items)
      (first items)))

(defun mint-api-key (oauth-token)
  "The durable `<apiKey>.<secretKey>' Z.AI key OAUTH-TOKEN provisions:
business login, the default organization and project, the key named
+KEY-NAME+ found or made there, its secret copied out. omp's mintZaiApiKey."
  (let* ((base (overridden "ZAI_BIZ_BASE" +biz-base+))
         (login (unwrap (biz-post (overridden "ZAI_BUSINESS_LOGIN_URL" +business-login-url+)
                                  (nlk:json-object "token" oauth-token))
                        "business login"))
         (biz (or (text-field login "access_token" "accessToken")
                  (error "Z.ai business login returned no access token")))
         (customer (unwrap (biz-get (format nil "~a/api/biz/customer/getCustomerInfo" base) biz)
                           "customer lookup"))
         (org (default-of (coerce (or (nlk:json-value customer :array "organizations") #()) 'list)))
         (project (default-of (coerce (or (nlk:json-value org :array "projects") #()) 'list)))
         (organization-id (text-field org "organizationId"))
         (project-id (text-field project "projectId")))
    (unless (and organization-id project-id)
      (error "Z.ai key provisioning failed: no organization/project on account"))
    (let* ((keys-url (format nil "~a/api/biz/v1/organization/~a/projects/~a/api_keys"
                             base organization-id project-id))
           (existing (find +key-name+ (key-array (unwrap (biz-get keys-url biz) "api key list"))
                           :key (lambda (key) (nlk:json-value key :string "name")) :test #'equal))
           (record (or existing
                       (unwrap (biz-post keys-url (nlk:json-object "name" +key-name+) biz)
                               "api key create")))
           (api-key (or (text-field record "apiKey")
                        (error "Z.ai key provisioning returned no apiKey")))
           ;; The copy endpoint, always: a listing masks the secret, and a
           ;; create's inline one is not reliable across account states.
           (copied (unwrap (biz-get (format nil "~a/copy/~a" keys-url (form-encode api-key)) biz)
                           "api key copy"))
           (secret (or (text-field copied "secretKey")
                       (error "Z.ai key provisioning returned no secretKey"))))
      (format nil "~a.~a" api-key secret))))

;;; --- the code, exchanged -----------------------------------------------------------

(defun scalar (value)
  "VALUE as a credential field: a non-empty string, or a number as a string."
  (typecase value
    (string (and (plusp (length value)) value))
    (integer (princ-to-string value))
    (t nil)))

(defun exchange-code (code state redirect-uri)
  "The oauth_tokens entry CODE signs in to: the token request ZCode makes,
then the key the token mints. A `code#state' paste is split, its state winning."
  (let* ((hash (position #\# code))
         (state (if (and hash (< (1+ hash) (length code))) (subseq code (1+ hash)) state))
         (code (if hash (subseq code 0 hash) code)))
    (multiple-value-bind (text status)
        (http :post (overridden "ZAI_OAUTH_TOKEN_URL" +token-url+)
              :headers '(("Content-Type" . "application/json"))
              :content (nlk:encode-json-object
                        (nlk:json-object "provider" "zai" "code" code
                                         "redirect_uri" redirect-uri "state" state)))
      (unless (ok-p status)
        (error "zai-coding-plan token exchange failed: ~a ~a" status (excerpt text)))
      (let* ((body (json-or-nil text))
             (access (nlk:json-value body :text "data" "zai" "access_token")))
        (unless access
          (error "zai-coding-plan token response missing access token: ~a" (excerpt text)))
        (nlk:json-object "access_token" (mint-api-key access)
                         :opt "email" (scalar (nlk:json-value body :any "data" "user" "email"))
                         :opt "account_id" (scalar (nlk:json-value body :any "data" "user" "id")))))))

;;; --- where the key is kept ----------------------------------------------------------

(defvar *store-lock* (bt2:make-lock :name "nodecode-zai-coding-plan auth.json")
  "Held across one read-modify-write of auth.json.")

(defun stored-entry (auth)
  "oauth_tokens.zai-coding-plan of the parsed AUTH, or NIL."
  (nlk:json-value auth :object "oauth_tokens" +provider+))

(defun save-entry (entry &optional (auth-path nle::*auth-file-path*))
  "Make ENTRY oauth_tokens.zai-coding-plan in the auth.json at AUTH-PATH, or
take it out when ENTRY is NIL, every other field kept: the way
NLE::SAVE-PROVIDER-API-KEY writes api_keys."
  (bt2:with-lock-held (*store-lock*)
    (let* ((path (merge-pathnames auth-path))
           (auth (or (nle::read-auth-file path) (make-hash-table :test #'equal)))
           (tokens (or (nlk:json-value auth :object "oauth_tokens")
                       (setf (gethash "oauth_tokens" auth) (make-hash-table :test #'equal)))))
      (if entry
          (setf (gethash +provider+ tokens) entry)
          (remhash +provider+ tokens))
      (nlk:write-file-atomically path (shasht:write-json auth nil)
                                 :mode #o600 :directory-mode #o700)
      entry)))

(defun credential (op next)
  "The :CREDENTIAL answer for zai-coding-plan: the key the sign-in minted,
else ZAI_API_KEY (omp's), else ZAI_CODING_PLAN_API_KEY (the core's own name
for it), else none; every other provider passes."
  ;; Never NEXT for zai-coding-plan: the ladder behind this point falls back
  ;; to the chat family's default variable, and would send OPENAI_API_KEY to
  ;; Z.AI. The minted key never expires, so nothing is refreshed, and a probe
  ;; costs no network.
  (if (equal (getf op :provider) +provider+)
      (let ((minted (nlk:json-value (stored-entry (getf op :auth)) :text "access_token")))
        (cond (minted (nle:make-credential minted :oauth))
              ((or (env-key) (nle::provider-env-key +provider+))
               (nle:make-credential (or (env-key) (nle::provider-env-key +provider+)) :env))
              (t (nle:make-credential "public" :public))))
      (funcall next op)))

;;; --- the sign-in in flight -------------------------------------------------------------

(defstruct (flow (:constructor make-flow (state redirect-uri url auth-path)))
  "One sign-in waiting for its code."
  state redirect-uri url auth-path
  (mailbox (sb-concurrency:make-mailbox :name "nodecode-zai-coding-plan sign-in"))
  (thread nil))

(defvar *flow* nil "The sign-in waiting for its code, or NIL.")

(defvar *flow-lock* (bt2:make-lock :name "nodecode-zai-coding-plan sign-in"))

(defun authorize-url (state redirect-uri)
  "The address the operator opens: the standard authorization-code request,
no PKCE, no scope."
  (format nil "~a?~a" (overridden "ZAI_OAUTH_AUTHORIZE_URL" +authorize-url+)
          (query-string `(("client_id" . ,(overridden "ZAI_OAUTH_CLIENT_ID" +client-id+))
                          ("response_type" . "code")
                          ("redirect_uri" . ,redirect-uri)
                          ("state" . ,state)))))

(defun say (text level)
  "TEXT to the operator, said once: an outcome is news, not a standing state."
  (nle:notice text :level level))

(defun finish-flow (flow)
  "Forget FLOW when it is still the one in flight."
  (bt2:with-lock-held (*flow-lock*)
    (when (eq *flow* flow) (setf *flow* nil))))

(defun run-flow (flow)
  "Wait for FLOW's code, then sign in with it and say how it went."
  (unwind-protect
       (handler-case
           (let ((message (sb-concurrency:receive-message (flow-mailbox flow) :timeout +login-seconds+)))
             (cond ((null message)
                    (say (format nil "zai-coding-plan: the sign-in timed out after ~d minutes with no code; start again with /zai-coding-plan login"
                                 (floor +login-seconds+ 60))
                         :warning))
                   ((eq (first message) :code)
                    (let ((entry (exchange-code (second message) (third message) (flow-redirect-uri flow))))
                      (save-entry entry (flow-auth-path flow))
                      (say (format nil "zai-coding-plan: signed in~@[ as ~a~]; the GLM Coding Plan key is saved, pick a model with /models"
                                   (nlk:json-value entry :string "email"))
                           :info)))))
         (serious-condition (condition)
           (say (format nil "zai-coding-plan: the sign-in failed: ~a" condition) :warning)))
    (finish-flow flow)))

(defun cancel-flow ()
  "End the sign-in in flight, if any, saying nothing."
  (let ((flow (bt2:with-lock-held (*flow-lock*) (shiftf *flow* nil))))
    (when flow
      (sb-concurrency:send-message (flow-mailbox flow) (list :cancel)))))

(defun start-login (&optional (auth-path nle::*auth-file-path*))
  "Start a sign-in that keeps its key at AUTH-PATH; => what the operator does next."
  (cancel-flow)
  (let* ((state (random-hex 16))
         (redirect-uri (overridden "ZAI_OAUTH_REDIRECT_URI" +redirect-uri+))
         (flow (make-flow state redirect-uri (authorize-url state redirect-uri) auth-path)))
    (bt2:with-lock-held (*flow-lock*) (setf *flow* flow))
    (setf (flow-thread flow)
          (bt2:make-thread (lambda () (run-flow flow)) :name "nodecode-zai-coding-plan sign-in"))
    (format nil "Open this address and sign in to Z.AI:~%~a~%~%Z.AI then sends the browser to ~a?code=...; copy that whole address (or the code in it) and paste it here as~%  /zai-coding-plan code <address or code>~%The sign-in waits ~d minutes."
            (flow-url flow) redirect-uri (floor +login-seconds+ 60))))

(defun paste (text)
  "Hand the sign-in in flight the code TEXT carries; => what happened."
  (let ((flow *flow*))
    (if (null flow)
        "No sign-in is waiting for a code: start one with /zai-coding-plan login."
        (multiple-value-bind (code state) (parse-callback-input text)
          (cond ((null code)
                 "That carries no code: paste the whole zcode://zai-auth/callback?... address, or the code in it.")
                ((and state (plusp (length state)) (string/= state (flow-state flow)))
                 "That address belongs to another sign-in (its state differs): paste the one this sign-in's page sent.")
                (t (sb-concurrency:send-message (flow-mailbox flow) (list :code code (or state "")))
                   "Code received: finishing the sign-in. The outcome follows as a notice."))))))

(defun status ()
  "Whether the plan is signed in, and as whom."
  (let ((entry (stored-entry (ignore-errors (nle::read-auth-file nle::*auth-file-path*)))))
    (cond (*flow* "A sign-in is waiting for its code: paste it with /zai-coding-plan code <address or code>.")
          ((nlk:json-value entry :text "access_token")
           (format nil "Signed in~@[ as ~a~]: the minted GLM Coding Plan key is in auth.json."
                   (nlk:json-value entry :string "email")))
          (t "Not signed in: /zai-coding-plan login, or save a key with /connect."))))

(defun logout ()
  "Forget the minted key."
  (cancel-flow)
  (save-entry nil)
  "Signed out: the minted key is gone from auth.json (it stays live at Z.AI until you delete it there).")

(defun run-command (args session-id)
  "/zai-coding-plan login | code TEXT | logout | status."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (space (position #\Space text))
         (verb (subseq text 0 space))
         (rest (if space (nlk:trimmed (subseq text space)) "")))
    (cond ((member verb '("" "status") :test #'string-equal) (status))
          ((string-equal verb "login") (start-login))
          ((string-equal verb "code") (paste rest))
          ((string-equal verb "logout") (logout))
          (t "Usage: /zai-coding-plan login | code <address or code> | logout | status"))))
