;;;; signin.lisp --- signing in to claude.ai, keeping the token, refreshing it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; auth/anthropic.kdl (the oauth-code rule and the refresh rule),
;;;; ai/src/registry/oauth/anthropic.ts (the anthropic-identity hook and the
;;;; claude_cli bootstrap), registry/oauth/callback-server.ts and pkce.ts
;;;; (the loopback callback, the pasted fallback, PKCE),
;;;; registry/engine/oauth-code.ts, common.ts and refresh.ts (the exchange,
;;;; the credential map, the refresh grant).
;;;;
;;;; The sign-in is claude.ai's authorization-code grant with PKCE. The
;;;; callback listens on port 54545, or on another free port when that one is
;;;; busy, as omp allows for this provider. claude.ai also shows the code on
;;;; its own page (code=true), so a browser on another machine is finished by
;;;; pasting it: /anthropic code CODE#STATE, or the address the browser ended on.
;;;;
;;;; The token lives in the shared auth.json under oauth_tokens.anthropic:
;;;; access_token, refresh_token, expires_at (epoch seconds, five minutes
;;;; early, the rule's skew), account_id, email, org_id, org_name,
;;;; installation_id. It is written the way the core writes api_keys: read,
;;;; one entry set, the file replaced atomically at mode 0600.

(in-package #:nodecode-anthropic)

(defparameter +client-id+
  (sb-ext:octets-to-string
   (cl-base64:base64-string-to-usb8-array "OWQxYzI1MGEtZTYxYi00NGQ5LTg4ZWQtNTk0NGQxOTYyZjVl")
   :external-format :utf-8)
  "Claude Code's public OAuth client, kept base64 as omp keeps it so secret
scanners stay quiet.")

(defparameter +authorize-url+ "https://claude.ai/oauth/authorize")

(defparameter +token-url+ "https://api.anthropic.com/v1/oauth/token")

(defparameter +bootstrap-url+ "https://api.anthropic.com/api/claude_cli/bootstrap"
  "Where the CLI asks which account and organization a token belongs to.")

(defparameter +scopes+
  '("org:create_api_key" "user:profile" "user:inference" "user:sessions:claude_code"
    "user:mcp_servers" "user:file_upload")
  "What the sign-in asks for: user:inference is what serves rounds.")

(defparameter +callback-path+ "/callback")

(defvar *callback-port* 54545
  "The port the callback tries first; a busy one gives way to a free one. A
test binds 0, a free port from the start.")

(defparameter +exchange-seconds+ 30
  "How long one exchange with the token or bootstrap endpoint may take.")

(defparameter +login-seconds+ 300
  "How long a sign-in waits for its browser or its pasted code.")

(defparameter +refresh-margin+ 60
  "A token expiring within this many seconds is refreshed before it is sent.")

(defparameter +expiry-skew+ 300
  "Seconds taken off a token's lifetime when it is saved (the rule's skew-ms 300000).")

;;; --- the store ---------------------------------------------------------------------

(defvar *store-lock* (bt2:make-lock :name "anthropic store")
  "Held across a read, refresh and write of this cell's entry, so two
resolutions never spend one refresh token twice.")

(defun stored-entry (auth)
  "The oauth_tokens.anthropic entry of the parsed store AUTH, or NIL."
  (let ((entry (nlk:json-value auth :object "oauth_tokens" +provider+)))
    (and (nlk:json-value entry :text "access_token") entry)))

(defun save-entry (path entry)
  "Write ENTRY as oauth_tokens.anthropic of the auth.json at PATH, or take it
out when ENTRY is NIL; every other field of the file is kept."
  (let* ((path (merge-pathnames path))
         (auth (or (nle::read-auth-file path) (make-hash-table :test #'equal)))
         (tokens (or (nlk:json-value auth :object "oauth_tokens")
                     (setf (gethash "oauth_tokens" auth) (make-hash-table :test #'equal)))))
    (if entry
        (setf (gethash +provider+ tokens) entry)
        (remhash +provider+ tokens))
    (nlk:write-file-atomically path (shasht:write-json auth nil) :mode #o600 :directory-mode #o700)
    entry))

;;; --- one exchange ------------------------------------------------------------------------

(defun body-string (body)
  "An HTTP BODY dexador answered, as text."
  (typecase body
    (string body)
    ((vector (unsigned-byte 8)) (sb-ext:octets-to-string body :external-format :utf-8))
    (null "")
    (t (princ-to-string body))))

(defun exchange (method url &key content headers)
  "One METHOD (dex:post or dex:get) to URL within +EXCHANGE-SECONDS+:
(values JSON STATUS TEXT), any status answered as a value; a transport
failure signals ANTHROPIC-ERROR."
  (handler-case
      (sb-sys:with-deadline (:seconds +exchange-seconds+)
        (multiple-value-bind (body status)
            (handler-case (if (eq method :post)
                              (dex:post url :headers headers :content content
                                            :connect-timeout +exchange-seconds+
                                            :read-timeout +exchange-seconds+
                                            :use-connection-pool nil)
                              (dex:get url :headers headers
                                           :connect-timeout +exchange-seconds+
                                           :read-timeout +exchange-seconds+
                                           :use-connection-pool nil))
              (dex:http-request-failed (condition)
                (values (dex:response-body condition) (dex:response-status condition))))
          (let ((text (body-string body)))
            (values (ignore-errors (nlk:decode-json text)) status text))))
    (anthropic-error (condition) (error condition))
    ((or error sb-sys:deadline-timeout) (condition)
      (fail "~a did not answer: ~a" url (nle:transport-failure-label condition url)))))

(defun post-json (url object &optional headers)
  "POST OBJECT as JSON to URL: EXCHANGE's values."
  (exchange :post url :content (nlk:encode-json-object object)
                      :headers (append '(("content-type" . "application/json")) headers)))

(defun unix-seconds ()
  "Now, in seconds since 1970."
  (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0)))

(defun describe-failure (status text)
  "STATUS and the first of what the body said."
  (let ((text (string-trim '(#\Space #\Newline) (or text ""))))
    (format nil "~d~@[ ~a~]" status (and (plusp (length text)) (subseq text 0 (min 300 (length text)))))))

(defun bootstrap-identity (access)
  "(values ACCOUNT EMAIL ORG-ID ORG-NAME) the claude_cli bootstrap names for
ACCESS, or nothing when it cannot say (omp's fetchAnthropicBootstrapIdentity)."
  (ignore-errors
   (multiple-value-bind (json status)
       (exchange :get (format nil "~a?entrypoint=cli&model=claude-opus-4-8" +bootstrap-url+)
                 :headers `(("Accept" . "application/json, text/plain, */*")
                            ("Authorization" . ,(format nil "Bearer ~a" access))
                            ("Content-Type" . "application/json")
                            ("User-Agent" . ,(format nil "claude-code/~a" +claude-code-version+))
                            ("anthropic-beta" . "oauth-2025-04-20")))
     (when (and (integerp status) (< status 300))
       (flet ((field (key) (nlk:json-value json :text "oauth_account" key)))
         (values (field "account_uuid") (field "account_email")
                 (field "organization_uuid") (field "organization_name")))))))

(defun token-entry (json &key previous login)
  "The stored entry the token response JSON makes, over the PREVIOUS entry's
fields: the rule's credential map, then the anthropic-identity hook, which
asks the bootstrap for what the response left out. The organization is fixed
at sign-in: a refresh keeps the one PREVIOUS names."
  (let ((access (nlk:json-value json :text "access_token"))
        (expires-in (nlk:json-value json :number "expires_in")))
    (unless access
      (fail "the token response carried no access_token: ~a"
            (let ((text (nlk:encode-json-object json))) (subseq text 0 (min 300 (length text))))))
    (unless expires-in
      (fail "the token response is missing expires_in"))
    (let ((entry (nlk:copy-json-object previous))
          (account (nlk:json-value json :text "account" "uuid"))
          (email (nlk:json-value json :text "account" "email_address"))
          (org-id (and login (nlk:json-value json :text "organization" "uuid")))
          (org-name (and login (nlk:json-value json :text "organization" "name"))))
      (unless (and account email (or (not login) org-id))
        (multiple-value-bind (b-account b-email b-org-id b-org-name) (bootstrap-identity access)
          (setf account (or account b-account)
                email (or email b-email))
          (when login
            (setf org-id (or org-id b-org-id)
                  org-name (or org-name b-org-name)))))
      (setf (gethash "provider" entry) +provider+
            (gethash "access_token" entry) access
            (gethash "refresh_token" entry) (or (nlk:json-value json :text "refresh_token")
                                                (nlk:json-value previous :text "refresh_token")
                                                "")
            (gethash "expires_at" entry) (- (+ (unix-seconds) (floor expires-in)) +expiry-skew+))
      (when account (setf (gethash "account_id" entry) account))
      (when email (setf (gethash "email" entry) email))
      (when org-id (setf (gethash "org_id" entry) org-id))
      (when org-name (setf (gethash "org_name" entry) org-name))
      (unless (nlk:json-value entry :text "installation_id")
        (setf (gethash "installation_id" entry) (uuid)))
      entry)))

(defun exchange-code (code state verifier redirect &optional previous)
  "The entry the authorization CODE buys: the standard grant and the rule's
state, as JSON."
  (multiple-value-bind (json status text)
      (post-json +token-url+ (nlk:json-object "grant_type" "authorization_code"
                                              "client_id" +client-id+
                                              "code" code
                                              "redirect_uri" redirect
                                              "code_verifier" verifier
                                              "state" state))
    (unless (and (integerp status) (< status 300))
      (fail "token exchange failed: ~a" (describe-failure status text)))
    (token-entry json :previous previous :login t)))

(defun refresh-entry (entry)
  "ENTRY with a fresh access token (the refresh rule: the standard grant as
JSON, with the headers Claude Code sends on a refresh)."
  (let ((refresh (nlk:json-value entry :text "refresh_token")))
    (unless refresh
      (fail "the saved sign-in has no refresh token; sign in again with /anthropic login"))
    (multiple-value-bind (json status text)
        (post-json +token-url+ (nlk:json-object "grant_type" "refresh_token"
                                                "client_id" +client-id+
                                                "refresh_token" refresh)
                   `(("anthropic-beta" . "oauth-2025-04-20")
                     ("User-Agent" . ,(format nil "anthropic-sdk-typescript/~a userOAuthProvider"
                                              +sdk-version+))))
      (unless (and (integerp status) (< status 300))
        (fail "token refresh failed: ~a" (describe-failure status text)))
      (token-entry json :previous entry))))

(defun due-p (entry)
  "Whether ENTRY's token expires within +REFRESH-MARGIN+ seconds."
  (let ((expiry (nlk:json-value entry :integer "expires_at")))
    (and expiry (< (- expiry (unix-seconds)) +refresh-margin+))))

(defun fresh-entry (path entry)
  "ENTRY, refreshed and written back to the auth.json at PATH when it is due;
another thread's refresh is taken rather than repeated. A refresh that fails
keeps a token that has not expired yet, and is a credential error once it has."
  (bt2:with-lock-held (*store-lock*)
    (let ((current (or (and path (stored-entry (ignore-errors (nle::read-auth-file path)))) entry)))
      (if (not (due-p current))
          current
          (handler-case (let ((refreshed (refresh-entry current)))
                          (when path (save-entry path refreshed))
                          refreshed)
            (anthropic-error (condition)
              ;; saved five minutes early, so a little life is left past expires_at
              (if (> (+ (or (nlk:json-value current :integer "expires_at") 0) +expiry-skew+)
                     (unix-seconds))
                  current
                  (error 'nle:credential-error
                         :detail (format nil "anthropic: ~a; sign in again with /anthropic login"
                                         condition)))))))))

;;; --- PKCE and the authorization address ------------------------------------------------

(defun base64url (octets)
  "OCTETS as unpadded base64url."
  (string-right-trim "." (cl-base64:usb8-array-to-base64-string octets :uri t)))

(defun sha256 (text)
  "The SHA-256 digest of TEXT's UTF-8 bytes, as octets."
  (let ((hex (sha256-hex text)))
    (coerce (loop for at from 0 below 64 by 2
                  collect (parse-integer hex :start at :end (+ at 2) :radix 16))
            '(vector (unsigned-byte 8)))))

(defun pkce ()
  "(values VERIFIER CHALLENGE): 96 random bytes base64url, and the S256 of it."
  (let ((verifier (base64url (nlk:random-bytes 96))))
    (values verifier (base64url (sha256 verifier)))))

(defun fresh-state ()
  "The CSRF state a sign-in carries: 16 random bytes in hex."
  (format nil "~(~{~2,'0x~}~)" (coerce (nlk:random-bytes 16) 'list)))

(defun redirect-uri (port)
  "The redirect claude.ai returns to, on PORT."
  (format nil "http://localhost:~d~a" port +callback-path+))

(defun authorize-url (state challenge redirect)
  "The address the operator opens: the standard parameters, then the rule's code=true."
  (format nil "~a?~a" +authorize-url+
          (quri:url-encode-params `(("client_id" . ,+client-id+)
                                    ("response_type" . "code")
                                    ("redirect_uri" . ,redirect)
                                    ("scope" . ,(format nil "~{~a~^ ~}" +scopes+))
                                    ("code_challenge" . ,challenge)
                                    ("code_challenge_method" . "S256")
                                    ("state" . ,state)
                                    ("code" . "true"))
                                  :space-to-plus t)))

(defun parse-callback-input (text)
  "(values CODE STATE) out of what the operator pasted: the address the
browser ended on, its query alone, or the code with the state after a #
(omp's parseCallbackInput)."
  (let ((value (string-trim '(#\Space #\Tab #\Newline #\Return) (or text ""))))
    (flet ((query (query)
             (let ((params (ignore-errors (quri:url-decode-params query))))
               (values (cdr (assoc "code" params :test #'equal))
                       (cdr (assoc "state" params :test #'equal))))))
      (cond ((zerop (length value)) (values nil nil))
            ((ppcre:scan "^[a-zA-Z][a-zA-Z0-9+.-]*://" value)
             (query (or (ignore-errors (quri:uri-query (quri:uri value))) "")))
            ((search "code=" value) (query (string-left-trim "?#" value)))
            (t (let ((hash (position #\# value)))
                 (if hash
                     (values (subseq value 0 hash) (subseq value (1+ hash)))
                     (values value nil))))))))

;;; --- the loopback callback ---------------------------------------------------------------

(defstruct (login (:copier nil))
  "One sign-in waiting for its browser or its pasted code."
  (listeners '())
  (port 0)
  (state "")
  (verifier "")
  (redirect "")
  (url "")
  (auth-path nil)
  (code nil)
  (failure nil)
  (cancelled nil)
  (thread nil))

(defvar *login* nil
  "The sign-in waiting for its browser, or NIL.")

(defun listen-on (host port)
  "A loopback listener on HOST:PORT."
  (usocket:socket-listen host port :reuse-address t :backlog 8 :element-type '(unsigned-byte 8)))

(defun open-listeners (port)
  "(values LISTENERS PORT): loopback listeners on PORT, or on a free port when
PORT is busy, the IPv4 one and, where this host has one, the IPv6 one."
  (let* ((v4 (or (ignore-errors (listen-on "127.0.0.1" port))
                 (listen-on "127.0.0.1" 0)))
         (actual (usocket:get-local-port v4))
         (v6 (ignore-errors (listen-on "::1" actual))))
    (values (remove nil (list v4 v6)) actual)))

(defun close-listeners (login)
  "Stop LOGIN's listeners; a second call does nothing."
  (dolist (listener (shiftf (login-listeners login) '()))
    (ignore-errors (usocket:socket-close listener))))

(defun read-head (stream)
  "The request line and headers STREAM delivers, as text, or NIL at EOF."
  (let ((buffer (make-array 256 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for byte = (read-byte stream nil nil)
          do (unless byte (return-from read-head nil))
             (vector-push-extend byte buffer)
             (when (> (fill-pointer buffer) 16384)
               (return-from read-head nil))
          until (let ((end (fill-pointer buffer)))
                  (and (>= end 4)
                       (= 13 (aref buffer (- end 4))) (= 10 (aref buffer (- end 3)))
                       (= 13 (aref buffer (- end 2))) (= 10 (aref buffer (- end 1))))))
    (sb-ext:octets-to-string buffer :external-format :latin-1)))

(defun respond (stream status text)
  "Answer STATUS with a small page saying TEXT, and close the exchange."
  (let ((octets (sb-ext:string-to-octets
                 (format nil "<!doctype html><meta charset=\"utf-8\"><title>Nodecode</title>~
                              <body style=\"font-family:sans-serif;margin:3em\"><p>~a</p></body>"
                         text)
                 :external-format :utf-8)))
    (write-sequence (sb-ext:string-to-octets
                     (format nil "HTTP/1.1 ~d ~a~c~cContent-Type: text/html; charset=utf-8~c~c~
                                  Content-Length: ~d~c~cConnection: close~c~c~c~c"
                             status (if (= status 200) "OK" (if (= status 404) "Not Found" "Error"))
                             #\Return #\Linefeed #\Return #\Linefeed (length octets)
                             #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed)
                     :external-format :latin-1)
                    stream)
    (write-sequence octets stream)
    (finish-output stream)))

(defun answer-callback (login stream)
  "Read one request off STREAM and answer it: the redirect carrying the code,
or the reason it carries none (omp's handleCallback)."
  (let* ((head (or (read-head stream) (return-from answer-callback)))
         (target (second (uiop:split-string (subseq head 0 (or (position #\Return head) (length head)))
                                            :separator " ")))
         (uri (ignore-errors (quri:uri target)))
         (params (and uri (ignore-errors (quri:uri-query-params uri)))))
    (flet ((param (name) (cdr (assoc name params :test #'equal))))
      (if (not (and uri (equal (quri:uri-path uri) +callback-path+)))
          (respond stream 404 "Not found.")
          (let ((code (param "code"))
                (state (or (param "state") ""))
                (denied (param "error")))
            (cond (denied
                   (let ((text (format nil "Authorization failed: ~a"
                                       (or (param "error_description") denied))))
                     ;; only a redirect carrying our state ends the sign-in
                     (when (equal state (login-state login))
                       (setf (login-failure login) text))
                     (respond stream 500 text)))
                  ((null code) (respond stream 500 "Missing authorization code."))
                  ((not (equal state (login-state login)))
                   (respond stream 500 "State mismatch: this is not the sign-in Nodecode started."))
                  (t (setf (login-code login) code)
                     (respond stream 200 "Signed in to Claude. You can close this tab and return to Nodecode."))))))))

(defun serve-callback (login deadline)
  "Answer LOGIN's callback requests until a code, a failure, a cancel or
DEADLINE (a universal time)."
  (loop until (or (login-code login) (login-failure login) (login-cancelled login))
        do (when (> (get-universal-time) deadline)
             (setf (login-failure login)
                   (format nil "no code came back within ~d minutes" (floor +login-seconds+ 60)))
             (return))
           (if (login-listeners login)
               (dolist (listener (handler-case (usocket:wait-for-input (login-listeners login)
                                                                       :timeout 0.25 :ready-only t)
                                   (error () (sleep 0.25) '())))
                 (handler-case
                     (let ((connection (usocket:socket-accept listener :element-type '(unsigned-byte 8))))
                       (unwind-protect
                            (sb-sys:with-deadline (:seconds 10)
                              (answer-callback login (usocket:socket-stream connection)))
                         (ignore-errors (usocket:socket-close connection))))
                   ((or error sb-sys:deadline-timeout) () nil)))
               (sleep 0.25))))

(defun say (text &optional (level :info))
  "Tell the operator TEXT, standing under this cell's key."
  (nle:notice text :level level :key +key+))

(defun finish-login (login)
  "LOGIN's thread: wait for the code, exchange it, keep the token, say how it went."
  (unwind-protect
       (handler-case
           (progn
             (serve-callback login (+ (get-universal-time) +login-seconds+))
             (close-listeners login)
             (cond ((login-cancelled login))
                   ((login-failure login) (fail "~a" (login-failure login)))
                   (t
                    ;; claude.ai may echo CODE#STATE: the fragment is the state
                    (let* ((raw (login-code login))
                           (hash (position #\# raw))
                           (code (subseq raw 0 (or hash (length raw))))
                           (state (or (and hash (plusp (- (length raw) hash 1)) (subseq raw (1+ hash)))
                                      (login-state login)))
                           (path (login-auth-path login))
                           (previous (bt2:with-lock-held (*store-lock*)
                                       (stored-entry (ignore-errors (nle::read-auth-file path)))))
                           (entry (exchange-code code state (login-verifier login) (login-redirect login)
                                                 ;; a new sign-in keeps only the installation id
                                                 (and previous
                                                      (nlk:json-object
                                                       :opt "installation_id"
                                                       (nlk:json-value previous :text "installation_id"))))))
                      (bt2:with-lock-held (*store-lock*) (save-entry path entry))
                      (say (format nil "anthropic: signed in~@[ as ~a~]~@[ (~a)~]; with no Anthropic key set, anthropic/ models run on the subscription"
                                   (nlk:json-value entry :text "email")
                                   (nlk:json-value entry :text "org_name")))))))
         (error (condition)
           (unless (login-cancelled login)
             (say (format nil "anthropic: sign-in failed: ~a" condition) :warning))))
    (close-listeners login)
    (when (eq *login* login) (setf *login* nil))))

(defun cancel-login ()
  "Stop the sign-in waiting for its browser, if one is."
  (alexandria:when-let (login (shiftf *login* nil))
    (setf (login-cancelled login) t)
    (close-listeners login)))

(defun start-login (auth-path)
  "Open the callback and answer what the operator does next; the rest of the
sign-in runs on a thread of its own."
  (cancel-login)
  (multiple-value-bind (verifier challenge) (pkce)
    (multiple-value-bind (listeners port) (open-listeners *callback-port*)
      (let* ((state (fresh-state))
             (redirect (redirect-uri port))
             (login (make-login :listeners listeners :port port :state state :verifier verifier
                                :redirect redirect :auth-path auth-path
                                :url (authorize-url state challenge redirect))))
        (setf *login* login
              (login-thread login) (bt2:make-thread (lambda () (finish-login login))
                                                    :name "anthropic sign-in"))
        (format nil "Open this address in a browser to sign in to Claude (Pro or Max):~%~%~a~%~%~
                     This machine waits ~d minutes for the browser to return to ~a. ~
                     If the browser cannot reach this machine, copy the code claude.ai shows ~
                     (or the address the browser ended on) and send /anthropic code CODE."
                (login-url login) (floor +login-seconds+ 60) redirect)))))

(defun paste-code (text)
  "Hand the waiting sign-in the code the operator pasted."
  (let ((login *login*))
    (unless login
      (fail "no sign-in is waiting; start one with /anthropic login"))
    (multiple-value-bind (code state) (parse-callback-input text)
      (cond ((null code) (fail "no authorization code in that; paste the code claude.ai shows"))
            ((and state (string/= state (login-state login)))
             (fail "that code belongs to another sign-in; start again with /anthropic login"))
            (t (setf (login-code login) code)
               "anthropic: code received, finishing the sign-in")))))

;;; --- the slash command -----------------------------------------------------------------

(defun status-text (path)
  "What /anthropic status says of the sign-in kept at PATH."
  (let ((entry (stored-entry (ignore-errors (nle::read-auth-file path))))
        (key (nle::env-credential +provider+ :anthropic)))
    (if (null entry)
        "anthropic: not signed in; /anthropic login signs in with a Claude Pro or Max account"
        (let ((expiry (nlk:json-value entry :integer "expires_at")))
          (format nil "anthropic: signed in~@[ as ~a~]~@[ (~a)~]~@[; ~a~]~:[~;; an Anthropic key in the environment outranks the sign-in~]"
                  (nlk:json-value entry :text "email")
                  (nlk:json-value entry :text "org_name")
                  (and expiry
                       (if (> expiry (unix-seconds))
                           (format nil "the token is good for ~d more minutes, then refreshes"
                                   (floor (- expiry (unix-seconds)) 60))
                           "the token has expired and refreshes at the next round"))
                  key)))))

(defun run-command (args session-id)
  "/anthropic login | code CODE | logout | status."
  (declare (ignore session-id))
  (let* ((args (string-trim " " (or args "")))
         (space (position #\Space args))
         (verb (string-downcase (subseq args 0 (or space (length args)))))
         (rest (if space (string-trim " " (subseq args space)) ""))
         (path nle::*auth-file-path*))
    (handler-case
        (cond ((equal verb "login") (start-login path))
              ((equal verb "code") (paste-code rest))
              ((equal verb "logout")
               (cancel-login)
               (bt2:with-lock-held (*store-lock*) (save-entry path nil))
               (say nil)
               "anthropic: signed out; the token is gone from auth.json")
              ((member verb '("" "status") :test #'equal) (status-text path))
              (t "usage: /anthropic login | code CODE | logout | status"))
      (anthropic-error (condition) (format nil "anthropic: ~a" condition)))))
