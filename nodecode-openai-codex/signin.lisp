;;;; signin.lisp --- signing in to ChatGPT, keeping the token, refreshing it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; auth/openai-codex.kdl (the oauth-code rule), ai/src/registry/oauth/
;;;; openai-codex.ts (the token's profile), registry/oauth/callback-server.ts
;;;; and pkce.ts (the loopback callback, the pasted fallback, PKCE),
;;;; registry/engine/oauth-code.ts, common.ts and refresh.ts (the exchange
;;;; and the refresh grant).
;;;;
;;;; The sign-in is OpenAI's authorization-code grant with PKCE. OpenAI
;;;; allowlists exactly one redirect for this client,
;;;; http://localhost:1455/auth/callback, so the callback listens on port 1455
;;;; or the sign-in fails: a busy port is said, never worked around with
;;;; another one. A browser on another machine cannot reach this one's
;;;; loopback, so the address it ends on can be pasted back instead
;;;; (/openai-codex code ADDRESS), as omp's manual input race allows.
;;;;
;;;; The token lives in the shared auth.json under oauth_tokens.openai-codex:
;;;; access_token, refresh_token, id_token, expires_at (epoch seconds),
;;;; account_id (the ChatGPT workspace), email, org_id, org_name (the plan),
;;;; installation_id. It is written the way the core writes api_keys: read,
;;;; one entry set, the file replaced atomically at mode 0600.

(in-package #:nodecode-openai-codex)

(defparameter +client-id+ "app_EMoamEEZ73f0CkXaXp7hrann"
  "The Codex CLI's public OAuth client.")

(defparameter +authorize-url+ "https://auth.openai.com/oauth/authorize")

(defparameter +token-url+ "https://auth.openai.com/oauth/token")

(defparameter +scopes+
  '("openid" "profile" "email" "offline_access" "api.connectors.read" "api.connectors.invoke")
  "What the sign-in asks for, space-joined on the wire.")

(defparameter +callback-path+ "/auth/callback")

(defvar *callback-port* 1455
  "The port the callback listens on: OpenAI's one allowlisted redirect. A
test binds 0, a free port, and the redirect follows it.")

(defparameter +token-seconds+ 15
  "How long one exchange with the token endpoint may take (omp's timeout-ms 15000).")

(defparameter +login-seconds+ 300
  "How long a sign-in waits for its browser (omp's callback timeout).")

(defparameter +refresh-margin+ 60
  "A token expiring within this many seconds is refreshed before it is sent.")

;;; A sign-in that cannot go on says why with FAIL, which signals
;;; OPENAI-CODEX-ERROR (both from NLK:DEFINE-PERIPHERAL in package.lisp).

;;; --- the store ---------------------------------------------------------------------

(defvar *store-lock* (bt2:make-lock :name "openai-codex store")
  "Held across a read, refresh and write of this cell's entry, so two
resolutions never spend one refresh token twice.")

(defun stored-entry (auth)
  "The oauth_tokens.openai-codex entry of the parsed store AUTH, or NIL."
  (let ((entry (nlk:json-value auth :object "oauth_tokens" +provider+)))
    (and (nlk:json-value entry :text "access_token") entry)))

(defun save-entry (path entry)
  "Write ENTRY as oauth_tokens.openai-codex of the auth.json at PATH, or take
it out when ENTRY is NIL; every other field of the file is kept."
  (let* ((path (merge-pathnames path))
         (auth (or (nle::read-auth-file path) (make-hash-table :test #'equal)))
         (tokens (or (nlk:json-value auth :object "oauth_tokens")
                     (setf (gethash "oauth_tokens" auth) (make-hash-table :test #'equal)))))
    (if entry
        (setf (gethash +provider+ tokens) entry)
        (remhash +provider+ tokens))
    (nlk:write-file-atomically path (shasht:write-json auth nil) :mode #o600 :directory-mode #o700)
    entry))

;;; --- one exchange with the token endpoint --------------------------------------------

(defun body-string (body)
  "An HTTP BODY dexador answered, as text."
  (typecase body
    (string body)
    ((vector (unsigned-byte 8)) (sb-ext:octets-to-string body :external-format :utf-8))
    (null "")
    (t (princ-to-string body))))

(defun post (url content &key headers)
  "POST CONTENT to URL within +TOKEN-SECONDS+: (values JSON STATUS TEXT), any
status answered as a value; a transport failure signals OPENAI-CODEX-ERROR."
  (handler-case
      (sb-sys:with-deadline (:seconds +token-seconds+)
        (multiple-value-bind (body status)
            (handler-case (dex:post url :headers headers :content content
                                        :connect-timeout +token-seconds+
                                        :read-timeout +token-seconds+
                                        :use-connection-pool nil)
              (dex:http-request-failed (condition)
                (values (dex:response-body condition) (dex:response-status condition))))
          (let ((text (body-string body)))
            (values (ignore-errors (nlk:decode-json text)) status text))))
    (openai-codex-error (condition) (error condition))
    ((or error sb-sys:deadline-timeout) (condition)
      (fail "~a did not answer: ~a" url (nle:transport-failure-label condition url)))))

(defun form (&rest pairs)
  "PAIRS, alternating names and values, as an x-www-form-urlencoded body; a
NIL value is left out."
  ;; spaces as +, as URLSearchParams writes them
  (quri:url-encode-params (loop for (name value) on pairs by #'cddr
                                when value collect (cons name value))
                          :space-to-plus t))

(defparameter +form-headers+ '(("content-type" . "application/x-www-form-urlencoded")))

(defun describe-failure (status json text)
  "omp's formatOpenAICodexTokenEndpointError: STATUS and what the body says."
  (let ((reason (or (nlk:json-value json :text "error") (nlk:json-value json :text "error" "code")))
        (description (or (nlk:json-value json :text "error_description")
                         (nlk:json-value json :text "error" "message"))))
    (cond ((and reason description (string/= reason description))
           (format nil "~d ~a: ~a" status reason description))
          ((or reason description) (format nil "~d ~a" status (or reason description)))
          ((plusp (length (string-trim " " text)))
           (format nil "~d ~a" status (subseq text 0 (min 300 (length text)))))
          (t (format nil "~d" status)))))

(defun unix-expiry (expires-in)
  "The epoch second a token living EXPIRES-IN seconds from now expires."
  (+ (unix-seconds) (floor expires-in)))

(defun token-entry (json &key previous login)
  "The stored entry the token response JSON makes, over the PREVIOUS entry's
fields: omp's credential map, then the openai-codex-profile hook. A LOGIN
response must carry a refresh token and say whose account it is."
  (let ((access (nlk:json-value json :text "access_token"))
        (refresh (or (nlk:json-value json :text "refresh_token")
                     (nlk:json-value previous :text "refresh_token")))
        (expires-in (nlk:json-value json :number "expires_in"))
        (id-token (nlk:json-value json :text "id_token")))
    (alexandria:when-let (missing (cond ((null access) "access_token")
                                        ((and login (null refresh)) "refresh_token")
                                        ((null expires-in) "expires_in")))
      (fail "the token response is missing ~a" missing))
    (multiple-value-bind (account email plan) (token-profile access id-token)
      (when (and login (null account) (null email))
        (fail "the token names no ChatGPT account"))
      (let ((entry (nlk:copy-json-object previous)))
        (setf (gethash "provider" entry) +provider+
              (gethash "access_token" entry) access
              (gethash "refresh_token" entry) (or refresh "")
              (gethash "expires_at" entry) (unix-expiry expires-in))
        (when id-token (setf (gethash "id_token" entry) id-token))
        (alexandria:when-let (account (or account (nlk:json-value previous :text "account_id")))
          (setf (gethash "account_id" entry) account))
        (alexandria:when-let (email (or email (nlk:json-value previous :text "email")))
          (setf (gethash "email" entry) email))
        ;; the workspace and the plan are fixed at sign-in; a refresh keeps them
        (when login
          (when account (setf (gethash "org_id" entry) account))
          (when plan (setf (gethash "org_name" entry) plan)))
        (unless (nlk:json-value entry :text "installation_id")
          (setf (gethash "installation_id" entry) (uuid)))
        entry))))

(defun exchange-code (code verifier redirect &optional previous)
  "The entry the authorization CODE buys, sent with the PKCE VERIFIER and
the REDIRECT it was issued for."
  (multiple-value-bind (json status text)
      (post +token-url+ (form "grant_type" "authorization_code" "client_id" +client-id+
                              "code" code "code_verifier" verifier "redirect_uri" redirect)
            :headers +form-headers+)
    (unless (and (integerp status) (< status 300))
      (fail "token exchange failed: ~a" (describe-failure status json text)))
    (token-entry json :previous previous :login t)))

(defun refresh-entry (entry)
  "ENTRY with a fresh access token, bought with its refresh token (the
refresh rule: the standard grant, form-encoded)."
  (let ((refresh (nlk:json-value entry :text "refresh_token")))
    (unless refresh
      (fail "the saved sign-in has no refresh token; sign in again with /openai-codex login"))
    (multiple-value-bind (json status text)
        (post +token-url+ (form "grant_type" "refresh_token" "client_id" +client-id+
                                "refresh_token" refresh)
              :headers +form-headers+)
      (unless (and (integerp status) (< status 300))
        (fail "token refresh failed: ~a" (describe-failure status json text)))
      (token-entry json :previous entry))))

(defun entry-expiry (entry)
  "The epoch second ENTRY's access token expires: its expires_at, else the
token's own exp claim, else NIL."
  (or (nlk:json-value entry :integer "expires_at")
      (token-expiry (nlk:json-value entry :text "access_token"))))

(defun due-p (entry)
  "Whether ENTRY's token expires within +REFRESH-MARGIN+ seconds."
  (let ((expiry (entry-expiry entry)))
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
            (openai-codex-error (condition)
              (if (> (or (entry-expiry current) 0) (unix-seconds))
                  current
                  (error 'nle:credential-error
                         :detail (format nil "openai-codex: ~a; sign in again with /openai-codex login"
                                         condition)))))))))

;;; --- PKCE and the authorization address ------------------------------------------------

(defun base64url (octets)
  "OCTETS as unpadded base64url."
  (string-right-trim "." (cl-base64:usb8-array-to-base64-string octets :uri t)))

(defun sha256 (text)
  "The SHA-256 digest of TEXT's UTF-8 bytes, as octets."
  (let ((hex (subseq (nlk:sha256-text text) 7)))
    (coerce (loop for at from 0 below 64 by 2
                  collect (parse-integer hex :start at :end (+ at 2) :radix 16))
            '(vector (unsigned-byte 8)))))

(defun pkce ()
  "(values VERIFIER CHALLENGE): 96 random bytes base64url, and the S256 of it (omp's generatePKCE)."
  (let ((verifier (base64url (nlk:random-bytes 96))))
    (values verifier (base64url (sha256 verifier)))))

(defun fresh-state ()
  "The CSRF state a sign-in carries: 16 random bytes in hex."
  (format nil "~(~{~2,'0x~}~)" (coerce (nlk:random-bytes 16) 'list)))

(defun redirect-uri (port)
  "The redirect the authorization server returns to, on PORT."
  (format nil "http://localhost:~d~a" port +callback-path+))

(defun authorize-url (state challenge redirect)
  "The address the operator opens: the standard parameters, then the rule's
own (omp's DeclarativeOAuthCodeFlow.generateAuthUrl)."
  (format nil "~a?~a" +authorize-url+
          (form "client_id" +client-id+
                "response_type" "code"
                "redirect_uri" redirect
                "scope" (format nil "~{~a~^ ~}" +scopes+)
                "code_challenge" challenge
                "code_challenge_method" "S256"
                "state" state
                "id_token_add_organizations" "true"
                "codex_cli_simplified_flow" "true"
                "originator" (setting :originator))))

(defun parse-callback-input (text)
  "(values CODE STATE) out of what the operator pasted: the address the
browser ended on, its query alone, or the bare code with the state after a
# (omp's parseCallbackInput)."
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
  "One sign-in waiting for its browser."
  (listeners '())
  (port 0)
  (state "")
  (verifier "")
  (redirect "")
  (url "")
  (auth-path nil)
  ;; set by the callback or a paste; the thread exchanges it
  (code nil)
  (failure nil)
  (cancelled nil)
  (thread nil))

(defvar *login* nil
  "The sign-in waiting for its browser, or NIL.")

(defun open-listeners (port)
  "(values LISTENERS PORT): loopback listeners on PORT, the IPv4 one and, where
this host has one, the IPv6 one beside it (a browser resolves localhost to
either); a busy PORT signals OPENAI-CODEX-ERROR, since no other is allowlisted."
  (let ((v4 (handler-case (usocket:socket-listen "127.0.0.1" port :reuse-address t :backlog 8
                                                                  :element-type '(unsigned-byte 8))
              (error ()
                (fail "OAuth callback port ~d is in use. OpenAI accepts only http://localhost:~d~a ~
                       as this sign-in's redirect, so falling back to another port would be refused. ~
                       Free port ~d (stop the process holding it) and retry, or sign in with a device ~
                       code instead (the nodecode-openai-codex-device cell)"
                      port port +callback-path+ port)))))
    (let* ((actual (usocket:get-local-port v4))
           (v6 (ignore-errors (usocket:socket-listen "::1" actual :reuse-address t :backlog 8
                                                                  :element-type '(unsigned-byte 8)))))
      (values (remove nil (list v4 v6)) actual))))

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
                     ;; only a redirect carrying our state ends the sign-in: any
                     ;; local process could send the other kind
                     (when (equal state (login-state login))
                       (setf (login-failure login) text))
                     (respond stream 500 text)))
                  ((null code) (respond stream 500 "Missing authorization code."))
                  ((not (equal state (login-state login)))
                   (respond stream 500 "State mismatch: this is not the sign-in Nodecode started."))
                  (t (setf (login-code login) code)
                     (respond stream 200 "Signed in to ChatGPT. You can close this tab and return to Nodecode."))))))))

(defun serve-callback (login deadline)
  "Answer LOGIN's callback requests until a code, a failure, a cancel or
DEADLINE (a universal time)."
  (loop until (or (login-code login) (login-failure login) (login-cancelled login))
        do (when (> (get-universal-time) deadline)
             (setf (login-failure login)
                   (format nil "no browser came back within ~d minutes" (floor +login-seconds+ 60)))
             (return))
           (dolist (listener (handler-case (usocket:wait-for-input (login-listeners login)
                                                                   :timeout 0.25 :ready-only t)
                               (error () (sleep 0.25) '())))
             (handler-case
                 (let ((connection (usocket:socket-accept listener :element-type '(unsigned-byte 8))))
                   (unwind-protect
                        (sb-sys:with-deadline (:seconds 10)
                          (answer-callback login (usocket:socket-stream connection)))
                     (ignore-errors (usocket:socket-close connection))))
               ((or error sb-sys:deadline-timeout) () nil)))))

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
                    ;; a code echoed as CODE#STATE: the fragment is not part of the code
                    (let* ((code (login-code login))
                           (code (subseq code 0 (or (position #\# code) (length code))))
                           (path (login-auth-path login))
                           (previous (bt2:with-lock-held (*store-lock*)
                                       (stored-entry (ignore-errors (nle::read-auth-file path)))))
                           (entry (exchange-code code (login-verifier login) (login-redirect login)
                                                 ;; a new sign-in keeps only the installation id
                                                 (and previous
                                                      (nlk:json-object
                                                       :opt "installation_id"
                                                       (nlk:json-value previous :text "installation_id"))))))
                      (bt2:with-lock-held (*store-lock*) (save-entry path entry))
                      (say (format nil "openai-codex: signed in~@[ as ~a~]~@[ (~a)~]; /models lists the Codex models under openai-codex/"
                                   (nlk:json-value entry :text "email")
                                   (nlk:json-value entry :text "org_name")))))))
         (error (condition)
           (unless (login-cancelled login)
             (say (format nil "openai-codex: sign-in failed: ~a" condition) :warning))))
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
                                                    :name "openai-codex sign-in"))
        (format nil "Open this address in a browser to sign in to ChatGPT:~%~%~a~%~%~
                     This machine waits ~d minutes for the browser to return to ~a. ~
                     If the browser runs elsewhere, copy the address it ends on (a page that does ~
                     not load) and send /openai-codex code ADDRESS."
                (login-url login) (floor +login-seconds+ 60) redirect)))))

(defun paste-code (text)
  "Hand the waiting sign-in the code the operator pasted."
  (let ((login *login*))
    (unless login
      (fail "no sign-in is waiting; start one with /openai-codex login"))
    (multiple-value-bind (code state) (parse-callback-input text)
      (cond ((null code) (fail "no authorization code in that; paste the whole address the browser ended on"))
            ((and state (string/= state (login-state login)))
             (fail "that address belongs to another sign-in; paste the one this sign-in's browser ended on"))
            (t (setf (login-code login) code)
               "openai-codex: code received, finishing the sign-in")))))

;;; --- the slash command -----------------------------------------------------------------

(defun status-text (path)
  "What /openai-codex status says of the sign-in kept at PATH."
  (let ((entry (stored-entry (ignore-errors (nle::read-auth-file path)))))
    (if (null entry)
        (format nil "openai-codex: not signed in; /openai-codex login signs in with a browser~@[, ~a holds a token~]"
                (some (lambda (name) (and (nle::credential-env name) name)) +env+))
        (let ((expiry (entry-expiry entry)))
          (format nil "openai-codex: signed in~@[ as ~a~]~@[ (~a)~]~@[, workspace ~a~]~@[; ~a~]"
                  (nlk:json-value entry :text "email")
                  (nlk:json-value entry :text "org_name")
                  (nlk:json-value entry :text "account_id")
                  (and expiry
                       (if (> expiry (unix-seconds))
                           (format nil "the token is good for ~d more minutes, then refreshes"
                                   (floor (- expiry (unix-seconds)) 60))
                           "the token has expired and refreshes at the next round")))))))

(defun run-command (args session-id)
  "/openai-codex login | code ADDRESS | logout | status."
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
               "openai-codex: signed out; the token is gone from auth.json")
              ((member verb '("" "status") :test #'equal) (status-text path))
              (t "usage: /openai-codex login | code ADDRESS | logout | status"))
      (openai-codex-error (condition) (format nil "openai-codex: ~a" condition)))))
