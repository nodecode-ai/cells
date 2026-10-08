;;;; signin.lisp --- signing in to a Snowflake account, keeping the token, refreshing it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; auth/snowflake.kdl (callback port 54551, a pasted code allowed),
;;;; ai/src/registry/oauth/snowflake.ts (the flow, the exchange, the refresh),
;;;; registry/oauth/callback-server.ts and pkce.ts (the loopback callback, its
;;;; port fallback, the pasted fallback, PKCE).
;;;;
;;;; The sign-in is Snowflake OAuth for local applications: the built-in
;;;; LOCAL_APPLICATION client, the authorization-code grant with PKCE, on the
;;;; account's own host. The account is asked first, which is why omp's login
;;;; is a custom hook: /snowflake login ACCOUNT names it here (else the
;;;; section's account, else SNOWFLAKE_ACCOUNT). The callback listens on
;;;; 127.0.0.1:54551 at /, and on any free port when that one is taken: the
;;;; local-application integration takes any loopback port. A browser on
;;;; another machine cannot reach this one's loopback, so the address it ends
;;;; on can be pasted back instead (/snowflake code ADDRESS).
;;;;
;;;; The token lives in the shared auth.json under oauth_tokens.snowflake:
;;;; access_token, refresh_token ("" when Snowflake issued none), expires_at
;;;; (epoch seconds) and account_url, written the way the core writes
;;;; api_keys: read, one entry set, the file replaced atomically at mode 0600.

(in-package #:nodecode-snowflake)

(defparameter +client-id+ "LOCAL_APPLICATION"
  "Snowflake's built-in OAuth client for local applications.")

(defparameter +callback-path+ "/"
  "Where the browser returns: some local-application integrations refuse a subpath.")

(defvar *callback-port* 54551
  "The port the callback listens on first; a test binds 0, a free port.")

(defparameter +token-seconds+ 15
  "How long one exchange with the token endpoint may take.")

(defparameter +login-seconds+ 300
  "How long a sign-in waits for its browser.")

(defparameter +refresh-margin+ 60
  "A token expiring within this many seconds is refreshed before it is sent.")

(defun unix-seconds ()
  "Now, in seconds since 1970."
  (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0)))

;;; --- the store ---------------------------------------------------------------------

(defvar *store-lock* (bt2:make-lock :name "snowflake store")
  "Held across a read, refresh and write of this cell's entry, so two
resolutions never spend one refresh token twice.")

(defun stored-entry (auth)
  "The oauth_tokens.snowflake entry of the parsed store AUTH, or NIL."
  (let ((entry (nlk:json-value auth :object "oauth_tokens" +provider+)))
    (and (nlk:json-value entry :text "access_token") entry)))

(defun save-entry (path entry)
  "Write ENTRY as oauth_tokens.snowflake of the auth.json at PATH, or take it
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

;;; --- one exchange with the token endpoint --------------------------------------------

(defun body-string (body)
  "An HTTP BODY dexador answered, as text."
  (typecase body
    (string body)
    ((vector (unsigned-byte 8)) (sb-ext:octets-to-string body :external-format :utf-8))
    (null "")
    (t (princ-to-string body))))

(defun form (&rest pairs)
  "PAIRS, alternating names and values, as an x-www-form-urlencoded body; a
NIL value is left out."
  (quri:url-encode-params (loop for (name value) on pairs by #'cddr
                                when value collect (cons name value))
                          :space-to-plus t))

(defun post-form (url content)
  "POST the form CONTENT to URL within +TOKEN-SECONDS+, following no
redirect (omp's redirect: error): (values JSON STATUS), any status answered
as a value; a transport failure signals SNOWFLAKE-ERROR."
  (handler-case
      (sb-sys:with-deadline (:seconds +token-seconds+)
        (multiple-value-bind (body status)
            (handler-case (dex:request url :method :post
                                           :headers '(("Content-Type" . "application/x-www-form-urlencoded"))
                                           :content content :max-redirects 0
                                           :connect-timeout +token-seconds+ :read-timeout +token-seconds+
                                           :use-connection-pool nil)
              (dex:http-request-failed (condition)
                (values (dex:response-body condition) (dex:response-status condition))))
          (values (ignore-errors (nlk:decode-json (body-string body))) status)))
    (snowflake-error (condition) (error condition))
    ((or error sb-sys:deadline-timeout) (condition)
      (fail "~a did not answer: ~a" url (nle:transport-failure-label condition url)))))

(defun token-entry (json status kind &optional previous)
  "The stored entry the token response JSON (answered with STATUS) makes,
over PREVIOUS: the access token, its expiry, and the refresh token, the one
before kept when Snowflake issues none (omp's parseTokenResponse)."
  (unless (and (integerp status) (< status 300))
    (fail "Snowflake token ~a failed (~a)" kind status))
  (let ((access (nlk:json-value json :text "access_token"))
        (expires-in (nlk:json-value json :number "expires_in"))
        (refresh (and (hash-table-p json) (gethash "refresh_token" json))))
    (unless (and (hash-table-p json) access expires-in (plusp expires-in)
                 (or (null refresh) (nlk:json-value json :text "refresh_token")))
      (fail "Snowflake returned an invalid token response"))
    (let ((entry (nlk:copy-json-object previous)))
      (setf (gethash "provider" entry) +provider+
            (gethash "access_token" entry) access
            (gethash "refresh_token" entry) (or (nlk:json-value json :text "refresh_token")
                                                (nlk:json-value previous :string "refresh_token")
                                                "")
            (gethash "expires_at" entry) (+ (unix-seconds) (floor expires-in)))
      entry)))

(defun exchange-code (account-url code verifier redirect)
  "The entry the authorization CODE buys on ACCOUNT-URL, sent with the PKCE
VERIFIER and the REDIRECT it was issued for."
  (multiple-value-bind (json status)
      (post-form (format nil "~a/oauth/token-request" account-url)
                 (form "grant_type" "authorization_code" "code" code "redirect_uri" redirect
                       "code_verifier" verifier "client_id" +client-id+))
    (token-entry json status "exchange" (nlk:json-object "account_url" account-url))))

(defun refresh-entry (entry)
  "ENTRY with a fresh access token, bought with its refresh token. One with
none stands until it expires (omp's refreshSnowflakeToken)."
  (let ((refresh (string-trim " " (or (nlk:json-value entry :string "refresh_token") ""))))
    (when (zerop (length refresh))
      (if (> (or (nlk:json-value entry :integer "expires_at") 0) (unix-seconds))
          (return-from refresh-entry entry)
          (fail "Snowflake did not issue a refresh token; run /snowflake login again or use ~
                 SNOWFLAKE_ACCOUNT and SNOWFLAKE_PAT")))
    (let ((account-url (handler-case (normalize-account-url (nlk:json-value entry :string "account_url"))
                         (snowflake-error ()
                           (fail "Invalid Snowflake account; run /snowflake login again")))))
      (multiple-value-bind (json status)
          (post-form (format nil "~a/oauth/token-request" account-url)
                     (form "grant_type" "refresh_token" "refresh_token" refresh "client_id" +client-id+))
        (token-entry json status "refresh" entry)))))

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
                          (when (and path (not (eq refreshed current))) (save-entry path refreshed))
                          refreshed)
            (snowflake-error (condition)
              (if (> (or (nlk:json-value current :integer "expires_at") 0) (unix-seconds))
                  current
                  (progn
                    (nle:notice (format nil "snowflake: ~a" condition) :level :warning :key +key+)
                    (error 'nle:credential-error
                           :detail (format nil "snowflake: ~a; sign in again with /snowflake login"
                                           condition))))))))))

;;; --- PKCE ----------------------------------------------------------------------------

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
  "The redirect Snowflake returns to, on PORT."
  (format nil "http://127.0.0.1:~d~a" port +callback-path+))

(defun authorize-url (account-url state challenge redirect)
  "The address the operator opens, its parameters in omp's order."
  (format nil "~a/oauth/authorize?~a" account-url
          (form "client_id" +client-id+
                "response_type" "code"
                "redirect_uri" redirect
                "scope" "refresh_token"
                "state" state
                "code_challenge" challenge
                "code_challenge_method" "S256")))

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
  (account-url "")
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

(defun listen-on (port)
  "A loopback listener on PORT, or NIL when PORT is taken."
  (ignore-errors (usocket:socket-listen "127.0.0.1" port :reuse-address t :backlog 8
                                                         :element-type '(unsigned-byte 8))))

(defun open-listener (port)
  "(values LISTENERS PORT): a 127.0.0.1 listener on PORT, else on a free port,
as omp's callback falls back when its preferred port is taken."
  (let ((listener (or (listen-on port) (listen-on 0)
                      (fail "no loopback port is free for the sign-in's callback"))))
    (values (list listener) (usocket:get-local-port listener))))

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
or the reason it carries none."
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
                     (respond stream 200 "Signed in to Snowflake. You can close this tab and return to Nodecode."))))))))

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
  "Tell the operator TEXT once."
  (nle:notice text :level level))

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
                    (let* ((code (login-code login))
                           (code (subseq code 0 (or (position #\# code) (length code))))
                           (entry (exchange-code (login-account-url login) code
                                                 (login-verifier login) (login-redirect login))))
                      (bt2:with-lock-held (*store-lock*) (save-entry (login-auth-path login) entry))
                      ;; a sign-in that worked clears what a failed refresh left standing
                      (nle:notice nil :key +key+)
                      (say (format nil "snowflake: signed in to ~a; /models lists the Cortex models under snowflake/"
                                   (login-account-url login)))))))
         (error (condition)
           (unless (login-cancelled login)
             (say (format nil "snowflake: sign-in failed: ~a" condition) :warning))))
    (close-listeners login)
    (when (eq *login* login) (setf *login* nil))))

(defun cancel-login ()
  "Stop the sign-in waiting for its browser, if one is."
  (alexandria:when-let (login (shiftf *login* nil))
    (setf (login-cancelled login) t)
    (close-listeners login)))

(defun start-login (account auth-path)
  "Open the callback for a sign-in to ACCOUNT and answer what the operator
does next; the rest of the sign-in runs on a thread of its own."
  (let ((account-url (normalize-account-url (if (plusp (length account)) account (configured-account)))))
    (cancel-login)
    (multiple-value-bind (verifier challenge) (pkce)
      (multiple-value-bind (listeners port) (open-listener *callback-port*)
        (let* ((state (fresh-state))
               (redirect (redirect-uri port))
               (login (make-login :listeners listeners :port port :account-url account-url
                                  :state state :verifier verifier :redirect redirect :auth-path auth-path
                                  :url (authorize-url account-url state challenge redirect))))
          (setf *login* login
                (login-thread login) (bt2:make-thread (lambda () (finish-login login))
                                                      :name "snowflake sign-in"))
          (format nil "Open this address in a browser, sign in to Snowflake and approve access:~%~%~a~%~%~
                       This machine waits ~d minutes for the browser to return to ~a. ~
                       If the browser runs elsewhere, copy the address it ends on (a page that does ~
                       not load) and send /snowflake code ADDRESS."
                  (login-url login) (floor +login-seconds+ 60) redirect))))))

(defun paste-code (text)
  "Hand the waiting sign-in the code the operator pasted."
  (let ((login *login*))
    (unless login
      (fail "no sign-in is waiting; start one with /snowflake login ACCOUNT"))
    (multiple-value-bind (code state) (parse-callback-input text)
      (cond ((null code) (fail "no authorization code in that; paste the whole address the browser ended on"))
            ((and state (string/= state (login-state login)))
             (fail "that address belongs to another sign-in; paste the one this sign-in's browser ended on"))
            (t (setf (login-code login) code)
               "snowflake: code received, finishing the sign-in")))))

;;; --- the slash command -----------------------------------------------------------------

(defun status-text (path)
  "What /snowflake status says of the sign-in kept at PATH."
  (let ((entry (stored-entry (ignore-errors (nle::read-auth-file path)))))
    (if (null entry)
        (format nil "snowflake: not signed in; /snowflake login ACCOUNT signs in with a browser~@[, ~a holds a PAT~]"
                (and (env-key) (first +env+)))
        (let ((expiry (nlk:json-value entry :integer "expires_at")))
          (format nil "snowflake: signed in to ~a~@[; ~a~]"
                  (nlk:json-value entry :string "account_url")
                  (and expiry
                       (cond ((> expiry (unix-seconds))
                              (format nil "the token is good for ~d more minutes~:[, and no refresh token was issued~;, then refreshes~]"
                                      (floor (- expiry (unix-seconds)) 60)
                                      (plusp (length (or (nlk:json-value entry :string "refresh_token") "")))))
                             (t "the token has expired and refreshes at the next round"))))))))

(defun run-command (args session-id)
  "/snowflake login [ACCOUNT] | code ADDRESS | logout | status."
  (declare (ignore session-id))
  (let* ((args (string-trim " " (or args "")))
         (space (position #\Space args))
         (verb (string-downcase (subseq args 0 (or space (length args)))))
         (rest (if space (string-trim " " (subseq args space)) ""))
         (path nle::*auth-file-path*))
    (handler-case
        (cond ((equal verb "login") (start-login rest path))
              ((equal verb "code") (paste-code rest))
              ((equal verb "logout")
               (cancel-login)
               (bt2:with-lock-held (*store-lock*) (save-entry path nil))
               (nle:notice nil :key +key+)
               "snowflake: signed out; the token is gone from auth.json")
              ((member verb '("" "status") :test #'equal) (status-text path))
              (t "usage: /snowflake login [ACCOUNT] | code ADDRESS | logout | status"))
      (snowflake-error (condition) (format nil "snowflake: ~a" condition)))))

(defun complete-command (text session-id)
  "What /snowflake's argument completes to while it is typed."
  (declare (ignore session-id))
  (let ((typed (string-left-trim " " (or text ""))))
    (loop for verb in '("login" "code" "logout" "status")
          when (uiop:string-prefix-p typed verb)
            collect (list :name verb :value verb))))
