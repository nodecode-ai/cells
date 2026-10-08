;;;; signin.lisp --- signing in to ChatGPT with a device code, keeping the token, refreshing it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; auth/openai-codex-device.kdl (a custom login, a refresh rule, and
;;;; store-as openai-codex), ai/src/registry/oauth/openai-codex.ts
;;;; (loginOpenAICodexDevice, exchangeCodeForToken, the token's profile) and
;;;; registry/engine/refresh.ts and common.ts (the refresh grant).
;;;;
;;;; For a machine with no browser, or none that can reach its loopback: this
;;;; machine asks OpenAI for a user code, the operator types it at
;;;; https://auth.openai.com/codex/device on any device, and this machine
;;;; polls until OpenAI hands back an authorization code and the PKCE verifier
;;;; it made for it, which buy the token as the browser sign-in's code does.
;;;; Nothing listens here.
;;;;
;;;; omp keeps the result as openai-codex, not under a name of its own: the
;;;; subscription is one, however it was signed in. So does this cell, in the
;;;; shared auth.json under oauth_tokens.openai-codex: access_token,
;;;; refresh_token, id_token, expires_at (epoch seconds), account_id, email,
;;;; org_id, org_name (the plan), installation_id, written the way the core
;;;; writes api_keys: read, one entry set, replaced atomically at mode 0600.

(in-package #:nodecode-openai-codex-device)

(defparameter +client-id+ "app_EMoamEEZ73f0CkXaXp7hrann"
  "The Codex CLI's public OAuth client.")

(defparameter +token-url+ "https://auth.openai.com/oauth/token")

(defparameter +usercode-url+ "https://auth.openai.com/api/accounts/deviceauth/usercode"
  "Where a device sign-in asks for its user code.")

(defparameter +poll-url+ "https://auth.openai.com/api/accounts/deviceauth/token"
  "Where a device sign-in asks whether its code was entered.")

(defparameter +device-page+ "https://auth.openai.com/codex/device"
  "Where the operator types the user code.")

(defparameter +device-redirect+ "https://auth.openai.com/deviceauth/callback"
  "The redirect the device grant's authorization code was issued for.")

(defparameter +token-seconds+ 15
  "How long one exchange with OpenAI may take (omp's TOKEN_REQUEST_TIMEOUT_MS).")

(defparameter +poll-margin+ 3
  "Seconds added to the interval OpenAI names (omp's DEVICE_POLL_SAFETY_MARGIN_MS).")

(defparameter +max-polls+ 120
  "How many times a sign-in asks before it gives up (omp's DEVICE_MAX_POLLS).")

(defparameter +refresh-margin+ 60
  "A token expiring within this many seconds is refreshed before it is sent.")

;;; A sign-in that cannot go on says why with FAIL, which signals
;;; OPENAI-CODEX-DEVICE-ERROR (both from NLK:DEFINE-PERIPHERAL in package.lisp).

;;; --- the store ---------------------------------------------------------------------

(defvar *store-lock* (bt2:make-lock :name "openai-codex-device store")
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
status answered as a value; a transport failure signals OPENAI-CODEX-DEVICE-ERROR."
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
    (openai-codex-device-error (condition) (error condition))
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
      (fail "the saved sign-in has no refresh token; sign in again with /openai-codex-device login"))
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
            (openai-codex-device-error (condition)
              (if (> (or (entry-expiry current) 0) (unix-seconds))
                  current
                  (error 'nle:credential-error
                         :detail (format nil "openai-codex-device: ~a; sign in again with /openai-codex-device login"
                                         condition)))))))))

;;; --- the device grant ------------------------------------------------------------------

(defun post-json (url object)
  "POST OBJECT as JSON to URL: POST's values."
  (post url (nlk:encode-json-object object) :headers '(("content-type" . "application/json"))))

(defstruct (login (:copier nil))
  "One device sign-in waiting for its code to be entered."
  (device-id "")
  (user-code "")
  ;; seconds between two polls: OpenAI's interval and the margin
  (interval 8)
  (auth-path nil)
  (cancelled nil)
  (thread nil))

(defvar *login* nil
  "The sign-in waiting for its code to be entered, or NIL.")

(defun pause (seconds)
  "Wait SECONDS between two polls."
  (sleep seconds))

(defun interval-seconds (value)
  "The poll interval OpenAI's VALUE names (a number or a numeral, else 5),
with the margin."
  (+ +poll-margin+
     (or (and (realp value) (plusp value) value)
         (and (stringp value) (ignore-errors (let ((n (parse-integer value))) (and (plusp n) n))))
         5)))

(defun request-user-code ()
  "(values DEVICE-AUTH-ID USER-CODE INTERVAL): OpenAI's answer to a new device sign-in."
  (multiple-value-bind (json status) (post-json +usercode-url+ (nlk:json-object "client_id" +client-id+))
    (unless (and (integerp status) (< status 300))
      (fail "device authorization initiation failed: ~a" status))
    (let ((device (nlk:json-value json :text "device_auth_id"))
          (code (nlk:json-value json :text "user_code")))
      (unless (and device code)
        (fail "the device authorization response is missing device_auth_id or user_code"))
      (values device code (interval-seconds (gethash "interval" json))))))

(defun poll (login)
  "Ask once whether LOGIN's code was entered: (values CODE VERIFIER) once it
was, NIL while it waits (OpenAI's 403 and 404)."
  (multiple-value-bind (json status)
      (post-json +poll-url+ (nlk:json-object "device_auth_id" (login-device-id login)
                                             "user_code" (login-user-code login)))
    (cond ((member status '(403 404)) nil)
          ((not (and (integerp status) (< status 300)))
           (fail "device token polling failed: ~a" status))
          (t (let ((code (nlk:json-value json :text "authorization_code"))
                   (verifier (nlk:json-value json :text "code_verifier")))
               (unless (and code verifier)
                 (fail "the device token response is missing authorization_code or code_verifier"))
               (values code verifier))))))

(defun say (text &optional (level :info))
  "Tell the operator TEXT, standing under this cell's key."
  (nle:notice text :level level :key +key+))

(defun finish-login (login)
  "LOGIN's thread: poll until the code is entered, exchange what comes back,
keep the token, say how it went."
  (unwind-protect
       (handler-case
           (loop for attempt from 0 below +max-polls+
                 do (pause (if (zerop attempt) (min (login-interval login) 5) (login-interval login)))
                    (when (login-cancelled login) (return))
                    (multiple-value-bind (code verifier) (poll login)
                      (when code
                        (let* ((path (login-auth-path login))
                               (previous (bt2:with-lock-held (*store-lock*)
                                           (stored-entry (ignore-errors (nle::read-auth-file path)))))
                               (entry (exchange-code code verifier +device-redirect+
                                                     ;; a new sign-in keeps only the installation id
                                                     (and previous
                                                          (nlk:json-object
                                                           :opt "installation_id"
                                                           (nlk:json-value previous :text "installation_id"))))))
                          (unless (login-cancelled login)
                            (bt2:with-lock-held (*store-lock*) (save-entry path entry))
                            (say (format nil "openai-codex-device: signed in~@[ as ~a~]~@[ (~a)~]; /models lists the Codex models under openai-codex/"
                                         (nlk:json-value entry :text "email")
                                         (nlk:json-value entry :text "org_name"))))
                          (return))))
                 finally (fail "the code was not entered in time; start again with /openai-codex-device login"))
         (error (condition)
           (unless (login-cancelled login)
             (say (format nil "openai-codex-device: sign-in failed: ~a" condition) :warning))))
    (when (eq *login* login) (setf *login* nil))))

(defun cancel-login ()
  "Stop the sign-in waiting for its code, if one is."
  (alexandria:when-let (login (shiftf *login* nil))
    (setf (login-cancelled login) t)))

(defun start-login (auth-path)
  "Ask OpenAI for a user code and answer what the operator does with it; the
polling runs on a thread of its own."
  (cancel-login)
  (multiple-value-bind (device code interval) (request-user-code)
    (let ((login (make-login :device-id device :user-code code :interval interval
                             :auth-path auth-path)))
      (setf *login* login
            (login-thread login) (bt2:make-thread (lambda () (finish-login login))
                                                  :name "openai-codex-device sign-in"))
      (format nil "On any device, open ~a and enter the code~%~%    ~a~%~%~
                   This machine checks every ~d seconds and finishes the sign-in once the code is entered."
              +device-page+ code interval))))

;;; --- the slash command -----------------------------------------------------------------

(defun status-text (path)
  "What /openai-codex-device status says of the sign-in kept at PATH."
  (let ((entry (stored-entry (ignore-errors (nle::read-auth-file path)))))
    (if (null entry)
        (format nil "openai-codex-device: not signed in; /openai-codex-device login signs in with a device code~@[, ~a holds a token~]"
                (some (lambda (name) (and (nle::credential-env name) name)) +env+))
        (let ((expiry (entry-expiry entry)))
          (format nil "openai-codex-device: signed in~@[ as ~a~]~@[ (~a)~]~@[, workspace ~a~]~@[; ~a~]"
                  (nlk:json-value entry :text "email")
                  (nlk:json-value entry :text "org_name")
                  (nlk:json-value entry :text "account_id")
                  (and expiry
                       (if (> expiry (unix-seconds))
                           (format nil "the token is good for ~d more minutes, then refreshes"
                                   (floor (- expiry (unix-seconds)) 60))
                           "the token has expired and refreshes at the next round")))))))

(defun run-command (args session-id)
  "/openai-codex-device login | logout | status."
  (declare (ignore session-id))
  (let* ((args (string-trim " " (or args "")))
         (space (position #\Space args))
         (verb (string-downcase (subseq args 0 (or space (length args)))))
         (path nle::*auth-file-path*))
    (handler-case
        (cond ((equal verb "login") (start-login path))
              ((equal verb "logout")
               (cancel-login)
               (bt2:with-lock-held (*store-lock*) (save-entry path nil))
               (say nil)
               "openai-codex-device: signed out; the openai-codex token is gone from auth.json")
              ((member verb '("" "status") :test #'equal) (status-text path))
              (t "usage: /openai-codex-device login | logout | status"))
      (openai-codex-device-error (condition) (format nil "openai-codex-device: ~a" condition)))))
