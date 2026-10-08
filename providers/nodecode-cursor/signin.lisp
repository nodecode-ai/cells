;;;; signin.lisp --- the Cursor browser sign-in, its refresh, and the kept token.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): ai/src/registry/oauth/cursor.ts and
;;;; its PKCE helper registry/oauth/pkce.ts, the `custom' login and refresh of
;;;; compat/rules/auth/cursor.kdl.
;;;;
;;;; The sign-in is Cursor's own deep-control poll, as the Cursor agent CLI
;;;; runs it: the client mints a PKCE verifier and a UUID, the operator opens
;;;; cursor.com/loginDeepControl with the challenge and the UUID and signs in
;;;; there, and the client polls api2.cursor.sh/auth/poll with the UUID and
;;;; the verifier until Cursor hands back an access token and a refresh token
;;;; (404 means not yet). The access token is a JWT: its exp, less omp's
;;;; five-minute skew, is when it expires, an hour from now when it carries
;;;; none. A refresh posts the refresh token to the IDE's OAuth client; Cursor
;;;; answers an access token only (the refresh token is kept), or 200 with
;;;; shouldLogout when it ended the session. The account's email comes from
;;;; its cursor.com profile, read with the token as the web session cookie;
;;;; a failure leaves it unknown.
;;;;
;;;; auth.json keeps the sign-in under oauth_tokens.cursor:
;;;;   {"access_token": T, "refresh_token": R, "expires_at": epoch seconds,
;;;;    "email": E?}

(in-package #:nodecode-cursor)

(defparameter +login-url+ "https://cursor.com/loginDeepControl")

(defparameter +poll-url+ "https://api2.cursor.sh/auth/poll")

(defparameter +refresh-url+ "https://api2.cursor.sh/oauth/token")

(defparameter +client-id+ "KbZUR41cY7W6zRSdpSUJ7I7mLYBKOCmB"
  "The OAuth client the Cursor IDE renews its login session with.")

(defparameter +profile-url+ "https://cursor.com/api/auth/me")

(defparameter +poll-max-attempts+ 150)

(defvar *poll-base-delay* 1
  "Seconds before the first poll; a 404 multiplies it by 1.2, up to the cap.")

(defparameter +poll-max-delay+ 10)

(defparameter +poll-backoff+ 1.2)

(defparameter +expiry-skew-seconds+ 300
  "Taken off the token's own exp when it is kept: omp's skew.")

(defparameter +refresh-skew-seconds+ 60
  "A kept token this close to its expiry is refreshed before it is sent.")

(defparameter +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))

(defun unix-now ()
  "Seconds since the Unix epoch."
  (- (get-universal-time) +unix-epoch+))

;;; --- one HTTP exchange -----------------------------------------------------------

(define-condition signin-failed (error)
  ((text :initarg :text :reader signin-failed-text))
  (:report (lambda (condition stream) (write-string (signin-failed-text condition) stream)))
  (:documentation "The sign-in or the refresh cannot go on, in words the operator reads."))

(define-condition signin-cancelled (error) ()
  (:documentation "A newer sign-in, a sign-out, or the cell stopping, ended this one."))

(defun refuse (control &rest arguments)
  "Signal SIGNIN-FAILED with CONTROL formatted over ARGUMENTS."
  (error 'signin-failed :text (apply #'format nil control arguments)))

(defun exchange (method url &key headers content (timeout 30))
  "(values TEXT STATUS) of one exchange with URL: a POST of CONTENT when METHOD
is :POST, else a GET. A refusal answers its status and body, not a signal."
  (handler-case
      (multiple-value-bind (body status)
          (if (eq method :post)
              (dex:post url :headers headers :content content
                            :connect-timeout timeout :read-timeout timeout)
              (dex:get url :headers headers :connect-timeout timeout :read-timeout timeout))
        (values (nlk:body-text body) status))
    (dex:http-request-failed (e)
      (values (nlk:body-text (ignore-errors (dex:response-body e))) (dex:response-status e)))))

(defun ok-p (status)
  "Whether STATUS is a 2xx."
  (and (integerp status) (<= 200 status 299)))

(defun json-of (text)
  "TEXT decoded, or NIL when it is not JSON."
  (and (stringp text) (plusp (length text)) (ignore-errors (nlk:decode-json text))))

;;; --- the token's own claims -------------------------------------------------------

(defun token-claims (token)
  "The payload of the JWT TOKEN, or NIL when it is not one."
  (let ((parts (uiop:split-string (or token "") :separator ".")))
    (when (= 3 (length parts))
      (let ((octets (unbase64 (second parts))))
        (and octets (json-of (text-of octets)))))))

(defun token-expiry (token)
  "When TOKEN is kept as expiring (getTokenExpiry): its exp less the skew,
else an hour from now."
  (let ((exp (nlk:json-value (token-claims token) :number "exp")))
    (if exp
        (- (round exp) +expiry-skew-seconds+)
        (+ (unix-now) 3600))))

(defun token-user-id (token)
  "The Cursor user id TOKEN names (extractCursorAccessTokenUserId): its sub,
after the `provider|' prefix when it carries one."
  (let ((sub (nlk:json-value (token-claims token) :string "sub")))
    (when sub
      (let* ((parts (uiop:split-string sub :separator "|"))
             (id (nlk:trimmed (if (> (length parts) 1) (second parts) sub))))
        (and (plusp (length id)) id)))))

(defun account-email (token)
  "The email of the account TOKEN signs in, read from its cursor.com profile
with TOKEN as the web session; NIL when the profile names another user or
cannot be read."
  (ignore-errors
   (let ((user (token-user-id token)))
     (when user
       (multiple-value-bind (text status)
           (exchange :get +profile-url+
                     :headers `(("Accept" . "application/json")
                                ("Cookie" . ,(format nil "WorkosCursorSessionToken=~a"
                                                     (quri:url-encode (format nil "~a::~a" user token)))))
                     :timeout 3)
         (let ((profile (and (ok-p status) (json-of text))))
           (and (equal user (nlk:json-value profile :string "sub"))
                (let ((email (nlk:trimmed (or (nlk:json-value profile :string "email") ""))))
                  (and (plusp (length email)) email)))))))))

;;; --- PKCE and the poll ---------------------------------------------------------------

(defun pkce ()
  "(values VERIFIER CHALLENGE): 96 random octets as base64url, and that
text's SHA-256 as base64url (generatePKCE)."
  (let ((verifier (base64url (nlk:random-bytes 96))))
    (values verifier (base64url (sha256-octets (utf8 verifier))))))

(defun login-url (challenge uuid)
  "The page the operator signs in on."
  (format nil "~a?~a" +login-url+
          (quri:url-encode-params `(("challenge" . ,challenge) ("uuid" . ,uuid)
                                    ("mode" . "login") ("redirectTarget" . "cli")))))

(defun wait-seconds (seconds cancelled)
  "Sleep SECONDS, looking at the thunk CANCELLED every twentieth of a second."
  (let ((until (+ (get-internal-real-time) (* seconds internal-time-units-per-second))))
    (loop while (< (get-internal-real-time) until)
          do (when (funcall cancelled) (error 'signin-cancelled))
             (sleep (min 0.05 (max 0 (/ (- until (get-internal-real-time)) internal-time-units-per-second)))))
    (when (funcall cancelled) (error 'signin-cancelled))))

(defun poll-tokens (uuid verifier cancelled)
  "Poll until Cursor hands over the sign-in's tokens (pollCursorAuth):
(values ACCESS REFRESH). A 404 is not yet, and backs off; anything else that
is no answer counts against three in a row."
  (let ((delay *poll-base-delay*) (errors 0))
    (loop repeat +poll-max-attempts+ do
      (wait-seconds delay cancelled)
      (multiple-value-bind (text status)
          (handler-case
              (exchange :get (format nil "~a?uuid=~a&verifier=~a" +poll-url+ uuid verifier))
            (error () (values nil nil)))
        (cond ((eql status 404)
               (setf errors 0
                     delay (min (* delay +poll-backoff+) +poll-max-delay+)))
              ((and (ok-p status) (hash-table-p (json-of text)))
               (let ((data (json-of text)))
                 (return-from poll-tokens
                   (values (nlk:json-value data :string "accessToken")
                           (nlk:json-value data :string "refreshToken")))))
              ;; a refusal, a body that is no JSON, no answer at all
              ((>= (incf errors) 3)
               (refuse "Too many consecutive errors during Cursor auth polling")))))
    (refuse "Cursor authentication polling timeout")))

;;; --- auth.json ------------------------------------------------------------------

(defvar *store-lock* (bt2:make-recursive-lock :name "cursor auth.json")
  "Held across one read-modify-write of auth.json, and across a refresh.")

(defun save-entry (path entry)
  "Set oauth_tokens.cursor to ENTRY in the auth.json at PATH (NIL takes it
out), every other field kept; written atomically, mode 0600."
  (bt2:with-recursive-lock-held (*store-lock*)
    (let* ((auth (or (nle::read-auth-file path) (make-hash-table :test #'equal)))
           (tokens (or (nlk:json-value auth :object "oauth_tokens")
                       (setf (gethash "oauth_tokens" auth) (make-hash-table :test #'equal)))))
      (if entry
          (setf (gethash +provider+ tokens) entry)
          (remhash +provider+ tokens))
      (nlk:write-file-atomically path (shasht:write-json auth nil) :mode #o600 :directory-mode #o700)
      entry)))

(defun stored-entry (auth)
  "The sign-in the parsed auth.json AUTH keeps, or NIL."
  (let ((entry (nlk:json-value auth :object "oauth_tokens" +provider+)))
    (and (nlk:json-value entry :text "access_token") entry)))

(defun token-entry (access refresh &optional email)
  "The auth.json entry of a sign-in."
  (nlk:json-object "access_token" access
                   "refresh_token" refresh
                   "expires_at" (token-expiry access)
                   :opt "email" email))

(defun refreshed-entry (entry)
  "ENTRY with a fresh access token from Cursor (refreshCursorToken); the
refresh token is kept unless Cursor rotated it, and the email too, read now
when the entry keeps none."
  (let ((refresh (nlk:json-value entry :text "refresh_token")))
    (unless refresh
      (refuse "~a: the sign-in keeps no refresh token; run /~a login" +provider+ +provider+))
    (multiple-value-bind (text status)
        (exchange :post +refresh-url+
                  :headers '(("Content-Type" . "application/json"))
                  :content (nlk:encode-json-object
                            (nlk:json-object "grant_type" "refresh_token"
                                             "client_id" +client-id+
                                             "refresh_token" refresh)))
      (unless (ok-p status)
        (refuse "Cursor token refresh failed: ~a" (or text status)))
      (let ((data (json-of text)))
        ;; a session Cursor will not renew answers 200, no token, shouldLogout
        (when (eq t (nlk:json-value data :boolean "shouldLogout"))
          (refuse "invalid_grant: Cursor ended this session; run /~a login again" +provider+))
        (let ((access (nlk:json-value data :text "access_token")))
          (unless access
            (refuse "Cursor token refresh returned no access token"))
          (token-entry access
                       (or (nlk:json-value data :text "refresh_token") refresh)
                       (or (nlk:json-value entry :text "email") (account-email access))))))))

(defun due-p (entry)
  "Whether ENTRY's token expires within +REFRESH-SKEW-SECONDS+."
  (let ((expires (nlk:json-value entry :number "expires_at")))
    (and expires (< (- expires (unix-now)) +refresh-skew-seconds+))))

(defun fresh-entry (entry path)
  "ENTRY, refreshed and written back to the auth.json at PATH first when it is due."
  (if (not (due-p entry))
      entry
      (bt2:with-recursive-lock-held (*store-lock*)
        ;; another round may have refreshed it while this one waited
        (let ((current (or (stored-entry (ignore-errors (nle::read-auth-file path))) entry)))
          (if (due-p current)
              (save-entry path (refreshed-entry current))
              current)))))

;;; --- the sign-in ---------------------------------------------------------------------

(defstruct (flow (:copier nil))
  (thread nil)
  (cancelled nil))

(defvar *flow* nil
  "The sign-in in progress, or NIL.")

(defun cancel-flow ()
  "End the sign-in in progress, if any, and wait for its thread a moment."
  (let ((flow *flow*))
    (setf *flow* nil)
    (when flow
      (setf (flow-cancelled flow) t)
      (let ((thread (flow-thread flow)))
        (when (and thread (not (eq thread (bt2:current-thread))))
          (loop repeat 40 while (bt2:thread-alive-p thread) do (sleep 0.05)))))))

(defun sign-in (flow uuid verifier auth-path)
  "The background half of a sign-in: poll until Cursor answers, keep the
token, and say how it went."
  (let ((cancelled (lambda () (flow-cancelled flow))))
    (handler-case
        (multiple-value-bind (access refresh) (poll-tokens uuid verifier cancelled)
          (unless (and access refresh)
            (refuse "Cursor answered the poll without both tokens"))
          (let ((email (account-email access)))
            (when (funcall cancelled) (error 'signin-cancelled))
            (save-entry auth-path (token-entry access refresh email))
            ;; a failure said before stands no longer; the success is said once
            (nle:notice nil :key +key+)
            (nle:notice (format nil "~a: signed in~@[ as ~a~]" +provider+ email))))
      (signin-cancelled () nil)
      (error (e)
        (unless (funcall cancelled)
          (nle:notice (format nil "~a: sign-in failed: ~a" +provider+ e) :level :warning :key +key+))))
    (when (eq *flow* flow) (setf *flow* nil))))

(defun login (auth-path)
  "Start a sign-in: answer what the operator must do, finish in the background."
  (cancel-flow)
  (multiple-value-bind (verifier challenge) (pkce)
    (let* ((id (uuid))
           (flow (make-flow)))
      (setf *flow* flow
            (flow-thread flow) (bt2:make-thread (lambda () (sign-in flow id verifier auth-path))
                                                :name (format nil "~a sign-in" +provider+)))
      (format nil "Open this address in a browser and sign in to Cursor: ~a~%~
Nodecode waits for Cursor to confirm it, keeps the token and says so in a notice."
              (login-url challenge id)))))
