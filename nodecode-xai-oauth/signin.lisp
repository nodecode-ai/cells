;;;; signin.lisp --- the xAI sign-in: a device code, a token, its refresh.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; auth/xai-oauth.kdl (itself adapted from NousResearch/hermes-agent),
;;;; run by the declarative engine of packages/ai/src/registry/engine/
;;;; (device-code.ts, refresh.ts, common.ts), the poller of registry/oauth/
;;;; device-code.ts, and the token-endpoint discovery and userinfo of
;;;; registry/oauth/xai-oauth.ts.
;;;;
;;;; RFC 8628 against https://auth.x.ai: POST the client id and the six
;;;; scopes to the device endpoint, show the operator the verification URL
;;;; and the user code, then poll the token endpoint until the grant
;;;; completes. The token endpoint is not fixed: it is read from xAI's OIDC
;;;; discovery document at every login and every refresh, and refused unless
;;;; it is https on x.ai or a subdomain of it, because every future refresh
;;;; token is sent there. After a token arrives, the userinfo endpoint names
;;;; the account's email and subject; a failure there costs only those.
;;;;
;;;; The token lives in the shared auth.json under oauth_tokens.xai-oauth,
;;;; written the way the core writes api_keys: read the file, set the one
;;;; entry, write it back atomically at 0600 with every other field kept.

(in-package #:nodecode-xai-oauth)

(defparameter +client-id+ "b1a00492-073a-47ea-816f-4c329264a828"
  "The Grok CLI's public OAuth client.")

(defparameter +scopes+ "openid profile email offline_access grok-cli:access api:access"
  "The scopes the device code is asked for, space-separated.")

(defparameter +device-url+ "https://auth.x.ai/oauth2/device/code")
(defparameter +discovery-url+ "https://auth.x.ai/.well-known/openid-configuration")
(defparameter +userinfo-url+ "https://auth.x.ai/oauth2/userinfo")

(defparameter +accept-json+ '(("Accept" . "application/json"))
  "The header the device and token requests carry; a refresh carries none.")

(defparameter +timeout+ 30
  "Seconds one sign-in exchange may take, the dial included.")

(defparameter +discovery-timeout+ 15
  "Seconds the discovery document and the userinfo answer may take.")

(defparameter +refresh-margin+ 60
  "A token that expires within this many seconds is refreshed before use.")

(defun now ()
  "The time as epoch seconds, the unit expires_at is kept in."
  (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0)))

;;; --- one exchange ----------------------------------------------------------------

(defmacro with-timeout ((seconds url) &body body)
  "BODY under a deadline of SECONDS, a lapse said as URL's silence."
  `(handler-case (sb-sys:with-deadline (:seconds ,seconds) ,@body)
     (sb-sys:deadline-timeout ()
       (error "~a did not answer within ~d s" ,url ,seconds))))

(defun answer (body status)
  "(values BODY STATUS) with BODY decoded as JSON, else its text."
  (let ((text (nlk:body-text body)))
    (values (or (ignore-errors (nlk:decode-json text)) text) status)))

(defun exchange (url params headers)
  "POST the form PARAMS (an alist) to URL with HEADERS. => (values BODY
STATUS): a refusal is an answer like any other; only a transport failure or
the deadline signals."
  (multiple-value-call #'answer
    (with-timeout (+timeout+ url)
      (handler-case
          (dex:post url :headers (append headers
                                         '(("Content-Type" . "application/x-www-form-urlencoded")))
                        :content (quri:url-encode-params params)
                        :connect-timeout +timeout+ :read-timeout +timeout+)
        (dex:http-request-failed (condition)
          (values (dex:response-body condition) (dex:response-status condition)))))))

(defun fetch (url headers)
  "GET URL with HEADERS. => (values BODY STATUS), as EXCHANGE answers."
  (multiple-value-call #'answer
    (with-timeout (+discovery-timeout+ url)
      (handler-case
          (dex:get url :headers headers
                       :connect-timeout +discovery-timeout+ :read-timeout +discovery-timeout+)
        (dex:http-request-failed (condition)
          (values (dex:response-body condition) (dex:response-status condition)))))))

(defun ok-p (status)
  "Whether STATUS is a success."
  (and (integerp status) (< status 400)))

(defun excerpt (body)
  "BODY as at most 500 characters of text, for a failure's words."
  (let ((text (if (stringp body) body (nlk:encode-json-object body))))
    (subseq text 0 (min 500 (length text)))))

(defun xai-host-p (url)
  "Whether URL is https on x.ai or a subdomain of it."
  (let ((uri (ignore-errors (quri:uri url))))
    (and uri
         (equal "https" (quri:uri-scheme uri))
         (let ((host (string-downcase (or (quri:uri-host uri) ""))))
           (or (equal host "x.ai") (uiop:string-suffix-p host ".x.ai"))))))

(defun token-url ()
  "The token endpoint xAI's discovery document names, pinned to x.ai."
  (multiple-value-bind (body status) (fetch +discovery-url+ +accept-json+)
    (unless (eql status 200)
      (error "xAI OIDC discovery returned status ~a" status))
    (unless (hash-table-p body)
      (error "xAI OIDC discovery response was not a JSON object"))
    (let ((endpoint (nlk:trimmed (or (nlk:json-value body :string "token_endpoint") ""))))
      (when (zerop (length endpoint))
        (error "xAI OIDC discovery response was missing token_endpoint"))
      (unless (xai-host-p endpoint)
        (error "Invalid xAI token_endpoint: ~a" endpoint))
      endpoint)))

;;; --- the token ---------------------------------------------------------------------

(defun jwt-claims (token)
  "The claims of the JWT TOKEN, unverified, or NIL when it is not one."
  (let* ((parts (and (stringp token) (uiop:split-string token :separator ".")))
         (text (and (= (length parts) 3) (second parts)))
         ;; base64url, unpadded: cl-base64's URI alphabet, padded with its dots
         (payload (and text
                       (ignore-errors
                        (cl-base64:base64-string-to-string
                         (concatenate 'string text (make-string (mod (- (length text)) 4)
                                                                :initial-element #\.))
                         :uri t)))))
    (let ((claims (and payload (ignore-errors (nlk:decode-json payload)))))
      (and (hash-table-p claims) claims))))

(defun scalar (value)
  "VALUE as a non-empty string when it is a string or a number, else NIL."
  (cond ((and (stringp value) (plusp (length value))) value)
        ((realp value) (princ-to-string value))))

(defun token-entry (body &optional previous)
  "The oauth_tokens entry the token answer BODY makes, PREVIOUS (the stored
entry, on a refresh) filling what the answer leaves out."
  (let ((access (nlk:json-value body :text "access_token"))
        (seconds (nlk:json-value body :number "expires_in")))
    (unless access
      (error "xai-oauth token response missing access token: ~a" (excerpt body)))
    (unless seconds
      (error "xai-oauth token response missing expires_in"))
    (let ((claims (jwt-claims access)))
      (nlk:json-object
       "access_token" access
       "refresh_token" (or (nlk:json-value body :text "refresh_token")
                           (nlk:json-value previous :string "refresh_token")
                           "")
       "expires_at" (+ (now) (round seconds))
       :opt "account_id" (or (and claims (scalar (gethash "sub" claims)))
                             (nlk:json-value previous :text "account_id"))
       :opt "email" (nlk:json-value previous :text "email")))))

(defun with-userinfo (entry)
  "ENTRY with the email and the subject xAI's userinfo names for its token;
a failure leaves ENTRY as it is."
  (multiple-value-bind (body status)
      (ignore-errors
       (fetch +userinfo-url+ `(("Authorization" . ,(format nil "Bearer ~a" (gethash "access_token" entry))))))
    (when (and (ok-p status) (hash-table-p body))
      (alexandria:when-let (email (scalar (gethash "email" body)))
        (setf (gethash "email" entry) email))
      (alexandria:when-let (sub (scalar (gethash "sub" body)))
        (setf (gethash "account_id" entry) sub)))
    entry))

(defvar *store-lock* (bt2:make-lock :name "xai-oauth auth store")
  "Held while the cell rewrites auth.json, so a refresh and a sign-in never
interleave their read and their write.")

(defun stored-entry (path)
  "The oauth_tokens entry the auth.json at PATH holds for xai-oauth, or NIL."
  (nlk:json-value (ignore-errors (nle::read-auth-file path)) :object "oauth_tokens" +provider+))

(defun save-entry (entry path)
  "Set oauth_tokens.xai-oauth in the auth.json at PATH to ENTRY, or take it
out when ENTRY is NIL, keeping every other field."
  (bt2:with-lock-held (*store-lock*)
    (let* ((path (merge-pathnames path))
           (auth (or (nle::read-auth-file path) (make-hash-table :test #'equal)))
           (tokens (or (nlk:json-value auth :object "oauth_tokens")
                       (setf (gethash "oauth_tokens" auth) (make-hash-table :test #'equal)))))
      (if entry
          (setf (gethash +provider+ tokens) entry)
          (remhash +provider+ tokens))
      (nlk:write-file-atomically path (shasht:write-json auth nil)
                                 :mode #o600 :directory-mode #o700)
      entry)))

(defun expiring-p (entry)
  "Whether ENTRY's token expires within +REFRESH-MARGIN+ seconds."
  (let ((at (nlk:json-value entry :number "expires_at")))
    (and at (< (- at (now)) +refresh-margin+))))

(defun refresh-entry (entry)
  "ENTRY refreshed with its refresh token: the refresh_token grant at the
discovered token endpoint, then userinfo."
  (let ((token (nlk:json-value entry :text "refresh_token")))
    (unless token
      (error 'nle:credential-error
             :detail "the xAI sign-in expired and holds no refresh token; run /xai-oauth login"))
    (multiple-value-bind (body status)
        (exchange (token-url)
                  `(("grant_type" . "refresh_token")
                    ("client_id" . ,+client-id+)
                    ("refresh_token" . ,token))
                  '())
      (unless (ok-p status)
        (error 'nle:credential-error
               :detail (format nil "xai-oauth token refresh failed: ~a ~a; run /xai-oauth login"
                               status (excerpt body))))
      (with-userinfo (token-entry body entry)))))

(defvar *refresh-lock* (bt2:make-lock :name "xai-oauth refresh")
  "Held across one refresh, so two rounds starting at once refresh once.")

(defun fresh-entry (path)
  "The stored entry at PATH, refreshed and written back first when it is
about to expire; another thread's refresh, landed while this one waited,
is used as it is."
  (bt2:with-lock-held (*refresh-lock*)
    (let ((entry (stored-entry path)))
      (if (and entry (expiring-p entry))
          (save-entry (refresh-entry entry) path)
          entry))))

(defun token-credential (op)
  "The credential the stored sign-in answers for the :CREDENTIAL op OP, or NIL."
  ;; Only a round refreshes: a probe (the auth state a picker shows) carries
  ;; no endpoint and must cost no network, so it answers the stored token as
  ;; it is.
  (let ((entry (nlk:json-value (getf op :auth) :object "oauth_tokens" +provider+)))
    (when (nlk:json-value entry :text "access_token")
      (when (and (expiring-p entry) (getf op :endpoint) (getf op :auth-path))
        (setf entry (fresh-entry (getf op :auth-path))))
      (nle:make-credential (gethash "access_token" entry) :oauth))))

;;; --- the device flow ---------------------------------------------------------------

(defstruct (flow (:copier nil))
  "One sign-in in progress."
  (user-code "" :type string)
  (url "" :type string)
  (device-code "" :type string)
  (token-url "" :type string)
  ;; seconds between polls, and the epoch second the code expires at (NIL: never)
  (interval 5)
  (deadline nil)
  (auth-path nil)
  (cancelled nil))

(defvar *flow* nil
  "The sign-in in progress, or NIL.")

(defun cancel-flow ()
  "Stop the sign-in in progress, if one is."
  (let ((flow (shiftf *flow* nil)))
    (when flow (setf (flow-cancelled flow) t))))

(defun start-flow (auth-path)
  "Ask for a device code, and find the token endpoint the poll will ask.
=> the FLOW that polls for it."
  (multiple-value-bind (body status)
      (exchange +device-url+ `(("client_id" . ,+client-id+) ("scope" . ,+scopes+)) +accept-json+)
    (unless (ok-p status)
      (error "xai-oauth device authorization failed: ~a ~a" status (excerpt body)))
    (let ((user-code (nlk:json-value body :text "user_code"))
          (device-code (nlk:json-value body :text "device_code"))
          (uri (nlk:json-value body :text "verification_uri"))
          (complete (nlk:json-value body :text "verification_uri_complete"))
          (interval (nlk:json-value body :number "interval"))
          (expires (nlk:json-value body :number "expires_in")))
      (unless (and user-code device-code uri)
        (error "xai-oauth device authorization response missing required fields"))
      (make-flow :user-code user-code :url (or complete uri) :device-code device-code
                 ;; resolved once, not at every poll
                 :token-url (token-url)
                 ;; RFC 8628's five seconds when the answer names none, never under one
                 :interval (max 1 (floor (or interval 5)))
                 :deadline (and expires (+ (now) expires))
                 :auth-path auth-path))))

(defun poll-once (flow)
  "Ask the token endpoint once. => :COMPLETE and the token answer, :PENDING,
:SLOW-DOWN, or :FAILED and why."
  (multiple-value-bind (body status)
      (exchange (flow-token-url flow)
                `(("grant_type" . "urn:ietf:params:oauth:grant-type:device_code")
                  ("client_id" . ,+client-id+)
                  ("device_code" . ,(flow-device-code flow)))
                +accept-json+)
    (let ((error (and (hash-table-p body) (gethash "error" body))))
      (cond ((and (ok-p status) (null error)) (values :complete body))
            ((equal error "authorization_pending") :pending)
            ((equal error "slow_down") :slow-down)
            ((equal error "expired_token")
             (values :failed "xai-oauth device code expired; restart the login"))
            ((equal error "access_denied")
             (values :failed "xai-oauth device authorization was denied"))
            (t (values :failed
                       (format nil "xai-oauth device token request failed: ~a~@[ ~a~]"
                               status (or (nlk:json-value body :string "error_description")
                                          (and (stringp error) error)))))))))

(defun pause (seconds flow)
  "Sleep SECONDS, or less once FLOW is cancelled."
  (loop with until = (+ (get-internal-real-time) (* seconds internal-time-units-per-second))
        while (and (< (get-internal-real-time) until) (not (flow-cancelled flow)))
        do (sleep 0.2)))

(defun poll-flow (flow)
  "Poll until the grant completes. => the oauth_tokens entry, or NIL when the
sign-in was cancelled; a refusal, an expired code or a transport failure
signals."
  ;; omp's pollOAuthDeviceCodeFlow: slow_down adds five seconds to the
  ;; interval, and a code past its expiry ends the poll with a timeout whose
  ;; words name clock drift when the server asked to slow down.
  (let ((slowed 0))
    (loop
      (when (flow-cancelled flow) (return nil))
      (when (and (flow-deadline flow) (>= (now) (flow-deadline flow)))
        (error (if (plusp slowed)
                   "device flow timed out after one or more slow_down responses; this is often clock drift in a VM, so sync its clock and try again"
                   "device flow timed out")))
      (multiple-value-bind (state value) (poll-once flow)
        (when (flow-cancelled flow) (return nil))
        (ecase state
          (:complete (return (with-userinfo (token-entry value))))
          (:failed (error "~a" value))
          (:slow-down (incf slowed) (incf (flow-interval flow) 5))
          (:pending)))
      (pause (if (flow-deadline flow)
                 (max 0 (min (flow-interval flow) (- (flow-deadline flow) (now))))
                 (flow-interval flow))
             flow))))

(defun finish-flow (flow)
  "Run FLOW to its end on this thread and say the outcome as a notice."
  (handler-case
      (let ((entry (poll-flow flow)))
        (when entry
          (save-entry entry (flow-auth-path flow))
          (nle:notice (format nil "xAI Grok: signed in~@[ as ~a~]"
                              (or (nlk:json-value entry :string "email")
                                  (nlk:json-value entry :string "account_id")))
                      :key +key+)))
    (serious-condition (condition)
      (unless (flow-cancelled flow)
        (nle:notice (format nil "xAI Grok: sign-in failed: ~a" condition)
                    :level :warning :key +key+))))
  (when (eq *flow* flow) (setf *flow* nil)))

;;; --- the command -------------------------------------------------------------------

(defun login ()
  "Start the device flow and answer what the operator must do; the poll runs
on a thread of its own."
  (cancel-flow)
  (let ((flow (start-flow (merge-pathnames nle::*auth-file-path*))))
    (setf *flow* flow)
    (bt2:make-thread (lambda () (finish-flow flow)) :name "xai-oauth sign-in")
    (format nil "xAI Grok sign-in (SuperGrok or X Premium+): open ~a~%Enter code: ~a~%Waiting in the background; the outcome comes as a notice."
            (flow-url flow) (flow-user-code flow))))

(defun logout ()
  "Forget the stored sign-in and stop one in progress."
  (cancel-flow)
  (save-entry nil (merge-pathnames nle::*auth-file-path*))
  (nle:notice nil :key +key+)
  "xAI Grok: signed out")

(defun status ()
  "Where the sign-in stands."
  (let ((flow *flow*)
        (entry (stored-entry (merge-pathnames nle::*auth-file-path*))))
    (cond (flow (format nil "xAI Grok: waiting for the code ~a at ~a"
                        (flow-user-code flow) (flow-url flow)))
          ((nlk:json-value entry :text "access_token")
           (let ((left (- (or (nlk:json-value entry :number "expires_at") 0) (now))))
             (format nil "xAI Grok: signed in~@[ as ~a~]; the token ~:[expired, and is refreshed at the next round~;expires in ~:*~d min~]"
                     (or (nlk:json-value entry :string "email")
                         (nlk:json-value entry :string "account_id"))
                     (and (plusp left) (ceiling left 60)))))
          (t "xAI Grok: not signed in; /xai-oauth login starts the sign-in"))))

(defun run-command (args session-id)
  "/xai-oauth ARGS: login, logout or status."
  (declare (ignore session-id))
  (let ((verb (string-downcase (or (first (uiop:split-string (nlk:trimmed (or args ""))
                                                             :separator '(#\Space #\Tab)))
                                   ""))))
    (cond ((equal verb "login") (login))
          ((equal verb "logout") (logout))
          ((member verb '("" "status") :test #'equal) (status))
          (t "usage: /xai-oauth login | logout | status"))))

(defun complete-command (text session-id)
  "What /xai-oauth's argument completes to while TEXT is typed."
  (declare (ignore session-id))
  (loop for verb in '("login" "logout" "status")
        when (uiop:string-prefix-p (string-downcase (nlk:trimmed (or text ""))) verb)
          collect (list :name verb :value verb)))
