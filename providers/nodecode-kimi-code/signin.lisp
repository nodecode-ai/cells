;;;; signin.lisp --- the Kimi Code sign-in: a device code, a token, its refresh.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; auth/kimi-code.kdl, run by the declarative engine of packages/ai/src/
;;;; registry/engine/ (device-code.ts, refresh.ts, common.ts) and the poller
;;;; of registry/oauth/device-code.ts.
;;;;
;;;; RFC 8628 against https://auth.kimi.com: POST the client id to the
;;;; device endpoint, show the operator the verification URL and the user
;;;; code, then poll the token endpoint at the interval the answer named
;;;; until the grant completes, is refused, or the code expires. Both
;;;; requests are forms and carry the Kimi CLI's fingerprint headers. The
;;;; token answer is the standard one: access_token, refresh_token,
;;;; expires_in; the account is the access token's user_id claim, else its
;;;; sub. A refresh is the refresh_token grant at the same token endpoint.
;;;;
;;;; The token lives in the shared auth.json under oauth_tokens.kimi-code,
;;;; written the way the core writes api_keys: read the file, set the one
;;;; entry, write it back atomically at 0600 with every other field kept.

(in-package #:nodecode-kimi-code)

(defparameter +client-id+ "17e5f671-d194-4dfb-9706-5516cb48c098"
  "The Kimi CLI's public OAuth client.")

(defparameter +oauth-host-env+ '("KIMI_CODE_OAUTH_HOST" "KIMI_OAUTH_HOST")
  "The environment variables that move the sign-in to another host, in order.")

(defparameter +timeout+ 30
  "Seconds one sign-in exchange may take, the dial included.")

(defparameter +refresh-margin+ 60
  "A token that expires within this many seconds is refreshed before use.")

(defun oauth-base ()
  "Where the sign-in is served: an override from the environment, else Kimi's."
  (or (some #'nle::credential-env +oauth-host-env+) "https://auth.kimi.com"))

(defun device-url () (format nil "~a/api/oauth/device_authorization" (oauth-base)))
(defun token-url () (format nil "~a/api/oauth/token" (oauth-base)))

(defun now ()
  "The time as epoch seconds, the unit expires_at is kept in."
  (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0)))

;;; --- one exchange ----------------------------------------------------------------

(defun exchange (url params headers)
  "POST the form PARAMS (an alist) to URL with HEADERS. => (values BODY
STATUS): BODY the answer decoded as JSON, else its text. A refusal is an
answer like any other; only a transport failure or the deadline signals."
  (multiple-value-bind (body status)
      (handler-case
          (sb-sys:with-deadline (:seconds +timeout+)
            (handler-case
                (dex:post url :headers (append headers
                                               '(("Content-Type" . "application/x-www-form-urlencoded")))
                              :content (quri:url-encode-params params)
                              :connect-timeout +timeout+ :read-timeout +timeout+)
              (dex:http-request-failed (condition)
                (values (dex:response-body condition) (dex:response-status condition)))))
        (sb-sys:deadline-timeout ()
          (error "~a did not answer within ~d s" url +timeout+)))
    (let ((text (nlk:body-text body)))
      (values (or (ignore-errors (nlk:decode-json text)) text) status))))

(defun ok-p (status)
  "Whether STATUS is a success."
  (and (integerp status) (< status 400)))

(defun excerpt (body)
  "BODY as at most 500 characters of text, for a failure's words."
  (let ((text (if (stringp body) body (nlk:encode-json-object body))))
    (subseq text 0 (min 500 (length text)))))

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

(defun claim (claims &rest names)
  "The first of NAMES CLAIMS carries as a string or a number, as a string."
  (loop for name in names
        for value = (gethash name claims)
        when (and (stringp value) (plusp (length value))) return value
        when (realp value) return (princ-to-string value)))

(defun token-entry (body &optional previous)
  "The oauth_tokens entry the token answer BODY makes, PREVIOUS (the stored
entry, on a refresh) filling what the answer leaves out: a refresh that does
not rotate keeps the refresh token, and the account stays the account."
  (let ((access (nlk:json-value body :text "access_token"))
        (seconds (nlk:json-value body :number "expires_in")))
    (unless access
      (error "kimi-code token response missing access token: ~a" (excerpt body)))
    (unless seconds
      (error "kimi-code token response missing expires_in"))
    (let ((claims (jwt-claims access)))
      (nlk:json-object
       "access_token" access
       "refresh_token" (or (nlk:json-value body :text "refresh_token")
                           (nlk:json-value previous :string "refresh_token")
                           "")
       "expires_at" (+ (now) (round seconds))
       :opt "account_id" (or (and claims (claim claims "user_id" "sub"))
                             (nlk:json-value previous :text "account_id"))))))

(defvar *store-lock* (bt2:make-lock :name "kimi-code auth store")
  "Held while the cell rewrites auth.json, so a refresh and a sign-in never
interleave their read and their write.")

(defun stored-entry (path)
  "The oauth_tokens entry the auth.json at PATH holds for Kimi Code, or NIL."
  (nlk:json-value (ignore-errors (nle::read-auth-file path)) :object "oauth_tokens" +provider+))

(defun save-entry (entry path)
  "Set oauth_tokens.kimi-code in the auth.json at PATH to ENTRY, or take it
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
  "ENTRY refreshed with its refresh token: the refresh_token grant."
  (let ((token (nlk:json-value entry :text "refresh_token")))
    (unless token
      (error 'nle:credential-error
             :detail "the Kimi Code sign-in expired and holds no refresh token; run /kimi-code login"))
    (multiple-value-bind (body status)
        (exchange (token-url)
                  `(("grant_type" . "refresh_token")
                    ("client_id" . ,+client-id+)
                    ("refresh_token" . ,token))
                  (client-headers))
      (unless (ok-p status)
        (error 'nle:credential-error
               :detail (format nil "kimi-code token refresh failed: ~a ~a; run /kimi-code login"
                               status (excerpt body))))
      (token-entry body entry))))

(defvar *refresh-lock* (bt2:make-lock :name "kimi-code refresh")
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
  "Ask for a device code. => the FLOW that polls for it."
  (multiple-value-bind (body status)
      (exchange (device-url) `(("client_id" . ,+client-id+)) (client-headers))
    (unless (ok-p status)
      (error "kimi-code device authorization failed: ~a ~a" status (excerpt body)))
    (let ((user-code (nlk:json-value body :text "user_code"))
          (device-code (nlk:json-value body :text "device_code"))
          (uri (nlk:json-value body :text "verification_uri"))
          (complete (nlk:json-value body :text "verification_uri_complete"))
          (interval (nlk:json-value body :number "interval"))
          (expires (nlk:json-value body :number "expires_in")))
      (unless (and user-code device-code uri)
        (error "kimi-code device authorization response missing required fields"))
      (make-flow :user-code user-code :url (or complete uri) :device-code device-code
                 ;; RFC 8628's five seconds when the answer names none, never under one
                 :interval (max 1 (floor (or interval 5)))
                 :deadline (and expires (+ (now) expires))
                 :auth-path auth-path))))

(defun poll-once (flow)
  "Ask the token endpoint once. => :COMPLETE and the token answer, :PENDING,
:SLOW-DOWN, or :FAILED and why."
  (multiple-value-bind (body status)
      (exchange (token-url)
                `(("grant_type" . "urn:ietf:params:oauth:grant-type:device_code")
                  ("client_id" . ,+client-id+)
                  ("device_code" . ,(flow-device-code flow)))
                (client-headers))
    (let ((error (and (hash-table-p body) (gethash "error" body))))
      (cond ((and (ok-p status) (null error)) (values :complete body))
            ((equal error "authorization_pending") :pending)
            ((equal error "slow_down") :slow-down)
            ((equal error "expired_token")
             (values :failed "kimi-code device code expired; restart the login"))
            ((equal error "access_denied")
             (values :failed "kimi-code device authorization was denied"))
            (t (values :failed
                       (format nil "kimi-code device token request failed: ~a~@[ ~a~]"
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
          (:complete (return (token-entry value)))
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
          (nle:notice (format nil "Kimi Code: signed in~@[ as ~a~]"
                              (nlk:json-value entry :string "account_id"))
                      :key +key+)))
    (serious-condition (condition)
      (unless (flow-cancelled flow)
        (nle:notice (format nil "Kimi Code: sign-in failed: ~a" condition)
                    :level :warning :key +key+))))
  (when (eq *flow* flow) (setf *flow* nil)))

;;; --- the command -------------------------------------------------------------------

(defun login ()
  "Start the device flow and answer what the operator must do; the poll runs
on a thread of its own."
  (cancel-flow)
  (let ((flow (start-flow (merge-pathnames nle::*auth-file-path*))))
    (setf *flow* flow)
    (bt2:make-thread (lambda () (finish-flow flow)) :name "kimi-code sign-in")
    (format nil "Kimi Code sign-in: open ~a~%Enter code: ~a~%Waiting in the background; the outcome comes as a notice."
            (flow-url flow) (flow-user-code flow))))

(defun logout ()
  "Forget the stored sign-in and stop one in progress."
  (cancel-flow)
  (save-entry nil (merge-pathnames nle::*auth-file-path*))
  (nle:notice nil :key +key+)
  "Kimi Code: signed out")

(defun status ()
  "Where the sign-in stands."
  (let ((flow *flow*)
        (entry (stored-entry (merge-pathnames nle::*auth-file-path*))))
    (cond (flow (format nil "Kimi Code: waiting for the code ~a at ~a"
                        (flow-user-code flow) (flow-url flow)))
          ((nlk:json-value entry :text "access_token")
           (let ((left (- (or (nlk:json-value entry :number "expires_at") 0) (now))))
             (format nil "Kimi Code: signed in~@[ as ~a~]; the token ~:[expired, and is refreshed at the next round~;expires in ~:*~d min~]"
                     (nlk:json-value entry :string "account_id")
                     (and (plusp left) (ceiling left 60)))))
          (t "Kimi Code: not signed in; /kimi-code login starts the sign-in"))))

(defun run-command (args session-id)
  "/kimi-code ARGS: login, logout or status."
  (declare (ignore session-id))
  (let ((verb (string-downcase (or (first (uiop:split-string (nlk:trimmed (or args ""))
                                                             :separator '(#\Space #\Tab)))
                                   ""))))
    (cond ((equal verb "login") (login))
          ((equal verb "logout") (logout))
          ((member verb '("" "status") :test #'equal) (status))
          (t "usage: /kimi-code login | logout | status"))))

(defun complete-command (text session-id)
  "What /kimi-code's argument completes to while TEXT is typed."
  (declare (ignore session-id))
  (loop for verb in '("login" "logout" "status")
        when (uiop:string-prefix-p (string-downcase (nlk:trimmed (or text ""))) verb)
          collect (list :name verb :value verb)))
