;;;; signin.lisp --- the Factory sign-in: a WorkOS device code, a token, its region, its refresh.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; auth/factory-droid.kdl, run by the declarative engine of packages/ai/
;;;; src/registry/engine/ (device-code.ts, refresh.ts, common.ts), the
;;;; poller of registry/oauth/device-code.ts, and the factory-droid-region
;;;; hook of registry/oauth/factory-droid.ts.
;;;;
;;;; RFC 8628 against WorkOS, Factory's identity provider: POST the client id
;;;; to user_management/authorize/device, show the operator the verification
;;;; URL and the user code, then poll user_management/authenticate until the
;;;; grant completes. The access token is a JWT: its exp is the expiry, its
;;;; external_org_id claim the Factory org (WorkOS's own organization_id is
;;;; internal and never sent as the org), its email and sub the account,
;;;; unless the answer's user object names them. Then Factory's whoami names
;;;; the org, the residency region (eu or global, which picks the host) and
;;;; the inference region (global, us or eu, which picks the upstream); a
;;;; login that resolves no org is refused. A refresh is the refresh_token
;;;; grant at the same endpoint, with the WorkOS organization the last answer
;;;; named, and asks whoami again; a whoami that fails then keeps what the
;;;; store had, so long as the org did not change.
;;;;
;;;; The token lives in the shared auth.json under oauth_tokens.factory-droid
;;;; (access_token, refresh_token, expires_at, email, account_id, org_id,
;;;; active_organization_id, region, inference_region), written the way the
;;;; core writes api_keys: read the file, set the one entry, write it back
;;;; atomically at 0600 with every other field kept.

(in-package #:nodecode-factory-droid)

(defparameter +client-id+ "client_01HNM792M5G5G1A2THWPXKFMXB"
  "The Factory CLI's public WorkOS client.")

(defparameter +device-url+ "https://api.workos.com/user_management/authorize/device")
(defparameter +token-url+ "https://api.workos.com/user_management/authenticate")

(defparameter +accept-json+ '(("Accept" . "application/json"))
  "The header every WorkOS request carries.")

(defparameter +timeout+ 15
  "Seconds one sign-in exchange may take, the dial included.")

(defparameter +fallback-lifetime+ 86400
  "Seconds a token without an exp claim is taken to live.")

(defparameter +refresh-margin+ 60
  "A token that expires within this many seconds is refreshed before use.")

(defun now ()
  "The time as epoch seconds, the unit expires_at is kept in."
  (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0)))

;;; --- one exchange ----------------------------------------------------------------

(defmacro with-timeout ((url) &body body)
  "BODY under the +TIMEOUT+ deadline, a lapse said as URL's silence."
  `(handler-case (sb-sys:with-deadline (:seconds +timeout+) ,@body)
     (sb-sys:deadline-timeout ()
       (error "~a did not answer within ~d s" ,url +timeout+))))

(defun answer (body status)
  "(values BODY STATUS) with BODY decoded as JSON, else its text."
  (let ((text (nlk:body-text body)))
    (values (or (ignore-errors (nlk:decode-json text)) text) status)))

(defun exchange (url params headers)
  "POST the form PARAMS (an alist) to URL with HEADERS. => (values BODY
STATUS): a refusal is an answer like any other; only a transport failure or
the deadline signals."
  (multiple-value-call #'answer
    (with-timeout (url)
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
    (with-timeout (url)
      (handler-case
          (dex:get url :headers headers :connect-timeout +timeout+ :read-timeout +timeout+)
        (dex:http-request-failed (condition)
          (values (dex:response-body condition) (dex:response-status condition)))))))

(defun ok-p (status)
  "Whether STATUS is a success."
  (and (integerp status) (< status 400)))

(defun excerpt (body)
  "BODY as at most 500 characters of text, for a failure's words."
  (let ((text (if (stringp body) body (nlk:encode-json-object body))))
    (subseq text 0 (min 500 (length text)))))

;;; --- the token ---------------------------------------------------------------------

(defun scalar (value)
  "VALUE as a non-empty string when it is a string or a number, else NIL."
  (cond ((and (stringp value) (plusp (length value))) value)
        ((realp value) (princ-to-string value))))

(defun token-entry (body &optional previous)
  "The oauth_tokens entry the WorkOS answer BODY makes, PREVIOUS (the stored
entry, on a refresh) filling what the answer leaves out."
  (let ((access (nlk:json-value body :text "access_token")))
    (unless access
      (error "factory-droid token response missing access token: ~a" (excerpt body)))
    (let* ((claims (jwt-claims access))
           (exp (and claims (realp (gethash "exp" claims)) (gethash "exp" claims))))
      (nlk:json-object
       "access_token" access
       "refresh_token" (or (nlk:json-value body :text "refresh_token")
                           (nlk:json-value previous :string "refresh_token")
                           "")
       "expires_at" (if exp (floor exp) (+ (now) +fallback-lifetime+))
       :opt "email" (or (scalar (nlk:json-value body :any "user" "email"))
                        (and claims (scalar (gethash "email" claims)))
                        (nlk:json-value previous :text "email"))
       :opt "account_id" (or (scalar (nlk:json-value body :any "user" "id"))
                             (and claims (scalar (gethash "sub" claims)))
                             (nlk:json-value previous :text "account_id"))
       :opt "org_id" (and claims (scalar (gethash "external_org_id" claims)))))))

(defun whoami (entry login)
  "ENTRY, a fresh token's entry, with the org and the regions Factory's
whoami names; PREVIOUS's scope carried where the org is the same. LOGIN
refuses what a refresh forgives."
  ;; omp's attachFactoryDroidRegion, phase login or refresh.
  (let ((org (nlk:json-value entry :text "org_id"))
        (region (nlk:json-value entry :text "region")))
    (multiple-value-bind (body status)
        (handler-case
            (fetch (format nil "~a/api/cli/whoami" (host region))
                   `(("Authorization" . ,(format nil "Bearer ~a" (gethash "access_token" entry)))
                     ,@+accept-json+
                     ,@(when org `(("X-Factory-Org-Id" . ,org)))))
          (error (condition)
            (if login (error condition) (values nil nil))))
      (when (and login (not (ok-p status)))
        (error "Factory identity check failed (~a): ~a" status (excerpt body)))
      (when (and (ok-p status) (hash-table-p body))
        (let* ((answered (or (nlk:json-value body :text "orgId") org))
               (carried (or (null answered) (null org) (equal answered org)))
               (region (let ((named (nlk:json-value body :string "region")))
                         (if (member named '("eu" "global") :test #'equal)
                             named
                             (and carried (nlk:json-value entry :text "region")))))
               (inference (let ((named (nlk:json-value body :string "inferenceRegion")))
                            (if (member named '("global" "eu" "us") :test #'equal)
                                named
                                (or (and carried (nlk:json-value entry :text "inference_region"))
                                    (if (equal region "eu") "eu" "global"))))))
          (when (and login (null answered))
            (error "Factory login did not resolve an organization"))
          (setf (gethash "org_id" entry) answered)
          (if region (setf (gethash "region" entry) region) (remhash "region" entry))
          (setf (gethash "inference_region" entry) inference)))
      (when (and login (null (nlk:json-value entry :text "org_id")))
        (error "Factory login did not resolve an organization"))
      entry)))

(defun identify (entry raw stored login)
  "ENTRY, the token answer RAW made, with the WorkOS organization, the Factory
org and the regions, carried from the STORED entry while the org stays the
same, then asked of whoami."
  (unless (and (nlk:json-value raw :string "refresh_token")
               (plusp (length (gethash "refresh_token" entry))))
    (error "Factory token response missing refresh token"))
  (let* ((selected (or (nlk:json-value raw :string "organization_id")
                       (nlk:json-value stored :text "active_organization_id")))
         (stored-org (nlk:json-value stored :text "org_id"))
         (org (nlk:json-value entry :text "org_id"))
         (changed (and org stored-org (not (equal org stored-org))))
         (same (and (not changed)
                    (or (null selected)
                        (null (nlk:json-value stored :text "active_organization_id"))
                        (equal selected (nlk:json-value stored :text "active_organization_id"))))))
    (when selected (setf (gethash "active_organization_id" entry) selected))
    (alexandria:when-let (org (or org (and same stored-org)))
      (setf (gethash "org_id" entry) org))
    (when same
      (alexandria:when-let (region (nlk:json-value stored :text "region"))
        (setf (gethash "region" entry) region))
      (alexandria:when-let (region (nlk:json-value stored :text "inference_region"))
        (setf (gethash "inference_region" entry) region)))
    (whoami entry login)))

(defvar *store-lock* (bt2:make-lock :name "factory-droid auth store")
  "Held while the cell rewrites auth.json, so a refresh and a sign-in never
interleave their read and their write.")

(defun stored-entry (path)
  "The oauth_tokens entry the auth.json at PATH holds for Factory, or NIL."
  (nlk:json-value (ignore-errors (nle::read-auth-file path)) :object "oauth_tokens" +provider+))

(defun save-entry (entry path)
  "Set oauth_tokens.factory-droid in the auth.json at PATH to ENTRY, or take
it out when ENTRY is NIL, keeping every other field."
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
  "ENTRY refreshed: the refresh_token grant with its WorkOS organization,
then whoami."
  (let ((token (nlk:json-value entry :text "refresh_token")))
    (unless token
      (error 'nle:credential-error
             :detail "the Factory sign-in expired and holds no refresh token; run /factory-droid login"))
    (multiple-value-bind (body status)
        (exchange +token-url+
                  `(("grant_type" . "refresh_token")
                    ("client_id" . ,+client-id+)
                    ("refresh_token" . ,token)
                    ,@(alexandria:when-let (org (nlk:json-value entry :text "active_organization_id"))
                        `(("organization_id" . ,org))))
                  +accept-json+)
      (unless (ok-p status)
        (error 'nle:credential-error
               :detail (format nil "factory-droid token refresh failed: ~a ~a; run /factory-droid login"
                               status (excerpt body))))
      (identify (token-entry body entry) body entry nil))))

(defvar *refresh-lock* (bt2:make-lock :name "factory-droid refresh")
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
  "The credential the stored sign-in answers for the :CREDENTIAL op OP, or
NIL: the WorkOS token, carrying the org and the regions its requests need."
  ;; Only a round refreshes: a probe (the auth state a picker shows) carries
  ;; no endpoint and must cost no network, so it answers the stored token as
  ;; it is.
  (let ((entry (nlk:json-value (getf op :auth) :object "oauth_tokens" +provider+)))
    (when (nlk:json-value entry :text "access_token")
      (when (and (expiring-p entry) (getf op :endpoint) (getf op :auth-path))
        (setf entry (fresh-entry (getf op :auth-path))))
      (nle:make-credential (gethash "access_token" entry) :oauth
                           (list :org-id (nlk:json-value entry :text "org_id")
                                 :region (nlk:json-value entry :text "region")
                                 :inference-region (nlk:json-value entry :text "inference_region"))))))

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
  "Ask WorkOS for a device code. => the FLOW that polls for it."
  (multiple-value-bind (body status)
      (exchange +device-url+ `(("client_id" . ,+client-id+)) +accept-json+)
    (unless (ok-p status)
      (error "factory-droid device authorization failed: ~a ~a" status (excerpt body)))
    (let ((user-code (nlk:json-value body :text "user_code"))
          (device-code (nlk:json-value body :text "device_code"))
          (uri (nlk:json-value body :text "verification_uri"))
          (complete (nlk:json-value body :text "verification_uri_complete"))
          (interval (nlk:json-value body :number "interval"))
          (expires (nlk:json-value body :number "expires_in")))
      (unless (and user-code device-code uri)
        (error "factory-droid device authorization response missing required fields"))
      (make-flow :user-code user-code :url (or complete uri) :device-code device-code
                 ;; RFC 8628's five seconds when the answer names none, never under one
                 :interval (max 1 (floor (or interval 5)))
                 :deadline (and expires (+ (now) expires))
                 :auth-path auth-path))))

(defun poll-once (flow)
  "Ask the token endpoint once. => :COMPLETE and the token answer, :PENDING,
:SLOW-DOWN, or :FAILED and why."
  (multiple-value-bind (body status)
      (exchange +token-url+
                `(("grant_type" . "urn:ietf:params:oauth:grant-type:device_code")
                  ("client_id" . ,+client-id+)
                  ("device_code" . ,(flow-device-code flow)))
                +accept-json+)
    (let ((error (and (hash-table-p body) (gethash "error" body))))
      (cond ((and (ok-p status) (null error)) (values :complete body))
            ((equal error "authorization_pending") :pending)
            ((equal error "slow_down") :slow-down)
            ((equal error "expired_token")
             (values :failed "factory-droid device code expired; restart the login"))
            ((equal error "access_denied")
             (values :failed "factory-droid device authorization was denied"))
            (t (values :failed
                       (format nil "factory-droid device token request failed: ~a~@[ ~a~]"
                               status (or (nlk:json-value body :string "error_description")
                                          (and (stringp error) error)))))))))

(defun pause (seconds flow)
  "Sleep SECONDS, or less once FLOW is cancelled."
  (loop with until = (+ (get-internal-real-time) (* seconds internal-time-units-per-second))
        while (and (< (get-internal-real-time) until) (not (flow-cancelled flow)))
        do (sleep 0.2)))

(defun poll-flow (flow)
  "Poll until the grant completes. => the token answer, or NIL when the
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
          (:complete (return value))
          (:failed (error "~a" value))
          (:slow-down (incf slowed) (incf (flow-interval flow) 5))
          (:pending)))
      (pause (if (flow-deadline flow)
                 (max 0 (min (flow-interval flow) (- (flow-deadline flow) (now))))
                 (flow-interval flow))
             flow))))

(defun finish-flow (flow)
  "Run FLOW to its end on this thread, resolve the org and the regions, and
say the outcome as a notice."
  (handler-case
      (let ((body (poll-flow flow)))
        (when body
          (let ((entry (identify (token-entry body) body nil t)))
            (unless (flow-cancelled flow)
              (save-entry entry (flow-auth-path flow))
              (nle:notice (format nil "Factory Droid: signed in~@[ as ~a~]~@[, ~a region~]"
                                  (or (nlk:json-value entry :string "email")
                                      (nlk:json-value entry :string "account_id"))
                                  (nlk:json-value entry :string "region"))
                          :key +key+)))))
    (serious-condition (condition)
      (unless (flow-cancelled flow)
        (nle:notice (format nil "Factory Droid: sign-in failed: ~a" condition)
                    :level :warning :key +key+))))
  (when (eq *flow* flow) (setf *flow* nil)))

;;; --- the command -------------------------------------------------------------------

(defun login ()
  "Start the device flow and answer what the operator must do; the poll runs
on a thread of its own."
  (cancel-flow)
  (let ((flow (start-flow (merge-pathnames nle::*auth-file-path*))))
    (setf *flow* flow)
    (bt2:make-thread (lambda () (finish-flow flow)) :name "factory-droid sign-in")
    (format nil "Factory Droid sign-in: open ~a~%Enter code: ~a~%Waiting in the background; the outcome comes as a notice."
            (flow-url flow) (flow-user-code flow))))

(defun logout ()
  "Forget the stored sign-in and stop one in progress."
  (cancel-flow)
  (save-entry nil (merge-pathnames nle::*auth-file-path*))
  (nle:notice nil :key +key+)
  "Factory Droid: signed out")

(defun status ()
  "Where the sign-in stands."
  (let ((flow *flow*)
        (entry (stored-entry (merge-pathnames nle::*auth-file-path*))))
    (cond (flow (format nil "Factory Droid: waiting for the code ~a at ~a"
                        (flow-user-code flow) (flow-url flow)))
          ((nlk:json-value entry :text "access_token")
           (let ((left (- (or (nlk:json-value entry :number "expires_at") 0) (now))))
             (format nil "Factory Droid: signed in~@[ as ~a~]~@[ (org ~a)~]; the token ~:[expired, and is refreshed at the next round~;expires in ~:*~d min~]"
                     (or (nlk:json-value entry :string "email")
                         (nlk:json-value entry :string "account_id"))
                     (nlk:json-value entry :string "org_id")
                     (and (plusp left) (ceiling left 60)))))
          (t "Factory Droid: not signed in; /factory-droid login starts the sign-in"))))

(defun run-command (args session-id)
  "/factory-droid ARGS: login, logout or status."
  (declare (ignore session-id))
  (let ((verb (string-downcase (or (first (uiop:split-string (nlk:trimmed (or args ""))
                                                             :separator '(#\Space #\Tab)))
                                   ""))))
    (cond ((equal verb "login") (login))
          ((equal verb "logout") (logout))
          ((member verb '("" "status") :test #'equal) (status))
          (t "usage: /factory-droid login | logout | status"))))

(defun complete-command (text session-id)
  "What /factory-droid's argument completes to while TEXT is typed."
  (declare (ignore session-id))
  (loop for verb in '("login" "logout" "status")
        when (uiop:string-prefix-p (string-downcase (nlk:trimmed (or text ""))) verb)
          collect (list :name verb :value verb)))
