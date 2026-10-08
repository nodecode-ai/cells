;;;; signin.lisp --- the Muse Code sign-in: a device code, an account token, a minted key.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; auth/muse-code.kdl, run by the declarative engine of packages/ai/src/
;;;; registry/engine/ (device-code.ts, common.ts), the poller of
;;;; registry/oauth/device-code.ts, and the key exchange of
;;;; registry/oauth/muse-code.ts (attachMuseCodeApiKey).
;;;;
;;;; RFC 8628 against https://auth.meta.com/oidc: POST the client id to the
;;;; device endpoint, show the operator the verification URL and the user
;;;; code, then poll the token endpoint until the grant completes; both
;;;; requests carry x-api-version 1.0.0. The access token that arrives is the
;;;; Meta account's, not a model credential: it is spent once, on
;;;; https://api.meta.ai/muse-code/key, which onboards the account and mints
;;;; the Model API key the subscription authorizes. That key is what model
;;;; requests carry. Meta's device answer names no expiry and its token
;;;; endpoint refuses the refresh grant, so the sign-in never expires and is
;;;; never refreshed: an account whose subscription lapses signs in again.
;;;;
;;;; The entry lives in the shared auth.json under oauth_tokens.muse-code
;;;; (access_token, the account token; api_key, the minted key; account_id;
;;;; email; no expires_at), written the way the core writes api_keys: read
;;;; the file, set the one entry, write it back atomically at 0600 with every
;;;; other field kept.

(in-package #:nodecode-muse-code)

(defparameter +client-id+ "1031625952748946"
  "Muse Code's public OAuth client.")

(defparameter +device-url+ "https://auth.meta.com/oidc/device/authorization/")
(defparameter +token-url+ "https://auth.meta.com/oidc/device/token/")
(defparameter +key-url+ "https://api.meta.ai/muse-code/key")

(defparameter +timeout+ 30
  "Seconds one sign-in exchange may take, the dial included.")

(defparameter +key-timeout+ 20
  "Seconds the key exchange may take.")

(defun meta-headers ()
  "The headers the device and token requests carry."
  `(("Accept" . "application/json") ("x-api-version" . ,+api-version+)))

(defun now ()
  "The time as epoch seconds."
  (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0)))

;;; --- one exchange ----------------------------------------------------------------

(defun post (url content content-type headers seconds)
  "POST CONTENT, of CONTENT-TYPE, to URL with HEADERS under a deadline of
SECONDS. => (values BODY STATUS): BODY the answer decoded as JSON, else its
text. A refusal is an answer like any other; only a transport failure or the
deadline signals."
  (multiple-value-bind (body status)
      (handler-case
          (sb-sys:with-deadline (:seconds seconds)
            (handler-case
                (dex:post url :headers (append headers `(("Content-Type" . ,content-type)))
                              :content content
                              :connect-timeout seconds :read-timeout seconds)
              (dex:http-request-failed (condition)
                (values (dex:response-body condition) (dex:response-status condition)))))
        (sb-sys:deadline-timeout ()
          (error "~a did not answer within ~d s" url seconds)))
    (let ((text (nlk:body-text body)))
      (values (or (ignore-errors (nlk:decode-json text)) text) status))))

(defun exchange (url params headers)
  "POST the form PARAMS (an alist) to URL with HEADERS, as POST answers."
  (post url (quri:url-encode-params params) "application/x-www-form-urlencoded" headers +timeout+))

(defun ok-p (status)
  "Whether STATUS is a success."
  (and (integerp status) (< status 400)))

(defun excerpt (body)
  "BODY as at most 500 characters of text, for a failure's words."
  (let ((text (if (stringp body) body (nlk:encode-json-object body))))
    (string-trim " " (subseq text 0 (min 500 (length text))))))

;;; --- the key -------------------------------------------------------------------------

(defun mint-key (access)
  "Exchange the account token ACCESS for the Model API key its subscription
authorizes, onboarding the account. => the oauth_tokens entry."
  (multiple-value-bind (body status)
      (post +key-url+ (nlk:encode-json-object (nlk:json-object "onboard" t)) "application/json"
            `(("Accept" . "application/json")
              ("Authorization" . ,(format nil "Bearer ~a" access))
              ("x-api-version" . ,+api-version+))
            +key-timeout+)
    (unless (ok-p status)
      (error "Muse Code key exchange failed: ~a~@[ ~a~]" status
             (let ((text (excerpt body))) (and (plusp (length text)) text))))
    (unless (hash-table-p body)
      (error "Muse Code key exchange returned invalid JSON"))
    ;; false, not absent: decoded JSON false is NIL beside a present key
    (multiple-value-bind (active present) (gethash "is_subs_active" body)
      (when (and present (null active))
        (error "invalid_grant: Muse Code subscription is inactive")))
    (let ((key (nlk:trimmed (or (nlk:json-value body :string "api_key") "")))
          (action (or (nlk:json-value body :text "action_url")
                      (nlk:json-value body :text "require_payment_action_url"))))
      (when (zerop (length key))
        (if (or (eq t (gethash "require_payment" body)) action)
            (error "Muse Code subscription is required~@[: ~a~]" action)
            (error "Muse Code key response is missing api_key")))
      (let* ((email (alexandria:when-let (email (nlk:json-value body :text "user_email"))
                      (string-downcase (nlk:trimmed email))))
             (account (or (nlk:json-value body :text "user_id") email)))
        (unless account
          (error "Muse Code key response is missing a stable account identity"))
        (nlk:json-object "access_token" access
                         "api_key" key
                         "account_id" account
                         :opt "email" email)))))

(defvar *store-lock* (bt2:make-lock :name "muse-code auth store")
  "Held while the cell rewrites auth.json.")

(defun stored-entry (path)
  "The oauth_tokens entry the auth.json at PATH holds for Muse Code, or NIL."
  (nlk:json-value (ignore-errors (nle::read-auth-file path)) :object "oauth_tokens" +provider+))

(defun save-entry (entry path)
  "Set oauth_tokens.muse-code in the auth.json at PATH to ENTRY, or take it
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

(defun token-credential (op)
  "The credential the stored sign-in answers for the :CREDENTIAL op OP: the
minted key, never the account token; NIL when there is none."
  (alexandria:when-let (key (nlk:json-value (getf op :auth) :text "oauth_tokens" +provider+ "api_key"))
    (nle:make-credential key :oauth)))

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
      (exchange +device-url+ `(("client_id" . ,+client-id+)) (meta-headers))
    (unless (ok-p status)
      (error "muse-code device authorization failed: ~a ~a" status (excerpt body)))
    (let ((user-code (nlk:json-value body :text "user_code"))
          (device-code (nlk:json-value body :text "device_code"))
          (uri (nlk:json-value body :text "verification_uri"))
          (complete (nlk:json-value body :text "verification_uri_complete"))
          (interval (nlk:json-value body :number "interval"))
          (expires (nlk:json-value body :number "expires_in")))
      (unless (and user-code device-code uri)
        (error "muse-code device authorization response missing required fields"))
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
                (meta-headers))
    (let ((error (and (hash-table-p body) (gethash "error" body))))
      (cond ((and (ok-p status) (null error)) (values :complete body))
            ((equal error "authorization_pending") :pending)
            ((equal error "slow_down") :slow-down)
            ((equal error "expired_token")
             (values :failed "muse-code device code expired; restart the login"))
            ((equal error "access_denied")
             (values :failed "muse-code device authorization was denied"))
            (t (values :failed
                       (format nil "muse-code device token request failed: ~a~@[ ~a~]"
                               status (or (nlk:json-value body :string "error_description")
                                          (and (stringp error) error)))))))))

(defun pause (seconds flow)
  "Sleep SECONDS, or less once FLOW is cancelled."
  (loop with until = (+ (get-internal-real-time) (* seconds internal-time-units-per-second))
        while (and (< (get-internal-real-time) until) (not (flow-cancelled flow)))
        do (sleep 0.2)))

(defun poll-flow (flow)
  "Poll until the grant completes. => the account token's answer, or NIL when
the sign-in was cancelled; a refusal, an expired code or a transport failure
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
  "Run FLOW to its end on this thread, mint the key, and say the outcome as a
notice."
  (handler-case
      (let ((body (poll-flow flow)))
        (when (and body (not (flow-cancelled flow)))
          (let ((access (nlk:json-value body :text "access_token")))
            (unless access
              (error "muse-code token response missing access token: ~a" (excerpt body)))
            (let ((entry (mint-key access)))
              (unless (flow-cancelled flow)
                (save-entry entry (flow-auth-path flow))
                (nle:notice (format nil "Muse Code: signed in as ~a"
                                    (or (nlk:json-value entry :string "email")
                                        (nlk:json-value entry :string "account_id")))
                            :key +key+))))))
    (serious-condition (condition)
      (unless (flow-cancelled flow)
        (nle:notice (format nil "Muse Code: sign-in failed: ~a" condition)
                    :level :warning :key +key+))))
  (when (eq *flow* flow) (setf *flow* nil)))

;;; --- the command -------------------------------------------------------------------

(defun login ()
  "Start the device flow and answer what the operator must do; the poll runs
on a thread of its own."
  (cancel-flow)
  (let ((flow (start-flow (merge-pathnames nle::*auth-file-path*))))
    (setf *flow* flow)
    (bt2:make-thread (lambda () (finish-flow flow)) :name "muse-code sign-in")
    (format nil "Muse Code sign-in: open ~a~%Enter code: ~a~%Waiting in the background; the outcome comes as a notice."
            (flow-url flow) (flow-user-code flow))))

(defun logout ()
  "Forget the stored sign-in and stop one in progress."
  (cancel-flow)
  (save-entry nil (merge-pathnames nle::*auth-file-path*))
  (nle:notice nil :key +key+)
  "Muse Code: signed out")

(defun status ()
  "Where the sign-in stands."
  (let ((flow *flow*)
        (entry (stored-entry (merge-pathnames nle::*auth-file-path*))))
    (cond (flow (format nil "Muse Code: waiting for the code ~a at ~a"
                        (flow-user-code flow) (flow-url flow)))
          ((nlk:json-value entry :text "api_key")
           (format nil "Muse Code: signed in as ~a; the key does not expire"
                   (or (nlk:json-value entry :string "email")
                       (nlk:json-value entry :string "account_id"))))
          (t "Muse Code: not signed in; /muse-code login starts the sign-in"))))

(defun run-command (args session-id)
  "/muse-code ARGS: login, logout or status."
  (declare (ignore session-id))
  (let ((verb (string-downcase (or (first (uiop:split-string (nlk:trimmed (or args ""))
                                                             :separator '(#\Space #\Tab)))
                                   ""))))
    (cond ((equal verb "login") (login))
          ((equal verb "logout") (logout))
          ((member verb '("" "status") :test #'equal) (status))
          (t "usage: /muse-code login | logout | status"))))

(defun complete-command (text session-id)
  "What /muse-code's argument completes to while TEXT is typed."
  (declare (ignore session-id))
  (loop for verb in '("login" "logout" "status")
        when (uiop:string-prefix-p (string-downcase (nlk:trimmed (or text ""))) verb)
          collect (list :name verb :value verb)))
