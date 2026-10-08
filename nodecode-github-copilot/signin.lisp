;;;; signin.lisp --- the GitHub device sign-in, and where its token is kept.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): ai/src/registry/oauth/github-copilot.ts
;;;; and the endpoint probe of catalog/src/wire/github-copilot.ts.
;;;;
;;;; The sign-in is GitHub's device flow with the minimal read:user grant: ask
;;;; GitHub for a device code, show the operator the code and the page to type
;;;; it into, and poll until GitHub hands back a token. Public github.com uses
;;;; the OpenCode OAuth app (narrow consent, accepted by organizations that
;;;; restrict OAuth apps); an Enterprise instance runs its own OAuth registry,
;;;; so it keeps the Copilot CLI's client. The GitHub token is what Copilot
;;;; takes as its bearer: omp at the pinned commit makes no Copilot token
;;;; exchange. The GitHub token does not expire on a clock, so it is kept
;;;; with no expiry; an entry that carries one (omp stamps ten years) is
;;;; refreshed the way omp's hook refreshes it, the token kept and the
;;;; expiry dropped.
;;;;
;;;; After the token, the plan's own API host is asked of GitHub
;;;; (copilot_internal/user), and every bundled model's policy is set to
;;;; enabled, since Copilot serves some models (Claude, Grok) only once the
;;;; account has accepted their policy.
;;;;
;;;; auth.json keeps the sign-in under oauth_tokens.github-copilot:
;;;;   {"access_token": T, "refresh_token": T,
;;;;    "enterprise_url": domain?, "api_endpoint": plan host?}

(in-package #:nodecode-github-copilot)

(defparameter +opencode-client-id+ "Ov23li8tweQw6odWQebz"
  "The OAuth app public github.com signs in through.")

(defparameter +copilot-cli-client-id+ "Ov23ctDVkRmgkPke0Mmm"
  "The OAuth app an Enterprise instance signs in through: the Copilot CLI's.")

(defparameter +scope+ "read:user"
  "The grant the sign-in asks for.")

(defparameter +refresh-skew-seconds+ 60
  "A stored token this close to its expiry is refreshed before it is sent.")

(defvar *poll-floor* 1
  "The shortest wait between two polls of the device flow, in seconds.")

(defvar *poll-scale* 1
  "Seconds per unit of the interval GitHub names: 1, as GitHub means it.")

(defparameter +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0)
  "The universal time of the Unix epoch.")

(defun unix-now ()
  "Seconds since the Unix epoch."
  (- (get-universal-time) +unix-epoch+))

(defun client-id (domain)
  "The OAuth app the sign-in at DOMAIN goes through."
  (if (public-host-p domain) +opencode-client-id+ +copilot-cli-client-id+))

(defun oauth-headers ()
  "The headers of every exchange with GitHub's OAuth endpoints."
  '(("Accept" . "application/json")
    ("Content-Type" . "application/x-www-form-urlencoded")
    ("User-Agent" . "copilot-developer-action/0.0.1")))

;;; --- one HTTP exchange -----------------------------------------------------------

(define-condition signin-failed (error)
  ((text :initarg :text :reader signin-failed-text))
  (:report (lambda (condition stream) (write-string (signin-failed-text condition) stream)))
  (:documentation "The sign-in cannot go on, in words the operator reads."))

(define-condition signin-cancelled (error) ()
  (:documentation "A newer sign-in, or the cell stopping, ended this one."))

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

(defun form (&rest pairs)
  "PAIRS, alternating names and values, as an urlencoded form."
  (quri:url-encode-params (loop for (name value) on pairs by #'cddr collect (cons name value))))

;;; --- the device flow ---------------------------------------------------------------

(defun start-device-flow (domain)
  "Ask DOMAIN for a device code: a plist (:device-code :user-code
:verification-uri :interval :expires-in)."
  (multiple-value-bind (text status)
      (exchange :post (format nil "https://~a/login/device/code" domain)
                :headers (oauth-headers)
                :content (form "client_id" (client-id domain) "scope" +scope+))
    (unless (ok-p status)
      (refuse "~a ~a" status text))
    (let ((data (json-of text)))
      (unless (hash-table-p data)
        (refuse "Invalid device code response"))
      (nlk:with-json ((device-code :string "device_code")
                      (user-code :string "user_code")
                      (uri :string "verification_uri")
                      (interval :number "interval")
                      (expires-in :number "expires_in"))
          data
        (unless (and device-code user-code uri interval expires-in)
          (refuse "Invalid device code response fields"))
        (list :device-code device-code :user-code user-code :verification-uri uri
              :interval interval :expires-in expires-in)))))

(defun wait-seconds (seconds cancelled)
  "Sleep SECONDS, looking at the thunk CANCELLED every twentieth of a second."
  (let ((until (+ (get-internal-real-time) (* seconds internal-time-units-per-second))))
    (loop while (< (get-internal-real-time) until)
          do (when (funcall cancelled) (error 'signin-cancelled))
             (sleep (min 0.05 (max 0 (/ (- until (get-internal-real-time)) internal-time-units-per-second)))))
    (when (funcall cancelled) (error 'signin-cancelled))))

(defun poll-for-token (domain device-code interval expires-in cancelled)
  "Poll DOMAIN until the operator confirms DEVICE-CODE: the GitHub token."
  ;; omp's cadence: the interval GitHub named, over a floor, times 1.2; a
  ;; slow_down takes the interval it names (else five seconds more) and 1.4.
  (let* ((deadline (+ (get-internal-real-time) (* expires-in internal-time-units-per-second)))
         (step (max *poll-floor* (* interval *poll-scale*)))
         (multiplier 1.2)
         (slow-downs 0))
    (flet ((remaining () (/ (- deadline (get-internal-real-time)) internal-time-units-per-second)))
      (loop while (plusp (remaining))
            do (wait-seconds (min (* step multiplier) (remaining)) cancelled)
               (multiple-value-bind (text status)
                   (exchange :post (format nil "https://~a/login/oauth/access_token" domain)
                             :headers (oauth-headers)
                             :content (form "client_id" (client-id domain)
                                            "device_code" device-code
                                            "grant_type" "urn:ietf:params:oauth:grant-type:device_code"))
                 (unless (ok-p status)
                   (refuse "~a ~a" status text))
                 (let ((data (json-of text)))
                   (alexandria:when-let (token (nlk:json-value data :string "access_token"))
                     (return-from poll-for-token token))
                   (alexandria:when-let (error (nlk:json-value data :string "error"))
                     (cond ((equal error "authorization_pending"))
                           ((equal error "slow_down")
                            (incf slow-downs)
                            (let ((named (nlk:json-value data :number "interval")))
                              (setf step (if (and named (plusp named))
                                             (max *poll-floor* (* named *poll-scale*))
                                             (max *poll-floor* (+ step (* 5 *poll-scale*))))
                                    multiplier 1.4)))
                           (t (refuse "Device flow failed: ~a~@[: ~a~]"
                                    error (nlk:json-value data :text "error_description")))))))))
    (if (plusp slow-downs)
        (refuse "Device flow timed out after one or more slow_down responses. This is often caused by clock drift in WSL or VM environments. Please sync or restart the VM clock and try again.")
        (refuse "Device flow timed out"))))

(defun discover-api-endpoint (token)
  "The plan's own API host GitHub names for TOKEN, or NIL: a best-effort probe."
  (ignore-errors
   (multiple-value-bind (text status)
       (exchange :get "https://api.github.com/copilot_internal/user"
                 :headers `(("Accept" . "application/json")
                            ("Authorization" . ,(format nil "token ~a" token))
                            ("User-Agent" . ,(cli-user-agent)))
                 :timeout 10)
     (and (ok-p status)
          (normalize-api-endpoint (nlk:json-value (json-of text) :string "endpoints" "api"))))))

(defun identity-denied-p (status text)
  "Whether a refusal is Copilot denying the client identity: a 403, or a 400
whose error code is model_not_supported."
  (or (eql status 403)
      (and (eql status 400)
           (equal "model_not_supported" (nlk:json-value (json-of text) :string "error" "code")))))

(defun enable-model (token model-id base pinned)
  "Set MODEL-ID's policy to enabled for TOKEN's account at BASE; true when
Copilot took it. PINNED is the operator's integration id, or NIL: a chat
identity Copilot refuses is then tried once more as the CLI."
  (flet ((post (integration-id)
           (exchange :post (format nil "~a/models/~a/policy" base model-id)
                     :headers `(("Content-Type" . "application/json")
                                ("Authorization" . ,(format nil "Bearer ~a" token))
                                ,@(remove-if (lambda (pair)
                                               (member (car pair) '("Copilot-Integration-Id" "Openai-Intent")
                                                       :test #'string-equal))
                                             (api-headers))
                                ("Copilot-Integration-Id" . ,integration-id)
                                ("Openai-Intent" . "chat-policy")
                                ("X-Initiator" . "user")
                                ("X-Interaction-Type" . "chat-policy"))
                     :content "{\"state\":\"enabled\"}"
                     :timeout 15)))
    (handler-case
        (multiple-value-bind (text status) (post (or pinned +chat-integration-id+))
          (if (and (not pinned) (identity-denied-p status text))
              (ok-p (nth-value 1 (post +cli-integration-id+)))
              (ok-p status)))
      (error () nil))))

(defun enable-all-models (token enterprise endpoint cancelled)
  "Enable every bundled model's policy for TOKEN's account: (values ENABLED TOTAL)."
  (let ((base (or endpoint (if enterprise (enterprise-base enterprise) +base+)))
        (pinned (pinned-integration-id))
        (ids (remove-duplicates (map 'list (lambda (row) (nlk:json-value row :string "id")) +models+)
                                :test #'equal :from-end t)))
    (values (loop for id in ids
                  do (when (funcall cancelled) (error 'signin-cancelled))
                  count (enable-model token id base pinned))
            (length ids))))

;;; --- auth.json ------------------------------------------------------------------

(defvar *store-lock* (bt2:make-lock :name "github-copilot auth.json")
  "Held across one read-modify-write of auth.json.")

(defun save-entry (path entry)
  "Set oauth_tokens.github-copilot to ENTRY in the auth.json at PATH (NIL takes
it out), every other field kept; written atomically, mode 0600."
  (bt2:with-lock-held (*store-lock*)
    (let* ((auth (or (nle::read-auth-file path) (make-hash-table :test #'equal)))
           (tokens (or (nlk:json-value auth :object "oauth_tokens")
                       (setf (gethash "oauth_tokens" auth) (make-hash-table :test #'equal)))))
      (if entry
          (setf (gethash +provider+ tokens) entry)
          (remhash +provider+ tokens))
      (nlk:write-file-atomically path (shasht:write-json auth nil) :mode #o600 :directory-mode #o700)
      entry)))

(defun token-entry (token &key enterprise endpoint)
  "The auth.json entry of a GitHub TOKEN: no expiry, since it has none."
  (nlk:json-object "access_token" token
                   "refresh_token" token
                   :opt "enterprise_url" enterprise
                   :opt "api_endpoint" endpoint))

(defun stored-entry (auth)
  "The sign-in the parsed auth.json AUTH keeps, or NIL."
  (let ((entry (nlk:json-value auth :object "oauth_tokens" +provider+)))
    (and (nlk:json-value entry :text "access_token") entry)))

(defun refreshed-entry (entry)
  "ENTRY refreshed the way omp refreshes a Copilot sign-in: the GitHub token
stays directly usable, so the refresh token becomes the access token again,
and the expiry that made it due goes (omp's is ten years off)."
  (let ((token (or (nlk:json-value entry :text "refresh_token")
                   (nlk:json-value entry :text "access_token"))))
    (token-entry token
                 :enterprise (nlk:json-value entry :text "enterprise_url")
                 :endpoint (nlk:json-value entry :text "api_endpoint"))))

(defun fresh-entry (entry path)
  "ENTRY, refreshed and written back to the auth.json at PATH first when it
carries an expiry within +REFRESH-SKEW-SECONDS+. No network: the refresh is
omp's, which only re-reads the entry."
  (let ((expires (nlk:json-value entry :number "expires_at")))
    (if (and expires path (< (- expires (unix-now)) +refresh-skew-seconds+))
        (save-entry path (refreshed-entry entry))
        entry)))

;;; --- the running sign-in -----------------------------------------------------------

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
          ;; it looks at the flag every twentieth of a second, or after its
          ;; exchange in flight returns
          (loop repeat 40 while (bt2:thread-alive-p thread) do (sleep 0.05)))))))

(defun finish-sign-in (flow domain enterprise device auth-path)
  "The background half of a sign-in: wait for the operator, keep the token,
enable the models, and say how it went."
  (let ((cancelled (lambda () (flow-cancelled flow))))
    (handler-case
        (let* ((token (poll-for-token domain (getf device :device-code) (getf device :interval)
                                      (getf device :expires-in) cancelled))
               (endpoint (discover-api-endpoint token)))
          (when (funcall cancelled) (error 'signin-cancelled))
          (save-entry auth-path (token-entry token :enterprise enterprise :endpoint endpoint))
          ;; a failure said before stands no longer; the success is said once
          (nle:notice nil :key +key+)
          (multiple-value-bind (enabled total) (enable-all-models token enterprise endpoint cancelled)
            (nle:notice (format nil "github-copilot: signed in~@[ to ~a~]~@[ (served at ~a)~]; ~d of ~d models enabled"
                                enterprise endpoint enabled total))))
      (signin-cancelled () nil)
      (error (e)
        (unless (funcall cancelled)
          (nle:notice (format nil "github-copilot: sign-in failed: ~a" e) :level :warning :key +key+))))
    (when (eq *flow* flow) (setf *flow* nil))))

(defun login (argument auth-path)
  "Start the device sign-in at public GitHub, or at the Enterprise instance
ARGUMENT names: answer what the operator must do, finish in the background."
  (let* ((trimmed (nlk:trimmed (or argument "")))
         (normalized (normalize-domain trimmed)))
    (when (and (plusp (length trimmed)) (null normalized))
      (return-from login (format nil "github-copilot: ~a is not a GitHub Enterprise URL or domain" trimmed)))
    (let* ((enterprise (enterprise-domain trimmed))
           (domain (or enterprise "github.com"))
           (device (handler-case (start-device-flow domain)
                     (error (e)
                       (return-from login
                         (format nil "github-copilot: could not start the sign-in at ~a: ~a" domain e)))))
           (flow (make-flow)))
      (cancel-flow)
      (setf *flow* flow
            (flow-thread flow) (bt2:make-thread (lambda () (finish-sign-in flow domain enterprise device auth-path))
                                                :name "github-copilot sign-in"))
      (format nil "Open ~a and enter the code ~a to sign in to GitHub Copilot~@[ on ~a~]. ~
Nodecode finishes the sign-in once GitHub confirms it and says so in a notice."
              (getf device :verification-uri) (getf device :user-code) enterprise))))
