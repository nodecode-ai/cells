;;;; signin.lisp --- the Google sign-in, the Cloud Code Assist project, and the kept token.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): the `oauth-code' login of compat/rules/
;;;; auth/google-gemini-cli.kdl as ai/src/registry/engine/oauth-code.ts and
;;;; common.ts run it, the loopback callback of registry/oauth/callback-
;;;; server.ts, the project discovery of registry/oauth/google-gemini-cli.ts,
;;;; and the validation message of ai/src/utils/google-validation.ts.
;;;;
;;;; The sign-in is Google's authorization-code grant with the Gemini CLI's
;;;; OAuth client (no PKCE, a hex state): the operator opens Google's consent
;;;; page, Google redirects the browser to http://127.0.0.1:8085/oauth2callback
;;;; (another port when that one is taken), and the code it carries is
;;;; exchanged for an access token, a refresh token and an expiry. When the
;;;; browser runs on another machine, the address it lands on is pasted back
;;;; with /google-gemini-cli code. The account's email comes from Google's
;;;; userinfo, and its Cloud Code Assist project from loadCodeAssist: an
;;;; existing one, else the free tier onboarded (onboardUser, polled), or the
;;;; project GOOGLE_CLOUD_PROJECT names for a paid tier.
;;;;
;;;; auth.json keeps the sign-in under oauth_tokens.google-gemini-cli:
;;;;   {"access_token": T, "refresh_token": R, "expires_at": epoch seconds,
;;;;    "project_id": P, "email": E?}
;;;; expires_at is Google's expiry less omp's five-minute skew, and a token
;;;; within a minute of it is refreshed before a round sends it.

(in-package #:nodecode-google-gemini-cli)

(defparameter +client-id+
  "NjgxMjU1ODA5Mzk1LW9vOGZ0Mm9wcmRybnA5ZTNhcWY2YXYzaG1kaWIxMzVqLmFwcHMuZ29vZ2xldXNlcmNvbnRlbnQuY29t"
  "The Gemini CLI's OAuth client id, base64 as omp keeps it.")

(defparameter +client-secret+ "R09DU1BYLTR1SGdNUG0tMW83U2stZ2VWNkN1NWNsWEZzeGw="
  "The Gemini CLI's OAuth client secret (an installed app's, not a secret), base64.")

(defparameter +authorize-url+ "https://accounts.google.com/o/oauth2/v2/auth")

(defparameter +token-url+ "https://oauth2.googleapis.com/token")

(defparameter +userinfo-url+ "https://www.googleapis.com/oauth2/v1/userinfo?alt=json")

(defparameter +scopes+
  '("https://www.googleapis.com/auth/cloud-platform"
    "https://www.googleapis.com/auth/userinfo.email"
    "https://www.googleapis.com/auth/userinfo.profile")
  "What the sign-in asks Google for.")

(defparameter +callback-port+ 8085
  "The loopback port Google is asked to redirect to, the Gemini CLI's.")

(defparameter +callback-path+ "/oauth2callback")

(defparameter +callback-seconds+ 300
  "How long a sign-in waits for the browser's answer.")

(defparameter +expiry-skew-seconds+ 300
  "Taken off Google's expiry when it is kept: omp's skew.")

(defparameter +refresh-skew-seconds+ 60
  "A kept token this close to its expiry is refreshed before it is sent.")

(defvar *operation-poll-seconds* 5
  "The wait between two polls of a project being provisioned.")

(defparameter +operation-polls+ 24
  "How many polls a project's provisioning gets before the sign-in gives up.")

(defparameter +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))

(defun unix-now ()
  "Seconds since the Unix epoch."
  (- (get-universal-time) +unix-epoch+))

(defun decoded (base64)
  "The text BASE64 encodes."
  (cl-base64:base64-string-to-string base64))

;;; --- one HTTP exchange -----------------------------------------------------------

(define-condition signin-failed (error)
  ((text :initarg :text :reader signin-failed-text))
  (:report (lambda (condition stream) (write-string (signin-failed-text condition) stream)))
  (:documentation "The sign-in cannot go on, in words the operator reads."))

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

(defun form (&rest pairs)
  "PAIRS, alternating names and values, as an urlencoded form; a NIL value is left out."
  (quri:url-encode-params (loop for (name value) on pairs by #'cddr
                                when value collect (cons name value))))

(defun hex (count)
  "COUNT random bytes as lowercase hex."
  (format nil "~(~{~2,'0x~}~)" (coerce (nlk:random-bytes count) 'list)))

;;; --- Google's account checks ------------------------------------------------------

(defun validation-url (text)
  "The verification page a VALIDATION_REQUIRED refusal TEXT names, or NIL."
  (when (and (stringp text) (search "VALIDATION_REQUIRED" text))
    (let* ((start (position #\{ text))
           (parsed (and start (json-of (subseq text start)))))
      (loop for detail across (nlk:json-array parsed "error" "details")
            when (equal "VALIDATION_REQUIRED" (nlk:json-value detail :string "reason"))
              do (alexandria:when-let (url (nlk:json-value detail :string "metadata" "validation_url"))
                   (return url))))))

(defun validation-message (url next-action &optional email)
  "What the operator does about an account Google wants verified."
  (format nil "Account verification required~@[ for ~a~]. Visit ~a to continue, then ~a."
          email url next-action))

;;; --- the token endpoint -----------------------------------------------------------

(defun token-request (grant &rest pairs)
  "POST the GRANT (authorization_code or refresh_token) with the form PAIRS to
Google's token endpoint, with the client's id and secret: the decoded answer,
or SIGNIN-FAILED with Google's words."
  (multiple-value-bind (text status)
      (exchange :post +token-url+
                :headers '(("Content-Type" . "application/x-www-form-urlencoded"))
                :content (apply #'form "grant_type" grant
                                (append pairs (list "client_id" (decoded +client-id+)
                                                    "client_secret" (decoded +client-secret+)))))
    (unless (ok-p status)
      (refuse "~a ~a failed: ~a ~a" +provider+
              (if (equal grant "refresh_token") "token refresh" "token exchange")
              status (subseq text 0 (min 500 (length text)))))
    (let ((body (json-of text)))
      (unless (nlk:json-value body :text "access_token")
        (refuse "~a token response missing access token: ~a" +provider+ (subseq text 0 (min 500 (length text)))))
      body)))

(defun expiry (body)
  "When the token in BODY expires, kept the way omp keeps it: its expires_in
from now, less the skew."
  (let ((seconds (nlk:json-value body :number "expires_in")))
    (unless seconds
      (refuse "~a token response missing expires_in" +provider+))
    (- (+ (unix-now) (round seconds)) +expiry-skew-seconds+)))

(defun user-email (access)
  "The email of the account ACCESS signs in, or NIL: a failure leaves it unknown."
  (ignore-errors
   (multiple-value-bind (text status)
       (exchange :get +userinfo-url+ :headers `(("Authorization" . ,(format nil "Bearer ~a" access))))
     (and (ok-p status) (nlk:json-value (json-of text) :text "email")))))

;;; --- the Cloud Code Assist project ----------------------------------------------------

(defun project-headers (access)
  "The headers of a Cloud Code Assist control request."
  `(("Authorization" . ,(format nil "Bearer ~a" access))
    ("Content-Type" . "application/json")
    ,@(cli-headers)))

(defun env-project ()
  "The Google Cloud project GOOGLE_CLOUD_PROJECT (or _ID) names, or NIL."
  (or (nle::credential-env "GOOGLE_CLOUD_PROJECT") (nle::credential-env "GOOGLE_CLOUD_PROJECT_ID")))

(defun security-policy-refusal-p (text)
  "Whether a loadCodeAssist refusal TEXT is a VPC Service Controls user's:
such an account is on the standard tier."
  (some (lambda (detail) (equal "SECURITY_POLICY_VIOLATED" (nlk:json-value detail :string "reason")))
        (coerce (nlk:json-array (json-of text) "error" "details") 'list)))

(defun project-required ()
  "Refuse: this account names its project through the environment."
  (refuse "This account requires setting the GOOGLE_CLOUD_PROJECT or GOOGLE_CLOUD_PROJECT_ID environment variable. See https://goo.gle/gemini-cli-auth-docs#workspace-gca"))

(defun poll-operation (name headers cancelled)
  "Poll the long-running operation NAME until it is done: its answer."
  (dotimes (attempt +operation-polls+)
    (when (plusp attempt)
      (wait-seconds *operation-poll-seconds* cancelled))
    (multiple-value-bind (text status)
        (exchange :get (format nil "~a/v1internal/~a" +base+ name) :headers headers)
      (unless (ok-p status)
        (refuse "Failed to poll operation: ~a" status))
      (let ((data (json-of text)))
        (when (nlk:json-value data :boolean "done")
          (return-from poll-operation data)))))
  (refuse "Project provisioning did not complete after ~d attempts" +operation-polls+))

(defun discover-project (access cancelled)
  "The Cloud Code Assist project of the account ACCESS signs in: the one it
has, else the free tier provisioned, else the project the environment names."
  (let* ((env (env-project))
         (headers (project-headers access))
         (metadata (nlk:json-object "ideType" "IDE_UNSPECIFIED"
                                    "platform" "PLATFORM_UNSPECIFIED"
                                    "pluginType" "GEMINI"
                                    :opt "duetProject" env))
         (data (multiple-value-bind (text status)
                   (exchange :post (format nil "~a/v1internal:loadCodeAssist" +base+)
                             :headers headers
                             :content (shasht:write-json
                                       (nlk:json-object :opt "cloudaicompanionProject" env
                                                        "metadata" metadata)
                                       nil))
                 (cond ((ok-p status) (json-of text))
                       ((security-policy-refusal-p text)
                        (nlk:json-object "currentTier" (nlk:json-object "id" "standard-tier")))
                       (t (refuse "loadCodeAssist failed: ~a ~a" status text))))))
    (when (nlk:json-value data :object "currentTier")
      (return-from discover-project
        (or (nlk:json-value data :text "cloudaicompanionProject") env (project-required))))
    (let* ((default (find-if (lambda (tier) (nlk:json-value tier :boolean "isDefault"))
                             (nlk:json-array data "allowedTiers")))
           (tier (cond (default (or (nlk:json-value default :text "id") "free-tier"))
                       (t "legacy-tier")))
           (free (equal tier "free-tier")))
      (when (and (not free) (null env))
        (project-required))
      (when (funcall cancelled) (error 'signin-cancelled))
      (let ((operation
              (multiple-value-bind (text status)
                  (exchange :post (format nil "~a/v1internal:onboardUser" +base+)
                            :headers headers
                            :content (shasht:write-json
                                      (nlk:json-object
                                       "tierId" tier
                                       "metadata" (nlk:json-object "ideType" "IDE_UNSPECIFIED"
                                                                   "platform" "PLATFORM_UNSPECIFIED"
                                                                   "pluginType" "GEMINI"
                                                                   :when (and (not free) env) "duetProject" env)
                                       :when (and (not free) env) "cloudaicompanionProject" env)
                                      nil))
                (unless (ok-p status)
                  (refuse "onboardUser failed: ~a ~a" status text))
                (json-of text))))
        (when (and (not (nlk:json-value operation :boolean "done"))
                   (nlk:json-value operation :text "name"))
          (setf operation (poll-operation (nlk:json-value operation :text "name") headers cancelled)))
        (or (nlk:json-value operation :text "response" "cloudaicompanionProject" "id")
            env
            (refuse "Could not discover or provision a Google Cloud project. Try setting the GOOGLE_CLOUD_PROJECT or GOOGLE_CLOUD_PROJECT_ID environment variable. See https://goo.gle/gemini-cli-auth-docs#workspace-gca"))))))

;;; --- auth.json ------------------------------------------------------------------

(defvar *store-lock* (bt2:make-recursive-lock :name "google-gemini-cli auth.json")
  "Held across one read-modify-write of auth.json, and across a refresh.")

(defun save-entry (path entry)
  "Set oauth_tokens.google-gemini-cli to ENTRY in the auth.json at PATH (NIL
takes it out), every other field kept; written atomically, mode 0600."
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

(defun refreshed-entry (entry)
  "ENTRY with a fresh access token from Google; the project and the email
are kept (omp's after-refresh hook), and so is the refresh token when Google
sends no new one."
  (let ((refresh (nlk:json-value entry :text "refresh_token")))
    (unless refresh
      (refuse "~a: the sign-in keeps no refresh token; run /~a login" +provider+ +provider+))
    (let ((body (token-request "refresh_token" "refresh_token" refresh)))
      (nlk:json-object "access_token" (nlk:json-value body :text "access_token")
                       "refresh_token" (or (nlk:json-value body :text "refresh_token") refresh)
                       "expires_at" (expiry body)
                       :opt "project_id" (nlk:json-value entry :text "project_id")
                       :opt "email" (nlk:json-value entry :text "email")))))

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

;;; --- the loopback callback --------------------------------------------------------------

(defun open-listener ()
  "A listener on 127.0.0.1 at +CALLBACK-PORT+, or on a free port when that
one is taken: (values LISTENER PORT)."
  (let ((listener (handler-case (usocket:socket-listen "127.0.0.1" +callback-port+
                                                       :reuse-address t :backlog 8
                                                       :element-type '(unsigned-byte 8))
                    (usocket:address-in-use-error ()
                      (usocket:socket-listen "127.0.0.1" 0 :reuse-address t :backlog 8
                                                           :element-type '(unsigned-byte 8))))))
    (values listener (usocket:get-local-port listener))))

(defun read-request-line (stream)
  "The request line of the HTTP request on STREAM, its head read through."
  (let ((buffer (make-array 256 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for byte = (read-byte stream nil nil)
          while byte
          do (vector-push-extend byte buffer)
             (when (> (fill-pointer buffer) 65536) (return))
          until (let ((end (fill-pointer buffer)))
                  (and (>= end 4)
                       (= 13 (aref buffer (- end 4))) (= 10 (aref buffer (- end 3)))
                       (= 13 (aref buffer (- end 2))) (= 10 (aref buffer (- end 1))))))
    (let ((text (sb-ext:octets-to-string buffer :external-format :latin-1)))
      (subseq text 0 (or (search (format nil "~c~c" #\Return #\Linefeed) text) (length text))))))

(defun respond (stream status text)
  "Answer STATUS with the page TEXT and close the exchange."
  (let ((octets (sb-ext:string-to-octets
                 (format nil "<!doctype html><meta charset=utf-8><title>Nodecode</title><p>~a</p>" text)
                 :external-format :utf-8)))
    (write-sequence (sb-ext:string-to-octets
                     (format nil "HTTP/1.1 ~d ~a~c~cContent-Type: text/html; charset=utf-8~c~cContent-Length: ~d~c~cConnection: close~c~c~c~c"
                             status (if (= status 200) "OK" "Not Found")
                             #\Return #\Linefeed #\Return #\Linefeed (length octets)
                             #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed)
                     :external-format :latin-1)
                    stream)
    (write-sequence octets stream)
    (finish-output stream)))

(defun callback-answer (target)
  "(values CODE STATE ERROR) a redirect TARGET (a URL or its path) carries."
  (let* ((query (let ((mark (position #\? target)))
                  (if mark (subseq target (1+ mark)) "")))
         (params (ignore-errors (quri:url-decode-params (subseq query 0 (or (position #\# query) (length query)))))))
    (flet ((param (name) (let ((value (cdr (assoc name params :test #'equal))))
                           (and (stringp value) (plusp (length value)) value))))
      (values (param "code") (param "state")
              (let ((error (param "error")))
                (and error (or (param "error_description") error)))))))

;;; --- the sign-in ---------------------------------------------------------------------

(defstruct (flow (:copier nil))
  (listener nil)
  (port nil)
  (state "")
  (redirect-uri "")
  ;; (CODE . STATE) once the answer arrived, from the browser or pasted
  (answer nil)
  ;; why the answer can never come, once it cannot
  (failure nil)
  (cancelled nil)
  (thread nil))

(defvar *flow* nil
  "The sign-in in progress, or NIL.")

(defun wait-seconds (seconds cancelled)
  "Sleep SECONDS, looking at the thunk CANCELLED every twentieth of a second."
  (let ((until (+ (get-internal-real-time) (* seconds internal-time-units-per-second))))
    (loop while (< (get-internal-real-time) until)
          do (when (funcall cancelled) (error 'signin-cancelled))
             (sleep (min 0.05 (max 0 (/ (- until (get-internal-real-time)) internal-time-units-per-second)))))
    (when (funcall cancelled) (error 'signin-cancelled))))

(defun authorize-url (state redirect-uri)
  "Google's consent page for this sign-in."
  (format nil "~a?~a" +authorize-url+
          (form "client_id" (decoded +client-id+)
                "response_type" "code"
                "redirect_uri" redirect-uri
                "scope" (format nil "~{~a~^ ~}" +scopes+)
                "state" state
                "access_type" "offline"
                "prompt" "consent")))

(defun take-callback (flow)
  "Serve one connection waiting on FLOW's listener: the browser's redirect
answers the flow, anything else a 404. True when one was served."
  (let ((listener (flow-listener flow)))
    (when (usocket:wait-for-input listener :timeout 0.05 :ready-only t)
      (let ((connection (usocket:socket-accept listener :element-type '(unsigned-byte 8))))
        (unwind-protect
             (sb-sys:with-deadline (:seconds 10)
               (let* ((stream (usocket:socket-stream connection))
                      (line (read-request-line stream))
                      (target (second (uiop:split-string line :separator " ")))
                      (path (and target (subseq target 0 (or (position #\? target) (length target))))))
                 (if (equal path +callback-path+)
                     (multiple-value-bind (code state error) (callback-answer target)
                       (cond (error
                              (setf (flow-failure flow) (format nil "Google refused the sign-in: ~a" error))
                              (respond stream 200 "Sign-in refused. You can close this tab."))
                             ((not (equal state (flow-state flow)))
                              (respond stream 200 "This answer is not for the sign-in Nodecode started. You can close this tab."))
                             (code
                              (setf (flow-answer flow) (cons code state))
                              (respond stream 200 "Signed in. You can close this tab and return to Nodecode."))
                             (t (respond stream 200 "No authorization code arrived. You can close this tab."))))
                     (respond stream 404 "Not found."))))
          (ignore-errors (usocket:socket-close connection))))
      t)))

(defun close-listener (flow)
  "Stop listening for FLOW's callback."
  (alexandria:when-let (listener (flow-listener flow))
    (setf (flow-listener flow) nil)
    (ignore-errors (usocket:socket-close listener))))

(defun await-code (flow)
  "Serve FLOW's listener until its answer arrives: the authorization code."
  (let ((deadline (+ (get-internal-real-time) (* +callback-seconds+ internal-time-units-per-second))))
    (loop
      (when (flow-cancelled flow) (error 'signin-cancelled))
      (alexandria:when-let (failure (flow-failure flow)) (refuse "~a" failure))
      (alexandria:when-let (answer (flow-answer flow)) (return (car answer)))
      (when (> (get-internal-real-time) deadline)
        (refuse "no answer from the browser within ~d seconds" +callback-seconds+))
      (unless (and (flow-listener flow) (ignore-errors (take-callback flow)))
        (sleep 0.05)))))

(defun sign-in (flow auth-path)
  "The background half of a sign-in: wait for the code, exchange it, find the
project, keep the token, and say how it went."
  (let ((cancelled (lambda () (flow-cancelled flow))))
    (handler-case
        (let* ((code (prog1 (await-code flow) (close-listener flow)))
               (body (token-request "authorization_code" "code" code
                                    "redirect_uri" (flow-redirect-uri flow)))
               (access (nlk:json-value body :text "access_token"))
               (refresh (nlk:json-value body :text "refresh_token"))
               (expires (expiry body))
               (email (user-email access)))
          (unless refresh
            (refuse "No refresh token received. Please try again."))
          (when (funcall cancelled) (error 'signin-cancelled))
          (let ((project (handler-case (discover-project access cancelled)
                           (signin-failed (refusal)
                             (alexandria:if-let (url (validation-url (signin-failed-text refusal)))
                               (refuse "~a" (validation-message url "sign in again" email))
                               (error refusal))))))
            (when (funcall cancelled) (error 'signin-cancelled))
            (save-entry auth-path (nlk:json-object "access_token" access
                                                   "refresh_token" refresh
                                                   "expires_at" expires
                                                   "project_id" project
                                                   :opt "email" email))
            ;; a failure said before stands no longer; the success is said once
            (nle:notice nil :key +key+)
            (nle:notice (format nil "~a: signed in~@[ as ~a~], project ~a" +provider+ email project))))
      (signin-cancelled () nil)
      (error (e)
        (unless (funcall cancelled)
          (nle:notice (format nil "~a: sign-in failed: ~a" +provider+ e) :level :warning :key +key+))))
    (close-listener flow)
    (when (eq *flow* flow) (setf *flow* nil))))

(defun cancel-flow ()
  "End the sign-in in progress, if any, and wait for its thread a moment."
  (let ((flow *flow*))
    (setf *flow* nil)
    (when flow
      (setf (flow-cancelled flow) t)
      (let ((thread (flow-thread flow)))
        (when (and thread (not (eq thread (bt2:current-thread))))
          (loop repeat 40 while (bt2:thread-alive-p thread) do (sleep 0.05))))
      (close-listener flow))))

(defun login (auth-path)
  "Start a sign-in: answer what the operator must do, finish in the background."
  (cancel-flow)
  (multiple-value-bind (listener port) (open-listener)
    (let* ((state (hex 16))
           (redirect-uri (format nil "http://127.0.0.1:~d~a" port +callback-path+))
           (flow (make-flow :listener listener :port port :state state :redirect-uri redirect-uri)))
      (setf *flow* flow
            (flow-thread flow) (bt2:make-thread (lambda () (sign-in flow auth-path))
                                                :name (format nil "~a sign-in" +provider+)))
      (format nil "Open this address in a browser and sign in with Google: ~a~%~
Nodecode listens at ~a for Google's answer, finishes the sign-in and says so in a notice. ~
If the browser runs on another machine, paste the address it lands on with /~a code ADDRESS."
              (authorize-url state redirect-uri) redirect-uri +provider+))))

(defun paste (text)
  "Hand the pending sign-in the code TEXT carries: the address the browser
landed on, or the code alone."
  (let ((flow *flow*)
        (trimmed (nlk:trimmed (or text ""))))
    (cond ((null flow) (format nil "~a: no sign-in is waiting; /~a login starts one" +provider+ +provider+))
          ((zerop (length trimmed)) (format nil "usage: /~a code ADDRESS-OR-CODE" +provider+))
          (t (multiple-value-bind (code state error)
                 (if (find #\? trimmed)
                     (callback-answer trimmed)
                     ;; a bare code, or omp's code#state
                     (let ((mark (position #\# trimmed)))
                       (values (subseq trimmed 0 mark) (and mark (subseq trimmed (1+ mark))) nil)))
               (cond (error (setf (flow-failure flow) (format nil "Google refused the sign-in: ~a" error))
                            (format nil "~a: Google refused the sign-in: ~a" +provider+ error))
                     ((and state (not (equal state (flow-state flow))))
                      (format nil "~a: that address is not for the sign-in in progress" +provider+))
                     ((null code) (format nil "~a: that address carries no authorization code" +provider+))
                     (t (setf (flow-answer flow) (cons code (flow-state flow)))
                        (format nil "~a: code received; finishing the sign-in" +provider+))))))))
