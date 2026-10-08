;;;; signin.lisp --- the Google sign-in, the Antigravity project, and the kept token.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): the `oauth-code' login of compat/rules/
;;;; auth/google-antigravity.kdl as ai/src/registry/engine/oauth-code.ts and
;;;; common.ts run it, the loopback callback of registry/oauth/callback-
;;;; server.ts, the project discovery of registry/oauth/google-antigravity.ts,
;;;; and the validation message of ai/src/utils/google-validation.ts.
;;;;
;;;; The sign-in is Google's authorization-code grant with Antigravity's own
;;;; OAuth client, which reaches its additional models (no PKCE, a hex state):
;;;; the operator opens Google's consent page, Google redirects the browser to
;;;; http://127.0.0.1:51121/oauth-callback (another port when that one is
;;;; taken), and the code it carries is exchanged for an access token, a
;;;; refresh token and an expiry. When the browser runs on another machine,
;;;; the address it lands on is pasted back with /google-antigravity code.
;;;; The account's email comes from Google's userinfo, and its project from
;;;; Antigravity's loadCodeAssist: the free tier onboarded first when the
;;;; account has none (onboardUser, polled for thirty seconds), refused in
;;;; Google's own words when the account is not eligible.
;;;;
;;;; auth.json keeps the sign-in under oauth_tokens.google-antigravity:
;;;;   {"access_token": T, "refresh_token": R, "expires_at": epoch seconds,
;;;;    "project_id": P, "email": E?}
;;;; expires_at is Google's expiry less omp's five-minute skew, and a token
;;;; within a minute of it is refreshed before a round sends it.

(in-package #:nodecode-google-antigravity)

(defparameter +client-id+
  "MTA3MTAwNjA2MDU5MS10bWhzc2luMmgyMWxjcmUyMzV2dG9sb2poNGc0MDNlcC5hcHBzLmdvb2dsZXVzZXJjb250ZW50LmNvbQ=="
  "Antigravity's OAuth client id, base64 as omp keeps it.")

(defparameter +client-secret+ "R09DU1BYLUs1OEZXUjQ4NkxkTEoxbUxCOHNYQzR6NnFEQWY="
  "Antigravity's OAuth client secret (an installed app's, not a secret), base64.")

(defparameter +authorize-url+ "https://accounts.google.com/o/oauth2/v2/auth")

(defparameter +token-url+ "https://oauth2.googleapis.com/token")

(defparameter +userinfo-url+ "https://www.googleapis.com/oauth2/v1/userinfo?alt=json")

(defparameter +scopes+
  '("https://www.googleapis.com/auth/cloud-platform"
    "https://www.googleapis.com/auth/userinfo.email"
    "https://www.googleapis.com/auth/userinfo.profile"
    "https://www.googleapis.com/auth/cclog"
    "https://www.googleapis.com/auth/experimentsandconfigs")
  "What the sign-in asks Google for.")

(defparameter +callback-port+ 51121
  "The loopback port Google is asked to redirect to, Antigravity's.")

(defparameter +callback-path+ "/oauth-callback")

(defparameter +callback-seconds+ 300
  "How long a sign-in waits for the browser's answer.")

(defparameter +expiry-skew-seconds+ 300
  "Taken off Google's expiry when it is kept: omp's skew.")

(defparameter +refresh-skew-seconds+ 60
  "A kept token this close to its expiry is refreshed before it is sent.")

(defvar *operation-poll-seconds* 1
  "The wait between two polls of the free tier being provisioned.")

(defparameter +onboard-seconds+ 30
  "How long the free tier's provisioning may take before the sign-in gives up.")

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

;;; --- the Antigravity project -------------------------------------------------------

(defparameter +metadata+ '(("ideType" . "ANTIGRAVITY"))
  "The metadata Antigravity's own control requests send.")

(defun metadata ()
  "+METADATA+ as a JSON object."
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key . value) in +metadata+ do (setf (gethash key object) value))
    object))

(defun project-headers (access)
  "The headers of an Antigravity control request."
  `(("Authorization" . ,(format nil "Bearer ~a" access))
    ("Content-Type" . "application/json")
    ("User-Agent" . ,(client-user-agent))))

(defun control-request (label method url headers &optional body)
  "One Antigravity control request: its decoded answer, or SIGNIN-FAILED
naming LABEL with the status and Google's words."
  (multiple-value-bind (text status)
      (exchange method url :headers headers :content (and body (shasht:write-json body nil)))
    (unless (eql status 200)
      (refuse "~a failed: ~a ~a" label status text))
    (let ((data (json-of text)))
      (unless (hash-table-p data)
        (refuse "failed to unmarshal ~a response" label))
      data)))

(defun load-code-assist (headers)
  "Antigravity's loadCodeAssist: asked again with the project it named when
the account has no paid tier, as the real client does."
  (let* ((url (format nil "~a/v1internal:loadCodeAssist" +base+))
         (data (control-request "loadCodeAssist" :post url headers (nlk:json-object "metadata" (metadata))))
         (project (nlk:json-value data :text "cloudaicompanionProject")))
    (if (and project (null (nlk:json-value data :object "paidTier")))
        (control-request "loadCodeAssist" :post url headers
                         (nlk:json-object "cloudaicompanionProject" project "metadata" (metadata)))
        data)))

(defun assert-free-tier-eligible (data)
  "Refuse in Google's words when the account may not take the free tier."
  (unless (find "free-tier" (nlk:json-array data "allowedTiers")
                :key (lambda (tier) (nlk:json-value tier :string "id")) :test #'equal)
    (let ((tier (find "free-tier" (nlk:json-array data "ineligibleTiers")
                      :key (lambda (tier) (nlk:json-value tier :string "tierId")) :test #'equal)))
      (alexandria:when-let (reason (nlk:json-value tier :text "reasonMessage"))
        (refuse "~a~@[~%~a~]" reason (nlk:json-value tier :text "validationUrl"))))))

(defun onboard-user (headers cancelled)
  "Provision the free tier: onboardUser, its operation polled until done."
  (let* ((deadline (+ (get-universal-time) +onboard-seconds+))
         (operation (control-request "onboardUser" :post (format nil "~a/v1internal:onboardUser" +base+) headers
                                     (nlk:json-object "tierId" "free-tier" "metadata" (metadata)))))
    (loop
      (when (nlk:json-value operation :boolean "done")
        (alexandria:when-let (error (nlk:json-value operation :object "error"))
          (refuse "OnboardUser operation failed: ~@[~a: ~]~a"
                  (nlk:json-value error :integer "code")
                  (or (nlk:json-value error :text "message") (nlk:encode-json-object error))))
        (unless (nlk:json-value operation :object "response")
          (refuse "failed to unmarshal OnboardUserResponse"))
        (return))
      (when (> (get-universal-time) deadline)
        (refuse "onboardUser timed out after ~dms" (* 1000 +onboard-seconds+)))
      (wait-seconds *operation-poll-seconds* cancelled)
      (let ((name (nlk:json-value operation :text "name")))
        (unless name
          (refuse "onboardUser returned an operation without a name"))
        (setf operation (control-request "onboardUser operation" :get
                                         (format nil "~a/v1internal/~a" +base+ name) headers))))))

(defun discover-project (access cancelled)
  "The Antigravity project of the account ACCESS signs in: the free tier
provisioned first when the account has no tier yet."
  (let ((headers (project-headers access)))
    (handler-case
        (let ((initial (load-code-assist headers)))
          (assert-free-tier-eligible initial)
          (unless (nlk:json-value initial :object "currentTier")
            (when (funcall cancelled) (error 'signin-cancelled))
            (onboard-user headers cancelled))
          (or (nlk:json-value (load-code-assist headers) :text "cloudaicompanionProject")
              (refuse "loadCodeAssist did not return a cloudaicompanionProject")))
      ((or signin-failed signin-cancelled) (condition) (error condition))
      (error (e) (refuse "Could not discover an Antigravity project. ~a" e)))))

;;; --- auth.json ------------------------------------------------------------------

(defvar *store-lock* (bt2:make-recursive-lock :name "google-antigravity auth.json")
  "Held across one read-modify-write of auth.json, and across a refresh.")

(defun save-entry (path entry)
  "Set oauth_tokens.google-antigravity to ENTRY in the auth.json at PATH (NIL
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
