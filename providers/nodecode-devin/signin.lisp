;;;; signin.lisp --- the Devin browser sign-in, and the kept session token.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): the `oauth-code' login of compat/rules/
;;;; auth/devin.kdl as ai/src/registry/engine/oauth-code.ts and common.ts run
;;;; it (its compiled form in catalog/src/compat/rules.json), the loopback
;;;; callback of registry/oauth/callback-server.ts and the PKCE of
;;;; registry/oauth/pkce.ts.
;;;;
;;;; The sign-in is an authorization-code grant with PKCE and no client id:
;;;; the operator opens https://app.devin.ai/auth/cli/continue, Devin
;;;; redirects the browser to http://127.0.0.1:59653/callback (another port
;;;; when that one is taken), and the code it carries is exchanged, as JSON
;;;; beside the PKCE verifier, at https://api.devin.ai/auth/cli/token for a
;;;; session token. When the browser runs on another machine, the address it
;;;; lands on is pasted back with /devin code.
;;;;
;;;; The token is both what omp keeps as the access and as the refresh token.
;;;; It is a JWT; it expires at its `exp' less omp's five-minute skew, or a
;;;; year from the sign-in when it names none. omp has no refresh for Devin
;;;; (refresh "none"): an expired sign-in is set aside and a new one is
;;;; asked for, which is what the :CREDENTIAL hook does here.
;;;;
;;;; auth.json keeps the sign-in under oauth_tokens.devin:
;;;;   {"access_token": T, "refresh_token": T, "expires_at": epoch seconds,
;;;;    "api_endpoint": "https://api.devin.ai", "enterprise_url": "https://app.devin.ai"}

(in-package #:nodecode-devin)

(defparameter +authorize-url+ "https://app.devin.ai/auth/cli/continue")

(defparameter +token-url+ "https://api.devin.ai/auth/cli/token")

(defparameter +api-endpoint+ "https://api.devin.ai"
  "The credential's api endpoint, a literal of the rule.")

(defparameter +enterprise-url+ "https://app.devin.ai"
  "The credential's enterprise url, a literal of the rule.")

(defparameter +callback-host+ "127.0.0.1")

(defparameter +callback-port+ 59653
  "The loopback port Devin is asked to redirect to.")

(defparameter +callback-path+ "/callback")

(defparameter +callback-seconds+ 300
  "How long a sign-in waits for the browser's answer (omp's DEFAULT_TIMEOUT).")

(defparameter +expiry-skew-seconds+ 300
  "Taken off the JWT's exp when it is kept: the rule's skew-ms.")

(defparameter +fallback-seconds+ (* 365 24 60 60)
  "How long a token naming no exp is kept: the rule's fallback-ms, a year.")

(defparameter +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))

(defun unix-now ()
  "Seconds since the Unix epoch."
  (- (get-universal-time) +unix-epoch+))

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

(defun exchange (url &key headers content (timeout 30))
  "(values TEXT STATUS) of one POST of CONTENT to URL. A refusal answers its
status and body, not a signal."
  (handler-case
      (multiple-value-bind (body status)
          (dex:post url :headers headers :content content :connect-timeout timeout :read-timeout timeout)
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
  "PAIRS, alternating names and values, as an urlencoded query; a NIL value is left out."
  (quri:url-encode-params (loop for (name value) on pairs by #'cddr
                                when value collect (cons name value))))

;;; --- PKCE, the state, the JWT -----------------------------------------------------------

(defun base64url (octets)
  "OCTETS as unpadded base64url."
  (string-right-trim "." (cl-base64:usb8-array-to-base64-string octets :uri t)))

(defun sha256 (text)
  "The SHA-256 digest of TEXT's UTF-8 bytes, as octets."
  (let ((hex (subseq (nlk:sha256-text text) 7)))
    (octets (loop for at from 0 below 64 by 2
                  collect (parse-integer hex :start at :end (+ at 2) :radix 16)))))

(defun pkce ()
  "(values VERIFIER CHALLENGE): 96 random bytes base64url, and the S256 of it (omp's generatePKCE)."
  (let ((verifier (base64url (nlk:random-bytes 96))))
    (values verifier (base64url (sha256 verifier)))))

(defun uuid ()
  "A random version-4 UUID: the state a sign-in carries (state \"uuid\")."
  (let ((bytes (nlk:random-bytes 16)))
    (setf (aref bytes 6) (logior #x40 (logand #x0f (aref bytes 6)))
          (aref bytes 8) (logior #x80 (logand #x3f (aref bytes 8))))
    (let ((hex (format nil "~(~{~2,'0x~}~)" (coerce bytes 'list))))
      (format nil "~a-~a-~a-~a-~a" (subseq hex 0 8) (subseq hex 8 12) (subseq hex 12 16)
              (subseq hex 16 20) (subseq hex 20 32)))))

(defun jwt-claims (token)
  "The payload of the JWT TOKEN, decoded without verification, or NIL."
  (let ((parts (uiop:split-string (or token "") :separator ".")))
    (when (and (= 3 (length parts)) (plusp (length (second parts))))
      (ignore-errors
       (let* ((payload (second parts))
              (padded (concatenate 'string payload
                                   (make-string (mod (- (length payload)) 4) :initial-element #\.))))
         (nlk:decode-json (sb-ext:octets-to-string
                           (cl-base64:base64-string-to-usb8-array padded :uri t)
                           :external-format :utf-8)))))))

(defun token-expiry (token)
  "When TOKEN is kept as expiring, in epoch seconds: its exp less the skew,
else a year from now."
  (let ((exp (nlk:json-value (jwt-claims token) :number "exp")))
    (if exp
        (- (floor exp) +expiry-skew-seconds+)
        (+ (unix-now) +fallback-seconds+))))

;;; --- the authorization address and the token ----------------------------------------

(defun authorize-url (state challenge redirect)
  "The address the operator opens: the standard parameters, then the rule's own."
  (format nil "~a?~a" +authorize-url+
          (form "response_type" "code"
                "redirect_uri" redirect
                "code_challenge" challenge
                "code_challenge_method" "S256"
                "state" state
                "prompt" "select_account")))

(defun token-entry (token)
  "The auth.json entry of a Devin session TOKEN."
  (nlk:json-object "access_token" token
                   "refresh_token" token
                   "expires_at" (token-expiry token)
                   "api_endpoint" +api-endpoint+
                   "enterprise_url" +enterprise-url+))

(defun exchange-code (code verifier)
  "POST CODE and the PKCE VERIFIER to Devin's token endpoint as JSON: the session token."
  (multiple-value-bind (text status)
      (exchange +token-url+
                :headers '(("Accept" . "application/json") ("Content-Type" . "application/json"))
                :content (nlk:encode-json-object (nlk:json-object "code" code "code_verifier" verifier)))
    (unless (ok-p status)
      (refuse "devin token exchange failed: ~a ~a" status (subseq (or text "") 0 (min 500 (length (or text ""))))))
    (or (nlk:json-value (json-of text) :text "token")
        (refuse "devin token response missing access token: ~a" (subseq (or text "") 0 (min 500 (length (or text ""))))))))

;;; --- auth.json ------------------------------------------------------------------

(defvar *store-lock* (bt2:make-lock :name "devin auth.json")
  "Held across one read-modify-write of auth.json.")

(defun save-entry (path entry)
  "Set oauth_tokens.devin to ENTRY in the auth.json at PATH (NIL takes it
out), every other field kept; written atomically, mode 0600."
  (bt2:with-lock-held (*store-lock*)
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

(defun expired-p (entry)
  "Whether ENTRY's token has passed the expiry it was kept with. omp has no
refresh for Devin, so an expired token is set aside, never refreshed."
  (let ((expires (nlk:json-value entry :number "expires_at")))
    (and expires (<= expires (unix-now)))))

;;; --- the loopback callback --------------------------------------------------------------

(defun open-listener ()
  "A listener on 127.0.0.1 at +CALLBACK-PORT+, or on a free port when that
one is taken (the rule's port fallback): (values LISTENER PORT)."
  (let ((listener (handler-case (usocket:socket-listen +callback-host+ +callback-port+
                                                       :reuse-address t :backlog 8
                                                       :element-type '(unsigned-byte 8))
                    (usocket:address-in-use-error ()
                      (usocket:socket-listen +callback-host+ 0 :reuse-address t :backlog 8
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
                 :external-format :utf-8))
        (crlf (format nil "~c~c" #\Return #\Linefeed)))
    (write-sequence (sb-ext:string-to-octets
                     (format nil "HTTP/1.1 ~d ~a~aContent-Type: text/html; charset=utf-8~aContent-Length: ~d~aConnection: close~a~a"
                             status (if (= status 200) "OK" "Not Found") crlf crlf (length octets) crlf crlf crlf)
                     :external-format :latin-1)
                    stream)
    (write-sequence octets stream)
    (finish-output stream)))

(defun callback-answer (target)
  "(values CODE STATE ERROR) a redirect TARGET (a URL or its path) carries;
the code rides `code', or `authCode' (parseNativeCallback)."
  (let* ((query (let ((mark (position #\? target)))
                  (if mark (subseq target (1+ mark)) "")))
         (params (ignore-errors (quri:url-decode-params (subseq query 0 (or (position #\# query) (length query)))))))
    (flet ((param (name) (let ((value (cdr (assoc name params :test #'equal))))
                           (and (stringp value) (plusp (length value)) value))))
      (values (or (param "code") (param "authCode"))
              (param "state")
              (let ((error (param "error")))
                (and error (or (param "error_description") error)))))))

;;; --- the sign-in ---------------------------------------------------------------------

(defstruct (flow (:copier nil))
  (listener nil)
  (port nil)
  (state "")
  (verifier "")
  (redirect-uri "")
  ;; (CODE . STATE) once the answer arrived, from the browser or pasted
  (answer nil)
  ;; why the answer can never come, once it cannot
  (failure nil)
  (cancelled nil)
  (thread nil))

(defvar *flow* nil
  "The sign-in in progress, or NIL.")

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
                              (setf (flow-failure flow) (format nil "Devin refused the sign-in: ~a" error))
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
  "Serve FLOW's listener until its answer arrives: (values CODE STATE)."
  (let ((deadline (+ (get-internal-real-time) (* +callback-seconds+ internal-time-units-per-second))))
    (loop
      (when (flow-cancelled flow) (error 'signin-cancelled))
      (alexandria:when-let (failure (flow-failure flow)) (refuse "~a" failure))
      (alexandria:when-let (answer (flow-answer flow)) (return (values (car answer) (cdr answer))))
      (when (> (get-internal-real-time) deadline)
        (refuse "no answer from the browser within ~d seconds" +callback-seconds+))
      (unless (and (flow-listener flow) (ignore-errors (take-callback flow)))
        (sleep 0.05)))))

(defun sign-in (flow auth-path)
  "The background half of a sign-in: wait for the code, exchange it, keep the
token, and say how it went."
  (handler-case
      (let* ((code (prog1 (await-code flow) (close-listener flow)))
             ;; a code may arrive as code#state; the code is what is exchanged
             (code (subseq code 0 (or (position #\# code) (length code))))
             (token (exchange-code code (flow-verifier flow))))
        (when (flow-cancelled flow) (error 'signin-cancelled))
        (let ((entry (save-entry auth-path (token-entry token))))
          ;; a failure said before stands no longer; the success is said once
          (nle:notice nil :key +key+)
          (nle:notice (format nil "devin: signed in; the session token holds until ~a"
                              (expiry-text (nlk:json-value entry :number "expires_at"))))))
    (signin-cancelled () nil)
    (error (e)
      (unless (flow-cancelled flow)
        (nle:notice (format nil "devin: sign-in failed: ~a" e) :level :warning :key +key+))))
  (close-listener flow)
  (when (eq *flow* flow) (setf *flow* nil)))

(defun expiry-text (seconds)
  "SECONDS since the epoch as an ISO date and time, UTC."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (+ +unix-epoch+ (floor seconds)) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d UTC" year month day hour minute second)))

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
    (multiple-value-bind (verifier challenge) (pkce)
      (let* ((state (uuid))
             (redirect-uri (format nil "http://~a:~d~a" +callback-host+ port +callback-path+))
             (flow (make-flow :listener listener :port port :state state :verifier verifier
                              :redirect-uri redirect-uri)))
        (setf *flow* flow
              (flow-thread flow) (bt2:make-thread (lambda () (sign-in flow auth-path))
                                                  :name "devin sign-in"))
        (format nil "Sign in to Devin in your browser: ~a~%~
Nodecode listens at ~a for Devin's answer, finishes the sign-in and says so in a notice. ~
If the browser runs on another machine, paste the address it lands on with /devin code ADDRESS."
                (authorize-url state challenge redirect-uri) redirect-uri)))))

(defun paste (text)
  "Hand the pending sign-in the code TEXT carries: the address the browser
landed on, or the code alone (omp's pasted-code path)."
  (let ((flow *flow*)
        (trimmed (nlk:trimmed (or text ""))))
    (cond ((null flow) "devin: no sign-in is waiting; /devin login starts one")
          ((zerop (length trimmed)) "usage: /devin code ADDRESS-OR-CODE")
          (t (multiple-value-bind (code state error)
                 (if (find #\? trimmed)
                     (callback-answer trimmed)
                     (let ((mark (position #\# trimmed)))
                       (values (subseq trimmed 0 mark) (and mark (subseq trimmed (1+ mark))) nil)))
               (cond (error (setf (flow-failure flow) (format nil "Devin refused the sign-in: ~a" error))
                            (format nil "devin: Devin refused the sign-in: ~a" error))
                     ((and state (plusp (length state)) (not (equal state (flow-state flow))))
                      "devin: that address is not for the sign-in in progress")
                     ((or (null code) (zerop (length code))) "devin: that address carries no authorization code")
                     (t (setf (flow-answer flow) (cons code (flow-state flow)))
                        "devin: code received; finishing the sign-in")))))))
