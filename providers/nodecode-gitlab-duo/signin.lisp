;;;; signin.lisp --- the GitLab sign-in: the browser flow, the token it yields, its refresh.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; auth/gitlab-duo.kdl (the login and refresh rules), ai/src/registry/
;;;; engine/oauth-code.ts, refresh.ts and common.ts (the engine those rules
;;;; drive), ai/src/registry/oauth/callback-server.ts (the loopback callback,
;;;; the /launch redirect, the pasted-code parse), oauth/pkce.ts, and
;;;; oauth/gitlab-duo.ts (a new token drops the direct-access grants).
;;;;
;;;; GitLab's authorization-code flow with PKCE, scope api, through omp's
;;;; registered OAuth application, whose redirect is
;;;; http://localhost:8080/callback. GITLAB_CLIENT_ID and GITLAB_REDIRECT_URI
;;;; name an OAuth application of the operator's own instead (GitLab refuses
;;;; the bundled one when its registered redirect drifts), and GITLAB_TOKEN
;;;; skips the sign-in for a personal access token.
;;;;
;;;; Kept in auth.json under oauth_tokens.gitlab-duo:
;;;;   {"access_token", "refresh_token", "expires_at"}
;;;; expires_at in epoch seconds, five minutes inside what GitLab granted
;;;; (omp's skew); the credential refreshes the token when it is within a
;;;; minute of that, and writes the new one back.

(in-package #:nodecode-gitlab-duo)

(defparameter +client-id+ "da4edff2e6ebd2bc3208611e2768bc1c1dd7be791dc5ff26ca34ca9ee44f7d4b"
  "omp's GitLab OAuth application.")

(defparameter +scope+ "api")

(defparameter *callback-port* 8080
  "The port GitLab's registered redirect names, the one the callback listens on first.")

(defparameter +callback-path+ "/callback")

(defparameter +callback-host+ "localhost")

(defparameter +launch-path+ "/launch"
  "The callback server's short address that redirects to the sign-in page.")

(defparameter +login-seconds+ 300
  "How long a sign-in waits for its code: omp's callback timeout.")

(defparameter +skew-seconds+ 300
  "How far inside GitLab's grant the kept expiry is: omp's skew-ms.")

(defparameter +refresh-margin-seconds+ 60
  "How close to its expiry a token is refreshed before it is sent.")

;;; --- words and bytes ------------------------------------------------------------

(defun base64url (octets)
  "OCTETS as unpadded base64url."
  (string-right-trim "=" (substitute #\_ #\/ (substitute #\- #\+ (cl-base64:usb8-array-to-base64-string octets)))))

(defun sha256-octets (text)
  "The SHA-256 of TEXT's UTF-8 bytes, as 32 octets."
  (let ((hex (subseq (nlk:sha256-text text) 7))
        (out (make-array 32 :element-type '(unsigned-byte 8))))
    (dotimes (i 32 out)
      (setf (aref out i) (parse-integer hex :start (* 2 i) :end (+ 2 (* 2 i)) :radix 16)))))

(defun pkce ()
  "(values VERIFIER CHALLENGE): 96 random bytes as base64url, and its S256
challenge. omp's generatePKCE."
  (let ((verifier (base64url (nlk:random-bytes 96))))
    (values verifier (base64url (sha256-octets verifier)))))

(defun random-hex (count)
  "COUNT random bytes as lowercase hex: omp's default state."
  (format nil "~(~{~2,'0x~}~)" (coerce (nlk:random-bytes count) 'list)))

(defun form-encode (text)
  "TEXT as application/x-www-form-urlencoded spells it, the way URLSearchParams does."
  (with-output-to-string (out)
    (loop for byte across (sb-ext:string-to-octets text :external-format :utf-8)
          for char = (code-char byte)
          do (cond ((or (char<= #\a char #\z) (char<= #\A char #\Z) (char<= #\0 char #\9)
                        (find char "*-._"))
                    (write-char char out))
                   ((= byte 32) (write-char #\+ out))
                   (t (format out "%~2,'0X" byte))))))

(defun form-decode (text)
  "TEXT, form-encoded, decoded: + is a space, %XX a byte."
  (let ((octets (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop with i = 0
          while (< i (length text))
          do (let ((char (char text i)))
               (cond ((char= char #\+) (vector-push-extend 32 octets) (incf i))
                     ((and (char= char #\%) (<= (+ i 3) (length text))
                           (digit-char-p (char text (+ i 1)) 16) (digit-char-p (char text (+ i 2)) 16))
                      (vector-push-extend (parse-integer text :start (1+ i) :end (+ i 3) :radix 16) octets)
                      (incf i 3))
                     (t (loop for byte across (sb-ext:string-to-octets (string char) :external-format :utf-8)
                              do (vector-push-extend byte octets))
                        (incf i)))))
    (sb-ext:octets-to-string (coerce octets '(vector (unsigned-byte 8))) :external-format :utf-8)))

(defun query-string (pairs)
  "PAIRS, an alist of strings, as a query string (or a form body)."
  (format nil "~{~a~^&~}"
          (loop for (key . value) in pairs
                collect (format nil "~a=~a" (form-encode key) (form-encode value)))))

(defun query-params (text)
  "The name/value alist of TEXT, a query string with or without its ? or #."
  (loop for pair in (uiop:split-string (string-left-trim "?#" text) :separator "&")
        for equals = (position #\= pair)
        when (plusp (length pair))
          collect (cons (form-decode (subseq pair 0 equals))
                        (if equals (form-decode (subseq pair (1+ equals))) ""))))

(defun param (params name)
  "NAME's value in PARAMS, the first one, or NIL."
  (cdr (assoc name params :test #'string=)))

(defun url-p (text)
  "Whether TEXT reads as an absolute URL: a scheme, then a colon."
  (let ((colon (position #\: text)))
    (and colon (plusp colon) (alpha-char-p (char text 0))
         (every (lambda (char) (or (alphanumericp char) (find char "+.-"))) (subseq text 0 colon)))))

(defun url-query (url)
  "The query of URL, between its ? and its #, or \"\"."
  (let* ((hash (position #\# url))
         (question (position #\? url :end hash)))
    (if question (subseq url (1+ question) hash) "")))

(defun parse-callback-input (input)
  "(values CODE STATE) out of what the operator pasted: the redirect address,
a query string, or the bare code with its state after a #. omp's
parseCallbackInput."
  (let ((value (nlk:trimmed (or input ""))))
    (cond ((zerop (length value)) (values nil nil))
          ((url-p value)
           (let ((params (query-params (url-query value))))
             (values (param params "code") (param params "state"))))
          ((search "code=" value)
           (let ((params (query-params value)))
             (values (param params "code") (param params "state"))))
          (t (let* ((hash (position #\# value))
                    (next-hash (and hash (position #\# value :start (1+ hash)))))
               (if hash
                   (values (subseq value 0 hash) (subseq value (1+ hash) next-hash))
                   (values value nil)))))))

(defun excerpt (text)
  "TEXT's first 500 characters, as omp quotes a body."
  (subseq text 0 (min 500 (length text))))

;;; --- the token endpoint ---------------------------------------------------------------

(defun now ()
  "The time, in epoch seconds."
  (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0)))

(defun client-id ()
  "The OAuth application: GITLAB_CLIENT_ID's, else omp's."
  (or (nle::credential-env "GITLAB_CLIENT_ID") +client-id+))

(defun token-url ()
  (format nil "~a/oauth/token" (string-right-trim "/" (setting :gitlab-url))))

(defun token-entry (body previous what)
  "The oauth_tokens entry GitLab's token BODY grants: omp's credential map
(access_token, refresh_token kept from PREVIOUS when not rotated, expires_in
counted from created_at, less the skew). WHAT names the request in a refusal."
  (let ((access (nlk:json-value body :text "access_token"))
        (seconds (nlk:json-value body :number "expires_in"))
        (from (nlk:json-value body :number "created_at")))
    (unless access
      (error "gitlab-duo ~a response missing access token: ~a" what
             (excerpt (if body (nlk:encode-json-object body) ""))))
    (unless seconds
      (error "gitlab-duo ~a response missing expires_in" what))
    (nlk:json-object "access_token" access
                     "refresh_token" (or (nlk:json-value body :text "refresh_token")
                                         (nlk:json-value previous :text "refresh_token")
                                         "")
                     "expires_at" (- (+ (round (or from (now))) (round seconds)) +skew-seconds+))))

(defun token-request (pairs what)
  "POST the form PAIRS to GitLab's token endpoint; => the decoded answer.
A refusal signals with GitLab's status and body."
  (multiple-value-bind (text status)
      (http :post (token-url)
            :headers '(("Content-Type" . "application/x-www-form-urlencoded"))
            :content (query-string pairs))
    (unless (ok-p status)
      (error "gitlab-duo ~a failed: ~a ~a" what status (excerpt text)))
    (and (plusp (length text)) (ignore-errors (nlk:decode-json text)))))

(defun exchange-code (code state verifier redirect-uri)
  "The oauth_tokens entry CODE signs in to. A `code#state' paste is split."
  (declare (ignore state))
  (let ((code (subseq code 0 (position #\# code))))
    (prog1 (token-entry (token-request `(("grant_type" . "authorization_code")
                                         ("client_id" . ,(client-id))
                                         ("code" . ,code)
                                         ("redirect_uri" . ,redirect-uri)
                                         ("code_verifier" . ,verifier))
                                       "token exchange")
                        nil "token exchange")
      ;; omp's gitlab-duo-clear-cache: a new token, no old grants
      (clear-direct-access))))

(defun refresh-entry (entry)
  "ENTRY refreshed at GitLab's token endpoint."
  (prog1 (token-entry (token-request `(("grant_type" . "refresh_token")
                                       ("client_id" . ,(client-id))
                                       ("refresh_token" . ,(nlk:json-value entry :string "refresh_token")))
                                     "token refresh")
                      entry "token refresh")
    (clear-direct-access)))

;;; --- where the token is kept ----------------------------------------------------------

(defvar *store-lock* (bt2:make-recursive-lock :name "nodecode-gitlab-duo auth.json")
  "Held across one read-modify-write of auth.json, and across a refresh.")

(defun stored-entry (auth)
  "oauth_tokens.gitlab-duo of the parsed AUTH, or NIL."
  (nlk:json-value auth :object "oauth_tokens" +provider+))

(defun save-entry (entry &optional (auth-path nle::*auth-file-path*))
  "Make ENTRY oauth_tokens.gitlab-duo in the auth.json at AUTH-PATH, or take
it out when ENTRY is NIL, every other field kept: the way
NLE::SAVE-PROVIDER-API-KEY writes api_keys."
  (bt2:with-recursive-lock-held (*store-lock*)
    (let* ((path (merge-pathnames auth-path))
           (auth (or (nle::read-auth-file path) (make-hash-table :test #'equal)))
           (tokens (or (nlk:json-value auth :object "oauth_tokens")
                       (setf (gethash "oauth_tokens" auth) (make-hash-table :test #'equal)))))
      (if entry
          (setf (gethash +provider+ tokens) entry)
          (remhash +provider+ tokens))
      (nlk:write-file-atomically path (shasht:write-json auth nil)
                                 :mode #o600 :directory-mode #o700)
      entry)))

(defun fresh-token (entry auth-path)
  "ENTRY's access token, refreshed first, and written back to AUTH-PATH, when
it expires within a minute; a failed refresh says so and signals."
  (let ((expires (nlk:json-value entry :number "expires_at")))
    (if (or (null expires) (> expires (+ (now) +refresh-margin-seconds+)))
        (nlk:json-value entry :text "access_token")
        (bt2:with-recursive-lock-held (*store-lock*)
          ;; another round may have refreshed it while this one waited
          (let* ((current (or (stored-entry (ignore-errors (nle::read-auth-file auth-path))) entry))
                 (expires (nlk:json-value current :number "expires_at")))
            (if (and expires (> expires (+ (now) +refresh-margin-seconds+)))
                (nlk:json-value current :text "access_token")
                (handler-case
                    (nlk:json-value (save-entry (refresh-entry current) auth-path) :text "access_token")
                  (error (condition)
                    ;; a state the model should keep seeing until a sign-in
                    ;; clears it: every round on this provider fails until then
                    (nle:notice "gitlab-duo: the GitLab sign-in has expired and could not be refreshed; sign in again with /gitlab-duo login"
                                :level :warning :key +key+)
                    (error 'nle::credential-error
                           :detail (format nil "the GitLab Duo sign-in could not be refreshed (~a): sign in again with /gitlab-duo login"
                                           condition))))))))))

(defun credential (op next)
  "The :CREDENTIAL answer for gitlab-duo: the signed-in token, refreshed
when it is about to expire, else GITLAB_TOKEN. Nothing else: the env ladder's
family default is another provider's key and never goes to GitLab."
  ;; A probe (no endpoint) asks where the credential comes from: it answers
  ;; the token as stored and refreshes nothing.
  (if (equal (getf op :provider) +provider+)
      (let ((entry (stored-entry (getf op :auth))))
        (cond ((and entry (nlk:json-value entry :text "access_token"))
               (nle:make-credential (if (getf op :endpoint)
                                        (fresh-token entry (or (getf op :auth-path) nle::*auth-file-path*))
                                        (nlk:json-value entry :text "access_token"))
                                    :oauth))
              ((env-key) (nle:make-credential (env-key) :env))
              (t (nle:make-credential "public" :public))))
      (funcall next op)))

;;; --- the loopback callback ------------------------------------------------------------

(defun read-request-line (stream)
  "One CRLF line of an HTTP request off the octet STREAM, or NIL at its end."
  (let ((octets (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for byte = (read-byte stream nil nil)
          do (cond ((null byte) (return-from read-request-line
                                  (and (plusp (length octets))
                                       (map 'string #'code-char octets))))
                   ((= byte 10) (return))
                   ((= byte 13))
                   ((> (length octets) 16384) (return-from read-request-line nil))
                   (t (vector-push-extend byte octets))))
    (map 'string #'code-char octets)))

(defun respond (stream status reason &key (body "") location)
  "Write one HTTP response to the octet STREAM and flush it."
  (let* ((octets (sb-ext:string-to-octets body :external-format :utf-8))
         (lines (append (list (format nil "HTTP/1.1 ~d ~a" status reason)
                              "Content-Type: text/html; charset=utf-8"
                              (format nil "Content-Length: ~d" (length octets)))
                        (and location (list (format nil "Location: ~a" location)))
                        (list "Connection: close" "" ""))))
    (write-sequence (sb-ext:string-to-octets
                     (format nil (format nil "~~{~~a~~^~c~c~~}" #\Return #\Newline) lines)
                     :external-format :latin-1)
                    stream)
    (write-sequence octets stream)
    (force-output stream)))

(defun page (title text)
  "The small page the browser is left on."
  (format nil "<!doctype html><html><head><meta charset=\"utf-8\"><title>~a</title></head><body style=\"font-family:sans-serif;margin:3em\"><h1>~a</h1><p>~a</p></body></html>"
          title title text))

;;; --- the sign-in in flight -------------------------------------------------------------

(defstruct (flow (:constructor make-flow (state verifier challenge auth-path)))
  "One sign-in waiting for its code."
  state verifier challenge auth-path
  (redirect-uri nil)
  (callback-path +callback-path+)
  (url nil)
  (sockets '())
  (done nil)
  (mailbox (sb-concurrency:make-mailbox :name "nodecode-gitlab-duo sign-in"))
  (thread nil))

(defvar *flow* nil "The sign-in waiting for its code, or NIL.")

(defvar *flow-lock* (bt2:make-lock :name "nodecode-gitlab-duo sign-in"))

(defun answer-callback (flow stream)
  "Answer one request to the callback server on STREAM: the callback, the
launch redirect, or nothing. A callback carrying another sign-in's state is
refused and changes nothing, an error carrying ours ends the sign-in."
  (let* ((request (or (read-request-line stream) ""))
         (target (or (second (uiop:split-string request :separator " ")) "/"))
         (path (subseq target 0 (position #\? target))))
    (loop for line = (read-request-line stream) while (and line (plusp (length line))))
    (cond ((equal path (flow-callback-path flow))
           (let* ((params (query-params (url-query target)))
                  (code (param params "code"))
                  (state (or (param params "state") ""))
                  (denied (param params "error"))
                  (ours (string= state (flow-state flow))))
             (cond ((and denied (plusp (length denied)))
                    (let ((why (format nil "Authorization failed: ~a"
                                       (or (param params "error_description") denied))))
                      (when ours
                        (sb-concurrency:send-message (flow-mailbox flow) (list :error why)))
                      (respond stream 500 "Internal Server Error" :body (page "Sign-in failed" why))))
                   ((null code)
                    (respond stream 500 "Internal Server Error"
                             :body (page "Sign-in failed" "Missing authorization code")))
                   ((not ours)
                    (respond stream 500 "Internal Server Error"
                             :body (page "Sign-in failed" "State mismatch - possible CSRF attack")))
                   (t (sb-concurrency:send-message (flow-mailbox flow) (list :code code state))
                      (respond stream 200 "OK"
                               :body (page "Signed in to GitLab"
                                           "Nodecode has the code; you can close this tab."))))))
          ((equal path +launch-path+)
           (if (flow-done flow)
               (respond stream 503 "Service Unavailable" :body "OAuth launch URL is no longer active")
               (respond stream 302 "Found" :location (flow-url flow))))
          (t (respond stream 404 "Not Found" :body "Not Found")))))

(defun serve-callbacks (flow)
  "Answer FLOW's callback server until the sign-in is done, then close it."
  (unwind-protect
       (loop until (flow-done flow)
             do (dolist (socket (ignore-errors
                                 (usocket:wait-for-input (flow-sockets flow) :timeout 0.5 :ready-only t)))
                  (let ((connection (ignore-errors (usocket:socket-accept socket))))
                    (when connection
                      ;; a browser that opens a socket and says nothing gets ten
                      ;; seconds; the deadline is a serious condition, not an error
                      (unwind-protect
                           (handler-case
                               (sb-sys:with-deadline (:seconds 10)
                                 (answer-callback flow (usocket:socket-stream connection)))
                             (serious-condition () nil))
                        (ignore-errors (usocket:socket-close connection)))))))
    (dolist (socket (flow-sockets flow))
      (ignore-errors (usocket:socket-close socket)))))

(defun listen-on (port)
  "The loopback listeners for PORT: IPv4, and IPv6 beside it when this host
has it, both on the one port, so `localhost' reaches it whichever family the
browser resolves first."
  (let* ((v4 (usocket:socket-listen "127.0.0.1" port :reuse-address t :backlog 8
                                                     :element-type '(unsigned-byte 8)))
         (v6 (ignore-errors
              (usocket:socket-listen "::1" (usocket:get-local-port v4) :reuse-address t :backlog 8
                                                                        :element-type '(unsigned-byte 8)))))
    (if v6 (list v4 v6) (list v4))))

(defun loopback-host-p (host)
  (member host '("localhost" "127.0.0.1" "[::1]" "::1") :test #'string-equal))

(defun open-callback (flow)
  "Bind FLOW's callback server and set its redirect: GITLAB_REDIRECT_URI's
port and path when it names a loopback address (that port exactly), nothing
when it names another host (the code is pasted), else *CALLBACK-PORT* or,
when that is taken, any free port."
  (let ((override (nle::credential-env "GITLAB_REDIRECT_URI")))
    (if override
        (let* ((uri (quri:uri override))
               (host (quri:uri-host uri)))
          (unless (member (quri:uri-scheme uri) '("http" "https") :test #'string-equal)
            (error "Redirect URI override must use http:// or https://, got: ~a" override))
          (setf (flow-redirect-uri flow) override)
          (when (loopback-host-p host)
            (setf (flow-callback-path flow) (or (quri:uri-path uri) +callback-path+)
                  (flow-sockets flow) (listen-on (or (quri:uri-port uri) 80)))))
        (let ((sockets (handler-case (listen-on *callback-port*)
                         (error () (listen-on 0)))))
          (setf (flow-sockets flow) sockets
                (flow-redirect-uri flow) (format nil "http://~a:~d~a" +callback-host+
                                                 (usocket:get-local-port (first sockets))
                                                 +callback-path+))))))

(defun authorize-url (flow)
  "The address the operator opens: GitLab's standard authorize request with PKCE."
  (format nil "~a/oauth/authorize?~a" (string-right-trim "/" (setting :gitlab-url))
          (query-string `(("client_id" . ,(client-id))
                          ("response_type" . "code")
                          ("redirect_uri" . ,(flow-redirect-uri flow))
                          ("scope" . ,+scope+)
                          ("code_challenge" . ,(flow-challenge flow))
                          ("code_challenge_method" . "S256")
                          ("state" . ,(flow-state flow))))))

(defun say (text level)
  "TEXT to the operator, said once: an outcome is news, not a standing state."
  (nle:notice text :level level))

(defun finish-flow (flow)
  "Mark FLOW done, so its callback server closes, and forget it."
  (setf (flow-done flow) t)
  (bt2:with-lock-held (*flow-lock*)
    (when (eq *flow* flow) (setf *flow* nil))))

(defun run-flow (flow)
  "Wait for FLOW's code, trade it for a token, keep the token and say how it went."
  (unwind-protect
       (handler-case
           (let ((message (sb-concurrency:receive-message (flow-mailbox flow) :timeout +login-seconds+)))
             (case (first message)
               ((nil) (say (format nil "gitlab-duo: the sign-in timed out after ~d minutes with no code; start again with /gitlab-duo login"
                                   (floor +login-seconds+ 60))
                           :warning))
               (:error (say (format nil "gitlab-duo: the sign-in failed: ~a" (second message)) :warning))
               (:code (save-entry (exchange-code (second message) (third message) (flow-verifier flow)
                                                 (flow-redirect-uri flow))
                                  (flow-auth-path flow))
                      (nle:notice nil :key +key+)
                      (say "gitlab-duo: signed in to GitLab; pick a Duo model with /models" :info))))
         (serious-condition (condition)
           (say (format nil "gitlab-duo: the sign-in failed: ~a" condition) :warning)))
    (finish-flow flow)))

(defun cancel-flow ()
  "End the sign-in in flight, if any, saying nothing."
  (let ((flow (bt2:with-lock-held (*flow-lock*) (shiftf *flow* nil))))
    (when flow
      (setf (flow-done flow) t)
      (sb-concurrency:send-message (flow-mailbox flow) (list :cancel)))))

(defun start-login (&optional (auth-path nle::*auth-file-path*))
  "Start a sign-in that keeps its token at AUTH-PATH; => what the operator does next."
  (cancel-flow)
  (multiple-value-bind (verifier challenge) (pkce)
    (let ((flow (make-flow (random-hex 16) verifier challenge auth-path)))
      (open-callback flow)
      (setf (flow-url flow) (authorize-url flow))
      (bt2:with-lock-held (*flow-lock*) (setf *flow* flow))
      (when (flow-sockets flow)
        (bt2:make-thread (lambda () (serve-callbacks flow)) :name "nodecode-gitlab-duo callback"))
      (setf (flow-thread flow)
            (bt2:make-thread (lambda () (run-flow flow)) :name "nodecode-gitlab-duo sign-in"))
      (format nil "Open this address and sign in to GitLab:~%~a~%~%~:[GitLab sends the browser to ~a; paste~;The browser comes back to ~a and the sign-in finishes on its own. On another machine, paste~] the address it ends on, or the code in it, with~%  /gitlab-duo code <address or code>~%If GitLab answers \"The redirect URI included is not valid\", register an OAuth application of your own and set GITLAB_CLIENT_ID and GITLAB_REDIRECT_URI, or set GITLAB_TOKEN to a personal access token (scope api). The sign-in waits ~d minutes."
              (flow-url flow) (flow-sockets flow) (flow-redirect-uri flow) (floor +login-seconds+ 60)))))

(defun paste (text)
  "Hand the sign-in in flight the code TEXT carries; => what happened."
  (let ((flow *flow*))
    (if (null flow)
        "No sign-in is waiting for a code: start one with /gitlab-duo login."
        (multiple-value-bind (code state) (parse-callback-input text)
          (cond ((null code)
                 "That carries no code: paste the address the browser ended on, or the code in it.")
                ((and state (plusp (length state)) (string/= state (flow-state flow)))
                 "That address belongs to another sign-in (its state differs): paste the one this sign-in's page sent.")
                (t (sb-concurrency:send-message (flow-mailbox flow) (list :code code (or state "")))
                   "Code received: finishing the sign-in. The outcome follows as a notice."))))))

(defun status ()
  "Whether GitLab Duo is signed in."
  (let ((entry (stored-entry (ignore-errors (nle::read-auth-file nle::*auth-file-path*)))))
    (cond (*flow* "A sign-in is waiting for the browser, or a pasted code with /gitlab-duo code.")
          ((nlk:json-value entry :text "access_token")
           (let ((expires (nlk:json-value entry :number "expires_at")))
             (format nil "Signed in to GitLab~@[; the token is refreshed after ~a~]."
                     (and expires (local-time-string expires)))))
          ((env-key) "Not signed in; GITLAB_TOKEN is set and is used.")
          (t "Not signed in: /gitlab-duo login, or set GITLAB_TOKEN."))))

(defun local-time-string (epoch)
  "EPOCH seconds as an ISO time, UTC."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (+ epoch #.(encode-universal-time 0 0 0 1 1 1970 0)) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ" year month day hour minute second)))

(defun logout ()
  "Forget the GitLab token."
  (cancel-flow)
  (save-entry nil)
  (clear-direct-access)
  "Signed out: the GitLab token is gone from auth.json.")

(defun run-command (args session-id)
  "/gitlab-duo login | code TEXT | logout | status."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (space (position #\Space text))
         (verb (subseq text 0 space))
         (rest (if space (nlk:trimmed (subseq text space)) "")))
    (cond ((member verb '("" "status") :test #'string-equal) (status))
          ((string-equal verb "login") (start-login))
          ((string-equal verb "code") (paste rest))
          ((string-equal verb "logout") (logout))
          (t "Usage: /gitlab-duo login | code <address or code> | logout | status"))))
