;;;; signin.lisp --- the browser sign-in that yields an OpenRouter key.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; auth/openrouter.kdl (the login rule), ai/src/registry/engine/
;;;; oauth-code.ts and common.ts (the authorization-code engine that rule
;;;; drives), ai/src/registry/oauth/callback-server.ts (the loopback
;;;; callback, the /launch redirect, the pasted-code parse), oauth/pkce.ts and
;;;; registry/api-key-validation.ts (a pasted key's check).
;;;;
;;;; OpenRouter's PKCE flow has no client registration: the S256 verifier is
;;;; the only proof of identity, and OpenRouter never echoes a state, so none
;;;; is sent or checked. The browser comes back to
;;;; http://localhost:54549/callback (another port when that one is taken:
;;;; OpenRouter takes any loopback callback), the code it carries is
;;;; exchanged at /api/v1/auth/keys for a durable key, and that key is saved
;;;; where /connect saves one, api_keys.openrouter, the way
;;;; NLE::SAVE-PROVIDER-API-KEY writes it. A key pasted instead of the code
;;;; (sk-or-...) is checked against /api/v1/auth/key and saved the same way.
;;;; The key never expires, so nothing is refreshed.

(in-package #:nodecode-openrouter)

(defparameter +authorize-url+ "https://openrouter.ai/auth")

(defparameter +token-url+ "https://openrouter.ai/api/v1/auth/keys"
  "Where the code and its verifier buy a key.")

(defparameter +key-check-url+ "https://openrouter.ai/api/v1/auth/key"
  "What a pasted key is checked against: it authenticates keys, where
/models answers any bearer.")

(defparameter +key-prefix+ "sk-or-"
  "What a pasted key starts with.")

(defparameter *callback-port* 54549
  "The port the callback listens on first: omp's.")

(defparameter +callback-path+ "/callback")

(defparameter +callback-host+ "localhost")

(defparameter +launch-path+ "/launch"
  "The callback server's short address that redirects to the sign-in page.")

(defparameter +login-seconds+ 300
  "How long a sign-in waits for its code: omp's callback timeout.")

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
  "PAIRS, an alist of strings, as a query string."
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
a query string, or the bare code (or key) with any state after a #. omp's
parseCallbackInput."
  (let ((value (nlk:trimmed (or input ""))))
    (cond ((zerop (length value)) (values nil nil))
          ((and (url-p value) (not (uiop:string-prefix-p +key-prefix+ value)))
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

;;; --- one HTTP exchange --------------------------------------------------------------

(defun body-string (body)
  "BODY, as dexador answered it, as a string."
  (typecase body
    (string body)
    ((vector (unsigned-byte 8)) (sb-ext:octets-to-string body :external-format :utf-8))
    (null "")
    (t (princ-to-string body))))

(defun http (method url &key headers content)
  "(values TEXT STATUS) of one request, a refusal's status and body included."
  (handler-case
      (multiple-value-bind (body status)
          (ecase method
            (:get (dex:get url :headers headers :connect-timeout 30 :read-timeout 30))
            (:post (dex:post url :headers headers :content content
                                 :connect-timeout 30 :read-timeout 30)))
        (values (body-string body) status))
    (dex:http-request-failed (condition)
      (values (body-string (dex:response-body condition)) (dex:response-status condition)))))

(defun ok-p (status)
  "Whether STATUS is a 2xx."
  (and (integerp status) (<= 200 status 299)))

(defun excerpt (text)
  "TEXT's first 500 characters, as omp quotes a body."
  (subseq text 0 (min 500 (length text))))

;;; --- the code, or a pasted key, made a key ------------------------------------------

(defun check-key (key)
  "Signal unless OpenRouter accepts KEY: omp's validateApiKeyAgainstModelsEndpoint."
  (multiple-value-bind (text status)
      (http :get +key-check-url+ :headers `(("Authorization" . ,(format nil "Bearer ~a" key))))
    (unless (ok-p status)
      (error "OpenRouter API key validation failed (~a)~@[: ~a~]" status
             (let ((trimmed (nlk:trimmed text))) (and (plusp (length trimmed)) trimmed))))
    key))

(defun exchange-code (code verifier)
  "The key CODE buys with VERIFIER, or CODE itself when it is a pasted key
OpenRouter accepts."
  (if (uiop:string-prefix-p +key-prefix+ code)
      (check-key code)
      (let ((code (subseq code 0 (position #\# code))))
        (multiple-value-bind (text status)
            (http :post +token-url+
                  :headers '(("Content-Type" . "application/json"))
                  :content (nlk:encode-json-object
                            (nlk:json-object "code" code "code_verifier" verifier
                                             "code_challenge_method" "S256")))
          (unless (ok-p status)
            (error "openrouter token exchange failed: ~a ~a" status (excerpt text)))
          (or (nlk:json-value (and (plusp (length text)) (ignore-errors (nlk:decode-json text))) :text "key")
              (error "openrouter token response missing access token: ~a" (excerpt text)))))))

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
  (let ((octets (sb-ext:string-to-octets body :external-format :utf-8))
        (lines (list (format nil "HTTP/1.1 ~d ~a" status reason)
                     "Content-Type: text/html; charset=utf-8"
                     (format nil "Content-Length: ~d" (length (sb-ext:string-to-octets body :external-format :utf-8))))))
    (when location (setf lines (append lines (list (format nil "Location: ~a" location)))))
    (setf lines (append lines (list "Connection: close" "" "")))
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

(defstruct (flow (:constructor make-flow (verifier challenge auth-path)))
  "One sign-in waiting for its code."
  verifier challenge auth-path
  (redirect-uri nil)
  (url nil)
  (sockets '())
  (done nil)
  (mailbox (sb-concurrency:make-mailbox :name "nodecode-openrouter sign-in"))
  (thread nil))

(defvar *flow* nil "The sign-in waiting for its code, or NIL.")

(defvar *flow-lock* (bt2:make-lock :name "nodecode-openrouter sign-in"))

(defun answer-callback (flow stream)
  "Answer one request to the callback server on STREAM: the callback, the
launch redirect, or nothing."
  (let* ((request (or (read-request-line stream) ""))
         (target (second (uiop:split-string request :separator " ")))
         (path (subseq (or target "/") 0 (position #\? (or target "/")))))
    ;; the headers, read and dropped
    (loop for line = (read-request-line stream) while (and line (plusp (length line))))
    (cond ((equal path +callback-path+)
           (let* ((params (query-params (url-query (or target ""))))
                  (code (param params "code"))
                  (denied (param params "error")))
             (cond ((and denied (plusp (length denied)))
                    (let ((why (format nil "Authorization failed: ~a"
                                       (or (param params "error_description") denied))))
                      (sb-concurrency:send-message (flow-mailbox flow) (list :error why))
                      (respond stream 500 "Internal Server Error" :body (page "Sign-in failed" why))))
                   ((null code)
                    (respond stream 500 "Internal Server Error"
                             :body (page "Sign-in failed" "Missing authorization code")))
                   (t (sb-concurrency:send-message (flow-mailbox flow) (list :code code ""))
                      (respond stream 200 "OK"
                               :body (page "Signed in to OpenRouter"
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
has it, both on the one port. omp binds both so `localhost' reaches it
whichever family the browser resolves first."
  (let* ((v4 (usocket:socket-listen "127.0.0.1" port :reuse-address t :backlog 8
                                                     :element-type '(unsigned-byte 8)))
         (v6 (ignore-errors
              (usocket:socket-listen "::1" (usocket:get-local-port v4) :reuse-address t :backlog 8
                                                                        :element-type '(unsigned-byte 8)))))
    (if v6 (list v4 v6) (list v4))))

(defun open-callback (flow)
  "Bind FLOW's callback server, on *CALLBACK-PORT* or, when that is taken,
any free port; => FLOW's redirect address."
  (let ((sockets (handler-case (listen-on *callback-port*)
                   (error () (listen-on 0)))))
    (setf (flow-sockets flow) sockets
          (flow-redirect-uri flow) (format nil "http://~a:~d~a" +callback-host+
                                           (usocket:get-local-port (first sockets)) +callback-path+))))

(defun authorize-url (redirect-uri challenge)
  "The address the operator opens: OpenRouter's own authorize parameters."
  (format nil "~a?~a" +authorize-url+
          (query-string `(("callback_url" . ,redirect-uri)
                          ("code_challenge" . ,challenge)
                          ("code_challenge_method" . "S256")))))

(defun say (text level)
  "TEXT to the operator, said once: an outcome is news, not a standing state."
  (nle:notice text :level level))

(defun finish-flow (flow)
  "Mark FLOW done, so its callback server closes, and forget it."
  (setf (flow-done flow) t)
  (bt2:with-lock-held (*flow-lock*)
    (when (eq *flow* flow) (setf *flow* nil))))

(defun run-flow (flow)
  "Wait for FLOW's code, make it a key, save the key and say how it went."
  (unwind-protect
       (handler-case
           (let ((message (sb-concurrency:receive-message (flow-mailbox flow) :timeout +login-seconds+)))
             (case (first message)
               ((nil) (say (format nil "openrouter: the sign-in timed out after ~d minutes with no code; start again with /openrouter login"
                                   (floor +login-seconds+ 60))
                           :warning))
               (:error (say (format nil "openrouter: the sign-in failed: ~a" (second message)) :warning))
               (:code (let ((key (exchange-code (second message) (flow-verifier flow))))
                        (nle::save-provider-api-key +provider+ key :auth-path (flow-auth-path flow))
                        (say "openrouter: signed in; the key is saved, pick a model with /models" :info)))))
         (serious-condition (condition)
           (say (format nil "openrouter: the sign-in failed: ~a" condition) :warning)))
    (finish-flow flow)))

(defun cancel-flow ()
  "End the sign-in in flight, if any, saying nothing."
  (let ((flow (bt2:with-lock-held (*flow-lock*) (shiftf *flow* nil))))
    (when flow
      (setf (flow-done flow) t)
      (sb-concurrency:send-message (flow-mailbox flow) (list :cancel)))))

(defun start-login (&optional (auth-path nle::*auth-file-path*))
  "Start a sign-in that saves its key at AUTH-PATH; => what the operator does next."
  (cancel-flow)
  (multiple-value-bind (verifier challenge) (pkce)
    (let ((flow (make-flow verifier challenge auth-path)))
      (open-callback flow)
      (setf (flow-url flow) (authorize-url (flow-redirect-uri flow) challenge))
      (bt2:with-lock-held (*flow-lock*) (setf *flow* flow))
      (bt2:make-thread (lambda () (serve-callbacks flow)) :name "nodecode-openrouter callback")
      (setf (flow-thread flow)
            (bt2:make-thread (lambda () (run-flow flow)) :name "nodecode-openrouter sign-in"))
      (format nil "Open this address and sign in to OpenRouter:~%~a~%~%The browser comes back to ~a and the sign-in finishes on its own (the same page: ~a). On another machine, paste the address the browser ends on, or a key, with~%  /openrouter code <address, code or sk-or-... key>~%The sign-in waits ~d minutes."
              (flow-url flow) (flow-redirect-uri flow)
              (format nil "http://~a:~d~a" +callback-host+
                      (usocket:get-local-port (first (flow-sockets flow))) +launch-path+)
              (floor +login-seconds+ 60)))))

(defun paste (text)
  "Hand the sign-in in flight the code (or key) TEXT carries; => what happened."
  (let ((flow *flow*))
    (if (null flow)
        "No sign-in is waiting for a code: start one with /openrouter login, or save a key with /connect."
        (let ((code (parse-callback-input text)))
          (cond ((null code)
                 "That carries no code: paste the address the browser ended on, the code in it, or an sk-or-... key.")
                (t (sb-concurrency:send-message (flow-mailbox flow) (list :code code ""))
                   "Received: finishing the sign-in. The outcome follows as a notice."))))))

(defun status ()
  "Whether OpenRouter has a key saved."
  (let ((auth (ignore-errors (nle::read-auth-file nle::*auth-file-path*))))
    (cond (*flow* (format nil "A sign-in is waiting for the browser at ~a, or a pasted code with /openrouter code."
                          (flow-redirect-uri *flow*)))
          ((nle::auth-api-key auth +provider+) "Signed in: an OpenRouter key is saved in auth.json.")
          ((env-key) "No key saved; OPENROUTER_API_KEY is set and is used.")
          (t "Not signed in: /openrouter login, or save a key with /connect."))))

(defun logout ()
  "Forget the saved key."
  (cancel-flow)
  (nle::save-provider-api-key +provider+ nil)
  (format nil "Signed out: the key is gone from auth.json (it stays live at OpenRouter until you delete it at ~a)." +key-page+))

(defun run-command (args session-id)
  "/openrouter login | code TEXT | logout | status."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (space (position #\Space text))
         (verb (subseq text 0 space))
         (rest (if space (nlk:trimmed (subseq text space)) "")))
    (cond ((member verb '("" "status") :test #'string-equal) (status))
          ((string-equal verb "login") (start-login))
          ((string-equal verb "code") (paste rest))
          ((string-equal verb "logout") (logout))
          (t "Usage: /openrouter login | code <address, code or key> | logout | status"))))
