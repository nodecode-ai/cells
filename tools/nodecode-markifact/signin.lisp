;;;; signin.lisp --- the browser sign-in that yields a Markifact access token.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Markifact's server has no API keys: it is OAuth 2.1 with PKCE and dynamic
;;;; client registration, the authorization the MCP spec puts in front of a
;;;; remote server. The flow, in the order the spec reads it:
;;;;
;;;;   1. An unauthenticated call answers 401 with a Bearer challenge naming the
;;;;      server's protected-resource metadata (RFC 9728); without one, the
;;;;      well-known path under the endpoint's own URL is read.
;;;;   2. That document names the authorization server, whose metadata
;;;;      (RFC 8414) carries /authorize, /token and /register.
;;;;   3. This client registers itself (RFC 7591) with a loopback redirect
;;;;      (RFC 8252), and keeps the client_id it was given.
;;;;   4. The authorization code grant with PKCE S256, the resource (RFC 8707)
;;;;      and a state this client checks; the browser comes back to
;;;;      127.0.0.1, or the address it landed on is pasted back.
;;;;   5. The tokens land in the shared auth.json under oauth_tokens.markifact,
;;;;      refreshed while there is a refresh token and it works.

(in-package #:nodecode-markifact)

(defparameter +callback-host+ "127.0.0.1")
(defparameter +callback-path+ "/callback")
(defparameter +login-seconds+ 300
  "How long a sign-in waits for its code.")

(defvar *open-browser-p* t
  "Whether a sign-in hands its address to a browser here; the tests say no.")

;;; --- words and bytes --------------------------------------------------------------

(defun base64url (octets)
  "OCTETS as unpadded base64url."
  (string-right-trim "=" (substitute #\_ #\/ (substitute #\- #\+
                                                 (cl-base64:usb8-array-to-base64-string octets)))))

(defun sha256-octets (text)
  "The SHA-256 of TEXT's UTF-8 bytes, as 32 octets."
  (let ((hex (subseq (nlk:sha256-text text) 7))
        (out (make-array 32 :element-type '(unsigned-byte 8))))
    (dotimes (i 32 out)
      (setf (aref out i) (parse-integer hex :start (* 2 i) :end (+ 2 (* 2 i)) :radix 16)))))

(defun pkce ()
  "(values VERIFIER CHALLENGE): 32 random bytes as base64url, and their S256."
  (let ((verifier (base64url (nlk:random-bytes 32))))
    (values verifier (base64url (sha256-octets verifier)))))

(defun random-state-token ()
  "A state value the answer must echo."
  (base64url (nlk:random-bytes 16)))

(defun form-encode (text)
  "TEXT as application/x-www-form-urlencoded spells it."
  (with-output-to-string (out)
    (loop for byte across (sb-ext:string-to-octets text :external-format :utf-8)
          for char = (code-char byte)
          do (cond ((or (char<= #\a char #\z) (char<= #\A char #\Z) (char<= #\0 char #\9)
                        (find char "*-._"))
                    (write-char char out))
                   ((= byte 32) (write-char #\+ out))
                   (t (format out "%~2,'0X" byte))))))

(defun form-decode (text)
  "TEXT, form-encoded, decoded."
  (let ((octets (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop with i = 0
          while (< i (length text))
          do (let ((char (char text i)))
               (cond ((char= char #\+) (vector-push-extend 32 octets) (incf i))
                     ((and (char= char #\%) (<= (+ i 3) (length text))
                           (digit-char-p (char text (+ i 1)) 16) (digit-char-p (char text (+ i 2)) 16))
                      (vector-push-extend (parse-integer text :start (1+ i) :end (+ i 3) :radix 16) octets)
                      (incf i 3))
                     (t (loop for byte across (sb-ext:string-to-octets (string char)
                                                                       :external-format :utf-8)
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
  (let ((text (string-left-trim "?#" text)))
    (loop for pair in (uiop:split-string text :separator "&")
          for equals = (position #\= pair)
          when (plusp (length pair))
            collect (cons (form-decode (subseq pair 0 equals))
                          (if equals (form-decode (subseq pair (1+ equals))) "")))))

(defun param (params name)
  "NAME's value in PARAMS, or NIL."
  (cdr (assoc name params :test #'string=)))

(defun url-p (text)
  "Whether TEXT reads as an absolute URL."
  (let ((colon (position #\: text)))
    (and colon (plusp colon) (alpha-char-p (char text 0))
         (every (lambda (char) (or (alphanumericp char) (find char "+.-")))
                (subseq text 0 colon)))))

(defun url-query (url)
  "The query of URL, between its ? and its #, or \"\"."
  (let* ((hash (position #\# url))
         (question (position #\? url :end hash)))
    (if question (subseq url (1+ question) hash) "")))

(defun origin-of (url)
  "URL's origin: scheme, host and port, with no path."
  (let ((colon (position #\: url)))
    (if (null colon)
        url
        (let ((slash (position #\/ url :start (+ colon 3))))
          (if slash (subseq url 0 slash) url)))))

(defun path-of (url)
  "URL's path, without its query, or \"/\"."
  (let* ((colon (position #\: url))
         (slash (and colon (position #\/ url :start (+ colon 3)))))
    (if (null slash)
        "/"
        (let* ((rest (subseq url slash))
               (question (position #\? rest)))
          (if question (subseq rest 1 question) (subseq rest 1))))))

;;; --- discovery --------------------------------------------------------------------

(defun get-json (url)
  "URL's JSON answer, or NIL when it does not answer 2xx."
  (multiple-value-bind (text status)
      (exchange :get url :headers (list (cons "Accept" "application/json")))
    (and status (< status 300) (plusp (length text))
         (ignore-errors (nlk:decode-json text)))))

(defun resource-metadata (endpoint)
  "The protected-resource metadata RFC 9728 puts under ENDPOINT's origin."
  (let* ((path (path-of endpoint))
         (origin (origin-of endpoint))
         (with-path (format nil "~a/.well-known/oauth-protected-resource/~a" origin
                             (string-left-trim "/" path))))
    (or (get-json with-path)
        (get-json (format nil "~a/.well-known/oauth-protected-resource" origin)))))

(defun authorization-server (endpoint)
  "The authorization server ENDPOINT's metadata names, else its own origin."
  (let* ((metadata (resource-metadata endpoint))
         (servers (and metadata (nlk:json-value metadata :array "authorization_servers"))))
    (if (and servers (plusp (length servers)))
        (aref servers 0)
        (origin-of endpoint))))

(defun server-metadata (server)
  "SERVER's authorization-server metadata, or an empty object."
  (or (get-json (format nil "~a/.well-known/oauth-authorization-server" server))
      (get-json (format nil "~a/.well-known/openid-configuration" server))
      (nlk:make-json-object)))

(defun metadata-url (metadata key fallback)
  (or (nlk:json-value metadata :string key) fallback))

(defun register-client (registration-endpoint redirect-uri)
  "Register this client (RFC 7591) and answer the client_id it was given."
  (let ((body (nlk:encode-json-object
               (nlk:json-object "client_name" "Nodecode"
                                "redirect_uris" (vector redirect-uri)
                                "grant_types" (vector "authorization_code" "refresh_token")
                                "response_types" (vector "code")
                                "token_endpoint_auth_method" "none"))))
    (multiple-value-bind (text status)
        (exchange :post registration-endpoint
                  :headers (list (cons "Content-Type" "application/json")
                                 (cons "Accept" "application/json"))
                  :content body)
      (unless (and status (< status 300))
        (fail "Markifact's client registration failed (~a)~@[: ~a~]" status (excerpt text)))
      (let ((object (ignore-errors (nlk:decode-json text))))
        (or (and object (nlk:json-value object :string "client_id"))
            (fail "Markifact's client registration answered no client_id"))))))

;;; --- the code made a token ---------------------------------------------------------

(defun token-request (token-endpoint pairs)
  "POST PAIRS to TOKEN-ENDPOINT as a form, and answer its JSON object."
  (multiple-value-bind (text status)
      (exchange :post token-endpoint
                :headers (list (cons "Content-Type" "application/x-www-form-urlencoded")
                               (cons "Accept" "application/json"))
                :content (query-string pairs))
    (unless (and status (< status 300))
      (fail "Markifact's token request failed (~a)~@[: ~a~]" status (excerpt text)))
    (or (ignore-errors (nlk:decode-json text))
        (fail "Markifact's token answer was not JSON"))))

(defun keep-answer (answer client-id token-endpoint resource)
  "Save ANSWER's tokens and answer the access token; NIL when it carried none."
  (let ((access (nlk:json-value answer :string "access_token"))
        (expires-in (nlk:json-value answer :integer "expires_in")))
    (when access
      (save-token :access access
                  :refresh (nlk:json-value answer :string "refresh_token")
                  :expires-at (and expires-in (+ (get-universal-time) expires-in))
                  :client-id client-id
                  :token-endpoint token-endpoint
                  :resource resource)
      access)))

(defun refresh-token (entry)
  "A new access token for ENTRY, or NIL when there is nothing to refresh with."
  (let ((refresh (entry-string entry "refresh_token"))
        (token-endpoint (entry-string entry "token_endpoint")))
    (when (and refresh token-endpoint)
      (handler-case
          (keep-answer (token-request token-endpoint
                                       (list (cons "grant_type" "refresh_token")
                                             (cons "refresh_token" refresh)
                                             (cons "client_id" (entry-string entry "client_id"))
                                             (cons "resource" (entry-string entry "resource"))))
                       (entry-string entry "client_id") token-endpoint
                       (entry-string entry "resource"))
        (markifact-error (condition)
          (declare (ignore condition))
          nil)))))

;;; --- the loopback callback ---------------------------------------------------------

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

(defun respond (stream status reason &key (body ""))
  "Write one HTTP response to the octet STREAM and flush it."
  (let ((length (length (sb-ext:string-to-octets body :external-format :utf-8))))
    (write-sequence (sb-ext:string-to-octets
                     (format nil "HTTP/1.1 ~d ~a~c~cContent-Type: text/html; charset=utf-8~c~cContent-Length: ~d~c~cConnection: close~c~c~c~c"
                             status reason #\Return #\Newline #\Return #\Newline length
                             #\Return #\Newline #\Return #\Newline #\Return #\Newline)
                     :external-format :latin-1)
                    stream)
    (write-sequence (sb-ext:string-to-octets body :external-format :utf-8) stream)
    (force-output stream)))

(defun page (title text)
  "The small page the browser is left on."
  (format nil "<!doctype html><html><head><meta charset=~s><title>~a</title></head><body style=~s><h1>~a</h1><p>~a</p></body></html>"
          "utf-8" title "font-family:sans-serif;margin:3em" title text))

(defstruct (flow (:constructor make-flow (verifier challenge state redirect-uri client-id token-endpoint resource)))
  "One sign-in waiting for its code."
  verifier challenge state redirect-uri client-id token-endpoint resource
  (url nil)
  (sockets '())
  (done nil)
  (mailbox (sb-concurrency:make-mailbox :name "nodecode-markifact sign-in"))
  (thread nil))

(defvar *flow* nil "The sign-in waiting for its code, or NIL.")

(defvar *flow-lock* (bt2:make-lock :name "nodecode-markifact sign-in"))

(defun answer-callback (flow stream)
  "Answer one request to the callback server on STREAM."
  (let* ((request (or (read-request-line stream) ""))
         (target (second (uiop:split-string request :separator " ")))
         (target (or target "/"))
         (path (subseq target 0 (or (position #\? target) (length target)))))
    (loop for line = (read-request-line stream) while (and line (plusp (length line))))
    (if (equal path +callback-path+)
        (let* ((params (query-params (url-query target)))
               (code (param params "code"))
               (denied (param params "error")))
          (cond ((and denied (plusp (length denied)))
                 (sb-concurrency:send-message (flow-mailbox flow)
                                              (list :error (or (param params "error_description") denied)))
                 (respond stream 500 "Internal Server Error"
                          :body (page "Sign-in failed" (or (param params "error_description") denied))))
                ((null code)
                 (respond stream 500 "Internal Server Error"
                          :body (page "Sign-in failed" "The answer carried no code.")))
                (t (sb-concurrency:send-message (flow-mailbox flow) (list :code code))
                   (respond stream 200 "OK"
                            :body (page "Signed in to Markifact"
                                         "Nodecode has the code; you can close this tab.")))))
        (respond stream 404 "Not Found" :body (page "Not here" "Nothing waits at this address.")))))

(defun serve-callbacks (flow)
  "Answer FLOW's callback server until the sign-in is done, then close it."
  (unwind-protect
       (loop until (flow-done flow)
             do (dolist (socket (ignore-errors
                                 (usocket:wait-for-input (flow-sockets flow) :timeout 0.5 :ready-only t)))
                  (let ((connection (ignore-errors (usocket:socket-accept socket))))
                    (when connection
                      (unwind-protect
                           (handler-case
                               (sb-sys:with-deadline (:seconds 10)
                                 (answer-callback flow (usocket:socket-stream connection)))
                             (serious-condition () nil))
                        (ignore-errors (usocket:socket-close connection)))))))
    (dolist (socket (flow-sockets flow))
      (ignore-errors (usocket:socket-close socket)))))

(defun listen-on (port)
  "The loopback listeners for PORT: IPv4, and IPv6 beside it when this host has it."
  (let* ((v4 (usocket:socket-listen "127.0.0.1" port :reuse-address t :backlog 8
                                                    :element-type '(unsigned-byte 8)))
         (v6 (ignore-errors
              (usocket:socket-listen "::1" (usocket:get-local-port v4) :reuse-address t :backlog 8
                                                                       :element-type '(unsigned-byte 8)))))
    (if v6 (list v4 v6) (list v4))))

(defun open-callback (flow)
  "Bind FLOW's callback server on any free loopback port; => its redirect address."
  (let ((sockets (listen-on 0)))
    (setf (flow-sockets flow) sockets)
    (format nil "http://~a:~d~a" +callback-host+ (usocket:get-local-port (first sockets))
            +callback-path+)))

;;; --- opening a browser -------------------------------------------------------------

(defun browser-command (url)
  "The command that shows URL in a browser here, or NIL."
  (let ((browser (string-trim " " (or (uiop:getenv "BROWSER") ""))))
    (cond ((plusp (length browser))
           (let ((words (nlk:split-words (first (uiop:split-string browser :separator ":")))))
             (and words
                  (if (member "%s" words :test #'string=)
                      (substitute url "%s" words :test #'string=)
                      (append words (list url))))))
          #+darwin ((list "open" url))
          #+win32 ((list "rundll32" "url.dll,FileProtocolHandler" url))
          #-(or darwin win32)
          ((some (lambda (variable)
                   (let ((value (uiop:getenv variable)))
                     (and value (plusp (length value)))))
                 '("WAYLAND_DISPLAY" "DISPLAY"))
           (list "xdg-open" url)))))

(defun open-in-browser (url)
  "Hand URL to a browser here, detached. => T when one started."
  (let ((command (and *open-browser-p* (browser-command url))))
    (and command
         (nlk:spawn-detached command (nlk:cache-path "nodecode/markifact-open-url.log"))
         t)))

;;; --- the sign-in in flight ---------------------------------------------------------

(defun finish-flow (flow)
  (setf (flow-done flow) t)
  (bt2:with-lock-held (*flow-lock*)
    (when (eq *flow* flow) (setf *flow* nil))))

(defun exchange-code (flow code)
  "CODE for tokens at FLOW's token endpoint, saved; => the access token."
  (let ((answer (token-request (flow-token-endpoint flow)
                                (list (cons "grant_type" "authorization_code")
                                      (cons "code" code)
                                      (cons "redirect_uri" (flow-redirect-uri flow))
                                      (cons "client_id" (flow-client-id flow))
                                      (cons "code_verifier" (flow-verifier flow))
                                      (cons "resource" (flow-resource flow))))))
    (or (keep-answer answer (flow-client-id flow) (flow-token-endpoint flow)
                     (flow-resource flow))
        (fail "Markifact answered no access token"))))

(defun run-flow (flow)
  "Wait for FLOW's code, exchange it, and say how it went."
  (unwind-protect
       (handler-case
           (let ((message (sb-concurrency:receive-message (flow-mailbox flow) :timeout +login-seconds+)))
             (case (first message)
               ((nil) (nle:notice (format nil "markifact: the sign-in timed out after ~d minutes with no answer; start again from /markifact"
                                          (floor +login-seconds+ 60))
                                  :level :warning))
               (:error (nle:notice (format nil "markifact: the sign-in failed: ~a" (second message))
                                   :level :warning))
               (:code (let ((params (query-params (url-query (second message)))))
                        (let ((code (or (param params "code") (second message)))
                              (state (param params "state")))
                          (when (and state (not (equal state (flow-state flow))))
                            (fail "the answer carries a different state than this sign-in sent"))
                          (exchange-code flow code)
                          (nle:notice "markifact: signed in; the tools answer now" :level :info))))))
         (serious-condition (condition)
           (nle:notice (format nil "markifact: the sign-in failed: ~a" condition) :level :warning)))
    (finish-flow flow)))

(defun start-login ()
  "Start a sign-in and answer what the operator does next."
  (let ((flow *flow*))
    (when flow (setf (flow-done flow) t)))
  (multiple-value-bind (verifier challenge) (pkce)
    (let* ((endpoint (endpoint))
           (server (authorization-server endpoint))
           (metadata (server-metadata server))
           (authorization-endpoint (metadata-url metadata "authorization_endpoint"
                                                 (format nil "~a/authorize" server)))
           (token-endpoint (metadata-url metadata "token_endpoint" (format nil "~a/token" server)))
           (registration-endpoint (metadata-url metadata "registration_endpoint"
                                                (format nil "~a/register" server)))
           (probe (make-flow verifier challenge (random-state-token) nil nil token-endpoint endpoint))
           (redirect-uri (open-callback probe))
           (client-id (register-client registration-endpoint redirect-uri)))
      (setf (flow-redirect-uri probe) redirect-uri
            (flow-client-id probe) client-id
            (flow-url probe) (format nil "~a?~a" authorization-endpoint
                                     (query-string (list (cons "response_type" "code")
                                                         (cons "client_id" client-id)
                                                         (cons "redirect_uri" redirect-uri)
                                                         (cons "code_challenge" challenge)
                                                         (cons "code_challenge_method" "S256")
                                                         (cons "state" (flow-state probe))
                                                         (cons "resource" endpoint)))))
      (bt2:with-lock-held (*flow-lock*) (setf *flow* probe))
      (bt2:make-thread (lambda () (serve-callbacks probe)) :name "nodecode-markifact callback")
      (setf (flow-thread probe)
            (bt2:make-thread (lambda () (run-flow probe)) :name "nodecode-markifact sign-in"))
      (open-in-browser (flow-url probe))
      (format nil "Open this address and sign in to Markifact:~%~a~%~%The browser comes back to ~a and the sign-in finishes on its own. On another machine, paste the address the browser ends on under /markifact code.~%The sign-in waits ~d minutes."
              (flow-url probe) redirect-uri (floor +login-seconds+ 60)))))

(defun paste (text)
  "Hand the sign-in in flight what the operator pasted; => what happened."
  (let ((value (nlk:trimmed (or text ""))))
    (if (zerop (length value))
        "Paste the address the browser ended on, or the code in it."
        (let ((flow *flow*))
          (if (null flow)
              "No sign-in is waiting for a code: start one with /markifact login."
              (progn (sb-concurrency:send-message (flow-mailbox flow) (list :code value))
                     "Received: finishing the sign-in. The outcome follows as a notice."))))))

;;; --- what it stands at -------------------------------------------------------------

(defun signed-in-p ()
  (let ((entry (kept-token)))
    (and entry (entry-string entry "access_token"))))

(defun status ()
  "Whether Markifact answers for this operator, in one line."
  (cond ((env-token) "Signed in: the markifact section's token_env names an access token.")
        ((let ((inline (nlk:trimmed (or (setting :access-token) ""))))
           (and (plusp (length inline)) inline))
         "Signed in: the markifact section carries an access token.")
        ((*flow*) (format nil "A sign-in is waiting at ~a, or paste what the browser landed on under /markifact code."
                           (flow-redirect-uri *flow*)))
        ((signed-in-p) "Signed in: a Markifact token is kept in auth.json.")
        (t "Not signed in: /markifact login signs in to Markifact.")))

(defun logout ()
  "Forget the kept sign-in."
  (let ((flow *flow*))
    (when flow (setf (flow-done flow) t)))
  (save-token)
  "Signed out: the kept Markifact token is gone from auth.json.")
