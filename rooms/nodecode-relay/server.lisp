;;;; server.lisp --- the second listener, its gate, and the page behind it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One socket on the address the operator named, one connection at a time:
;;;; the relay sits in front of a page that stays on loopback, so it reads the
;;;; request itself, decides who is asking, and either answers one of its own
;;;; paths or replays the request on loopback and copies the answer back. A
;;;; proxied request carries none of the browser's cookies: what reaches the
;;;; page is this machine's own request. Every stream here is a byte stream,
;;;; so a page's pictures and scripts survive the trip.

(in-package #:nodecode-relay)

(defstruct (listener (:constructor make-listener))
  socket
  port
  (thread nil)
  (open t)
  handler)

(defstruct (request (:constructor make-request))
  method path headers body)

(defun request-header (request name)
  (cdr (assoc name (request-headers request) :test #'string-equal)))

(defun body-text (body)
  (if (and body (plusp (length body)))
      (sb-ext:octets-to-string body :external-format :utf-8)
      ""))

(defun server-octets (text)
  (if (stringp text)
      (sb-ext:string-to-octets text :external-format :utf-8)
      (or text (make-array 0 :element-type '(unsigned-byte 8)))))

(defun read-line-octets (stream)
  "One CRLF line off a byte STREAM, as a string, or NIL at its end."
  (let ((line (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for byte = (read-byte stream nil nil)
          while byte
          do (if (= byte 10)
                 (return (sb-ext:octets-to-string
                          (coerce (if (and (plusp (length line))
                                           (= (aref line (1- (length line))) 13))
                                      (subseq line 0 (1- (length line)))
                                      line)
                                  '(vector (unsigned-byte 8)))
                          :external-format :utf-8))
                 (vector-push-extend byte line))
          finally (return (when (plusp (length line))
                            (sb-ext:octets-to-string
                             (coerce line '(vector (unsigned-byte 8)))
                             :external-format :utf-8))))))

(defun form-fields (request)
  "The urlencoded body's fields as an alist, or NIL."
  (let ((text (body-text (request-body request))))
    (loop for pair in (uiop:split-string text :separator "&")
          for eq = (position #\= pair)
          when eq
            collect (cons (string-downcase (subseq pair 0 eq))
                          (quri:url-decode (subseq pair (1+ eq)) :lenient t)))))

;;; --- reading and writing one exchange ---------------------------------------

(defun read-request (stream)
  "The request on STREAM => a REQUEST, or NIL when the peer said nothing."
  (let ((line (read-line-octets stream)))
    (when (and line (plusp (length line)))
      (let ((parts (uiop:split-string line :separator " ")))
        (when (>= (length parts) 2)
          (let ((headers (loop for header = (read-line-octets stream)
                               while (and header (plusp (length header)))
                               for colon = (position #\: header)
                               when colon
                                 collect (cons (string-downcase (subseq header 0 colon))
                                               (string-trim " " (subseq header (1+ colon))))))
                (body nil))
            (let ((length (parse-integer
                           (or (cdr (assoc "content-length" headers :test #'string-equal)) "0")
                           :junk-allowed t)))
              (when (and length (plusp length))
                (setf body (make-array length :element-type '(unsigned-byte 8)))
                (read-sequence body stream)))
            (make-request :method (string-upcase (first parts))
                          :path (second parts)
                          :headers headers
                          :body body)))))))

(defun status-text (status)
  (case status
    (200 "OK") (302 "Found") (303 "See Other") (400 "Bad Request")
    (401 "Unauthorized") (403 "Forbidden") (404 "Not Found")
    (405 "Method Not Allowed") (409 "Conflict") (429 "Too Many Requests")
    (502 "Bad Gateway") (t "OK")))

(defun write-response (stream status headers body)
  "STATUS and HEADERS, then BODY's bytes, on the byte STREAM."
  (let* ((payload (server-octets body))
         (head (with-output-to-string (out)
                (format out "HTTP/1.1 ~d ~a~c~c" status (status-text status) #\Return #\Linefeed)
                (dolist (header headers)
                  (format out "~a: ~a~c~c" (car header) (cdr header) #\Return #\Linefeed))
                (format out "content-length: ~d~c~cconnection: close~c~c~c~c"
                        (length payload) #\Return #\Linefeed #\Return #\Linefeed
                        #\Return #\Linefeed))))
    (write-sequence (server-octets head) stream)
    (write-sequence payload stream)
    (finish-output stream)))

;;; --- the answers the relay itself gives --------------------------------------

(defparameter +form+
  "<!doctype html><meta name=viewport content='width=device-width,initial-scale=1'>
<title>Relay</title><body style='font:16px system-ui;max-width:26rem;margin:3rem auto'>
<h1>Relay</h1>~a<form method=post action='~a'>~a<p><button>Send</button></form></body>")

(defun html (status text)
  (values status (list (cons "content-type" "text/html; charset=utf-8")) text))

(defun page (title action fields &optional note)
  (values 200 (list (cons "content-type" "text/html; charset=utf-8"))
          (format nil +form+ (or note "") action
                  (with-output-to-string (out)
                    (format out "<h2>~a</h2>" title)
                    (dolist (field fields)
                      (format out "<p><label>~a<br><input name='~a'~@[ type='~a'~]></label>"
                              (car field) (cadr field) (caddr field)))))))

(defun redirect (to)
  (values 303 (list (cons "location" to)) ""))

(defun text-answer (status text)
  (values status (list (cons "content-type" "text/plain; charset=utf-8")) text))

(defun json-answer (object)
  (values 200 (list (cons "content-type" "application/json")) (nlk:encode-json-object object)))

(defun proxy (request host port)
  "REQUEST replayed on the page at HOST:PORT => (VALUES STATUS HEADERS BODY)."
  (let ((socket (usocket:socket-connect host port :element-type '(unsigned-byte 8) :nodelay t)))
    (unwind-protect
         (let ((stream (usocket:socket-stream socket)))
           (let ((head (with-output-to-string (out)
                         (format out "~a ~a HTTP/1.1~c~c" (request-method request)
                                 (request-path request) #\Return #\Linefeed)
                         (dolist (header (request-headers request))
                           (unless (member (car header)
                                           '("cookie" "authorization" "host" "connection"
                                             "content-length" "x-relay-peer")
                                           :test #'string-equal)
                             (format out "~a: ~a~c~c" (car header) (cdr header)
                                     #\Return #\Linefeed)))
                         (format out "host: ~a:~d~c~cconnection: close~c~c~c~c"
                                 host port #\Return #\Linefeed #\Return #\Linefeed
                                 #\Return #\Linefeed)))
                 (body (request-body request)))
             (write-sequence (server-octets head) stream)
             (when (and body (plusp (length body))) (write-sequence body stream))
             (finish-output stream))
           (let* ((line (read-line-octets stream))
                  (parts (and line (uiop:split-string line :separator " ")))
                  (status (if parts (parse-integer (second parts) :junk-allowed t) 502))
                  (headers (loop for header = (read-line-octets stream)
                                 while (and header (plusp (length header)))
                                 for colon = (position #\: header)
                                 when colon
                                   collect (cons (string-downcase (subseq header 0 colon))
                                                 (string-trim " " (subseq header (1+ colon))))))
                  (length (cdr (assoc "content-length" headers :test #'string-equal)))
                  (payload (make-array 0 :element-type '(unsigned-byte 8)
                                          :adjustable t :fill-pointer 0)))
             (if length
                 (let ((want (parse-integer length :junk-allowed t)))
                   (when (and want (plusp want))
                     (let ((chunk (make-array want :element-type '(unsigned-byte 8))))
                       (read-sequence chunk stream)
                       (setf payload chunk))))
                 (loop for byte = (read-byte stream nil nil)
                       while byte do (vector-push-extend byte payload)))
             (values (or status 502)
                     (remove-if (lambda (header)
                                  (member (car header) '("connection" "transfer-encoding")
                                          :test #'string-equal))
                                headers)
                     (coerce payload '(vector (unsigned-byte 8))))))
      (ignore-errors (usocket:socket-close socket)))))

;;; --- the routes -------------------------------------------------------------

(defun relay-response (store request &key harness-host harness-port trusted-p
                                    (now (get-universal-time)))
  "The answer to REQUEST: one of the relay's own paths, or the page behind it."
  (let ((path (request-path request))
        (method (request-method request))
        (address (or (request-header request "x-relay-peer") "network")))
    (cond
      ((equal path "/relay/health")
       (json-answer (nlk:json-object "ok" t
                                      "devices" (length (store-devices store))
                                      "password" (and (password-set-p store) t))))
      ((equal path "/relay/login")
       (cond
         ((equal method "POST")
          (let ((secret (cdr (assoc "password" (form-fields request) :test #'equal))))
            (cond
              ((and (not trusted-p) (locked-p store address :now now))
               (html 429 "<h2>Too many guesses</h2><p>Try again in a few minutes.</p>"))
              ((password-ok-p store secret)
               (note-success store address)
               (let ((device (or (caller-device store (request-headers request))
                                 (mint-device store "a browser" :now now))))
                 (values 303 (list (cons "location" "/")
                                   (cons "set-cookie"
                                         (format nil "~a=~a; Path=/; HttpOnly; SameSite=Lax"
                                                 +session-cookie+ (session-cookie store device))))
                         "")))
              ((not (password-set-p store))
               (if trusted-p
                   (progn (set-password store (or secret "")) (redirect "/relay/login"))
                   (html 403 "<h2>No password yet</h2><p>Set one on the machine itself.</p>")))
              (t (note-failure store address :now now)
                 (html 401 "<h2>Wrong password</h2>")))))
         ((not (password-set-p store))
          (page "Choose a password" "/relay/login" '(("password" "password" "password"))
                (if trusted-p "" "<p>Until one exists, only the machine can set it.</p>")))
         (t (page "Sign in" "/relay/login" '(("password" "password" "password"))))))
      ((equal path "/relay/pair")
       (if (equal method "POST")
           (let* ((fields (form-fields request))
                  (code (cdr (assoc "code" fields :test #'equal)))
                  (name (cdr (assoc "name" fields :test #'equal))))
             (multiple-value-bind (device token) (redeem-code store code name :now now)
               (if device
                   (values 200 (list (cons "content-type" "application/json"))
                           (nlk:encode-json-object
                            (nlk:json-object "id" (plist device :id)
                                             "device" (plist device :name)
                                             "token" token
                                             "cookie" (session-cookie store device))))
                   (text-answer 403 "that code is spent or has expired"))))
           (if trusted-p
               (page "Pair a device" "/relay/pair" '(("code" "code") ("name" "name"))
                     "<p>Ask the shell for a code: /relay pair name.</p>")
               (text-answer 403 "codes are made on the machine"))))
      ((equal path "/relay/devices")
       (if (caller-device store (request-headers request))
           (json-answer
            (coerce (mapcar (lambda (device)
                              (nlk:json-object "id" (plist device :id)
                                               "name" (plist device :name)
                                               "added" (plist device :added 0)))
                            (store-devices store))
                    'vector))
           (text-answer 401 "not signed in")))
      ((equal path "/relay/revoke")
       (cond
         ((not (caller-device store (request-headers request))) (text-answer 401 "not signed in"))
         ((equal method "POST")
          (let ((name (revoke-device store (or (cdr (assoc "id" (form-fields request)
                                                           :test #'equal)) ""))))
            (if name (text-answer 200 (format nil "~a is out" name))
                (text-answer 404 "no such device"))))
         (t (text-answer 405 "POST the device's id"))))
      ((equal path "/relay/signout")
       (if (equal method "POST")
           (progn (sign-out-everywhere store)
                  (values 303 (list (cons "location" "/relay/login")) ""))
           (text-answer 405 "POST to sign out everywhere")))
      ((or trusted-p (caller-device store (request-headers request)))
       (proxy request harness-host harness-port))
      (t (values 302 (list (cons "location" "/relay/login")) "")))))

;;; --- accepting --------------------------------------------------------------

(defun serve-connection (listener connection)
  (unwind-protect
       (let ((stream (usocket:socket-stream connection)))
         (let ((request (handler-case (read-request stream) (error () nil))))
           (when request
             (let ((peer (usocket:get-peer-address connection)))
               (when peer
                 (setf (request-headers request)
                       (acons "x-relay-peer" peer (request-headers request))))
               (handler-case
                   (multiple-value-bind (status headers body)
                       (funcall (listener-handler listener) request)
                     (write-response stream status headers body))
                 (usocket:connection-refused-error ()
                   (write-response stream 502 nil "the page behind the relay is not answering"))
                 (error (condition)
                   (write-response stream 502 nil (format nil "relay: ~a" condition))))))))
    (ignore-errors (usocket:socket-close connection))))

(defun accept-loop (listener)
  (loop while (listener-open listener)
        do (let ((connection (handler-case
                                 (usocket:socket-accept (listener-socket listener)
                                                        :element-type '(unsigned-byte 8))
                               (error () nil))))
             (when connection (serve-connection listener connection)))))

(defun start-listener (host port handler)
  "HANDLER, a function of one REQUEST, on HOST:PORT (PORT 0: any free one)."
  (let* ((socket (usocket:socket-listen host port :reuse-address t
                                                   :element-type '(unsigned-byte 8)))
         (listener (make-listener :socket socket
                                  :port (usocket:get-local-port socket)
                                  :handler handler)))
    (setf (listener-thread listener)
          (bt2:make-thread (lambda () (accept-loop listener))
                           :name "nodecode-relay listener"))
    listener))

(defun stop-listener (listener)
  (when (and listener (listener-open listener))
    (setf (listener-open listener) nil)
    (ignore-errors (usocket:socket-close (listener-socket listener)))
    (let ((thread (listener-thread listener)))
      (when (and thread (bt2:thread-alive-p thread))
        (ignore-errors (bt2:join-thread thread :timeout 2))))
    t))
