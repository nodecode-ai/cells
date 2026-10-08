;;;; relay.lisp --- the loopback address the CLI sends its request to.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The CLI dials ANTHROPIC_BASE_URL, and for one round that is a listener on
;;;; 127.0.0.1 under a path no other process can guess. It takes the one
;;;; Messages POST the CLI makes — request line, headers, body — and answers
;;;; it with an error, so nothing the CLI writes ever leaves this machine
;;;; through the CLI; the round's owner sends those bytes itself. The CLI's
;;;; reachability probe (HEAD /api/hello) is answered 200, anything else 404.
;;;;
;;;; One connection at a time, on a thread of the relay's own: the CLI may
;;;; probe the relay before it reads its stdin, and a history longer than a
;;;; pipe holds would otherwise wait on a relay nobody was serving.

(in-package #:nodecode-claude-code)

(defparameter +head-limit+ (* 64 1024)
  "The most bytes a request line and its headers may take.")

(defparameter +body-limit+ (* 256 1024 1024)
  "The most bytes a captured request body may take: a whole history of
images is large, but never this.")

(defparameter +read-seconds+ 30
  "How long one connection may take to deliver its request.")

(defparameter +upstream+ "https://api.anthropic.com"
  "Where the CLI would have sent its request: the path it asked for is
composed onto this.")

(defparameter +dropped-headers+
  '("host" "connection" "keep-alive" "content-length" "transfer-encoding"
    "accept-encoding" "content-type" "proxy-authorization" "proxy-connection")
  "Headers a captured request does not carry on: the hop's own, and the
content type the transport sets itself.")

(defstruct (relay (:copier nil))
  (listener nil)
  (port 0 :type integer)
  (token "" :type string)
  ;; the Messages POST, once it arrived: (:path P :headers ALIST :body OCTETS)
  (capture nil)
  ;; why an exchange could not be read, once one could not
  (failure nil)
  (thread nil)
  (stopping nil))

(defun hex-token (count)
  "COUNT bytes of entropy as lowercase hex."
  (format nil "~(~{~2,'0x~}~)" (coerce (nlk:random-bytes count) 'list)))

(defun open-relay ()
  "A RELAY listening on a free loopback port and serving it."
  (let* ((listener (usocket:socket-listen "127.0.0.1" 0 :reuse-address t :backlog 8
                                                         :element-type '(unsigned-byte 8)))
         (relay (make-relay :listener listener :port (usocket:get-local-port listener)
                            :token (hex-token 24))))
    (setf (relay-thread relay)
          (bt2:make-thread (lambda ()
                             (loop until (or (relay-stopping relay) (relay-capture relay)
                                             (relay-failure relay))
                                   do (handler-case (relay-poll relay 0.05)
                                        ((or error sb-sys:deadline-timeout) (condition)
                                          (setf (relay-failure relay) (princ-to-string condition))))))
                           :name "claude-code relay"))
    relay))

(defun relay-prefix (relay)
  "The path every request to RELAY starts with."
  (format nil "/admit/~a" (relay-token relay)))

(defun relay-url (relay)
  "RELAY's address, as ANTHROPIC_BASE_URL."
  (format nil "http://127.0.0.1:~d~a" (relay-port relay) (relay-prefix relay)))

(defun close-relay (relay)
  "Stop serving and listening."
  (setf (relay-stopping relay) t)
  (ignore-errors (bt2:join-thread (relay-thread relay)))
  (ignore-errors (usocket:socket-close (relay-listener relay))))

(defun read-head (stream)
  "The request line and headers STREAM delivers, as text, or NIL at EOF."
  (let ((buffer (make-array 1024 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for byte = (read-byte stream nil nil)
          do (unless byte (return-from read-head nil))
             (vector-push-extend byte buffer)
             (when (> (fill-pointer buffer) +head-limit+)
               (error "request head over ~d bytes" +head-limit+))
          until (let ((end (fill-pointer buffer)))
                  (and (>= end 4)
                       (= 13 (aref buffer (- end 4))) (= 10 (aref buffer (- end 3)))
                       (= 13 (aref buffer (- end 2))) (= 10 (aref buffer (- end 1))))))
    (sb-ext:octets-to-string buffer :external-format :latin-1)))

(defun parse-head (text)
  "(values METHOD TARGET HEADERS) of the request head TEXT, HEADERS an alist
in the order they arrived."
  (let* ((lines (remove "" (ppcre:split "\\r\\n" text) :test #'string=))
         (request (uiop:split-string (first lines) :separator " ")))
    (values (first request) (second request)
            (loop for line in (rest lines)
                  for colon = (position #\: line)
                  when colon
                    collect (cons (subseq line 0 colon)
                                  (string-trim " \t" (subseq line (1+ colon))))))))

(defun header (headers name)
  "The value of header NAME in HEADERS, any case."
  (cdr (assoc name headers :test #'string-equal)))

(defun respond (stream status reason &optional (body ""))
  "Answer STATUS with the JSON BODY and close the exchange."
  (let ((octets (sb-ext:string-to-octets body :external-format :utf-8)))
    (write-sequence (sb-ext:string-to-octets
                     (format nil "HTTP/1.1 ~d ~a~c~cContent-Type: application/json~c~cContent-Length: ~d~c~cConnection: close~c~c~c~c"
                             status reason #\Return #\Linefeed #\Return #\Linefeed (length octets)
                             #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed)
                     :external-format :latin-1)
                    stream)
    (write-sequence octets stream)
    (finish-output stream)))

(defun handle-exchange (relay stream)
  "Read one request off STREAM: keep the Messages POST, answer the rest."
  (multiple-value-bind (method target headers) (parse-head (or (read-head stream) (return-from handle-exchange)))
    (let ((messages (concatenate 'string (relay-prefix relay) "/v1/messages")))
      (cond
        ((and (equal method "POST")
              (uiop:string-prefix-p messages target)
              (member (length messages) (list (length target) (position #\? target))))
         (let ((length (ignore-errors (parse-integer (or (header headers "content-length") "")))))
           (unless (and length (<= 0 length +body-limit+))
             (respond stream 411 "Length Required")
             (error "the CLI's request carried no usable Content-Length"))
           (let ((body (make-array length :element-type '(unsigned-byte 8))))
             (unless (= length (read-sequence body stream))
               (error "the CLI's request ended before its ~d bytes" length))
             (setf (relay-capture relay)
                   (list :path (subseq target (length (relay-prefix relay)))
                         :headers headers
                         :body body))
             (respond stream 400 "Bad Request"
                      "{\"type\":\"error\",\"error\":{\"type\":\"invalid_request_error\",\"message\":\"nodecode sends this request itself\"}}"))))
        ((member method '("HEAD" "GET") :test #'equal) (respond stream 200 "OK"))
        (t (respond stream 404 "Not Found"))))))

(defun relay-poll (relay seconds)
  "Take one exchange waiting on RELAY within SECONDS: true when one was
handled."
  (let ((listener (relay-listener relay)))
    (when (usocket:wait-for-input listener :timeout seconds :ready-only t)
      (let ((connection (usocket:socket-accept listener :element-type '(unsigned-byte 8))))
        (unwind-protect
             (sb-sys:with-deadline (:seconds +read-seconds+)
               (handle-exchange relay (usocket:socket-stream connection)))
          (ignore-errors (usocket:socket-close connection))))
      t)))

(defun captured-headers (capture)
  "The headers a captured request is sent with: the CLI's own, in order, but
the hop's."
  (remove-if (lambda (pair) (member (car pair) +dropped-headers+ :test #'string-equal))
             (getf capture :headers)))

(defun captured-endpoint (capture)
  "Where a captured request goes: the path the CLI asked for, on the API it
would have dialled."
  (concatenate 'string +upstream+ (getf capture :path)))
