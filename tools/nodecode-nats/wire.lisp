;;;; wire.lisp --- enough of the NATS client protocol to publish, pull and probe.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; NATS is a line protocol over TCP: the server greets with INFO, the
;;;; client answers CONNECT and PING, publishes with PUB <subject> [<reply>]
;;;; <bytes> and subscribes with SUB <subject> <sid>; messages arrive as MSG
;;;; <subject> <sid> [<reply>] <bytes>. JetStream is the same protocol with
;;;; API subjects: a publish carries a reply inbox and the server answers the
;;;; ack on it, and a pull is a consumer created through
;;;; $JS.API.CONSUMER.CREATE.<stream> followed by one
;;;; $JS.API.CONSUMER.MSG.NEXT.<stream>.<consumer> request.
;;;;
;;;; So no NATS client library and no CLI is needed: usocket (already in the
;;;; image) and the bytes. Every function here takes a SETTINGS plist
;;;; (:SERVERS (:TIMEOUT :CLIENT-NAME)) rather than reading the cell's
;;;; section, so a test calls them against a fake server on loopback.

(in-package #:nodecode-nats)

(defparameter *default-server* "nats://127.0.0.1:4222"
  "The server a section that names none is read against.")

(defparameter *max-line-bytes* 65536
  "A protocol line longer than this is not a server we can talk to.")

(defstruct (connection (:constructor make-connection (socket stream)))
  socket stream)

(defun now ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun deadline-in (seconds)
  (+ (now) (max 1 seconds)))

(defun split-host-port (url)
  "URL as (values HOST PORT): nats://host:4222, host:4222 or a bare host."
  (let* ((url (nlk:trimmed (princ-to-string url)))
         (scheme (search "://" url))
         (rest (if scheme (subseq url (+ 3 scheme)) url))
         (slash (position #\/ rest))
         (rest (if slash (subseq rest 0 slash) rest))
         (colon (position #\: rest :from-end t)))
    (if (and colon (> colon 0))
        (values (subseq rest 0 colon)
                (or (ignore-errors (parse-integer rest :start (1+ colon))) 4222))
        (values rest 4222))))

(defun first-server (settings)
  "The first server in SETTINGS, or the loopback default."
  (let ((servers (getf settings :servers)))
    (let ((first (and servers (plusp (length servers)) (elt servers 0))))
      (if (and first (plusp (length (princ-to-string first))))
          (princ-to-string first)
          *default-server*))))

(defun open-connection (settings)
  "A connection to SETTINGS's first server, greeting already answered."
  (let* ((url (first-server settings))
         (timeout (or (getf settings :timeout) 10))
         (socket (handler-case
                     (multiple-value-bind (host port) (split-host-port url)
                       (usocket:socket-connect host port :element-type '(unsigned-byte 8)
                                                          :nodelay t))
                   (error (e)
                     (fail "cannot reach NATS at ~a: ~a" url
                           (nlk:one-line (princ-to-string e))))))
         (connection (make-connection socket (usocket:socket-stream socket))))
    (handler-case (progn (greet connection settings timeout) connection)
      (error (e)
        (ignore-errors (usocket:socket-close socket))
        (error e)))))

(defun close-connection (connection)
  (when connection
    (ignore-errors (finish-output (connection-stream connection)))
    (ignore-errors (usocket:socket-close (connection-socket connection)))))

(defmacro with-connection ((var settings) &body body)
  "BODY with VAR a live connection, closed on the way out however it leaves."
  `(let ((,var (open-connection ,settings)))
     (nlk:with-cleanup ((close-connection ,var)) ,@body)))

;;; --- bytes on the socket ------------------------------------------------------

(defun send-line (connection text)
  "TEXT and the CRLF a protocol line ends with."
  (let ((stream (connection-stream connection)))
    (write-sequence (babel:string-to-octets text :encoding :utf-8) stream)
    (write-sequence (make-array 2 :element-type '(unsigned-byte 8) :initial-contents '(13 10)) stream)
    (finish-output stream)))

(defun wait-readable (connection deadline what)
  "Return once CONNECTION has a byte to read, or refuse at DEADLINE."
  (loop
    (when (plusp (length (usocket:wait-for-input (connection-socket connection)
                                                 :timeout 0.05)))
      (return t))
    (when (>= (now) deadline)
      (fail "NATS did not answer ~a within ~,1fs" what (max 1 (- deadline 0))))))

(defun read-byte* (connection deadline what)
  (wait-readable connection deadline what)
  (let ((byte (read-byte (connection-stream connection) nil :eof)))
    (when (eq byte :eof) (fail "NATS closed the connection during ~a" what))
    byte))

(defun read-line* (connection deadline what)
  "One protocol line, CRLF stripped."
  (let ((bytes (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for byte = (read-byte* connection deadline what)
          until (= byte 10)
          do (vector-push-extend byte bytes)
             (when (> (length bytes) *max-line-bytes*)
               (fail "NATS sent a line longer than ~d bytes during ~a" *max-line-bytes* what)))
    (let ((end (length bytes)))
      (when (and (plusp end) (= (aref bytes (1- end)) 13)) (decf end))
      (babel:octets-to-string (subseq bytes 0 end) :encoding :utf-8))))

(defun read-payload (connection count deadline what)
  (let ((bytes (make-array count :element-type '(unsigned-byte 8))))
    (loop for i from 0 below count
          do (setf (aref bytes i) (read-byte* connection deadline what)))
    ;; the payload's own CRLF
    (read-byte* connection deadline what)
    (read-byte* connection deadline what)
    (babel:octets-to-string bytes :encoding :utf-8)))

(defun greet (connection settings timeout)
  "Read INFO, answer CONNECT and PING, and wait for PONG."
  (let ((deadline (deadline-in timeout))
        (info (read-line* connection (deadline-in timeout) "INFO")))
    (unless (and (>= (length info) 4) (string= "INFO" (subseq info 0 4)))
      (fail "~a does not speak NATS (its first line was ~s)" (first-server settings) info))
    (send-line connection
                (format nil "CONNECT {\"verbose\":false,\"pedantic\":false,\"name\":\"~a\",\"lang\":\"lisp\",\"version\":\"1.0\",\"protocol\":1}"
                        (or (getf settings :client-name) "nodecode")))
    (send-line connection "PING")
    (loop for line = (read-line* connection deadline "PONG")
          do (cond ((string= line "PONG") (return t))
                   ((and (plusp (length line)) (char= (char line 0) #\-))
                    (fail "NATS refused the connection: ~a" line))
                   (t nil)))))

;;; --- messages -------------------------------------------------------------------

(defun subscribe (connection subject sid)
  (send-line connection (format nil "SUB ~a ~a" subject sid)))

(defun publish-raw (connection subject payload &optional reply)
  "PUB, then the payload, then its CRLF: the one write a publish is."
  (let ((bytes (babel:string-to-octets (or payload "") :encoding :utf-8)))
    (send-line connection (format nil "PUB ~a~@[ ~a~] ~d" subject reply (length bytes)))
    (let ((stream (connection-stream connection)))
      (write-sequence bytes stream)
      (write-sequence (make-array 2 :element-type '(unsigned-byte 8) :initial-contents '(13 10)) stream)
      (finish-output stream))))

(defun read-message (connection deadline what)
  "The next MSG as (values SUBJECT REPLY PAYLOAD), skipping chatter and
answering the server's PING. NIL for a control line that is no message."
  (loop for line = (read-line* connection deadline what)
        do (cond ((and (>= (length line) 4) (string= "PING" line))
                  (send-line connection "PONG"))
                 ((and (>= (length line) 3) (string= "MSG" line))
                  (let* ((parts (uiop:split-string line :separator " "))
                         (subject (second parts))
                         (reply (and (>= (length parts) 5) (fourth parts)))
                         (count (parse-integer (car (last parts)))))
                    (return (values subject reply (read-payload connection count deadline what)))))
                 ((and (>= (length line) 4) (string= "HMSG" line))
                  ;; a status message: NATS/1.0 408 and the like, payload discarded
                  (read-payload connection (parse-integer (car (last (uiop:split-string line :separator " "))))
                                 deadline what)
                  nil)
                 ((and (plusp (length line)) (char= (char line 0) #\-))
                  (fail "NATS says ~a" (subseq line 1)))
                 (t nil))))

;;; --- JetStream --------------------------------------------------------------------

(defun request (connection subject body &key (timeout 10))
  "One request/reply: SUB an inbox, PUB SUBJECT with it as the reply, and the
answer's payload."
  (let ((inbox (format nil "_INBOX.nodecode.~36r" (random (expt 36 10)))))
    (subscribe connection inbox 1)
    (publish-raw connection subject body inbox)
    (multiple-value-bind (subject reply payload)
        (read-message connection (deadline-in timeout) subject)
      (declare (ignore subject reply))
      payload)))

(defun json-text (value)
  (with-output-to-string (out) (shasht:write-json value out)))

(defun json-object (text)
  "TEXT decoded, or NIL when it is not JSON."
  (ignore-errors (nlk:decode-json text)))

(defun api-error (body)
  "BODY's JetStream error description, or NIL."
  (let* ((object (json-object body))
         (error (and object (nlk:json-value object :object "error"))))
    (and error (nlk:json-value error :string "description" "err_code"))))

(defun wire-publish (settings subject payload)
  "Publish PAYLOAD to SUBJECT as a JetStream message and return the ack as
(:STREAM STRING :SEQ INTEGER)."
  (with-connection (connection settings)
    (let ((answer (request connection subject payload
                           :timeout (or (getf settings :timeout) 10))))
      (when (null answer)
        (fail "no JetStream ack on ~a: is streaming on and a stream bound to it?" subject))
      (when (api-error answer)
        (fail "JetStream refused ~a: ~a" subject (api-error answer)))
      (let ((object (json-object answer)))
        (unless object (fail "unreadable JetStream ack on ~a: ~a" subject answer))
        (list :stream (or (nlk:json-value object :string "stream") "?")
              :seq (nlk:json-value object :integer "seq" "?"))))))

(defun wire-fetch (settings subject &key (batch 1) (timeout 5))
  "Pull up to BATCH messages bound to SUBJECT from the stream, acking each.
=> a list of plists (:subject, :payload, :stream-seq)."
  (with-connection (connection (list* :timeout (max timeout (getf settings :timeout 5)) settings))
    (let* ((stream (or (getf settings :stream) "hermes"))
           (created (request connection (format nil "$JS.API.CONSUMER.CREATE.~a" stream)
                              (json-text (list :deliver-policy "all"
                                               :ack-policy "explicit"
                                               :filter-subject subject
                                               :inactive-threshold 30000000000))
                              :timeout timeout))
           (object (json-object created))
           (name (and object (nlk:json-value object :string "name")))
           (error (api-error created)))
      (when error (fail "JetStream refused a consumer on ~a: ~a" subject error))
      (unless name (fail "JetStream created no consumer on ~a: ~a" subject created))
      (let ((inbox (format nil "_INBOX.nodecode.~36r" (random (expt 36 10))))
            (deadline (deadline-in timeout))
            (got '()))
        (subscribe connection inbox 2)
        (publish-raw connection (format nil "$JS.API.CONSUMER.MSG.NEXT.~a.~a" stream name)
                      (json-text (list :batch batch
                                       :expires (* 1000000000 (max 1 timeout))))
                      inbox)
        (loop while (< (length got) batch)
              do (handler-case
                     (multiple-value-bind (message-subject reply payload)
                         (read-message connection deadline subject)
                       (declare (ignore message-subject))
                       (when reply
                         (publish-raw connection reply ""))
                       (when (and payload (plusp (length payload)))
                         (push (list :subject subject :payload payload) got)))
                   (nats-error ()
                     (return))))
        (nreverse got)))))

(defun wire-probe (settings)
  "Connect, read the server's INFO and answer it as text, then leave."
  (with-connection (connection settings)
    (let ((info (read-line* connection (deadline-in (or (getf settings :timeout) 10)) "INFO")))
      (declare (ignore info))
      (format nil "reached the NATS server at ~a" (first-server settings)))))
