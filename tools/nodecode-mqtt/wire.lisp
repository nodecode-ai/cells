;;;; wire.lisp --- enough of the MQTT client protocol to publish, read and probe.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; MQTT is a binary protocol over TCP: every packet is one control byte
;;;; (type in the high nibble, flags in the low), a remaining length as a
;;;; variable integer, then the body. A client opens with CONNECT (protocol
;;;; name, level 4, clean session, client id, optional user and password) and
;;;; reads CONNACK (a return code: zero accepted); it subscribes with
;;;; SUBSCRIBE and gets SUBACK, publishes with PUBLISH at QoS 1 and gets
;;;; PUBACK, and ends with DISCONNECT. Messages arrive as PUBLISH packets
;;;; with the retain bit and a QoS-1 packet id that must be acked.
;;;;
;;;; So no MQTT client library and no CLI is needed: usocket (already in the
;;;; image) and the bytes, with TLS through cl+ssl for an mqtts:// url. Every
;;;; function here takes a SETTINGS plist (:URL :USERNAME :PASSWORD
;;;; :CLIENT-ID :TIMEOUT) rather than reading the cell's section, so a test
;;;; calls them against a fake broker on loopback.

(in-package #:nodecode-mqtt)

(defparameter *default-broker* "tcp://127.0.0.1:1883"
  "The broker a section that names none is read against.")

(defparameter *max-packet-bytes* 16777216
  "A remaining length past this is not a packet a house bus needs: refuse it
before it becomes an allocation.")

(defstruct (connection (:constructor make-connection (socket stream)))
  socket stream)

(defun now ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun deadline-in (seconds)
  (+ (now) (max 1 seconds)))

(defun split-url (url)
  "URL as (values HOST PORT TLS-P): mqtts://host:8883, tcp://host:1883,
host:1883 or a bare host. The scheme is the only TLS switch."
  (let* ((url (nlk:trimmed (princ-to-string url)))
         (mark (search "://" url))
         (scheme (and mark (string-downcase (subseq url 0 mark))))
         (rest (if mark (subseq url (+ 3 mark)) url))
         (slash (position #\/ rest))
         (rest (if slash (subseq rest 0 slash) rest))
         (tls (and scheme (member scheme '("mqtts" "ssl" "tls") :test #'string=)))
         (colon (position #\: rest :from-end t)))
    (when (and scheme (not tls) (not (member scheme '("tcp" "mqtt") :test #'string=)))
      (fail "~a is not an MQTT url: use tcp://host:1883 or mqtts://host:8883" url))
    (let* ((host (if (and colon (plusp colon)) (subseq rest 0 colon) rest))
           (port (if (and colon (plusp colon))
                     (or (ignore-errors (parse-integer rest :start (1+ colon)))
                         (if tls 8883 1883))
                     (if tls 8883 1883))))
      (when (zerop (length host))
        (fail "an MQTT url needs a host: ~s" url))
      (values host port (and tls t)))))

(defun broker-url (settings)
  "The url SETTINGS names, or the loopback default."
  (let ((url (nlk:trimmed (princ-to-string (or (getf settings :url) "")))))
    (if (plusp (length url)) url *default-broker*)))

(defun open-connection (settings)
  "A connection to SETTINGS's broker, CONNACK already read."
  (let* ((url (broker-url settings))
         (timeout (or (getf settings :timeout) 10)))
    (multiple-value-bind (host port tls) (split-url url)
      (let* ((socket (handler-case
                          (usocket:socket-connect host port
                                                   :element-type '(unsigned-byte 8)
                                                   :nodelay t :timeout timeout)
                        (error (e)
                          (fail "cannot reach the MQTT broker at ~a: ~a" url
                                (nlk:one-line (princ-to-string e))))))
             (stream (usocket:socket-stream socket)))
        (when tls
          (setf stream (handler-case
                            (funcall (or (find-symbol "MAKE-SSL-CLIENT-STREAM" :cl+ssl)
                                         (fail "~a wants TLS and cl+ssl is not in this image" url))
                                     stream :hostname host :verify :required)
                          (error (e)
                            (ignore-errors (usocket:socket-close socket))
                            (fail "TLS to ~a failed: ~a" url
                                  (nlk:one-line (princ-to-string e)))))))
        (let ((connection (make-connection socket stream)))
          (handler-case (progn (greet connection settings timeout) connection)
            (error (e)
              (close-connection connection)
              (error e))))))))

(defun close-connection (connection)
  (when connection
    (ignore-errors (finish-output (connection-stream connection)))
    (ignore-errors (usocket:socket-close (connection-socket connection)))))

(defmacro with-connection ((var settings) &body body)
  "BODY with VAR a live connection, closed on the way out however it leaves."
  `(let ((,var (open-connection ,settings)))
     (nlk:with-cleanup ((close-connection ,var)) ,@body)))

;;; --- bytes on the socket ------------------------------------------------------

(defun octets (&rest parts)
  "PARTS, each a vector of bytes or a list of them, as one vector."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (dolist (part parts)
      (dolist (byte (if (vectorp part) part part))
        (vector-push-extend byte out)))
    (coerce out '(vector (unsigned-byte 8)))))

(defun send-bytes (connection bytes)
  "BYTES to CONNECTION, flushed: the one write every packet is."
  (let ((stream (connection-stream connection)))
    (write-sequence bytes stream)
    (finish-output stream)))

(defun wait-readable (connection seconds what)
  "Return once CONNECTION has a byte to read, or refuse after SECONDS."
  (let ((deadline (deadline-in seconds)))
    (loop
      (when (plusp (length (usocket:wait-for-input (connection-socket connection)
                                                   :timeout (min 0.05 (max 0.01 (- deadline (now)))))))
        (return t))
      (when (>= (now) deadline)
        (fail "the MQTT broker did not answer ~a within ~,1fs" what (max 1 seconds))))))

(defun read-exact (connection count deadline what)
  "COUNT bytes from CONNECTION, or a refusal at DEADLINE."
  (let ((bytes (make-array count :element-type '(unsigned-byte 8))))
    (loop for i from 0 below count
          do (wait-readable connection (max 0.01 (- deadline (now))) what)
             (let ((byte (read-byte (connection-stream connection) nil :eof)))
               (when (eq byte :eof)
                 (fail "the MQTT broker closed the connection during ~a" what))
               (setf (aref bytes i) byte)))
    bytes))

(defun read-varint (connection deadline what)
  "The remaining length: a variable integer of up to four bytes."
  (let ((value 0) (shift 0))
    (loop
      (let ((byte (aref (read-exact connection 1 deadline what) 0)))
        (setf value (logior value (ash (logand byte 127) shift)))
        (when (zerop (logand byte 128)) (return value))
        (incf shift 7)
        (when (> shift 21)
          (fail "a malformed MQTT packet (its length never ended) during ~a" what))
        (when (> value *max-packet-bytes*)
          (fail "an MQTT packet of ~d bytes during ~a is past the ~d-byte cap"
                value what *max-packet-bytes*))))))

(defun varint (value)
  "VALUE as the remaining length's variable integer."
  (let ((bytes '()))
    (loop (let ((byte (logand value 127)))
            (setf value (ash value -7))
            (push (if (plusp value) (logior byte 128) byte) bytes))
          (when (zerop value) (return (coerce (nreverse bytes) '(vector (unsigned-byte 8))))))))

(defun read-packet (connection deadline what)
  "The next packet as (values TYPE FLAGS BODY) — BODY a vector of bytes."
  (let* ((first (aref (read-exact connection 1 deadline what) 0))
         (length (read-varint connection deadline what)))
    (values (ash first -4) (logand first 15)
            (read-exact connection length deadline what))))

(defun packet (first &rest parts)
  "A whole packet: its control byte, the remaining length of PARTS, PARTS."
  (let ((body (apply #'octets parts)))
    (octets (vector first) (varint (length body)) body)))

(defun utf8 (text)
  "TEXT as an MQTT UTF-8 string: its length in two bytes, then its bytes."
  (let ((bytes (babel:string-to-octets (princ-to-string (or text "")) :encoding :utf-8)))
    (octets (vector (ldb (byte 8 8) (length bytes)) (ldb (byte 8 0) (length bytes))) bytes)))

(defun u16 (value)
  (vector (ldb (byte 8 8) value) (ldb (byte 8 0) value)))

;;; --- the packets --------------------------------------------------------------

(defun connect-packet (settings)
  "CONNECT: level 4, clean session, this client's id, and its user and password."
  (let* ((username (or (getf settings :username) ""))
         (password (getf settings :password))
         (has-user (plusp (length username)))
         (flags (logior 2 (if has-user 128 0) (if (and password (plusp (length password))) 64 0)))
         (keepalive (or (getf settings :keepalive) 60))
         (parts (list (utf8 "MQTT")
                      (vector 4 flags (ldb (byte 8 8) keepalive) (ldb (byte 8 0) keepalive))
                      (utf8 (or (getf settings :client-id) "nodecode")))))
    (when has-user (setf parts (append parts (list (utf8 username)))))
    (when (logbitp 6 flags) (setf parts (append parts (list (utf8 password)))))
    (apply #'packet (ash 1 4) parts)))

(defun disconnect-packet ()
  (packet (ash 14 4)))

(defun subscribe-packet (filter packet-id)
  "SUBSCRIBE at QoS 1: the fixed header flags of 2 are required."
  (packet (logior (ash 8 4) 2)
          (u16 packet-id)
          (utf8 filter)
          (vector 1)))

(defun publish-packet (topic payload packet-id retain)
  "PUBLISH at QoS 1, with the retain bit when RETAIN."
  (packet (logior (ash 3 4) 2 (if retain 1 0))
          (utf8 topic)
          (u16 packet-id)
          payload))

(defun puback-packet (packet-id)
  (packet (ash 4 4) (u16 packet-id)))

(defun connack-reason (code)
  (case code
    (1 "unacceptable protocol version")
    (2 "client id rejected")
    (3 "server unavailable")
    (4 "bad username or password")
    (5 "not authorised")
    (t (format nil "return code ~d" code))))

(defun greet (connection settings timeout)
  "Send CONNECT, read the CONNACK, and answer the broker's return code."
  (send-bytes connection (connect-packet settings))
  (multiple-value-bind (type flags body)
      (read-packet connection (deadline-in timeout) "CONNACK")
    (declare (ignore flags))
    (unless (= type 2)
      (fail "the broker's first packet was type ~d, not a CONNACK" type))
    (when (< (length body) 2)
      (fail "a truncated CONNACK from ~a" (broker-url settings)))
    (let ((code (aref body 1)))
      (unless (zerop code)
        (fail "the MQTT broker refused the connection: ~a" (connack-reason code)))
      code)))

;;; --- what a message is ------------------------------------------------------------

(defun decode-publish (flags body)
  "BODY as (values TOPIC PAYLOAD RETAINED-P PACKET-ID-or-NIL)."
  (let* ((qos (ldb (byte 2 1) flags))
         (retained (logbitp 0 flags))
         (topic-length (logior (ash (aref body 0) 8) (aref body 1)))
         (topic (babel:octets-to-string (subseq body 2 (+ 2 topic-length)) :encoding :utf-8))
         (start (+ 2 topic-length))
         (packet-id (when (plusp qos)
                       (when (>= (length body) (+ start 2))
                         (logior (ash (aref body start) 8) (aref body (1+ start))))))
         (payload (subseq body (if packet-id (+ start 2) start))))
    (values topic payload retained packet-id)))

(defun message-text (payload)
  "PAYLOAD as text: UTF-8 when it is, an octet count when it is not."
  (or (ignore-errors (babel:octets-to-string payload :encoding :utf-8))
      (format nil "~d bytes, not UTF-8" (length payload))))

;;; --- the three things the cell does ------------------------------------------------

(defun wire-publish (settings topic payload &key retain)
  "Publish PAYLOAD to TOPIC at QoS 1 and wait for the broker's PUBACK
=> a plist (:bytes N)."
  (with-connection (connection settings)
    (let* ((deadline (deadline-in (or (getf settings :timeout) 10)))
           (id 1))
      (send-bytes connection (publish-packet topic payload id retain))
      (loop
        (multiple-value-bind (type flags body)
            (read-packet connection deadline "PUBACK")
          (declare (ignore flags))
          (when (and (= type 4) (>= (length body) 2)
                     (= id (logior (ash (aref body 0) 8) (aref body 1))))
            (return (list :bytes (length payload))))))
      (send-bytes connection (disconnect-packet)))))

(defun wire-read (settings filter &key (count 1) (timeout 5))
  "Subscribe to FILTER and collect up to COUNT messages within TIMEOUT,
leaving the connection open only that long.
=> a list of plists (:topic, :payload, :retained)."
  (with-connection (connection (list* :timeout (max timeout (getf settings :timeout 5)) settings))
    (let ((deadline (deadline-in timeout))
          (got '()))
      (send-bytes connection (subscribe-packet filter 1))
      (loop while (< (length got) count)
            do (handler-case
                   (multiple-value-bind (type flags body)
                       (read-packet connection deadline filter)
                     (case type
                       (9 (when (and (>= (length body) 3) (= 128 (aref body 2)))
                            (fail "the broker refused the subscription ~s" filter)))
                       (3 (multiple-value-bind (topic payload retained id)
                              (decode-publish flags body)
                            (when id (send-bytes connection (puback-packet id)))
                            (push (list :topic topic :payload (message-text payload)
                                        :retained retained)
                                  got)))))
                 (mqtt-error ()
                   ;; the window closed, or the broker left: what arrived is the answer
                   (return))))
      (ignore-errors (send-bytes connection (disconnect-packet)))
      (nreverse got))))

(defun wire-probe (settings)
  "Connect, read the broker's CONNACK, and answer as text; then leave."
  (with-connection (connection settings)
    (ignore-errors (send-bytes connection (disconnect-packet)))
    (format nil "the MQTT broker at ~a accepted the connection" (broker-url settings))))
