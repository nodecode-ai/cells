;;;; wire.lisp --- Minecraft's framing, as bytes over one stream.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A packet is a VarInt length, an optional VarInt data length and zlib
;;;; body when the server turned compression on (threshold > 0), then the
;;;; packet id and its fields, all VarInts and big-endian numbers. This file
;;;; is the arithmetic and nothing about the game: PROTOCOL.LISP names the
;;;; packets, CELL.LISP decides what a chat message means.

(in-package #:nodecode-minecraft)

(define-condition minecraft-error (error)
  ((detail :initarg :detail :reader minecraft-error-detail))
  (:report (lambda (condition stream) (write-string (minecraft-error-detail condition) stream)))
  (:documentation "The connection broke, or the server said no."))

(defun fail (format &rest arguments)
  (error 'minecraft-error :detail (apply #'format nil format arguments)))

;;; --- writing -------------------------------------------------------------------

(defstruct (buf (:copier nil) (:constructor make-buf ()))
  (octets (make-array 64 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))

(defun buf-byte (buf n)
  "One unsigned byte of N, low eight bits."
  (vector-push-extend (ldb (byte 8 0) n) (buf-octets buf))
  (buf-octets buf))

(defun buf-bytes (buf octets)
  (loop for octet across octets do (buf-byte buf octet))
  (buf-octets buf))

(defun buf-varint (buf n)
  "N as a VarInt: seven bits a byte, the top bit meaning another follows."
  (let ((n (ldb (byte 32 0) n)))
    (loop
      (let ((seven (ldb (byte 7 0) n)))
        (setf n (ash n -7))
        (if (zerop n)
            (progn (buf-byte buf seven) (return (buf-octets buf)))
            (buf-byte buf (logior seven #x80)))))))

(defun buf-us (buf n)
  "N as an unsigned big-endian short."
  (buf-byte buf (ldb (byte 8 8) n))
  (buf-byte buf (ldb (byte 8 0) n))
  (buf-octets buf))

(defun buf-long (buf n)
  "N as a signed big-endian 64-bit long, two's complement."
  (let ((n (ldb (byte 64 0) n)))
    (loop for shift from 56 downto 0 by 8 do (buf-byte buf (ldb (byte 8 shift) n))))
  (buf-octets buf))

(defun buf-float (buf x)
  (let ((bits (sb-kernel:single-float-bits (coerce x 'single-float))))
    (loop for shift from 24 downto 0 by 8 do (buf-byte buf (ldb (byte 8 shift) bits))))
  (buf-octets buf))

(defun buf-double (buf x)
  (let* ((x (coerce x 'double-float))
         (high (sb-kernel:double-float-high-bits x))
         (low (sb-kernel:double-float-low-bits x)))
    (loop for shift from 24 downto 0 by 8 do (buf-byte buf (ldb (byte 8 shift) high)))
    (loop for shift from 24 downto 0 by 8 do (buf-byte buf (ldb (byte 8 shift) low))))
  (buf-octets buf))

(defun buf-string (buf string)
  "STRING as a VarInt byte count and its UTF-8 bytes."
  (let ((octets (sb-ext:string-to-octets (or string "") :external-format :utf-8)))
    (buf-varint buf (length octets))
    (buf-bytes buf octets)))

(defun buf-bool (buf value)
  (buf-byte buf (if value 1 0))
  (buf-octets buf))

;;; --- reading -------------------------------------------------------------------

(defun read-octets (stream count)
  "COUNT bytes off STREAM; the end of the stream is the connection ending."
  (let ((octets (make-array count :element-type '(unsigned-byte 8))))
    (dotimes (i count octets)
      (let ((byte (read-byte stream nil nil)))
        (unless byte (fail "minecraft: the server closed the connection"))
        (setf (aref octets i) byte)))))

(defun read-u8 (stream)
  (let ((byte (read-byte stream nil nil)))
    (unless byte (fail "minecraft: the server closed the connection"))
    byte))

(defun read-varint (stream)
  "A VarInt off STREAM: at most five bytes, so a corrupt length cannot hang."
  (let ((value 0) (shift 0))
    (dotimes (i 5 value)
      (let ((byte (read-u8 stream)))
        (setf value (logior value (ash (ldb (byte 7 0) byte) shift)))
        (when (zerop (ldb (byte 1 7) byte))
          (return (ldb (byte 32 0) value)))
        (incf shift 7)))))

(defun read-us (stream)
  (let ((octets (read-octets stream 2)))
    (logior (ash (aref octets 0) 8) (aref octets 1))))

(defun simple-octets (octets)
  "OCTETS as the kind of array the compressors take: no fill pointer, no
adjustability."
  (coerce octets '(simple-array (unsigned-byte 8) (*))))

(defun deflate-bytes (octets)
  (coerce (salza2:compress-data (simple-octets octets) 'salza2:zlib-compressor)
          '(vector (unsigned-byte 8))))

(defun inflate-bytes (octets)
  (multiple-value-bind (out size) (chipz:decompress nil 'chipz:zlib (simple-octets octets))
    (if size (subseq out 0 size) out)))

(defun read-frame (stream threshold)
  "The next packet off STREAM => (VALUES ID BODY-OCTETS), the length prefix
and any compression already taken off. THRESHOLD is NIL before the server
turned compression on."
  (let* ((length (read-varint stream))
         (raw (read-octets stream length)))
    (if (null threshold)
        (let ((head (make-cur raw)))
          (values (cur-varint head) (subseq raw (cur-pos head))))
        (let* ((head (make-cur raw))
               (data-length (cur-varint head)))
          (let ((body (if (zerop data-length)
                          (subseq raw (cur-pos head))
                          (inflate-bytes (subseq raw (cur-pos head))))))
            (let ((inner (make-cur (coerce body '(vector (unsigned-byte 8))))))
              (values (cur-varint inner) (subseq (cur-data inner) (cur-pos inner)))))))))

(defun write-frame (stream threshold &rest body)
  "Write one packet: BODY is the pieces of it, the first the packet id.
THRESHOLD is NIL before the server turned compression on, then the server's
threshold: a body at or over it goes out deflated behind its own length."
  (let ((payload (make-buf)))
    (dolist (piece body)
      (cond ((integerp piece) (buf-varint payload piece))
            ((typep piece '(vector (unsigned-byte 8))) (buf-bytes payload piece))
            (t (buf-string payload piece))))
    (let* ((octets (buf-octets payload))
           (frame (make-buf)))
      (cond
        ((null threshold) (buf-bytes frame octets))
        ((>= (length octets) (max 0 threshold))
         (buf-varint frame (length octets))
         (buf-bytes frame (deflate-bytes octets)))
        (t (buf-varint frame 0) (buf-bytes frame octets)))
      (let ((whole (make-buf)))
        (buf-varint whole (length (buf-octets frame)))
        (buf-bytes whole (buf-octets frame))
        (write-sequence (buf-octets whole) stream))
      (finish-output stream)
      (length octets))))

;;; --- a cursor over one packet's body --------------------------------------------

(defstruct (cur (:copier nil) (:constructor make-cur (data &optional (pos 0))))
  (data #())
  (pos 0 :type fixnum))

(defun cur-left (cur) (- (length (cur-data cur)) (cur-pos cur)))

(defun cur-exhausted-p (cur) (<= (cur-left cur) 0))

(defun cur-take (cur count)
  (when (> count (cur-left cur))
    (fail "minecraft: a packet ended early (~d bytes wanted, ~d left)" count (cur-left cur)))
  (let ((octets (subseq (cur-data cur) (cur-pos cur) (+ (cur-pos cur) count))))
    (incf (cur-pos cur) count)
    octets))

(defun cur-u8 (cur)
  (when (cur-exhausted-p cur) (fail "minecraft: a packet ended early"))
  (prog1 (aref (cur-data cur) (cur-pos cur))
    (incf (cur-pos cur))))

(defun cur-bool (cur) (plusp (cur-u8 cur)))

(defun cur-varint (cur)
  (let ((value 0) (shift 0))
    (dotimes (i 5 value)
      (let ((byte (cur-u8 cur)))
        (setf value (logior value (ash (ldb (byte 7 0) byte) shift)))
        (when (zerop (ldb (byte 1 7) byte))
          (return (ldb (byte 32 0) value)))
        (incf shift 7)))))

(defun cur-long (cur)
  (let ((octets (cur-take cur 8))
        (value 0))
    (dotimes (i 8 value)
      (setf value (logior (ash value 8) (aref octets i))))))

(defun cur-float (cur)
  (let ((octets (cur-take cur 4))
        (bits 0))
    (dotimes (i 4)
      (setf bits (logior (ash bits 8) (aref octets i))))
    (sb-kernel:make-single-float (if (logbitp 31 bits) (- bits (ash 1 32)) bits))))

(defun cur-double (cur)
  (let ((octets (cur-take cur 8))
        (bits 0))
    (dotimes (i 8)
      (setf bits (logior (ash bits 8) (aref octets i))))
    (let ((high (ldb (byte 32 32) bits))
          (low (ldb (byte 32 0) bits)))
      (sb-kernel:make-double-float (if (logbitp 31 high) (- high (ash 1 32)) high) low))))

(defun cur-string (cur)
  (let* ((length (cur-varint cur))
         (octets (cur-take cur length)))
    (sb-ext:octets-to-string octets :external-format :utf-8)))

(defun cur-uuid (cur)
  "The 16 bytes as the dashed id the game shows, so a name map can key on it."
  (let ((octets (cur-take cur 16)))
    (format nil "~(~{~2,'0x~}~)"
            (coerce octets 'list))))

(defun cur-skip (cur count) (cur-take cur count) nil)
