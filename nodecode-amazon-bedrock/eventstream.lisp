;;;; eventstream.lisp --- the application/vnd.amazon.eventstream decoder.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/ai/src/providers/
;;;; aws-eventstream.ts. Converse Stream answers with binary frames, not
;;;; server-sent events, so the core's line walk cannot read it; this is the
;;;; frame reader the cell's lane reads instead. A frame, every integer
;;;; big-endian:
;;;;
;;;;   [total length u32] [headers length u32] [prelude CRC32 u32]
;;;;   [headers]          [payload]            [message CRC32 u32]
;;;;
;;;; the prelude CRC over the first eight octets, the message CRC over all
;;;; but the last four. A header is [name length u8][name][type u8][value];
;;;; every typed value is surfaced as text, as omp does (Bedrock sets only
;;;; string headers: :event-type, :message-type, :content-type,
;;;; :exception-type, :error-code, :error-message).

(in-package #:nodecode-amazon-bedrock)

(defparameter +minimum-frame+ 16
  "The shortest frame: a prelude, its CRC and the message CRC.")

(defparameter +crc-table+
  (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (n 256 table)
      (let ((c n))
        (dotimes (k 8) (setf c (if (logbitp 0 c) (logxor #xEDB88320 (ash c -1)) (ash c -1))))
        (setf (aref table n) c))))
  "The CRC-32 table of the IEEE / zlib polynomial.")

(defun crc32 (octets &key (start 0) (end (length octets)))
  "The CRC-32 (IEEE, zlib's) of OCTETS between START and END."
  (let ((crc #xFFFFFFFF))
    (loop for at from start below end
          do (setf crc (logxor (aref +crc-table+ (logand #xFF (logxor crc (aref octets at)))) (ash crc -8))))
    (logxor crc #xFFFFFFFF)))

(defun u32 (octets at)
  (logior (ash (aref octets at) 24) (ash (aref octets (+ at 1)) 16) (ash (aref octets (+ at 2)) 8) (aref octets (+ at 3))))

(defun octets-integer (octets start end)
  "The unsigned big-endian integer OCTETS spell between START and END."
  (loop with value = 0
        for at from start below end
        do (setf value (logior (ash value 8) (aref octets at)))
        finally (return value)))

(defun signed (value bits)
  "VALUE, an unsigned BITS-bit integer, read as two's complement."
  (if (logbitp (1- bits) value) (- value (ash 1 bits)) value))

(defun frame-error (format-control &rest arguments)
  "A frame that cannot be read: the stream is cut, as omp's EventStreamFrameError says."
  (error 'nle::provider-stream-incomplete
         :detail (format nil "Bedrock event stream: ~?" format-control arguments)))

(defun utf-8-text (octets start end)
  (sb-ext:octets-to-string octets :start start :end end :external-format '(:utf-8 :replacement #\?)))

(defun parse-headers (octets start end)
  "The headers between START and END of OCTETS, as an alist of name to text."
  (let ((headers '()) (at start))
    (loop while (< at end)
          do (let* ((name-length (aref octets at))
                    (name (utf-8-text octets (+ at 1) (+ at 1 name-length)))
                    (type (aref octets (+ at 1 name-length))))
               (setf at (+ at 2 name-length))
               (flet ((take (count) (prog1 (octets-integer octets at (+ at count)) (incf at count))))
                 (push (cons name
                             (case type
                               (0 "true")
                               (1 "false")
                               (2 (princ-to-string (signed (take 1) 8)))
                               (3 (princ-to-string (signed (take 2) 16)))
                               (4 (princ-to-string (signed (take 4) 32)))
                               (5 (princ-to-string (signed (take 8) 64)))
                               (6 (let ((length (take 2)))
                                    (prog1 (cl-base64:usb8-array-to-base64-string (subseq octets at (+ at length)))
                                      (incf at length))))
                               (7 (let ((length (take 2)))
                                    (prog1 (utf-8-text octets at (+ at length)) (incf at length))))
                               (8 (let ((ms (signed (take 8) 64)))
                                    (multiple-value-bind (seconds millis) (floor ms 1000)
                                      (multiple-value-bind (s mi h d mo y)
                                          (decode-universal-time (+ seconds #.(encode-universal-time 0 0 0 1 1 1970 0)) 0)
                                        (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d.~3,'0dZ" y mo d h mi s millis)))))
                               (9 (let ((hex (hex (subseq octets at (+ at 16)))))
                                    (incf at 16)
                                    (format nil "~a-~a-~a-~a-~a" (subseq hex 0 8) (subseq hex 8 12) (subseq hex 12 16)
                                            (subseq hex 16 20) (subseq hex 20 32))))
                               (t (frame-error "unknown header value type ~d" type))))
                       headers))))
    (nreverse headers)))

(defun decode-message (frame)
  "(values HEADERS PAYLOAD) of one whole FRAME: HEADERS an alist of name to
text, PAYLOAD octets. A short frame, a length that disagrees, or either CRC
wrong is a cut stream (omp's decodeMessage)."
  (let ((length (length frame)))
    (when (< length +minimum-frame+) (frame-error "frame too short"))
    (let ((total (u32 frame 0))
          (headers-length (u32 frame 4)))
      (unless (= total length) (frame-error "framed length ~d != buffer ~d" total length))
      (unless (= (u32 frame 8) (crc32 frame :end 8)) (frame-error "prelude CRC mismatch"))
      (unless (= (u32 frame (- total 4)) (crc32 frame :end (- total 4))) (frame-error "message CRC mismatch"))
      (when (> (+ 12 headers-length) (- total 4)) (frame-error "headers length ~d overruns the frame" headers-length))
      (values (parse-headers frame 12 (+ 12 headers-length))
              (subseq frame (+ 12 headers-length) (- total 4))))))

(defun read-full (stream buffer start end)
  "Fill BUFFER from START to END off STREAM => the index reached (END unless EOF came first)."
  (loop while (< start end)
        do (let ((reached (read-sequence buffer stream :start start :end end)))
             (when (= reached start) (return))
             (setf start reached)))
  start)

(defun read-frame (stream seconds)
  "The next whole frame STREAM delivers, as octets, or NIL at a clean end of
stream. Each read waits at most SECONDS; an end inside a frame is a cut stream."
  (let ((prelude (make-array 4 :element-type '(unsigned-byte 8))))
    (let ((got (sb-sys:with-deadline (:seconds seconds) (read-full stream prelude 0 4))))
      (cond ((zerop got) nil)
            ((< got 4) (frame-error "truncated message at end of stream"))
            (t (let ((total (u32 prelude 0)))
                 (when (< total +minimum-frame+) (frame-error "total length ~d below minimum" total))
                 (let ((frame (make-array total :element-type '(unsigned-byte 8))))
                   (replace frame prelude)
                   (unless (= total (sb-sys:with-deadline (:seconds seconds) (read-full stream frame 4 total)))
                     (frame-error "truncated message at end of stream"))
                   frame)))))))
