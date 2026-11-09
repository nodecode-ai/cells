;;;; wav.lisp --- reading a recording, and writing one.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original decoded any audio or video through ffmpeg and analysed it
;;;; with numpy. This cell reads the one container the Lisp can read without
;;;; a decoder of its own: RIFF/WAVE, PCM 8/16/24/32 or 32-bit float, any
;;;; channel count (mixed down to mono). Anything else refuses by name, and
;;;; the README says to convert with ffmpeg first.

(in-package #:nodecode-doppler-geo)

(defconstant +fmt-pcm+ 1)
(defconstant +fmt-float+ 3)
(defconstant +fmt-extensible+ #xFFFE)

(declaim (inline u8 u16 u32 i16 i24 i32))

(defun u8 (bytes i) (aref bytes i))

(defun u16 (bytes i)
  (+ (aref bytes i) (ash (aref bytes (1+ i)) 8)))

(defun u32 (bytes i)
  (logior (aref bytes i)
          (ash (aref bytes (+ i 1)) 8)
          (ash (aref bytes (+ i 2)) 16)
          (ash (aref bytes (+ i 3)) 24)))

(defun i16 (bytes i)
  (let ((v (u16 bytes i))) (if (logbitp 15 v) (- v 65536) v)))

(defun i24 (bytes i)
  (let ((v (logior (aref bytes i)
                    (ash (aref bytes (+ i 1)) 8)
                    (ash (aref bytes (+ i 2)) 16))))
    (if (logbitp 23 v) (- v 16777216) v)))

(defun i32 (bytes i)
  (let ((v (u32 bytes i))) (if (logbitp 31 v) (- v 4294967296) v)))

(defun ascii4 (bytes i)
  (string-downcase (map 'string #'code-char (subseq bytes i (+ i 4)))))

(defun read-bytes (path)
  "PATH's whole content as an octet vector."
  (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence bytes in)
      bytes)))

(defun read-wav (path)
  "PATH's samples as one double-float vector, and the rate they were taken at.
Mixes every channel down to mono. Signals DOPPLER-GEO-ERROR for anything that
is not RIFF/WAVE PCM or float."
  (let* ((bytes (read-bytes path))
         (n (length bytes)))
    (when (< n 44) (fail "~a is too short to be a WAV file" path))
    (unless (and (string= "riff" (ascii4 bytes 0)) (string= "wave" (ascii4 bytes 8)))
      (fail "~a is not a RIFF/WAVE file; convert it first (ffmpeg -i in -ac 1 out.wav)" path))
    (let ((pos 12) (tag nil) (channels nil) (rate nil) (bits nil) (data nil) (data-length nil))
      (loop while (<= (+ pos 8) n)
            for id = (ascii4 bytes pos)
            for size = (u32 bytes (+ pos 4))
            for body = (+ pos 8)
            do (cond ((string= id "fmt ")
                      (setf tag (u16 bytes body)
                            channels (u16 bytes (+ body 2))
                            rate (u32 bytes (+ body 4))
                            bits (u16 bytes (+ body 14)))
                      (when (= tag +fmt-extensible+)
                        (setf tag (u16 bytes (+ body 24)))))
                     ((string= id "data")
                      (setf data body
                            data-length (min size (- n body)))))
               (setf pos (+ body size (if (oddp size) 1 0))))
      (unless (and data channels rate bits)
        (fail "~a has no fmt or data chunk" path))
      (unless (or (= tag +fmt-pcm+) (= tag +fmt-float+))
        (fail "~a is not PCM or 32-bit float WAV (format tag ~d)" path tag))
      (unless (member bits '(8 16 24 32))
        (fail "~a has ~d-bit samples, which this cell does not read" path bits))
      (let* ((width (ceiling bits 8))
             (frame-bytes (* width channels))
             (frames (floor data-length frame-bytes))
             (out (make-array frames :element-type 'double-float)))
        (dotimes (f frames)
          (let ((sum 0d0))
            (dotimes (ch channels)
              (let ((i (+ data (* f frame-bytes) (* ch width))))
                (incf sum
                      (cond ((= tag +fmt-float+)
                             (float (sb-kernel:make-single-float (i32 bytes i)) 1d0))
                            ((= bits 8) (/ (- (u8 bytes i) 128) 128d0))
                            ((= bits 16) (/ (i16 bytes i) 32768d0))
                            ((= bits 24) (/ (i24 bytes i) 8388608d0))
                            (t (/ (i32 bytes i) 2147483648d0))))))
            (setf (aref out f) (/ sum channels))))
        (values out rate)))))

(defun write-wav (path samples &optional (rate 8000))
  "Write SAMPLES (floats in -1..1, any number of them) as 16-bit mono WAV at RATE."
  (let* ((n (length samples))
         (total (+ 44 (* 2 n)))
         (bytes (make-array total :element-type '(unsigned-byte 8) :initial-element 0)))
    (labels ((put-ascii (at text)
               (loop for ch across text for i from 0
                     do (setf (aref bytes (+ at i))
                              (char-code (char-downcase ch)))))
             (put-u16 (at v)
               (setf (aref bytes at) (logand v #xff)
                     (aref bytes (1+ at)) (logand (ash v -8) #xff)))
             (put-u32 (at v)
               (loop for i from 0 below 4
                     do (setf (aref bytes (+ at i)) (logand (ash v (* -8 i)) #xff)))))
      (put-ascii 0 "RIFF") (put-u32 4 (- total 8)) (put-ascii 8 "WAVE")
      (put-ascii 12 "fmt ") (put-u32 16 16) (put-u16 20 1) (put-u16 22 1)
      (put-u32 24 rate) (put-u32 28 (* 2 rate)) (put-u16 32 2) (put-u16 34 16)
      (put-ascii 36 "data") (put-u32 40 (* 2 n))
      (dotimes (i n)
        (let ((v (max -1d0 (min 1d0 (coerce (aref samples i) 'double-float))))
              (at (+ 44 (* 2 i))))
          (put-u16 at (logand (round (* v 32767)) #xffff)))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (write-sequence bytes out))
    path))

(defun decimate (samples rate target)
  "Samples halved down to about TARGET Hz with a three-tap average. => samples, rate."
  (loop while (> rate (* 2.2 target))
        do (let* ((n (length samples))
                  (m (floor n 2))
                  (out (make-array (max 1 m) :element-type 'double-float)))
             (dotimes (i (length out))
               (let ((j (* 2 i)))
                 (setf (aref out i)
                       (/ (+ (aref samples (max 0 (1- j)))
                             (aref samples j)
                             (aref samples (min (1- n) (1+ j))))
                          3d0))))
             (setf samples out rate (/ rate 2d0))))
  (values samples rate))
