;;;; wav.lisp --- a sound, as a 16-bit mono PCM WAV any player, editor or
;;;; engine opens.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode-gameforge)

(defparameter +rate+ 44100
  "Samples a second. The original plugin's rate, and every WAV this cell
writes is that: 44.1 kHz mono, which is what sfxr and a browser both read")

(defun samples->i16 (samples)
  "SAMPLES (reals in -1..1) as the little-endian octets of a 16-bit PCM body.
Hard-clipped, so a generator that overshoots distorts rather than wraps."
  (let ((out (make-octets (max 256 (* 2 (length samples))))))
    (loop for sample across samples
          for clipped = (max -1.0 (min 1.0 (coerce sample 'single-float)))
          for value = (round (* clipped 32767))
          do (push-le16 out (if (minusp value) (+ value 65536) value)))
    out))

(defun wav-bytes (samples &key (rate +rate+))
  "The whole .wav file for SAMPLES: a RIFF header and the PCM body."
  (let* ((body (samples->i16 samples))
         (size (length body))
         (out (make-octets (+ 44 size))))
    (push-ascii out "RIFF")
    (push-le32 out (+ 36 size))
    (push-ascii out "WAVEfmt ")
    (push-le32 out 16)              ; the fmt chunk's size
    (push-le16 out 1)               ; PCM
    (push-le16 out 1)               ; one channel: a game sound is positional at playback
    (push-le32 out rate)
    (push-le32 out (* rate 2))      ; bytes a second
    (push-le16 out 2)               ; bytes a frame
    (push-le16 out 16)              ; bits a sample
    (push-ascii out "data")
    (push-le32 out size)
    (push-octets out body)
    out))

(defun write-wav (path samples &key (rate +rate+))
  "Write SAMPLES to PATH as a WAV; answer the path's namestring."
  (ensure-directories-exist path)
  (with-open-file (stream path :direction :output :if-exists :supersede
                                :element-type '(unsigned-byte 8))
    (write-sequence (wav-bytes samples :rate rate) stream))
  (namestring path))

(defun read-wav-header (path)
  "(values CHANNELS RATE BITS DATA-BYTES) read back off a file this cell wrote,
so a test checks the bytes rather than the request => the header's own numbers."
  (with-open-file (stream path :direction :input :element-type '(unsigned-byte 8))
    (let ((head (make-array 44 :element-type '(unsigned-byte 8))))
      (read-sequence head stream)
      (labels ((le16 (at) (+ (aref head at) (ash (aref head (1+ at)) 8)))
               (le32 (at) (+ (aref head at) (ash (aref head (+ at 1)) 8)
                             (ash (aref head (+ at 2)) 16) (ash (aref head (+ at 3)) 24))))
        (values (le16 22) (le32 24) (le16 34) (le32 40))))))
