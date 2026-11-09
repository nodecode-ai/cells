;;;; audio.lisp --- the bytes ElevenLabs hands back: which container they are,
;;;; how long they play, and the name they are saved under.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin this cell rewrites read the playing time of the file it saved
;;;; rather than the length it asked for, because the two differ: a 1 s loop
;;;; comes back 1.5 s long and the bill follows the request. So the length is
;;;; parsed here out of the bytes -- MP3 frames, the WAV header, the Ogg
;;;; granule -- and ffprobe never runs.

(in-package #:nodecode-sound-effects)

(defun ascii (text)
  "TEXT's characters as the octets a container header stores them as."
  (map '(vector (unsigned-byte 8)) #'char-code text))

(defun octets-of (data)
  "DATA as octets: NLK:HTTP answers a string when the peer sent text."
  (etypecase data
    (string (map '(vector (unsigned-byte 8)) #'char-code data))
    ((array (unsigned-byte 8) (*)) data)
    ((array t (*)) data)
    (null (make-array 0 :element-type '(unsigned-byte 8)))))

(defun octets-text (data)
  "DATA, text or octets, as a string."
  (if (stringp data) data (map 'string #'code-char data)))

(defun u8 (octets at)
  (aref octets at))

(defun uint-le (octets at bytes)
  "BYTES little-endian at AT."
  (loop for i below bytes sum (ash (aref octets (+ at i)) (* 8 i))))

(defun uint-be (octets at bytes)
  "BYTES big-endian at AT."
  (loop for i below bytes sum (ash (aref octets (+ at i)) (* 8 (- bytes 1 i)))))

;;; --- which container is this ---------------------------------------------------

(defun audio-p (octets extension)
  "Whether OCTETS begin like the container EXTENSION names. ElevenLabs' MIME
header is not trusted: a 200 carrying a JSON error is what this catches."
  (let ((n (length octets)))
    (cond ((string= extension "wav")
           (and (>= n 12) (equalp (subseq octets 0 4) (ascii "RIFF"))
                (equalp (subseq octets 8 12) (ascii "WAVE"))))
          ((string= extension "opus")
           (and (>= n 4) (equalp (subseq octets 0 4) (ascii "OggS"))
                (and (search (ascii "OpusHead") octets :end2 (min n 512)) t)))
          (t (or (and (>= n 3) (equalp (subseq octets 0 3) (ascii "ID3")))
                 (and (>= n 2) (= (u8 octets 0) #xFF)
                      (= (logand (u8 octets 1) #xE0) #xE0)))))))

;;; --- a name ---------------------------------------------------------------------

(defun slug (text)
  "TEXT as a file name's stem: lowercase letters, digits and single dashes, at
most 40 of them, never empty."
  (let ((chars '()) (dash t) (n 0))
    (loop for char across (string-downcase (or text ""))
          while (< n 40)
          do (cond ((alphanumericp char) (push char chars) (setf dash nil) (incf n))
                   ((not dash) (push #\- chars) (setf dash t) (incf n))))
    (let ((out (string-right-trim "-" (coerce (nreverse chars) 'string))))
      (if (zerop (length out)) "sound" out))))

(defun datestamp ()
  "The local time as 20261108_101500, the way a file name carries it."
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (format nil "~4,'0d~2,'0d~2,'0d_~2,'0d~2,'0d~2,'0d"
            year month date hour minute second)))

;;; --- WAV --------------------------------------------------------------------------

(defun wav-seconds (octets)
  "A RIFF/WAVE stream's playing time from its fmt chunk's byte rate and its
data chunk's size, or NIL."
  (let ((size (length octets)) (at 12) (byte-rate nil))
    (when (and (>= size 12) (equalp (subseq octets 0 4) (ascii "RIFF"))
               (equalp (subseq octets 8 12) (ascii "WAVE")))
      (loop while (<= (+ at 8) size)
            do (let ((id (subseq octets at (+ at 4)))
                     (n (uint-le octets (+ at 4) 4)))
                 (cond ((equalp id (ascii "fmt "))
                        (when (and (>= n 16) (<= (+ at 20) size))
                          (setf byte-rate (uint-le octets (+ at 16) 4))))
                       ((equalp id (ascii "data"))
                        (let ((bytes (max 0 (min (if (or (zerop n) (= n #xFFFFFFFF))
                                                  (- size at 8)
                                                  n)
                                              (- size at 8)))))
                          (return (and byte-rate (plusp byte-rate) (/ bytes byte-rate))))))
                 (setf at (+ at 8 n (* (mod n 2)))))))))

;;; --- MP3 -------------------------------------------------------------------------

(defparameter +mp3-bitrates+
  '(((t . 1) 0 32 64 96 128 160 192 224 256 288 320 352 384 416 448)
    ((t . 2) 0 32 48 56 64 80 96 112 128 160 192 224 256 320 384)
    ((t . 3) 0 32 40 48 56 64 80 96 112 128 160 192 224 256 320)
    ((nil . 1) 0 32 48 56 64 80 96 112 128 144 160 176 192 224 256)
    ((nil . 2) 0 8 16 24 32 40 48 56 64 80 96 112 128 144 160)
    ((nil . 3) 0 8 16 24 32 40 48 56 64 80 96 112 128 144 160))
  "MPEG audio bitrates in kbps, by (MPEG1-P . LAYER) and header index.")

(defparameter +mp3-rates+
  '((3 44100 48000 32000) (2 22050 24000 16000) (0 11025 12000 8000))
  "Sample rates by the header's version bits.")

(defparameter +mp3-junk-scan+ 65536
  "How far past an ID3v2 tag the first MPEG frame may start.")

(defun mp3-frame (octets at)
  "The MPEG audio frame at AT as (values BYTES SAMPLES RATE), or NIL when
nothing valid syncs there."
  (when (and (<= (+ at 4) (length octets))
             (= (u8 octets at) #xFF)
             (= (logand (u8 octets (1+ at)) #xE0) #xE0))
    (let* ((b1 (u8 octets (1+ at)))
           (b2 (u8 octets (+ at 2)))
           (version (ldb (byte 2 3) b1))
           (layer-bits (ldb (byte 2 1) b1))
           (bitrate-index (ldb (byte 4 4) b2))
           (rate-index (ldb (byte 2 2) b2))
           (padding (ldb (byte 1 1) b2)))
      (unless (or (= version 1) (zerop layer-bits)
                  (zerop bitrate-index) (= bitrate-index 15) (= rate-index 3))
        (let* ((layer (- 4 layer-bits))
               (mpeg1 (= version 3))
               (row (cdr (assoc (cons mpeg1 layer) +mp3-bitrates+ :test #'equal)))
               (bitrate (* 1000 (nth bitrate-index row)))
               (rate (nth (1+ rate-index) (assoc version +mp3-rates+))))
          (if (= layer 1)
              (values (* (+ (floor (* 12 bitrate) rate) padding) 4) 384 rate)
              (let ((samples (if (or (= layer 2) mpeg1) 1152 576)))
                (values (+ (floor (* (/ samples 8) bitrate) rate) padding) samples rate))))))))

(defun mp3-start (octets)
  "Where the first MPEG frame begins: past an ID3v2 tag when one is there,
then at the first offset that parses as a frame."
  (let ((size (length octets)) (at 0))
    (when (and (>= size 10) (equalp (subseq octets 0 3) (ascii "ID3")))
      (let ((tag-size (logior (ash (logand (u8 octets 6) #x7F) 21)
                              (ash (logand (u8 octets 7) #x7F) 14)
                              (ash (logand (u8 octets 8) #x7F) 7)
                              (logand (u8 octets 9) #x7F))))
        (setf at (min size (+ 10 tag-size (if (logbitp 4 (u8 octets 5)) 10 0))))))
    (let ((stop (min size (+ at +mp3-junk-scan+))))
      (loop while (and (< at (- stop 3)) (not (mp3-frame octets at))) do (incf at)))
    (min at size)))

(defun mp3-header-frame-p (octets at bytes)
  "Whether the frame at AT is a Xing/Info/VBRI header: it counts in the file
size and carries no audio."
  (let ((frame (subseq octets at (min (length octets) (+ at bytes)))))
    (and (or (search (ascii "Xing") frame)
             (search (ascii "Info") frame)
             (search (ascii "VBRI") frame))
         t)))

(defun mp3-seconds (octets)
  "An MPEG stream's playing time, counted frame by frame, or NIL when no frame
parses. A junk byte that passes one header check would add one frame's worth;
the walk stops at the first offset that parses nothing."
  (let ((size (length octets)) (at (mp3-start octets)) (seconds 0) (frames 0) (first t))
    (loop while (< at size)
          do (multiple-value-bind (bytes samples rate) (mp3-frame octets at)
               (if (and bytes (plusp bytes) rate (plusp rate))
                   (progn
                     (unless (and first (mp3-header-frame-p octets at bytes))
                       (incf seconds (/ samples (float rate)))
                       (incf frames))
                     (setf first nil at (+ at bytes)))
                   (return))))
    (and (plusp frames) seconds)))

;;; --- Ogg Opus ----------------------------------------------------------------------

(defun opus-pre-skip (octets)
  "The pre-skip the stream's OpusHead names: samples a decoder drops, which the
granule position counts."
  (let ((at (search (ascii "OpusHead") octets :end2 (min (length octets) 512))))
    (when (and at (<= (+ at 12) (length octets)))
      (uint-le octets (+ at 10) 2))))

(defun ogg-seconds (octets)
  "An Ogg Opus stream's playing time: the last page's granule over 48 kHz, less
the pre-skip, or NIL."
  (when (and (>= (length octets) 27) (equalp (subseq octets 0 4) (ascii "OggS")))
    (let ((size (length octets)) (at 0) (granule nil))
      (loop while (and (<= (+ at 27) size)
                       (equalp (subseq octets at (+ at 4)) (ascii "OggS")))
            do (let* ((segments (u8 octets (+ at 26)))
                      (table (+ at 27))
                      (body (loop for i below segments sum (u8 octets (+ table i)))))
                 (when (> (+ table segments body) size) (return))
                 (let ((position (uint-le octets (+ at 6) 8)))
                   ;; -1 marks a page that finishes no packet: it is not a length.
                   (unless (= position #xFFFFFFFFFFFFFFFF) (setf granule position)))
                 (setf at (+ table segments body))))
      (when granule
        (max 0.0 (/ (- granule (or (opus-pre-skip octets) 0)) 48000.0))))))

;;; --- the whole answer ----------------------------------------------------------------

(defun audio-seconds (octets extension)
  "How long OCTETS play, in seconds, or NIL when the bytes carry no length
this cell can read. Never signals: a sound is delivered whatever this says."
  (let ((seconds (handler-case
                     (cond ((string= extension "wav") (wav-seconds octets))
                           ((string= extension "opus") (ogg-seconds octets))
                           (t (mp3-seconds octets)))
                   (error () nil))))
    (and (realp seconds) (plusp seconds) seconds)))
