;;;; render.lisp --- score to audio: fluidsynth per part, a mix, ffmpeg's mastering.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin mixed numpy stems through its own EQ and convolution reverb.
;;;; This cell renders one stem per part with fluidsynth, sums the stems with
;;;; per-part gain and pan, adds a synthesised room, and hands the result to
;;;; ffmpeg for EBU R128 loudness and the mp3. Nothing leaves the machine, and
;;;; the same score renders the same audio.

(in-package #:nodecode-hermes-band)

;;; --- the seams a test stubs ---------------------------------------------------------

(defun run-argv (argv)
  "ARGV through the shell-free runner: (values OUT ERR EXIT)."
  (multiple-value-bind (out err code)
      (uiop:run-program argv :output :string :error-output :string
                              :ignore-error-status t)
    (values (or out "") (or err "") (or code 0))))

(defvar *run* #'run-argv
  "The one seam that runs a program: (ARGV) => (values OUT ERR EXIT).")

(defun find-program (name)
  "NAME on this machine's PATH, or NIL."
  (let ((direct (probe-file name)))
    (if direct
        (uiop:native-namestring direct)
        (loop for directory in (uiop:split-string (or (uiop:getenv "PATH") "")
                                                  :separator ":")
              for candidate = (merge-pathnames name
                                               (uiop:ensure-directory-pathname directory))
              when (and (plusp (length directory)) (probe-file candidate))
                return (uiop:native-namestring candidate)))))

(defvar *find-program* #'find-program
  "How a binary is looked for. Tests replace it; nothing else does.")

;;; --- what the machine has ------------------------------------------------------------

(defun soundfont-path ()
  "The soundfont the section names, else the first one this machine has."
  (let ((named (nlk:trimmed (or (setting :soundfont) ""))))
    (cond ((plusp (length named)) named)
          (t (loop for candidate in +soundfont-candidates+
                   when (probe-file candidate) return candidate)))))

(defun doctor-report ()
  "What this host can and cannot render with, as a plist."
  (let* ((fluidsynth (funcall *find-program* (or (setting :fluidsynth) "fluidsynth")))
         (ffmpeg (funcall *find-program* (or (setting :ffmpeg) "ffmpeg")))
         (soundfont (soundfont-path))
         (output (setting :output-dir))
         (writable (handler-case
                       (progn (ensure-directories-exist output) t)
                     (error () nil))))
    (list :fluidsynth (or fluidsynth "")
          :ffmpeg (or ffmpeg "")
          :soundfont (or soundfont "")
          :soundfont_mb (if (and soundfont (probe-file soundfont))
                            (round (/ (with-open-file (in soundfont :element-type '(unsigned-byte 8))
                                        (file-length in))
                                      1000000.0))
                            0)
          :output_dir (namestring output)
          :output_writable writable
          :ready (and fluidsynth ffmpeg soundfont writable))))

(defun doctor-hint (report)
  (cond ((getf report :ready) "")
        (t "install the audio toolchain: apt install fluidsynth fluid-soundfont-gm ffmpeg, or set hermes-band.soundfont to an existing .sf2")))

;;; --- WAV -----------------------------------------------------------------------

(defun le16 (octets at)
  (+ (aref octets at) (ash (aref octets (1+ at)) 8)))

(defun le32 (octets at)
  (+ (aref octets at) (ash (aref octets (+ at 1)) 8)
     (ash (aref octets (+ at 2)) 16) (ash (aref octets (+ at 3)) 24)))

(defun octet-string (octets at count)
  (loop for index below count collect (code-char (aref octets (+ at index)))))

(defun read-wav (path)
  "PATH as (values LEFT RIGHT RATE): two float vectors, and the sample rate."
  (let ((octets (with-open-file (in path :element-type '(unsigned-byte 8))
                   (let ((buffer (make-array (file-length in) :element-type '(unsigned-byte 8))))
                     (read-sequence buffer in)
                     buffer))))
    (unless (and (>= (length octets) 44) (equalp (subseq octets 0 4) (map '(vector (unsigned-byte 8)) #'char-code "RIFF")))
      (fail "~a is not a RIFF/WAVE file" path))
    (let ((at 12) (rate 44100) (channels 1) (bits 16) (data nil))
      (loop while (<= (+ at 8) (length octets))
            do (let* ((id (coerce (octet-string octets at 4) 'string))
                      (size (le32 octets (+ at 4))))
                 (cond ((string= id "fmt ")
                        (setf channels (le16 octets (+ at 10))
                              rate (le32 octets (+ at 12))
                              bits (le16 octets (+ at 22))))
                       ((string= id "data")
                        (let ((bytes (min size (- (length octets) (+ at 8)))))
                          (setf data (+ at 8 bytes)))))
                 (setf at (+ at 8 size (* (mod size 2))))))
      (unless (and data (plusp data)) (fail "~a holds no audio data" path))
      (unless (= bits 16) (fail "~a is ~d-bit; this cell reads 16-bit wav" path bits))
      (let* ((frames (/ data (* 2 channels)))
             (left (make-array frames :element-type 'double-float :initial-element 0.0d0))
             (right (make-array frames :element-type 'double-float :initial-element 0.0d0))
             (start 44))
        ;; The data chunk begins where it was found, not always at 44.
        (setf start (- data (* 2 channels frames)))
        (loop for frame below frames
              do (let ((sample (le16 octets (+ start (* frame 2 channels)))))
                   (setf (aref left frame) (/ (if (> sample 32767) (- sample 65536) sample) 32768.0d0))
                   (if (> channels 1)
                       (let ((second (le16 octets (+ start (* frame 2 channels) 2))))
                         (setf (aref right frame) (/ (if (> second 32767) (- second 65536) second) 32768.0d0)))
                       (setf (aref right frame) (aref left frame)))))
        (values left right rate)))))

(defun write-wav (path left right rate)
  "Write two float vectors as a 16-bit stereo wav. => PATH."
  (let* ((frames (length left))
         (data-bytes (* frames 4))
         (octets (make-array (+ 44 data-bytes) :element-type '(unsigned-byte 8) :initial-element 0)))
    (labels ((text (at string)
               (replace octets (map '(vector (unsigned-byte 8)) #'char-code string) :start1 at))
             (put (at number bytes)
               (loop for index below bytes
                     do (setf (aref octets (+ at index)) (ldb (byte 8 (* 8 index)) number))))
             (sample (value)
               (min 32767 (max -32768 (round (* 32767.0 (max -1.0 (min 1.0 value))))))))
      (text 0 "RIFF") (put 4 (+ 36 data-bytes) 4) (text 8 "WAVE")
      (text 12 "fmt ") (put 16 16 4) (put 20 1 2) (put 22 2 2) (put 24 rate 4)
      (put 28 (* rate 4) 4) (put 32 4 2) (put 34 16 2)
      (text 36 "data") (put 40 data-bytes 4)
      (loop for frame below frames
            do (put (+ 44 (* frame 4)) (sample (aref left frame)) 2)
               (put (+ 46 (* frame 4)) (sample (aref right frame)) 2))
      (ensure-directories-exist path)
      (with-open-file (out path :direction :output :if-exists :supersede
                                 :if-does-not-exist :create
                                 :element-type '(unsigned-byte 8))
        (write-sequence octets out))
      path)))

;;; --- the mix ------------------------------------------------------------------

(defun room-settings (room)
  (or (cdr (assoc room +rooms+ :test #'string=)) (cdr (assoc "room" +rooms+))))

(defun add-reverb (left right rate room)
  "A synthesised room: three feedback combs a side, scaled by the room's size
and rt60, mixed under the dry signal by its wet share. Deterministic."
  (let* ((config (room-settings room))
         (wet (float (getf config :wet) 1.0d0))
         (size (float (getf config :size) 1.0d0))
         (rt60 (float (getf config :rt60) 1.0d0))
         (delays (mapcar (lambda (ms) (max 1 (round (* ms 0.001 size rate))))
                         '(23.7 31.1 41.3)))
         (mix (copy-seq left))
         (mix-right (copy-seq right)))
    (loop for delay in delays
          for gain = (expt 0.001d0 (/ (* delay 1.0d0) (* rt60 rate)))
          do (loop for index from delay below (length left)
                   do (incf (aref mix index)
                            (* 0.5d0 (- 1.0d0 gain) (+ (aref left (- index delay))
                                                   (aref mix (- index delay)))))
                      (incf (aref mix-right index)
                            (* 0.5d0 (- 1.0d0 gain) (+ (aref right (- index delay))
                                                   (aref mix-right (- index delay)))))))
    (values (loop for index below (length left)
                  collect (+ (* (- 1.0d0 wet) (aref left index)) (* wet (aref mix index)))
                  into out
                  finally (return (coerce out 'vector)))
            (loop for index below (length right)
                  collect (+ (* (- 1.0d0 wet) (aref right index)) (* wet (aref mix-right index)))
                  into out
                  finally (return (coerce out 'vector))))))

(defun trim-to (vector seconds rate)
  (let ((frames (min (length vector) (round (* seconds rate)))))
    (subseq vector 0 (max 1 frames))))

(defun normalize-peak (left right target-db)
  (let* ((peak (max 1.0e-9 (loop for index below (length left)
                                 maximize (max (abs (aref left index)) (abs (aref right index))))))
         (gain (/ (expt 10.0d0 (/ (float target-db 1.0d0) 20.0d0)) peak)))
    (values (map 'vector (lambda (value) (* value gain)) left)
            (map 'vector (lambda (value) (* value gain)) right))))

(defun apply-fades (left right rate fade-in fade-out)
  (let ((in-frames (min (length left) (round (* rate fade-in))))
        (out-frames (min (length left) (round (* rate fade-out)))))
    (loop for index below in-frames
          do (let ((factor (expt (/ (float index 1.0d0) (max 1 in-frames)) 1.5d0)))
               (setf (aref left index) (* (aref left index) factor)
                     (aref right index) (* (aref right index) factor))))
    (loop for index below out-frames
          do (let* ((at (- (length left) 1 index))
                    (factor (expt (/ (float index 1.0d0) (max 1 out-frames)) 1.5d0)))
               (setf (aref left at) (* (aref left at) factor)
                     (aref right at) (* (aref right at) factor))))
    (values left right)))

;;; --- the render ---------------------------------------------------------------

(defun slug (text)
  "TEXT as a track folder name: letters, digits and single dashes, never empty."
  (let ((out '()) (dash t))
    (loop for char across (string-downcase (or text ""))
          while (< (length out) 64)
          do (cond ((or (alphanumericp char) (member char '(#\. #\_ #\-)))
                    (unless (and (char= char #\-) dash) (push char out))
                    (setf dash (char= char #\-)))
                   ((not dash) (push #\- out) (setf dash t))))
    (let ((text (string-trim "-._" (coerce (nreverse out) 'string))))
      (if (zerop (length text)) "track" text))))

(defun render-stem (part events directory index score)
  "One part rendered through fluidsynth: (values PATH FAILURE-OR-NIL)."
  (let* ((name (format nil "~a-~2,'0d-~a" (band-score-name score) index
                        (slug (band-part-inst part))))
         (midi (ensure-directories-exist (merge-pathnames (format nil "~a.mid" name) directory)))
         (wav (merge-pathnames (format nil "~a.wav" name) directory)))
    (write-midi-file midi score (list (part-track part events)))
    (let* ((soundfont (soundfont-path))
           (argv (list (or (setting :fluidsynth) "fluidsynth") "-ni"
                       "-F" (namestring wav)
                       "-r" (princ-to-string (or (setting :sample-rate) 44100))
                       "-O" "s16"
                       "-o" "synth.reverb.active=0"
                       "-o" "synth.chorus.active=0"
                       "-o" "synth.gain=0.85"
                       (or soundfont "") (namestring midi))))
      (multiple-value-bind (out err code) (funcall *run* argv)
        (declare (ignore out))
        (if (and (= code 0) (probe-file wav))
            (values wav nil)
            (values nil (format nil "fluidsynth failed on ~a (exit ~a): ~a"
                                (band-part-inst part) code
                                (subseq (or err "") 0 (min 300 (length (or err "")))))))))))

(defun render-track (score room &key (mp3 t) (lufs -14.0) (tail 1.4))
  "Render SCORE to a wav (and mp3) under the output folder. => PLIST."
  (let* ((problems (validate-score score))
         (soundfont (soundfont-path)))
    (when problems (fail "~{~a~^; ~}" problems))
    (unless (and soundfont (probe-file soundfont))
      (fail "no soundfont found: set hermes-band.soundfont to a General MIDI .sf2, ~
             or install one (apt install fluid-soundfont-gm)"))
    (unless (funcall *find-program* (or (setting :fluidsynth) "fluidsynth"))
      (fail "fluidsynth is not on PATH: install it (apt install fluidsynth) or set hermes-band.fluidsynth"))
    (let* ((name (slug (band-score-name score)))
           (directory (ensure-directories-exist
                       (merge-pathnames (format nil "~a/" name) (setting :output-dir))))
           (music (music-duration score))
           (rate (or (setting :sample-rate) 44100))
           (left nil) (right nil) (stems '()))
      (loop for (part . events) in (performance-parts score)
            for index from 0
            do (multiple-value-bind (wav failure)
                   (render-stem part events directory index score)
                 (when failure (fail "~a" failure))
                 (multiple-value-bind (stem-left stem-right stem-rate)
                     (read-wav wav)
                   (setf rate stem-rate)
                   (let* ((wav-left (trim-to stem-left (+ music tail) rate))
                          (wav-right (trim-to stem-right (+ music tail) rate))
                          (gain (* (band-part-gain part) (expt 10.0 (/ (band-part-level part) 20.0)))))
                     (cond ((null left)
                            (setf left (map 'vector (lambda (v) (* v gain)) wav-left)
                                  right (map 'vector (lambda (v) (* v gain)) wav-right)))
                           (t (let ((frames (min (length left) (length wav-left))))
                                (loop for i below frames
                                      do (incf (aref left i) (* gain (aref wav-left i)))
                                         (incf (aref right i) (* gain (aref wav-right i)))))))))
                 (push (file-namestring wav) stems)))
      (multiple-value-bind (wet-left wet-right) (add-reverb left right rate room)
        (setf left (trim-to wet-left (+ music tail) rate)
              right (trim-to wet-right (+ music tail) rate))
        (multiple-value-bind (faded-left faded-right) (apply-fades left right rate 0.04 1.2)
          (multiple-value-bind (final-left final-right) (normalize-peak faded-left faded-right -3.2)
            (let ((wav (merge-pathnames (format nil "~a.wav" name) directory))
                  (mp3-path (merge-pathnames (format nil "~a.mp3" name) directory)))
              (write-wav wav final-left final-right rate)
              (when lufs
                (let ((tmp (merge-pathnames (format nil "~a.tmp.wav" name) directory)))
                  (multiple-value-bind (out err code)
                      (funcall *run* (list (or (setting :ffmpeg) "ffmpeg") "-y" "-hide_banner"
                                           "-v" "error" "-i" (namestring wav)
                                           "-af" (format nil "loudnorm=I=~a:TP=-1:LRA=11" lufs)
                                           "-ar" (princ-to-string rate) (namestring tmp)))
                    (declare (ignore out))
                    (if (and (= code 0) (probe-file tmp))
                        (rename-file tmp wav)
                        (fail "ffmpeg loudnorm failed (exit ~a): ~a" code
                              (subseq (or err "") 0 (min 300 (length (or err "")))))))))
              (when mp3
                (multiple-value-bind (out err code)
                    (funcall *run* (list (or (setting :ffmpeg) "ffmpeg") "-y" "-hide_banner"
                                         "-v" "error" "-i" (namestring wav)
                                         "-c:a" "libmp3lame" "-b:a" "192k" "-ar" "44100"
                                         (namestring mp3-path)))
                  (declare (ignore out))
                  (unless (and (= code 0) (probe-file mp3-path))
                    (fail "ffmpeg mp3 failed (exit ~a): ~a" code
                          (subseq (or err "") 0 (min 300 (length (or err ""))))))))
              (list :wav (namestring wav)
                    :mp3 (if (and mp3 (probe-file mp3-path)) (namestring mp3-path) "")
                    :directory (namestring directory)
                    :music_duration music
                    :duration (* (length final-left) (/ 1.0 rate))
                    :stems (nreverse stems)))))))))
