;;;; media.lisp --- ffprobe in, ffmpeg in, a measured note out.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here is a service: every call runs the ffmpeg the operator
;;;; already has, on a file the operator already has, and reads the numbers
;;;; back. Three measurements, the plugin's own:
;;;;
;;;;   filled    the share of pixels that differ clearly from the frame's
;;;;             own median brightness -- a frame 0% filled is an empty canvas;
;;;;   moves     the share of half-seconds whose frame differs from the one
;;;;             before it by more than noise -- 0% is a frozen clip;
;;;;   sound     the integrated loudness and the silences over 0.8 s, and for
;;;;             a video a contact sheet of eight frames to look at.
;;;;
;;;; Frames are read as 96x96 grayscale bytes, ffmpeg writing them to a file
;;;; of the cell's own folder, so no PNG decoder is needed and nothing reads
;;;; pixels in Lisp it did not ask ffmpeg for.

(in-package #:nodecode-eyes)

;;; --- what is measured, and the words ----------------------------------------------

(defparameter +width+ 96 "How wide a frame is sampled, in pixels.")
(defparameter +height+ 96 "How tall a frame is sampled, in pixels.")

(defparameter +still-extensions+ '("png" "jpg" "jpeg" "webp")
  "A file with one of these is measured as a still: one frame, filled and bright.")

(defparameter +sound-extensions+ '("wav" "mp3" "ogg" "m4a" "flac")
  "A file with one of these is measured as sound alone: loudness and silences.")

(defparameter +media-extensions+
  (append +still-extensions+ +sound-extensions+
          '("mp4" "webm" "mov" "mkv" "gif"))
  "Every extension a note is written for. A tool result naming anything else
is not looked at, however recently it was written.")

(defparameter +path-characters+ "/._-@+%~"
  "What may continue a path the text names; a path starts at a slash or at ~/.")

(defparameter +path-tail+ ".,;:)]}'\""
  "What a sentence puts after a path and the path does not own.")

(defparameter *framing* "[eyes] ~a"
  "How one note opens, over the file's own name. Data, so a layer can reword it.")

;;; --- the one seam that runs a program ---------------------------------------------

(defun run-argv (argv output)
  "Run ARGV: (values STDOUT STDERR STATUS). With OUTPUT a pathname the
program's stdout is written there as bytes, so a raw frame dump needs no
string; without, it is read back as text. A program that cannot start
answers status -1 and the condition's own text on stderr, so the caller can
say why nothing was measured."
  (handler-case
      (multiple-value-bind (out err status)
          (if output
              (uiop:run-program argv :output output :error-output :string
                                :ignore-error-status t :timeout (run-timeout))
              (uiop:run-program argv :output :string :error-output :string
                                :ignore-error-status t :timeout (run-timeout)))
        (values (if (stringp out) out "") (or err "") (or status 0)))
    (error (condition) (values "" (princ-to-string condition) -1))))

(defvar *run* #'run-argv
  "The one seam that runs a program: (ARGV OUTPUT) => (values STDOUT STDERR
STATUS). Tests replace it; nothing else does.")

(defun call-run (argv &optional output)
  (funcall *run* argv output))

(defun run-timeout ()
  "One call's ceiling, read off the section while the cell runs."
  (if *eyes* (max 1 (setting :timeout-seconds)) 25))

;;; --- the binaries -------------------------------------------------------------

(defun ffmpeg-binary ()
  (let ((named (nlk:trimmed (or (setting :ffmpeg) ""))))
    (if (plusp (length named)) named "ffmpeg")))

(defun ffprobe-binary ()
  "The ffprobe to run: the section's, else one beside ffmpeg, else the
bare name, which is where an install puts it."
  (let ((named (nlk:trimmed (or (setting :ffprobe) ""))))
    (if (plusp (length named))
        named
        (let ((ffmpeg (ffmpeg-binary)))
          (if (find #\/ ffmpeg)
              (concatenate 'string (subseq ffmpeg 0 (1+ (position #\/ ffmpeg :from-end t))) "ffprobe")
              "ffprobe")))))

;;; --- small readings -----------------------------------------------------------

(defun to-number (value)
  "VALUE as a number, or NIL: ffprobe answers every duration as a string."
  (typecase value
    (number value)
    (string (let ((text (nlk:trimmed value)))
              (and (plusp (length text))
                   (let ((*read-eval* nil))
                     (ignore-errors (read-from-string text))))))
    (t nil)))

(defun json-get (object &rest path)
  "The value at PATH in decoded JSON OBJECT, or NIL wherever the walk stops."
  (let ((value object))
    (dolist (key path value)
      (setf value (and (hash-table-p value) (gethash key value))))))

(defun base-name (path)
  (or (ignore-errors (file-namestring path)) (namestring path)))

(defun extension (path)
  "PATH's extension, lowercased, or an empty string."
  (let* ((name (base-name path))
         (dot (position #\. name :from-end t)))
    (if dot (string-downcase (subseq name (1+ dot))) "")))

(defun expand-path (text)
  "TEXT with a leading ~/ taken to the home directory, as it was written."
  (if (and (>= (length text) 2) (char= (char text 0) #\~) (char= (char text 1) #\/))
      (concatenate 'string (namestring (user-homedir-pathname)) (subseq text 2))
      text))

;;; --- which paths a text names --------------------------------------------------

(defun path-character-p (character)
  (or (alphanumericp character) (find character +path-characters+ :test #'char=)))

(defun trim-tail (text)
  "TEXT without the punctuation a sentence puts after a path."
  (let ((end (length text)))
    (loop while (and (plusp end) (find (char text (1- end)) +path-tail+ :test #'char=))
          do (decf end))
    (subseq text 0 end)))

(defun media-path-p (text)
  (member (extension text) +media-extensions+ :test #'string=))

(defun scan-paths (text)
  "Every image, video or sound path TEXT names, in the order it names them."
  (let ((found '())
        (length (length text))
        (index 0))
    (loop while (< index length) do
      (let* ((character (char text index))
             (home (and (char= character #\~) (< (1+ index) length)
                        (char= (char text (1+ index)) #\/)))
             (slash (and (char= character #\/)
                         (or (zerop index)
                             (not (path-character-p (char text (1- index))))))))
        (if (or home slash)
            (let ((start index))
              (loop while (and (< index length) (path-character-p (char text index)))
                    do (incf index))
              (let ((candidate (trim-tail (subseq text start index))))
                (when (and (media-path-p candidate)
                           (not (member candidate found :test #'string=)))
                  (push candidate found))))
            (incf index))))
    (nreverse found)))

(defun argument-text (arguments)
  "Every string a tool call carries, one a line, so a path named only in the
arguments is seen as a result that names it."
  (with-output-to-string (out)
    (labels ((walk (value depth)
               (when (< depth 4)
                 (cond ((stringp value)
                        (write-string value out)
                        (terpri out))
                       ((hash-table-p value)
                        (maphash (lambda (key item)
                                   (declare (ignore key))
                                   (walk item (1+ depth)))
                                 value))
                       ((vectorp value)
                        (loop for item across value do (walk item (1+ depth))))))))
      (walk arguments 0))))

;;; --- the frame, measured ---------------------------------------------------------

(defun median-brightness (frame)
  "The frame's lower median brightness, off a 256-bucket count."
  (let ((counts (make-array 256 :element-type 'fixnum :initial-element 0)))
    (loop for value across frame do (incf (aref counts value)))
    (let ((target (ceiling (length frame) 2)))
      (loop for value from 0 below 256
            do (decf target (aref counts value))
            when (<= target 0) return value
            finally (return 255)))))

(defun filled-share (frame)
  "The share of pixels that differ clearly from the frame's own background,
which is its median brightness: what is drawn, against what is not."
  (let ((median (median-brightness frame)))
    (/ (loop for value across frame count (> (abs (- value median)) 18))
       (max (length frame) 1))))

(defun mean-brightness (frame)
  (/ (loop for value across frame sum value) (max (length frame) 1)))

(defun moved-p (before after)
  "Whether the picture changed more than noise between two samples."
  (> (/ (loop for a across before
              for b across after
              sum (abs (- a b)))
        (max (length before) 1))
     1.5))

;;; --- ffprobe, ffmpeg, and what they answer ---------------------------------------

(defun probe (path)
  "VALUES DURATION WIDTH HEIGHT HAS-AUDIO, off ffprobe's own JSON."
  (let* ((argv (list (ffprobe-binary) "-v" "error" "-show_entries"
                      "format=duration:stream=codec_type,width,height"
                      "-of" "json" (namestring path)))
         (data (ignore-errors (nlk:decode-json (call-run argv))))
         (streams (json-get data "streams"))
         (video (and (vectorp streams)
                     (find-if (lambda (stream)
                                (equal "video" (json-get stream "codec_type")))
                              streams)))
         (audio (and (vectorp streams)
                     (some (lambda (stream)
                             (equal "audio" (json-get stream "codec_type")))
                           streams))))
    (values (or (to-number (json-get data "format" "duration")) 0)
            (to-number (json-get video "width"))
            (to-number (json-get video "height"))
            (and audio t))))

(defun sheet-directory ()
  "The folder the contact sheets and the frame dumps land in, made if it is
not there; NIL while the section names none."
  (let ((named (setting :sheet-dir)))
    (when named
      (let ((path (uiop:ensure-directory-pathname named)))
        (ensure-directories-exist path)
        path))))

(defun read-octets (path)
  "PATH's bytes, or an empty vector when it is not there."
  (let ((file (probe-file path)))
    (if (null file)
        (make-array 0 :element-type '(unsigned-byte 8))
        (with-open-file (in file :element-type '(unsigned-byte 8))
          (let ((bytes (make-array (file-length in) :element-type '(unsigned-byte 8))))
            (read-sequence bytes in)
            bytes)))))

(defun frames (path fps)
  "Grayscale +WIDTH+ x +HEIGHT+ frames of PATH, at most 240, sampled FPS
frames a second (NIL for the single frame of a still)."
  (let* ((filter (concatenate 'string
                               (if fps (format nil "fps=~a," fps) "")
                               (format nil "scale=~d:~d" +width+ +height+)))
         (dump (merge-pathnames (format nil "frames-~10,'0x.raw" (random (expt 16 10)))
                                 (sheet-directory)))
         (argv (list (ffmpeg-binary) "-v" "error" "-i" (namestring path)
                     "-vf" filter "-frames:v" "240" "-f" "rawvideo"
                     "-pix_fmt" "gray" (namestring dump))))
    (unwind-protect
         (progn (call-run argv dump)
                (let ((bytes (read-octets dump))
                      (size (* +width+ +height+)))
                  (loop for start from 0 below (length bytes) by size
                        while (<= (+ start size) (length bytes))
                        collect (subseq bytes start (+ start size)))))
      (ignore-errors (delete-file dump)))))

(defun last-lufs (text)
  "The integrated loudness ffmpeg's ebur128 printed last, or NIL."
  (let ((found nil)
        (from 0))
    (loop
      (let ((hit (search " LUFS" text :start2 from)))
        (unless hit (return found))
        (let* ((window (max 0 (- hit 24)))
               (at (search "I:" text :start2 window :end2 hit)))
          (when at
            (let ((value (to-number (subseq text (+ at 2) hit))))
              (when value (setf found value)))))
        (setf from (1+ hit))))))

(defun sound (path)
  "VALUES LOUDNESS SILENCES, off ffmpeg's own ebur128 and silencedetect."
  (let ((argv (list (ffmpeg-binary) "-nostats" "-i" (namestring path)
                     "-af" "ebur128=framelog=quiet,silencedetect=n=-45dB:d=0.8"
                     "-f" "null" "-")))
    (multiple-value-bind (out err status) (call-run argv)
      (declare (ignore status))
      (let ((text (concatenate 'string (or out "") (or err ""))))
        (values (last-lufs text)
                (loop with start = 0
                      for hit = (search "silence_start" text :start2 start)
                      while hit count 1 do (setf start (1+ hit))))))))

(defun sheet (path duration)
  "A contact sheet of eight frames of PATH in the cell's folder, or NIL."
  (let* ((step (max (/ duration 8.0) 0.25))
         (out (merge-pathnames
               (format nil "~a-~6,'0x.png"
                       (let ((name (string-downcase (or (pathname-name path) "clip"))))
                         (subseq name 0 (min 40 (length name))))
                       (logand (sxhash (namestring path)) #xffffff))
               (sheet-directory)))
         (argv (list (ffmpeg-binary) "-v" "error" "-y" "-i" (namestring path)
                     "-vf" (format nil "fps=1/~,3f,scale=180:-1,tile=8x1" step)
                     "-frames:v" "1" (namestring out))))
    (call-run argv)
    (and (probe-file out) (namestring out))))

;;; --- the note the model reads ----------------------------------------------------

(defun fault-line (fill light motion)
  "One line naming what is wrong with the picture, or an empty string."
  (let ((wrong '()))
    (cond ((< light 12) (push "almost black" wrong))
          ((> light 243) (push "almost white" wrong)))
    (when (< fill 0.05) (push "nearly empty frame" wrong))
    (when (and motion (< motion 0.1)) (push "barely moves" wrong))
    (if wrong
        (format nil " WARNING: ~{~a~^, ~}." (nreverse wrong))
        "")))

(defun percent (share)
  (round (* share 100.0)))

(defun note-for-still (name width height fill light)
  (format nil "~a~a: ~@[~dx~d image; ~]frame filled ~d% (the rest is background); brightness ~d/255.~a~%"
          (format nil *framing* "") name width height (percent fill) (round light)
          (fault-line fill light nil)))

(defun note-for-sound (name duration lufs silences)
  (format nil "~a~a: ~,1f s of sound; loudness ~:[?~;~,1f~] LUFS; ~d silence~:p over 0.8 s.~a~%"
          (format nil *framing* "") name duration lufs lufs silences
          (if (or (null lufs) (< lufs -50)) " WARNING: effectively silent." "")))

(defun note-for-video (name duration width height fill motion light lufs silences sheet)
  (format nil "~a~a: ~,1f s, ~@[~dx~d; ~]frame filled ~d% on the median frame; something moves in ~d% of half-seconds; brightness ~d/255~a.~@[ Contact sheet to look at: ~a.~]~a~%"
          (format nil *framing* "") name duration width height (percent fill) (percent motion)
          (round light)
          (if lufs
              (format nil "; loudness ~,1f LUFS, ~d silence~:p over 0.8 s" lufs silences)
              "; no sound track")
          sheet
          (fault-line fill light motion)))
(defun see (path &key (sheet t))
  "The measured note for PATH, or NIL when it cannot be measured at all."
  (let* ((name (base-name path))
         (ext (extension path)))
    (multiple-value-bind (duration width height audio) (probe path)
      (cond
        ((member ext +still-extensions+ :test #'string=)
         (let ((first-frame (first (frames path nil))))
           (when first-frame
             (note-for-still name width height
                             (filled-share first-frame)
                             (mean-brightness first-frame)))))
        ((or (member ext +sound-extensions+ :test #'string=) (null width))
         (multiple-value-bind (lufs silences) (sound path)
           (note-for-sound name duration lufs silences)))
        (t
         (let ((sampled (frames path "2")))
           (when (>= (length sampled) 2)
             (let* ((motion (/ (loop for (before after) on sampled
                                      while after count (moved-p before after))
                               (1- (length sampled))))
                    (fill (/ (loop for frame in sampled sum (filled-share frame))
                             (length sampled)))
                    (light (/ (loop for frame in sampled sum (mean-brightness frame))
                              (length sampled))))
               (multiple-value-bind (lufs silences)
                   (if audio (sound path) (values nil 0))
                 (note-for-video name duration width height fill motion light
                                 lufs silences
                                 (and sheet (sheet path duration))))))))))))

;;; --- the one verb ---------------------------------------------------------------

(defun measure (path &key (sheet t))
  "PATH's measured note, or a refusal naming what could not be measured."
  (running-settings)
  (let* ((named (nlk:trimmed (if (stringp path) path (princ-to-string (or path "")))))
         (file (and (plusp (length named)) (probe-file (expand-path named)))))
    (unless file
      (fail "no file at ~a; give the full path of an image, video or sound file"
            (if (plusp (length named)) named "an empty path")))
    (or (see file :sheet sheet)
        (fail "~a could not be measured: ffmpeg read no frame from it, or it is not a media file"
              (namestring file)))))
