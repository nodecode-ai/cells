;;;; cell-test.lisp --- the note, the warnings, the tools, and every refusal,
;;;; with ffmpeg and ffprobe stubbed.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here runs ffmpeg: *RUN* records the argv and answers handcrafted
;;;; ffprobe JSON, ebur128 lines and raw grayscale frames, and the files it
;;;; "writes" are bytes in a folder of this test's own under the scratch
;;;; home. What is pinned is what a user gets -- a black clip flagged before
;;;; the model claims done, a good one described without a warning, a sound
;;;; that is silent said so, a file measured once -- and what a refusal says.

(in-package #:nodecode.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(define-test-slice "eyes" "EYES-CELL-" :start nodecode-eyes:start-cell
  :defaults (("sheet_dir" (namestring (eyes-scratch-directory)))))

;;; --- a folder of this run's own ---------------------------------------------------

(defvar *scratch* nil)

(defun eyes-scratch-directory ()
  "A folder of this test run's own, made once, so no two runs share one."
  (or *scratch*
      (setf *scratch* (merge-pathnames (format nil "eyes-~10,'0x/" (random (expt 16 10)))
                                        (nlk:home)))))

(defun eyes-file (name)
  "A file in the scratch folder, a byte a byte, which the stubs never read."
  (let ((path (merge-pathnames name (eyes-scratch-directory))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create
                              :element-type '(unsigned-byte 8))
      (write-sequence (make-array 16 :element-type '(unsigned-byte 8) :initial-element 7)
                      out))
    path))

;;; --- the stub -------------------------------------------------------------------

(defvar *calls* '())
(defvar *probe-answer* "{}")
(defvar *sound-answer* "")
(defvar *frames* '())
(defvar *sheet* t)

(defparameter +frame-bytes+ 9216 "96 x 96, the size a sampled frame is.")

(defun eyes-frame (level &optional other)
  "One sampled frame: every pixel LEVEL, or the left half LEVEL and the
right half OTHER."
  (let ((frame (make-array +frame-bytes+ :element-type '(unsigned-byte 8)
                                          :initial-element level)))
    (when other
      (loop for index from (/ +frame-bytes+ 2) below +frame-bytes+
            do (setf (aref frame index) other)))
    frame))

(defun write-octets (path octets)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                             :if-does-not-exist :create
                             :element-type '(unsigned-byte 8))
    (write-sequence octets out)))

(defun frames-octets (frames)
  (let ((bytes (make-array (* (length frames) +frame-bytes+)
                            :element-type '(unsigned-byte 8))))
    (loop for frame in frames
          for start from 0 by +frame-bytes+
          do (replace bytes frame :start1 start))
    bytes))

(defun eyes-run-stub (argv output)
  "The one seam, stubbed: ffprobe answers PROBE-ANSWER, a raw frame dump
writes FRAMES, ebur128 answers SOUND-ANSWER, and a contact sheet writes a
PNG nobody decodes."
  (push argv *calls*)
  (let ((line (format nil "~{~a~^ ~}" argv)))
    (cond ((search "ffprobe" (first argv) :test #'char-equal)
           (values *probe-answer* "" 0))
          ((search "rawvideo" line)
           (when output (write-octets output (frames-octets *frames*)))
           (values "" "" 0))
          ((search "ebur128" line)
           (values "" *sound-answer* 0))
          (t
           (when *sheet*
             (write-octets (pathname (car (last argv)))
                            (make-array 8 :element-type '(unsigned-byte 8) :initial-element 137)))
           (values "" "" 0)))))

(defmacro with-ffmpeg-stub ((&key (probe "{}") (sound "") frames (sheet t)) &body body)
  `(let ((*calls* '())
         (*probe-answer* ,probe)
         (*sound-answer* ,sound)
         (*frames* ,frames)
         (*sheet* ,sheet)
         (nodecode-eyes::*run* #'eyes-run-stub))
     ,@body))

(defun calls-for (needle)
  "Every stubbed call whose argv carries NEEDLE, oldest first."
  (reverse (remove-if-not (lambda (argv) (search needle (format nil "~{~a~^ ~}" argv)))
                          *calls*)))

(defun probe-json (&key (duration "3.0") (width 640) (height 360) (audio nil))
  (format nil "{\"format\":{\"duration\":\"~a\"},\"streams\":[{\"codec_type\":\"video\",\"width\":~d,\"height\":~d}~@[,{\"codec_type\":\"audio\"}~]]}"
          duration width height (and audio "")))

;;; --- calling what the cell registered ---------------------------------------------

(defun eyes-measure-call (path &key (sheet t))
  "Call eyes_measure the way the engine does: through its advertised
contract, so a member the schema does not carry is refused here."
  (let ((entry (nle::find-tool "eyes_measure")))
    (unless entry (error "eyes_measure is not registered"))
    (funcall (third entry)
             (nlk:contract-decode (second entry)
                                   (nlk:make-json-object "path" path "sheet" sheet)))))

(defun eyes-hook (tool-name text &optional arguments)
  "Run the cell's :TOOL hook over a tool whose answer is TEXT."
  (funcall (cell-hook "nodecode-eyes" :tool)
           (list :name tool-name
                 :arguments (or arguments (nlk:json-object))
                 :call-id "call-1")
           (lambda (op) (declare (ignore op)) text)))

;;; --- the lifecycle every cell proves ---------------------------------------------

(define-cell-lifecycle-tests "eyes"
  (:config)
  (:hooks :tool)
  (:help :eyes)
  (:command "eyes")
  (:running (is (equal 300 (nodecode-eyes::setting :fresh-seconds)) "a file is fresh for five minutes")
            (is (equal 3 (nodecode-eyes::setting :max-notes)) "at most three files a result")
            (is (equal '("eval") (nodecode-eyes::setting :tools)) "and only an eval's own answer is read")
            (is-present (entry (nle::find-tool "eyes_measure")) "eyes_measure is registered"
                        (is (not (null (second entry))) "against a declared contract"))
            (is-present (entry (nle::find-tool "eyes_status")) "eyes_status is registered" t))
  (:refused ("sheet_dir" nil) ("fresh_seconds" 0) ("max_notes" 0))
  (:idle nodecode-eyes:eyes-error (nodecode-eyes::status-text)
         (nodecode-eyes:measure "/nowhere/at-all.png")))

;;; --- the note the model reads -----------------------------------------------------

(deftest eyes-cell-a-black-video-is-flagged-before-the-model-claims-done ()
  (with-cell-stop ((eyes-start))
    (let ((video (eyes-file "promo.mp4")))
      (with-ffmpeg-stub (:probe (probe-json)
                          :frames (list (eyes-frame 0) (eyes-frame 0) (eyes-frame 0)))
        (let ((answer (eyes-hook "eval" (format nil "wrote ~a" video))))
          (is-carrying answer
            ("wrote" "the tool's own answer is kept first")
            ("[eyes] promo.mp4" "the note names the file")
            ("3.0 s, 640x360" "its length and frame, off ffprobe")
            ("frame filled 0%" "an empty canvas")
            ("moves in 0%" "and nothing moves")
            ("brightness 0/255" "in the dark")
            ("WARNING: almost black, nearly empty frame, barely moves"
             "one warning naming every fault")))))))

(deftest eyes-cell-a-good-clip-is-described-and-gets-no-warning ()
  (with-cell-stop ((eyes-start))
    (let ((video (eyes-file "clip.mp4")))
      (with-ffmpeg-stub (:probe (probe-json)
                          :frames (list (eyes-frame 0 200) (eyes-frame 200 0)
                                         (eyes-frame 0 200)))
        (let ((answer (eyes-hook "eval" (format nil "rendered ~a" video))))
          (is-carrying answer
            ("frame filled 50%" "half the frame is drawn")
            ("moves in 100% of half-seconds" "it moves")
            ("no sound track" "and has no audio")
            ("Contact sheet to look at:" "with a sheet to look at")
            (:absent "WARNING" "nothing to warn about"))
          (is (search "clip-" answer) "the sheet is named by the clip's own name")
          (is (equal 2 (length (calls-for "ffmpeg"))) "the frames and the sheet, and nothing else ran")
          (is (null (directory (merge-pathnames "frames-*.raw" (eyes-scratch-directory)))) "the frame dump is cleaned up"))))))

(deftest eyes-cell-one-write-is-measured-once-and-only-for-a-watched-tool ()
  (with-cell-stop ((eyes-start))
    (let ((video (eyes-file "once.mp4")))
      (with-ffmpeg-stub (:probe (probe-json) :frames (list (eyes-frame 0) (eyes-frame 0)))
        (let* ((text (format nil "wrote ~a" video))
               (first (eyes-hook "eval" text)))
          (is (search "[eyes] once.mp4" first) "the first result carries the note")
          (is (equal text (eyes-hook "eval" text)) "the second, of the same file, does not")
          (is (equal text (eyes-hook "web_search" text)) "and a tool the section does not watch is never read")
          (is (= 1 (length (calls-for "ffprobe"))) "one probe for the note, and no more"))))))

(deftest eyes-cell-an-old-file-is-left-alone ()
  (with-cell-stop ((eyes-start))
    (let* ((video (eyes-file "old.mp4"))
           (hour-ago (- (nlk:unix-now) 3600)))
      (sb-posix:utime video hour-ago hour-ago)
      (with-ffmpeg-stub (:probe (probe-json) :frames (list (eyes-frame 0) (eyes-frame 0)))
        (let ((text (format nil "wrote ~a" video)))
          (is (equal text (eyes-hook "eval" text)) "a file written an hour ago is not measured")
          (is (null (calls-for "ffprobe")) "so nothing ran"))))))

;;; --- the tools, and the operator's command ----------------------------------------

(deftest eyes-cell-the-measure-tool-says-what-an-image-is ()
  (with-cell-stop ((eyes-start))
    (with-ffmpeg-stub (:probe (probe-json :duration "0" :width 400 :height 400)
                        :frames (list (eyes-frame 255)))
      (let ((image (eyes-file "logo.png")))
        (is-carrying (answer (eyes-measure-call (namestring image)))
          ("400x400 image" "the frame, off ffprobe")
          ("frame filled 0%" "an unstamped canvas")
          ("brightness 255/255" "and all of it white")
          ("WARNING: almost white, nearly empty frame" "said as a warning"))))))

(deftest eyes-cell-sound-is-measured-for-loudness-and-silence ()
  (with-cell-stop ((eyes-start))
    (let ((voice (eyes-file "voice.wav")))
      (with-ffmpeg-stub (:probe (probe-json :duration "2.0")
                          :sound (format nil "    I:         -70.0 LUFS~% silence_start: 0~%"))
        (is-carrying (answer (eyes-measure-call (namestring voice)))
          ("voice.wav: 2.0 s of sound" "the length, off ffprobe")
          ("loudness -70.0 LUFS" "ebur128's own number")
          ("1 silence over 0.8 s" "and where the silence is")
          ("WARNING: effectively silent" "a voice-over nobody can hear")))
      (with-ffmpeg-stub (:probe (probe-json :duration "2.0")
                          :sound (format nil "    I:         -16.4 LUFS~%"))
        (is-carrying (answer (eyes-measure-call (namestring voice)))
          ("loudness -16.4 LUFS" "a voice at a normal level")
          (:absent "WARNING" "needs no warning"))))))

(deftest eyes-cell-a-file-that-is-not-there-is-refused-in-words ()
  (with-cell-stop ((eyes-start))
    (with-ffmpeg-stub ()
      (is-carrying (answer (refusal-text nodecode-eyes:eyes-error
                                          (eyes-measure-call "/nowhere/gone.mp4")))
        ("no file at /nowhere/gone.mp4" "the path it wanted")))))

(deftest eyes-cell-the-status-tool-and-eyes-say-what-the-cell-does ()
  (with-cell-stop ((eyes-start))
    (let ((video (eyes-file "status.mp4")))
      (with-ffmpeg-stub (:probe (probe-json) :frames (list (eyes-frame 0) (eyes-frame 0)))
        (is-carrying (answer (nodecode-eyes::status-text))
          ("ffmpeg and ffprobe" "the binaries it runs")
          ("measures a file written in the last 300 s" "the window")
          ("at most 3 a result" "and the cap")
          ("0 notes since start" "nothing measured yet"))
        (eyes-hook "eval" (format nil "wrote ~a" video))
        (is (search "1 note since start" (nodecode-eyes::status-text)) "one note was made")
        (is-carrying (answer (nodecode-eyes::run-slash (namestring video) "s1"))
          ("[eyes] status.mp4" "the command measures the path it was given")
          (:absent "Contact sheet" "words alone, without a sheet"))
        (is-carrying (answer (nodecode-eyes::run-slash "" "s1"))
          ("eyes: ffmpeg and ffprobe" "no path answers the status"))))))
