;;;; cell-test.lisp --- the five tools, the receipts, and every refusal, with
;;;; ffmpeg and ffprobe stubbed.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Nothing here renders: NLE::*RUN* is a stub that records the argv and
;;;; answers handcrafted ffprobe JSON and ffmpeg diagnostics, and the files it
;;;; "writes" are bytes in a folder of this test's own under the scratch home.
;;;; What is pinned is what a user gets -- a probe read off the file's own
;;;; metadata, a cut whose argv is the plugin's, a frame for a phone, a burned
;;;; subtitle, an honest quality score -- and what a refused call never costs:
;;;; a missing render folder, a clip past the end of the source, a file that
;;;; is not a subtitle.

(in-package #:nodecode.test)

(define-test-slice "kinocut" "KINOCUT-CELL-" :start nodecode-kinocut:start-cell
  :defaults (("output_dir" (namestring (kinocut-scratch-directory)))))

;;; --- fixtures -------------------------------------------------------------------

(defvar *scratch* nil)

(defun kinocut-scratch-directory ()
  "A render folder of this test run's own, made once, so no two runs share
one and nothing outside the scratch home is touched."
  (or *scratch*
      (setf *scratch* (merge-pathnames (format nil "kinocut-~10,'0x/" (random (expt 16 10)))
                                        (nlk:home)))))

(defun kinocut-file (name text)
  (let ((path (merge-pathnames name (kinocut-scratch-directory))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (write-string text out))
    (namestify path)))

(defun namestify (path) (namestring path))

(defun kinocut-source ()
  "A file the tools accept as a video: the stubs never read its bytes."
  (kinocut-file "clip.mp4" "not really a video"))

(defun kinocut-subtitles (&optional text)
  (kinocut-file "clip.srt"
                (or text (format nil "1~%00:00:00,000 --> 00:00:02,000~%hello there~%"))))

(defun kinocut-probe-json (&key (duration "12.5") (width 1920) (height 1080) (audio t))
  "What ffprobe answers for a 1920x1080 clip, 29.97 fps, with or without audio."
  (format nil "{\"streams\":[{\"codec_type\":\"video\",\"codec_name\":\"h264\",\"width\":~d,\"height\":~d,\"avg_frame_rate\":\"30000/1001\",\"duration\":\"~a\"}~@[,{\"codec_type\":\"audio\",\"codec_name\":\"aac\",\"channels\":2,\"sample_rate\":\"48000\"}~]],\"format\":{\"duration\":\"~a\",\"size\":\"2048000\",\"format_name\":\"mov,mp4\"}}"
          width height duration (and audio "") duration))

;;; --- the stub -------------------------------------------------------------------

(defvar *calls* '())
(defvar *probe-answer* "{}")
(defvar *ffmpeg-answer* "")
(defvar *status* 0)
(defvar *touch* t)

(defun stub-file (path)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                             :if-does-not-exist :create
                             :element-type '(unsigned-byte 8))
    (write-sequence (make-array 4096 :element-type '(unsigned-byte 8) :initial-element 66) out)))

(defmacro with-ffmpeg-stub ((&key probe (answer "") (status 0) (touch t)) &body body)
  "BODY with both programs stubbed: ffprobe answers PROBE, ffmpeg answers
ANSWER and, with TOUCH, writes the file it was told to write. Every argv is
pushed on *CALLS*, newest first."
  `(let ((*calls* '())
         (*probe-answer* ,probe)
         (*ffmpeg-answer* ,answer)
         (*status* ,status)
         (*touch* ,touch)
         (nodecode-kinocut::*run*
           (lambda (argv)
             (push argv *calls*)
             (if (and (first argv) (search "ffprobe" (first argv)))
                 (values *probe-answer* 0)
                 (let ((last (car (last argv))))
                   (when (and *touch* (stringp last)
                              (or (find #\. last) (find #\/ last)))
                     (stub-file last))
                   (values *ffmpeg-answer* *status*))))))
     ,@body))

;;; --- reading what the stub was asked ----------------------------------------------

(defun calls-to (name)
  "Every stubbed call whose program name holds NAME, oldest first."
  (reverse (remove-if-not (lambda (argv) (search name (or (first argv) "") :test #'char-equal))
                          *calls*)))

(defun last-call-to (name)
  (car (last (calls-to name))))

(defun argument-after (flag argv)
  (second (member flag argv :test #'equal)))

(defun kinocut-tool (name &rest arguments)
  "Call the registered tool NAME the way the engine does: through its
advertised contract, so a member the schema does not carry is refused here."
  (let* ((entry (nle::find-tool name))
         (schema (second entry)))
    (unless entry (error "no tool named ~a is registered" name))
    (funcall (third entry)
             (nlk:contract-decode schema (apply #'nlk:make-json-object arguments)))))

;;; --- the lifecycle every cell proves ---------------------------------------------

(define-cell-lifecycle-tests "kinocut"
  (:config "output_dir" (namestring (kinocut-scratch-directory)))
  (:help :kino)
  (:command "kino")
  (:running (is (equal "medium" (nodecode-kinocut::setting :preset)) "the default x264 preset")
            (is (equal 20 (nodecode-kinocut::setting :crf)) "and quality")
            (driterion))
  (:refused ("crf" -1) ("preset" "ludicrous"))
  (:idle nodecode-kinocut:kinocut-error (nodecode-kinocut::output-directory)))

(defun driterion ()
  (dolist (name '("kino_probe" "kino_trim" "kino_reformat" "kino_captions" "kino_check"))
    (is-present (entry (nle::find-tool name))
      (format nil "~a is registered" name)
      (is (hash-table-p (nlk:contract-schema (second entry))) "against a declared contract"))))

;;; --- kino_probe -------------------------------------------------------------------

(deftest kinocut-cell-the-probe-reads-the-file-and-says-what-it-is ()
  (with-cell-stop ((kinocut-start))
    (with-ffmpeg-stub (:probe (kinocut-probe-json))
      (let* ((source (kinocut-source))
             (answer (kinocut-tool "kino_probe" "path" source))
             (argv (last-call-to "ffprobe")))
        (is (equal (list "ffprobe" "-v" "error" "-print_format" "json"
                         "-show_format" "-show_streams" source)
                   argv) "ffprobe is asked for the container and every stream")
        (is (search "12.50 s" answer) "the length, as a person reads it")
        (is (search "1920x1080" answer) "the frame")
        (is (search "29.97 fps" answer) "the rate, read off the rational ffprobe answers")
        (is (search "aac" answer) "the audio stream")
        (is (search "nothing was written" answer) "and it says the file was not touched")))))

;;; --- kino_trim --------------------------------------------------------------------

(deftest kinocut-cell-a-trim-asks-for-the-cut-the-plugin-asked-for ()
  (with-cell-stop ((kinocut-start))
    (with-ffmpeg-stub (:probe (kinocut-probe-json))
      (let* ((source (kinocut-source))
             (answer (kinocut-tool "kino_trim" "path" source "start" 2 "duration" 5))
             (argv (last-call-to "ffmpeg")))
        (is (equal (list "-ss" "2" "-i" source "-t" "5")
                   (subseq argv 6 12)) "the seek goes in before the input, the length after it")
        (is (equal "libx264" (argument-after "-c:v" argv)) "accurate mode re-encodes the picture")
        (is (equal "20" (argument-after "-crf" argv)) "at the section's quality")
        (is (equal "medium" (argument-after "-preset" argv)) "and speed")
        (is (search "saved " answer) "the answer names the file")
        (is (search (namestring (kinocut-scratch-directory)) answer) "inside the render folder")
        (is (search "the source is untouched" answer) "and never the source")
        (let ((receipt (car (last (nodecode-kinocut::receipts)))))
          (is (equal "trim" (nlk:json-value receipt :string "kind")) "one receipt, named for what it was")
          (is (equal source (nlk:json-value receipt :string "source")) "with its source"))))))

(deftest kinocut-cell-a-copy-trim-keeps-the-source-codecs ()
  (with-cell-stop ((kinocut-start))
    (with-ffmpeg-stub (:probe (kinocut-probe-json))
      (let* ((source (kinocut-source))
             (answer (kinocut-tool "kino_trim" "path" source "start" 0 "end" 3 "mode" "copy"))
             (argv (last-call-to "ffmpeg")))
        (is (equal (list "-c" "copy")
                   (subseq argv (- (length argv) 5)
                           (- (length argv) 3))) "a stream copy, not a re-encode")
        (is (search ".mp4" answer) "the file keeps the source's container")))))

(deftest kinocut-cell-a-clip-past-the-end-is-refused-before-anything-renders ()
  (with-cell-stop ((kinocut-start))
    (with-ffmpeg-stub (:probe (kinocut-probe-json :duration "4.0"))
      (let ((answer (kinocut-tool "kino_trim" "path" (kinocut-source) "start" 2 "duration" 5)))
        (is (search "runs past its end" answer) "the source's own length refuses the cut")
        (is (null (calls-to "ffmpeg")) "and nothing was rendered")))))

;;; --- kino_reformat ------------------------------------------------------------------

(deftest kinocut-cell-a-vertical-reformat-crops-the-middle-of-the-frame ()
  (with-cell-stop ((kinocut-start))
    (with-ffmpeg-stub (:probe (kinocut-probe-json))
      (kinocut-tool "kino_reformat" "path" (kinocut-source) "aspect" "9:16")
      (is (equal "crop=608:1080:656:0,scale=1080:1920"
                 (argument-after "-vf"
                                 (last-call-to "ffmpeg"))) "a centred 9:16 crop")
      (kinocut-tool "kino_reformat" "path" (kinocut-source) "aspect" "1:1" "fit" "pad")
      (is (equal "scale=1080:1080:force_original_aspect_ratio=decrease,pad=1080:1080:(ow-iw)/2:(oh-ih)/2:color=black"
                 (argument-after "-vf"
                                 (last-call-to "ffmpeg"))) "pad keeps the whole picture, adds bars"))))

;;; --- kino_captions ------------------------------------------------------------------

(deftest kinocut-cell-captions-burn-a-real-subtitle-file ()
  (with-cell-stop ((kinocut-start))
    (with-ffmpeg-stub (:probe (kinocut-probe-json))
      (let* ((subtitle (kinocut-subtitles))
             (answer (kinocut-tool "kino_captions" "path" (kinocut-source) "subtitles" subtitle))
             (filter (argument-after "-vf" (last-call-to "ffmpeg"))))
        (is (search "subtitles=filename=" filter) "libass burns the file in")
        (is (search (format nil "force_style='FontName=~a,FontSize=~d"
                            (nodecode-kinocut::setting :font-name)
                            (nodecode-kinocut::setting :font-size))
                    filter) "in the section's font and size")
        (is (search "saved " answer) "and the answer names the burned file"))
      (let ((answer (kinocut-tool "kino_captions" "path" (kinocut-source)
                                                "subtitles" (kinocut-subtitles "no cues here"))))
        (is (search "not a subtitle file" answer) "a file that is not subtitles is refused")
        (is (= 1 (length (calls-to "ffmpeg"))) "and no second render was attempted")))))

;;; --- kino_check -------------------------------------------------------------------

(deftest kinocut-cell-the-check-passes-a-clean-clip ()
  (with-cell-stop ((kinocut-start))
    (with-ffmpeg-stub (:probe (kinocut-probe-json)
                       :answer (format nil "mean_volume: -18.0 dB~%max_volume: -3.0 dB~%"))
      (let ((answer (kinocut-tool "kino_check" "path" (kinocut-source))))
        (is (search "PASS, 100/100" answer) "a readable, lit, audible clip passes whole")
        (is (search "no finding" answer) "with nothing to report")
        (is (search "look at the frames yourself" answer) "and the human review still named")))))

(deftest kinocut-cell-the-check-fails-a-mostly-black-clip ()
  (with-cell-stop ((kinocut-start))
    (with-ffmpeg-stub (:probe (kinocut-probe-json)
                       :answer (format nil "[blackdetect @ 0x1] black_start:0 black_end:8 black_duration:8~%max_volume: -20.0 dB~%"))
      (let ((answer (kinocut-tool "kino_check" "path" (kinocut-source))))
        (is (search "FAIL, 75/100" answer) "8 s of 12.5 s is over the 50% ceiling")
        (is (search "is black" answer) "and the finding says what it saw")))))

(deftest kinocut-cell-the-check-prices-silence-and-still-passes-it ()
  (with-cell-stop ((kinocut-start))
    (with-ffmpeg-stub (:probe (kinocut-probe-json) :answer (format nil "max_volume: -60.0 dB~%"))
      (let ((answer (kinocut-tool "kino_check" "path" (kinocut-source))))
        (is (search "80/100" answer) "silence costs twenty points")
        (is (search "PASS" answer) "and eighty is the section's pass mark")
        (is (search "silence" answer) "the finding is named")))))

;;; --- the section, and the command ----------------------------------------------------

(deftest kinocut-cell-a-missing-render-folder-refuses-every-render ()
  ;; Started on a section that names no folder at all: the shared defaults
  ;; every other test gets would answer for it.
  (with-cell-stop ((nodecode-kinocut:start-cell
                    (nlk:json-object "kinocut" (nlk:make-json-object))))
    (with-ffmpeg-stub (:probe (kinocut-probe-json))
      (let ((answer (kinocut-tool "kino_trim" "path" (kinocut-source) "start" 0 "duration" 1)))
        (is (search "kinocut.output_dir" answer) "the refusal names the setting to fill")
        (is (null (calls-to "ffmpeg")) "and nothing was rendered")))))

(deftest kinocut-cell-the-kino-command-reports-the-binaries-and-the-receipts ()
  (with-cell-stop ((kinocut-start))
    (with-ffmpeg-stub (:probe (kinocut-probe-json) :answer "ffmpeg version 6.1.1")
      (let ((answer (nodecode-kinocut::run-slash "" nil)))
        (is (search "ffmpeg version 6.1.1" answer) "what the binary says it is")
        (is (search "renders:" answer) "where renders land")
        (is (search "receipts:" answer) "and how many are kept")))))
