;;;; cell-test.lisp --- the four tools, the refused section, and a render with
;;;; fluidsynth and ffmpeg stubbed.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here runs a program: NODEcode-BAND::*RUN* writes the wav a real
;;;; fluidsynth would and an empty file where ffmpeg's output goes, and
;;;; *FIND-PROGRAM* answers a path that exists. What is pinned is what a user
;;;; gets -- a preset list, a validated score, a rendered track with its stems
;;;; and its exact duration -- and what a broken host is told.

(in-package #:nodecode.test)

(define-test-slice "hermes-band" "HERMES-BAND-CELL-" :start nodecode-hermes-band:start-cell)

(define-cell-lifecycle-tests "hermes-band"
  (:config "output_dir" (namestring (nlk:home "band-cell-test/")))
  (:help :hermes-band)
  (:command "band")
  (:running
   (is (search "band-cell-test" (format nil "~a" (nodecode-hermes-band::setting :output-dir))) "the section's output folder is what a render uses")
   (let ((entry (nle::find-tool "band_render")))
     (is-present entry "the plugin's render tool is registered under its name"
       (is (equal "band_render" (first entry))))))
  (:stopped
   (is (null (nle::find-tool "band_render")) "stop takes the tools back out")
   (is (null (nle::find-tool "band_doctor"))))
  (:refused ("sample_rate" 0))
  (:idle nodecode-hermes-band::hermes-band-error (nodecode-hermes-band::doctor-tool nil)))

;;; --- fixtures -------------------------------------------------------------------

(defun band-temp (name)
  "A folder of this test's own under the scratch home."
  (ensure-directories-exist (merge-pathnames name (nlk:home))))

(defun band-soundfont ()
  "A file that stands in for a General MIDI soundfont."
  (let ((path (merge-pathnames "test.sf2" (band-temp "band-fixtures/"))))
    (ensure-directories-exist path)
    (unless (probe-file path)
      (with-open-file (out path :direction :output :if-exists :supersede)
        (write-line "stand-in soundfont" out)))
    path))

(defun band-call (name &rest pairs)
  "The tool NAME called the way the engine calls it: the contract decoded, or
the raw hash for a tool that takes nothing."
  (let ((entry (nle::find-tool name)))
    (unless entry (error "no tool ~a" name))
    (funcall (third entry)
             (if (typep (second entry) 'nlk:contract)
                 (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs))
                 (apply #'nlk:make-json-object pairs)))))

(defun band-tone (path)
  "Two seconds of a 220 Hz tone where a stem would be, as a 16-bit wav."
  (let* ((rate 44100) (frames (* rate 2))
         (left (make-array frames :element-type 'double-float))
         (right (make-array frames :element-type 'double-float)))
    (loop for index below frames
          do (let ((value (* 0.4d0 (sin (/ (* 2.0d0 pi 220.0d0 index) (float rate 1.0d0))))))
               (setf (aref left index) value (aref right index) value)))
    (ensure-directories-exist path)
    (nodecode-hermes-band::write-wav path left right rate)))

(defun band-fake-run (argv)
  "The stubbed runner: -F gets a real wav; every other tool's output file is
created empty, whatever ffmpeg would have written there."
  (let ((out (car (last argv))))
    (cond ((member "-F" argv :test #'string=)
           (band-tone (nth (1+ (position "-F" argv :test #'string=)) argv)))
          (t (ensure-directories-exist out)
             (with-open-file (stream out :direction :output :if-exists :supersede
                                          :if-does-not-exist :create)
               (write-line "stub" stream))))
    (values "" "" 0)))

(defmacro with-band-host (&body body)
  "BODY with a host whose binaries all exist and whose programs are stubs."
  `(let ((nodecode-hermes-band::*find-program*
           (lambda (name) (declare (ignore name)) "/usr/local/bin/tool"))
         (nodecode-hermes-band::*run* #'band-fake-run))
     ,@body))

(defmacro with-band-cell ((&key (soundfont '(band-soundfont))) &body body)
  "BODY with the cell started on a folder of its own."
  `(let* ((directory (band-temp (format nil "band-out-~12,'0x/" (random (expt 16 10)))))
          (started (hermes-band-start "output_dir" (namestring directory)
                               "soundfont" (namestring ,soundfont))))
     (nlk:with-cleanup ((progn (funcall started)
                               (uiop:delete-directory-tree directory :validate t
                                                            :if-does-not-exist :ignore)))
       ,@body)))

;;; --- what the tools answer --------------------------------------------------------

(deftest hermes-band-cell-lists-its-presets-and-their-numbers ()
  (with-band-cell ()
    (let ((answer (band-call "band_list_presets")))
      (dolist (name '("bed" "band" "pulse" "funk"))
        (is (search name answer) (format nil "the ~a preset is offered" name)))
      (is (search "room" answer) "and the rooms it can render in"))))

(deftest hermes-band-cell-doctor-names-what-this-host-is-missing ()
  (with-band-cell ()
    (let ((nodecode-hermes-band::*find-program* (lambda (name) (declare (ignore name)) nil)))
      (let ((answer (band-call "band_doctor")))
        (is (search "ready: no" answer) "an empty host is not ready")
        (is (search "MISSING" answer) "and the answer names each missing piece")
        (is (search "fluidsynth" answer) "starting with the synthesizer")
        (is (search "soundfont" answer) "and the soundfont that is a required setting")))
    (with-band-host
      (let ((answer (band-call "band_doctor")))
        (is (search "ready: yes" answer) "a host with the toolchain is ready")))))

(deftest hermes-band-cell-validates-a-score-and-refuses-a-bad-part ()
  (with-band-cell ()
    (let ((good (band-call "band_validate_score"
                           "score" (nlk:make-json-object
                                    "name" "checks" "bpm" 100 "bars" 4
                                    "parts" (vector (nlk:make-json-object
                                                     "inst" "steel" "role" "fingerpick"
                                                     "pattern" "folk"))))))
      (is (search "ok:" good) "a complete score is accepted")
      (is (search "checks" good) "and described by name"))
    (let ((bad (band-call "band_validate_score"
                          "score" (nlk:make-json-object
                                   "bars" 4
                                   "parts" (vector (nlk:make-json-object
                                                    "inst" "steel" "role" "shred"))))))
      (is (search "invalid" bad) "an unknown role is refused")
      (is (search "parts[0]" bad) "naming the part that is wrong")
      (is (search "shred" bad) "and the value it carries"))))

(deftest hermes-band-cell-render-writes-a-track-its-stems-and-an-mp3 ()
  (with-band-cell ()
    (with-band-host
      (let* ((answer (band-call "band_render"
                                 "score" (nlk:make-json-object
                                          "name" "test-tune" "bpm" 120 "bars" 2
                                          "parts" (vector (nlk:make-json-object
                                                           "inst" "steel" "role" "block")))))
             (folder (merge-pathnames "test-tune/" (nodecode-hermes-band::setting :output-dir)))
             (files (uiop:directory-files folder)))
        (is (search "rendered test-tune" answer) "the answer names the track")
        (is (search ".wav" answer) "and the file it wrote")
        (is (search ".mp3" answer) "with the mp3 beside it")
        (is (search "duration 2.00 s" answer) "and the length it really is")
        (is (search "4.00 s of music" answer) "next to the score's musical length")
        (is-present (probe-file (merge-pathnames "test-tune.wav" folder))
          "the wav is there" (is t))
        (is-present (probe-file (merge-pathnames "test-tune.mp3" folder))
          "the mp3 is there too")
        (is-present (find ".mid" (mapcar #'file-namestring files) :test #'search)
          "one midi file per part was written for fluidsynth")))))

(deftest hermes-band-cell-render-refuses-without-a-score-or-a-soundfont ()
  (with-band-cell ()
    (with-band-host
      (let ((answer (band-call "band_render")))
        (is (search "ERROR:" answer) "a call with no music answers a failure")
        (is (search "preset" answer) "naming the ways to call it")))
    (with-band-cell (:soundfont (merge-pathnames "nowhere.sf2" (nlk:home)))
      (with-band-host
        (let ((answer (band-call "band_render" "preset" "bed")))
          (is (search "no soundfont found" answer) "a missing required setting is refused")
          (is (search "hermes-band.soundfont" answer) "naming the setting to fill")
          (is (search "fluid-soundfont-gm" answer) "and the package that provides one"))))))

(deftest hermes-band-cell-theory-voices-a-progression-by-moving-least ()
  (let* ((first (nodecode-hermes-band::voice-lead "D" "major" "I" nil))
         (second (nodecode-hermes-band::voice-lead "D" "major" "V" first)))
    (is (>= (length first) 3) "at least three voices")
    (is (< (abs (- (first (sort (copy-list second) #'<)) (first (sort (copy-list first) #'<)))) 24) "the next chord stays near the one before it")))
