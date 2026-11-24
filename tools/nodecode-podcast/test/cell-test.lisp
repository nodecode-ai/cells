;;;; cell-test.lisp --- the cell's own wiring, offline.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The speech synthesizer, the voice list and the ffmpeg probe are stubs, so
;;;; no test dials the voice service or runs the mastering program: the episode
;;;; is joined from the clips the stub wrote.

(in-package #:nodecode.test)

(define-test-slice "podcast" "PODCAST-CELL-" :start nodecode-podcast:start-cell)

(defvar *episodes-dir* nil "A scratch episodes folder the tests' sections name.")

(defun episodes-dir ()
  (unless *episodes-dir*
    (let ((path (merge-pathnames "podcast-episodes/" (uiop:temporary-directory))))
      (setf *episodes-dir* (namestring (uiop:ensure-directory-pathname path)))))
  *episodes-dir*)

(defun section (&rest pairs)
  "A healthy section: a scratch folder, no chime, no ffmpeg, stubs for the seams."
  (append (list "episodes_dir" (episodes-dir) "intro_chime" nil
                "ffmpeg_path" "/nonexistent/ffmpeg")
          pairs))

(defun audio-clip (text voice rate pitch path)
  "A stub voice: an MP3-looking clip of the size a spoken line would have."
  (declare (ignore voice rate pitch))
  (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)
                            :if-exists :supersede :if-does-not-exist :create)
    (dotimes (index (* 60 (length text))) (write-byte (mod index 256) out))))

(defun voice-row (name locale gender personality)
  (nlk:json-object "ShortName" name "Locale" locale "Gender" gender
                    "VoiceTag" (nlk:json-object "VoicePersonalities" (vector personality))))

(defmacro with-stubbed-voices (&body body)
  "Every seam a test must not dial: the voice, its list, and the ffmpeg probe."
  `(let ((nodecode-podcast:*synth* #'audio-clip)
         (nodecode-podcast::*ffmpeg-probe* (lambda () nil))
         (nodecode-podcast:*fetch-voices*
           (lambda ()
             (list (voice-row "en-US-AndrewMultilingualNeural" "en-US" "Male" "Warm")
                   (voice-row "en-US-AvaMultilingualNeural" "en-US" "Female" "Bright")
                   (voice-row "es-ES-ElviraNeural" "es-ES" "Female" "")))))
     ,@body))

(defun script-args ()
  "A two-host script, as the model sends it: a decoded hash table."
  (nlk:decode-json
   (nlk:encode-json-object
    (nlk:json-object
     "title" "The week in open source"
     "description" "A short episode about one release."
     "lines" (vector
              (nlk:json-object "speaker" "Alex" "text" "Here is the story." "chapter" "Opening")
              (nlk:json-object "speaker" "Sam" "text" "And here is why it matters." "chapter" "Opening")
              (nlk:json-object "speaker" "Alex" "text" "That is all." "chapter" "Close"))))))

;;; --- lifecycle -------------------------------------------------------------------

(define-cell-lifecycle-tests "podcast"
  (:config "intro_chime" nil "ffmpeg_path" "/nonexistent/ffmpeg")
  (:help :podcast)
  (:command "podcast")
  (:running
   (is (nle::find-tool "podcast_produce") "podcast_produce is registered")
   (is (nle::find-tool "podcast_voices") "podcast_voices is registered")
   (is (nle::find-tool "podcast_episodes") "podcast_episodes is registered"))
  (:stopped
   (is (null (nle::find-tool "podcast_produce")) "stop unregisters the tools"))
  (:refused
   ;; a setting of the wrong shape refuses at start, and names the key
   ("pause_ms" 0)
   ("max_lines" "many")))

;;; --- the tools ---------------------------------------------------------------------

(deftest podcast-cell-produce-writes-an-episode (with-stubbed-voices)
  (with-cell-stop ((apply #'podcast-start (section)))
    (let* ((answer (nlk:decode-json (nodecode-podcast:produce-tool (script-args))))
           (transcript (gethash "transcript" answer)))
      (is (eql t (gethash "success" answer)) "the episode was produced")
      (is (probe-file (gethash "audio" answer)) "the MP3 is on disk")
      (is (probe-file transcript) "the timestamped transcript is on disk")
      (is (search "Opening" (uiop:read-file-string transcript)) "the chapter headings reached the transcript")
      (is (search "Sam" (uiop:read-file-string transcript)) "both hosts are named in it")
      (is (null (gethash "mastered" answer)) "without ffmpeg the answer says it was joined unmastered"))))

(deftest podcast-cell-a-bad-script-is-answered-not-raised (with-stubbed-voices)
  (with-cell-stop ((apply #'podcast-start (section)))
    (let* ((args (script-args))
           (copy (nlk:copy-json-object args))
           (answer nil))
      (setf (gethash "lines" copy) (vector (nlk:json-object "speaker" "Nobody" "text" "hello")))
      (setf answer (nlk:decode-json (nodecode-podcast:produce-tool copy)))
      (is (null (gethash "success" answer)) "the refusal is an answer, not a condition")
      (is (search "Nobody" (gethash "error" answer)) "it names the speaker that is not a host")
      (is (stringp (gethash "hint" answer)) "and says how to fix it"))))

(deftest podcast-cell-voices-and-episodes-answer (with-stubbed-voices)
  (with-cell-stop ((apply #'podcast-start (section)))
    (let ((args (nlk:make-json-object)))
      (setf (gethash "locale" args) "en-US")
      (let ((voices (nlk:decode-json (nodecode-podcast:voices-tool args))))
        (is (eql 2 (gethash "count" voices)) "the locale filter kept the two en-US voices")
        (is (search "Andrew" (nlk:encode-json-object (gethash "voices" voices))) "the voices are named")))
    (nodecode-podcast:produce-tool (script-args))
    (let* ((listing (nlk:decode-json (nodecode-podcast:episodes-tool (nlk:make-json-object))))
           (first-row (aref (gethash "episodes" listing) 0)))
      (is (plusp (gethash "count" listing)) "the episode just produced is listed")
      (is (probe-file (gethash "audio" first-row)) "with its audio on disk"))))
