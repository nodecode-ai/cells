;;;; cell.lisp --- four tools over the engine, the section, and the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin this rewrites gave the model four tools -- band_render,
;;;; band_validate_score, band_list_presets, band_doctor -- and this cell keeps
;;;; their names and their arguments. Nothing is dialled and nothing is billed:
;;;; a score is JSON in, and a wav and an mp3 of an exact length come out of
;;;; fluidsynth and ffmpeg on this machine.

(in-package #:nodecode-hermes-band)

(defparameter +primer+
  "The band cell composes and renders original music on this machine: no music API, no
model, no key, and the same score always renders the same audio.
  band_doctor      check the host first: fluidsynth, a General MIDI soundfont and ffmpeg.
  band_list_presets  the four bundled arrangements (bed, band, pulse, funk) and the rooms.
  band_validate_score  check a score without rendering it: instruments, roles, patterns,
      registers, tempo and bar count. Refusals name the part and the field.
  band_render      a score (inline object or score_path) or a preset becomes a mastered wav
      and mp3 in the output folder, one stem per part, exact length = bars * 4 beats at
      the tempo. It answers the file paths, the duration and the peak.
A score names bpm, key, mode, bars, progression (roman numerals such as I, vi, bVImaj7,
im7) and one part per player: inst, role, register [low high], start_bar, and optionally
pattern, gain, voices, jitter, swing and a mix object with pan, level and send.
Roles: fingerpick strum block bass_root5 bass_walk arp melody sparkle drums funk_guitar
slap_bass stab. Fingering patterns: folk travis sparse arp_up rolling oompah. Drum
patterns: folk_soft folk_full pulse heartbeat boom_bap funk. Rooms: dry close room hall
cathedral. Renders run in the foreground and take seconds to tens of seconds at the
default 44.1 kHz; a twelve-bar bed at 90 BPM is 32 seconds of audio. Tell the operator
the wav path, since the file plays anywhere and can be cut against picture."
  "What (help :band) answers while the cell runs.")

;;; --- the boundaries ---------------------------------------------------------------

(nlk:define-contract band-render-request
  (:doc "Render original music on this machine: pass a score (inline object or score_path) or a preset name. Writes a wav and an mp3 into the output folder, one stem per part, exact length from the score's bars and tempo. Deterministic: the same score always renders the same audio. The answer names the file paths, the duration and the peak level.")
  ("score" :object :doc "a score object: bpm, key, mode, bars, progression, parts")
  ("score_path" :text :doc "a path to a score JSON file, instead of score")
  ("preset" :text :options '("bed" "band" "pulse" "funk")
            :doc "a bundled arrangement, when no score is passed")
  ("name" :text :doc "the track name, which names the output subfolder")
  ("room" :text :slot room-name :options '("dry" "close" "room" "hall" "cathedral")
          :doc "reverb room; the preset's own unless named")
  ("lufs" :number :default -14.0
          :doc "loudness target for ffmpeg's EBU R128 pass; -14 for social and video")
  ("mp3" :boolean :default t :doc "also write an mp3"))

(nlk:define-contract band-validate-request
  (:doc "Check a score without rendering it: instruments, roles, patterns, registers, tempo and bar count. Answers a one-line summary, or every reason it cannot be rendered.")
  ("score" :object :doc "a score object: bpm, key, mode, bars, progression, parts")
  ("score_path" :text :doc "a path to a score JSON file, instead of score"))

(defun no-arguments (description)
  "The schema of a tool whose call carries nothing."
  (list :description description
        :parameters (nlk:json-object "type" "object"
                                      "properties" (nlk:make-json-object))))

(defparameter +doctor-schema+
  (no-arguments "Check whether this machine can render audio: the fluidsynth binary, a General MIDI soundfont, ffmpeg and the output folder. Run this before the first render.")
  "band_doctor takes nothing.")

(defparameter +presets-schema+
  (no-arguments "List the bundled arrangements (bed, band, pulse, funk) with their tempo, key, bars and length in seconds, plus the reverb rooms.")
  "band_list_presets takes nothing.")

;;; --- reading a score --------------------------------------------------------------

(defun score-from-request (score-object score-path)
  "A BAND-SCORE from an inline object or a path; a failure says which is missing."
  (cond ((hash-table-p score-object) (score-from-json score-object))
        ((and (stringp score-path) (plusp (length (nlk:trimmed score-path))))
         (let ((path (nlk:trimmed score-path)))
           (unless (probe-file path)
             (fail "score_path ~a is not a file on this machine" path))
           (let ((object (handler-case (nlk:decode-json (uiop:read-file-string path))
                           (error () nil))))
             (unless (hash-table-p object)
               (fail "score_path ~a does not hold a JSON score object" path))
             (score-from-json object))))
        (t nil)))

;;; --- the tools --------------------------------------------------------------------

(defun doctor-tool (arguments)
  (declare (ignore arguments))
  (let* ((report (doctor-report))
         (ready (getf report :ready))
         (lines (list (format nil "ready: ~:[no~;yes~]" ready)
                      (format nil "fluidsynth  ~:[MISSING (apt install fluidsynth)~;~a~]"
                              (plusp (length (getf report :fluidsynth))) (getf report :fluidsynth))
                      (format nil "soundfont   ~:[MISSING (apt install fluid-soundfont-gm, or set hermes-band.soundfont)~;~a (~d MB)~]"
                              (plusp (length (getf report :soundfont)))
                              (getf report :soundfont) (getf report :soundfont_mb))
                      (format nil "ffmpeg      ~:[MISSING (apt install ffmpeg)~;~a~]"
                              (plusp (length (getf report :ffmpeg))) (getf report :ffmpeg))
                      (format nil "output_dir  ~a~:[ (not writable)~;~]"
                              (getf report :output_dir) (getf report :output_writable)))))
    (handler-case
        (format nil "~{~a~%~}~@[~a~]" lines (doctor-hint report))
      (error () (format nil "~{~a~^; ~}" lines)))))

(defun presets-tool (arguments)
  (declare (ignore arguments))
  (format nil "presets:~%~{  ~a~^~%~}~%rooms: ~{~a~^, ~}"
          (loop for name in (preset-names)
                for score = (preset name)
                collect (format nil "~a - ~d bars at ~a BPM in ~a ~a, ~,1f s, room ~a"
                                name (band-score-bars score) (band-score-bpm score)
                                (band-score-key score) (band-score-mode score)
                                (music-duration score) (preset-room name)))
          +room-names+))

(defun validate-tool (request)
  (let ((score (score-from-request (band-validate-request-score request)
                                   (band-validate-request-score-path request))))
    (unless score
      (fail "pass 'score' (an object) or 'score_path' (a file) to validate"))
    (let ((problems (validate-score score)))
      (if problems
          (format nil "invalid: ~a~%~{~a~^~%~}" (band-score-name score) problems)
          (format nil "ok: ~a" (describe-score score))))))

(defun render-tool (request)
  (let ((passed (score-from-request (band-render-request-score request)
                                    (band-render-request-score-path request))))
    (multiple-value-bind (score room)
        (cond (passed (values passed nil))
              ((plusp (length (nlk:trimmed (or (band-render-request-preset request) ""))))
               (let* ((name (string-downcase (nlk:trimmed (band-render-request-preset request))))
                      (preset (preset name)))
                 (unless preset
                   (fail "~s is not a preset; choose ~{~a~^, ~} or pass a score"
                         (band-render-request-preset request) (preset-names)))
                 (values preset (preset-room name))))
              (t (fail "pass 'score' (an object), 'score_path' (a file) or 'preset' (~{~a~^, ~})"
                       (preset-names))))
      (let ((named (nlk:trimmed (or (band-render-request-name request) ""))))
        (when (plusp (length named)) (setf (band-score-name score) named))
        (let* ((named-room (nlk:trimmed (or (band-render-request-room-name request) "")))
               (room (if (plusp (length named-room)) named-room (or room "room")))
               (lufs (band-render-request-lufs request))
               (mp3 (band-render-request-mp3 request))
               (result (render-track score room
                                     :mp3 (if (null mp3) t mp3)
                                     :lufs (if (numberp lufs) lufs -14.0))))
          (format nil "rendered ~a~%~a~@[~%mp3 ~a~]~%duration ~,2f s (~,2f s of music)~%stems: ~{~a~^, ~}~%~a"
                  (band-score-name score) (getf result :wav) (getf result :mp3)
                  (or (getf result :duration) 0.0) (or (getf result :music_duration) 0.0)
                  (getf result :stems) (describe-score score)))))))

(defun answering (function)
  "FUNCTION's answer, or its refusal as the one-line failure the model reads."
  (handler-case (funcall function)
    (hermes-band-error (condition) (nle:failure (format nil "ERROR: ~a" (hermes-band-error-detail condition))))
    (error (condition) (nle:failure (format nil "ERROR: ~a" condition)))))

;;; --- /band -----------------------------------------------------------------------

(defun run-slash (args session-id)
  "What this profile has rendered, or one preset's numbers."
  (declare (ignore session-id))
  (let ((argument (nlk:trimmed (or args ""))))
    (if (zerop (length argument))
        (presets-tool nil)
        (handler-case
            (let ((score (preset argument)))
              (unless score (fail "~s is not a preset; choose ~{~a~^, ~}" argument (preset-names)))
              (format nil "~a~%~a" (describe-score score)
                      (format nil "~,2f s of music; render it with band_render preset=~a"
                              (music-duration score) argument)))
          (hermes-band-error (condition) (hermes-band-error-detail condition))))))

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell hermes-band
  (:section ("hermes-band")
    (:guide "fluidsynth, a General MIDI soundfont and ffmpeg must be installed; run band_doctor to see which is missing; output_dir is where renders are written; soundfont overrides the found .sf2 path")
    ("output_dir" :directory :default "hermes-band-out/"
                  :doc "where rendered tracks are written, one subfolder per track")
    ("soundfont" :path :default ""
                 :doc "a General MIDI .sf2; empty means the usual system locations are searched")
    ("fluidsynth" :string :default "fluidsynth"
                  :doc "the fluidsynth binary, as it is on PATH")
    ("ffmpeg" :string :default "ffmpeg"
              :doc "the ffmpeg binary, as it is on PATH")
    ("sample_rate" :integer :default 44100 :min 8000
                   :doc "the render's sample rate")
    ("lufs" :integer :default -14 :min -40 :doc "the loudness target a render without its own uses"))
  (:tool "band_doctor" +doctor-schema+ #'doctor-tool)
  (:tool "band_list_presets" +presets-schema+ #'presets-tool)
  (:tool "band_validate_score" (nlk:contract 'band-validate-request)
         (lambda (request) (answering (lambda () (validate-tool request)))))
  (:tool "band_render" (nlk:contract 'band-render-request)
         (lambda (request) (answering (lambda () (render-tool request)))))
  (:help :hermes-band "hermes-band: compose and render original music on this machine - band_render, band_doctor" +primer+)
  (:command "band" 'run-slash
            :description "The bundled arrangements and their numbers"
            :argument-hint "[preset]")
  (:start (lambda ()
            (ensure-directories-exist (setting :output-dir)))))
