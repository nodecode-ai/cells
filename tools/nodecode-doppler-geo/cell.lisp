;;;; cell.lisp --- the section, the two tools, /doppler and the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What the Hermes plugin registered through ctx.register_tool() is here a
;;;; clause of one NLE:DEFINE-CELL: doppler_detect and doppler_locate, the
;;;; /doppler command, a topic on the manual, and the `doppler-geo' section
;;;; they run on. Stop takes every one of them back, because the declaration
;;;; derives it.

(in-package #:nodecode-doppler-geo)

(defparameter *primer*
  "doppler-geo: where a recording was made, from the aircraft that flew over it.
  doppler_detect   the flyover tones in a WAV file: closest approach, speed,
                   slant distance, rest tone, fit residual (offline, no network)
  doppler_locate   the ground location they and an ADS-B track CSV agree on:
                   best point, top 5, 68/95 % areas, a confidence flag
A flyover leaves a faint tone in the spectrogram that falls away either side of
closest approach; its drop and steepness are the aircraft's speed and how near
it passed. Match those against real tracks near the recording and one small
area survives. The original fetched a day of adsb.lol history itself (about
3 GB per day); this cell reads the CSV you give it: icao,time_s,lat,lon,alt_ft,
with time in the recording's own clock. From eval the same two are
(doppler-geo:detect \"rec.wav\") and (doppler-geo:locate \"rec.wav\" \"lat,lon,km\").
Results are a hypothesis: report the 95 % area and the confidence, never the
best point of a low-confidence run. (help :doppler-geo) has this while the cell
runs; the section is in ~/.nodecode/config.jsonc under doppler-geo."
  "What (help :doppler-geo) answers while the cell runs.")

;;; --- the arguments each tool is held to ------------------------------------------

(nlk:define-contract detect-request
  (:doc "Find aircraft flyover Doppler tones in one recording, offline.")
  ("recording" :text :required t
               :doc "Path to a WAV file (PCM or 32-bit float) on this machine.")
  ("max_events" :integer :doc "Report at most this many flyovers.")
  ("fmin" :integer :doc "Lowest tone to search, Hz.")
  ("fmax" :integer :doc "Highest tone to search, Hz."))

(nlk:define-contract locate-request
  (:doc "Estimate where a recording was made, from its flyover tones and ADS-B tracks.")
  ("recording" :text :required t :doc "Path to a WAV file.")
  ("region" :text :required t :doc "lat,lon,radius_km: where to look; radius at most 200 km.")
  ("tracks" :string :doc "ADS-B CSV: icao,time_s,lat,lon,alt_ft. Default: the tracks setting.")
  ("sigma_hz" :number :doc "One location's tolerance in Hz; larger flattens the posterior."))

;;; --- the two verbs ---------------------------------------------------------------

(defun event-line (event number)
  (format nil "  ~d. closest approach ~,2f s: rest tone ~,1f Hz, ~,0f m/s (~,0f km/h), ~,0f m slant, fit rms ~,2f Hz, ~d frames, z ~,1f"
          number
          (getf event :cpa-time)
          (getf event :rest-hz)
          (getf event :speed-mps)
          (* 3.6d0 (getf event :speed-mps))
          (getf event :slant-m)
          (getf event :rms-hz)
          (getf event :frames)
          (getf event :z)))

(define-verb detect (path &key max-events fmin fmax)
  "The flyover report for PATH, as text. Offline: it reads the file and computes."
  (let* ((limit (or max-events (setting :max-events)))
         (lo (or fmin (setting :fmin)))
         (hi (or fmax (setting :fmax))))
    (when (> lo hi) (fail "fmin ~d is above fmax ~d" lo hi))
    (multiple-value-bind (samples rate) (read-wav path)
      (let ((seconds (/ (length samples) (as-double rate))))
        (multiple-value-bind (samples rate) (decimate samples rate 8000)
          (let ((events (detect-events samples rate :fmin (as-double lo) :fmax (as-double hi)
                                                  :max-events limit
                                                  :sound-speed (setting :sound-speed))))
            (with-output-to-string (out)
              (if (null events)
                  (format out "no flyover tones in ~a (~,2f s, ~d Hz): nothing in ~d-~d Hz cleared the detector.~
~%A narrow-band prop or rotor tone is what it looks for; a jet pass is usually broadband and invisible here."
                          path seconds rate lo hi)
                  (progn
                    (format out "~d flyover event~:p in ~a (~,2f s, ~d Hz):~%"
                            (length events) path seconds rate)
                    (loop for event in events for i from 1
                          do (format out "~a~%" (event-line event i)))
                    (format out "Nothing here says where it was: doppler_locate scores these against ADS-B tracks."))))))))))

(defun locate-events (path)
  (multiple-value-bind (samples rate) (read-wav path)
    (multiple-value-bind (samples rate) (decimate samples rate 8000)
      (detect-events samples rate :fmin (as-double (setting :fmin))
                                  :fmax (as-double (setting :fmax))
                                  :max-events (setting :max-events)
                                  :sound-speed (setting :sound-speed)))))

(defun locate-report (path region tracks-file events report)
  (declare (ignore path))
  (with-output-to-string (out)
    (format out "~d event~:p, ~d track~:p, ~d cells around ~a, ~,1f km each~%"
            (length events) (getf report :tracks) (getf report :cells)
            region (getf report :step-km))
    (destructuring-bind (&key lat lon fraction rms icao) (getf report :best)
      (format out "best: ~,4f, ~,4f (~,0f%% of the posterior)~@[, rms ~,2f Hz~]~@[, aircraft ~a~]~%"
              lat lon (* 100d0 fraction) rms icao))
    (format out "next: ~{~a~^; ~}~%"
            (loop for cell in (rest (getf report :top))
                  collect (format nil "~,4f,~,4f ~,0f%%"
                                  (getf cell :lat) (getf cell :lon)
                                  (* 100d0 (getf cell :fraction)))))
    (format out "68% area ~,0f km2, 95% area ~,0f km2~%" (getf report :area68) (getf report :area95))
    (format out "confidence: ~a~%" (getf report :confidence))
    (when (equal (getf report :confidence) "low")
      (format out "Read this as the search region, not a point: report the 95% area and say it is weak.~
~%The tracks were ~a." tracks-file))))

(define-verb locate (path region &key tracks sigma-hz)
  "The location report for PATH inside REGION (\"lat,lon,radius_km\"), as text."
  (let ((tracks-file (or tracks (setting :tracks))))
    (unless (and tracks-file (plusp (length tracks-file)))
      (fail "no ADS-B tracks are set: put a CSV path in the tracks member of the doppler-geo section, or name one in the call"))
    (unless (probe-file tracks-file)
      (fail "the tracks file ~a does not exist" tracks-file))
    (multiple-value-bind (lat lon radius) (parse-region region)
      (let ((events (locate-events path)))
        (if (null events)
            (format nil "no flyover tones in ~a: there is nothing to score." path)
            (locate-report path region tracks-file events
                            (locate-region events (read-tracks tracks-file) lat lon radius
                                           :c (setting :sound-speed)
                                           :sigma (or sigma-hz (setting :sigma-hz)))))))))
;;; --- what the model calls ---------------------------------------------------------

(defun tool-detect (request)
  (detect (detect-request-recording request)
          :max-events (detect-request-max-events request)
          :fmin (detect-request-fmin request)
          :fmax (detect-request-fmax request)))

(defun tool-locate (request)
  (locate (locate-request-recording request)
          (locate-request-region request)
          :tracks (locate-request-tracks request)
          :sigma-hz (locate-request-sigma-hz request)))

;;; --- /doppler ----------------------------------------------------------------------

(defun run-command (args session-id)
  "`/doppler recording.wav` reports the flyovers; `/doppler locate rec.wav lat,lon,km` locates."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (space (position #\space text))
         (head (if space (subseq text 0 space) text))
         (rest (if space (nlk:trimmed (subseq text (1+ space))) "")))
    (cond
      ((zerop (length text))
       (values "usage: /doppler RECORDING.wav  |  /doppler locate RECORDING.wav lat,lon,radius_km"
               (nle:list-dialog
                :title "Aircraft Doppler"
                :summary "Find the flyovers in a recording, or where it was made."
                :rows (list (nle:ask-row "Find the flyovers" "Recording" "/doppler"
                                          :placeholder "rec.wav" :verb "detect")))))
      ((string-equal head "locate")
       (let* ((second-space (position #\space rest)))
         (if second-space
             (values (locate (subseq rest 0 second-space)
                             (nlk:trimmed (subseq rest (1+ second-space))))
                     nil)
             (values (format nil "usage: /doppler locate RECORDING.wav lat,lon,radius_km") nil))))
      (t (values (detect text) nil)))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell doppler-geo
  (:section ("doppler-geo")
    (:guide "nothing is required to detect; tracks is the ADS-B CSV locate scores against; next step: /doppler RECORDING.wav")
    ("tracks" :path :required t
              :doc "ADS-B track CSV, icao,time_s,lat,lon,alt_ft, in the recording's clock")
    ("fmin" :integer :default 60 :min 20 :doc "lowest tone searched, Hz (prop tones sit 60-400)")
    ("fmax" :integer :default 400 :min 60 :doc "highest tone searched, Hz")
    ("max_events" :integer :default 3 :min 1 :doc "flyovers reported per recording")
    ("sigma_hz" :integer :default 2 :min 1 :doc "a location's tolerance in Hz; larger flattens the posterior")
    ("sound_speed" :integer :default 343 :min 250 :doc "metres per second; 331 + 0.6 per degree C"))
  (:tool "doppler_detect" (nlk:contract 'detect-request) 'tool-detect)
  (:tool "doppler_locate" (nlk:contract 'locate-request) 'tool-locate)
  (:command "doppler" 'run-command
            :description "Aircraft flyover tones in a recording, and where it was made"
            :argument-hint "RECORDING.wav | locate RECORDING.wav lat,lon,km")
  (:help :doppler-geo
         "doppler-geo: flyover Doppler tones in a recording, and the area it was made in - (doppler-geo:detect \"rec.wav\")"
         *primer*))
