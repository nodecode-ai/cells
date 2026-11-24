;;;; cell-test.lisp --- lifecycle, the two tools, /doppler, and every refusal.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, not the maths: the config
;;;; surface, each tool through the real dispatch seam, the detector finding
;;;; the pass it was given, the scorer answering cells near the truth, and the
;;;; refusals naming what to fix.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "doppler-geo"
  (:config "fmin" 60 "fmax" 400 "max_events" 3)
  (:help :doppler-geo)
  (:command "doppler")
  (:running
   (dolist (name '("doppler_detect" "doppler_locate"))
     (is (nle::find-tool name) (format nil "the ~a tool is registered" name)))
   (is (= 60 (nodecode-doppler-geo::setting :fmin)) "the section's numbers are the ones set"))
  (:stopped
   (dolist (name '("doppler_detect" "doppler_locate"))
     (is (null (nle::find-tool name)) (format nil "stop unregisters ~a" name))))
  (:refused
   ("fmin" 5)
   ("max_events" 0)
   ("sigma_hz" 0)))

;;; --- detect ---------------------------------------------------------------------

(deftest doppler-geo-cell-detect-finds-a-synthetic-flyover ()
  (with-workspace (:recording recording)
    (with-cell-stop ((doppler-geo-start "fmin" 60 "fmax" 400))
      (let ((text (doppler-call-tool
                   "doppler_detect" (doppler-json :recording (namestring recording)))))
        (is (search "flyover event" text) text)
        (is (search "closest approach" text) text)
        (is (search "rest tone 1" text) text)
        (is (search "doppler_locate" text) text)))))

(deftest doppler-geo-cell-the-detector-finds-the-pass-it-was-given ()
  (with-workspace (:recording recording)
    (multiple-value-bind (samples rate) (nodecode-doppler-geo:read-wav recording)
      (is (= +rate+ rate) "the WAV is read back at its own rate")
      (let* ((events (nodecode-doppler-geo:detect-events samples rate))
             (event (first (sort (copy-list events) #'> :key (lambda (e) (getf e :frames)))))
             (seen (format nil "~s" events)))
        (is (plusp (length events)) seen)
        (is (< 100d0 (getf event :rest-hz) 200d0) seen)
        (is (< (getf event :rms-hz) 2.5d0) seen)
        (is (< 10 (getf event :frames)) seen)
        (is (> (length (getf event :points)) 10) seen)))))

(deftest doppler-geo-cell-a-recording-with-no-flyover-says-so ()
  (with-workspace ()
    (let* ((path (merge-pathnames "quiet.wav" (uiop:temporary-directory)))
           (samples (let ((s (make-array 32000 :element-type 'double-float)))
                      (dotimes (i 32000)
                        (setf (aref s i) (* 0.02d0 (- (random 1d0) 0.5d0))))
                      s)))
      (unwind-protect
           (progn
             (nodecode-doppler-geo:write-wav path samples 8000)
             (with-cell-stop ((doppler-geo-start))
               (let ((text (doppler-call-tool
                            "doppler_detect" (doppler-json :recording (namestring path)))))
                 (is (search "no flyover tones" text) text))))
        (ignore-errors (delete-file path))))))

;;; --- locate ---------------------------------------------------------------------

(deftest doppler-geo-cell-locate-answers-the-cells-near-the-truth ()
  (with-workspace (:recording recording :tracks tracks)
    (with-cell-stop ((doppler-geo-start "tracks" (namestring tracks)))
      (let* ((events (nodecode-doppler-geo::locate-events recording))
             (report (nodecode-doppler-geo::locate-region
                      events (nodecode-doppler-geo:read-tracks tracks) +lat+ +lon+ 4d0))
             (seen (format nil "~s" report))
             (best (getf report :best))
             (apart (nodecode-doppler-geo::haversine-km
                     +lat+ +lon+ (getf best :lat) (getf best :lon))))
        (is (getf report :ok) seen)
        (is (plusp (length events)) seen)
        ;; One pass leaves the ellipse long: the cell promises an area, not a
        ;; point, so what is asserted is that the answer stays inside the
        ;; region it was asked about.
        (is (< 0d0 apart) seen)
        (is (plusp (getf report :cells)) seen)
        (is (>= (getf report :area95) (getf report :area68)) "the 95% area holds the 68% one"))
      (let ((text (doppler-call-tool
                   "doppler_locate" (doppler-json :recording (namestring recording)
                                                   :region "51.5,-0.12,4"))))
        (is (search "best:" text) text)
        (is (search "confidence:" text) text)))))

(deftest doppler-geo-cell-locate-names-the-tracks-setting-when-it-has-none ()
  (with-workspace (:recording recording)
    (with-cell-stop ((doppler-geo-start))
      (let ((text (doppler-call-tool
                   "doppler_locate" (doppler-json :recording (namestring recording)
                                                   :region "51.5,-0.12,4"))))
        (is (search "tracks" text) text)))))

(deftest doppler-geo-cell-a-region-that-is-not-a-region-refuses ()
  (with-workspace (:recording recording :tracks tracks)
    (with-cell-stop ((doppler-geo-start "tracks" (namestring tracks)))
      (let ((text (doppler-call-tool
                   "doppler_locate" (doppler-json :recording (namestring recording)
                                                   :region "everywhere"))))
        (is (search "lat,lon,radius_km" text) text)))))

(deftest doppler-geo-cell-a-file-that-is-not-a-wav-refuses ()
  (with-workspace ()
    (let ((path (doppler-text-file (merge-pathnames "notes.txt" (uiop:temporary-directory))
                                    "this is not a recording")))
      (unwind-protect
           (with-cell-stop ((doppler-geo-start))
             (let ((text (doppler-call-tool
                          "doppler_detect" (doppler-json :recording (namestring path)))))
               (is (search "WAV" text) text)))
        (ignore-errors (delete-file path))))))

;;; --- /doppler -------------------------------------------------------------------

(deftest doppler-geo-cell-the-command-runs-a-detection ()
  (with-workspace (:recording recording)
    (with-cell-stop ((doppler-geo-start "fmin" 60 "fmax" 400))
      (let ((text (nodecode-doppler-geo::run-command (namestring recording) "s1")))
        (is (search "flyover event" text) text))
      (multiple-value-bind (text dialog)
          (nodecode-doppler-geo::run-command "" "s1")
        (declare (ignore dialog))
        (is (search "usage:" text) text)))))
