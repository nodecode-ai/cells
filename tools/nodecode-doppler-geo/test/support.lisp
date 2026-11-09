;;;; support.lisp --- a synthetic flyover, its ADS-B track, and a temp home.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; DOPPLER-GEO-CELL- name prefix; RUN-DOPPLER-GEO-TESTS runs exactly that
;;;; slice. Nothing here dials: the cell reads a WAV and a CSV, so the one
;;;; seam that reaches outside is the filesystem, and every test writes its
;;;; own files into a temporary directory and takes them away.

(in-package #:nodecode.test)

(define-test-slice "doppler-geo" "DOPPLER-GEO-CELL-" :start nodecode-doppler-geo:start-cell)

;;; --- the flyover the tests listen to ---------------------------------------------

(defparameter +rate+ 8000 "Sample rate of the synthetic recording.")

(defparameter +truth+ (list :f0 140d0 :speed 80d0 :slant 800d0 :cpa 4d0 :duration 8d0)
  "The pass written into every synthetic recording: 140 Hz, 80 m/s, 800 m of
slant at closest approach, 4 s in, 8 s long.")

(defparameter +lat+ 51.5d0 "Where the recording was made.")
(defparameter +lon+ -0.12d0 "Where the recording was made.")
(defparameter +alt-m+ 300d0 "The aircraft's altitude above the ground.")

(defun heard-frequency (time)
  "The tone of a plane with +TRUTH+ heard at T."
  (let* ((v (getf +truth+ :speed))
         (d (getf +truth+ :slant))
         (x (* v (- time (getf +truth+ :cpa))))
         (r (sqrt (+ (* d d) (* x x)))))
    (* (getf +truth+ :f0) (/ 343d0 (+ 343d0 (* v (/ x r)))))))

(defun make-recording (path)
  "Write a synthetic flyover to PATH as 16-bit mono WAV; return PATH."
  (let* ((n (round (* +rate+ (getf +truth+ :duration))))
         (samples (make-array n :element-type 'double-float))
         (phase 0d0))
    (dotimes (i n)
      (let ((time (/ i (float +rate+ 1d0))))
        (incf phase (/ (* 2d0 pi (heard-frequency time)) +rate+))
        (setf (aref samples i)
              (+ (* 0.25d0 (sin phase))
                 (* 0.02d0 (- (random 1d0) 0.5d0))))))
    (nodecode-doppler-geo:write-wav path samples +rate+)))

(defun make-tracks (path)
  "Write the ADS-B track of the same pass to PATH, time_s in the recording's
clock and the aircraft 800 m north of the recording point. => PATH."
  (let* ((lat-m 111320d0)
         (lon-m (* lat-m (cos (* pi (/ +lat+ 180d0))))))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (format out "icao,time_s,lat,lon,alt_ft~%")
      (loop for step from 0 to 40
            for time = (/ step 5d0)
            do (format out "abc123,~,1f,~,7f,~,7f,~,1f~%"
                       time
                       (+ +lat+ (/ 800d0 lat-m))
                       (+ +lon+ (/ (* (getf +truth+ :speed) (- time (getf +truth+ :cpa)))
                                   lon-m))
                       (/ +alt-m+ 0.3048d0)))))
  path)

(defmacro with-workspace ((&key recording tracks) &body body)
  "Run BODY in a scratch directory, with a synthetic RECORDING and TRACKS file
written into it under those names, and both taken away afterwards."
  (let ((dir (gensym "DIR")))
    `(let* ((,dir (merge-pathnames
                   (format nil "doppler-geo-test-~a/" (random 1000000))
                   (uiop:temporary-directory))))
       (ensure-directories-exist ,dir)
       (unwind-protect
            (let (,@(when recording `((,recording (make-recording (merge-pathnames "rec.wav" ,dir)))))
                  ,@(when tracks `((,tracks (make-tracks (merge-pathnames "tracks.csv" ,dir))))))
              ,@body)
         (ignore-errors (uiop:delete-directory-tree ,dir :validate t))))))

;;; --- asking the cell what a model would -------------------------------------------

(defun doppler-call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

(defun doppler-json (&rest pairs)
  "A JSON object of alternating key/value pairs, strings quoted."
  (let ((parts '()))
    (loop while pairs
          do (let ((key (pop pairs))
                   (value (pop pairs)))
               (push (format nil "~s:~a"
                             (string-downcase (string key))
                             (if (stringp value)
                                 (format nil "~s" value)
                                 (princ-to-string value)))
                     parts)))
    (format nil "{~{~a~^,~}}" (nreverse parts))))

(defun doppler-text-file (path text)
  "Write TEXT to PATH, for a test that needs something that is not a WAV."
  (with-open-file (out path :direction :output :if-exists :supersede)
    (write-string text out))
  path)
