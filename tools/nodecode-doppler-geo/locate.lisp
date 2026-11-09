;;;; locate.lisp --- ADS-B tracks, and the grid that scores against them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original downloaded a day of adsb.lol history (about 3 GB per
;;;; uncached day, cropped to the region and cached) and scored ground
;;;; locations by predicting each event's Doppler curve from the real
;;;; tracks. This cell reads the same ADS-B from a CSV the user already has
;;;; or exports, in the columns the original's own scorer took
;;;; (icao,time_s,lat,lon,alt_ft), and scores the region the same way: for
;;;; every candidate cell, the curve each track would produce at that point,
;;;; its least-squares misfit against the observed one, a Gaussian
;;;; likelihood, and the 68 and 95 per cent areas off the posterior.

(in-package #:nodecode-doppler-geo)

(defun split-comma (line)
  (let ((parts '()) (start 0))
    (loop for i = (position #\, line :start start)
          do (push (subseq line start i) parts)
          while i do (setf start (1+ i)))
    (nreverse parts)))

(defun number-in (text)
  "TEXT as a double-float, or NIL when it is not a number."
  (let ((s (string-trim '(#\space #\tab #\return #\") text)))
    (when (plusp (length s))
      (handler-case (let ((v (read-from-string s)))
                      (when (numberp v) (coerce v 'double-float)))
        (error () nil)))))

(defun column-index (header names default)
  (or (loop for name in names
            for i from 0
            when (member (string-downcase (string-trim " " name)) header :test #'string=)
              return i)
      default))

(defun read-tracks (path)
  "ADS-B points from PATH's CSV. => list of (:icao STR :points #(#(T LAT LON ALT-M))).
A header line is optional; without one the columns are icao,time_s,lat,lon,alt_ft."
  (let* ((lines (uiop:read-file-lines path))
         (first (first lines))
         (header (and first (some #'alpha-char-p (subseq first 0 (min 4 (length first)))))))
    (when (and header (< (length lines) 2))
      (fail "~a has a header and no rows" path))
    (let* ((rows (if header (rest lines) lines))
           (cols (if header
                     (let ((h (split-comma first)))
                       (list (column-index h '("icao" "hex" "icao24" "callsign") 0)
                             (column-index h '("time_s" "t" "time" "seconds") 1)
                             (column-index h '("lat" "latitude") 2)
                             (column-index h '("lon" "lng" "longitude") 3)
                             (column-index h '("alt_ft" "alt" "altitude_ft" "baro_alt") 4)))
                     '(0 1 2 3 4)))
           (groups (make-hash-table :test #'equal)))
      (dolist (row rows)
        (let* ((fields (split-comma row))
               (field (lambda (i) (and (< i (length fields)) (nth i fields))))
               (t0 (number-in (or (funcall field (second cols)) "")))
               (lat (number-in (or (funcall field (third cols)) "")))
               (lon (number-in (or (funcall field (nth 3 cols)) "")))
               (alt (number-in (or (funcall field (nth 4 cols)) "")))
               (id (string-trim " \"" (or (funcall field (first cols)) ""))))
          (when (and t0 lat lon alt (/= (length id) 0))
            (push (vector t0 lat lon (* alt 0.3048d0))
                  (gethash id groups)))))
      (let ((out '()))
        (maphash (lambda (id points)
                   (push (list :icao id
                               :points (coerce (sort points #'< :key (lambda (p) (aref p 0)))
                                               'vector))
                         out))
                 groups)
        (sort out #'string< :key (lambda (g) (getf g :icao)))))))

(defun haversine-km (lat1 lon1 lat2 lon2)
  (let* ((r 6371.0088d0)
         (p1 (* pi (/ lat1 180d0)))
         (p2 (* pi (/ lat2 180d0)))
         (dp (- p2 p1))
         (dl (* pi (/ (- lon2 lon1) 180d0)))
         (a (+ (* (sin (/ dp 2d0)) (sin (/ dp 2d0)))
               (* (cos p1) (cos p2) (sin (/ dl 2d0)) (sin (/ dl 2d0))))))
    (* 2d0 r (asin (min 1d0 (sqrt a))))))

(defun point-time (p) (aref p 0))

(defun slant-series (track lat lon elevation c)
  "Predicted received frequency factor g(t) = c / (c + vr(t)) at (LAT, LON)
for TRACK's points."
  (let* ((points (the simple-vector (getf track :points)))
         (n (length points))
         (ranges (make-array n :element-type 'double-float))
         (vrs (make-array n :element-type 'double-float))
         (gs (make-array n :element-type 'double-float)))
    (dotimes (i n)
      (let* ((p (aref points i))
             (h (* 1000d0 (haversine-km lat lon (aref p 1) (aref p 2))))
             (dz (- (aref p 3) elevation)))
        (setf (aref ranges i) (sqrt (+ (* h h) (* dz dz))))))
    (dotimes (i n)
      (let* ((i0 (max 0 (1- i)))
             (i1 (min (1- n) (1+ i)))
             (dt (- (point-time (aref points i1)) (point-time (aref points i0)))))
        (setf (aref vrs i) (if (plusp dt) (/ (- (aref ranges i1) (aref ranges i0)) dt) 0d0))
        (setf (aref gs i) (/ c (+ c (aref vrs i))))))
    gs))

(defun interpolate (points values when)
  "VALUES at WHEN, linear between the samples of POINTS (both in time order)."
  (let* ((n (length points))
         (last (1- n)))
    (cond ((<= when (point-time (aref points 0))) (aref values 0))
          ((>= when (point-time (aref points last))) (aref values last))
          (t (loop for i below last
                   for t0 = (point-time (aref points i))
                   for t1 = (point-time (aref points (1+ i)))
                   when (<= t0 when t1)
                     do (let ((w (if (> t1 t0) (/ (- when t0) (- t1 t0)) 0d0)))
                          (return (+ (* (aref values i) (- 1d0 w))
                                     (* (aref values (1+ i)) w))))
                   finally (return (aref values last)))))))
(defun misfit-hz (event track lat lon elevation c)
  "Rms Hz between EVENT's observed curve and the curve TRACK would give at
(LAT, LON), F0 solved in closed form. => rms or NIL when TRACK cannot answer."
  (let* ((points (the simple-vector (getf track :points)))
         (obs (getf event :points))
         (n (length points)))
    (when (and (>= n 3) (>= (length obs) 3))
      (let ((gs (slant-series track lat lon elevation c))
            (sgg 0d0) (sfg 0d0) (m 0))
        (dolist (o obs)
          (let ((g (interpolate points gs (first o))))
            (incf sgg (* g g))
            (incf sfg (* (second o) g))
            (incf m)))
        (when (and (plusp m) (plusp sgg))
          (let ((f0 (/ sfg sgg)) (ss 0d0))
            (dolist (o obs)
              (let ((e (- (second o) (* f0 (interpolate points gs (first o))))))
                (incf ss (* e e))))
            (sqrt (/ ss m))))))))

(defun parse-region (text)
  "TEXT as \"LAT,LON,RADIUS_KM\" => lat, lon, radius."
  (let ((parts (split-comma (or text ""))))
    (unless (= (length parts) 3)
      (fail "region is \"lat,lon,radius_km\", not ~s" text))
    (destructuring-bind (a b r) parts
      (let ((lat (number-in a)) (lon (number-in b)) (km (number-in r)))
        (unless (and lat lon km)
          (fail "region is \"lat,lon,radius_km\", not ~s" text))
        (unless (and (<= -90d0 lat 90d0) (<= -180d0 lon 180d0) (<= 0d0 km 200d0))
          (fail "region ~s is off the globe or wider than 200 km" text))
        (values lat lon km)))))

(defun score-cell (events tracks lat lon elevation c sigma)
  "The posterior weight of one ground cell, and its best (RMS ICAO)."
  (let ((best most-positive-double-float) (who nil))
    (dolist (event events)
      (dolist (track tracks)
        (let ((rms (misfit-hz event track lat lon elevation c)))
          (when (and rms (< rms best))
            (setf best rms who (getf track :icao))))))
    (list lat lon
          (if (< best most-positive-double-float)
              (exp (- (/ (* best best) (* 2d0 sigma sigma))))
              0d0)
          (if (< best most-positive-double-float) best nil)
          who)))

(defun locate-region (events tracks lat lon radius &key (elevation 0d0) (c *sound-speed*)
                                                (sigma 1.5d0) (steps 8))
  "Score every ground cell of the region against EVENTS and TRACKS.
=> a plist report: best, top-5, the two areas, the confidence."
  (let* ((step (/ radius (as-double steps)))
         (cells '()))
    (loop for i from (- steps) to steps
          do (loop for j from (- steps) to steps
                   do (let ((d-lat (* i step))
                            (d-lon (/ (* j step) (max 0.1d0 (cos (* pi (/ lat 180d0)))))))
                        (when (<= (sqrt (+ (* d-lat d-lat) (* d-lon d-lon))) radius)
                          (push (list (+ lat d-lat) (+ lon d-lon) i j) cells)))))
    (let* ((scored (loop for cell in cells
                         collect (score-cell events tracks (first cell) (second cell)
                                             elevation c sigma)))
           (total (reduce #'+ scored :key #'third)))
      (let* ((sorted (sort scored #'> :key #'third))
             (area-km2 (* step 111.0d0 step 111.0d0
                          (max 0.1d0 (cos (* pi (/ lat 180d0))))))
             (norm (if (plusp total) total 1d0)))
        (labels ((area-at (target)
                   (let ((cum 0d0) (count 0))
                     (dolist (cell sorted (* count area-km2))
                       (when (< cum target)
                         (incf cum (/ (third cell) norm))
                         (incf count))))))
          (let* ((best (first sorted))
                 (fraction (if (plusp total) (/ (third best) norm) 0d0))
                 (rms (fourth best))
                 (confident (and (>= fraction 0.15d0) rms (<= rms 2d0))))
            (list :ok t
                  :confidence (if confident "ok" "low")
                  :events events
                  :cells (length scored)
                  :tracks (length tracks)
                  :step-km (* step 111.0d0)
                  :best (list :lat (first best) :lon (second best)
                              :fraction fraction :rms rms :icao (fifth best))
                  :top (loop for cell in (subseq sorted 0 (min 5 (length sorted)))
                             collect (list :lat (first cell) :lon (second cell)
                                           :fraction (if (plusp total) (/ (third cell) norm) 0d0)
                                           :rms (fourth cell) :icao (fifth cell)))
                  :area68 (area-at 0.68d0)
                  :area95 (area-at 0.95d0))))))))
