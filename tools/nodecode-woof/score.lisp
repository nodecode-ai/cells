;;;; score.lisp --- a finished run against observations.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The original scores a run by queueing a job that runs WOOF's own
;;;; observation doors and its battery scorer inside the WOOF environment,
;;;; under a one-off registration its registration library builds for the
;;;; run's length. This does the same, in the same order and with the same
;;;; arguments, through the interpreter that runs the woof command: the
;;;; fetches (MRMS, Stage-IV, ASOS) write their packs beside the run, and the
;;;; scorer writes battery-score.json, which is read into score.json next to
;;;; the run. A one-off score is proposed-unratified by construction, and that
;;;; status is carried through untouched.

(in-package #:nodecode-woof)

(defparameter +state-boxes+
  '(("AL" -88.5 30.1 -84.9 35.0) ("AZ" -114.9 31.3 -109.0 37.0)
    ("AR" -94.7 33.0 -89.6 36.5) ("CA" -124.5 32.5 -114.1 42.0)
    ("CO" -109.1 36.9 -102.0 41.0) ("CT" -73.7 40.9 -71.8 42.1)
    ("DE" -75.8 38.4 -75.0 39.9) ("DC" -77.2 38.8 -76.9 39.0)
    ("FL" -87.7 24.4 -80.0 31.0) ("GA" -85.6 30.3 -80.8 35.0)
    ("ID" -117.3 41.9 -111.0 49.0) ("IL" -91.6 36.9 -87.0 42.5)
    ("IN" -88.1 37.7 -84.8 41.8) ("IA" -96.7 40.3 -90.1 43.5)
    ("KS" -102.1 36.9 -94.6 40.0) ("KY" -89.6 36.5 -81.9 39.2)
    ("LA" -94.1 28.9 -88.8 33.1) ("ME" -71.1 43.0 -66.9 47.5)
    ("MD" -79.5 37.9 -75.0 39.8) ("MA" -73.5 41.2 -69.9 42.9)
    ("MI" -90.5 41.7 -82.4 48.3) ("MN" -97.3 43.5 -89.5 49.4)
    ("MS" -91.7 30.1 -88.1 35.0) ("MO" -95.8 35.9 -89.1 40.6)
    ("MT" -116.1 44.3 -104.0 49.0) ("NE" -104.1 40.0 -95.3 43.0)
    ("NV" -120.0 35.0 -114.0 42.0) ("NH" -72.6 42.7 -70.6 45.3)
    ("NJ" -75.6 38.9 -73.9 41.4) ("NM" -109.1 31.3 -103.0 37.0)
    ("NY" -79.8 40.5 -71.8 45.0) ("NC" -84.3 33.8 -75.4 36.6)
    ("ND" -104.1 45.9 -96.5 49.0) ("OH" -84.8 38.4 -80.5 42.3)
    ("OK" -103.0 33.6 -94.4 37.0) ("OR" -124.6 41.9 -116.5 46.3)
    ("PA" -80.6 39.7 -74.7 42.3) ("RI" -71.9 41.1 -71.1 42.0)
    ("SC" -83.4 32.0 -78.5 35.2) ("SD" -104.1 42.5 -96.4 45.9)
    ("TN" -90.3 35.0 -81.6 36.7) ("TX" -106.7 25.8 -93.5 36.5)
    ("UT" -114.1 37.0 -109.0 42.0) ("VT" -73.4 42.7 -71.5 45.0)
    ("VA" -83.7 36.5 -75.2 39.5) ("WA" -124.8 45.5 -116.9 49.0)
    ("WV" -82.7 37.2 -77.7 40.6) ("WI" -92.9 42.5 -86.8 47.1)
    ("WY" -111.1 41.0 -104.1 45.0))
  "US state boxes (west, south, east, north) the original uses to name the ASOS networks a run touches. Public geography, not case data.")

;;; --- the interpreter that runs WOOF -------------------------------------------------------

(defun executable-file-p (path)
  (and (stringp path) (plusp (length path)) (probe-file path)))

(defun on-path (name)
  "NAME found in the PATH this image runs under, or NIL."
  (let ((path (uiop:getenv "PATH")))
    (when (and path (plusp (length path)))
      (loop for directory in (uiop:split-string path :separator '(#\:))
            for candidate = (merge-pathnames name (uiop:ensure-directory-pathname directory))
            when (executable-file-p (uiop:native-namestring candidate))
              return (uiop:native-namestring candidate)))))

(defun woof-python ()
  "The interpreter that runs WOOF's own doors: the section's python when it
names one, else the python3 beside the woof command, the way the original
takes the WOOF environment's own."
  (let ((named (nlk:trimmed (or (setting :python) ""))))
    (if (plusp (length named))
        named
        (let* ((command (woof-command))
               (resolved (if (find #\/ command) command (or (on-path command) command)))
               (directory (uiop:pathname-directory-pathname (uiop:parse-native-namestring resolved)))
               (candidate (merge-pathnames "python3" directory)))
          (if (executable-file-p (uiop:native-namestring candidate))
              (uiop:native-namestring candidate)
              (or (on-path "python3") "python3"))))))

;;; --- the run's own output ------------------------------------------------------------------

(defun wrfout-files (run-id)
  "The d01 forecast files a run wrote, oldest first."
  (let* ((root (run-directory run-id))
         (found (ignore-errors (directory (merge-pathnames "woof-out/**/wrfout_d01_*" root)))))
    (sort (remove-if (lambda (path)
                       (let ((name (file-namestring path)))
                         (or (search "/ready/" (uiop:native-namestring path))
                             (some (lambda (suffix) (search suffix name))
                                   '(".tmp" ".partial" ".json")))))
                     found)
          #'string< :key #'file-namestring)))

(defparameter +valid-time-length+ 19
  "Characters in the valid time a wrfout name ends with: 2026-08-17_18:00:00.")

(defun valid-time-of (path)
  "The valid time a wrfout name carries, as (values YEAR MONTH DAY HOUR MINUTE SECOND)."
  (let* ((name (file-namestring path))
         (at (- (length name) +valid-time-length+)))
    (when (plusp at)
      (let ((text (subseq name at)))
        (when (and (char= (char text 4) #\-) (char= (char text 7) #\-)
                   (char= (char text 10) #\_) (char= (char text 13) #\:)
                   (char= (char text 16) #\:))
          (values (parse-integer text :start 0 :end 4)
                  (parse-integer text :start 5 :end 7)
                  (parse-integer text :start 8 :end 10)
                  (parse-integer text :start 11 :end 13)
                  (parse-integer text :start 14 :end 16)
                  (parse-integer text :start 17 :end 19)))))))

(defun utc-seconds (year month day hour minute second)
  (encode-universal-time second minute hour day month year 0))

(defun iso-time (year month day hour minute second)
  (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d"
          year month day hour minute second))

(defun run-window (files)
  "A run's start and its last valid time, from the files it wrote.
=> (values START-ISO END-ISO HOURS) or NIL when the names carry no time."
  (let ((times '()))
    (dolist (path files)
      (multiple-value-bind (year month day hour minute second) (valid-time-of path)
        (when year
          (push (list (utc-seconds year month day hour minute second)
                      (iso-time year month day hour minute second))
                times))))
    (when times
      (let* ((sorted (sort times #'< :key #'first))
             (first-time (first sorted))
             (last-time (car (last sorted))))
        (values (second first-time) (second last-time)
                (floor (- (first last-time) (first first-time)) 3600))))))

;;; --- the observation doors ----------------------------------------------------------------

(defun fetch-with-retries (argv tries pause)
  "Run one door, trying again while the archive refuses a burst. => STATUS."
  (loop for attempt from 1 to tries
        do (multiple-value-bind (out err status) (run-woof argv)
             (declare (ignore out))
             (if (or (= 0 status) (= attempt tries))
                 (return (values status err))
                 (sleep pause)))))

(defun networks-for (box)
  "The ASOS networks a box touches, by state."
  (destructuring-bind (west south east north) box
    (loop for (state bw bs be bn) in +state-boxes+
          when (not (or (< be west) (> bw east) (< bn south) (> bs north)))
            collect (format nil "~a_ASOS" state))))

(defun bbox-text (box)
  (format nil "~,3f,~,3f,~,3f,~,3f" (first box) (second box) (third box) (fourth box)))

;;; --- what the scorer's record says ----------------------------------------------------------

(defun numeric-entries (object)
  "OBJECT's entries whose value is a number, as (key . number) pairs."
  (when (hash-table-p object)
    (loop for key being the hash-keys of object using (hash-value value)
          when (numberp value) collect (cons key value))))

(defun object-entries (object)
  "OBJECT's entries whose value is another object."
  (when (hash-table-p object)
    (loop for key being the hash-keys of object using (hash-value value)
          when (hash-table-p value) collect (cons key value))))

(defun battery-summary (battery)
  "The headline numbers, read off the scorer's own record, as the original reads them."
  (let ((rows '()))
    (let ((reflectivity (member-value battery "reflectivity")))
      (let ((primary (and (hash-table-p reflectivity)
                          (gethash "primary_scalar" reflectivity))))
        (when (numberp primary) (push (cons "reflectivity_fss_primary" primary) rows)))
      (dolist (key '("primary_threshold_dbz" "primary_half_width_cells"))
        (let ((value (and (hash-table-p reflectivity) (gethash key reflectivity))))
          (when (numberp value) (push (cons key value) rows)))))
    (let ((surface (member-value battery "surface")))
      (dolist (entry (numeric-entries surface))
        (when (or (search "rmse" (car entry)) (search "bias" (car entry)) (search "median" (car entry)))
          (push (cons (concatenate 'string "surface_" (car entry)) (cdr entry)) rows)))
      (let ((variables (or (member-value surface "variables")
                           (member-value surface "per_variable"))))
        (loop for (name . row) in (object-entries variables)
              do (loop for key in '("median_station_rmse" "rmse" "bias" "stations")
                       for value = (gethash key row)
                       when (numberp value)
                         do (push (cons (format nil "surface_~a_~a" name key) value) rows)))))
    (let ((precipitation (member-value battery "precipitation")))
      (dolist (entry (numeric-entries precipitation))
        (push (cons (concatenate 'string "precipitation_" (car entry)) (cdr entry)) rows)))
    (nreverse rows)))

;;; --- the registration, made by WOOF's own library --------------------------------------------

(defparameter +registration-door+
  "import json, sys
spec = json.load(open(sys.argv[1]))
import glob, os, netCDF4
files = sorted(f for f in glob.glob(os.path.join(spec['wrfout_dir'], 'wrfout_d01_*'))
               if not f.endswith(('.tmp', '.partial', '.json')) and '/ready/' not in f)
if not files:
    raise SystemExit('no wrfout_d01 file under ' + spec['wrfout_dir'])
ds = netCDF4.Dataset(files[0])
lat, lon = ds.variables['XLAT'][0], ds.variables['XLONG'][0]
box = [float(lon.min()), float(lat.min()), float(lon.max()), float(lat.max())]
ds.close()
from woof.verify.obs import registration as reg
doc = reg.make_registration(
    evaluator_commit=spec['evaluator_commit'],
    reflectivity=reg.reflectivity_parameters(),
    surface=reg.surface_parameters(),
    precipitation=reg.precipitation_parameters(window_hours=spec['windows'],
                                               guardrail_window_hours=spec['windows'][-1]),
    promotion=reg.promotion_parameters(),
    cases=[{'case_id': spec['case_id'], 'init_time': spec['init_time'],
            'station_table_sha256': spec['station_table_sha256']}],
    arms=[{'arm_id': spec['arm_id']}],
    twin={'rung': 'rung-1-one-ulp-theta', 'perturbation': 'not run for a one-off score'},
    scored_lead_hours_=spec['leads'])
json.dump(doc, open(spec['registration'], 'w'), indent=2, sort_keys=True)
print(json.dumps({'box': box, 'rule_status': doc.get('rule_status'),
                  'registration_sha256': doc.get('registration_sha256')}))
"
  "WOOF's own registration library, called through the interpreter that runs WOOF: the one-off registration a score needs, and the domain box the run's first forecast file carries. Nothing of the original plugin's Python is used.")

(defun evaluator-commit ()
  "The WOOF source commit the documentation pack was built from, as the original's registration takes it."
  (let ((manifest (pack-manifest)))
    (or (and manifest (nlk:json-value manifest :string "woof_source_commit_full")) "")))

(defun registration-for (run-id folder &key start windows leads case-id arm-id)
  "Ask WOOF for this run's one-off registration. => (values BOX DOC) or (values NIL NIL)."
  (let* ((wrfout (or (first (wrfout-files run-id)) ""))
         (wrfout-dir (if (stringp wrfout)
                         (uiop:native-namestring (uiop:pathname-directory-pathname wrfout))
                         ""))
         (registration (merge-pathnames "registration.json" folder))
         (path (write-json (merge-pathnames "registration-spec.json" folder)
                            (nlk:make-json-object
                             "wrfout_dir" wrfout-dir
                             "evaluator_commit" (evaluator-commit)
                             "windows" (coerce windows 'vector)
                             "leads" (coerce leads 'vector)
                             "case_id" case-id
                             "arm_id" arm-id
                             "init_time" start
                             "station_table_sha256" ""
                             "registration" (uiop:native-namestring registration)))))
    (multiple-value-bind (out err status)
        (run-woof (list (woof-python) "-c" +registration-door+ (uiop:native-namestring path)))
      (let ((document (and (= 0 status) (json-document out))))
        (if document
            (values (coerce (member-value document "box") 'list) document)
            (values nil (list :error (tail-text err 900))))))))

;;; --- the verb ------------------------------------------------------------------------------

(defun slug-for-id (text)
  "TEXT as the arm id a registration takes: letters, digits, dash, dot, underscore."
  (let ((cleaned (with-output-to-string (out)
                   (loop for character across (if (and (stringp text) (plusp (length text))) text "arm")
                         do (write-char (if (or (alphanumericp character)
                                                (member character '(#\- #\_ #\.) :test #'char=))
                                            character
                                            #\-)
                                        out)))))
    (subseq cleaned 0 (min 48 (length cleaned)))))

(defun fetch-mrms (python out first last bbox)
  "MRMS composite reflectivity for the run's box: the scorer's primary instrument."
  (fetch-with-retries (list python "-m" "tools.obs_fetch_mrms"
                             "--start" first "--end" last "--bbox" bbox
                             "--out" (uiop:native-namestring (merge-pathnames "mrms/" out)))
                       3 30))

(defun fetch-stage4 (python out first last windows)
  "Stage-IV precipitation, one-hour windows and a six-hour one for a run of six hours or more."
  (fetch-with-retries (list python "-m" "tools.obs_fetch_stage4"
                             "--start" first "--end" last
                             "--accumulations" (format nil "~{~2,'0dh~^,~}" windows)
                             "--out" (uiop:native-namestring (merge-pathnames "stage4/" out)))
                       3 30))

(defun fetch-asos (python out first last bbox networks)
  "ASOS surface reports for the states the run's box touches."
  (fetch-with-retries (list python "-m" "tools.obs_fetch_asos"
                             "--networks" (format nil "~{~a~^,~}" networks)
                             "--bbox" bbox "--start" first "--end" last
                             "--out" (uiop:native-namestring (merge-pathnames "asos/" out)))
                       3 30))

(defun score-attempts (precipitation surface)
  "The combinations to try, in the original's order: everything, then without
one instrument when the scorer refuses a combination, then without either."
  (remove nil (list (list precipitation surface nil)
                    (and precipitation (list nil surface "precipitation"))
                    (and surface (list precipitation nil "surface"))
                    (and precipitation surface (list nil nil "precipitation and surface")))))

(defun finish-score (folder answer)
  "ANSWER written beside the run as score.json, and answered as the tool's text."
  (write-json (merge-pathnames "score.json" folder) answer)
  (nlk:encode-json-object answer))

(defun run-score (call)
  "woof_score: a finished run against observations, through WOOF's own doors."
  (let* ((run-id (score-call-run-id call))
         (record (read-record run-id)))
    (unless record
      (fail "no run ~a under ~a" run-id (uiop:native-namestring (runs-root))))
    (let ((state (run-state record)))
      (unless (equal state "ended")
        (fail "run ~a is ~a; only a finished run can be scored" run-id state)))
    (let* ((folder (run-directory run-id))
           (files (wrfout-files run-id)))
      (unless files
        (fail "run ~a wrote no woof-out/wrfout_d01_* file, so there is nothing to score" run-id))
      (multiple-value-bind (start finish hours) (run-window files)
        (unless (and start (>= hours 1))
          (fail "run ~a covers less than one forecast hour, so it has no lead to score" run-id))
        (let* ((asked (coerce (or (score-call-against call) '()) 'list))
               (wanted (if asked
                           (loop for item in asked collect (string-downcase (princ-to-string item)))
                           '("asos" "mrms" "stage4")))
               (leads (if (< hours 6)
                          (loop for lead from 1 to hours collect lead)
                          (loop for lead from 2 to (min hours 18) collect lead)))
               (windows (if (>= hours 6) '(1 6) '(1)))
               (out (ensure-directories-exist (merge-pathnames "score/" folder)))
               (first (concatenate 'string start "Z"))
               (last (concatenate 'string finish "Z"))
               (case-id (format nil "run-~a" run-id))
               (arm-id (slug-for-id (or (record-value record "label") "")))
               (python (woof-python))
               (notes '()))
          (multiple-value-bind (box registration)
              (registration-for run-id out :start start :windows windows :leads leads
                                :case-id case-id :arm-id arm-id)
            (unless box
              (fail "WOOF could not write this run's registration: ~a"
                    (or (getf registration :error) "no answer")))
            (let ((bbox (bbox-text box))
                  (mrms (fetch-mrms python out first last (bbox-text box))))
              (unless (= 0 mrms)
                (fail "the MRMS fetch failed; reflectivity carries the scorer's primary score, so nothing was scored"))
              (let* ((stage (if (member "stage4" wanted :test #'string=)
                                (fetch-stage4 python out first last windows) 127))
                     (precipitation (and (member "stage4" wanted :test #'string=) (= 0 stage)
                                         (merge-pathnames "stage4/" out)))
                     (networks (and (member "asos" wanted :test #'string=) (networks-for box)))
                     (asos (if networks (fetch-asos python out first last bbox networks) 127))
                     (stations (and networks (= 0 asos) (merge-pathnames "asos/stations.json" out)))
                     (surface (and networks (= 0 asos) (merge-pathnames "asos/surface.json" out))))
                (when (and (member "stage4" wanted :test #'string=) (/= 0 stage))
                  (push "Stage-IV fetch failed; precipitation not scored" notes))
                (when (and (member "asos" wanted :test #'string=) (null networks))
                  (push "the run's box touches no US ASOS network; surface not scored" notes))
                (when (and networks (/= 0 asos))
                  (push "ASOS fetch failed; surface not scored" notes))
                (let* ((score-out (merge-pathnames "battery-score.json" out))
                       (base (list python "-m" "tools.obs_battery_score"
                                    "--run-directory" (uiop:native-namestring
                                                         (uiop:pathname-directory-pathname (first files)))
                                    "--domain" "d01" "--case-id" case-id "--arm-id" arm-id
                                    "--init-time" start
                                    "--reflectivity-packs" (uiop:native-namestring (merge-pathnames "mrms/" out))
                                    "--boundary-width-cells" "10"
                                    "--registration" (uiop:native-namestring (merge-pathnames "registration.json" out))
                                    "--score-out" (uiop:native-namestring score-out)))
                       (precip-args (and precipitation
                                         (list "--precipitation-packs" (uiop:native-namestring precipitation)
                                               "--precipitation-window-hours"
                                               (format nil "~{~d~^,~}" windows))))
                       (surface-args (and stations surface
                                          (list "--asos-stations" (uiop:native-namestring stations)
                                                "--asos-surface" (uiop:native-namestring surface))))
                       (answer nil))
                  (loop for (precip-now surface-now dropped) in (score-attempts precip-args surface-args)
                        while (null answer)
                        do (multiple-value-bind (drawn refusal status)
                               (run-woof (append base (or precip-now '()) (or surface-now '())))
                             (declare (ignore drawn))
                             (if (= 0 status)
                                 (progn
                                   (when dropped
                                     (push (format nil "~a not scored: the scorer refused it" dropped) notes))
                                   (let ((battery (read-json score-out)))
                                     (setf answer
                                           (nlk:make-json-object
                                            "ok" t
                                            "run_id" run-id
                                            "rule_status" (or (member-value registration "rule_status") "")
                                            "registration_sha256" (or (member-value registration "registration_sha256") "")
                                            "scored_lead_hours" (coerce leads 'vector)
                                            "precipitation_window_hours" (coerce (if precip-now windows '()) 'vector)
                                            "box" (coerce box 'vector)
                                            "against" (nlk:make-json-object "mrms" t
                                                                             "stage4" (and precip-now t)
                                                                             "asos" (and surface-now t))
                                            "summary" (apply #'nlk:make-json-object
                                                              (loop for (key . value) in (battery-summary battery)
                                                                    append (list key value)))
                                            "notes" (coerce (reverse notes) 'vector)
                                            "folder" (uiop:native-namestring out)
                                            "next" "woof_report carries this score beside the run"))))
                                 (push (tail-text refusal 500) notes))))
                  (if answer
                      (finish-score folder answer)
                      (finish-score folder
                                     (nlk:make-json-object
                                      "ok" nil
                                      "run_id" run-id
                                      "notes" (coerce (reverse notes) 'vector)
                                      "next" "the scorer refused every combination of instruments; its last words are in the notes"))))))))))))
