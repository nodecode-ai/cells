;;;; cell.lisp --- the mind kit: 25 subsystem meters, one aggregate, one read-only gate.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of ousiaresearch/biomimetic-brain (MIT). The kit reads
;;;; a directory of small JSON state files the agent's own passes write
;;;; (<agent>/brain/<subsystem>/<name>.json), folds them into one
;;;; <agent>/brain-state.json the rest of the agent reads, and answers a
;;;; PROCEED / CAUTION / VETO gate from named inputs -- read-only, every
;;;; absent reading reported as a default rather than passed off as a value.

(defpackage #:nodecode-mind-kit
  (:nicknames #:mind-kit)
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:state #:summary #:gate #:seed #:health
           #:agent-dir #:*mind-kit*))

(in-package #:nodecode-mind-kit)

(nlk:define-peripheral mind-kit :not-running t)

;;; --- readings ------------------------------------------------------------

(defparameter +default-quiet-start+ 23.0 "22:00-05:30 as docs/DECISION_GATE.md prints it.")
(defparameter +default-quiet-end+ 5.5)

(defun round2 (number)
  "NUMBER to two decimals, an integer when it lands on one."
  (let ((scaled (round (* (coerce number 'double-float) 100))))
    (if (zerop (mod scaled 100)) (truncate scaled 100) (/ scaled 100.0d0))))

(defun jget (data key)
  "DATA's member KEY as decoded JSON, or NIL."
  (nlk:json-value data :any key))

(defun jnum (data key default)
  "DATA's member KEY as a real, or DEFAULT when it is absent or not a number."
  (or (nlk:json-value data :number key) default))

(defun jbool (data key default)
  "DATA's KEY as a boolean; an absent key answers DEFAULT."
  (multiple-value-bind (value present) (nlk:json-value data :boolean key)
    (if present (not (null value)) default)))

(defun jtext (data key)
  "DATA's KEY as a trimmed lowercase string, or NIL."
  (let ((value (nlk:json-value data :text key)))
    (and value (string-downcase (string-trim '(#\Space #\Tab) value)))))

(defun jarray (data key)
  "DATA's KEY as a vector of elements, or the empty vector."
  (let ((value (nlk:json-value data :any key)))
    (if (and (vectorp value) (not (stringp value))) value #())))

(defun jtable (data key)
  (let ((value (nlk:json-value data :object key)))
    (or value (nlk:make-json-object))))

(defun entries-by (sequence key)
  "The ACTIVE objects in SEQUENCE."
  (loop for entry across sequence
        when (and (hash-table-p entry) (equal "active" (jtext entry key)))
          collect entry))

(defun count-entries (list key)
  (length (entries-by list key)))

(defun first-concepts (sequence)
  (loop for entry across sequence
        for concept = (and (hash-table-p entry) (nlk:json-value entry :string "concept"))
        when concept collect concept))

(defun clamp (value low high)
  (max low (min high (coerce value 'double-float))))

;;; --- clocks ---------------------------------------------------------------

(defun iso-now ()
  "The local clock as an ISO-8601 timestamp, seconds precision."
  (multiple-value-bind (second minute hour day month year) (decode-universal-time (get-universal-time))
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d"
            year month day hour minute second)))

(defun parse-int (text start end)
  (let ((value 0) (any nil))
    (loop for index from start below end
          for digit = (and (< index (length text)) (digit-char-p (char text index)))
          while digit
          do (setf value (+ (* value 10) digit) any t))
    (and any value)))

(defun iso->universal (text)
  "UNIVERSAL-TIME for an ISO-8601 timestamp, or NIL when TEXT is not one."
  (when (and (stringp text) (>= (length text) 19))
    (let ((year (parse-int text 0 4)) (month (parse-int text 5 7)) (day (parse-int text 8 10))
          (hour (parse-int text 11 13)) (minute (parse-int text 14 16)) (second (parse-int text 17 19)))
      (when (and year month day hour minute second
                 (<= 1 month 12) (<= 1 day 31) (<= 0 hour 23) (<= 0 minute 59) (<= 0 second 59))
        (let ((offset 0))
          (when (> (length text) 19)
            (let ((sign (char text 19)))
              (when (and (member sign '(#\+ #\-)) (>= (length text) 25)
                         (parse-int text 20 22) (parse-int text 23 25))
                (setf offset (* (if (char= sign #\-) -1 1)
                                (+ (* 60 (parse-int text 20 22)) (parse-int text 23 25)))))))
          (- (encode-universal-time second minute hour day month year 0) (* 60 offset)))))))

(defun hours-since (universal)
  (/ (- (get-universal-time) universal) 3600.0))

(defun reading-age (data path)
  "Hours since DATA's own timestamp, else since PATH was written, else 999."
  (or (let ((stamp (or (jget data "lastUpdated") (jget data "lastUpdate") (jget data "_updated"))))
        (let ((universal (iso->universal stamp)))
          (and universal (max 0.0 (hours-since universal)))))
      (ignore-errors (hours-since (file-write-date path)))
      999.0))

;;; --- one state file -------------------------------------------------------

(defun read-state-file (brain relatives)
  "The first of RELATIVES that exists, under BRAIN.
=> (VALUES DATA STATUS PATH): STATUS is \"read\", \"unreadable\" or \"missing\"."
  (block found
    (dolist (relative relatives)
      (let ((path (merge-pathnames relative (pathname brain))))
        (when (probe-file path)
          (handler-case
              (let ((data (nlk:decode-json (uiop:read-file-string path :external-format :utf-8) :whole t)))
                (return-from found (values (if (hash-table-p data) data (nlk:make-json-object))
                                           "read" path)))
            (error ()
              (return-from found (values (nlk:make-json-object) "unreadable" path)))))))
    (values (nlk:make-json-object) "missing" nil)))

(defun agent-dir ()
  "The directory the agent's own state lives in, from the section, as a directory."
  (uiop:ensure-directory-pathname (pathname (setting :agent-dir))))

(defun brain-dir ()
  (merge-pathnames "brain/" (agent-dir)))

;;; --- the 25 subsystems -----------------------------------------------------
;;; Each row is (name relative-paths max-age readings critical): READINGS is
;;; the plist a subsystem contributes to the aggregate when its file is there,
;;; CRITICAL what the aggregate calls critical about it.

(defparameter *systems*
  (list
   (list "somatic" '("somatic/markers.json") 8
         (lambda (d)
           (list :gut (or (jget d "gut_feeling") "unknown")
                 :energy (jnum d "energy" 0)
                 :valence (jnum d "valence" 0.5)
                 :confidence (jnum d "confidence" 0)
                 :tension (jnum d "tension" 0)))
         (lambda (d age) (declare (ignore d)) (> age 24)))
   (list "fatigue" '("fatigue/fatigue-state.json") 24
         (lambda (d)
           (list :level (jnum d "fatigue_level" 0)
                 :recovery-needed (jbool d "recovery_needed" nil)
                 :sustainable (jbool d "sustainable" t)
                 :engagement-override (jbool d "engagement_override" nil)
                 :subsidy-active (jbool d "subsidy_active" nil)
                 :override-reason (jget d "override_reason")))
         (lambda (d age)
           (declare (ignore age))
           (and (jbool d "recovery_needed" nil) (not (jbool d "engagement_override" nil)))))
   (list "dopamine/VTA" '("dopamine/vta-state.json") 24
         (lambda (d)
           (list :drive (jnum d "current_drive" (jnum d "currentDrive" 0.57))
                 :baseline (jnum d "baseline_drive" 0.57)
                 :novelty-boosts (jnum d "novelty_boosts" 0)))
         nil)
   (list "novelty" '("dopamine/novelty-state.json") 24
         (lambda (d) (list :novelty (jnum d "novelty_seeking" 0.5)
                           :vta-boost (jnum d "vta_boost" 0)))
         nil)
   (list "SCN" '("scn/scn-state.json") 24
         (lambda (d) (list :phase (or (jget d "phase") "unknown")
                           :quiet-hours (jbool d "quiet_hours" nil)
                           :local-hour (or (jget d "localHour") "?")))
         nil)
   (list "LC" '("lc/lc-state.json") 24
         (lambda (d) (list :arousal (jnum d "current_arousal" 0)
                           :behavioral-state (or (jget d "behavioral_state") "unknown")))
         nil)
   (list "amygdala" '("amy/emotional-decay-state.json") 24
         (lambda (d) (list :valence (jnum d "valence" 0.5)
                           :fear (jnum d "fear" 0)))
         (lambda (d age) (declare (ignore age)) (> (jnum d "fear" 0) 0.7)))
   (list "prefrontal" '("prefrontal/interest-emergence-state.json") 48
         (lambda (d)
           (let ((interests (jarray d "emerged_interests"))
                 (convictions (jarray d "convictions"))
                 (passions (jarray d "passions")))
             (list :interests (length interests) :convictions (length convictions)
                   :passions (length passions)
                   :top-interest (or (first (first-concepts interests)) "none")
                   :top-conviction (or (first (first-concepts convictions)) "none"))))
         nil)
   (list "ACC" '("acc/acc-state.json") 48
         (lambda (d) (list :conflict-detected (jbool d "conflict_detected" nil)
                           :conflict-intensity (jnum d "conflict_intensity" 0)
                           :monitoring-active (jbool d "monitoring_active" nil)))
         (lambda (d age) (declare (ignore age)) (jbool d "conflict_detected" nil)))
   (list "basal-ganglia" '("basal-ganglia/habit-state.json") 168
         (lambda (d) (list :habits (length (jarray d "habits"))
                           :patterns (hash-table-count (jtable d "patterns"))))
         nil)
   (list "cerebellum" '("cerebellum/cerebellum-state.json" "cerebellum-state.json") 168
         (lambda (d) (list :procedural (length (jarray d "procedural_habits"))))
         nil)
   (list "hypothalamus" '("hypothalamus/hypothalamus-state.json" "hypothalamus-state.json") 24
         (lambda (d) (list :arousal (jnum d "arousal_level" (jnum d "arousal" 0))
                           :homeostasis (jnum d "homeostasis_offset" 0)
                           :drive (or (jtext d "drive_state") (jtext d "drive") "unknown")))
         nil)
   (list "hippocampus" '("hippocampus/dream-state.json") 48
         (lambda (d) (let ((stats (jtable d "stats")))
                       (list :total-dreams (jnum stats "total_dreams" 0)
                             :videos-remaining-today (jnum stats "videos_remaining_today" 0))))
         nil)
   (list "thalamus" '("thalamus/thalamus-state.json") 48
         (lambda (d) (list :relay-active (jbool d "relay_active" nil)
                           :sensory-routing (or (jget d "sensory_routing") "unknown")))
         nil)
   (list "DMN" '("dmn/dmn-state.json") 48
         (lambda (d)
           (let* ((model (jtable d "self_model"))
                  (narrative (nlk:json-value d :string "self_narrative")))
             (list :active (jbool d "active" nil)
                   :self-narrative (if narrative (subseq narrative 0 (min 120 (length narrative))) "")
                   :core-traits (let ((traits (jarray model "core_traits")))
                                  (coerce (if (> (length traits) 5) (subseq traits 0 5) traits)
                                          'vector))
                   :self-discrepancy (or (jget model "self_discrepancy") "none"))))
         nil)
   (list "OFC" '("ofc/ofc-state.json") 48
         (lambda (d) (list :reward-anticipation (jnum d "reward_anticipation" 0)))
         nil)
   (list "nucleus-accumbens" '("nac/nac-state.json") 24
         (lambda (d) (list :urgency (jnum d "current_urgency" 0)
                           :motivation-level (jnum d "motivation_level" 0)
                           :reward-anticipation (jnum d "reward_anticipation" 0)))
         (lambda (d age) (declare (ignore age)) (> (jnum d "current_urgency" 0) 0.8)))
   (list "somatosensory" '("somatosensory/somatosensory-state.json") 24
         (lambda (d) (list :battery (or (jget d "battery_level") "?")
                           :thermal (or (jget d "thermal_state") "?")))
         nil)
   (list "values" '("values/values-state.json") 168
         (lambda (d)
           (let ((values (jtable d "values")))
             (list :top-value (or (loop for key being the hash-keys of values return key) "none")
                   :total-values (hash-table-count values))))
         nil)
   (list "predictive" '("predictive/predictor-state.json") 48
         (lambda (d)
           (let* ((stats (jtable d "stats"))
                  (accuracy (or (nlk:json-value stats :number "accuracy_rate")
                                (let ((history (jarray d "outcome_history")))
                                  (if history
                                      (/ (count-if (lambda (entry) (= 0 (jnum entry "prediction_error" 1)))
                                                   history)
                                         (length history))
                                      0)))))
             (list :accuracy (round2 (or accuracy 0))
                   :total-predictions (jnum stats "total_predictions" (hash-table-count (jtable d "predictions")))
                   :resolved-cases (+ (jnum stats "matches" 0) (jnum stats "mismatches" 0))
                   :unresolved (jnum stats "unresolved" 0)
                   :unfalsifiable (jnum stats "unfalsifiable" 0))))
         (lambda (d age)
           (declare (ignore age))
           (let ((accuracy (nlk:json-value (jtable d "stats") :number "accuracy_rate")))
             (and accuracy (< accuracy 0.3)))))
   (list "ToM" '("tom/observation-log.json") 48
         (lambda (d) (list :observations (length (jarray d "observations"))
                           :self-model-updates (length (jarray d "self_model_updates"))))
         nil)
   (list "raphe-nuclei" '("raphé/serotonin-state.json" "raphe/raphe-state.json") 48
         (lambda (d) (list :serotonin (or (jget d "current_serotonin") (jget d "level") "?")
                           :patience (or (jget d "patience_state") "?")))
         nil)
   (list "visual-cortex" '("visual/visual-state.json") 168
         (lambda (d) (let ((scenes (jtable d "scenes")))
                       (list :scene-count (hash-table-count scenes)
                             :scenes (coerce (loop for key being the hash-keys of scenes collect key)
                                             'vector))))
         nil)
   (list "habenula" '("habenula/habenula-state.json") 168
         (lambda (d) (list :tonic-suppression (or (jget d "tonic_suppression") "?")
                           :recent-losses (length (jarray d "recent_losses"))))
         nil)
   (list "reticular-formation" '("reticular/reticular-state.json") 24
         (lambda (d) (list :state (or (jget d "state") "?")
                           :arousal-threshold (or (jget d "arousal_threshold") "?")))
         nil))
  "Every subsystem the aggregate folds: name, the files it may live in, how old
it may get, what it contributes, when it is critical.")

;;; --- the fold --------------------------------------------------------------

(defun fold-system (spec brain)
  "One subsystem's entry in the aggregate: (VALUES PLIST STATUS CRITICAL)."
  (destructuring-bind (name relatives max-age readings critical) spec
    (declare (ignore name))
    (multiple-value-bind (data status path) (read-state-file brain relatives)
      (let* ((missing (string= status "missing"))
             (age (if missing 999.0 (reading-age data path)))
             (state-name (cond (missing "missing") ((<= age max-age) "active") (t "stale")))
             (critical-p (and (not missing) critical (funcall critical data age))))
        (values (append (list :status state-name :age-h (round2 age) :max-age-h max-age :critical critical-p)
                        (unless missing (funcall readings data)))
                state-name critical-p)))))

(defun fold (brain)
  "Every subsystem folded: (VALUES ENTRIES SUMMARY) -- ENTRIES is an alist of
name and plist, in the order the kit lists them."
  (let ((entries '()) (active 0) (stale 0) (missing 0) (criticals '()))
    (dolist (spec *systems*)
      (multiple-value-bind (reading state critical) (fold-system spec brain)
        (push (cons (first spec) reading) entries)
        (cond ((string= state "active") (incf active))
              ((string= state "stale") (incf stale))
              (t (incf missing)))
        (when critical (push (first spec) criticals))))
    (setf entries (nreverse entries) criticals (nreverse criticals))
    (let* ((total (length entries))
           (summary (list :timestamp (iso-now)
                          :score (format nil "~d%" (round (* 100 (/ active total))))
                          :healthy active :stale stale :missing missing
                          :critical (not (null criticals)) :criticals (coerce criticals 'vector)
                          :total total)))
      (values entries summary))))

(defun reading-json (plist)
  "One subsystem's reading as a JSON object."
  (let ((object (nlk:make-json-object)))
    (loop for (key value) on plist by #'cddr
          do (setf (gethash (string-downcase (substitute #\_ #\- (symbol-name key))) object)
                   value))
    object))

(defun summary-json (summary)
  (let ((object (nlk:make-json-object)))
    (setf (gethash "score" object) (getf summary :score)
          (gethash "healthy" object) (getf summary :healthy)
          (gethash "stale" object) (getf summary :stale)
          (gethash "missing" object) (getf summary :missing)
          (gethash "critical" object) (getf summary :critical)
          (gethash "total" object) (getf summary :total))
    object))

(defun state (&key (write t))
  "Fold every subsystem state file under <agent>/brain and answer the health
summary plist. With WRITE (the default) the aggregate also lands in
<agent>/brain-state.json, the one file the rest of the agent reads."
  (multiple-value-bind (entries summary) (fold (brain-dir))
    (when write
      (let ((document (nlk:make-json-object))
            (systems (nlk:make-json-object)))
        (dolist (entry entries)
          (setf (gethash (car entry) systems) (reading-json (cdr entry))))
        (setf (gethash "timestamp" document) (getf summary :timestamp)
              (gethash "generated_by" document)
              "nodecode-mind-kit - reads the 25 subsystem files"
              (gethash "health_summary" document) (summary-json summary)
              (gethash "systems" document) systems)
        (let ((path (merge-pathnames "brain-state.json" (agent-dir))))
          (ensure-directories-exist path)
          (with-open-file (stream path :direction :output :if-exists :supersede
                                        :if-does-not-exist :create
                                        :external-format :utf-8)
            (write-string (nlk:encode-json-object document) stream))
          (setf (getf summary :path) (namestring path)))))
    summary))

(defun health-string (summary)
  "SUMMARY as the one line every surface says the same way."
  (format nil "~a/~a healthy, ~a stale, ~a missing~@[ - critical: ~{~a~^, ~}~]"
          (getf summary :healthy) (getf summary :total)
          (getf summary :stale) (getf summary :missing)
          (let ((criticals (coerce (getf summary :criticals) 'list)))
            (and criticals criticals))))

(defun health (&key (write t))
  "The aggregate, one line: 'N/25 healthy, N stale, N missing'."
  (health-string (state :write write)))

(defun seed (&key force)
  "Copy this cell's shipped *.example.json files into <agent>/brain/, one file
per subsystem, and answer how many were written. A file already there is left
alone unless FORCE. The shipped files carry structure, not truth: filling them
with what is actually true is the install, and no pass can do it."
  (let ((source (asdf:system-relative-pathname "nodecode-mind-kit" "brain/"))
        (target (brain-dir))
        (written 0))
    (labels ((copy-one (file)
               (let* ((relative (enough-namestring file source))
                      (clean (subseq relative 0 (- (length relative) (length ".example.json"))))
                      (destination (merge-pathnames (concatenate 'string clean ".json") target)))
                 (when (or force (not (probe-file destination)))
                   (ensure-directories-exist destination)
                   (uiop:copy-file file destination)
                   (incf written))))
             (walk (directory)
               (dolist (file (uiop:directory-files directory))
                 (when (search ".example.json" (file-namestring file) :from-end t)
                   (copy-one file)))
               (dolist (sub (uiop:subdirectories directory))
                 (walk sub))))
      (walk (pathname source)))
    written))

;;; --- the gate ---------------------------------------------------------------

(defparameter *modes* '("standard" "low_scope" "minimal" "essential")
  "The four mode names; only essential changes a verdict, by bypassing quiet hours.")

(defun hour-of (value)
  "'23:00' | 23 | 23.5 | '23.5' as hours past midnight, else NIL."
  (cond
    ((null value) nil)
    ((numberp value) (and (<= 0 value) (< value 24) (coerce value 'double-float)))
    ((stringp value)
     (let ((colon (position #\: value)))
       (if colon
           (let ((hour (parse-int value 0 colon))
                 (minute (parse-int value (1+ colon) (length value))))
             (and hour minute (< hour 24) (< minute 60) (+ hour (/ minute 60.0))))
           (ignore-errors
            (let ((number (read-from-string value)))
              (and (numberp number) (<= 0 number) (< number 24) (coerce number 'double-float)))))))
    (t nil)))

(defun hhmm (hours)
  (let ((minutes (mod (round (* (mod hours 24) 60)) 1440)))
    (format nil "~2,'0d:~2,'0d" (floor minutes 60) (mod minutes 60))))

(defun hour-gap (a b)
  (let ((difference (abs (- a b))))
    (min (mod difference 24.0) (- 24.0 (mod difference 24.0)))))

(defun in-window-p (hour start finish)
  (cond ((= start finish) nil)
        ((> start finish) (or (>= hour start) (< hour finish)))
        (t (and (>= hour start) (< hour finish)))))

(defun quiet-window (entrainment)
  "The quiet window from the entrainment reading, else the documented one.
=> (VALUES START END SOURCE)."
  (dolist (key '("targetSchedule" "target_schedule" "schedule" "quiet_hours" "quietHours"))
    (let ((value (jget entrainment key)))
      (dolist (block (cond ((hash-table-p value) (list value))
                            ((and (vectorp value) (not (stringp value))) (coerce value 'list))))
        (when (hash-table-p block)
          (labels ((first-of (keys)
                     (loop for name in keys
                           for hour = (hour-of (jget block name))
                           when hour return hour)))
            (let ((start (first-of '("quiet_start" "quietStart" "start" "start_hour" "startHour" "from")))
                  (finish (first-of '("quiet_end" "quietEnd" "end" "end_hour" "endHour" "to"))))
              (when (and start finish)
                (return-from quiet-window (values start finish "entrainment")))))))))
  (values +default-quiet-start+ +default-quiet-end+ "default"))

(defun clock (now)
  "The local hour the gate runs on: the real clock, or the NOW override.
=> (VALUES HOUR-IN-HOURS SOURCE)."
  (when now
    (let ((text (string-trim '(#\Space #\Tab) now)))
      (let ((hour (hour-of text)))
        (when hour (return-from clock (values hour "override"))))
      (let ((universal (iso->universal text)))
        (when universal
          (multiple-value-bind (second minute hour) (decode-universal-time universal 0)
            (declare (ignore second))
            (return-from clock (values (+ hour (/ minute 60.0)) "override")))))))
  (multiple-value-bind (second minute hour) (decode-universal-time (get-universal-time))
    (declare (ignore second))
    (values (+ hour (/ minute 60.0)) "system")))

(defparameter *rules*
  ;; docs/DECISION_GATE.md "Gate Logic (v4)", in the order the document prints them.
  (list (list "veto_recovery" "veto" (lambda (g) (getf g :recovery-needed)))
        (list "veto_fatigue" "veto" (lambda (g) (> (getf g :fatigue) 0.70)))
        (list "veto_low_arousal" "veto" (lambda (g) (< (getf g :arousal) 0.30)))
        (list "veto_recovery_state" "veto"
              (lambda (g) (and (equal (getf g :drive) "recovery")
                               (member (getf g :gut) '("pause" "stop") :test #'equal))))
        (list "proceed_habit_gut_stop" "proceed"
              (lambda (g) (and (getf g :strong-habit) (equal (getf g :gut) "stop"))))
        (list "proceed_habit_gut_pause" "proceed"
              (lambda (g) (and (getf g :strong-habit) (equal (getf g :gut) "pause")
                               (>= (getf g :energy) 0.20))))
        (list "proceed_habit_gut_doubt" "proceed"
              (lambda (g) (and (getf g :strong-habit) (equal (getf g :gut) "doubt")
                               (>= (getf g :adj-confidence) 0.30))))
        (list "veto_gut_stop" "veto" (lambda (g) (equal (getf g :gut) "stop")))
        (list "veto_distracted_pause" "veto"
              (lambda (g) (and (getf g :distracted) (equal (getf g :gut) "pause"))))
        (list "veto_distracted_doubt" "veto"
              (lambda (g) (and (getf g :distracted) (equal (getf g :gut) "doubt")
                               (< (getf g :adj-confidence) 0.50))))
        (list "veto_pause_energy" "veto"
              (lambda (g) (and (equal (getf g :gut) "pause") (< (getf g :energy) 0.35))))
        (list "veto_doubt_confidence" "veto"
              (lambda (g) (and (equal (getf g :gut) "doubt") (< (getf g :adj-confidence) 0.40))))
        (list "veto_quiet_gut" "veto"
              (lambda (g) (and (getf g :quiet-hours) (member (getf g :gut) '("pause" "stop") :test #'equal))))
        (list "proceed_passion" "proceed" (lambda (g) (plusp (getf g :active-passions))))
        (list "proceed_conviction" "proceed" (lambda (g) (plusp (getf g :active-convictions))))
        (list "proceed_curiosity_extended" "proceed" (lambda (g) (plusp (getf g :emergent-interests))))
        (list "proceed_extended_surge" "proceed"
              (lambda (g) (and (> (getf g :novelty) 0.7) (> (getf g :vta) 0.7))))
        (list "proceed_novelty_extended" "proceed" (lambda (g) (> (getf g :novelty) 0.7)))
        (list "proceed_vta_extended" "proceed" (lambda (g) (> (getf g :vta) 0.7)))
        (list "caution_distracted" "caution"
              (lambda (g) (and (getf g :distracted) (equal (getf g :gut) "go_ahead"))))
        (list "caution_gut" "caution" (lambda (g) (member (getf g :gut) '("pause" "wait") :test #'equal)))
        (list "caution_doubt" "caution"
              (lambda (g) (and (equal (getf g :gut) "doubt") (>= (getf g :adj-confidence) 0.40))))
        (list "caution_fatigue" "caution" (lambda (g) (> (getf g :fatigue) 0.50)))
        (list "caution_low_arousal" "caution" (lambda (g) (< (getf g :arousal) 0.50)))
        (list "caution_tension" "caution" (lambda (g) (> (getf g :tension) 0.70)))
        (list "caution_quiet_hours" "caution" (lambda (g) (getf g :quiet-hours)))))

(defparameter *rule-inputs*
  '(;; the inputs each reason is decided on, so a refusal reads back to its numbers
    ("veto_recovery" :recovery-needed :fatigue)
    ("veto_fatigue" :fatigue)
    ("veto_low_arousal" :arousal)
    ("veto_recovery_state" :drive :gut)
    ("proceed_habit_gut_stop" :strong-habit :habit-strength :gut)
    ("proceed_habit_gut_pause" :strong-habit :habit-strength :gut :energy)
    ("proceed_habit_gut_doubt" :strong-habit :habit-strength :gut :adj-confidence)
    ("veto_gut_stop" :gut :tension)
    ("veto_distracted_pause" :distracted :gut :energy)
    ("veto_distracted_doubt" :distracted :gut :adj-confidence)
    ("veto_pause_energy" :gut :energy)
    ("veto_doubt_confidence" :gut :adj-confidence :surprise-accuracy)
    ("veto_quiet_gut" :quiet-hours :gut)
    ("proceed_passion" :active-passions)
    ("proceed_conviction" :active-convictions)
    ("proceed_curiosity_extended" :emergent-interests)
    ("proceed_extended_surge" :novelty :vta)
    ("proceed_novelty_extended" :novelty :gut)
    ("proceed_vta_extended" :vta :gut)
    ("caution_distracted" :distracted :gut :confidence)
    ("caution_gut" :gut :energy :fatigue)
    ("caution_doubt" :gut :adj-confidence)
    ("caution_fatigue" :fatigue)
    ("caution_low_arousal" :arousal)
    ("caution_tension" :tension)
    ("caution_quiet_hours" :quiet-hours :real-local-hour)
    ("proceed" :gut :energy :fatigue :adj-confidence)))

(defun gate (&key label (mode nil) now)
  "PROCEED / CAUTION / VETO from the subsystem readings, read-only.
=> a plist with :VERDICT, :REASON, :EXIT-CODE, :DETAIL, :INPUTS, :WARNINGS.
LABEL names the decision; MODE is one of standard, low_scope, minimal,
essential (essential bypasses quiet hours); NOW pins the clock ('23:30' or an
ISO timestamp) for a caller that has to assert a quiet-hours rule."
  (let ((brain (brain-dir)) (mode-name (or mode (setting :mode) "standard")) (warnings '()))
    (unless (member mode-name *modes* :test #'string=)
      (push (format nil "unknown mode ~s; running the standard rule set" mode-name) warnings)
      (setf mode-name "standard"))
    (flet ((read-of (relatives)
             (multiple-value-bind (data status path) (read-state-file brain relatives)
               (declare (ignore path))
               (values data status))))
      (multiple-value-bind (somatic s-status) (read-of '("somatic/markers.json"))
        (multiple-value-bind (fatigue f-status) (read-of '("fatigue/fatigue-state.json"))
          (multiple-value-bind (novelty n-status) (read-of '("dopamine/novelty-state.json"))
            (multiple-value-bind (attention a-status) (read-of '("attention/attention-state.json"))
              (multiple-value-bind (hypothalamus h-status)
                  (read-of '("hypothalamus/hypothalamus-state.json" "hypothalamus-state.json"))
                (multiple-value-bind (habits b-status) (read-of '("basal-ganglia/habit-state.json"))
                  (multiple-value-bind (prefrontal p-status)
                      (read-of '("prefrontal/interest-emergence-state.json"))
                    (multiple-value-bind (surprise s-status2) (read-of '("predictive/surprise-log.json"))
                      (multiple-value-bind (scn c-status) (read-of '("scn/scn-state.json"))
                        (multiple-value-bind (entrainment e-status) (read-of '("entrainment-state.json"))
                          (multiple-value-bind (vta v-status) (read-of '("dopamine/vta-state.json"))
                            (let* ((sources (list (cons "brain/somatic/markers.json" s-status)
                                                  (cons "brain/fatigue/fatigue-state.json" f-status)
                                                  (cons "brain/dopamine/novelty-state.json" n-status)
                                                  (cons "brain/attention/attention-state.json" a-status)
                                                  (cons "brain/hypothalamus/hypothalamus-state.json" h-status)
                                                  (cons "brain/basal-ganglia/habit-state.json" b-status)
                                                  (cons "brain/prefrontal/interest-emergence-state.json" p-status)
                                                  (cons "brain/predictive/surprise-log.json" s-status2)
                                                  (cons "brain/scn/scn-state.json" c-status)
                                                  (cons "brain/entrainment-state.json" e-status)
                                                  (cons "brain/dopamine/vta-state.json" v-status)))
                                   (gut (or (jtext somatic "gut_feeling") "neutral"))
                                   (energy (jnum somatic "energy" 0.5))
                                   (confidence (jnum somatic "confidence" 0.5))
                                   (tension (jnum somatic "tension" 0.0))
                                   (fatigue-level (jnum fatigue "fatigue_level" 0.0))
                                   (recovery-needed (jbool fatigue "recovery_needed" nil))
                                   (novelty-seeking (jnum novelty "novelty_seeking" 0.5))
                                   (vta-drive (jnum vta "current_drive" (jnum vta "currentDrive" 0.57)))
                                   (emerged (jarray prefrontal "emerged_interests"))
                                   (convictions (jarray prefrontal "convictions"))
                                   (passions (jarray prefrontal "passions"))
                                   (distracted (equal "distracted" (or (jtext attention "mode") "unknown")))
                                   (arousal (jnum hypothalamus "arousal_level" (jnum hypothalamus "arousal" 0.5)))
                                   (drive (or (jtext hypothalamus "drive_state") (jtext hypothalamus "drive") "unknown"))
                                   (habit-strength
                                     (let ((best 0.0))
                                       (loop for entry across (jarray habits "habits")
                                             when (and (hash-table-p entry)
                                                       (member (or (jtext entry "status") "active")
                                                               '("active" "forming") :test #'string=))
                                               do (setf best (max best (jnum entry "strength" 0.0))))
                                       best))
                                   (accuracy (jnum (jtable surprise "stats") "accuracy_rate" 0.5))
                                   (adjusted (clamp (+ confidence (* (- accuracy 0.5) 0.3)) 0.1 0.99)))
                              (multiple-value-bind (real-hour clock-source) (clock now)
                                (multiple-value-bind (quiet-start quiet-end window-source)
                                    (quiet-window entrainment)
                                  (let* ((real-quiet (in-window-p real-hour quiet-start quiet-end))
                                         (scn-phase (or (jtext scn "phase") "unknown"))
                                         (scn-says-quiet (or (string= scn-phase "night")
                                                             (jbool scn "quiet_hours" nil)))
                                         (scn-hour (hour-of (or (jget scn "local_hour")
                                                                (jget scn "localHour"))))
                                         (scn-age (and (string= c-status "read")
                                                       (reading-age scn (probe-file
                                                                         (merge-pathnames "scn/scn-state.json" brain)))))
                                         (scn-fresh (and scn-age (<= scn-age 2.0)))
                                         (scn-drift (and scn-hour (> (hour-gap scn-hour real-hour) 2.0)))
                                         (raw-quiet (if (and (string= c-status "read") scn-fresh (not scn-drift))
                                                        (or scn-says-quiet real-quiet)
                                                        real-quiet))
                                         (essential (string= mode-name "essential"))
                                         (quiet (and raw-quiet (not essential)))
                                         (g (list :recovery-needed recovery-needed
                                                  :fatigue fatigue-level
                                                  :arousal arousal
                                                  :drive drive
                                                  :gut gut
                                                  :distracted distracted
                                                  :adj-confidence adjusted
                                                  :energy energy
                                                  :tension tension
                                                  :novelty novelty-seeking
                                                  :vta vta-drive
                                                  :strong-habit (>= habit-strength 0.7)
                                                  :habit-strength habit-strength
                                                  :active-passions (length (entries-by passions "status"))
                                                  :active-convictions (length (entries-by convictions "status"))
                                                  :emergent-interests (length emerged)
                                                  :surprise-accuracy accuracy
                                                  :quiet-hours quiet
                                                  :real-local-hour (round2 real-hour)
                                                  :confidence confidence)))
                                    (when (and (string= c-status "missing"))
                                      (push "no SCN reading; using the real clock for quiet hours" warnings))
                                    (when (and (string= c-status "read") (not scn-fresh))
                                      (push "SCN phase stale; using the real clock for quiet hours" warnings))
                                    (when (and (string= c-status "read") scn-fresh scn-drift)
                                      (push "SCN drift: the real clock is more than two hours away; using it" warnings))
                                    (when (and essential raw-quiet)
                                      (push "mode essential: quiet hours bypassed" warnings))
                                    (let* ((found (or (find-if (lambda (rule) (funcall (third rule) g)) *rules*)
                                                      (list "proceed" "proceed" nil)))
                                           (reason (first found)) (verdict (second found))
                                           (inputs (rest (assoc reason *rule-inputs* :test #'string=)))
                                           (detail (format nil "~a: ~{~(~a~)=~s~^, ~}" reason
                                                           (loop for key in inputs
                                                                 collect (string-downcase (symbol-name key))
                                                                 collect (getf g key))))
                                           (missing (loop for (path . status) in sources
                                                          unless (string= status "read") collect path)))
                                      (list :decision (or label "manual_check")
                                            :mode mode-name
                                            :reason reason :verdict verdict
                                            :exit-code (cond ((string= verdict "proceed") 0)
                                                             ((string= verdict "veto") 1)
                                                             (t 2))
                                            :detail detail :inputs g
                                            :quiet-hours quiet :quiet-hours-source (if quiet clock-source "none")
                                            :quiet-hours-window (list (hhmm quiet-start) (hhmm quiet-end) window-source)
                                            :real-local-hour (round2 real-hour)
                                            :clock-source clock-source
                                            :read (loop for (path . status) in sources
                                                        when (string= status "read") collect path)
                                            :defaulted (loop for (path . status) in sources
                                                             unless (string= status "read") collect path)
                                            :missing missing
                                            :warnings (nreverse warnings)))))))))))))))))))))
;;; --- what the model and the operator call ------------------------------------

(defparameter *state-schema*
  (list :description
        "How the agent is doing right now: fold the 25 subsystem state files under the agent directory into brain-state.json and answer one health line. Reads files; needs no model call."
        :parameters (nlk:make-json-object "type" "object" "properties" (nlk:make-json-object))))

(defparameter *gate-schema*
  (list :description
        "Gate one decision on the measured state: PROCEED, CAUTION or VETO, with the reason code and the numbers behind it. Read-only; every value behind a missing state file is reported as a default."
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties"
         (nlk:make-json-object
          "decision" (nlk:make-json-object
                      "type" "string"
                      "description" "the decision being gated, one short phrase")
          "mode" (nlk:make-json-object
                  "type" "string"
                  "enum" (vector "standard" "low_scope" "minimal" "essential")
                  "description" "essential bypasses quiet hours; the rule set is the same")))))

(defun critical-string (summary)
  (let ((criticals (coerce (getf summary :criticals) 'list)))
    (if criticals (format nil "; critical: ~{~a~^, ~}" criticals) "")))

(defun brain-state-tool (arguments)
  "The brain_state tool: the aggregate, one line, and the file it wrote."
  (declare (ignore arguments))
  (let ((summary (state)))
    (format nil "~a~a~@[ (brain-state.json written to ~a)~]"
            (health-string summary) (critical-string summary) (getf summary :path))))

(defun gate-tool (arguments)
  "The decision_gate tool: the verdict line a caller reads back."
  (let ((result (gate :label (nlk:json-value arguments :string "decision")
                      :mode (nlk:json-value arguments :string "mode"))))
    (format nil "~a (exit ~a): ~a~@[ - ~{~a~^; ~}~]"
            (string-upcase (getf result :verdict)) (getf result :exit-code)
            (getf result :detail) (getf result :warnings))))

(defun mind-command (args session-id)
  "One /mind invocation: the aggregate and the gate on the state as it stands."
  (declare (ignore args session-id))
  (let ((summary (state)) (result (gate :label "manual_check")))
    (format nil "~a~a~%~a: ~a (exit ~a) - ~a"
            (health-string summary) (critical-string summary)
            "gate" (string-upcase (getf result :verdict)) (getf result :exit-code)
            (getf result :detail))))

;;; --- the manual -------------------------------------------------------------

(defparameter +summary+
  "Meters for a long-running agent: 25 subsystem state files, one folded brain-state.json, and a read-only PROCEED / CAUTION / VETO gate")

(defparameter +manual+
  "The mind kit gives an agent internal readings -- energy, fatigue, circadian phase, attention, conflict, motivation, novelty, open questions -- folds them into one file the rest of the agent reads, and gates a decision on them. It decides nothing by itself: it reads and writes files, and says stale or missing rather than guessing.

The section: agent_dir is the directory the agent's own state lives in; the kit reads $AGENT_DIR/brain/<subsystem>/<name>.json and writes $AGENT_DIR/brain-state.json. mode is the gate's default rule set (standard, low_scope, minimal, essential -- only essential changes a verdict, by bypassing quiet hours).

1. Seed, then fold. (mind-kit:seed) copies the 25 shipped *.example.json files into the agent directory, leaving any file already there; (mind-kit:state) folds them into brain-state.json and answers a health line. The shipped files carry structure, not truth: filling them with what is actually true for this agent is the install, and no pass can do it.

2. Keep it honest. Every reading carries its age and the age it is allowed to reach, so a subsystem that stopped updating reads stale instead of asserting an old number -- and one whose file is gone reads missing, never stale. A state file nothing updates ages into fiction: if the agent's own passes (snapshot, maintenance, consolidation) have not run, say so when reporting state rather than presenting yesterday's numbers as now.

3. Gate on the numbers. (mind-kit:gate) returns PROCEED (exit 0), CAUTION (exit 2) or VETO (exit 1) with a reason code and the inputs behind it, so a refusal traces to a number instead of to a mood. It is read-only: gating never mutates the state that produced it, and every value it had to default because a file was absent is reported as a default. (mind-kit:gate :now \"23:30\") pins the clock; :mode \"essential\" bypasses quiet hours.

4. Report honestly. State is a reading, not a feeling: when asked how you are, answer from the aggregate and say what is stale; do not upgrade a number into a claim about experience.")

;;; --- the cell -----------------------------------------------------------------

(defun state-file-count (directory)
  (let ((count 0))
    (labels ((walk (dir)
               (when (probe-file dir)
                 (dolist (file (uiop:directory-files dir))
                   (unless (search ".example.json" (file-namestring file) :from-end t)
                     (when (string-equal "json" (pathname-type file)) (incf count))))
                 (dolist (sub (uiop:subdirectories dir)) (walk sub)))))
      (walk directory))
    count))

(nle:define-cell mind-kit
  (:section ("mind-kit")
    (:guide "agent_dir is where this agent's own state lives - the kit reads <agent_dir>/brain/ and writes <agent_dir>/brain-state.json; (mind-kit:seed) copies the shipped examples in there; mode is the gate's default rule set")
    ("agent_dir" :path :required t :doc "the directory holding brain/ and brain-state.json")
    ("mode" :choice :options '("standard" "low_scope" "minimal" "essential") :default "standard"
     :doc "the decision gate's rule set; essential bypasses quiet hours"))
  (:tool "brain_state" *state-schema* 'brain-state-tool)
  (:tool "decision_gate" *gate-schema* 'gate-tool)
  (:command "mind" 'mind-command :description "How the agent is doing, and the gate's verdict")
  (:help :mind-kit +summary+ +manual+)
  (:start (lambda ()
            (ensure-directories-exist (brain-dir))
            (let ((seeded (state-file-count (brain-dir))))
              (if (plusp seeded)
                  (let ((summary (state)))
                    (nle:notice (format nil "mind-kit: ~a~a" (health-string summary)
                                        (critical-string summary))
                                :key "nodecode-mind-kit"))
                  (nle:notice (format nil "mind-kit: no state files under ~a yet - (mind-kit:seed) copies the examples"
                                      (namestring (brain-dir)))
                              :level :warning :key "nodecode-mind-kit"))
              (nle:on-stop (lambda () (nle:notice nil :key "nodecode-mind-kit")))))))
