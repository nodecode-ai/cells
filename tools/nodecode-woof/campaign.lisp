;;;; campaign.lisp --- several runs of one plan, differing along one axis.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The original keeps a campaign as a spec (a base plan, one axis, the
;;;; values to try) that it sizes arm by arm and then runs one after another,
;;;; because one card runs one forecast at a time. This is that: the same
;;;; spec, the same per-arm sizing through WOOF's own resolver and estimator,
;;;; and a queue that starts the next arm when the one before it has ended.
;;;; The campaign is a folder under runs_dir/campaigns/, so nothing lives
;;;; only in memory.

(in-package #:nodecode-woof)

;;; --- reading a spec -----------------------------------------------------------------

(defun json-copy (object)
  "OBJECT again, as a fresh JSON value: no arm's edits reach another's plan."
  (and object (nlk:decode-json (nlk:encode-json-object object))))

(defun member-value (object key)
  "KEY in the JSON object OBJECT, or NIL."
  (and (hash-table-p object) (gethash key object)))

(defun path-parts (text)
  "PATH split at its dots, as the keys of a nested JSON object."
  (when (and (stringp text) (plusp (length text)))
    (loop with start = 0
          for dot = (position #\. text :start start)
          collect (subseq text start dot)
          while dot do (setf start (1+ dot)))))

(defun set-plan-path (object path value)
  "Set VALUE at PATH (a dotted key) inside the JSON object OBJECT, and answer OBJECT."
  (let* ((parts (path-parts path))
         (last (car (last parts))))
    (when (null parts)
      (fail "the axis must name a key of the base plan, like \"physics\" or \"domain.cells\""))
    (let ((cursor object))
      (dolist (part (butlast parts))
        (let ((next (member-value cursor part)))
          (unless (hash-table-p next)
            (setf next (nlk:make-json-object))
            (setf (gethash part cursor) next))
          (setf cursor next)))
      (setf (gethash last cursor) value))
    object))

(defun arm-label (value fallback)
  "What an arm is called: the value itself when it is short, else its own name."
  (let ((named (cond ((stringp value) value)
                     ((hash-table-p value)
                      (or (member-value value "label")
                          (member-value value "name")
                          (member-value value "preset")))
                     ((numberp value) (princ-to-string value))
                     (t nil))))
    (let ((label (nlk:trimmed (or named fallback))))
      (if (plusp (length label)) (subseq label 0 (min 40 (length label))) "arm"))))

(defun slug-of (text)
  "TEXT as a folder name: letters, digits and dashes."
  (let ((out (make-string-output-stream)))
    (loop for character across (or text "campaign")
          do (write-char (if (or (alphanumericp character) (member character '(#\- #\_ #\.) :test #'char=))
                             character
                             #\-)
                         out))
    (let ((slug (string-downcase (get-output-stream-string out))))
      (subseq slug 0 (min 40 (length slug))))))

;;; --- the campaign on disk --------------------------------------------------------------

(defun campaigns-root ()
  "Where campaigns live: campaigns/ under runs_dir."
  (ensure-directories-exist (merge-pathnames "campaigns/" (runs-root))))

(defun campaign-directory (campaign-id)
  (ensure-directories-exist (merge-pathnames (format nil "~a/" campaign-id) (campaigns-root))))

(defun campaign-path (campaign-id)
  (merge-pathnames "campaign.json" (campaign-directory campaign-id)))

(defun read-campaign (campaign-id)
  "CAMPAIGN-ID's record, or a refusal naming it."
  (or (and campaign-id (plusp (length campaign-id)) (read-json (campaign-path campaign-id)))
      (fail "no campaign ~a under ~a; plan one with action=plan first"
            campaign-id (uiop:native-namestring (campaigns-root)))))

(defun save-campaign (campaign)
  "CAMPAIGN written back, and answered."
  (write-json (campaign-path (member-value campaign "campaign_id")) campaign))

(defun campaign-arms (campaign)
  "CAMPAIGN's arms, as a list of hash tables."
  (coerce (or (member-value campaign "arms") #()) 'list))

;;; --- sizing one arm ---------------------------------------------------------------------

(defun number-in (object path)
  "The number at PATH inside OBJECT, or NIL when it is not a number."
  (let ((value (loop with cursor = object
                     for part in (path-parts path)
                     do (setf cursor (member-value cursor part))
                     finally (return cursor))))
    (and (numberp value) value)))

(defun gib (bytes)
  (and (numberp bytes) (one-decimal (/ bytes 1073741824))))

(defun one-decimal (number)
  "NUMBER to one decimal place, as a JSON number."
  (/ (fround (* (coerce number 'double-float) 10)) 10))

(defun arm-sizing (estimate)
  "What WOOF's own estimator said about one arm, as the campaign counts it."
  (list :wall (or (number-in estimate "expected_pace.wall_seconds_high")
                   (number-in estimate "expected_pace.wall_seconds_low"))
        :wall_low (number-in estimate "expected_pace.wall_seconds_low")
        :wall_high (number-in estimate "expected_pace.wall_seconds_high")
        :disk_gib (gib (number-in estimate "disk.total_bytes"))
        :download_gib (gib (number-in estimate "download.bytes"))
        :pace (or (member-value (member-value estimate "expected_pace") "sentence") "")))

(defun size-plan (plan-path)
  "WOOF's resolver and estimator on one arm's plan. => (values ESTIMATE REFUSAL)."
  (multiple-value-bind (resolved resolve-err resolve-status)
      (run-woof (list (woof-command) "run-plan" (uiop:native-namestring plan-path) "--resolve"))
    (declare (ignore resolved))
    (multiple-value-bind (estimate estimate-err estimate-status)
        (run-woof (list (woof-command) "run-plan" (uiop:native-namestring plan-path) "--estimate"))
      (if (and (= 0 resolve-status) (= 0 estimate-status))
          (values (json-document estimate) nil)
          (values nil (tail-text (concatenate 'string resolve-err estimate-err) 800))))))

;;; --- action: plan ------------------------------------------------------------------------

(defun plan-campaign (call)
  "Build a campaign's arms from its spec, size each one, and answer the table."
  (let* ((spec (campaign-call-spec call))
         (axis (and (hash-table-p spec) (member-value spec "axis")))
         (values-list (coerce (or (and (hash-table-p spec) (member-value spec "values")) #()) 'list))
         (base (and (hash-table-p spec) (member-value spec "base")))
         (budget (and (hash-table-p spec) (member-value spec "budget"))))
    (unless (and (stringp axis) (plusp (length axis)))
      (fail "a campaign spec needs an axis: the key of the base plan to vary, like \"physics\" or \"cycle\""))
    (unless (hash-table-p base)
      (fail "a campaign spec needs a base plan: the same object woof_plan takes, which every arm starts from"))
    (when (null values-list)
      (fail "a campaign spec needs values: what to put at ~s in each arm" axis))
    (when (> (length values-list) 12)
      (fail "a campaign is at most 12 arms; split the question into two campaigns"))
    (let* ((name (or (member-value spec "name") axis))
           (campaign-id (format nil "campaign-~d-~4,'0d-~a"
                                (get-universal-time) (random 10000) (slug-of name)))
           (directory (campaign-directory campaign-id))
           (arms '()))
      (loop for value in values-list
            for index from 1
            do (let* ((arm-id (format nil "a~d" index))
                      (label (arm-label value (format nil "arm ~d" index)))
                      (plan (set-plan-path (json-copy base) axis (json-copy value)))
                      (path (write-json (merge-pathnames (format nil "~a/plan.json" arm-id) directory)
                                        plan)))
                 (multiple-value-bind (estimate refusal) (size-plan path)
                   (let ((sizing (and estimate (arm-sizing estimate))))
                     (push (nlk:make-json-object
                            "arm_id" arm-id
                            "label" label
                            "value" (princ-to-string (if (stringp value) value
                                                         (nlk:encode-json-object value)))
                            "plan" (uiop:native-namestring path)
                            "state" (if estimate "planned" "skipped")
                            "refused" refusal
                            "wall_seconds" (or (getf sizing :wall) 0)
                            "disk_gib" (or (getf sizing :disk_gib) 0)
                            "download_gib" (or (getf sizing :download_gib) 0)
                            "pace" (or (getf sizing :pace) ""))
                           arms)))))
      (setf arms (nreverse arms))
      (let* ((wall (loop for arm in arms sum (or (member-value arm "wall_seconds") 0)))
             (disk (loop for arm in arms sum (or (member-value arm "disk_gib") 0)))
             (download (loop for arm in arms sum (or (member-value arm "download_gib") 0)))
             (refused (remove-if-not (lambda (arm) (member-value arm "refused")) arms))
             (notes '())
             (within t))
        (let ((max-wall (member-value budget "max_wall_minutes"))
              (max-disk (member-value budget "max_disk_gib")))
          (when (and (numberp max-wall) (> wall (* max-wall 60)))
            (setf within nil)
            (push (format nil "wall ~d s is over ~a min" (round wall) max-wall) notes))
          (when (and (numberp max-disk) (> disk max-disk))
            (setf within nil)
            (push (format nil "disk ~,1f GiB is over ~a GiB" disk max-disk) notes)))
        (let ((campaign (nlk:make-json-object
                         "schema" "nodecode-woof.campaign.v1"
                         "campaign_id" campaign-id
                         "name" (princ-to-string name)
                         "question" (or (member-value spec "question") "")
                         "axis" axis
                         "spec" spec
                         "arms" (coerce arms 'vector)
                         "total" (nlk:make-json-object "wall_seconds" (round wall)
                                                        "disk_gib" (one-decimal disk)
                                                        "download_gib" (one-decimal download))
                         "refused" (length refused)
                         "within_budget" (and within t)
                         "budget_notes" (coerce (nreverse notes) 'vector)
                         "state" "planned"
                         "created" (princ-to-string (get-universal-time))
                         "runs_serially" "one card runs one arm at a time; action=status starts the next")))
          (save-campaign campaign)
          (nlk:encode-json-object
           (nlk:make-json-object
            "ok" t
            "campaign_id" campaign-id
            "folder" (uiop:native-namestring directory)
            "axis" axis
            "arms" (nlk:json-value campaign :array "arms")
            "total" (nlk:json-value campaign :object "total")
            "within_budget" (and within t)
            "summary" (format nil "campaign ~a: ~d arms along ~a, ~d s of wall (~a), ~,1f GiB disk; ~@[over budget: ~a; ~]~d arm(s) refused"
                               campaign-id (length arms) axis (round wall)
                               (if within "within budget" "over budget")
                               disk
                               (and notes (format nil "~{~a~^; ~}" (reverse notes)))
                               (length refused))
            "next" "call woof_campaign with action=start and this campaign_id")))))))

;;; --- action: start, status, stop -------------------------------------------------------------

(defun arm-run-state (arm)
  "What ARM's run is doing, read from the run's own record."
  (let ((run-id (member-value arm "run_id")))
    (if (and run-id (plusp (length run-id)))
        (let ((record (read-record run-id)))
          (if record (run-state record) "lost"))
        (or (member-value arm "state") "planned"))))

(defun score-summary (run-id)
  "The headline a run's score.json carries, or NIL when it was never scored."
  (let ((score (read-json (merge-pathnames "score.json" (run-directory run-id)))))
    (or (and score (member-value score "summary")) nil)))

(defun start-arm (campaign arm)
  "Start one arm's run through the same path woof_launch uses. => the arm, updated."
  (let* ((plan (read-json (member-value arm "plan")))
         (label (format nil "~a / ~a" (member-value campaign "name") (member-value arm "label")))
         (record (submit-run plan label (member-value campaign "campaign_id"))))
    (setf (gethash "run_id" arm) (record-value record "run_id")
          (gethash "state" arm) "running")
    arm))

(defun advance-campaign (campaign)
  "Run the queue on: with no arm running, start the next one that is planned."
  (let* ((arms (campaign-arms campaign))
         (running (loop for arm in arms
                        when (equal (arm-run-state arm) "running") collect arm))
         (pending (loop for arm in arms
                        when (equal (member-value arm "state") "planned") collect arm))
         (ending (loop for arm in arms
                       when (equal (arm-run-state arm) "ended") collect arm)))
    (declare (ignore ending))
    (when (and (null running) pending)
      (start-arm campaign (first pending)))
    (when (and (null running) (null pending))
      (setf (gethash "state" campaign) "done"))
    (save-campaign campaign)
    campaign))

(defun campaign-call-answer (campaign)
  "CAMPAIGN as the tool answers it: every arm's run, what it wrote, and its score."
  (let* ((arms (campaign-arms campaign))
         (rows (loop for arm in arms
                     collect (let* ((run-id (member-value arm "run_id"))
                                    (state (arm-run-state arm))
                                    (score (and run-id (plusp (length run-id))
                                                (score-summary run-id))))
                               (nlk:make-json-object
                                "arm_id" (member-value arm "arm_id")
                                "label" (member-value arm "label")
                                "run_id" (or run-id "")
                                "state" state
                                "wall_seconds" (member-value arm "wall_seconds")
                                "disk_gib" (member-value arm "disk_gib")
                                "maps" (if (and run-id (plusp (length run-id)))
                                           (length (ignore-errors (image-paths run-id)))
                                           0)
                                "score" (or score nil)
                                "refused" (or (member-value arm "refused") "")))))
         (done (count-if (lambda (row) (member (member-value row "state")
                                               '("ended" "stopped" "skipped" "failed")
                                               :test #'string=))
                         rows)))
    (nlk:encode-json-object
     (nlk:make-json-object
      "ok" t
      "campaign_id" (member-value campaign "campaign_id")
      "name" (member-value campaign "name")
      "axis" (member-value campaign "axis")
      "state" (member-value campaign "state")
      "arms" (coerce rows 'vector)
      "finished" done
      "of" (length rows)
      "summary" (format nil "campaign ~a: ~a; ~d of ~d arms settled"
                         (member-value campaign "campaign_id")
                         (member-value campaign "state") done (length rows))
      "next" (if (equal (member-value campaign "state") "done")
                 "compare the arms with woof_report campaign_id=..., then woof_compare two of their runs"
                 "call woof_campaign action=status again to start the next arm when this one ends")))))

(defun start-campaign (campaign)
  "action=start: launch the first planned arm and leave the rest queued."
  (when (equal (member-value campaign "state") "running")
    (fail "campaign ~a is already running; ask action=status" (member-value campaign "campaign_id")))
  (let ((pending (loop for arm in (campaign-arms campaign)
                       when (equal (member-value arm "state") "planned") collect arm)))
    (unless pending
      (fail "campaign ~a has no arm left to start: every arm was refused"
            (member-value campaign "campaign_id")))
    (setf (gethash "state" campaign) "running"
          (gethash "started" campaign) (princ-to-string (get-universal-time)))
    (start-arm campaign (first pending))
    (save-campaign campaign)
    (campaign-call-answer campaign)))

(defun stop-campaign (campaign)
  "action=stop: signal the running arm, and leave the plan for the arms behind it."
  (let ((arms (campaign-arms campaign)))
    (dolist (arm arms)
      (let ((run-id (member-value arm "run_id")))
        (cond ((and run-id (plusp (length run-id))
                    (equal (arm-run-state arm) "running"))
               (let ((record (read-record run-id)))
                 (when record (signal-pid (or (record-value record "pid" :integer) 0) "TERM")))
               (setf (gethash "state" arm) "stopped"))
              ((equal (member-value arm "state") "planned")
               (setf (gethash "state" arm) "skipped")))))
    (setf (gethash "state" campaign) "stopped")
    (save-campaign campaign)
    (campaign-call-answer campaign)))

(defun campaign (call)
  "woof_campaign: plan, start, watch and stop a set of runs that differ along one axis."
  (let ((action (campaign-call-action call))
        (campaign-id (nlk:trimmed (campaign-call-campaign-id call))))
    (cond ((equal action "plan") (plan-campaign call))
          ((equal action "start")
           (start-campaign (read-campaign campaign-id)))
          ((equal action "status")
           (campaign-call-answer (advance-campaign (read-campaign campaign-id))))
          ((equal action "stop")
           (stop-campaign (read-campaign campaign-id)))
          (t (fail "~a is not a campaign action: use plan, start, status or stop" action)))))
