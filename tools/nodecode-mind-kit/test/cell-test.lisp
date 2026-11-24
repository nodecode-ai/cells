;;;; cell-test.lisp --- the fold, the gate's reason codes, seeding, and the lifecycle.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Everything here is offline: the kit reads and writes JSON files in a scratch
;;;; agent directory under the test home and calls no provider, no network and no
;;;; clock but the pinned one.

(in-package #:nodecode.test)

(define-test-slice "mind-kit" "MIND-KIT-CELL-" :start nodecode-mind-kit:start-cell)

;;; --- fixtures ---------------------------------------------------------------

(defun mind-kit-test-dir (&optional (name "agent"))
  "A scratch agent directory of NAMEs own, created."
  (let ((dir (nlk:home (format nil "mind-kit-test/~a/" name))))
    (ensure-directories-exist dir)
    dir))

(defun mind-kit-put (directory relative text)
  "TEXT at RELATIVE inside DIRECTORY, parents created."
  (let ((path (merge-pathnames relative (uiop:ensure-directory-pathname (pathname directory)))))
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output :if-exists :supersede
                                   :if-does-not-exist :create :external-format :utf-8)
      (write-string text stream))
    path))

(defmacro with-mind-kit ((name &rest pairs) &body body)
  "BODY with the cell started on agent directory NAME and stopped after."
  `(let ((directory (mind-kit-test-dir ,name)))
     (with-cell-stop ((mind-kit-start "agent_dir" (namestring directory) ,@pairs))
       ,@body)))

;;; --- the lifecycle ----------------------------------------------------------

(define-cell-lifecycle-tests "mind-kit"
  (:config "agent_dir" (namestring (mind-kit-test-dir)))
  (:help :mind-kit)
  (:command "mind")
  (:running (is (nle::find-tool "brain_state") "the aggregate is a tool the model can call")
            (is (nle::find-tool "decision_gate") "so is the gate")
            (is (search "PROCEED" (nle:help :mind-kit)) "the manual names the verdicts"))
  (:stopped (is (null (nle::find-tool "brain_state")) "stop takes the tools back"))
  (:refused ("mode" "nonsense")))

;;; --- seeding ----------------------------------------------------------------

(deftest mind-kit-cell-seeds-the-example-files ()
  (with-mind-kit ("seed")
    (let ((written (mind-kit:seed)))
      (is (plusp written) "the shipped examples are copied in")
      (is (probe-file (merge-pathnames "brain/fatigue/fatigue-state.json" (mind-kit:agent-dir))) "one file per subsystem, named without .example")
      (is (zerop (mind-kit:seed)) "a second seed leaves what is already there"))))

;;; --- the fold ---------------------------------------------------------------

(deftest mind-kit-cell-folds-the-subsystem-files ()
  (with-mind-kit ("fold")
    (mind-kit-put (mind-kit:agent-dir) "brain/somatic/markers.json"
                  "{\"energy\": 0.3, \"gut_feeling\": \"go_ahead\", \"lastUpdated\": \"not-a-time\"}")
    (mind-kit-put (mind-kit:agent-dir) "brain/fatigue/fatigue-state.json"
                  "{\"fatigue_level\": 0.9, \"recovery_needed\": true}")
    (let* ((directory (mind-kit:agent-dir))
           (summary (mind-kit:state))
           (path (merge-pathnames "brain-state.json" directory))
           (document (and (probe-file path)
                          (nlk:decode-json (uiop:read-file-string path :external-format :utf-8)
                                           :whole t))))
      (is (= 25 (getf summary :total)) "all 25 subsystems are folded")
      (is (= 2 (getf summary :healthy)) "the two files that are there read active")
      (is (= 23 (getf summary :missing)) "an absent subsystem is missing, never stale")
      (is (getf summary :critical) "a fatigue reading that needs recovery is critical")
      (is (equal "8%" (nlk:json-value (nlk:json-value document :object "health_summary")
                                        :string "score")) "the score is the active share")
      (let ((somatic (nlk:json-value (nlk:json-value document :object "systems")
                                      :object "somatic")))
        (is (equal "active" (nlk:json-value somatic :string "status")))
        (is (< (abs (- 0.3 (nlk:json-value somatic :number "energy"))) 1e-6) "the reading is carried over"))
      (let ((prefrontal (nlk:json-value (nlk:json-value document :object "systems")
                                         :object "prefrontal")))
        (is (equal "missing" (nlk:json-value prefrontal :string "status")) "and a subsystem with no file says so")))))

(deftest mind-kit-cell-state-does-not-need-a-seeded-kit ()
  (with-mind-kit ("empty")
    (let ((summary (mind-kit:state)))
      (is (= 25 (getf summary :missing)) "an unseeded kit reports 25 missing, not a lie")
      (is (equal "0%" (getf summary :score))))))

;;; --- the gate ---------------------------------------------------------------

(deftest mind-kit-cell-gate-names-the-numbers-behind-it ()
  (with-mind-kit ("gate")
    (mind-kit-put (mind-kit:agent-dir) "brain/somatic/markers.json"
                  "{\"gut_feeling\": \"stop\", \"energy\": 0.8}")
    (let ((veto (mind-kit:gate :label "deploy" :now "12:00")))
      (is (equal "veto_gut_stop" (getf veto :reason)) "a gut stop vetoes")
      (is (equal "veto" (getf veto :verdict)))
      (is (= 1 (getf veto :exit-code)) "VETO is exit 1")
      (is (and (search "gut=" (getf veto :detail)) (search "stop" (getf veto :detail))) "the refusal names the number that caused it")
      (is (member "brain/scn/scn-state.json" (getf veto :defaulted) :test #'string=) "a reading nobody made is reported as a default"))
    (mind-kit-put (mind-kit:agent-dir) "brain/basal-ganglia/habit-state.json"
                  "{\"habits\": [{\"pattern\": \"ship it\", \"strength\": 0.9, \"status\": \"active\"}]}")
    (let ((habit (mind-kit:gate :label "deploy" :now "12:00")))
      (is (equal "proceed_habit_gut_stop" (getf habit :reason)) "a strong habit the state calls on suspends the gut stop")
      (is (member "brain/basal-ganglia/habit-state.json" (getf habit :read) :test #'string=) "and the habit file is reported as read"))))

(deftest mind-kit-cell-gate-quiet-hours-and-essential-mode ()
  (with-mind-kit ("hours")
    (mind-kit-put (mind-kit:agent-dir) "brain/somatic/markers.json"
                  "{\"gut_feeling\": \"go_ahead\", \"energy\": 0.7}")
    (let ((quiet (mind-kit:gate :now "23:30")))
      (is (getf quiet :quiet-hours) "23:30 is inside the documented quiet window")
      (is (equal "caution_quiet_hours" (getf quiet :reason)))
      (is (= 2 (getf quiet :exit-code)) "CAUTION is exit 2"))
    (let ((day (mind-kit:gate :now "13:00")))
      (is (not (getf day :quiet-hours)))
      (is (equal "proceed" (getf day :reason)) "a quiet machine in the day proceeds"))
    (let ((essential (mind-kit:gate :now "23:30" :mode "essential")))
      (is (not (getf essential :quiet-hours)) "essential bypasses quiet hours")
      (is (equal "proceed" (getf essential :reason))))
    (let ((unknown (mind-kit:gate :now "13:00" :mode "nonsense")))
      (is (equal "standard" (getf unknown :mode)) "an unknown mode falls back and says so")
      (is (getf unknown :warnings)))))

;;; --- what the model and the operator call ------------------------------------

(deftest mind-kit-cell-tools-and-command-answer ()
  (with-mind-kit ("tools")
    (mind-kit-put (mind-kit:agent-dir) "brain/somatic/markers.json"
                  "{\"gut_feeling\": \"go_ahead\"}")
    (let* ((tool (nle::find-tool "brain_state"))
           (text (funcall (third tool) (nlk:make-json-object))))
      (is (search "healthy" text) "the aggregate tool answers one health line"))
    (let* ((tool (nle::find-tool "decision_gate"))
           (text (funcall (third tool) (nlk:make-json-object
                                        "decision" "ship it" "mode" "standard"))))
      (is (search "PROCEED" text) "the gate tool answers the verdict")
      (is (search "exit 0" text)))
    (let ((line (nodecode-mind-kit::mind-command "" "s1")))
      (is (search "gate:" line) "and /mind answers both in one line"))
    (is (stringp (mind-kit:health)))))
