;;;; cell-test.lisp --- the WOOF cell against a stubbed command line.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; No WOOF is installed and no process is started: RUN-WOOF answers the text
;;;; the real command would, LAUNCH-WOOF answers a pid, and PID-ALIVE-P and
;;;; SIGNAL-PID say what the test wants. Each tool is called the way a model's
;;;; call reaches it -- through the real dispatch, contract and all -- so the
;;;; schema a provider is shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "woof" "WOOF-CELL-")

(defparameter +plan+ "{\"domain\":{\"center\":\"35.2,-97.4\",\"cells\":96},\"hours\":6}"
  "One WOOF run plan, as a model would pass it: an area and six hours.")

(defun woof-runs-home ()
  "This test's own runs folder, so nothing is written into a real home."
  (namestring (ensure-directories-exist (nlk:home "woof-runs/"))))

(defun woof-config (&rest pairs)
  "A whole shared config holding the `woof' section PAIRS make."
  (let ((section (apply #'nlk:make-json-object pairs)))
    (alexandria:ensure-gethash "runs_dir" section (woof-runs-home))
    (nlk:json-object "woof" section)))

(defun woof-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-woof:start-cell (apply #'woof-config pairs)))

(defun woof-answer (id tool arguments)
  "TOOL's answer text, called the way a model's call reaches it."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun woof-object (id tool arguments)
  "TOOL's answer, decoded; an ERROR answer is raised so the failure says what it was."
  (let ((text (woof-answer id tool arguments)))
    (if (search "ERROR:" text)
        (error "~a" text)
        (nlk:decode-json text))))

(defun woof-reply (object key &optional (type :string))
  (nlk:json-value object type key))

;;; --- what the stubs answer ------------------------------------------------------------

(defmacro with-plan-command ((&key (status 0)) &body body)
  "BODY with RUN-WOOF answering WOOF's resolve and estimate, or refusing when STATUS is not 0."
  `(with-stubbed-fdefinition (nodecode-woof:run-woof (argv)
                                (cond ((member "--resolve" argv :test #'string=)
                                       ,(if (= status 0)
                                            `(values "{\"grid\":\"96x96x40\",\"memory_gib\":9.4}" "" 0)
                                            `(values "" "refused: 96 cells needs 41 GiB, the card has 24" 2)))
                                      ((member "--estimate" argv :test #'string=)
                                       (values "{\"total_s\":740,\"basis\":\"measured throughput\"}" "" 0))
                                      (t (values "" "" 0))))
     ,@body))

(defmacro with-live-run ((&key (alive t) (pid 4242)) &body body)
  "BODY with the launch answering PID and the pid asked about answering ALIVE."
  `(with-stubbed-fdefinition (nodecode-woof:launch-woof (argv log)
                                (declare (ignore argv log))
                                ,pid)
     (with-stubbed-fdefinition (nodecode-woof:pid-alive-p (candidate)
                                 (declare (ignore candidate))
                                 ,alive)
       ,@body)))

(defun woof-launch-once (&key (plan +plan+) (label "test run"))
  "One run started through the seams WITH-LIVE-RUN stubs; => its run id."
  (let ((object (woof-object "woof-launch-1" "woof_launch"
                              (format nil "{\"plan\":~a,\"label\":~s}" plan label))))
    (is (eq t (woof-reply object "ok" :boolean)) "a started run is ok")
    (is (probe-file (woof-reply object "folder")) "the run folder exists")
    (woof-reply object "run_id")))

;;; --- the lifecycle, from the one declaration ------------------------------------------

(define-cell-lifecycle-tests "woof"
  (:config "woof" "woof")
  (:help :woof)
  (:command "woof")
  (:refused ("max_images" 0))
  (:idle nodecode-woof:woof-error (nodecode-woof:doctor)))

(deftest woof-cell-names-the-command-it-lacks ()
  (with-cell-stop ((nodecode-woof:start-cell (nlk:json-object "woof" (nlk:make-json-object))))
    (let ((answer (woof-answer "woof-no-command-1" "woof_doctor" "{}")))
      (is (search "ERROR:" answer) "a cell with no command set refuses the call")
      (is (search "woof section" answer) "and names the key to set"))))

;;; --- woof_doctor ---------------------------------------------------------------------

(deftest woof-cell-doctor-names-the-command ()
  (with-stubbed-fdefinition (nodecode-woof:run-woof (argv)
                              (cond ((member "doctor" argv :test #'string=)
                                     (values "GPU: NVIDIA RTX 4090, 24 GiB; CUDA 12.8" "" 0))
                                    (t (values "woof 1.0.3" "" 0))))
    (with-cell-stop ((woof-start "woof" "/opt/woof/bin/woof"))
      (let ((object (woof-object "woof-doctor-1" "woof_doctor" "{}")))
        (is (eq t (woof-reply object "ok" :boolean)) "a command that answers is ok")
        (is (equal "/opt/woof/bin/woof" (woof-reply object "woof")) "the configured command is named back")
        (is (equal "woof 1.0.3" (woof-reply object "version")) "the version is WOOF's own")
        (is (search "RTX 4090" (woof-reply object "doctor")) "the doctor report comes from WOOF")
        (is (equal (woof-runs-home) (woof-reply object "runs_dir")) "and the runs folder is this test's")))))

(deftest woof-cell-doctor-says-when-the-command-is-missing ()
  (with-stubbed-fdefinition (nodecode-woof:run-woof (argv)
                              (declare (ignore argv))
                              (values "" "woof: command not found" 127))
    (with-cell-stop ((woof-start "woof" "woof"))
      (let ((object (woof-object "woof-doctor-2" "woof_doctor" "{}")))
        (is (null (woof-reply object "ok" :boolean)) "a command that does not answer is not ok")
        (is (null (woof-reply object "can_check" :boolean)) "and nothing can be sized")))))

;;; --- woof_plan -------------------------------------------------------------------------

(deftest woof-cell-plans-without-running ()
  (with-plan-command ()
    (with-cell-stop ((woof-start "woof" "woof"))
      (let* ((object (woof-object "woof-plan-1" "woof_plan"
                                   (format nil "{\"plan\":~a}" +plan+)))
             (resolve (woof-reply object "resolve" :object))
             (estimate (woof-reply object "estimate" :object)))
        (is (eq t (woof-reply object "ok" :boolean)) "both answers came back")
        (is (equal "96x96x40" (nlk:json-value resolve :string "grid")) "WOOF's own grid is what is reported")
        (is (probe-file (woof-reply object "plan")) "the plan was written where the answer says")
        (let ((basis (nlk:one-line (nlk:encode-json-object estimate))))
          (is (search "measured throughput" basis) "the estimate carries its basis"))))))

(deftest woof-cell-plan-names-a-refusal ()
  (with-plan-command (:status 2)
    (with-cell-stop ((woof-start "woof" "woof"))
      (let ((object (woof-object "woof-plan-2" "woof_plan" (format nil "{\"plan\":~a}" +plan+))))
        (is (null (woof-reply object "ok" :boolean)) "a refused plan is not ok")
        (is (search "41 GiB" (woof-reply object "refused")) "and WOOF's refusal is quoted back")))))

(deftest woof-cell-plan-refuses-an-empty-call ()
  (with-cell-stop ((woof-start "woof" "woof"))
    (let ((answer (woof-answer "woof-plan-3" "woof_plan" "{}")))
      (is (search "plan" (string-downcase answer)) "the required member is named in the refusal"))))

;;; --- woof_launch, woof_status, woof_show, woof_stop ------------------------------------

(deftest woof-cell-launches-into-its-own-folder ()
  (with-live-run ()
    (with-cell-stop ((woof-start "woof" "woof"))
      (let ((run-id (woof-launch-once)))
        (is (plusp (length run-id)) "the answer carries a run id")
        (let ((plan (merge-pathnames "plan.json" (nodecode-woof:run-directory run-id)))
              (record (merge-pathnames "record.json" (nodecode-woof:run-directory run-id))))
          (is (probe-file plan) "the plan is kept in the run's own folder")
          (let ((label (woof-reply (nlk:decode-json (uiop:read-file-string record)) "label")))
            (is (equal "test run" label) "the label rides the record")))))))

(deftest woof-cell-status-reads-one-run ()
  (with-live-run ()
    (with-cell-stop ((woof-start "woof" "woof"))
      (let* ((run-id (woof-launch-once))
             (object (woof-object "woof-status-1" "woof_status"
                                   (format nil "{\"run_id\":\"~a\"}" run-id)))
             (run (woof-reply object "run" :object)))
        (is (equal "running" (nlk:json-value run :string "state")) "a live pid is a running run")
        (is (equal run-id (nlk:json-value run :string "run_id")) "the run answers by its own id")))))

(deftest woof-cell-status-lists-every-run ()
  (with-live-run ()
    (with-cell-stop ((woof-start "woof" "woof"))
      (woof-launch-once)
      (let* ((object (woof-object "woof-status-2" "woof_status" "{}"))
             (runs (woof-reply object "runs" :array)))
        (is (= 1 (length runs)) "the one run is listed")
        (is (equal "running" (nlk:json-value (aref runs 0) :string "state")) "with the state it is in")))))

(deftest woof-cell-show-answers-media-lines ()
  (with-live-run ()
    (with-cell-stop ((woof-start "woof" "woof"))
      (let* ((run-id (woof-launch-once))
             (map (merge-pathnames "woof_reflectivity_d01_f001.png"
                                    (nodecode-woof:run-directory run-id))))
        (with-open-file (out map :direction :output :if-exists :supersede
                                  :element-type '(unsigned-byte 8))
          (write-sequence (make-array 8 :initial-element 137 :element-type '(unsigned-byte 8)) out))
        (let ((object (woof-object "woof-show-1" "woof_show"
                                    (format nil "{\"run_id\":\"~a\",\"max_images\":2}" run-id)))
              (other (woof-object "woof-show-2" "woof_show"
                                  (format nil "{\"run_id\":\"~a\"}" run-id))))
          (is (= 1 (woof-reply object "total" :integer)) "the one map is found")
          (is (search "MEDIA:" (woof-reply object "media")) "as a MEDIA: line the model can copy")
          (is (search "woof_reflectivity_d01_f001.png" (woof-reply object "media")) "naming the map's own path")
          (is (= 1 (length (woof-reply other "maps" :array))) "and the answer stays small"))))))

(deftest woof-cell-stop-signals-the-recorded-pid ()
  (let ((signalled '()))
    (with-live-run ()
      (with-stubbed-fdefinition (nodecode-woof:signal-pid (pid signal)
                                  (declare (ignore signal))
                                  (push pid signalled)
                                  t)
        (with-cell-stop ((woof-start "woof" "woof"))
          (let* ((run-id (woof-launch-once))
                 (object (woof-object "woof-stop-1" "woof_stop"
                                       (format nil "{\"run_id\":\"~a\"}" run-id))))
            (is (equal '(4242) (reverse signalled)) "the run's own pid was signalled once")
            (is (equal "stopped" (woof-reply object "state")) "and the run is marked stopped")
            (let ((again (woof-object "woof-stop-2" "woof_stop"
                                       (format nil "{\"run_id\":\"~a\"}" run-id))))
              (is (equal "stopped" (woof-reply again "state")) "stopping twice says it was already over"))))))))

(deftest woof-cell-refuses-unknown-and-missing-arguments ()
  (with-cell-stop ((woof-start "woof" "woof"))
    (let ((unknown (woof-answer "woof-unknown-1" "woof_status" "{\"run_id\":\"nobody\"}"))
          (unasked (woof-answer "woof-unknown-2" "woof_show" "{}")))
      (is (search "no run" unknown) "an unknown run is named back")
      (is (search "run_id" (string-downcase unasked)) "a missing required argument refuses by name"))))

;;; --- woof_catalog ------------------------------------------------------------------------

(deftest woof-cell-catalog-asks-woof ()
  (with-stubbed-fdefinition (nodecode-woof:run-woof (argv)
                              (if (search "physics-catalog" (format nil "~{~a~^ ~}" argv))
                                  (values "{\"profiles\":[{\"id\":\"thompson\"}],\"mixes\":[]}" "" 0)
                                  (values "{}" "" 0)))
    (with-cell-stop ((woof-start "woof" "woof"))
      (let* ((object (woof-object "woof-catalog-1" "woof_catalog" "{\"kind\":\"physics\"}"))
             (catalog (woof-reply object "catalog" :object)))
        (is (eq t (woof-reply object "ok" :boolean)) "WOOF's own answer is ok")
        (is (equal "physics" (woof-reply object "kind")) "the kind is named back")
        (is (= 1 (length (nlk:json-array catalog "profiles"))) "and the suites are WOOF's")))))

(deftest woof-cell-catalog-refuses-a-kind-woof-keeps-no-catalog-of ()
  (with-cell-stop ((woof-start "woof" "woof"))
    (let ((answer (woof-answer "woof-catalog-2" "woof_catalog" "{\"kind\":\"colour\"}")))
      (is (search "sources" (string-downcase answer)) "the refusal names the kinds there are"))))

;;; --- woof_compare and woof_report ---------------------------------------------------------

(defun woof-write-map (run-id name)
  "Write a map into RUN-ID's folder, as WOOF's renderer would."
  (let ((path (merge-pathnames name (nodecode-woof:run-directory run-id))))
    (with-open-file (out path :direction :output :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (write-sequence (make-array 8 :initial-element 137 :element-type '(unsigned-byte 8)) out))
    path))

(deftest woof-cell-compares-two-runs ()
  (let ((asked '()))
    (with-live-run ()
      (with-stubbed-fdefinition (nodecode-woof:run-woof (argv)
                                  (push (format nil "~{~a~^ ~}" argv) asked)
                                  (let ((folder (and (member "--out" argv :test #'string=)
                                                     (nth (1+ (position "--out" argv :test #'string=)) argv))))
                                    (when folder
                                      (let ((path (merge-pathnames "sheet.png"
                                                                   (uiop:ensure-directory-pathname folder))))
                                        (ensure-directories-exist path)
                                        (with-open-file (out path :direction :output :if-exists :supersede)
                                          (write-string "png" out)))))
                                  (values "" "" 0))
        (with-cell-stop ((woof-start "woof" "woof"))
          (let ((left (woof-launch-once :label "control"))
                (right (woof-launch-once :label "warm rain")))
            (woof-write-map left "woof_reflectivity_d01_f001.png")
            (woof-write-map right "woof_reflectivity_d01_f001.png")
            (let ((object (woof-object "woof-compare-1" "woof_compare"
                                        (format nil "{\"left\":\"~a\",\"right\":\"~a\"}" left right))))
              (is (eq t (woof-reply object "ok" :boolean)) "WOOF drew the pair")
              (is (search "--pair" (first asked)) "through WOOF's own pair renderer")
              (is (search "control" (first asked)) "labelled with the run's label")
              (is (search "MEDIA:" (woof-reply object "media")) "and answered as MEDIA: lines"))))))))

(deftest woof-cell-compare-needs-two-runs-with-maps ()
  (with-live-run ()
    (with-cell-stop ((woof-start "woof" "woof"))
      (let* ((left (woof-launch-once))
             (right (woof-launch-once))
             (answer (woof-answer "woof-compare-2" "woof_compare"
                                   (format nil "{\"left\":\"~a\",\"right\":\"~a\"}" left right)))
             (ghost (woof-answer "woof-compare-3" "woof_compare"
                                  "{\"left\":\"ghost\",\"right\":\"ghost\"}")))
        (is (search "no map yet" answer) "a run with no map is named as the reason")
        (is (search "no run" ghost) "and an unknown run is named too")))))

(deftest woof-cell-report-reads-what-a-run-left ()
  (with-live-run ()
    (with-cell-stop ((woof-start "woof" "woof"))
      (let* ((run-id (woof-launch-once :label "report run"))
             (folder (nodecode-woof:run-directory run-id)))
        (woof-write-map run-id "woof_reflectivity_d01_f001.png")
        (with-open-file (out (merge-pathnames "woof.log" folder) :direction :output
                                                             :if-exists :supersede)
          (write-string "wrote frame 1" out))
        (let* ((object (woof-object "woof-report-1" "woof_report"
                                     (format nil "{\"run_id\":\"~a\"}" run-id)))
               (run (woof-reply object "run" :object)))
          (is (eq t (woof-reply object "ok" :boolean)) "the report came back")
          (is (equal "report run" (nlk:json-value run :string "label")) "with the run's own label")
          (is (= 1 (length (woof-reply object "maps" :array))) "its maps")
          (is (search "wrote frame 1" (woof-reply object "log_tail")) "and the tail of its log"))))))

;;; --- /woof ------------------------------------------------------------------------------

(deftest woof-cell-command-answers ()
  (with-stubbed-fdefinition (nodecode-woof:run-woof (argv)
                              (declare (ignore argv))
                              (values "woof 1.0.3" "GPU: NVIDIA RTX 4090" 0))
    (with-cell-stop ((woof-start "woof" "woof"))
      (let* ((entry (nle::find-registered-command "woof"))
             (text (and entry (funcall (nle::slash-command-headless entry) "" "s1"))))
        (is (not (null entry)) "/woof is registered")
        (is (and text (search "woof 1.0.3" text)) "the command names the WOOF it found")
        (is (and text (search (woof-runs-home) text)) "and where runs live")))))

;;; --- woof_docs ---------------------------------------------------------------------------

(deftest woof-cell-docs-answers-from-the-pack-it-carries ()
  (with-cell-stop ((woof-start "woof" "woof"))
    (let* ((object (woof-object "woof-docs-1" "woof_docs"
                                 "{\"query\":\"physics profile\",\"max_results\":2}"))
           (results (woof-reply object "results" :array))
           (pack (woof-reply object "pack" :object)))
      (is (eq t (woof-reply object "ok" :boolean)) "the pack answers")
      (is (<= 1 (length results) 2) "at most the number asked for, and at least one")
      (is (plusp (length (nlk:json-value (aref results 0) :string "text"))) "a passage carries its text")
      (is (plusp (length (nlk:json-value (aref results 0) :string "doc"))) "and the page it came from")
      (is (plusp (nlk:json-value pack :integer "chunks")) "the answer says how big the pack is"))))

(deftest woof-cell-docs-says-when-nothing-matches ()
  (with-cell-stop ((woof-start "woof" "woof"))
    (let ((object (woof-object "woof-docs-2" "woof_docs" "{\"query\":\"xylophone quintet\"}")))
      (is (null (woof-reply object "ok" :boolean)) "a query the pack cannot answer is not ok")
      (is (zerop (length (woof-reply object "results" :array))) "and carries no passage")
      (is (search "try the subject" (woof-reply object "note")) "it says what to try instead"))))

;;; --- woof_campaign -------------------------------------------------------------------------

(defparameter +campaign-estimate+
  "{\"expected_pace\":{\"wall_seconds_low\":240,\"wall_seconds_high\":300,\"sentence\":\"about five minutes\"},\"disk\":{\"total_bytes\":2147483648},\"download\":{\"bytes\":1073741824}}"
  "What WOOF's estimator answers about one arm, in the shape the campaign reads.")

(defmacro with-campaign-sizing (&body body)
  "BODY with WOOF answering a plan's resolve and estimate, sized per arm."
  `(with-stubbed-fdefinition (nodecode-woof:run-woof (argv)
                                (let ((line (format nil "~{~a~^ ~}" argv)))
                                  (cond ((search "--estimate" line)
                                         (values +campaign-estimate+ "" 0))
                                        ((search "--resolve" line) (values "{}" "" 0))
                                        (t (values "" "" 0)))))
     ,@body))

(defparameter +campaign-spec+
  "{\"name\":\"quick sweep\",\"axis\":\"physics\",\"values\":[\"thompson\",\"wsm6\"],\"base\":{\"domain\":{\"center\":\"35.2,-97.4\"},\"hours\":3},\"budget\":{\"max_wall_minutes\":20,\"max_disk_gib\":5}}"
  "One campaign spec: two physics suites, one plan.")

(deftest woof-cell-campaign-plans-and-sizes-its-arms ()
  (with-campaign-sizing ()
    (with-cell-stop ((woof-start "woof" "woof"))
      (let* ((object (woof-object "woof-campaign-1" "woof_campaign"
                                   (format nil "{\"action\":\"plan\",\"spec\":~a}" +campaign-spec+)))
             (arms (woof-reply object "arms" :array))
             (total (woof-reply object "total" :object)))
        (is (eq t (woof-reply object "ok" :boolean)) "the campaign is sized")
        (is (= 2 (length arms)) "one arm per value")
        (is (equal "thompson" (nlk:json-value (aref arms 0) :string "label")) "an arm is named by its value")
        (is (equal 300 (nlk:json-value (aref arms 0) :integer "wall_seconds")) "and carries WOOF's own estimate")
        (is (eq t (woof-reply object "within_budget" :boolean)) "the budget is met")
        (is (= 600 (nlk:json-value total :integer "wall_seconds")) "the total adds the arms up")))
    (with-cell-stop ((woof-start "woof" "woof"))
      (let ((answer (woof-answer "woof-campaign-2" "woof_campaign"
                                  "{\"action\":\"plan\",\"spec\":{\"values\":[\"a\"]}}")))
        (is (search "axis" (string-downcase answer)) "a spec with no axis names the key it needs")))))

(deftest woof-cell-campaign-runs-one-arm-at-a-time ()
  (with-campaign-sizing ()
    (with-live-run (:alive nil)
      (with-cell-stop ((woof-start "woof" "woof"))
        (let* ((planned (woof-object "woof-campaign-3" "woof_campaign"
                                      (format nil "{\"action\":\"plan\",\"spec\":~a}" +campaign-spec+)))
               (campaign-id (woof-reply planned "campaign_id"))
               (started (woof-object "woof-campaign-4" "woof_campaign"
                                      (format nil "{\"action\":\"start\",\"campaign_id\":\"~a\"}" campaign-id)))
               (first-arms (woof-reply started "arms" :array)))
          (is (equal "running" (woof-reply started "state")) "the campaign is under way")
          (is (plusp (length (nlk:json-value (aref first-arms 0) :string "run_id"))) "its first arm has a run")
          (is (equal "" (nlk:json-value (aref first-arms 1) :string "run_id"))) "and the second waits"
          (let* ((moved (woof-object "woof-campaign-5" "woof_campaign"
                                      (format nil "{\"action\":\"status\",\"campaign_id\":\"~a\"}" campaign-id)))
                 (mid (woof-reply moved "arms" :array)))
            (is (equal "ended" (nlk:json-value (aref mid 0) :string "state")) "the first arm ended")
            (is (plusp (length (nlk:json-value (aref mid 1) :string "run_id"))) "and the status call started the second")
            (let* ((settled (woof-object "woof-campaign-6" "woof_campaign"
                                          (format nil "{\"action\":\"status\",\"campaign_id\":\"~a\"}" campaign-id)))
                   (arms (woof-reply settled "arms" :array)))
              (is (equal "done" (woof-reply settled "state")) "with both arms over, the campaign is done")
            (let* ((report (woof-object "woof-campaign-7" "woof_report"
                                         (format nil "{\"campaign_id\":\"~a\"}" campaign-id)))
                   (rows (woof-reply report "arms" :array)))
              (is (eq t (woof-reply report "ok" :boolean)) "woof_report answers for a campaign")
              (is (= 2 (length rows)) "with one row per arm")))))))))

(deftest woof-cell-campaign-refuses-unknown-and-stray-actions ()
  (with-cell-stop ((woof-start "woof" "woof"))
    (let ((stray (string-downcase (woof-answer "woof-campaign-8" "woof_campaign"
                                                "{\"action\":\"dance\"}"))))
      (is (and (search "plan" stray) (search "stop" stray)) "an action that is not one of the four is refused, and the four are named back"))
    (is (search "no campaign" (string-downcase (woof-answer "woof-campaign-9" "woof_campaign"
                                                               "{\"action\":\"status\",\"campaign_id\":\"nobody\"}"))) "and an unknown campaign is named")))

;;; --- woof_score -----------------------------------------------------------------------------

(deftest woof-cell-score-refuses-a-run-that-has-not-ended ()
  (with-live-run ()
    (with-cell-stop ((woof-start "woof" "woof"))
      (let* ((run-id (woof-launch-once))
             (answer (woof-answer "woof-score-1" "woof_score"
                                   (format nil "{\"run_id\":\"~a\"}" run-id))))
        (is (search "only a finished run" answer) "a running run cannot be scored, and the answer says why")))))

(deftest woof-cell-score-reads-the-scorers-record ()
  (let ((called '()))
    (with-live-run (:alive nil)
      (with-stubbed-fdefinition (nodecode-woof:run-woof (argv)
                                  (let ((line (format nil "~{~a~^ ~}" argv)))
                                    (push line called)
                                    (cond ((search "-c " (concatenate 'string line " "))
                                           (values "{\"box\":[-97.5,35.2,-95.0,37.0],\"rule_status\":\"proposed-unratified\",\"registration_sha256\":\"deadbeef\"}" "" 0))
                                          ((search "obs_battery_score" line)
                                           (let ((at (position "--score-out" argv :test #'string=)))
                                             (with-open-file (out (nth (1+ at) argv) :direction :output
                                                                                   :if-exists :supersede)
                                               (write-string "{\"reflectivity\":{\"primary_scalar\":0.61,\"primary_threshold_dbz\":20},\"surface\":{\"temperature_rmse\":2.4}}" out)))
                                           (values "" "" 0))
                                          (t (values "" "" 0)))))
        (with-cell-stop ((woof-start "woof" "woof"))
          (let* ((run-id (woof-launch-once :label "scored arm"))
                 (folder (nodecode-woof:run-directory run-id)))
            (loop for hour in '("18:00:00" "19:00:00")
                  do (let ((path (merge-pathnames (format nil "woof-out/d01/wrfout_d01_2026-08-17_~a" hour) folder)))
                       (ensure-directories-exist path)
                       (with-open-file (out path :direction :output :if-exists :supersede)
                         (write-string "frame" out))))
            (let* ((object (woof-object "woof-score-2" "woof_score"
                                         (format nil "{\"run_id\":\"~a\",\"against\":[\"mrms\"]}" run-id)))
                   (summary (woof-reply object "summary" :object)))
              (is (eq t (woof-reply object "ok" :boolean)) "the score came back")
              (is (< (abs (- 0.61d0 (coerce (nlk:json-value summary :number "reflectivity_fss_primary") 'double-float))) 1d-6) "with the scorer's own primary number")
              (is (equal "proposed-unratified" (woof-reply object "rule_status")) "and the registration's own status")
              (is (= 1 (length (woof-reply object "scored_lead_hours" :array))) "an hour of forecast is one scored lead")
              (is (some (lambda (line) (search "tools.obs_fetch_mrms" line)) called) "the observations came through WOOF's own door")
              (is (probe-file (merge-pathnames "score.json" folder)) "and score.json sits beside the run"))))))))
