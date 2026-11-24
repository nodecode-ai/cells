;;;; cell-test.lisp --- lifecycle, the tool, the parameters, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, not DevTune: the config
;;;; surface (a section naming no key refuses the start), the tool through the
;;;; real dispatch seam against a stubbed API, every parameter it sends, the
;;;; project it resolves, and the text its answers and refusals arrive as.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "devtune"
  (:config "api_key" +key+)
  (:help :devtune)
  (:command "devtune")
  (:running
   (is (nle::find-tool "devtune_read") "devtune_read is registered")
   (is (equal +key+ (nodecode-devtune::setting :api-key)) "the key the section carries"))
  (:stopped
   (is (null (nle::find-tool "devtune_read")) "stop unregisters the tool"))
  (:refused
   ("api_key" "" "api_key_env" "UNSET_DEVTUNE_KEY")
   ("api_key" +key+ "window_days" 0)
   ("api_key" +key+ "timeout_seconds" 0)))

(deftest devtune-cell-without-a-key-refuses ()
  ;; The one setting no read can work without: a section naming no key, over a
  ;; variable that holds none, is refused at start, as the original offered no
  ;; DevTune tool without DEVTUNE_API_KEY.
  (with-wire
    (let ((condition (signals-error nlk:config-refusal
                                    (devtune-start "api_key" ""
                                                   "api_key_env" "UNSET_DEVTUNE_KEY"))))
      (is (cl:search "no key" (princ-to-string condition)) "the refusal says what is missing")
      (is (null (nle::find-tool "devtune_read")) "and installs nothing"))))

;;; --- the tool -------------------------------------------------------------------

(deftest devtune-cell-the-tool-reads-a-report (with-devtune ()
  (let ((text (devtune-call-tool "devtune_read" "{\"report\":\"visibility-summary\"}")))
    (is (cl:search "brandAwareness" text) "the answer is DevTune's own JSON, member for member")
    (is (cl:search "0.23" text) "with its numbers as DevTune sent them")
    (is (cl:search "windowDays 30" text) "under one line naming the window the read asked for"))
  (let ((call (devtune-call-to "visibility/summary")))
    (is (and call (cl:search "https://devtune.ai/api/v2/projects/p_1234/visibility/summary"
                              (getf call :url))) "the path is the project the connection named")
    (is (equal "Bearer dtk_test_key" (devtune-header call "authorization")) "with the key as a bearer header")
    (is (equal "application/json" (devtune-header call "accept")) "asking for JSON")
    (is (equal "30" (devtune-param call "windowDays")) "and the section's window when the call names none"))))

(deftest devtune-cell-the-project-is-resolved-once (with-devtune ()
  ;; The connection is one read per start, not one per report: a model asking
  ;; three questions must not spend three calls finding out where it is.
  (nodecode-devtune:report :visibility-summary)
  (nodecode-devtune:report :outcomes-ledger)
  (is (= 3 (length *calls*)) "two reads and one connection")
  (is (= 1 (length (remove-if-not (lambda (call) (cl:search "/connection" (getf call :url)))
                                  *calls*))) "the connection was asked once, and cached")))

(deftest devtune-cell-a-section-project-skips-the-connection (with-devtune ("project_id" "p_9999")
  (nodecode-devtune:report :visibility-summary)
  (is (= 1 (length *calls*)) "one request, not one to find out where it is")
  (is (and (devtune-call-to "/projects/p_9999/visibility/summary") t) "the section's project is the one read")
  (is (null (devtune-call-to "/connection")) "and GET /connection never went out")))

(deftest devtune-cell-window-cursor-and-setup-ride-the-request (with-devtune ()
  (nodecode-devtune:report :visibility-summary :window-days 90)
  (let ((call (devtune-call-to "visibility/summary")))
    (is (equal "90" (devtune-param call "windowDays")) "a call's own window wins over the section's"))
  (let ((call (progn (nodecode-devtune:report :agent-picks-history :setup "web" :cursor "c_2")
                     (devtune-call-to "agent-picks/history"))))
    (is (equal "web" (devtune-param call "setup")) "setup picks one Agent Picks setup")
    (is (equal "c_2" (devtune-param call "cursor")) "and a page's cursor asks for the one after it")
    (is (null (devtune-param call "windowDays")) "an Agent Picks read sends no window at all"))))

(deftest devtune-cell-a-single-thing-report-carries-its-id (with-devtune ()
  (nodecode-devtune:report :action-brief :id "act 7")
  (is (and (devtune-call-to "/actions/act%207/brief") t) "an id rides the path, encoded")
  (nodecode-devtune:report :agent-picks-test :id "t_9")
  (is (equal "t_9" (devtune-param (devtune-call-to "agent-picks/test") "test_id")) "or the query member its path names")))

(deftest devtune-cell-a-single-thing-report-with-no-id-refuses (with-devtune ()
  ;; The refusal comes before the connection, so a model that forgot the id
  ;; spends no request learning it did.
  (let ((condition (signals-error nodecode-devtune:devtune-error
                                  (nodecode-devtune:report :action-brief))))
    (is (cl:search "needs" (princ-to-string condition)) "the refusal says an id is missing")
    (is (cl:search "action" (princ-to-string condition)) "and which id it wants"))
  (is (null *calls*) "and nothing was dialled")))

(deftest devtune-cell-an-unknown-report-refuses (with-devtune ()
  (let ((condition (signals-error nodecode-devtune:devtune-error
                                  (nodecode-devtune:report :nonsense))))
    (is (cl:search "no report" (princ-to-string condition)) "the refusal names the mistake")
    (is (cl:search "visibility-summary" (princ-to-string condition)) "and lists the reports there are"))
  (is (null *calls*) "with nothing dialled")))

;;; --- the text a model reads -------------------------------------------------------

(deftest devtune-cell-a-refused-key-carries-devTunes-own-sentence (with-devtune ()
  (setf *answer* (lambda (url)
                   (if (cl:search "/connection" url)
                       (values +connection+ 200)
                       (values +refused+ 401))))
  (let ((condition (signals-error nodecode-devtune:devtune-error
                                  (nodecode-devtune:report :visibility-summary))))
    (is (cl:search "HTTP 401" (princ-to-string condition)) "the status is named")
    (is (cl:search "invalid_token" (princ-to-string condition)) "and DevTune's own code")
    (is (cl:search "scope" (princ-to-string condition)) "and what a refusal like it means"))))

(deftest devtune-cell-several-setups-says-how-to-pick-one (with-devtune ()
  (setf *answer* (lambda (url)
                   (if (cl:search "/connection" url)
                       (values +connection+ 200)
                       (values +setup-required+ 409))))
  (let ((condition (signals-error nodecode-devtune:devtune-error
                                  (nodecode-devtune:report :agent-picks-summary))))
    (is (cl:search "agent_picks_setup_required" (princ-to-string condition)) "the answer's own reason")
    (is (cl:search "setup" (princ-to-string condition)) "and that one of details.setups goes in setup"))))

(deftest devtune-cell-a-paused-project-is-said-so (with-devtune ()
  (setf *answer* (lambda (url) (declare (ignore url)) (values +connection-paused+ 200)))
  (let ((text (nodecode-devtune::connection-line)))
    (is (cl:search "connected to project p_1234" text) "the line names the project")
    (is (cl:search "paused" text) "and that a paused project can be read but not changed")
    (is (cl:search "outcomes-ledger" text) "and lists what this cell can read"))))

;;; --- the operator's door, and the two lists ---------------------------------------

(deftest devtune-cell-the-command-names-its-reports (with-devtune ()
  (let ((bare (cell-entry "nodecode-devtune" "devtune" "" "devtune-cell-1")))
    (is (cl:search "connected to project p_1234" bare) "with no report it says where it is")
    (is (cl:search "visibility-summary" bare) "and what it can read")
    (is (cl:search "/devtune visibility-summary" bare) "and how to ask for one"))
  (let ((text (cell-entry "nodecode-devtune" "devtune" "outcomes-ledger" "devtune-cell-1")))
    (is (cl:search "devtune: outcomes-ledger" text) "with a report it reads it")
    (is (cl:search "improved" text) "and answers what DevTune said"))))

(deftest devtune-cell-the-eval-verb-answers-what-the-tool-answers (with-devtune ()
  (let ((from-tool (devtune-call-tool "devtune_read" "{\"report\":\"outcomes-ledger\"}"))
        (from-eval (nodecode-devtune:report :outcomes-ledger)))
    (is (equal from-tool from-eval) "one read, one answer, whichever door it came through"))))

(deftest devtune-cell-the-tool-advertises-every-report ()
  ;; The contract's enum and the table the reads are made from are two lists;
  ;; this is what keeps a report added to one from never reaching the model.
  (is (equal nodecode-devtune::+report-options+
             (mapcar (lambda (key) (string-downcase (symbol-name key)))
                     (nodecode-devtune:reports))) "every report the table reads is one the tool offers")
  (is (= 28 (length (nodecode-devtune:reports))) "and the table is the original's 28 reads"))
