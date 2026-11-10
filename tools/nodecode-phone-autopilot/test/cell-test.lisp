;;;; cell-test.lisp --- lifecycle, saving and replaying over a stubbed runner, /phone-autopilot.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here runs phone-harness and nothing touches the operator's own
;;;; folder: *RUN* is the cell's one seam to the child and every test binds
;;;; it to a stub, and the section's directory is a scratch folder. What is
;;;; proved is the cell's own wiring -- a routine saved with its version and
;;;; sha, the script the runner is handed, a passed run, a missed checkpoint
;;;; with its screenshot, a refusal to run bytes nobody approved, a name that
;;;; would leave the folder, the record on disk, and the command.

(in-package #:nodecode.test)

(define-test-slice "phone-autopilot" "PHONE-AUTOPILOT-" :start nodecode-phone-autopilot:start-cell)

;;; --- the stubs --------------------------------------------------------------------

(defvar *runs* '()
  "What the stubbed runner was called with, newest first.")

(defvar *answers* '()
  "What the stubbed runner answers next: (STDOUT STDERR STATUS) each.")

(defun stub-run (argv &key seconds directory environment input)
  "The runner, as the cell calls it: one call recorded, one canned answer."
  (push (list :argv argv :seconds seconds :directory directory
              :environment environment :input input)
        *runs*)
  (destructuring-bind (out err status) (or (pop *answers*) (list "" "" 0))
    (values out err status)))

(defun scratch-directory ()
  "A folder of this test's own, for the section's directory."
  (let ((path (merge-pathnames (format nil "phone-autopilot-~d/" (random 1000000))
                                (uiop:temporary-directory))))
    (ensure-directories-exist path)
    path))

(defmacro with-phone-autopilot ((&rest pairs) &body body)
  "The cell started with PAIRS as its section, over a scratch folder, its
runner stubbed."
  (let ((folder (gensym "FOLDER")))
    `(let* ((,folder (scratch-directory))
            (*runs* '()) (*answers* '()))
       (unwind-protect
            (with-cell-stop ((phone-autopilot-start "directory" (namestring ,folder)
                                                    ,@pairs))
              (let ((nodecode-phone-autopilot::*run* 'stub-run))
                ,@body))
         (ignore-errors (uiop:delete-directory-tree ,folder :validate t))))))

(defun routine-call (&rest pairs)
  "The routine tool's answer for PAIRS, decoded through the declaration it
registered, as the dispatch seam does it."
  (let ((entry (or (nle::find-tool "routine") (error "the routine tool is not registered"))))
    (funcall (third entry)
             (nlk:contract-decode
              (second entry)
              (apply #'nlk:make-json-object
                     (loop for (key value) on pairs by #'cddr
                           collect (if (keywordp key) (string-downcase (symbol-name key)) key)
                           collect value))))))

(defun answer-json (text)
  "TEXT, the tool's answer, read as JSON."
  (shasht:read-json text))

(defun json (object key) (and (hash-table-p object) (gethash key object)))

(defun saved (&rest pairs)
  "A routine saved with PAIRS, its answer read as JSON."
  (answer-json (apply #'routine-call :action "save" pairs)))

;;; --- lifecycle ----------------------------------------------------------------------

(define-cell-lifecycle-tests "phone-autopilot"
  (:help :phone-autopilot)
  (:command "phone-autopilot")
  (:running (is (nle::find-tool "routine") "the tool is registered")
            (is (equal "phone-harness" (nodecode-phone-autopilot::setting :command)))
            (is (nodecode-phone-autopilot::setting :approve) "the approval question starts on")
            (is (null (nodecode-phone-autopilot::setting :ask-before-apps))))
  (:refused ("timeout_seconds" 0) ("allowed_apps" 5)))

;;; --- saving --------------------------------------------------------------------------

(deftest phone-autopilot-save-keeps-a-version-and-its-sha ()
  (with-phone-autopilot ("approve" nil)
    (let* ((json (saved :name "claim-reward" :title "Claim the daily reward"
                         :code "checkpoint('home', text='Home')\ndone(claimed=True)"
                         :params (nlk:make-json-object "account" "main")))
           (meta (json json "routine"))
           (folder (nodecode-phone-autopilot::routine-folder "claim-reward")))
      (is (eq t (json json "success")))
      (is (= 1 (json meta "version")) "a first save is version 1")
      (is (eq t (json meta "approved")) "approval is off, so saving approves")
      (is (equal (json meta "title") "Claim the daily reward"))
      (is (probe-file (merge-pathnames "routine.py" folder)) "the code is on disk")
      (is (probe-file (merge-pathnames "versions/1.py" folder)) "and kept as version 1")
      (is (equal (json meta "sha256")
                 (nodecode-phone-autopilot::sha256-hex
                  (uiop:read-file-string (merge-pathnames "routine.py" folder)))) "the sha is the bytes'")
      (let ((again (saved :name "claim-reward" :code "done()")))
        (is (= 2 (json (json again "routine") "version")) "a second save is version 2")
        (is (probe-file (merge-pathnames "versions/2.py" folder)))))))

(deftest phone-autopilot-save-refuses-an-empty-or-dear-name ()
  (with-phone-autopilot ("approve" nil)
    (is-carrying (answer (routine-call :action "save" :name "claim" :code "   "))
      ("code is required for save"))
    (is (null (nodecode-phone-autopilot::routine-meta "claim")) "nothing was written")
    (dolist (name (list "../escape" "Claim" "a/b" "with space"))
      (is-carrying (answer (routine-call :action "save" :name name :code "done()"))
        ("is not a routine name")))))

;;; --- running -------------------------------------------------------------------------

(deftest phone-autopilot-run-replays-the-script-through-the-runner ()
  (with-phone-autopilot ("approve" nil)
    (saved :name "claim-reward" :code "checkpoint('claim', text='Claim')\ndone(claimed=True)"
           :params (nlk:make-json-object "account" "main"))
    (setf *answers*
          (list (list (format nil "~a~%~a~%printed by the routine~%"
                               "@@AUTOPILOT {\"event\":\"checkpoint\",\"name\":\"claim\",\"status\":\"passed\"}"
                               "@@AUTOPILOT {\"event\":\"done\",\"claimed\":true,\"account\":\"main\"}")
                      "" 0)))
    (let* ((json (answer-json (routine-call :action "run" :name "claim-reward"
                                             :params (nlk:make-json-object "account" "other"))))
           (call (first *runs*))
           (script (getf call :input)))
      (is (eq t (json json "success")) "the run passed")
      (is (eq t (json json "ok")))
      (is (equal '("phone-harness") (getf call :argv)) "the command line is the argv")
      (is (= 300 (getf call :seconds)) "the original's default bound")
      (is (search "def checkpoint(" script) "the prelude rides along")
      (is (search "GUARD_ASK = False" script) "the guard is told the allowlist setting")
      (is (search "PARAMS.update(_json.loads(" script) "and the run's params")
      (is (search "other" script) "the run's value, not the saved one")
      (is (search "checkpoint('claim'" script) "the routine's own code is last")
      (is (search "DO_NOT_TRACK=1" (format nil "~{~a~^ ~}" (getf call :environment))) "phone-harness is told not to phone home")
      (is (= 2 (length (json json "events"))) "both events: the checkpoint and the done")
      (let ((record (nodecode-phone-autopilot::list-runs "claim-reward")))
        (is (= 1 (length record)) "the run is on disk")
        (is (eq t (json (first record) "ok")))
        (is (search "printed by the routine" (json (first record) "output_tail")) "the tail of what it printed")
        (is (equal (json (first record) "run_id") (json json "run_id")))))))

(deftest phone-autopilot-a-missed-checkpoint-names-the-screen-and-the-shot ()
  (with-phone-autopilot ("approve" nil)
    (saved :name "claim-reward" :code "checkpoint('claim', text='Claim')\ndone()")
    (setf *answers*
          (list (list (format nil "~a~%"
                               "@@AUTOPILOT {\"event\":\"checkpoint\",\"name\":\"claim\",\"status\":\"failed\",\"expected\":\"Claim\",\"seen_text\":[\"Sign in\"],\"screenshot\":\"/screens/shot-1.png\"}")
                      "Traceback: CheckpointMiss" 1)))
    (let* ((json (answer-json (routine-call :action "run" :name "claim-reward")))
           (failed (json json "failed_checkpoint")))
      (is (null (json json "success")) "the run failed")
      (is (equal "claim" (json failed "name")))
      (is (equal "/screens/shot-1.png" (json failed "screenshot")))
      (is (search "Run failed at checkpoint" (json json "next_steps")))
      (is (search "(look" (json json "next_steps")) "the operator is told how to see the shot")
      (is (equal "checkpoint \"claim\" failed" (json json "error"))))))

(deftest phone-autopilot-a-run-nobody-approved-is-refused ()
  (with-phone-autopilot ()
    (let ((json (saved :name "claim-reward" :code "done()")))
      (is (eq t (json json "success")) "the save was written")
      (is (null (json (json json "routine") "approved")) "but nobody answered the question")
      (is (search "Nobody answered" (json json "next_steps"))))
    (is (null *runs*) "and no script ran")
    (is-carrying (answer (routine-call :action "run" :name "claim-reward"))
      ("is not approved") ("Nobody answered"))
    (is (null *runs*) "still nothing ran")
    (is-carrying (answer (nodecode-phone-autopilot::run-slash "approve claim-reward" nil))
      ("approved"))
    (setf *answers* (list (list "" "" 0)))
    (is (eq t (json (answer-json (routine-call :action "run" :name "claim-reward")) "success")) "approved now, and the run goes ahead")))

(deftest phone-autopilot-a-routine-edited-on-disk-is-refused ()
  (with-phone-autopilot ("approve" nil)
    (saved :name "claim-reward" :code "done()")
    (with-open-file (out (nodecode-phone-autopilot::routine-code-path "claim-reward")
                          :direction :output :if-exists :supersede)
      (write-string "done(edited=True)" out))
    (is-carrying (answer (routine-call :action "run" :name "claim-reward"))
      ("was edited on disk") ("save it again"))
    (is (null *runs*) "the edited bytes never ran")))

;;; --- list, show, runs, delete --------------------------------------------------------

(deftest phone-autopilot-list-show-runs-and-delete ()
  (with-phone-autopilot ("approve" nil)
    (saved :name "claim-reward" :code "done()" :title "Claim")
    (let ((list (answer-json (routine-call :action "list")))
          (show (answer-json (routine-call :action "show" :name "claim-reward"))))
      (is (= 1 (length (json list "routines"))))
      (is (equal "done()" (json (json show "routine") "code")) "show answers the code the routine runs"))
    (setf *answers* (list (list "" "" 0)))
    (routine-call :action "run" :name "claim-reward")
    (let ((runs (answer-json (routine-call :action "runs" :name "claim-reward"))))
      (is (= 1 (length (json runs "runs"))) "the run list has the run"))
    (is (eq t (json (answer-json (routine-call :action "delete" :name "claim-reward")) "success")))
    (is (null (nodecode-phone-autopilot::routine-meta "claim-reward")) "the routine is gone")
    (is-carrying (answer (routine-call :action "run" :name "claim-reward"))
      ("no routine named"))))

;;; --- the question ---------------------------------------------------------------------

(deftest phone-autopilot-a-run-that-cannot-start-says-why ()
  (with-phone-autopilot ("approve" nil)
    (saved :name "claim-reward" :code "done()")
    (let ((nodecode-phone-autopilot::*run*
            (lambda (argv &key seconds directory environment input)
              (declare (ignore argv seconds directory environment input))
              (error "spawning phone-harness: No such file or directory"))))
      (is-carrying (answer (routine-call :action "run" :name "claim-reward"))
        ("could not start phone-harness")))
    (setf *answers* (list (list "" "" :timeout)))
    (is-carrying (answer (routine-call :action "run" :name "claim-reward"))
      ("timed out after 300s"))))

;;; --- /phone-autopilot --------------------------------------------------------------------

(deftest phone-autopilot-slash-answers-and-moves-the-allowlist ()
  (with-phone-autopilot ("approve" nil)
    (saved :name "claim-reward" :code "done()" :title "Claim")
    (is-carrying (answer (nodecode-phone-autopilot::run-slash "" nil))
      ("claim-reward") ("| Routine |") ("never"))
    (is-carrying (answer (nodecode-phone-autopilot::run-slash "show claim-reward" nil))
      ("Claim") ("version 1") ("done()"))
    (is-carrying (answer (nodecode-phone-autopilot::run-slash "allow com.spotify.music" nil))
      ("com.spotify.music allowed"))
    (is-carrying (answer (nodecode-phone-autopilot::run-slash "apps" nil))
      ("com.spotify.music"))
    (is (equal '("com.spotify.music") (nodecode-phone-autopilot::allowed-apps)) "the allowlist moved for this run too")
    (is-carrying (answer (nodecode-phone-autopilot::run-slash "revoke com.spotify.music" nil))
      ("revoked"))
    (is-carrying (answer (nodecode-phone-autopilot::run-slash "allow Spotify" nil))
      ("is not an Android package name"))
    (is-carrying (answer (nodecode-phone-autopilot::run-slash "show nope" nil))
      ("no routine named"))
    (is (null *runs*) "the command starts nothing")))
