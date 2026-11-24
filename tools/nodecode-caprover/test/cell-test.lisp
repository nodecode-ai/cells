;;;; cell-test.lisp --- the lifecycle, the five tools over a stubbed CapRover,
;;;; and the refusals that hold a deployment back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here dials: *HTTP* is bound to a stub answering one CapRover
;;;; server from a struct, so every request is recorded and every reply is
;;;; this file's own. The tests prove that the doctor and the plan read the
;;;; app through it, that apply waits for the operator's approval and makes
;;;; exactly one deployment request, that a changed app is refused before the
;;;; request, and that the ledger answers with no server at all.

(in-package #:nodecode.test)

(define-test-slice "caprover" "CAPROVER-CELL-" :start nodecode-caprover:start-cell)

;;; --- one CapRover server, answering from a struct -----------------------------

(defparameter +old-image+ (format nil "docker.io/library/demo:1.0@sha256:~a" (make-string 64 :initial-element #\a)))

(defparameter +new-image+ (format nil "docker.io/library/demo:2.0@sha256:~a" (make-string 64 :initial-element #\b)))

(defstruct (fake (:constructor make-fake))
  "One stand-in CapRover server: what it reports, and every request it got."
  (calls nil)
  (version 11)
  (image +old-image+)
  (replicas 2)
  (building nil)
  (health 200)
  (lost nil)
  (app "demo-app"))

(defvar *fake* nil "The stub server the running test is talking to.")

(defun jso (&rest pairs)
  "One JSON object from PAIRS."
  (apply #'nlk:make-json-object pairs))

(defun wire-json (object)
  (nlk:encode-json-object object))

(defun answer (text)
  "TEXT, one tool's answer, as the object it encodes."
  (nlk:decode-json text))

(defun at (object &rest path)
  "The value at PATH in decoded JSON OBJECT."
  (apply #'nlk:json-value object :any path))

(defun arguments (&rest pairs)
  (apply #'jso pairs))

(defun fake-posts (fake)
  "The deployment requests FAKE was sent."
  (remove-if-not (lambda (call) (eq (first call) :post)) (fake-calls fake)))

(defun fake-definitions (fake)
  (wire-json
   (jso "status" 100
        "data" (jso "appDefinitions"
                     (vector (jso "appName" (fake-app fake)
                                   "deployedVersion" (fake-version fake)
                                   "instanceCount" (fake-replicas fake)
                                   "isAppBuilding" (fake-building fake)
                                   "environmentVariables" (vector (jso "key" "K" "value" "v"))
                                   "versions" (vector (jso "version" (fake-version fake)
                                                           "deployedImageName" (fake-image fake)))))))))

(defun fake-deploy (fake content)
  "Record the image CONTENT names as the app's own, as CapRover would."
  (let* ((outer (ignore-errors (nlk:decode-json content)))
         (inner (and outer (ignore-errors (nlk:decode-json (at outer "captainDefinitionContent")))))
         (image (and inner (at inner "imageName"))))
    (when (stringp image)
      (setf (fake-image fake) image))
    (incf (fake-version fake))))

(defun fake-http (fake)
  "A *HTTP* answering FAKE's one CapRover server, recording every request."
  (lambda (method url &key headers content &allow-other-keys)
    (let ((path (quri:uri-path (quri:uri url)))
          (host (quri:uri-host (quri:uri url))))
      (push (list method path (cdr (assoc "x-captain-auth" headers :test #'string-equal)) host) (fake-calls fake))
      (cond ((and (eq method :get) (equal host "demo.example.com")) (values "ok" (fake-health fake)))
            ((equal path "/api/v2/user/apps/appDefinitions/") (values (fake-definitions fake) 200))
            ((and (eq method :post) (stringp content))
             (when (fake-lost fake)
               (fake-deploy fake content)
               (error "the connection went away"))
             (fake-deploy fake content)
             (values (wire-json (jso "status" 100 "data" (jso))) 200))
            ((equal path (format nil "/api/v2/user/apps/appData/~a/" (fake-app fake)))
             (values (wire-json (jso "status" 100 "data" (jso "isAppBuilding" (fake-building fake) "isBuildFailed" nil))) 200))
            (t (values (wire-json (jso "status" 1105 "data" (jso))) 401))))))

(defun forget-ledger ()
  (ignore-errors (uiop:delete-directory-tree (nlk:home "caprover/") :validate t)))

(defmacro with-caprover ((&rest pairs) &body body)
  "BODY with the cell started on PAIRS (or the default section), a stub server bound as *HTTP*, and a token in hand."
  `(let* ((*fake* (make-fake))
          (nodecode-caprover::*http* (fake-http *fake*))
          (nodecode-caprover::*token* "test-token"))
     (forget-ledger)
     (with-cell-stop ((caprover-start ,@(or pairs
                                            '("url" "https://captain.example.com"
                                              "app_name" "demo-app"
                                              "health_url" "https://demo.example.com/health"))))
       ,@body)))

;;; --- lifecycle ----------------------------------------------------------------

(define-cell-lifecycle-tests "caprover"
  (:config "url" "https://captain.example.com" "app_name" "demo-app" "token_env" "CAPROVER_TEST_TOKEN")
  (:command "caprover")
  (:running (is (equal "https://captain.example.com" (nodecode-caprover::target)) "the target is the configured origin")
            (is (= 300 (nodecode-caprover::ttl)) "a plan lives five minutes by default"))
  (:refused ("plan_ttl_seconds" 5))
  (:idle nodecode-caprover:caprover-error (nodecode-caprover::target)))

;;; --- the doctor ---------------------------------------------------------------

(deftest caprover-cell-doctor-reads-the-app-through-the-stub ()
  (with-caprover ()
    (let ((report (answer (nodecode-caprover:doctor-tool (arguments)))))
      (is (equal "ready" (at report "status")) "a configured, reachable app is ready to plan")
      (is (equal "present_unverified" (at report "checks" "credential" "status")) "credential presence is reported, not proven")
      (is (= 11 (at report "checks" "app" "version")) "the doctor reports the deployed version")
      (is (equal +old-image+ (at report "checks" "app" "image")) "and the image it runs")
      (is (eq t (at report "checks" "http_health" "ok")) "and the health URL's answer")
      (is (find "test-token" (fake-calls *fake*) :key #'third :test #'equal) "the app read carried the token header")
      (is (null (fake-posts *fake*)) "and the doctor deployed nothing"))))

;;; --- planning -----------------------------------------------------------------

(deftest caprover-cell-plan-saves-a-plan-and-refuses-a-mutable-image ()
  (with-caprover ()
    (let ((mutable (answer (nodecode-caprover:plan-tool (arguments "image" "demo:latest")))))
      (is (search "immutable image" (at mutable "error" "detail")) "a mutable tag is refused")
      (is (null (nodecode-caprover::plans)) "and nothing is saved"))
    (let* ((plan (answer (nodecode-caprover:plan-tool (arguments "image" +new-image+))))
           (id (at plan "plan_id")))
      (is (equal "planned" (at plan "status")) "a pinned image saves a plan")
      (is (equal +old-image+ (at plan "baseline" "image")) "with the baseline it read")
      (is (= 11 (at plan "baseline" "version")) "including the deployed version")
      (is (probe-file (merge-pathnames (format nil "~a.json" id) (nodecode-caprover::plans-dir))) "the plan is a file under the home")
      (is (null (fake-posts *fake*)) "and planning deployed nothing"))))

;;; --- the operator's approval, one request, and the receipt --------------------

(deftest caprover-cell-apply-waits-for-the-operator-and-posts-once ()
  (with-caprover ()
    (let* ((plan (answer (nodecode-caprover:plan-tool (arguments "image" +new-image+))))
           (id (at plan "plan_id")))
      (let ((refused (answer (nodecode-caprover:apply-tool (arguments "plan_id" id)))))
        (is (search "approval from the operator" (at refused "error" "detail")) "the model cannot approve its own deployment")
        (is (null (fake-posts *fake*)) "and no deployment request went out"))
      (is (equal id (nodecode-caprover:approve-plan id)) "the operator approves the plan")
      (let ((applied (answer (nodecode-caprover:apply-tool (arguments "plan_id" id)))))
        (is (equal "reconcile_required" (at applied "status")) "an accepted request is uncertainty, not success")
        (is (= 1 (length (fake-posts *fake*))) "exactly one deployment request"))
      (let ((again (answer (nodecode-caprover:apply-tool (arguments "plan_id" id)))))
        (is (search "not planned" (at again "error" "detail")) "a second apply is refused")
        (is (= 1 (length (fake-posts *fake*))) "and sends nothing"))
      (let ((receipt (answer (nodecode-caprover:reconcile-tool (arguments "plan_id" id)))))
        (is (equal "metadata_http_observed" (at receipt "status")) "reconcile sees the new image and a healthy endpoint")
        (is (eq t (at receipt "checks" "exact_image")) "the image matches the plan")
        (is (null (at receipt "checks" "runtime_rollout_proven")) "runtime rollout is not claimed, and the receipt says so")))))

;;; --- drift, and a ledger that needs no server ---------------------------------

(deftest caprover-cell-apply-refuses-drift-and-status-needs-no-server ()
  (with-caprover ()
    (let* ((plan (answer (nodecode-caprover:plan-tool (arguments "image" +new-image+))))
           (id (at plan "plan_id")))
      (nodecode-caprover:approve-plan id)
      (setf (fake-version *fake*) 12)
      (let ((refused (answer (nodecode-caprover:apply-tool (arguments "plan_id" id)))))
        (is (equal "consumed_drift" (at refused "status")) "a changed app is refused")
        (is (null (fake-posts *fake*)) "before any deployment request went out"))
      (let* ((nodecode-caprover::*http* (lambda (&rest args) (declare (ignore args)) (error "no network")))
             (receipt (answer (nodecode-caprover:status-tool (arguments "plan_id" id)))))
        (is (equal "consumed_drift" (at receipt "plan" "status")) "the ledger answers with no server at all")
        (is (null (at receipt "server_request")) "and says it made no request")))))

;;; --- a missing setting --------------------------------------------------------

(deftest caprover-cell-a-missing-setting-refuses-with-nothing-dialled ()
  (with-caprover ("app_name" "demo-app")
    (let ((report (answer (nodecode-caprover:doctor-tool (arguments)))))
      (is (equal "missing_or_invalid" (at report "checks" "configuration" "status")) "a section without a url says so")
      (is (null (fake-calls *fake*)) "and nothing was dialled"))
    (let ((refused (answer (nodecode-caprover:plan-tool (arguments "image" +new-image+)))))
      (is (search "caprover url" (at refused "error" "detail")) "planning refuses, naming the url"))))
