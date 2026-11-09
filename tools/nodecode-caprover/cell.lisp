;;;; cell.lisp --- one CapRover app: plan it, approve it, deploy once, reconcile.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the CapRover Operator plugin for Hermes; see NOTICE. Five
;;;; tools the model calls and one slash command the operator types: the model
;;;; plans an immutable image for the one configured app, the operator
;;;; approves that plan id by hand with /caprover approve ID, and apply makes
;;;; at most one deployment POST. Plans, receipts, approvals and claims are
;;;; JSON files under the home, so an uncertain outcome survives a restart and
;;;; a second POST for the same plan is refused.

(defpackage #:nodecode-caprover
  (:use #:cl)
  (:export #:caprover-error #:start-cell #:stop-cell
           #:doctor-tool #:plan-tool #:apply-tool #:reconcile-tool #:status-tool
           #:run-slash #:approve-plan))

(in-package #:nodecode-caprover)

(nlk:define-peripheral caprover :not-running t)

;;; --- the words -------------------------------------------------------------------

(defparameter *approval-seconds* 300
  "How long the operator's approval of one plan stays usable; the plan's own
expiry may cut it shorter.")

(defparameter *request-timeout* 10
  "One CapRover request's ceiling, in seconds.")

(defparameter +terminal+
  '("verified" "metadata_http_observed" "deployment_recorded" "observed_unhealthy"
    "superseded" "consumed_drift" "consumed_expired" "consumed_preflight_failed" "expired")
  "A plan in one of these is done; only reconcile may look at it again.")

(defparameter +reobservable+
  '("deployment_recorded" "metadata_http_observed" "observed_unhealthy")
  "A receipt in one of these can be observed again, for later evidence only.")

(defparameter +primer+
  "CapRover Operator: plan, approve and deploy one immutable image to the one
app named in the caprover section, through the caprover_* tools.
  1. caprover_doctor      read-only setup and app check; no plan, no request
                           that writes.
  2. caprover_plan        IMAGE must end in @sha256:<64 lowercase hex>. Saves
                           a plan and reads the app's current version, image,
                           replicas and a fingerprint of its definition.
  3. caprover_apply       PLAN_ID. Refused until the OPERATOR types
                           /caprover approve PLAN_ID: the model cannot approve
                           its own deployment. It re-reads the app, refuses
                           drift, and makes at most one deployment request.
  4. caprover_reconcile   PLAN_ID. Read-only observation of a plan whose
                           outcome is uncertain. Never sends a deployment.
  5. caprover_status      the local ledger; no server request.
The model cannot choose a server, an app, a health URL or an approval. A lost
reply never makes a plan applicable again: reconcile it, never re-apply. An
image's startup behavior runs on deploy, and returning to an older image
restores no environment values, volumes or data."
  "What (help :caprover) answers while the cell runs.")

;;; --- the seam to the wire ---------------------------------------------------------

(defvar *http* #'nlk:http
  "The one seam to the network: (METHOD URL &key HEADERS CONTENT TIMEOUT)
=> (values BODY STATUS). Tests bind it to a stub; nothing else here dials.")

(defvar *token* nil
  "Bound by a layer or a test to answer the API token without the section.")

;;; --- JSON, and the objects the ledger holds ---------------------------------------

(defun object (&rest pairs)
  "One JSON object from alternating key/value PAIRS."
  (apply #'nlk:make-json-object pairs))

(defun field (object key)
  "OBJECT's member KEY, or NIL."
  (and (hash-table-p object) (gethash key object)))

(defun put (object key value)
  "OBJECT with KEY set to VALUE."
  (setf (gethash key object) value)
  object)

(defun truth (object key)
  "Whether OBJECT's member KEY is JSON true."
  (eq (field object key) t))

(defun encode (object)
  (nlk:encode-json-object object))

(defun read-json (path)
  "The one JSON object saved at PATH, or NIL when there is none."
  (let ((path (probe-file path)))
    (when path
      (let ((text (ignore-errors (uiop:read-file-string path))))
        (when (and text (plusp (length text)))
          (let ((value (ignore-errors (nlk:decode-json text))))
            (and (hash-table-p value) value)))))))

(defun write-text (path text)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                             :if-does-not-exist :create :external-format :utf-8)
    (write-string text out))
  path)

(defun write-json (path object)
  "OBJECT as JSON at PATH, one line."
  (write-text path (format nil "~a~%" (encode object))))

;;; --- what the section says --------------------------------------------------------

(defun setting-text (key)
  "The section's KEY as a trimmed string, \"\" when it is absent."
  (let ((value (setting key)))
    (if (stringp value) (string-trim '(#\Space #\Tab #\Newline #\Return) value) "")))

(defun target ()
  "The one CapRover origin this cell may reach, without its trailing slash."
  (let* ((value (string-right-trim "/" (setting-text :url)))
         (uri (and (plusp (length value)) (ignore-errors (quri:uri value))))
         (scheme (and uri (quri:uri-scheme uri)))
         (host (and uri (quri:uri-host uri)))
         (path (and uri (quri:uri-path uri))))
    (unless (and uri (member scheme '("https" "http") :test #'string=)
                 host (plusp (length host))
                 (null (quri:uri-userinfo uri))
                 (null (quri:uri-query uri))
                 (or (null path) (zerop (length path)) (string= path "/")))
      (fail "the caprover url is not a plain http(s) origin: ~s" (setting-text :url)))
    value))

(defun app-name ()
  "The one existing app this cell may deploy to."
  (let ((app (setting-text :app-name)))
    (when (zerop (length app))
      (fail "the caprover section names no app: set app_name"))
    (unless (every (lambda (character)
                     (or (alphanumericp character) (char= character #\-) (char= character #\.)))
                   app)
      (fail "~s is no CapRover app name" app))
    app))

(defun ttl ()
  "How long a plan stays applicable, from the section."
  (let ((value (setting :plan-ttl-seconds)))
    (if (and (integerp value) (<= 30 value 3600)) value 300)))

(defun token-value ()
  "The CapRover API token, from the environment variable or the file the
section names."
  (or *token*
      (let ((variable (setting-text :token-env))
            (file (setting-text :token-file)))
        (cond ((plusp (length variable))
               (let ((value (uiop:getenv variable)))
                 (unless (and value (plusp (length value)))
                   (fail "the environment variable ~a holds no CapRover token" variable))
                 value))
              ((plusp (length file))
               (let ((path (probe-file file)))
                 (unless path
                   (fail "the token file is not there: ~a" file))
                 (let ((value (string-trim '(#\Space #\Tab #\Newline #\Return)
                                            (uiop:read-file-string path))))
                   (when (zerop (length value))
                     (fail "the token file is empty: ~a" file))
                   value)))
              (t (fail "no CapRover token is configured: set token_env or token_file"))))))

;;; --- the ledger under the home ----------------------------------------------------

(defun data-dir () (nlk:home "caprover/"))
(defun plans-dir () (nlk:home "caprover/plans/"))
(defun approvals-dir () (nlk:home "caprover/approvals/"))

(defun id-p (value)
  "Whether VALUE is a plan id this cell wrote."
  (and (stringp value) (<= 1 (length value) 128) (alphanumericp (char value 0))
       (every (lambda (character)
                (or (alphanumericp character) (char= character #\-) (char= character #\_)))
              value)))

(defun plan-path (plan-id)
  (merge-pathnames (format nil "~a.json" plan-id) (plans-dir)))

(defun read-plan (plan-id)
  "The plan saved under PLAN-ID, or a refusal."
  (unless (id-p plan-id)
    (fail "~s is no plan id; caprover_status lists the ones saved" plan-id))
  (or (read-json (plan-path plan-id))
      (fail "no plan is saved under ~a" plan-id)))

(defun write-plan (plan)
  (write-json (plan-path (field plan "plan_id")) plan))

(defun plans ()
  "Every saved plan, newest first."
  (sort (remove nil (mapcar #'read-json (ignore-errors (uiop:directory-files (plans-dir) "*.json"))))
        #'string> :key (lambda (plan) (or (field plan "created_at") ""))))

(defun save-status (plan status &rest pairs)
  "PLAN saved with STATUS and PAIRS set, released when the status is terminal."
  (put plan "status" status)
  (loop for (key value) on pairs by #'cddr do (put plan key value))
  (write-plan plan)
  (when (member status +terminal+ :test #'string=)
    (release-claim (field plan "app") (field plan "plan_id")))
  plan)

;;; --- the claim: one unresolved plan an app ----------------------------------------

(defun claim-path (app)
  (merge-pathnames (format nil "~a.txt" app) (nlk:home "caprover/claims/")))

(defun active-claim (app)
  "The plan id an app has not finished with, or NIL."
  (let* ((path (claim-path app))
         (saved (and (probe-file path)
                     (string-trim '(#\Space #\Tab #\Newline #\Return)
                                  (uiop:read-file-string path)))))
    (when (and saved (plusp (length saved)))
      (let ((plan (ignore-errors (read-plan saved))))
        (cond ((null plan) (ignore-errors (delete-file path)) nil)
              ((member (field plan "status") +terminal+ :test #'string=)
               (ignore-errors (delete-file path)) nil)
              (t saved))))))

(defun claim (app plan-id)
  (write-text (claim-path app) plan-id))

(defun release-claim (app plan-id)
  (let ((path (claim-path app)))
    (when (probe-file path)
      (when (equal plan-id (string-trim '(#\Space #\Tab #\Newline #\Return)
                                         (uiop:read-file-string path)))
        (ignore-errors (delete-file path))))))

;;; --- the operator's approval ------------------------------------------------------

(defun approval-path (plan-id)
  (merge-pathnames (format nil "~a.json" plan-id) (approvals-dir)))

(defun save-approval (plan)
  "Record the operator's approval of PLAN; one apply may consume it."
  (write-json (approval-path (field plan "plan_id"))
              (object "plan_id" (field plan "plan_id")
                      "plan_hash" (field plan "plan_hash")
                      "target" (field plan "target")
                      "app" (field plan "app")
                      "approved_at" (nlk:iso-now)
                      "usable_until" (min (field plan "expires_at")
                                           (+ (get-universal-time) *approval-seconds*)))))

(defun take-approval (plan)
  "PLAN's unspent, unexpired approval, consumed here; NIL when there is none."
  (let* ((path (approval-path (field plan "plan_id")))
         (approval (read-json path)))
    (when approval
      (ignore-errors (delete-file path))
      (when (and (equal (field approval "plan_hash") (field plan "plan_hash"))
                 (equal (field approval "target") (field plan "target"))
                 (equal (field approval "app") (field plan "app"))
                 (integerp (field approval "usable_until"))
                 (> (field approval "usable_until") (get-universal-time)))
        approval))))

(defun approve-plan (plan-id)
  "The operator's approval of PLAN-ID: one deployment attempt may follow."
  (let ((plan (read-plan plan-id)))
    (unless (and (equal (field plan "app") (app-name))
                 (equal (field plan "target") (target)))
      (fail "plan ~a was made for another app or server" plan-id))
    (unless (equal (field plan "status") "planned")
      (fail "plan ~a is ~a, not planned" plan-id (field plan "status")))
    (when (> (get-universal-time) (field plan "expires_at"))
      (fail "plan ~a has expired; plan the image again" plan-id))
    (save-approval plan)
    plan-id))

;;; --- CapRover, read only ----------------------------------------------------------

(defun dial (method url &key headers content)
  "One request through *HTTP*; a transport failure is a refusal, never a retry."
  (handler-case
      (funcall *http* method url :headers headers :content content :timeout *request-timeout*)
    (caprover-error (condition) (error condition))
    (error () (fail "the request to ~a did not complete" url))))

(defun api (method path &key content)
  "One CapRover v2 call, the token in its own header; => the envelope's data."
  (multiple-value-bind (body status)
      (dial method (concatenate 'string (target) path)
            :headers (list (cons "x-captain-auth" (token-value))
                           (cons "content-type" "application/json"))
            :content content)
    (unless (and (integerp status) (<= 200 status 299))
      (fail "CapRover answered HTTP ~a for ~a" status path))
    (let ((envelope (ignore-errors (nlk:decode-json (if (stringp body) body "")))))
      (unless (hash-table-p envelope)
        (fail "the reply to ~a was not a CapRover envelope" path))
      (let ((code (field envelope "status")))
        (cond ((member code '(1105 1106 1112 1113 1115))
               (fail "CapRover rejected the API token"))
              ((eql code 1102)
               (fail "this token may not act on the app"))
              ((not (eql code 100))
               (fail "CapRover answered status ~a for ~a" code path)))
        (field envelope "data")))))

(defun definition-of (app)
  "The one app definition CapRover reports for APP."
  (let* ((data (api :get "/api/v2/user/apps/appDefinitions/"))
         (definitions (coerce (nlk:json-array data "appDefinitions") 'list))
         (matches (remove-if-not (lambda (item) (equal (field item "appName") app))
                                  definitions)))
    (when (zerop (length definitions))
      (fail "CapRover returned no app definitions"))
    (unless matches
      (fail "the app ~a is not on this CapRover server" app))
    (when (cdr matches)
      (fail "CapRover returned the app ~a more than once" app))
    (first matches)))

(defun snapshot ()
  "Read the configured app: version, image, replicas and a fingerprint of its
definition. => a JSON object; the definition itself is never returned."
  (let* ((app (app-name))
         (definition (definition-of app))
         (build (api :get (format nil "/api/v2/user/apps/appData/~a/" app)))
         (version (field definition "deployedVersion"))
         (replicas (field definition "instanceCount"))
         (versions (nlk:json-array definition "versions"))
         (entry (find-if (lambda (item) (eql (field item "version") version)) versions))
         (image (field entry "deployedImageName")))
    (unless (and (integerp version) (>= version 0))
      (fail "the app definition of ~a carries no deployed version" app))
    (unless (and (integerp replicas) (>= replicas 0))
      (fail "the app definition of ~a carries no replica count" app))
    (object "app" app
            "version" version
            "replicas" replicas
            "image" (or (and (stringp image) image) "")
            "is_building" (or (truth build "isAppBuilding") (truth definition "isAppBuilding"))
            "config_fingerprint" (nlk:sha256-text (encode definition)))))

(defun health ()
  "The configured health URL's answer, or that none is configured."
  (let ((url (setting-text :health-url)))
    (if (zerop (length url))
        (object "configured" nil "ok" nil "status_code" :null "reason" "not_configured")
        (handler-case
            (multiple-value-bind (body status) (dial :get url)
              (declare (ignore body))
              (object "configured" t
                      "ok" (and (integerp status) (<= 200 status 299))
                      "status_code" (if (integerp status) status :null)
                      "reason" (if (and (integerp status) (<= 200 status 299))
                                   "http_ok" "http_unhealthy")))
          (caprover-error ()
            (object "configured" t "ok" nil "status_code" :null "reason" "unreachable"))))))

;;; --- the tools --------------------------------------------------------------------

(defun immutable-image-p (value)
  "Whether VALUE is an OCI reference pinned to a sha256 digest."
  (let ((at (and (stringp value) (position #\@ value :from-end t))))
    (and at
         (plusp at)
         (<= (length value) 512)
         (every (lambda (character)
                  (and (<= 33 (char-code character) 126)
                       (or (alphanumericp character)
                           (find character ".-_/:@"))))
                value)
         (null (search "//" value))
         (let ((digest (subseq value (1+ at))))
           (and (string= "sha256:" digest :end2 (min 7 (length digest)))
                (= (length digest) 71)
                (every (lambda (character) (digit-char-p character 16))
                       (subseq digest 7))
                (string= (subseq digest 7) (string-downcase (subseq digest 7))))))))

(defun doctor-json ()
  "Read-only setup and app checks; no plan, approval or deployment."
  (let* ((checks (object))
         (report (object "status" "unavailable" "checks" checks)))
    (let ((url (setting-text :url))
          (app (setting-text :app-name)))
      (put checks "configuration"
           (if (and (plusp (length url)) (plusp (length app)))
               (object "status" "present")
               (object "status" "missing_or_invalid")))
      (when (or (zerop (length url)) (zerop (length app)))
        (put report "guidance"
             (object "next_step" "set url and app_name in the caprover section, then call caprover_doctor again"
                     "automatic" nil))
        (return-from doctor-json report)))
    (let ((present (handler-case (progn (token-value) t)
                     (caprover-error () nil))))
      (put checks "credential" (object "status" (if present "present_unverified" "missing")))
      (unless present
        (put report "status" "needs_credentials")
        (put report "guidance"
             (object "next_step" "set token_env to the name of the variable holding the CapRover API token"
                     "automatic" nil))
        (return-from doctor-json report)))
    (let ((observed (handler-case (snapshot) (caprover-error () nil))))
      (unless observed
        (put checks "app" (object "status" "unavailable"))
        (put report "guidance"
             (object "next_step" "check url and app_name, then call caprover_doctor again"
                     "automatic" nil))
        (return-from doctor-json report))
      (put checks "app"
           (object "status" "observed"
                   "version" (field observed "version")
                   "image" (field observed "image")
                   "replicas" (field observed "replicas")
                   "is_building" (truth observed "is_building")))
      (put checks "http_health" (health))
      (put report "status" "ready")
      (put report "guidance"
           (object "next_step" "call caprover_plan with an image pinned to a sha256 digest"
                   "automatic" nil))
      report)))

(defun digest (text)
  "TEXT's sha256, in hex."
  (let ((digest (nlk:sha256-text text)))
    (if (and (< 7 (length digest)) (string= "sha256:" digest :end2 7))
        (subseq digest 7)
        digest)))

(defun new-plan-id ()
  (format nil "p~a"
          (subseq (digest (format nil "~a|~a|~a"
                                   (get-universal-time) (get-internal-real-time)
                                   (random 1000000)))
                  0 20)))

(defun plan-hash (plan)
  "The digest binding a plan's identity, target and saved baseline."
  (let ((baseline (field plan "baseline")))
    (digest
     (format nil "~a|~a|~a|~a|~a|~a|~a"
             (field plan "plan_id") (field plan "target") (field plan "app")
             (field plan "image") (field baseline "version")
             (field baseline "config_fingerprint") (field plan "expires_at")))))

(defun plan-json (image)
  "caprover_plan: save a state-bound plan for IMAGE; no deployment request."
  (let ((app (app-name))
        (target (target)))
    (unless (immutable-image-p image)
      (fail "~s is no immutable image: it must end in @sha256: and 64 lowercase hex characters"
            image))
    (when (active-claim app)
      (fail "plan ~a is still unresolved; reconcile it before planning another"
            (active-claim app)))
    (let* ((baseline (snapshot)))
      (when (truth baseline "is_building")
        (fail "the app ~a is building; wait for it to settle" app))
      (when (equal (field baseline "image") image)
        (fail "the app ~a already runs that image" app))
      (let ((plan (object "plan_id" (new-plan-id)
                          "target" target
                          "app" app
                          "image" image
                          "baseline" baseline
                          "created_at" (nlk:iso-now)
                          "expires_at" (+ (get-universal-time) (ttl))
                          "observations" (vector))))
        (put plan "plan_hash" (plan-hash plan))
        (put plan "status" "planned")
        (put plan "guidance"
             (object "next_step" "ask the operator to run /caprover approve <plan_id>, then call caprover_apply"
                     "automatic" nil))
        (write-plan plan)
        plan))))

(defun same-baseline-p (left right)
  (and (eql (field left "version") (field right "version"))
       (equal (field left "image") (field right "image"))
       (eql (field left "replicas") (field right "replicas"))
       (equal (field left "config_fingerprint") (field right "config_fingerprint"))
       (eq (truth left "is_building") (truth right "is_building"))))

(defun apply-json (plan-id)
  "caprover_apply: consume the operator's approval and make at most one POST."
  (let* ((app (app-name))
         (target (target))
         (plan (read-plan plan-id)))
    (unless (and (equal (field plan "app") app) (equal (field plan "target") target))
      (fail "plan ~a was made for another app or server" plan-id))
    (unless (equal (field plan "status") "planned")
      (fail "plan ~a is ~a, not planned" plan-id (field plan "status")))
    (when (> (get-universal-time) (field plan "expires_at"))
      (return-from apply-json (save-status plan "expired")))
    (unless (take-approval plan)
      (fail "plan ~a has no approval from the operator: they run /caprover approve ~a, then you call caprover_apply"
            plan-id plan-id))
    (let ((claimed (active-claim app)))
      (when claimed
        (fail "plan ~a is still unresolved; reconcile it before another deployment" claimed)))
    (claim app plan-id)
    (let ((fresh (handler-case (snapshot) (caprover-error () nil))))
      (unless fresh
        (return-from apply-json
          (save-status plan "consumed_preflight_failed"
                       "checks" (object "preflight" "unavailable" "post_attempted" nil))))
      (unless (same-baseline-p (field plan "baseline") fresh)
        (return-from apply-json
          (save-status plan "consumed_drift" "observed" fresh
                       "checks" (object "preflight" "drift" "post_attempted" nil))))
      (when (> (get-universal-time) (field plan "expires_at"))
        (return-from apply-json
          (save-status plan "consumed_expired"
                       "checks" (object "preflight" "expired" "post_attempted" nil)))))
    (save-status plan "sending"
                 "checks" (object "preflight" "matched" "post_attempted" t)
                 "attempted_at" (nlk:iso-now))
    (let ((outcome (handler-case
                       (progn
                         (api :post (format nil "/api/v2/user/apps/appData/~a/" app)
                              :content (encode (object "captainDefinitionContent"
                                                       (encode (object "schemaVersion" 2
                                                                       "imageName" (field plan "image"))))))
                         "accepted")
                     (caprover-error () "uncertain"))))
      (save-status plan "reconcile_required"
                   "trigger_outcome" outcome
                   "guidance" (object "next_step" "call caprover_reconcile with this plan id; never apply it again"
                                      "automatic" nil)))))

(defun observation (plan status observed checks)
  (let* ((history (nlk:json-array plan "observations"))
         (entry (object "status" status
                        "observed_at" (nlk:iso-now)
                        "version" (or (and observed (field observed "version")) :null)
                        "image" (or (and observed (field observed "image")) :null)))
         (kept (coerce (last (append (coerce history 'list) (list entry)) 8) 'vector)))
    (put plan "observations" kept)
    (put plan "checks" checks)))

(defun reconcile-json (plan-id)
  "caprover_reconcile: observe a plan's outcome, read-only, and record it."
  (let* ((app (app-name))
         (plan (read-plan plan-id))
         (status (field plan "status")))
    (unless (and (equal (field plan "app") app) (equal (field plan "target") (target)))
      (fail "plan ~a was made for another app or server" plan-id))
    (unless (or (member status '("planned" "preflight_pending" "sending" "reconcile_required")
                         :test #'string=)
                (member status +reobservable+ :test #'string=))
      (fail "plan ~a is ~a; reconciliation is not required" plan-id status))
    (let ((observed (handler-case (snapshot) (caprover-error () nil)))
          (baseline (field plan "baseline")))
      (unless observed
        (observation plan
                     (if (member status +reobservable+ :test #'string=) status "reconcile_required")
                     nil (object "observation" "unavailable"))
        (put plan "guidance"
             (object "next_step" "the observation failed; keep the plan and call caprover_reconcile again"
                     "automatic" nil))
        (return-from reconcile-json (save-status plan (field plan "status"))))
      (let* ((newer (> (field observed "version") (field baseline "version")))
             (exact (equal (field observed "image") (field plan "image")))
             (deployed (and newer exact (not (truth observed "is_building"))))
             (probe (if deployed (health)
                         (object "configured" nil "ok" nil "status_code" :null
                                 "reason" "deployment_not_observed")))
             (replicas (field observed "replicas"))
             (unhealthy (or (eql replicas 0) (eq (field probe "ok") nil)))
             (result (cond ((and deployed (not unhealthy) (eq (field probe "ok") t))
                            "metadata_http_observed")
                           (deployed (if unhealthy "observed_unhealthy" "deployment_recorded"))
                           ((member status +reobservable+ :test #'string=) "deployment_recorded")
                           (t "reconcile_required")))
             (checks (object "newer_version" newer
                             "exact_image" exact
                             "not_building" (not (truth observed "is_building"))
                             "deployment_recorded" deployed
                             "configured_replicas" replicas
                             "http_health" probe
                             "runtime_rollout_proven" nil)))
        (observation plan result (if deployed observed nil) checks)
        (put plan "observed" observed)
        (put plan "observed_at" (nlk:iso-now))
        (put plan "guidance"
             (object "next_step" (if (member result +reobservable+ :test #'string=)
                                      "call caprover_reconcile again later for later evidence; this never authorizes another deployment"
                                      "inspect the checks; no deployment was observed")
                     "automatic" nil))
        (save-status plan result)))))

(defun status-json (plan-id)
  "caprover_status: the local ledger, and no server request."
  (if (and (stringp plan-id) (plusp (length plan-id)))
      (object "plan" (read-plan plan-id) "server_request" nil)
      (object "plans" (coerce (plans) 'vector) "server_request" nil)))

(defun refusal-json (text)
  (object "error" (object "detail" text)
          "guidance" (object "next_step" "fix what the detail names, then call the same tool again"
                             "automatic" nil)))

(defun tool-answer (function arguments)
  (handler-case (encode (funcall function arguments))
    (caprover-error (condition)
      (encode (refusal-json (caprover-error-detail condition))))))

(defun doctor-tool (arguments)
  "caprover_doctor: the read-only setup and app check, as JSON."
  (tool-answer (lambda (ignored) (declare (ignore ignored)) (doctor-json)) arguments))

(defun plan-tool (arguments)
  "caprover_plan: ARGUMENTS' image planned for the configured app, as JSON."
  (tool-answer (lambda (args) (plan-json (field args "image"))) arguments))

(defun apply-tool (arguments)
  "caprover_apply: ARGUMENTS' plan applied once, after the operator approved it."
  (tool-answer (lambda (args) (apply-json (field args "plan_id"))) arguments))

(defun reconcile-tool (arguments)
  "caprover_reconcile: ARGUMENTS' plan observed, read-only."
  (tool-answer (lambda (args) (reconcile-json (field args "plan_id"))) arguments))

(defun status-tool (arguments)
  "caprover_status: the ledger, and no server request."
  (tool-answer (lambda (args) (status-json (field args "plan_id"))) arguments))

;;; --- the slash command: where the operator approves --------------------------------

(defun plans-text ()
  (let ((saved (plans)))
    (if (null saved)
        "caprover: no plan is saved yet"
        (with-output-to-string (out)
          (format out "caprover: ~d saved plan~:p" (length saved))
          (dolist (plan saved)
            (format out "~%~a  ~a  ~a"
                    (field plan "plan_id") (field plan "status") (field plan "image")))))))

(defun run-slash (args session-id)
  "/caprover list | show ID | approve ID. Approving is the operator's act:
one deployment attempt of that plan may follow."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (space (position #\Space text))
         (head (string-downcase (subseq text 0 space)))
         (rest (string-trim '(#\Space #\Tab) (if space (subseq text space) ""))))
    (cond ((string= head "approve")
           (format nil "caprover: plan ~a is approved; one deployment attempt may follow it"
                   (approve-plan rest)))
          ((string= head "show")
           (let ((plan (read-plan rest)))
             (format nil "caprover: ~a  ~a  ~a  available until ~a"
                     (field plan "plan_id") (field plan "status")
                     (field plan "image") (field plan "expires_at"))))
          (t (plans-text)))))

;;; --- what the model is offered ----------------------------------------------------

(defparameter +doctor-schema+
  (list :description
        "Diagnose the CapRover setup with read-only checks: the settings, the API token's presence, the app and an optional health URL. Creates no plan, approval or deployment."
        :parameters (object "type" "object" "properties" (object) "required" (vector))))

(defparameter +plan-schema+
  (list :description
        "Plan an immutable image for the configured CapRover app: read its current version, image, replicas and definition fingerprint, and save a plan. Sends no deployment."
        :parameters (object "type" "object"
                            "properties" (object "image"
                                                 (object "type" "string"
                                                         "description" "An image reference ending in @sha256: and 64 lowercase hex characters."))
                            "required" (vector "image"))))

(defparameter +apply-schema+
  (list :description
        "Apply one saved plan after the operator approved that plan id with /caprover approve. Re-reads the app, refuses drift, and makes at most one deployment request; an uncertain outcome must be reconciled, never re-applied."
        :parameters (object "type" "object"
                            "properties" (object "plan_id"
                                                 (object "type" "string"
                                                         "description" "The plan id caprover_plan answered."))
                            "required" (vector "plan_id"))))

(defparameter +reconcile-schema+
  (list :description
        "Observe a saved plan's outcome, read-only, and record the receipt. Never sends a deployment request."
        :parameters (object "type" "object"
                            "properties" (object "plan_id" (object "type" "string"))
                            "required" (vector "plan_id"))))

(defparameter +status-schema+
  (list :description
        "Read one saved plan and its receipt, or list the recent ones. The local ledger only: no server request."
        :parameters (object "type" "object"
                            "properties" (object "plan_id" (object "type" "string"))
                            "required" (vector))))

;;; --- the declaration --------------------------------------------------------------

(nle:define-cell caprover
  (:section ("caprover")
    (:guide "url is the https origin of the one CapRover server; app_name an app that already exists there; token_env or token_file holds its API token; plan_ttl_seconds bounds how long a plan waits for approval")
    ("url" :string :required t
           :doc "the https origin of the one CapRover server this cell may operate")
    ("app_name" :string :required t
                :doc "the existing app on that server; a tool call cannot choose another")
    ("health_url" :string :default ""
                  :doc "optional fixed health URL, read separately from the deployment record")
    ("plan_ttl_seconds" :integer :default 300 :min 30
                        :doc "how long a plan stays applicable, in seconds; approval does not extend it")
    ("token_env" :env
                 :doc "the name of the environment variable holding the CapRover API token")
    ("token_file" :path
                  :doc "a file holding the CapRover API token, when it is not in the environment"))
  (:help :caprover "caprover: plan, approve and deploy one immutable image to your CapRover app; /caprover approve ID" +primer+)
  (:command "caprover" 'run-slash
            :catches 'caprover-error
            :description "CapRover plans: list, show ID, or approve ID to allow one deployment"
            :argument-hint "list | show ID | approve ID")
  (:tool "caprover_doctor" +doctor-schema+ #'doctor-tool)
  (:tool "caprover_plan" +plan-schema+ #'plan-tool)
  (:tool "caprover_apply" +apply-schema+ #'apply-tool)
  (:tool "caprover_reconcile" +reconcile-schema+ #'reconcile-tool)
  (:tool "caprover_status" +status-schema+ #'status-tool))
