;;;; cell.lisp --- Comfyrizz: the film pipeline as twelve tools over a loopback engine.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of mojomast/comfyrizz-hermes (see NOTICE). The Hermes
;;;; plugin is a skill and a stdio MCP server over the user's own Comfyrizz
;;;; engine; here the twelve comfyrizz_* tools are Nodecode tools of the
;;;; cell's own, the engine is reached with NLK:HTTP instead of Node's fetch,
;;;; and the skill's rules live in (help :comfyrizz), which the model reads
;;;; before it starts. The original's loopback rule is kept: an engine_url
;;;; that is not a loopback http(s) URL with no credentials refuses at start,
;;;; and the token rides the two headers the engine already accepts, never a
;;;; description, a result or an error message.

(defpackage #:nodecode-comfyrizz
  (:use #:cl)
  (:export #:comfyrizz-error #:comfyrizz-error-detail))

(in-package #:nodecode-comfyrizz)

(nlk:define-peripheral comfyrizz :not-running t)

(nlk:define-error comfyrizz-stub (comfyrizz-error) ()
  (:documentation "The engine was not reached: the original answers `stub: true' with
a reason instead of failing, so the model can say what is missing and not invent a film."))

(defparameter +detail-chars+ 300
  "How much of an engine's own words a refusal repeats.")

(defparameter +primer+
  "Comfyrizz runs a film pipeline on your own machine: brief, plan, spend card, explicit
acceptance, generation, CPU assembly, export, and one clip's retake. Twelve tools talk to
the loopback engine the comfyrizz section names; without it they answer `stub: true' with
a reason, and nothing may be invented in its place.
  comfyrizz_status                 engine health; start here
  comfyrizz_list_projects          reuse a project before creating one
  comfyrizz_create_project         {brief, title?} create a production and draft its plan
  comfyrizz_plan                    {brief | projectId + level + compute} planning is free
  comfyrizz_get_plan                the latest proposed plan and its hash
  comfyrizz_get_spend_card         {projectId} the cap, the estimate, the approvalId
  comfyrizz_accept_generation      {projectId, actionId, planId, planRevision, approvalId}
  comfyrizz_job_status             {jobId} narrate queued, running, completed, failed
  comfyrizz_list_takes             {projectId, clipId?} compare takes before a retake
  comfyrizz_assemble               {projectId} the CPU timeline, no generation spend
  comfyrizz_export                 {projectId} the bundle's path, quoted exactly
  comfyrizz_retake                 {projectId, clipId, change} one clip, no approval of its own
The rules that never bend: no silent spend (show the spend card and get an unambiguous yes
before comfyrizz_accept_generation, and before every paid retake); start no Colab or paid
capacity yourself; never say footage, a job or a path is ready without the tool result that
says so; never print or ask for a key."
  "What (help :comfyrizz) answers while the cell runs: the original's skill, in short.")

;;; --- the section, read back ------------------------------------------------

(defun nonblank (text)
  "TEXT trimmed, when it is a non-blank string; else NIL."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun authority-host (authority)
  "AUTHORITY's host, lowercased: a bracketed IPv6 literal, else what stands
before the port."
  (if (and (plusp (length authority)) (char= #\[ (char authority 0)))
      (subseq authority 0 (min (length authority) (or (position #\] authority) (length authority))))
      (subseq authority 0 (or (position #\: authority) (length authority)))))

(defun parse-engine-url (text)
  "TEXT as (values BASE HOST), BASE without its trailing slashes, when it is
an http or https URL with a host and no embedded credentials; else
(values NIL REASON) in the words the section's refusal uses."
  (let* ((url (string-downcase (or text "")))
         (scheme (cond ((uiop:string-prefix-p "https://" url) "https://")
                       ((uiop:string-prefix-p "http://" url) "http://")))
         (rest (and scheme (subseq url (length scheme))))
         (authority (and rest (subseq rest 0 (or (position #\/ rest) (length rest))))))
    (cond ((null scheme) (values nil "has no http:// or https:// scheme"))
          ((or (null authority) (zerop (length authority))) (values nil "names no host"))
          ((find #\@ authority) (values nil "carries credentials; name the host alone"))
          (t (values (string-right-trim "/" url) (authority-host authority))))))

(defun loopback-host-p (host)
  "Whether HOST is a loopback name or address, as the original's client allows."
  (member (string-downcase host) '("127.0.0.1" "localhost" "[::1]" "::1") :test #'equal))

(defun read-settings (values table)
  "The declared VALUES, plus the base URL they were checked into and the
token token_env names, read once, here. A URL that is not loopback, and a
token_env naming an empty variable, refuse at start."
  (declare (ignore table))
  (let* ((url (nonblank (getf values :engine-url)))
         (env (nonblank (getf values :token-env)))
         (token (and env (nonblank (uiop:getenv env))))
         (plist (copy-list values)))
    (multiple-value-bind (base host) (parse-engine-url url)
      (cond ((null base)
             (nlk:config-error "engine_url ~s is not a URL: ~a. Set it to the Comfyrizz engine on this machine, such as http://127.0.0.1:4319." url host))
            ((not (loopback-host-p host))
             (nlk:config-error "engine_url ~s is not a loopback URL: the original reaches only 127.0.0.1, localhost or ::1, and never a remote or tailnet host. Point it at the engine on this machine." url))
            (t (setf (getf plist :base-url) base))))
    (when (and env (null token))
      (nlk:config-error "token_env names ~a, but nothing is set there: put the engine's token in it, or clear token_env when the engine asks for none" env))
    (setf (getf plist :token) token)
    plist))

;;; --- one request -----------------------------------------------------------

(defun request-headers ()
  "JSON in and out, and the token in the two headers the engine accepts."
  (let ((headers (list (cons "accept" "application/json"))))
    (when (setting :token)
      (push (cons "authorization" (format nil "Bearer ~a" (setting :token))) headers)
      (push (cons "x-comfyrizz-token" (setting :token)) headers))
    headers))

(defun decode-body (text)
  "TEXT, an HTTP body, decoded; an empty or unreadable one reads as NIL."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (when (and text (plusp (length text)))
      (or (ignore-errors (nlk:decode-json text)) text))))

(defun refuse-answer (status text path)
  "The refusal one answered status becomes: the engine's own error and code
when it named them, the original's route-not-wired reason for a bare 404."
  (let* ((body (decode-body text))
         (error (nlk:json-value body :text "error"))
         (code (nlk:json-value body :text "code")))
    (cond ((and (eql status 404) (null error) (null code))
           (fail "engine_route_not_wired: the running Comfyrizz engine does not expose ~a yet (HTTP 404). The engine is older than this cell's route list; update Comfyrizz, or use the routes it does expose." path))
          (t (fail "engine_error: the Comfyrizz engine refused ~a with HTTP ~a~@[: ~a~]~@[ (~a)~]"
                   path status (and error (nlk:clip error +detail-chars+)) code)))))

(defun request (method path &key body)
  "METHOD PATH on the configured engine, BODY as its JSON, => the decoded
answer. COMFYRIZZ-STUB when the engine is not reached at all; the cell's
refusal when it answers a status that is not 2xx."
  (let ((text nil)
        (status nil)
        (timeout (setting :timeout-seconds)))
    (handler-case
        (multiple-value-setq (text status)
          (nlk:http method (concatenate 'string (setting :base-url) path)
                    :headers (request-headers)
                    :content (and body (nlk:encode-json-object body))
                    :timeout timeout
                    :connect-timeout (min 10 timeout)))
      (error (condition)
        (error 'comfyrizz-stub
               :detail (format nil "the Comfyrizz engine did not answer on ~a: ~a"
                               (setting :base-url)
                               (nlk:one-line (princ-to-string condition))))))
    (if (and (integerp status) (< status 300))
        (decode-body text)
        (refuse-answer status text path))))

(defun render (header body)
  "HEADER, then BODY as the JSON text one tool result carries, clipped to
the section's answer_chars."
  (format nil "~a~%~a" header (nlk:clip (nlk:encode-json-object body) (setting :answer-chars))))

(defun api-path (control &rest ids)
  "CONTROL with each of IDS percent-encoded into its one ~a."
  (apply #'format nil control
         (mapcar (lambda (id) (quri:url-encode (princ-to-string id) :encoding :utf-8)) ids)))

(defun command-id ()
  "One dispatch id, fresh per call, sent as both the command id and the
idempotency key, as the original's dispatchCommandId is."
  (format nil "nc_~d_~d" (get-universal-time) (random 1000000)))

;;; --- the verbs the tools and /comfyrizz call -------------------------------

(define-verb status ()
  "Whether the engine answers, and its health. The place to start."
  (render (format nil "comfyrizz_status: the engine at ~a answers~@[, with the token token_env names~]"
                  (setting :base-url) (setting :token))
          (request :get "/api/health")))

(define-verb create-project (brief &key title)
  "BRIEF as a new production on the engine, with its plan drafted when the
engine will draft one. Never spends."
  (let* ((id (command-id))
         (created (request :post "/api/productions"
                           :body (nlk:json-object
                                  "commandId" id "idempotencyKey" id
                                  "kind" "production.create"
                                  "expected" (nlk:json-object "productionRevision" 0)
                                  "payload" (nlk:json-object "production" (nlk:json-object)))))
         (draft (handler-case
                    (request :post "/api/brief"
                             :body (nlk:json-object "prompt" brief :opt "title" title))
                  (comfyrizz-error (condition)
                    (nlk:json-object "error" (comfyrizz-error-detail condition)))))
         (project (nlk:json-value created :text "productionId")))
    (render (format nil "comfyrizz_create_project: ~a~@[ (~a)~]; the project id every later call carries is ~a"
                    (or (nonblank title) "a new production") brief (or project "not stated by the engine"))
            (nlk:json-object "project" created :opt "projectId" project "brief" draft))))

(define-verb list-projects (&key limit)
  "The productions the engine knows about."
  (render (format nil "comfyrizz_list_projects:~@[ at most ~d~] packages" limit)
          (request :get (if limit
                            (format nil "/api/packages?limit=~d" (min 200 (max 1 limit)))
                            "/api/packages"))))

(define-verb plan-project (&key project-id brief level compute only-shot-ids)
  "A brief's proposed plan, a project's easy plan at LEVEL and COMPUTE, or
the project as the engine has it. Planning never dispatches paid work."
  (cond
    ((nonblank brief)
     (render "comfyrizz_plan: a free proposal for the brief" 
             (request :post "/api/brief" :body (nlk:json-object "prompt" brief))))
    ((and (nonblank project-id) (nonblank level) (nonblank compute))
     (render (format nil "comfyrizz_plan: an easy plan for ~a at ~a on ~a" project-id level compute)
             (request :post (api-path "/api/productions/~a/easy-plan" project-id)
                      :body (nlk:json-object "level" level "compute" compute
                                              :opt "onlyShotIds" only-shot-ids))))
    ((nonblank project-id)
     (render (format nil "comfyrizz_plan: ~a as the engine has it" project-id)
             (request :get (api-path "/api/productions/~a" project-id))))
    (t (fail "PLAN_TARGET_REQUIRED: name a brief, or a projectId with level and compute, before planning. No engine call was made."))))

(define-verb get-plan (&key project-id plan-id)
  "The latest plan.proposed entry of a project's journal, with its hash."
  (let ((id (or (nonblank project-id) (nonblank plan-id)
                (fail "PLAN_TARGET_REQUIRED: name a projectId or a planId. No engine call was made."))))
    (let* ((journal (request :get (api-path "/api/productions/~a/events?after=0" id)))
           (events (or (nlk:json-array journal "events") #()))
           (latest nil))
      (loop for event across events
            when (and (hash-table-p event)
                      (equal "plan.proposed" (nlk:json-value event :string "type")))
              do (setf latest event))
      (let ((plan (and latest (nlk:json-value latest :object "data"))))
        (render (format nil "comfyrizz_get_plan: ~a, ~a; last sequence ~a"
                        id
                        (if plan "a plan was proposed" "no plan.proposed entry in the journal yet")
                        (or (nlk:json-value journal :integer "lastSequence") "unstated"))
                (nlk:json-object "projectId" id
                                 :when plan "plan" plan
                                 :when plan "planHash" (nlk:json-value plan :text "planHash")
                                 "lastSequence" (nlk:json-value journal :integer "lastSequence")))))))

(define-verb spend-card (project-id)
  "A project's spend cap and budget ledger: the numbers to show the user
before any paid work."
  (let ((card (request :get (api-path "/api/productions/~a/spend-cap" project-id))))
    (render (format nil "comfyrizz_get_spend_card: ~a, ceiling ~a. Show this in chat and ask for an unambiguous yes before accepting anything."
                    project-id (or (nlk:json-value card :any "ceiling") "unstated"))
            card)))

(define-verb accept-generation
    (project-id action-id
     &key plan-id plan-revision production-revision approval-id approval-token)
  "Dispatch one plan action. Refuses without an explicit approval id, and
without the plan id and revision the engine dispatches against."
  (let ((approval (or (nonblank approval-id) (nonblank approval-token))))
    (unless approval
      (fail "APPROVAL_REQUIRED: no approval id was given, so no spend was authorized. Read the spend card with comfyrizz_get_spend_card, show it, and ask for the user's explicit yes."))
    (unless (and (nonblank plan-id) plan-revision)
      (fail "APPROVAL_TARGET_INCOMPLETE: planId and planRevision are required to dispatch an approved plan action. No spend was authorized."))
    (let* ((id (command-id))
           (dispatched (request :post (api-path "/api/productions/~a/actions/~a" project-id action-id)
                                :body (nlk:json-object
                                       "commandId" id "idempotencyKey" id
                                       "kind" "action.dispatch"
                                       "expected" (nlk:json-object "productionRevision" (or production-revision 0))
                                       "payload" (nlk:json-object "planId" plan-id
                                                                   "planRevision" plan-revision
                                                                   "actionId" action-id
                                                                   "approvalId" approval)))))
      (render (format nil "comfyrizz_accept_generation: action ~a on ~a accepted with approval ~a"
                      action-id project-id approval)
              dispatched))))

(define-verb job (job-id)
  "One run or job record."
  (render (format nil "comfyrizz_job_status: ~a" job-id)
          (request :get (api-path "/api/run/~a" job-id))))

(define-verb takes (project-id &key clip-id)
  "A project's clips and takes, or one clip's takes."
  (render (format nil "comfyrizz_list_takes: ~a~@[ clip ~a~]" project-id clip-id)
          (request :get (if (nonblank clip-id)
                            (api-path "/api/package/~a/clips/~a/takes" project-id clip-id)
                            (api-path "/api/package/~a/clips" project-id)))))

(define-verb assemble (project-id &key package editor revision)
  "The project timeline, compiled on the local CPU. Starts no GPU or Colab work."
  (let ((document (and (or package editor)
                       (nlk:json-object :opt "package" package :opt "editor" editor))))
    (render (format nil "comfyrizz_assemble: the CPU timeline for ~a" project-id)
            (request :post "/api/compile"
                     :body (nlk:json-object :opt "doc" document
                                             "packageId" project-id
                                             :opt "revision" revision)))))

(define-verb export-bundle (project-id &key variants)
  "The portable project bundle, and the path the engine wrote."
  (render (format nil "comfyrizz_export: the bundle for ~a, the path read from the engine" project-id)
          (request :post "/api/export"
                   :body (nlk:json-object "packageId" project-id :opt "variants" variants))))

(define-verb retake (project-id clip-id &key change backend-id budget-units production-revision)
  "One clip's retake. The request carries no spend approval of its own."
  (let ((id (command-id)))
    (render (format nil "comfyrizz_retake: ~a clip ~a requested; a paid retake still needs a fresh spend card and the user's yes"
                    project-id clip-id)
            (request :post (api-path "/api/package/~a/clips/~a/regenerate" project-id clip-id)
                     :body (nlk:json-object
                            "commandId" id "idempotencyKey" id
                            "change" change
                            :opt "backendId" backend-id
                            :opt "budgetUnits" budget-units
                            :when production-revision "expected"
                            (nlk:json-object "productionRevision" production-revision))))))

;;; --- what one tool call answers ---------------------------------------------

(defmacro answering (&body body)
  "BODY's text; a stub is that text, and a refusal becomes the ERROR line the
model repairs from, never an unhandled error."
  `(handler-case (progn ,@body)
     (comfyrizz-stub (condition)
       (format nil "stub: true - ~a" (comfyrizz-error-detail condition)))
     (comfyrizz-error (condition)
       (nle:failure (format nil "ERROR: ~a" (comfyrizz-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: Comfyrizz: ~a" (nlk:one-line (princ-to-string condition)))))))

;;; --- the twelve boundaries --------------------------------------------------

(defparameter +status-schema+
  (list :description
        "Report whether the local Comfyrizz engine answers, and its health. Start here before promising any footage: a stub answer is said plainly, never filled in."
        :parameters (nlk:make-json-object
                     "type" "object"
                     "properties" (nlk:make-json-object)
                     "additionalProperties" nil))
  "comfyrizz_status takes no arguments: the original's empty input schema, written by hand
because a contract with no members types nothing.")

(nlk:define-contract comfyrizz-create-project
  (:doc "Create a production on the local Comfyrizz engine and draft a plan for the brief. Never spends.")
  ("brief" :text :required t
           :doc "the film the user wants, in words: title, audience, message, duration, style, budget ceiling")
  ("title" :text :doc "a short title for the production"))

(nlk:define-contract comfyrizz-list-projects
  (:doc "List the productions the local Comfyrizz engine knows about, so an existing one is reused instead of duplicated.")
  ("limit" :integer :doc "how many to ask for, 1 to 200"))

(nlk:define-contract comfyrizz-plan
  (:doc "Draft a plan: a free proposal for a brief, or a project's easy plan at a level and a compute. Planning dispatches no paid work.")
  ("projectId" :text :slot project-id :doc "the project to plan; with level and compute an easy plan, else the project as the engine has it")
  ("brief" :text :doc "a brief to propose a plan for, when no project is named yet")
  ("level" :text :options '("draft" "balanced" "director") :doc "how far the plan goes")
  ("compute" :text :options '("cpu" "colab") :doc "where the engine runs the work")
  ("onlyShotIds" :array :slot only-shot-ids :of :string :doc "plan only these shot ids")
  ("revision" :integer :doc "the plan revision the caller last saw"))

(nlk:define-contract comfyrizz-get-plan
  (:doc "Read the latest proposed plan for a project from its production journal, with its plan hash. The plan to show in chat.")
  ("projectId" :text :slot project-id :doc "the project whose journal to read")
  ("planId" :text :slot plan-id :doc "a plan id, read when no project is named"))

(nlk:define-contract comfyrizz-get-spend-card
  (:doc "Read a project's spend cap and budget ledger, with the approvalId an acceptance needs. Show it before any paid work; never estimate a cost yourself.")
  ("projectId" :text :slot project-id :required t :doc "the project whose spend card to read"))

(nlk:define-contract comfyrizz-accept-generation
  (:doc "Accept and dispatch one approved plan action. Without an explicit approvalId from the spend card it refuses and nothing is spent.")
  ("projectId" :text :slot project-id :required t :doc "the project the action belongs to")
  ("actionId" :text :slot action-id :required t :doc "the plan action to dispatch, exactly as the plan names it")
  ("planId" :text :slot plan-id :doc "the accepted plan's id")
  ("planRevision" :integer :slot plan-revision :doc "the accepted plan's revision")
  ("productionRevision" :integer :slot production-revision :doc "the production revision the caller saw; unset reads as 0")
  ("approvalId" :text :slot approval-id :doc "the approval id the spend card carried")
  ("approvalToken" :text :slot approval-token :doc "the same approval id under the engine's other name for it"))

(nlk:define-contract comfyrizz-job-status
  (:doc "Read one Comfyrizz run or job record, and narrate its transitions: queued, running, completed, failed.")
  ("jobId" :text :slot job-id :required t :doc "the run or job id the engine answered with"))

(nlk:define-contract comfyrizz-list-takes
  (:doc "List a project's clips and takes, or one clip's takes, so existing takes are compared before a retake.")
  ("projectId" :text :slot project-id :required t :doc "the project whose takes to list")
  ("clipId" :text :slot clip-id :doc "one clip, when only its takes matter"))

(nlk:define-contract comfyrizz-assemble
  (:doc "Compile the project timeline on the local CPU, after the paid shots exist. Starts no GPU or Colab work.")
  ("projectId" :text :slot project-id :required t :doc "the project to compile")
  ("package" :object :slot package-document :doc "the package document, when the call carries one")
  ("editor" :object :doc "the editor document, beside the package")
  ("revision" :integer :doc "the package revision the caller holds"))

(nlk:define-contract comfyrizz-export
  (:doc "Export the portable project bundle through the local engine, and answer the path it wrote. Quote that path exactly.")
  ("projectId" :text :slot project-id :required t :doc "the project to export")
  ("variants" :object :doc "the variants to export, when not the engine's default"))

(nlk:define-contract comfyrizz-retake
  (:doc "Request one clip retake with the change it needs. The request carries no spend approval: a paid retake needs a fresh spend card and the user's explicit yes.")
  ("projectId" :text :slot project-id :required t :doc "the project the clip belongs to")
  ("clipId" :text :slot clip-id :required t :doc "the clip to replace")
  ("change" :object :required t :doc "what to change about the clip")
  ("backendId" :text :slot backend-id :doc "the backend to run the retake on, when the user named one")
  ("budgetUnits" :number :slot budget-units :doc "the most the retake may spend, in the engine's own units")
  ("productionRevision" :integer :slot production-revision :doc "the production revision the caller saw"))

(defun status-tool (call)
  "comfyrizz_status: the engine's health, or the stub reason."
  (declare (ignore call))
  (answering (status)))

(defun create-project-tool (call)
  "comfyrizz_create_project."
  (answering (create-project (comfyrizz-create-project-brief call)
                             :title (comfyrizz-create-project-title call))))

(defun list-projects-tool (call)
  "comfyrizz_list_projects."
  (answering (list-projects :limit (comfyrizz-list-projects-limit call))))

(defun plan-tool (call)
  "comfyrizz_plan."
  (answering (plan-project :project-id (comfyrizz-plan-project-id call)
                            :brief (comfyrizz-plan-brief call)
                            :level (comfyrizz-plan-level call)
                            :compute (comfyrizz-plan-compute call)
                            :only-shot-ids (comfyrizz-plan-only-shot-ids call))))

(defun get-plan-tool (call)
  "comfyrizz_get_plan."
  (answering (get-plan :project-id (comfyrizz-get-plan-project-id call)
                       :plan-id (comfyrizz-get-plan-plan-id call))))

(defun spend-card-tool (call)
  "comfyrizz_get_spend_card."
  (answering (spend-card (comfyrizz-get-spend-card-project-id call))))

(defun accept-generation-tool (call)
  "comfyrizz_accept_generation."
  (answering (accept-generation (comfyrizz-accept-generation-project-id call)
                                (comfyrizz-accept-generation-action-id call)
                                :plan-id (comfyrizz-accept-generation-plan-id call)
                                :plan-revision (comfyrizz-accept-generation-plan-revision call)
                                :production-revision (comfyrizz-accept-generation-production-revision call)
                                :approval-id (comfyrizz-accept-generation-approval-id call)
                                :approval-token (comfyrizz-accept-generation-approval-token call))))

(defun job-status-tool (call)
  "comfyrizz_job_status."
  (answering (job (comfyrizz-job-status-job-id call))))

(defun list-takes-tool (call)
  "comfyrizz_list_takes."
  (answering (takes (comfyrizz-list-takes-project-id call)
                    :clip-id (comfyrizz-list-takes-clip-id call))))

(defun assemble-tool (call)
  "comfyrizz_assemble."
  (answering (assemble (comfyrizz-assemble-project-id call)
                       :package (comfyrizz-assemble-package-document call)
                       :editor (comfyrizz-assemble-editor call)
                       :revision (comfyrizz-assemble-revision call))))

(defun export-tool (call)
  "comfyrizz_export."
  (answering (export-bundle (comfyrizz-export-project-id call)
                     :variants (comfyrizz-export-variants call))))

(defun retake-tool (call)
  "comfyrizz_retake."
  (answering (retake (comfyrizz-retake-project-id call)
                     (comfyrizz-retake-clip-id call)
                     :change (comfyrizz-retake-change call)
                     :backend-id (comfyrizz-retake-backend-id call)
                     :budget-units (comfyrizz-retake-budget-units call)
                     :production-revision (comfyrizz-retake-production-revision call))))

;;; --- /comfyrizz --------------------------------------------------------------

(defun run-command (args session-id)
  "/comfyrizz: with no argument, whether the engine answers; with a brief, a
free plan proposal for it. The operator's own way in, beside the tools."
  (declare (ignore session-id))
  (let ((brief (nlk:trimmed (or args ""))))
    (handler-case
        (if (plusp (length brief))
            (plan-project :brief brief)
            (status))
      (comfyrizz-stub (condition)
        (format nil "Comfyrizz: ~a" (comfyrizz-error-detail condition)))
      (comfyrizz-error (condition)
        (nle:failure (format nil "ERROR: ~a" (comfyrizz-error-detail condition)))
        ))))

;;; --- the declaration ---------------------------------------------------------

(nle:define-cell comfyrizz
  (:section ("comfyrizz")
    (:guide "start the Comfyrizz engine on this machine, then set engine_url to its loopback URL (the default is its port 4319); a token_env names the variable holding the token when the engine asks for one; next step: ask comfyrizz_status")
    ("engine_url" :string :default "http://127.0.0.1:4319"
     :doc "the loopback URL of the running Comfyrizz engine: 127.0.0.1, localhost or ::1")
    ("token_env" :env
     :doc "the environment variable holding the engine's token, when it needs one")
    ("timeout_seconds" :integer :default 120 :min 1
     :doc "one engine request's ceiling, in seconds; a brief proposal may take a minute")
    ("answer_chars" :integer :default 6000 :min 200
     :doc "how much of an engine answer one tool result carries"))
  (:settings #'read-settings)
  (:help :comfyrizz
         "comfyrizz: run the Comfyrizz film pipeline in chat - brief, plan, spend card, accept, assemble, export, retake"
         +primer+)
  (:tool "comfyrizz_status" +status-schema+ #'status-tool)
  (:tool "comfyrizz_create_project" (nlk:contract 'comfyrizz-create-project) #'create-project-tool)
  (:tool "comfyrizz_list_projects" (nlk:contract 'comfyrizz-list-projects) #'list-projects-tool)
  (:tool "comfyrizz_plan" (nlk:contract 'comfyrizz-plan) #'plan-tool)
  (:tool "comfyrizz_get_plan" (nlk:contract 'comfyrizz-get-plan) #'get-plan-tool)
  (:tool "comfyrizz_get_spend_card" (nlk:contract 'comfyrizz-get-spend-card) #'spend-card-tool)
  (:tool "comfyrizz_accept_generation" (nlk:contract 'comfyrizz-accept-generation) #'accept-generation-tool)
  (:tool "comfyrizz_job_status" (nlk:contract 'comfyrizz-job-status) #'job-status-tool)
  (:tool "comfyrizz_list_takes" (nlk:contract 'comfyrizz-list-takes) #'list-takes-tool)
  (:tool "comfyrizz_assemble" (nlk:contract 'comfyrizz-assemble) #'assemble-tool)
  (:tool "comfyrizz_export" (nlk:contract 'comfyrizz-export) #'export-tool)
  (:tool "comfyrizz_retake" (nlk:contract 'comfyrizz-retake) #'retake-tool)
  (:command "comfyrizz" 'run-command
            :description "Comfyrizz: the engine's health, or a free plan for a brief"
            :argument-hint "BRIEF"))
