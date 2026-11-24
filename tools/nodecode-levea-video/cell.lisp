;;;; cell.lisp --- the five tools, /levea, and the section they run on.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; POST {root}/execute (an edit) and /queue-edit (a queued one), and GET
;;;; {root}/jobs/{id}, /task-status/{id}, /projects/{id}/active-task. The
;;;; editor's request fields are declared once each, in the two contracts
;;;; below, and the body is built off the declaration -- so the schema the
;;;; model reads and the body the backend receives cannot drift apart.

(in-package #:nodecode-levea-video)

;;; --- the edit request, as the editor reads it -------------------------------------

(defparameter +caption-modes+ '("add" "recreate" "style" "translate" "clear" "remove_fillers")
  "What captionTemplateMode may ask for.")

(nlk:define-contract levea-edit
  (:doc "One natural-language video edit with the Levea autonomous editor: it plans,
    executes, verifies and exports. Send the whole intent in one prompt (trim, 9:16
    reframe, captions, B-roll, multicam sync, audio cleanup, colour, motion graphics,
    MP4 export) and pass the projectId it returns back as project_id for follow-ups.
    Billed to the operator's Levea account, and a call can take minutes.")
  ("prompt" :text :required t
             :doc "the desired edit or creative brief, in natural language: what to cut, keep, add and deliver")
  ("project_id" :text
                 :doc "the project to continue; reuse the projectId of the first call so edits stack on one timeline")
  ("video_url" :text
               :doc "a public HTTP(S) source video: S3, a public Google Drive or Dropbox link")
  ("assets" :array :of :object
            :doc "input media assets as public HTTP(S) links, for an edit that needs more than one source")
  ("scene" :object
           :doc "an existing scene/timeline to continue editing (advanced)")
  ("captionTemplatePreset" :text :slot caption-template-preset
                            :doc "the caption template when captions are (re)generated, such as hormozi, viral-pop, karaoke, subtitle-bar")
  ("captionTemplateMode" :text :slot caption-template-mode :options +caption-modes+
                          :doc "what to do with captions: add, recreate, style, translate, clear, or remove_fillers")
  ("requirePlanApproval" :boolean :slot require-plan-approval
                          :doc "true to stop after planning and return workingMemory for approval")
  ("workingMemory" :object :slot working-memory
                    :doc "the workingMemory an awaiting-approval call returned; resend it to resume")
  ("editedPlan" :object :slot edited-plan
                :doc "a plan object returned by an awaiting_approval response, edited before resuming")
  ("attachedImages" :array :of :string :slot attached-images
                    :doc "reference image URLs used for planning")
  ("flaggedIssues" :array :of :string :slot flagged-issues
                   :doc "specific problems the user flagged, for this pass to fix")
  ("brandId" :text :slot brand-id
             :doc "a brand profile id for this call only")
  ("projectBrandId" :text :slot project-brand-id
                    :doc "the project's default brand profile; it persists, and a per-call brandId overrides it")
  ("core_only" :boolean
                :doc "return rendering-essential scene fields only, for a smaller answer"))

(nlk:define-contract levea-queued-edit
  (:doc "A long Levea edit queued against a project: it answers a taskId, which
    levea_task_status reads. Requires project_id, so a queued edit can never attach
    to a shared default project.")
  ("prompt" :text :required t :doc "the desired edit, in natural language")
  ("project_id" :text :required t
                 :doc "the stable project id this edit belongs to")
  ("video_url" :text :doc "a public HTTP(S) source video")
  ("assets" :array :of :object :doc "input media assets as public HTTP(S) links")
  ("scene" :object :doc "an existing scene/timeline to continue editing (advanced)")
  ("captionTemplatePreset" :text :slot caption-template-preset
                            :doc "the caption template when captions are (re)generated")
  ("captionTemplateMode" :text :slot caption-template-mode :options +caption-modes+
                          :doc "what to do with captions: add, recreate, style, translate, clear, remove_fillers")
  ("platforms" :array :of :string
               :doc "export platforms for the queued edit")
  ("autoExport" :boolean :slot auto-export
                 :doc "export automatically once the queued edit finishes"))

(nlk:define-contract levea-job-query
  (:doc "Read an export or render job by the id an edit answered.")
  ("job_id" :text :required t :doc "the jobId an edit or export returned")
  ("wait" :boolean :doc "true to poll until the job ends, up to the section's timeout_seconds"))

(nlk:define-contract levea-task-query
  (:doc "Read a queued edit by the taskId levea_queue_edit answered.")
  ("task_id" :text :required t :doc "the taskId levea_queue_edit returned"))

(nlk:define-contract levea-active-query
  (:doc "Reconnect to the edit still running on a project, after a dropped connection.")
  ("project_id" :text :required t :doc "the project whose running edit to read"))

;;; --- the body, built off the declaration ------------------------------------------

(defparameter +param-keys+ '("prompt" "video_url" "assets")
  "The request members the editor's executor reads inside params.")

(defparameter +project-keys+ '("project_id")
  "The request member that names the project.")

(defun edit-body (encoded tool)
  "ENCODED, a request's encoded members, as the body the editor takes: params for
what the executor reads, projectId and project_id for the project, the rest at
the top level. A member the caller did not set is left out, so a false flag
never rides where an absent one means the same thing."
  (let ((params (nlk:json-object))
        (body (nlk:json-object "tool" tool)))
    (maphash (lambda (key value)
               (unless (null value)
                 (cond ((member key +param-keys+ :test #'string=)
                        (setf (gethash key params) value))
                       ((member key +project-keys+ :test #'string=)
                        (setf (gethash "projectId" body) value
                              (gethash "project_id" body) value))
                       (t (setf (gethash key body) value)))))
             encoded)
    (setf (gethash "params" body) params)
    body))

(defun default-project (body)
  "BODY with this profile's default project as the project when the call named
none: how a follow-up edit continues the same timeline."
  (let ((default (nlk:trimmed (or (setting :project-id) ""))))
    (when (and (plusp (length default)) (null (gethash "projectId" body)))
      (setf (gethash "projectId" body) default
            (gethash "project_id" body) default))
    body))

;;; --- the five tools -------------------------------------------------------------

(defun answered (label thunk)
  "THUNK's answer, or the refusal it raised as the failure the model reads: a
missing key and a bad host are answered, never left as a signal to unwind."
  (handler-case (funcall thunk)
    (levea-video-error (condition)
      (nle:failure (format nil "~a: ~a" label (levea-video-error-detail condition))))
    (error (condition)
      (nle:failure (format nil "~a: ~a" label condition)))))

(defun video-edit (request)
  "levea_video_edit: one edit, verified by the backend before it counts."
  (answered "levea_video_edit"
    (lambda ()
      (let ((body (default-project (edit-body (encode-levea-edit request) "autonomous_edit"))))
        (answer (call :post "/execute" :body body :label "autonomous_edit"
                      :require-message t :require-verification t
                      :fallback-project (nlk:json-value body :string "projectId"))
                :label "levea_video_edit")))))

(defun queue-edit (request)
  "levea_queue_edit: a long edit queued against a project it names."
  (answered "levea_queue_edit"
    (lambda ()
      (let* ((encoded (encode-levea-queued-edit request))
             (project (nlk:json-value encoded :string "project_id"))
             (body (edit-body encoded "autonomous_edit")))
        (answer (call :post "/queue-edit" :body body :label "queue_edit"
                      :require-message t :fallback-project project)
                :label "levea_queue_edit")))))

(defun job-status (request)
  "levea_job_status: an export or render job, followed to the end with wait."
  (answered "levea_job_status"
    (lambda ()
      (let* ((job (levea-job-query-job-id request))
             (result (poll (format nil "/jobs/~a" (url-segment job))
                           (format nil "job ~a" job)
                           :wait (levea-job-query-wait request))))
        (answer result :label "levea_job_status")))))

(defun task-status (request)
  "levea_task_status: a queued edit, by its taskId."
  (answered "levea_task_status"
    (lambda ()
      (let ((task (levea-task-query-task-id request)))
        (answer (poll (format nil "/task-status/~a" (url-segment task))
                      (format nil "task ~a" task))
                :label "levea_task_status")))))

(defun active-task (request)
  "levea_active_task: the edit still running on a project, after a dropped call."
  (answered "levea_active_task"
    (lambda ()
      (let ((project (levea-active-query-project-id request)))
        (answer (poll (format nil "/projects/~a/active-task" (url-segment project))
                      (format nil "project ~a" project)
                      :fallback-project project :accept-active t)
                :label "levea_active_task")))))

;;; --- /levea ---------------------------------------------------------------------

(defun key-set-p ()
  "Whether a key is there to call with, from either place the section names."
  (let ((key (nlk:trimmed (or (setting :api-key) ""))))
    (or (plusp (length key))
        (plusp (length (nlk:trimmed (or (funcall *getenv* (setting :api-key-env)) "")))))))

(defun setup-text ()
  "What this profile runs on: host, key, default project, how long a wait lasts."
  (with-output-to-string (out)
    (format out "levea: ~a~%" (api-root))
    (format out "key: ~a~%"
            (if (key-set-p)
                "set"
                (format nil "not set -- paste api_key into the levea-video section, or set ~a"
                        (setting :api-key-env))))
    (format out "default project: ~a~%"
            (let ((project (nlk:trimmed (or (setting :project-id) ""))))
              (if (plusp (length project))
                  project
                  "none -- pass project_id, or take the projectId an edit returns")))
    (format out "one call: ~a s; a wait polls every ~a s for at most ~a s~%"
            (setting :timeout-seconds) (setting :poll-seconds) (setting :timeout-seconds))
    (format out "tools: levea_video_edit, levea_queue_edit, levea_job_status, ~
                 levea_task_status, levea_active_task~%")
    (format out "/levea <project id> reconnects to the edit running on that project.~%")))

(defun run-slash (args session-id)
  "The setup this profile runs on, or with a project id the edit running there."
  (declare (ignore session-id))
  (let ((argument (nlk:trimmed (or args ""))))
    (if (zerop (length argument))
        (setup-text)
        (answer (poll (format nil "/projects/~a/active-task" (url-segment argument))
                      (format nil "project ~a" argument)
                      :fallback-project argument :accept-active t)
                :label "levea"))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell levea-video
  (:section ("levea-video")
    (:guide "generate a key at https://livecore.ai and paste it in api_key, or name the variable that holds it in api_key_env; every edit and export is billed to that Levea account, and a long edit holds the turn for minutes")
    ("api_key" :secret
               :doc "the Levea API key, written here; left empty, the variable api_key_env names is read")
    ("api_key_env" :env :default "LEVEA_API_KEY"
                   :doc "the environment variable holding the key, read when api_key is empty")
    ("api_url" :string :default "https://api.livecore.ai"
               :doc "the editor's API host; the product's web host and the full API path are both accepted")
    ("project_id" :string
                   :doc "the project a call continues when it names none; every result names the project it used")
    ("timeout_seconds" :integer :default 600 :min 1
                       :doc "one call's ceiling, and how long wait: true polls before it answers unfinished")
    ("poll_seconds" :integer :default 4 :min 0
                    :doc "how often a wait: true reads the job again"))
  ;; The one thing the declaration cannot say: a base URL is a web address, and
  ;; plain http is a cleartext key.
  (:settings (lambda (values table)
               (declare (ignore table))
               (setf (getf values :api-url)
                     (checked-api-url (or (getf values :api-url) +default-api-url+)))
               values))
  (:tool "levea_video_edit" (nlk:contract 'levea-edit) 'video-edit)
  (:tool "levea_queue_edit" (nlk:contract 'levea-queued-edit) 'queue-edit)
  (:tool "levea_job_status" (nlk:contract 'levea-job-query) 'job-status)
  (:tool "levea_task_status" (nlk:contract 'levea-task-query) 'task-status)
  (:tool "levea_active_task" (nlk:contract 'levea-active-query) 'active-task)
  (:help :levea
         "levea: Levea's agentic video editor -- one natural-language edit, its queue and job tools"
         +primer+)
  (:command "levea" 'run-slash
            :description "The Levea setup this profile runs on, or the edit running on a project"
            :argument-hint "[project id]"))
