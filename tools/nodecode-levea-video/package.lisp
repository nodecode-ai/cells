;;;; package.lisp --- the cell's own package, and the vocabulary every file shares.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, as a peripheral's always is: the
;;;; loader finds START-CELL there (NLE:DEFINE-CELL exports it), and every
;;;; contribution is filed under this package's name downcased,
;;;; "nodecode-levea-video", which is this folder's name.

(defpackage #:nodecode-levea-video
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:*levea-video*
           #:levea-video-error #:levea-video-error-detail))

(in-package #:nodecode-levea-video)

(nlk:define-peripheral levea-video :not-running t)

;;; --- the service, as data --------------------------------------------------------

(defparameter +default-api-url+ "https://api.livecore.ai"
  "The host every call goes to unless the section names another one.")

(defparameter +api-suffix+ "/api/v1/misc/openclaw/v1"
  "The path the editor's API answers under, appended to the configured host.")

(defparameter +key-portal+ "https://livecore.ai/"
  "Where a key is generated, for the refusal that says one is missing.")

(defparameter +client-header+ "nodecode-levea-video/0.1.0"
  "What X-Levea-Client says about the caller: this cell, not another client.")

(defparameter +pending-states+
  '("pending" "waiting" "queued" "processing" "rendering" "planning"
    "executing" "dispatch" "awaiting_approval" "awaiting_input")
  "The states a running edit reports: a call that answers one is not finished.")

(defparameter +failed-states+
  '("failed" "failure" "error" "errored"
    "cancelled" "canceled" "timeout" "timed_out" "not_found")
  "The states that end an edit badly, whatever its success flag says.")

(defparameter *getenv* #'uiop:getenv
  "How an environment variable is read. A test binds this; nothing else does.")

(defparameter +primer+
  "Levea agentic video editing is available through the nodecode-levea-video cell: five
tools, and /levea for the setup this profile runs on.
  levea_video_edit(prompt, video_url | assets, project_id?, ...)
      ONE natural-language edit per call: trim, 9:16 reframe, captions, B-roll, multicam
      sync, audio cleanup, colour, motion graphics, MP4 export. Send the whole intent as
      one prompt instead of many small calls. Keep the projectId it returns and pass it
      back as project_id on every follow-up, so edits stack on the same timeline.
  levea_queue_edit(prompt, project_id, ...) queues a long edit and answers a taskId.
  levea_task_status(task_id) reads that queued edit.
  levea_active_task(project_id) reconnects to the edit still running on a project.
  levea_job_status(job_id, wait?) reads an export or render job; wait: true polls every
      poll_seconds until it finishes, up to timeout_seconds (10 minutes by default).
Read success and status, never the fact that the call returned:
  success true        the backend verified the edit; report videoUrl when it is present.
  awaiting_approval   show the plan, then resend with workingMemory (and editedPlan).
  pending + jobId     levea_job_status with wait: true for the rendered file.
  partial or error    say what message says, and never claim success.
Every edit and export is billed to the operator's Levea account, and a long edit holds the
turn for minutes: say what one costs and that it will take a while before starting it.
What leaves the machine is the prompt, the source URLs and the project ids, sent to the
Levea host with the operator's key."
  "What (help :levea) answers while the cell runs.")
