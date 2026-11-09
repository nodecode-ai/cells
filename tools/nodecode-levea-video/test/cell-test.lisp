;;;; cell-test.lisp --- every tool, the refusals and the lifecycle, with the
;;;; editor's API stubbed.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here dials: NLK:HTTP is a stub that records the call and answers a
;;;; handcrafted JSON body, and the environment is a stub. What is pinned is
;;;; what a user gets -- one prompt becomes one verified edit whose file and
;;;; project id are in the answer, a queued edit needs its project, a wait
;;;; follows a job to its end, each status tool reads its own path -- and what
;;;; a call never costs: no key, an unverified success, a refused status.

(in-package #:nodecode.test)

(define-test-slice "levea-video" "LEVEA-VIDEO-" :start nodecode-levea-video:start-cell)

;;; --- the cell's own lifecycle ---------------------------------------------------

(define-cell-lifecycle-tests "levea-video"
  (:config "api_key" "test-key")
  (:help :levea)
  (:command "levea")
  (:running (is (equal "test-key" (nodecode-levea-video::setting :api-key)) "the section's own key")
            (is (equal 600 (nodecode-levea-video::setting :timeout-seconds)) "ten minutes by default")
            (let ((entry (nle::find-tool "levea_video_edit")))
              (is-present entry "the edit tool is registered under the plugin's name"
                (is (equal "levea_video_edit" (first entry)))))
            (is-present (entry (nle::find-tool "levea_queue_edit")) "the queue tool too")
            (is-present (entry (nle::find-tool "levea_job_status")) "the job tool too")
            (is-present (entry (nle::find-tool "levea_task_status")) "the task tool too")
            (is-present (entry (nle::find-tool "levea_active_task")) "the reconnect tool too"))
  (:stopped (is (null (nle::find-tool "levea_video_edit")) "stop takes the tools back out"))
  (:refused ("api_url" "ftp://api.example.test")
            ("api_url" "http://api.example.test"))
  (:idle nodecode-levea-video:levea-video-error (nodecode-levea-video::setup-text)))

;;; --- fixtures -------------------------------------------------------------------

(defmacro with-levea ((&rest pairs) &body body)
  "BODY with the cell started on PAIRS, and stopped however it leaves."
  `(let ((stop (levea-video-start ,@pairs)))
     (nlk:with-cleanup ((when stop (funcall stop))) ,@body)))

(defun levea-tool (name &rest pairs)
  "The tool NAME as the engine calls it: the declared contract decoded, then the body."
  (let ((entry (nle::find-tool name)))
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defun levea-json (object)
  "The three values a stubbed NLK:HTTP answers: OBJECT as JSON, status 200, no headers."
  (values (nlk:encode-json-object object) 200 nil))

(defmacro with-levea-endpoint ((asked &key (answer '(lambda (url headers content)
                                                       (declare (ignore url headers content))
                                                       (levea-json (nlk:json-object)))))
                               &body body)
  "BODY with the editor's endpoint stubbed: ASKED collects (METHOD URL HEADERS BODY)
oldest first, and ANSWER answers (values BODY STATUS HEADERS)."
  `(let ((,asked '()))
     (with-stubbed-fdefinition (nlk:http (method url &key headers content &allow-other-keys)
                                (push (list method url headers content) ,asked)
                                (funcall ,answer url headers content))
       ,@body)))

(defun levea-call (asked &optional (index 0))
  "The INDEXth call ASKED recorded, as (values METHOD URL HEADERS BODY-AS-JSON)."
  (let ((call (nth index (reverse asked))))
    (values (first call) (second call) (third call)
            (and (fourth call) (nlk:decode-json (fourth call))))))

(defun levea-sequence (objects)
  "An endpoint answering OBJECTS in turn, the last one repeating."
  (lambda (url headers content)
    (declare (ignore url headers content))
    (levea-json (if (rest objects) (pop objects) (first objects)))))

;;; --- one edit -------------------------------------------------------------------

(deftest levea-video-edit-posts-one-verified-edit ()
  (with-levea ("api_key" "test-key" "api_url" "https://api.example.test")
    (let ((asked '()))
      (with-levea-endpoint (asked :answer (levea-sequence
                                            (list (nlk:json-object
                                                   "success" t
                                                   "verificationPassed" t
                                                   "status" "completed"
                                                   "message" "6 clips cut, verified"
                                                   "projectId" "prj_1"
                                                   "videoUrl" "https://cdn.example.test/out.mp4"))))
        (let ((answer (levea-tool "levea_video_edit"
                                   "prompt" "cut the first 10 seconds and export mp4"
                                   "video_url" "https://example.test/in.mp4")))
          (multiple-value-bind (method url headers body) (levea-call asked)
            (is (eq :post method) "one POST")
            (is (equal "https://api.example.test/api/v1/misc/openclaw/v1/execute" url)
                "to the editor's execute endpoint")
            (is (equal "Bearer test-key" (cdr (assoc "authorization" headers :test #'string-equal)))
                "with the operator's key")
            (is (equal "autonomous_edit" (nlk:json-value body :string "tool")) "the one edit tool")
            (is (equal "cut the first 10 seconds and export mp4"
                       (nlk:json-value body :string "params" "prompt"))
                "the prompt rides inside params")
            (is (equal "https://example.test/in.mp4" (nlk:json-value body :string "params" "video_url"))
                "and so does the source"))
          (is (search "https://cdn.example.test/out.mp4" answer) "the exported file is in the answer")
          (is (search "prj_1" answer) "with the project id to continue from"))))))

(deftest levea-video-edit-success-without-verification-is-a-failure ()
  (with-levea ("api_key" "test-key" "api_url" "https://api.example.test")
    (let ((asked '()))
      (with-levea-endpoint (asked :answer (levea-sequence
                                            (list (nlk:json-object "success" t "message" "done"
                                                                   "projectId" "prj_2"))))
        (let ((answer (levea-tool "levea_video_edit" "prompt" "trim it")))
          (is (search "without authoritative verification" answer)
              "a success the backend did not verify is said so, not claimed")
          (is (= 1 (length asked)) "one call"))))))

;;; --- the queue ------------------------------------------------------------------

(deftest levea-video-queue-edit-needs-a-project ()
  (with-levea ("api_key" "test-key" "api_url" "https://api.example.test")
    (is-present (condition (signals-error nlk:contract-refusal
                             (levea-tool "levea_queue_edit" "prompt" "a long edit")))
      "a queued edit without a project is refused before any call"
      (is (search "project_id" (format nil "~a" condition)) "and the refusal names it"))
    (let ((asked '()))
      (with-levea-endpoint (asked :answer (levea-sequence
                                            (list (nlk:json-object "success" t "message" "queued"
                                                                   "projectId" "prj_3"
                                                                   "taskId" "task_1"))))
        (let ((answer (levea-tool "levea_queue_edit" "prompt" "a long edit"
                                   "project_id" "prj_3")))
          (multiple-value-bind (method url headers body) (levea-call asked)
            (declare (ignore headers))
            (is (eq :post method) "one POST")
            (is (equal "https://api.example.test/api/v1/misc/openclaw/v1/queue-edit" url)
                "to the queue endpoint")
            (is (equal "prj_3" (nlk:json-value body :string "projectId")) "for the named project"))
          (is (search "task_1" answer) "the task id is in the answer"))))))

;;; --- status, wait, and the active edit -------------------------------------------

(deftest levea-video-job-status-waits-until-the-job-ends ()
  (with-levea ("api_key" "test-key" "api_url" "https://api.example.test" "poll_seconds" 0)
    (let* ((asked '())
           (sequence (levea-sequence
                      (list (nlk:json-object "success" t "status" "rendering"
                                             "jobId" "job_1"
                                             "projectId" "prj_4")
                            (nlk:json-object "success" t "status" "completed"
                                             "jobId" "job_1"
                                             "videoUrl" "https://cdn.example.test/render.mp4")))))
      (with-levea-endpoint (asked :answer sequence)
        (let ((answer (levea-tool "levea_job_status" "job_id" "job_1" "wait" t)))
          (is (= 2 (length asked)) "a wait reads the job again until it ends")
          (multiple-value-bind (method url) (levea-call asked 1)
            (is (eq :get method) "a read, not a write")
            (is (equal "https://api.example.test/api/v1/misc/openclaw/v1/jobs/job_1" url)
                "by the job's own path"))
          (is (search "https://cdn.example.test/render.mp4" answer) "the rendered file is in the answer")
          (is (not (search "rendering" answer)) "and the pending state is not the answer"))))))

(deftest levea-video-status-tools-read-their-own-paths ()
  (with-levea ("api_key" "test-key" "api_url" "https://api.example.test")
    (let* ((asked '())
           (sequence (levea-sequence
                      (list (nlk:json-object "success" t "status" "completed"
                                             "message" "rendered")
                            (nlk:json-object "active" t "taskId" "task_9"
                                             "status" "rendering")))))
      (with-levea-endpoint (asked :answer sequence)
        (levea-tool "levea_task_status" "task_id" "task_9")
        (let ((answer (levea-tool "levea_active_task" "project_id" "prj/9")))
          (multiple-value-bind (method url) (levea-call asked 0)
            (declare (ignore method))
            (is (equal "https://api.example.test/api/v1/misc/openclaw/v1/task-status/task_9" url)
                "a task is read by its id"))
          (multiple-value-bind (method url) (levea-call asked 1)
            (declare (ignore method))
            (is (equal "https://api.example.test/api/v1/misc/openclaw/v1/projects/prj%2F9/active-task" url)
                "a project id is one path segment, slash and all"))
          (is (search "task_9" answer) "the running edit is in the answer")
          (is (search "rendering" answer) "with the state it is in"))))))

;;; --- refusals -------------------------------------------------------------------

(deftest levea-video-refuses-without-a-key ()
  (let ((nodecode-levea-video::*getenv*
          (lambda (name) (declare (ignore name)) nil)))
    (with-levea ("api_url" "https://api.example.test")
      (let ((asked '()))
        (with-levea-endpoint (asked)
          (let ((answer (levea-tool "levea_video_edit" "prompt" "trim it")))
            (is (search "no Levea key" answer) "the answer says which setting to fill")
            (is (search "api_key" answer) "naming it")
            (is (null asked) "and nothing was sent")))))))

(deftest levea-video-a-refused-status-is-an-error ()
  (with-levea ("api_key" "test-key" "api_url" "https://api.example.test")
    (let ((asked '()))
      (with-levea-endpoint (asked :answer (lambda (url headers content)
                                             (declare (ignore url headers content))
                                             (values (nlk:encode-json-object
                                                      (nlk:json-object "message" "bad key"))
                                                     401 nil)))
        (let ((answer (levea-tool "levea_video_edit" "prompt" "trim it")))
          (is (search "HTTP 401" answer) "the status is named")
          (is (search "bad key" answer) "and what the backend said of it")
          (is (= 1 (length asked)) "one call, no retry"))))))

(deftest levea-video-slash-shows-the-setup ()
  (with-levea ("api_key" "test-key" "api_url" "https://api.example.test"
               "project_id" "prj_default")
    (let ((text (nodecode-levea-video::run-slash "" "s1")))
      (is (search "https://api.example.test/api/v1/misc/openclaw/v1" text) "the host every call goes to")
      (is (search "prj_default" text) "the project a call continues")
      (is (search "levea_video_edit" text) "and the tools it offers"))))
