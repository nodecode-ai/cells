;;;; cell-test.lisp --- lifecycle, the eight tools, and /quegly, against a stubbed wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches Quegly, needs a
;;;; key or touches the network. What is proved is the cell's own wiring: the
;;;; section (a healthy start, a wrong value and a missing key refused), the
;;;; tools through the seam the model calls them by -- their method, url,
;;;; headers, body and answer -- and the operator's command.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *quegly-requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *quegly-reply* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-quegly-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged and answered by
*QUEGLY-REPLY*, so no test dials Quegly."
  `(let ((*quegly-requests* '())
         (*quegly-reply* (lambda (method url args)
                            (declare (ignore method url args))
                            (error "unexpected quegly request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *quegly-requests*)
                                (funcall *quegly-reply* method url args))
       ,@body)))

(defun quegly-asked ()
  "The stubbed requests, oldest first."
  (reverse *quegly-requests*))

(defun quegly-reply (text &optional (status 200))
  (setf *quegly-reply* (lambda (method url args)
                          (declare (ignore method url args))
                          (values text status))))

(defun quegly-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun quegly-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content) :whole t))

(defun quegly-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "quegly-call" name arguments)))

(defparameter +quegly-workspaces+
  "{\"workspaces\": [{\"id\": \"ws-1\", \"name\": \"Brand One\", \"slug\": \"brand-one\",
                       \"is_archived\": false},
                      {\"id\": \"ws-2\", \"name\": \"Old Brand\", \"slug\": \"old-brand\",
                       \"is_archived\": true}]}")

(defparameter +quegly-accounts+
  "{\"accounts\": [{\"id\": \"acc-ig\", \"platform\": \"instagram\", \"account_username\": \"brand\",
                     \"is_active\": true},
                    {\"id\": \"acc-tt\", \"platform\": \"tiktok\", \"account_username\": \"brand\",
                     \"is_active\": true}]}")

(defparameter +quegly-post+
  "{\"id\": \"post-1\", \"status\": \"partial\", \"scheduled_at\": \"2026-06-01T14:00:00.000Z\",
    \"platforms\": [{\"platform\": \"instagram\", \"account_username\": \"brand\", \"status\": \"posted\",
                     \"post_url\": \"https://instagram.test/p/1\", \"error\": null},
                    {\"platform\": \"x\", \"account_username\": \"brand\", \"status\": \"failed\",
                     \"post_url\": null, \"error\": \"Couldn't reach X.\",
                     \"error_detail\": \"Request timed out after 30000ms\"}]}")

;;; --- lifecycle -----------------------------------------------------------------

(define-test-slice "quegly" "QUEGLY-CELL-" :start nodecode-quegly:start-cell)

(define-cell-lifecycle-tests "quegly"
  (:fixture with-quegly-wire)
  (:config "api_key" "test-key")
  (:help :quegly)
  (:command "quegly")
  (:running
   (is (nle::find-tool "quegly_workspaces") "the workspaces tool")
   (is (nle::find-tool "quegly_accounts") "and accounts")
   (is (nle::find-tool "quegly_upload") "and upload")
   (is (nle::find-tool "quegly_create_post") "and create")
   (is (nle::find-tool "quegly_posts") "and list")
   (is (nle::find-tool "quegly_post") "and get")
   (is (nle::find-tool "quegly_change_post") "and act")
   (is (nle::find-tool "quegly_analytics") "and analytics")
   (is (= 8 (count-if (lambda (entry) (eql 0 (search "quegly_" (first entry)))) nle::*tools*)) "eight of them, and no others"))
  (:stopped
   (is (null (nle::find-tool "quegly_create_post")) "and stop takes every one back")
   (is (null (nle::find-tool "quegly_analytics"))))
  (:refused ("timeout_seconds" 0)))

(deftest quegly-cell-a-start-with-no-key-refuses-and-installs-nothing (with-quegly-wire)
  (with-stubbed-fdefinition (uiop:getenv (name) nil)
    (let ((text (refusal-text nlk:config-refusal (quegly-start))))
      (is-carrying text "needs a key" "QUEGLY_API_KEY")
      (is (null (nle::find-tool "quegly_create_post")) "nothing was installed"))))

(deftest quegly-cell-the-key-may-come-from-the-environment (with-quegly-wire)
  (with-stubbed-fdefinition (uiop:getenv (name) (and (equal name "QUEGLY_API_KEY") "qgl_env"))
    (with-cell-stop ((quegly-start))
      (quegly-reply +quegly-workspaces+)
      (quegly-call "quegly_workspaces" "{}")
      (is (equal "Bearer qgl_env" (quegly-header (first (quegly-asked)) "authorization")) "the exported key is the one sent"))))

;;; --- workspaces and accounts ----------------------------------------------------

(deftest quegly-cell-workspaces-answers-the-ids-to-use (with-quegly-wire)
  (with-cell-stop ((quegly-start "api_key" "qgl_test"))
    (quegly-reply +quegly-workspaces+)
    (let ((answer (quegly-call "quegly_workspaces" "{}")))
      (destructuring-bind (method url . args) (first (quegly-asked))
        (declare (ignore args))
        (is (eq :get method) "one GET")
        (is (equal "https://api.quegly.com/v1/workspaces" url) "to the workspaces endpoint")
        (is (equal "Bearer qgl_test" (quegly-header (first (quegly-asked)) "authorization")) "with the section's key")
        (is (null (quegly-header (first (quegly-asked)) "x-workspace-id")) "and no workspace header: workspaces is the discovery call"))
      (is (search "ws-1" answer) "the workspace id reaches the model")
      (is (search "Brand One" answer) "with its name")
      (is (null (search "ws-2" answer)) "the archived one is left out unless asked for"))))

(deftest quegly-cell-accounts-carry-the-workspace-of-the-call (with-quegly-wire)
  (with-cell-stop ((quegly-start "api_key" "qgl_test"))
    (quegly-reply +quegly-accounts+)
    (let ((answer (quegly-call "quegly_accounts" "{\"workspace_id\": \"ws-1\", \"platform\": \"TikTok\"}")))
      (is (equal "ws-1" (quegly-header (first (quegly-asked)) "x-workspace-id")) "the call's workspace rides as X-Workspace-Id")
      (is (equal "https://api.quegly.com/v1/accounts" (second (first (quegly-asked)))))
      (is (search "acc-tt" answer) "the tiktok account")
      (is (null (search "acc-ig" answer)) "and only the platform asked for"))))

;;; --- posts ----------------------------------------------------------------------

(deftest quegly-cell-create-post-sends-one-post-for-every-account (with-quegly-wire)
  (with-cell-stop ((quegly-start "api_key" "qgl_test"))
    (quegly-reply +quegly-post+ 201)
    (let ((answer (quegly-call "quegly_create_post"
                                "{\"type\": \"schedule\", \"scheduled_at\": \"2026-06-01T14:00:00.000Z\",
                                  \"caption\": \"Ten salads\", \"account_ids\": [\"acc-ig\", \"acc-tt\"],
                                  \"media_ids\": [\"media-1\"], \"idempotency_key\": \"k-1\",
                                  \"platform_settings\": {\"tiktok\": {\"privacy_level\": \"PUBLIC_TO_EVERYONE\"}}}")))
      (destructuring-bind (method url . args) (first (quegly-asked))
        (declare (ignore args))
        (let ((body (quegly-body (first (quegly-asked)))))
          (is (eq :post method) "one POST")
          (is (equal "https://api.quegly.com/v1/posts" url) "to the posts endpoint")
          (is (equal "k-1" (quegly-header (first (quegly-asked)) "idempotency-key")) "carrying the idempotency key as its header")
          (is (equal "schedule" (nlk:json-value body :string "type")))
          (is (equal "2026-06-01T14:00:00.000Z" (nlk:json-value body :string "scheduled_at")))
          (is (equalp #("acc-ig" "acc-tt") (nlk:json-value body :array "account_ids")) "one call, both accounts")
          (is (equalp #("media-1") (nlk:json-value body :array "media_ids")))
          (is (equal "PUBLIC_TO_EVERYONE" (nlk:json-value body :string "platform_settings" "tiktok" "privacy_level")) "the platform's own settings ride through as sent")))
      (is (search "post-1" answer) "the new post's id reaches the model")
      (is (search "partial" answer) "with its status")
      (is (search "https://instagram.test/p/1" answer) "and the platform that posted")
      (is (search "Couldn't reach X." answer) "and the platform that failed, in its own words"))))

(deftest quegly-cell-create-post-refuses-a-schedule-with-no-time (with-quegly-wire)
  (with-cell-stop ((quegly-start "api_key" "qgl_test"))
    (let ((answer (quegly-call "quegly_create_post" "{\"type\": \"schedule\", \"account_ids\": [\"acc-ig\"]}")))
      (is (search "needs scheduled_at" answer) "the refusal names the missing member")
      (is (null (quegly-asked)) "and nothing was sent"))))

(deftest quegly-cell-posts-reads-a-range-and-one-post (with-quegly-wire)
  (with-cell-stop ((quegly-start "api_key" "qgl_test"))
    (quegly-reply "{\"posts\": [{\"id\": \"post-1\", \"status\": \"scheduled\",
                                  \"scheduled_at\": \"2026-06-01T14:00:00.000Z\", \"platforms\": []}]}")
    (let ((answer (quegly-call "quegly_posts" "{\"start_date\": \"2026-06-01\", \"end_date\": \"2026-06-30\"}")))
      (is (search "start_date=2026-06-01" (second (first (quegly-asked)))) "the range rides in the query")
      (is (search "post-1" answer) "the post is listed"))
    (quegly-reply +quegly-post+)
    (let ((answer (quegly-call "quegly_post" "{\"post_id\": \"post-1\"}")))
      (is (equal "https://api.quegly.com/v1/posts/post-1" (second (second (quegly-asked)))))
      (is (search "Request timed out after 30000ms" answer) "the platform's detail reaches the model"))))

(deftest quegly-cell-change-post-takes-one-of-three-actions (with-quegly-wire)
  (with-cell-stop ((quegly-start "api_key" "qgl_test"))
    (quegly-reply +quegly-post+)
    (quegly-call "quegly_change_post" "{\"action\": \"publish_now\", \"post_id\": \"post-1\"}")
    (destructuring-bind (method url . args) (first (quegly-asked))
      (declare (ignore args))
      (is (eq :post method))
      (is (equal "https://api.quegly.com/v1/posts/post-1/publish-now" url) "publish now's own path"))
    (quegly-call "quegly_change_post" "{\"action\": \"retry\", \"post_id\": \"post-1\",
                                           \"platforms\": [\"x\"]}")
    (let ((request (second (quegly-asked))))
      (is (equal "https://api.quegly.com/v1/posts/post-1/retry" (second request)))
      (is (equalp #("x") (nlk:json-value (quegly-body request) :array "platforms")) "the platforms to retry, and no others"))
    (quegly-reply "{\"id\": \"post-1\"}" 200)
    (let ((answer (quegly-call "quegly_change_post" "{\"action\": \"delete\", \"post_id\": \"post-1\"}")))
      (destructuring-bind (method url . args) (third (quegly-asked))
        (declare (ignore args))
        (is (eq :delete method) "delete is its own method")
        (is (equal "https://api.quegly.com/v1/posts/post-1" url)))
      (is (search "post post-1 deleted" answer)))
    (let ((answer (quegly-call "quegly_change_post" "{\"action\": \"archive\", \"post_id\": \"post-1\"}")))
      (is (search "publish_now" answer) "an action the tool does not carry is refused"))))

;;; --- uploads and analytics ------------------------------------------------------

(deftest quegly-cell-upload-answers-the-media-id (with-quegly-wire)
  (with-cell-stop ((quegly-start "api_key" "qgl_test"))
    (quegly-reply "{\"id\": \"media-1\", \"media_type\": \"image\",
                    \"url\": \"https://media.test/one.jpg\"}" 201)
    (let ((answer (quegly-call "quegly_upload" "{\"url\": \"https://example.test/one.jpg\"}")))
      (let ((body (quegly-body (first (quegly-asked)))))
        (is (equal "https://api.quegly.com/v1/upload-from-url" (second (first (quegly-asked)))))
        (is (equal "https://example.test/one.jpg" (nlk:json-value body :string "url")) "Quegly fetches the bytes; the cell sends the link"))
      (is (search "media-1" answer) "the media id reaches the model"))))

(deftest quegly-cell-analytics-answers-one-post-or-a-range (with-quegly-wire)
  (with-cell-stop ((quegly-start "api_key" "qgl_test"))
    (quegly-reply "{\"post_id\": \"post-1\", \"platforms\": [{\"platform\": \"instagram\",
                     \"account_id\": \"acc-ig\", \"metrics\": {\"views\": 15420, \"likes\": 892},
                     \"fetched_at\": \"2026-06-02T10:00:00.000Z\"}]}")
    (let ((answer (quegly-call "quegly_analytics" "{\"post_id\": \"post-1\"}")))
      (is (equal "https://api.quegly.com/v1/analytics/post/post-1" (second (first (quegly-asked)))))
      (is (search "likes 892" answer) "each number the platform gave")
      (is (search "views 15420" answer)))
    (quegly-reply "{\"posts\": [], \"start_date\": \"2026-06-01T00:00:00.000Z\",
                    \"end_date\": \"2026-06-30T23:59:59.000Z\"}")
    (let ((answer (quegly-call "quegly_analytics" "{\"start_date\": \"2026-06-01\", \"end_date\": \"2026-06-30\"}")))
      (is (search "analytics?start_date=2026-06-01" (second (second (quegly-asked)))) "the whole workspace's range")
      (is (search "0 published posts" answer) "and an empty range says so"))))

;;; --- refusals and the command ---------------------------------------------------

(deftest quegly-cell-a-refused-key-is-the-model-s-repair (with-quegly-wire)
  (with-cell-stop ((quegly-start "api_key" "qgl_test"))
    (quegly-reply "{\"error\": {\"code\": \"UNAUTHORIZED\", \"message\": \"Invalid API key\"}}" 401)
    (let ((answer (quegly-call "quegly_workspaces" "{}")))
      (is (search "401" answer) "the failure names the status")
      (is (search "Invalid API key" answer) "and Quegly's own message")
      (is (search "api_key" answer) "and tells the model what to check")
      (is (null (search "qgl_test" answer)) "and never echoes the key"))))

(deftest quegly-cell-a-paused-channel-says-what-to-do (with-quegly-wire)
  (with-cell-stop ((quegly-start "api_key" "qgl_test"))
    (quegly-reply "{\"error\": {\"code\": \"CHANNEL_PAUSED\", \"message\": \"acc-tt is paused\",
                    \"field\": \"account_ids\"}}" 400)
    (let ((answer (quegly-call "quegly_create_post"
                               "{\"type\": \"now\", \"account_ids\": [\"acc-tt\"], \"caption\": \"x\"}")))
      (is (search "CHANNEL_PAUSED" answer) "the code")
      (is (search "leave it out of account_ids" answer) "and what the model should do about it"))))

(deftest quegly-cell-the-command-says-what-it-is-connected-to (with-quegly-wire)
  (with-cell-stop ((quegly-start "api_key" "qgl_test"))
    (let ((status (nodecode-quegly::run-command "" nil)))
      (is (search "https://api.quegly.com/v1" status) "no argument reports the endpoint")
      (is (search "key set" status) "and that a key is there")
      (is (null (search "qgl_test" status)) "without ever printing the key")
      (is (search "8 tools" status) "and how many tools it serves")
      (is (null (quegly-asked)) "and sends nothing"))
    (quegly-reply +quegly-accounts+)
    (let ((answer (nodecode-quegly::run-command "accounts" nil)))
      (is (search "acc-ig" answer) "accounts asks Quegly for the connected accounts")
      (is (equal "https://api.quegly.com/v1/accounts" (second (first (quegly-asked))))))))

