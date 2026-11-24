;;;; tools.lisp --- the eight tools, their arguments, and what each answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin carried seventeen MCP tools served by Quegly's own server.
;;;; Eight of them -- the ones a posting session runs through: workspaces,
;;;; accounts, an upload by url, create, list, get, act on, and analytics --
;;;; are this cell's tools, each one NLK:DEFINE-CONTRACT, so the shape the
;;;; model is shown and the shape the body reads are one declaration. Each
;;;; answers text a model reads: what was done, the ids to keep, and the
;;;; platform's own error where one failed. A refusal is QUEGLY-ERROR with
;;;; the service's words in it, the key never.

(in-package #:nodecode-quegly)

;;; --- the calls the model makes --------------------------------------------------

(nlk:define-contract workspaces-call
  (:doc "List the Quegly workspaces this API key can act on, with the id of each. Call it
first: when the key reaches all the workspaces, every other tool needs a workspace_id from
here. Call it again after the operator adds or archives a workspace on quegly.com.")
  ("include_archived" :boolean
   :doc "list the archived workspaces too, which still take reads and refuse posts"))

(nlk:define-contract accounts-call
  (:doc "List the connected social accounts of a workspace, with the account id each is posted
to. A post names accounts by id, never by platform: one workspace can hold two accounts of one
platform. Connecting an account happens on quegly.com, never through these tools.")
  ("workspace_id" :string
   :doc "the workspace whose accounts to list; leave it out when the key is bound to one")
  ("platform" :string
   :doc "keep one platform's accounts only, such as instagram, tiktok, youtube, x or bluesky"))

(nlk:define-contract create-post-call
  (:doc "Create one post on Quegly and schedule it, save it as a draft, or publish it now. One
call, every account: do not loop one call per platform. Media goes in by id, uploaded first
with quegly_upload. Ask the operator before type now, and before scheduling anything that
reaches their real accounts.")
  ("type" :text :required t :slot post-type :options '("schedule" "now" "draft")
   :doc "schedule for later, now to publish at once, or draft to save without publishing")
  ("account_ids" :array :of :string :required t
   :doc "the account ids the post goes to, from quegly_accounts")
  ("caption" :string
   :doc "the text of the post, at most 5000 characters; platform_settings.<platform>.caption gives one platform its own")
  ("scheduled_at" :string
   :doc "when it goes out, ISO 8601 UTC such as 2026-06-01T14:00:00.000Z, up to 90 days ahead; required for type schedule")
  ("media_ids" :array :of :string
   :doc "the media ids from quegly_upload: 1 to 10 images, or one video, never both")
  ("workspace_id" :string
   :doc "the workspace to create in; leave it out when the key is bound to one")
  ("platform_settings" :object
   :doc "an object keyed by platform, each a settings object for every account of that platform: caption, and the platform's own fields such as pinterest board_id or tiktok privacy_level")
  ("account_settings" :object
   :doc "an object keyed by account id, for one account's settings of its own; an account here publishes with these instead of its platform's")
  ("idempotency_key" :string
   :doc "any short string; a retried call carrying the same one answers the first post instead of making a second"))

(nlk:define-contract posts-call
  (:doc "List the posts of a date range, scheduled and published both, with each one's status
and the platforms it goes to. Give the range in UTC: to list a week the operator means, turn
their local dates into UTC first.")
  ("start_date" :text :required t :doc "the range's start, ISO 8601 such as 2026-06-01")
  ("end_date" :text :required t :doc "the range's end, ISO 8601 such as 2026-06-30")
  ("workspace_id" :string
   :doc "the workspace to read; leave it out when the key is bound to one"))

(nlk:define-contract post-call
  (:doc "Read one post by id: its status, and for each platform whether it is scheduled,
posted or failed, with the post's url and the platform's own error. Publishing is
asynchronous, so read it again after a publish_now or a retry until nothing is posting.")
  ("post_id" :text :required t
   :doc "the post's id, as quegly_posts or quegly_create_post answered it")
  ("workspace_id" :string
   :doc "the workspace it lives in; leave it out when the key is bound to one"))

(nlk:define-contract change-post-call
  (:doc "Act on one post. publish_now sends a scheduled post at once; retry publishes a
partial or failed post again on the platforms that failed, on the day it was scheduled, three
times per platform; delete removes a scheduled post or a draft for good. Ask the operator
before any of the three: each reaches the public or cannot be undone.")
  ("action" :text :required t :options '("publish_now" "retry" "delete")
   :doc "publish_now, retry or delete")
  ("post_id" :text :required t :doc "the post's id, from quegly_posts")
  ("platforms" :array :of :string
   :doc "retry only: the platforms to try again, such as [\"facebook\"]; leave it out to retry every failed one")
  ("workspace_id" :string
   :doc "the workspace it lives in; leave it out when the key is bound to one"))

(nlk:define-contract upload-call
  (:doc "Upload a photo or video to Quegly from a public url, which answers the media id a
post carries. The url must be reachable without a sign-in: a pre-signed link or a public
file. Quegly checks the bytes, not the name: JPEG or PNG at most 25 MB, or MP4 or MOV at
most 500 MB.")
  ("url" :text :required t :doc "a public http or https link to the photo or video")
  ("filename" :string :doc "the name to store it under; taken from the url when left out")
  ("workspace_id" :string
   :doc "the workspace to upload into; leave it out when the key is bound to one"))

(nlk:define-contract analytics-call
  (:doc "Read the engagement numbers of published posts: views, likes, comments, shares and
whatever else each platform gives. One post's numbers when post_id is given, otherwise every
published post of a date range. A number a platform does not give is missing, never zero: say
so rather than reporting 0. Telegram gives none, and Pinterest only for business accounts.")
  ("post_id" :string :doc "one post's analytics by its id; leave it out for the whole workspace")
  ("start_date" :string :doc "the range's start, such as 2026-06-01; 30 days back by default")
  ("end_date" :string :doc "the range's end, such as 2026-06-30; now by default")
  ("workspace_id" :string
   :doc "the workspace to read; leave it out when the key is bound to one"))

;;; --- reading what a call carries -----------------------------------------------

(defun rows (value)
  "VALUE, a decoded JSON array, as a list; NIL for anything else."
  (if (and value (not (stringp value)) (vectorp value))
      (coerce value 'list)
      '()))

(defun lower (value &key fallback)
  "VALUE lowercased and trimmed, FALLBACK when it is empty."
  (let ((text (string-downcase (nlk:trimmed (or value "")))))
    (if (plusp (length text)) text fallback)))

(defun path-id (value name)
  "VALUE as one path segment: a non-empty string, url-encoded, or a refusal."
  (let ((id (nonempty value)))
    (unless id (fail "~a is required" name))
    (when (find #\/ id) (fail "~a must be one id, not a path" name))
    (quri:url-encode id :encoding :utf-8)))

(defun iso-time (value name)
  "VALUE as an ISO 8601 date-time, or a refusal naming the shape."
  (let ((text (nonempty value)))
    (unless text (fail "~a is required" name))
    (unless (and (>= (length text) 16)
                 (member (char text 10) '(#\T #\Space)))
      (fail "~a must be an ISO 8601 UTC date-time such as 2026-06-01T14:00:00.000Z" name))
    text))

(defun date-value (value name)
  "VALUE as a date at least YYYY-MM-DD long, or NIL."
  (let ((text (nonempty value)))
    (when (and text (< (length text) 10))
      (fail "~a must use YYYY-MM-DD" name))
    text))

(defun metrics-text (metrics)
  "METRICS, a decoded JSON object, as one line of name and value; empty when absent."
  (if (hash-table-p metrics)
      (format nil "~{~a ~a~^, ~}"
              (loop for key in (sort (loop for name being the hash-keys of metrics collect name)
                                      #'string<)
                    append (list key (princ-to-string (gethash key metrics)))))
      ""))

;;; --- what Quegly's objects read as ----------------------------------------------

(defun describe-workspaces (body include-archived)
  (let ((found (remove-if-not (lambda (row)
                                (or include-archived
                                    (not (nlk:json-value row :boolean "is_archived"))))
                              (rows (nlk:json-value body :array "workspaces")))))
    (if (null found)
        "quegly: this key reaches no workspace"
        (with-output-to-string (out)
          (format out "~d workspace~:p this key can act on; pass one's id as workspace_id.~%"
                  (length found))
          (dolist (row found)
            (format out "- ~a  id ~a~@[  slug ~a~]~@[  archived~]~%"
                    (or (nlk:json-value row :text "name") "(unnamed)")
                    (or (nlk:json-value row :text "id") "?")
                    (nlk:json-value row :text "slug")
                    (nlk:json-value row :boolean "is_archived")))))))

(defun describe-accounts (body platform)
  (let ((found (remove-if-not (lambda (row)
                                (or (null platform)
                                    (equal platform (nlk:json-value row :string "platform"))))
                              (rows (nlk:json-value body :array "accounts")))))
    (if (null found)
        (format nil "quegly: no connected account~@[ on ~a~]" platform)
        (with-output-to-string (out)
          (format out "~d connected account~:p; a post goes to account ids, never to a platform name.~%"
                  (length found))
          (dolist (row found)
            (format out "- ~a  ~a  id ~a~%"
                    (or (nlk:json-value row :text "platform") "?")
                    (or (nlk:json-value row :text "account_username")
                        (nlk:json-value row :text "display_name") "?")
                    (or (nlk:json-value row :text "id") "?")))))))

(defun describe-platforms (out row)
  (format out "  - ~a~@[ ~a~]: ~a~@[  ~a~]~@[  error: ~a~]~@[ (~a)~]~%"
          (or (nlk:json-value row :text "platform") "?")
          (nlk:json-value row :text "account_username")
          (or (nlk:json-value row :text "status") "?")
          (nlk:json-value row :text "post_url")
          (nlk:json-value row :text "error")
          (nlk:json-value row :text "error_detail")))

(defun describe-post (body &optional (lead "post"))
  (with-output-to-string (out)
    (format out "~a ~a: ~a~@[  scheduled ~a~]~%"
            lead
            (or (nlk:json-value body :text "id") "?")
            (or (nlk:json-value body :text "status") "?")
            (nlk:json-value body :text "scheduled_at"))
    (let ((found (rows (nlk:json-value body :array "platforms"))))
      (if (null found)
          (format out "  no platform yet~%")
          (dolist (row found) (describe-platforms out row))))))

(defun describe-posts (body)
  (let ((found (rows (nlk:json-value body :array "posts"))))
    (if (null found)
        "quegly: no post in that range"
        (with-output-to-string (out)
          (format out "~d post~:p~%" (length found))
          (dolist (row found)
            (format out "- ~a  ~a~@[  scheduled ~a~]  ~d platform~:p~%"
                    (or (nlk:json-value row :text "id") "?")
                    (or (nlk:json-value row :text "status") "?")
                    (nlk:json-value row :text "scheduled_at")
                    (length (rows (nlk:json-value row :array "platforms")))))))))

(defun describe-post-analytics (body)
  (with-output-to-string (out)
    (format out "analytics for post ~a~%" (or (nlk:json-value body :text "post_id") "?"))
    (let ((found (rows (nlk:json-value body :array "platforms"))))
      (if (null found)
          (format out "  no platform measured yet~%")
          (dolist (row found)
            (let ((metrics (metrics-text (nlk:json-value row :object "metrics"))))
              (format out "  - ~a~@[ ~a~]~@[: ~a~]~@[  measured ~a~]~%"
                      (or (nlk:json-value row :text "platform") "?")
                      (nlk:json-value row :text "account_id")
                      (and (plusp (length metrics)) metrics)
                      (nlk:json-value row :text "fetched_at"))))))))

(defun describe-analytics (body)
  (let ((found (rows (nlk:json-value body :array "posts"))))
    (with-output-to-string (out)
      (format out "~d published post~:p, ~a to ~a~%"
              (length found)
              (or (nlk:json-value body :text "start_date") "?")
              (or (nlk:json-value body :text "end_date") "?"))
      (dolist (row found)
        (format out "- ~a  ~a~@[  ~a~]~%"
                (or (nlk:json-value row :text "id") "?")
                (or (nlk:json-value row :text "status") "?")
                (nlk:json-value row :text "scheduled_at"))
        (dolist (platform (rows (nlk:json-value row :array "platforms")))
          (let ((metrics (metrics-text (nlk:json-value platform :object "metrics"))))
            (format out "  - ~a~@[ ~a~]~@[: ~a~]~@[  ~a~]~%"
                    (or (nlk:json-value platform :text "platform") "?")
                    (nlk:json-value platform :text "account_username")
                    (and (plusp (length metrics)) metrics)
                    (nlk:json-value platform :text "post_url"))))))))

;;; --- the tools -----------------------------------------------------------------

(defun tool-workspaces (call)
  (describe-workspaces (request :get "/workspaces" :what "listing workspaces")
                       (workspaces-call-include-archived call)))

(defun tool-accounts (call)
  (describe-accounts (request :get "/accounts"
                              :workspace (accounts-call-workspace-id call)
                              :what "listing accounts")
                     (lower (accounts-call-platform call))))

(defun tool-create-post (call)
  (let* ((type (lower (create-post-call-post-type call) :fallback "schedule"))
         (caption (nonempty (create-post-call-caption call)))
         (scheduled (nonempty (create-post-call-scheduled-at call)))
         (media (create-post-call-media-ids call))
         (accounts (create-post-call-account-ids call))
         (platform-settings (create-post-call-platform-settings call))
         (account-settings (create-post-call-account-settings call)))
    (unless (member type '("schedule" "now" "draft") :test #'equal)
      (fail "type must be schedule, now or draft, not ~s" type))
    (when (and (equal type "schedule") (null scheduled))
      (fail "a scheduled post needs scheduled_at, an ISO 8601 UTC time such as 2026-06-01T14:00:00.000Z"))
    (unless accounts
      (fail "account_ids is required: the accounts to post to, from quegly_accounts"))
    (let ((body (apply #'nlk:make-json-object
                       (append (list "type" type)
                               (when caption (list "caption" caption))
                               (when scheduled (list "scheduled_at" (iso-time scheduled "scheduled_at")))
                               (when media (list "media_ids" media))
                               (list "account_ids" accounts)
                               (when (hash-table-p platform-settings)
                                 (list "platform_settings" platform-settings))
                               (when (hash-table-p account-settings)
                                 (list "account_settings" account-settings))))))
      (describe-post (request :post "/posts"
                              :body body
                              :workspace (create-post-call-workspace-id call)
                              :idempotency (create-post-call-idempotency-key call)
                              :what (format nil "creating a ~a post" type))
                     "created post"))))

(defun tool-posts (call)
  (describe-posts (request :get "/posts"
                           :query (list (cons "start_date" (date-value (posts-call-start-date call)
                                                                        "start_date"))
                                        (cons "end_date" (date-value (posts-call-end-date call)
                                                                      "end_date")))
                           :workspace (posts-call-workspace-id call)
                           :what "listing posts")))

(defun tool-post (call)
  (describe-post (request :get (format nil "/posts/~a" (path-id (post-call-post-id call) "post_id"))
                          :workspace (post-call-workspace-id call)
                          :what "reading a post")))

(defun tool-change-post (call)
  (let* ((id (path-id (change-post-call-post-id call) "post_id"))
         (action (lower (change-post-call-action call)))
         (workspace (change-post-call-workspace-id call)))
    (cond ((equal action "publish_now")
           (describe-post (request :post (format nil "/posts/~a/publish-now" id)
                                   :workspace workspace
                                   :what "publishing a post now")
                          "published post"))
          ((equal action "retry")
           (describe-post (request :post (format nil "/posts/~a/retry" id)
                                   :body (let ((platforms (change-post-call-platforms call)))
                                           (and platforms
                                                (nlk:make-json-object "platforms" platforms)))
                                   :workspace workspace
                                   :what "retrying a post")
                          "retried post"))
          ((equal action "delete")
           (request :delete (format nil "/posts/~a" id)
                    :workspace workspace
                    :what "deleting a post")
           (format nil "post ~a deleted" id))
          (t (fail "action must be publish_now, retry or delete, not ~s" action)))))

(defun tool-upload (call)
  (let ((body (request :post "/upload-from-url"
                       :body (apply #'nlk:make-json-object
                                    (append (list "url" (nonempty (upload-call-url call)))
                                            (let ((filename (nonempty (upload-call-filename call))))
                                              (when filename (list "filename" filename)))))
                       :workspace (upload-call-workspace-id call)
                       :what "uploading media")))
    (format nil "media ~a (~a)~@[  ~a~]; pass its id in media_ids~%"
            (or (nlk:json-value body :text "id") "?")
            (or (nlk:json-value body :text "media_type") "?")
            (nlk:json-value body :text "url"))))

(defun tool-analytics (call)
  (let ((post (nonempty (analytics-call-post-id call))))
    (if post
        (describe-post-analytics
         (request :get (format nil "/analytics/post/~a" (path-id post "post_id"))
                  :workspace (analytics-call-workspace-id call)
                  :what "reading one post's analytics"))
        (describe-analytics
         (request :get "/analytics"
                  :query (list (cons "start_date" (date-value (analytics-call-start-date call)
                                                               "start_date"))
                               (cons "end_date" (date-value (analytics-call-end-date call)
                                                             "end_date")))
                  :workspace (analytics-call-workspace-id call)
                  :what "reading analytics")))))
