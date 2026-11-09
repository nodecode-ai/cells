;;;; cell.lisp --- the seven tools, /github-app, and the one declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin's seven tools, with the names, arguments and answers
;;;; the model already knows (github_app/tools.py, TOOL_SPECS), and one gate:
;;;; the original's allow_write_actions setting, so the four writing verbs
;;;; refuse until the operator turns them on. Every answer carries the
;;;; attribution the original insisted on -- which identity acted.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "github_app": {"app_id": "123456", "installation_id": "45678901",
;;;;                  "private_key_file": "~/.config/github-app.pem",
;;;;                  "allow_write_actions": false}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-github-app)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "The nodecode-github-app cell acts on GitHub as one identity: a GitHub App
installation (preferred, and what the original called bot-first), a personal
access token, or the operator's own gh CLI. Seven tools, also callable from
eval in the GITHUB-APP package:
  github_identity        {}                     who the token is, and the repositories it reaches
  github_list_issues     {repo, state, labels, assignee, creator, sort, direction, per_page}
      The issues and pull requests of one repository, as GitHub lists them.
  github_get_issue       {repo, number, include_comments}
      One thread: number, title, state, author, body, labels, comments.
  github_create_issue    {repo, title, body, labels, assignees}
  github_comment_issue   {repo, number, body}
  github_review_pr       {repo, number, event, commit_id, body, comments}
      event APPROVE, REQUEST_CHANGES or COMMENT; commit_id is the full 40-character
      head SHA the review applies to, and the review is refused when the head moved.
  github_merge_pr        {repo, number, expected_head_sha, method, commit_title}
      method squash (default), merge or rebase; the merge is refused unless the
      head is still expected_head_sha.
repo is always the explicit owner/name. The four writing tools need the
section's allow_write_actions, which is off by default: until it is on they
answer write_actions_disabled, and nothing is sent. Read a thread before
answering it, and say in the comment what you did."
  "What (help :github-app) answers while the cell runs.")

;;; --- answers, and the one condition ---------------------------------------------

(defun answer (object)
  (nlk:encode-json-object object))

(defun refusal (code message)
  (answer (nlk:json-object "success" nil
                            "error" (nlk:json-object "code" code "message" message))))

(defmacro answering (&body body)
  "BODY's text; a refusal becomes the call's own failure text, never an
unhandled error -- the original answered the same JSON-shaped failure."
  `(handler-case (progn ,@body)
     (github-app-error (condition)
       (refusal "github_error" (github-app-error-detail condition)))
     (error (condition)
       (refusal "internal_error"
                (format nil "GitHub connector failed: ~a"
                        (nlk:one-line (princ-to-string condition)))))))

(defun attribution-object () (attribution))

;;; --- reading the section --------------------------------------------------------

(defparameter +environ-fields+ '("GITHUB_APP_ID" "GITHUB_APP_INSTALLATION_ID"
                                  "GITHUB_APP_PRIVATE_KEY_PATH" "GITHUB_TOKEN")
  "The variables the original read from the environment; here only token_env
names one, and the rest are written in the section.")

(defun read-settings (values table)
  "The declared VALUES, with the credential resolved here, once: an App's ID,
installation and key, else a token, else the gh CLI -- the original's order
(github_app/client.py, _resolve_credentials). A section that names no
credential refuses the start."
  (declare (ignore table))
  (let* ((plist (copy-list values))
         (app-id (nonblank (getf values :app-id)))
         (installation (nonblank (getf values :installation-id)))
         (key-file (nonblank (getf values :private-key-file)))
         (key (nonblank (getf values :private-key)))
         (env (nonblank (getf values :token-env)))
         (file (nonblank (getf values :token-file))))
    (when (and env file)
      (nlk:config-error "set one of token_env and token_file, not both"))
    (cond
      (app-id
       (unless installation
         (nlk:config-error "app_id names a GitHub App: set installation_id too"))
       (unless (or key key-file)
         (nlk:config-error "app_id names a GitHub App: give its private key, in private_key_file or private_key"))
       (setf (getf plist :method) :app
             (getf plist :private-key)
             (or key
                 (or (ignore-errors (uiop:read-file-string (nlk:expand-home key-file)))
                     (nlk:config-error "private_key_file ~a cannot be read" key-file)))))
      (t
       (let ((token (cond (env (or (nonblank (nle::credential-env env))
                                   (nlk:config-error "token_env names ~a, which is unset or empty" env)))
                          (file (or (nonblank (ignore-errors
                                               (uiop:read-file-string (nlk:expand-home file))))
                                    (nlk:config-error "token_file ~a cannot be read, or is empty" file))))))
         (cond
           (token (setf (getf plist :method) :pat (getf plist :token) token))
           ((getf values :gh-cli)
            (let ((read (gh-cli-token (nonblank (getf values :api-url)))))
              (unless read
                (nlk:config-error "no GitHub credential: set app_id, or token_env, or sign in with the gh CLI"))
              (setf (getf plist :method) :gh (getf plist :token) read)))
           (t (nlk:config-error
               "no GitHub credential: set app_id with installation_id and private_key_file, or token_env to the variable holding a token, or token_file to a file holding one"))))))
    plist))

;;; --- the write gate ---------------------------------------------------------------

(defun writes-allowed-p ()
  (setting :allow-write-actions))

(defun write-refusal ()
  "The original's write_actions_disabled answer, as JSON the model reads."
  (refusal "write_actions_disabled"
           (format nil "GitHub write actions are disabled. Set allow_write_actions to true in the ~
github_app section of the config, then (restart-cells \"nodecode-github-app\") to enable them.")))

;;; --- the seven tools ---------------------------------------------------------------

(defun verify-identity (request)
  (declare (ignore request))
  (answering
    (let ((info (github-identity-info :repositories (github-identity-repositories request))))
      (setf (gethash "success" info) t
            (gethash "action" info) "identity")
      (answer info))))

(defun list-issues (request)
  (answering
    (multiple-value-bind (owner repo) (parse-repo (issue-list-repo request))
      (let ((rows (github-list-issues owner repo
                                      :state (issue-list-state request)
                                      :labels (or (issue-list-label-names request) "")
                                      :assignee (or (issue-list-assignee request) "")
                                      :creator (or (issue-list-creator request) "")
                                      :sort (issue-list-sort-field request)
                                      :direction (issue-list-direction request)
                                      :per-page (issue-list-per-page request))))
        (answer (nlk:json-object
                 "success" t
                 "action" "list_issues"
                 "count" (length rows)
                 "issues" (coerce (loop for issue across (or rows #())
                                         collect (nlk:json-object
                                                  "number" (nlk:json-value issue :integer "number")
                                                  "title" (or (nlk:json-value issue :string "title") "")
                                                  "state" (or (nlk:json-value issue :string "state") "")
                                                  "user" (or (nlk:json-value issue :string "user" "login") "")
                                                  "labels" (coerce (loop for label across (nlk:json-array issue "labels")
                                                                         collect (or (nlk:json-value label :string "name") ""))
                                                                   'vector)
                                                  "created_at" (or (nlk:json-value issue :string "created_at") "")
                                                  "updated_at" (or (nlk:json-value issue :string "updated_at") "")
                                                  "pull_request" (and (gethash "pull_request" issue) t)))
                               'vector)
                 "attribution" (attribution-object)))))))

(defun read-issue (request)
  (answering
    (multiple-value-bind (owner repo) (parse-repo (issue-read-repo request))
      (let* ((number (positive-int (issue-read-thread-number request) "number"))
             (issue (github-read-issue owner repo number
                                       :include-comments (issue-read-include-comments request)))
             (comments (nlk:json-array issue "comments_data")))
        (answer (nlk:json-object
                 "success" t
                 "action" "get_issue"
                 "issue" (nlk:json-object
                          "number" (nlk:json-value issue :integer "number")
                          "title" (or (nlk:json-value issue :string "title") "")
                          "state" (or (nlk:json-value issue :string "state") "")
                          "author" (or (nlk:json-value issue :string "user" "login") "")
                          "url" (or (nlk:json-value issue :string "html_url") "")
                          "body" (nlk:clip (or (nlk:json-value issue :string "body") "") 8000)
                          "labels" (coerce (loop for label across (nlk:json-array issue "labels")
                                                  collect (or (nlk:json-value label :string "name") ""))
                                            'vector)
                          "comments" (nlk:json-value issue :integer "comments")
                          "created_at" (or (nlk:json-value issue :string "created_at") "")
                          "updated_at" (or (nlk:json-value issue :string "updated_at") "")
                          :opt "comments_data"
                          (and (plusp (length comments))
                               (coerce (loop for comment across comments
                                              collect (nlk:json-object
                                                       "author" (or (nlk:json-value comment :string "user" "login") "")
                                                       "created_at" (or (nlk:json-value comment :string "created_at") "")
                                                       "body" (nlk:clip (or (nlk:json-value comment :string "body") "") 2000)))
                                       'vector)))
                 "attribution" (attribution-object)))))))

(defun create-issue (request)
  (if (not (writes-allowed-p))
      (write-refusal)
      (answering
        (multiple-value-bind (owner repo) (parse-repo (issue-create-repo request))
          (let ((issue (github-create-issue owner repo (issue-create-title request)
                                            :body (issue-create-body request)
                                            :labels (coerce (issue-create-label-names request) 'list)
                                            :assignees (coerce (issue-create-assignees request) 'list))))
            (answer (nlk:json-object
                     "success" t
                     "action" "create_issue"
                     "issue_number" (nlk:json-value issue :integer "number")
                     "url" (or (nlk:json-value issue :string "html_url") "")
                     "state" (or (nlk:json-value issue :string "state") "")
                     "title" (or (nlk:json-value issue :string "title") "")
                     "attribution" (attribution-object))))))))

(defun comment-issue (request)
  (if (not (writes-allowed-p))
      (write-refusal)
      (answering
        (multiple-value-bind (owner repo) (parse-repo (issue-comment-repo request))
          (let ((comment (github-comment-issue owner repo
                                               (positive-int (issue-comment-thread-number request) "number")
                                               (issue-comment-body request))))
            (answer (nlk:json-object
                     "success" t
                     "action" "comment_issue"
                     "comment_id" (nlk:json-value comment :integer "id")
                     "url" (or (nlk:json-value comment :string "html_url") "")
                     "author" (or (nlk:json-value comment :string "user" "login") "")
                     "attribution" (attribution-object))))))))

(defun review-pull-request (request)
  (if (not (writes-allowed-p))
      (write-refusal)
      (answering
        (multiple-value-bind (owner repo) (parse-repo (pr-review-repo request))
          (let ((review (github-review-pull-request owner repo
                                                    (positive-int (pr-review-thread-number request) "number")
                                                    (pr-review-event request)
                                                    (pr-review-commit-id request)
                                                    :body (pr-review-body request)
                                                    :comments (pr-review-comments request))))
            (answer (nlk:json-object
                     "success" t
                     "action" "review_pr"
                     "review_id" (nlk:json-value review :integer "id")
                     "state" (or (nlk:json-value review :string "state") "")
                     "url" (or (nlk:json-value review :string "html_url") "")
                     "attribution" (attribution-object))))))))

(defun merge-pull-request (request)
  (if (not (writes-allowed-p))
      (write-refusal)
      (answering
        (multiple-value-bind (owner repo) (parse-repo (pr-merge-repo request))
          (let ((result (github-merge-pull-request owner repo
                                                   (positive-int (pr-merge-thread-number request) "number")
                                                   (pr-merge-expected-head-sha request)
                                                   :method (pr-merge-merge-method request)
                                                   :commit-title (pr-merge-commit-title request))))
            (answer (nlk:json-object
                     "success" (and (nlk:json-value result :boolean "merged") t)
                     "action" "merge_pr"
                     "merged" (and (nlk:json-value result :boolean "merged") t)
                     "message" (or (nlk:json-value result :string "message") "")
                     "sha" (or (nlk:json-value result :string "sha") "")
                     "url" (or (nlk:json-value result :string "html_url") "")
                     "attribution" (attribution-object))))))))

;;; --- /github-app ---------------------------------------------------------------

(defun run-slash (args session-id)
  "The /github-app command: who the cell acts as, and, given a repository, what
is open there -- as text a person reads."
  (declare (ignore session-id))
  (handler-case
      (let ((repo (nonblank (nlk:trimmed (or args "")))))
        (if repo
            (multiple-value-bind (owner name) (parse-repo repo)
              (let ((rows (github-list-issues owner name :per-page 20)))
                (if (zerop (length rows))
                    (format nil "No open issues in ~a/~a." owner name)
                    (format nil "~d open issue~:p or pull request~:p in ~a/~a:~%~{~a~^~%~}"
                            (length rows) owner name
                            (loop for issue across rows
                                  collect (format nil "- #~a ~a (~a, ~a)~@[ [pull]~]"
                                                  (nlk:json-value issue :integer "number")
                                                  (or (nlk:json-value issue :string "title") "")
                                                  (or (nlk:json-value issue :string "state") "")
                                                  (or (nlk:json-value issue :string "user" "login") "")
                                                  (and (gethash "pull_request" issue) t)))))))
            (let* ((info (github-identity-info))
                   (repos (nlk:json-array info "accessible_repos")))
              (format nil "GitHub as ~a (~a), reaching ~d repositor~:@p: ~@[~{~a~^, ~}~]"
                      (nlk:json-value info :string "actor")
                      (nlk:json-value info :string "auth_method")
                      (length repos)
                      (coerce (loop for repo across repos
                                     for index from 0 below 8
                                     collect repo)
                              'vector)))))
    (github-app-error (condition)
      (format nil "ERROR: ~a" (github-app-error-detail condition)))
    (error (condition)
      (format nil "ERROR: GitHub App: ~a" (nlk:one-line (princ-to-string condition))))))

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell github-app
  (:section ("github_app")
    (:guide "create a GitHub App and install it on the repositories it may reach, then set app_id, installation_id and private_key_file; or name a personal access token in token_env (classic: repo; fine-grained: the repositories' issues and pull requests, read, and write if the agent may act); allow_write_actions is off until you turn it on; next step: ask what the GitHub identity reaches")
    ("app_id" :string
              :doc "the GitHub App's App ID; with installation_id and a private key it is the bot-first identity the original used")
    ("installation_id" :string
                       :doc "the installation of that App on your account or organization")
    ("private_key_file" :path
                        :doc "the .pem the App's private key is in, as GitHub hands it out")
    ("private_key" :secret
                   :doc "the PEM itself, for a key kept in a secret store; private_key_file wins when both are set")
    ("app_slug" :string
                :doc "the App's slug, used to name the bot; empty asks GitHub")
    ("token_env" :env
                 :doc "the environment variable holding a personal access token (GITHUB_TOKEN), read when no App is configured")
    ("token_file" :path
                  :doc "a file holding a personal access token, which the file's own permissions guard")
    ("gh_cli" :boolean :default t
              :doc "read a token from an already-signed-in gh CLI when the section names none")
    ("allow_write_actions" :boolean :default nil
                           :doc "the gate: while false, creating issues, commenting, reviewing and merging refuse")
    ("api_url" :string :default "https://api.github.com"
               :doc "the REST API base, https only; set it for GitHub Enterprise Server")
    ("timeout" :integer :default 30 :min 1
               :doc "how long one GitHub request may take, in seconds"))
  (:settings #'read-settings)
  (:help :github-app
         "github-app: seven GitHub tools - identity, issues, comments, reviews, merges - as a GitHub App or your own token"
         *manual*)
  (:tool "github_identity" (nlk:contract 'github-identity) #'verify-identity)
  (:tool "github_list_issues" (nlk:contract 'issue-list) #'list-issues)
  (:tool "github_get_issue" (nlk:contract 'issue-read) #'read-issue)
  (:tool "github_create_issue" (nlk:contract 'issue-create) #'create-issue)
  (:tool "github_comment_issue" (nlk:contract 'issue-comment) #'comment-issue)
  (:tool "github_review_pr" (nlk:contract 'pr-review) #'review-pull-request)
  (:tool "github_merge_pr" (nlk:contract 'pr-merge) #'merge-pull-request)
  (:command "github-app" 'run-slash
            :description "The GitHub identity this cell acts as, and a repository's open issues"
            :argument-hint "REPO")
  (:start (lambda ()
            (setf *auth* nil
                  *installation* nil
                  *slug* nil))))
