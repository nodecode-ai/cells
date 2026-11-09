;;;; client.lisp --- the REST verbs, as the original's GitHubClient had them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One function per endpoint the seven tools reach, with the arguments the
;;;; original validated (github_app/client.py): an explicit owner/name
;;;; repository, a positive number, a full 40-character SHA for a review or a
;;;; merge. Each answers the JSON GitHub answered.

(in-package #:nodecode-github-app)

(defparameter +name-pattern+ "^[A-Za-z0-9_.-]+$"
  "What GitHub allows in an owner or repository name.")

(defparameter +full-sha-pattern+ "^[0-9A-Fa-f]{40}$"
  "A full commit SHA, as the original required for a review and a merge.")

(defun parse-repo (text)
  "OWNER and NAME (values) of \"owner/name\", or a refusal. The original
refused anything but the explicit two-part form, so a tool call can never
reach a repository the model did not name."
  (let* ((raw (string-trim " /" (or text "")))
         (parts (uiop:split-string raw :separator '(#\/))))
    (unless (and (= 2 (length parts))
                 (every #'plusp (mapcar #'length parts))
                 (every (lambda (part) (ppcre:scan +name-pattern+ part)) parts)
                 (notany (lambda (part) (member part '("." "..") :test #'string=)) parts))
      (fail "repo must use the explicit owner/name format, not ~s" text))
    (values (first parts) (second parts))))

(defun positive-int (value field)
  "VALUE as a positive integer, or a refusal naming FIELD."
  (let ((number (and (integerp value) value)))
    (unless (and number (plusp number))
      (fail "~a must be a positive integer, not ~s" field value))
    number))

(defun full-sha (value field)
  "VALUE as a full 40-character commit SHA, or a refusal naming FIELD."
  (let ((sha (and (stringp value) (nlk:trimmed value))))
    (unless (and sha (ppcre:scan +full-sha-pattern+ sha))
      (fail "~a must be a full 40-character Git commit SHA, not ~s" field value))
    sha))

(defun rest-call (method path &key params body)
  (api-json method path :token (token) :params params :body body))

(defun github-identity-info (&key (repositories t))
  "What the credential is, and -- when REPOSITORIES -- what it reaches: the
original's verify_identity, minus the second /user read it deferred."
  (let* ((method (getf (auth) :method))
         (info (nlk:json-object "auth_method" method "actor" (actor))))
    (if (string= method "github-app")
        (progn
          (setf (gethash "app_slug" info)
                (subseq (actor) 0 (max 0 (- (length (actor)) 5))))
          (when repositories
            (let ((answer (ignore-errors
                            (rest-call :get "/installation/repositories"
                                       :params '("per_page" 100)))))
              (setf (gethash "accessible_repos" info)
                    (coerce (loop for repo across (nlk:json-array answer "repositories")
                                  collect (or (nlk:json-value repo :string "full_name") ""))
                            'vector)))))
        (let ((user (rest-call :get "/user")))
          (setf (gethash "account" info) (actor)
                (gethash "user_type" info) (nlk:json-value user :string "type"))
          (when repositories
            (let ((repos (rest-call :get "/user/repos" :params '("per_page" 100 "sort" "updated"))))
              (setf (gethash "accessible_repos" info)
                    (coerce (loop for repo across (nlk:json-array repos)
                                  collect (or (nlk:json-value repo :string "full_name") ""))
                            'vector))))))
    info))

(defun github-list-issues (owner repo &key (state "open") (labels "") (assignee "") (creator "")
                                (sort "created") (direction "desc") (per-page 30))
  "The issues and pull requests of a repository, as GitHub lists them."
  (rest-call :get (format nil "/repos/~a/~a/issues" owner repo)
             :params (list "state" (if (member state '("open" "closed" "all") :test #'string=)
                                        state "open")
                           "sort" (if (member sort '("created" "updated" "comments") :test #'string=)
                                      sort "created")
                           "direction" (if (member direction '("asc" "desc") :test #'string=)
                                           direction "desc")
                           "per_page" (min (max (or per-page 30) 1) 100)
                           "labels" labels
                           "assignee" assignee
                           "creator" creator)))

(defun github-read-issue (owner repo number &key include-comments)
  "One issue or pull request, and its comments when INCLUDE-COMMENTS."
  (let ((issue (rest-call :get (format nil "/repos/~a/~a/issues/~a" owner repo number))))
    (when (and include-comments (hash-table-p issue))
      (let ((comments (rest-call :get (format nil "/repos/~a/~a/issues/~a/comments" owner repo number)
                                  :params '("per_page" 100))))
        (setf (gethash "comments_data" issue) comments)))
    issue))

(defun github-create-issue (owner repo title &key body (labels nil) (assignees nil))
  "Open an issue. The original's own ordering: the title first, then what was
actually given."
  (rest-call :post (format nil "/repos/~a/~a/issues" owner repo)
             :body (nlk:json-object "title" title
                                     :opt "body" body
                                     :opt "labels" (and labels (coerce labels 'vector))
                                     :opt "assignees" (and assignees (coerce assignees 'vector)))))

(defun github-comment-issue (owner repo number body)
  (rest-call :post (format nil "/repos/~a/~a/issues/~a/comments" owner repo number)
             :body (nlk:json-object "body" body)))

(defun github-pull-request (owner repo number)
  (rest-call :get (format nil "/repos/~a/~a/pulls/~a" owner repo number)))

(defun review-comment-payload (comment)
  "One inline comment as GitHub takes it: position, or line with a side."
  (let ((path (nonblank (review-comment-path comment)))
        (body (nonblank (review-comment-body comment)))
        (position (review-comment-position comment))
        (line (review-comment-line comment))
        (side (nonblank (review-comment-side comment))))
    (unless (and path body)
      (fail "each inline comment needs a path and a body"))
    (cond
      (position
       (when (or line side)
         (fail "an inline comment takes position, or line with side, not both"))
       (nlk:json-object "path" path "body" body
                         "position" (positive-int position "comments.position")))
      (line
       (unless (member side '("LEFT" "RIGHT") :test #'string=)
         (fail "a line comment needs side LEFT or RIGHT"))
       (nlk:json-object "path" path "body" body
                         "line" (positive-int line "comments.line")
                         "side" side))
      (t (fail "an inline comment needs position, or line with side")))))

(defun github-review-pull-request (owner repo number event commit-id &key (body "") (comments nil))
  "Submit a review, once the pull request's head is still COMMIT-ID: the
original refused a stale head rather than review a diff it had not read."
  (let* ((event (string-upcase (or event "")))
         (sha (full-sha commit-id "commit_id")))
    (unless (member event '("APPROVE" "REQUEST_CHANGES" "COMMENT") :test #'string=)
      (fail "event must be APPROVE, REQUEST_CHANGES, or COMMENT, not ~s" event))
    (let* ((pull (github-pull-request owner repo number))
           (head (or (nlk:json-value pull :string "head" "sha") "")))
      (unless (string-equal head sha)
        (fail "the pull request head moved: read the live head ~a before reviewing"
              (if (plusp (length head)) head "GitHub did not answer")))
      (rest-call :post (format nil "/repos/~a/~a/pulls/~a/reviews" owner repo number)
                 :body (nlk:json-object "event" event "commit_id" sha
                                         :opt "body" (nonblank body)
                                         :opt "comments"
                                         (and comments
                                              (coerce (loop for comment across comments
                                                             collect (review-comment-payload comment))
                                                      'vector)))))))

(defun github-merge-pull-request (owner repo number expected-head-sha
                           &key (method "squash") (commit-title ""))
  "Merge a pull request, but only the head EXPECTED-HEAD-SHA names."
  (let ((sha (full-sha expected-head-sha "expected_head_sha"))
        (method (string-downcase (or method "squash"))))
    (unless (member method '("squash" "merge" "rebase") :test #'string=)
      (fail "method must be squash, merge, or rebase, not ~s" method))
    (rest-call :put (format nil "/repos/~a/~a/pulls/~a/merge" owner repo number)
               :body (nlk:json-object "merge_method" method "sha" sha
                                       :opt "commit_title" (nonblank commit-title)))))
