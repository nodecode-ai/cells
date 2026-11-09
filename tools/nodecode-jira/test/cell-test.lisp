;;;; cell-test.lisp --- the cell against a stubbed Jira site.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches Jira, no
;;;; credential is needed and nothing is written. What is proved is the cell's
;;;; own wiring: the section (a healthy start, and a missing site, account or
;;;; token refused), the four tools through the dispatch the model calls them
;;;; by -- the JQL and the page sent, the issue key validated before it is a
;;;; URL, the untrusted warning on every answer -- and the /jira command.

(in-package #:nodecode.test)

(define-test-slice "jira" "JIRA-CELL-" :start nodecode-jira:start-cell)

;;; --- fixtures --------------------------------------------------------------------

(defparameter *calls* nil
  "Every request the stub answered, newest first, as (URL HEADERS).")

(defparameter *answer* nil
  "When set, (TEXT STATUS): what the stub answers to every request.")

(defparameter *story-fields* t
  "Whether the stubbed site has a story points field at all.")

(defparameter +site+ "https://example.atlassian.net")

(defun json-encoded (object)
  (nlk:encode-json-object object))

(defun issue-fields (key summary &key (status "In Progress"))
  (nlk:json-object "summary" summary
                    "status" (nlk:json-object "name" status
                                               "statusCategory" (nlk:json-object "key" "indeterminate"))
                    "issuetype" (nlk:json-object "name" "Story")
                    "priority" (nlk:json-object "name" "High")
                    "project" (nlk:json-object "key" "PROJ" "name" "Project")
                    "assignee" (nlk:json-object "displayName" "Dana")
                    "reporter" (nlk:json-object "displayName" "Robin")
                    "labels" (vector "auth" "backend")
                    "created" "2026-10-01T09:00:00.000+0000"
                    "updated" "2026-11-01T10:00:00.000+0000"
                    "customfield_10016" 3
                    "parent" (nlk:json-object "key" "PROJ-2"
                                                "fields" (nlk:json-object "summary" "Sign-in epic"))
                    "subtasks" (vector (nlk:json-object "key" (format nil "~a-1" key)
                                                           "fields" (nlk:json-object
                                                                      "summary" "Write the test"
                                                                      "status" (nlk:json-object "name" "To Do"))))
                    "description" (nlk:json-object
                                   "type" "doc"
                                   "content" (vector
                                              (nlk:json-object
                                               "type" "paragraph"
                                               "content" (vector (nlk:json-object "type" "text"
                                                                                   "text" "Log in with SSO and keep the session.")))
                                              (nlk:json-object
                                               "type" "bulletList"
                                               "content" (vector
                                                          (nlk:json-object
                                                           "type" "listItem"
                                                           "content" (vector
                                                                      (nlk:json-object
                                                                       "type" "paragraph"
                                                                       "content" (vector (nlk:json-object "type" "text"
                                                                                                           "text" "SAML first")))))))))))

(defun search-payload (&key next)
  (nlk:json-object "issues" (vector (nlk:json-object "id" "10012" "key" "PROJ-12"
                                                       "fields" (issue-fields "PROJ-12" "Log in with SSO"))
                                     (nlk:json-object "id" "10013" "key" "PROJ-13"
                                                       "fields" (issue-fields "PROJ-13" "Fix the redirect"
                                                                               :status "To Do")))
                  "nextPageToken" next))

(defun issue-payload ()
  (nlk:json-object "id" "10012" "key" "PROJ-12"
                    "fields" (let ((fields (issue-fields "PROJ-12" "Log in with SSO")))
                                (setf (gethash "attachment" fields)
                                      (vector (nlk:json-object "filename" "login.png"
                                                                "size" 2048
                                                                "mimeType" "image/png")))
                                (setf (gethash "components" fields)
                                      (vector (nlk:json-object "name" "auth")))
                                fields)))

(defun comments-payload ()
  (nlk:json-object "comments" (vector (nlk:json-object
                                         "author" (nlk:json-object "displayName" "Robin")
                                         "created" "2026-11-02T08:00:00.000+0000"
                                         "body" (nlk:json-object
                                                  "type" "doc"
                                                  "content" (vector (nlk:json-object
                                                                     "type" "paragraph"
                                                                     "content" (vector (nlk:json-object
                                                                                        "type" "text"
                                                                                        "text" "Please use the staging IdP.")))))))
                  "total" 1))

(defun transitions-payload ()
  (nlk:json-object
   "transitions" (vector (nlk:json-object "id" "31" "name" "Start progress"
                                            "to" (nlk:json-object "name" "In Progress"
                                                                   "statusCategory" (nlk:json-object "key" "indeterminate")))
                         (nlk:json-object "id" "41" "name" "Done"
                                          "to" (nlk:json-object "name" "Done"
                                                                 "statusCategory" (nlk:json-object "key" "done"))))))

(defun path-of (url)
  (quri:uri-path (quri:uri url)))

(defun query-of (url)
  (or (quri:uri-query (quri:uri url)) ""))

(defun header-of (url name)
  "NAME's value in the headers the cell sent with URL's request, or NIL."
  (let ((call (find url *calls* :test #'equal :key #'first)))
    (cdr (assoc name (second call) :test #'string-equal))))

(defun stub-wire (method url options)
  "NLK:HTTP, answering from the request's own path; nothing leaves the machine."
  (declare (ignore method))
  (push (list url (getf options :headers)) *calls*)
  (when *answer*
    (return-from stub-wire (values (first *answer*) (second *answer*) nil nil)))
  (let ((path (path-of url)))
    (cond ((equal path "/rest/api/3/field/search")
           (values (json-encoded
                    (nlk:json-object "values"
                                     (if *story-fields*
                                         (vector (nlk:json-object "id" "customfield_10016"
                                                                  "name" "Story Points"))
                                         #())))
                   200 nil nil))
          ((equal path "/rest/api/3/search/jql")
           (values (json-encoded (search-payload)) 200 nil nil))
          ((equal path "/rest/api/3/issue/PROJ-12/comment")
           (values (json-encoded (comments-payload)) 200 nil nil))
          ((equal path "/rest/api/3/issue/PROJ-12/transitions")
           (values (json-encoded (transitions-payload)) 200 nil nil))
          ((equal path "/rest/api/3/issue/PROJ-12")
           (values (json-encoded (issue-payload)) 200 nil nil))
          (t (values (json-encoded (nlk:json-object "errorMessages" (vector "not here")))
                     404 nil nil)))))

(defmacro with-jira-wire (&body body)
  "BODY with NLK:HTTP stubbed, and the section's fallbacks (the environment and
another tool's config file) silent, so the fixture's own values are the ones
in play."
  `(let ((*calls* '())
         (*answer* nil)
         (*story-fields* t))
     (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                (stub-wire method url options))
       (with-stubbed-fdefinition (nodecode-jira::environment-value (name) nil)
         (with-stubbed-fdefinition (nodecode-jira::jira-cli-config () nil)
           ,@body)))))

(defmacro with-jira-running (&body body)
  "BODY with the cell started on a literal section, stopped on unwind."
  `(with-cell-stop ((jira-start "base_url" +site+ "email" "dana.example" "token" "t-1"))
     ,@body))

(defun jira-called (tool arguments)
  "TOOL called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "jira-call" tool arguments)))

(defun search-request ()
  "The newest search request the cell sent, or NIL."
  (find "/rest/api/3/search/jql" *calls* :test #'equal :key (lambda (call) (path-of (first call)))))

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "jira"
  (:fixture with-jira-wire)
  (:config "base_url" +site+ "email" "dana.example" "token" "t-1")
  (:help :jira)
  (:command "jira")
  (:running
   (is (nle::find-tool "jira_assigned_issues") "the assigned-issues tool is registered")
   (is (nle::find-tool "jira_search_issues") "and the JQL search")
   (is (nle::find-tool "jira_issue_detail") "and the detail")
   (is (nle::find-tool "jira_issue_transitions") "and the transitions")
   (is (= 4 (count-if (lambda (entry) (eql 0 (search "jira_" (first entry))))
                       nle::*tools*)) "four of them, and no others"))
  (:stopped
   (is (null (nle::find-tool "jira_search_issues")) "stop takes every one back"))
  (:refused ("email" "dana.example" "token" "t-1")
            ("base_url" +site+ "token" "t-1")
            ("base_url" "http://example.atlassian.net" "email" "dana.example" "token" "t-1")
            ("base_url" +site+ "email" "dana.example")))

(deftest jira-cell-a-refusal-names-the-member-to-set (with-jira-wire)
  (let ((text (refusal-text nlk:config-refusal (jira-start "email" "dana.example"))))
    (is-carrying text "base_url" "JIRA_BASE_URL")
    (is (null (nle::find-tool "jira_search_issues")) "nothing was installed")))

;;; --- the wire ---------------------------------------------------------------------

(deftest jira-cell-assigned-issues-are-the-persons-open-tickets (with-jira-wire)
  (with-jira-running
    (let ((answer (jira-called "jira_assigned_issues" "{}")))
      (is (search "PROJ-12" answer) "the person's ticket is listed")
      (is (search "Log in with SSO" answer) "with its summary")
      (is (search "untrusted" answer) "under the untrusted warning")
      (is (search (quri:url-encode nodecode-jira::+assigned-jql+ :encoding :utf-8)
                  (query-of (first (search-request)))) "and the query is the original's assigned view"))))

(deftest jira-cell-a-search-sends-its-jql-page-and-fields (with-jira-wire)
  (with-jira-running
    (jira-called "jira_search_issues"
                  "{\"jql\": \"project = PROJ ORDER BY updated DESC\", \"max_results\": 5}")
    (let ((url (first (search-request))))
      (is (search "project%20%3D%20PROJ%20ORDER%20BY%20updated%20DESC" (query-of url)) "the JQL goes as the person wrote it")
      (is (search "maxResults=5" (query-of url)) "with the page size asked for")
      (is (search "customfield_10016" (query-of url)) "and the story points field the site turns out to have")
      (is (string= "Basic ZGFuYS5leGFtcGxlOnQtMQ==" (header-of url "authorization")) "the Basic credential is the account email and token, and nothing else"))))

(deftest jira-cell-paging-hands-the-token-back (with-jira-wire)
  (with-jira-running
    (let ((answer (jira-called "jira_search_issues"
                               "{\"jql\": \"ORDER BY created DESC\", \"next_page_token\": \"page-2\"}")))
      (is (search "nextPageToken=page-2" (query-of (first (search-request)))) "the token a page answered is sent back unchanged")
      (is (search "PROJ-13" answer) "and the page's own issues are listed"))))

(deftest jira-cell-a-search-refuses-a-missing-or-oversized-jql (with-jira-wire)
  (with-jira-running
    (let ((missing (jira-called "jira_search_issues" "{}"))
          (long (jira-called "jira_search_issues"
                              (format nil "{\"jql\": \"~a\"}" (make-string 4001 :initial-element #\a)))))
      (is (search "jql" missing) "a call without a JQL is refused, not guessed")
      (is (search "required" missing) "and says the argument is required")
      (is (search "the limit is 4000" long) "and a JQL past the bound is named")
      (is (null *calls*) "with no request sent"))))

(deftest jira-cell-an-issue-key-is-validated-before-it-is-a-url (with-jira-wire)
  (with-jira-running
    (let ((answer (jira-called "jira_issue_detail" "{\"issue_key\": \"../../admin\"}")))
      (is (search "ERROR: issue_key" answer) "the bad key is refused")
      (is (null *calls*) "before anything is sent"))))

(deftest jira-cell-detail-flattens-the-description-and-the-comments (with-jira-wire)
  (with-jira-running
    (let ((answer (jira-called "jira_issue_detail" "{\"issue_key\": \"proj-12\"}")))
      (is (search "PROJ-12" answer) "the ticket is named")
      (is (search "Log in with SSO and keep the session." answer) "its description is read out of the document format")
      (is (search "SAML first" answer) "a list inside it becomes text lines")
      (is (search "comments (1" answer) "the comments are there")
      (is (search "Please use the staging IdP." answer) "with what they say")
      (is (search "login.png (2048 bytes, image/png)" answer) "and the attachment names, not their bytes")
      (is (search "parent: PROJ-2" answer) "with the parent ticket")
      (is (search "PROJ-12-1" answer) "and the subtasks")
      (is (find "/rest/api/3/issue/PROJ-12" *calls* :test #'equal
                :key (lambda (call) (path-of (first call)))) "the key is upper-cased into the one request it names"))))

(deftest jira-cell-transitions-are-listed-and-none-applied (with-jira-wire)
  (with-jira-running
    (let ((answer (jira-called "jira_issue_transitions" "{\"issue_key\": \"PROJ-12\"}")))
      (is (search "31  Start progress -> In Progress (indeterminate)" answer) "each move is its id, name and target")
      (is (search "never applies one" answer) "with the read-only rule said")
      (is (= 1 (length *calls*)) "and one request, a GET at that"))))

(deftest jira-cell-a-site-without-story-points-still-answers (with-jira-wire)
  (with-jira-running
    (setf *story-fields* nil)
    (let ((answer (jira-called "jira_search_issues" "{\"jql\": \"project = PROJ\"}")))
      (is (search "PROJ-12" answer) "the search answers")
      (is (not (search "customfield_" (query-of (first (search-request))))) "without a story points field to ask for"))))

(deftest jira-cell-a-rejected-credential-says-what-to-do (with-jira-wire)
  (with-jira-running
    (setf *answer* (list (json-encoded (nlk:json-object "errorMessages" (vector "bad token")))
                         401))
    (let ((answer (jira-called "jira_assigned_issues" "{}")))
      (is (search "ERROR: Jira rejected these credentials" answer) "the call fails")
      (is (search "restart-cells" answer) "and says how to fix it"))))

;;; --- the operator's command ---------------------------------------------------------


(deftest jira-cell-the-command-says-how-it-is-set-up (with-jira-wire)
  (with-jira-running
    (let ((answer (nodecode-jira:run-command "" nil)))
      (is (search +site+ answer) "the site is named")
      (is (search "the token member of the jira section" answer) "and where the token came from")
      (is (not (search "t-1" answer)) "never the token itself")
      (is (null *calls*) "with no request to the site"))))

(deftest jira-cell-the-command-reads-a-key-or-a-query (with-jira-wire)
  (with-jira-running
    (let ((detail (nodecode-jira:run-command "PROJ-12" nil)))
      (is (search "Log in with SSO and keep the session." detail) "an issue key shows that ticket"))
    (setf *calls* nil)
    (nodecode-jira:run-command "assignee = currentUser()" nil)
    (is (search (quri:url-encode "assignee = currentUser()" :encoding :utf-8)
                (query-of (first (search-request)))) "and any other text runs as JQL")))
