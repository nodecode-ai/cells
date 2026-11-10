;;;; api.lisp --- DevTune's REST API: the report table, the one seam, the text.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original names what a DevTune project can be asked one line at a
;;;; time, twice: as an MCP tool served by devtune.ai, and as the REST path
;;;; behind it. This file is that table as data -- the same reads, the same
;;;; paths, the same parameters and the same windows -- made with the image's
;;;; own HTTP call, and answered the way DevTune answered: its JSON, as it
;;;; sent it, so nothing here rounds a number or renames a member the two
;;;; skills tell a model how to read.
;;;;
;;;; Every read is a GET under /projects/{projectId}, with the project API
;;;; key as a bearer header, exactly as the original's mcp.json sent it. The
;;;; project id is the section's own when it names one, else whatever
;;;; GET /connection says the key reaches, resolved once per start.
;;;;
;;;; Nothing here touches a file, a process or the environment: the only way
;;;; out is HTTP, the one seam a test stubs.

(in-package #:nodecode-devtune)

(defparameter +default-base-url+ "https://devtune.ai/api/v2"
  "The REST root the two skills name, used when the section leaves base_url empty.")

(defparameter +body-limit+ 40000
  "How much of one answer a read hands the model before it says how much it cut.")

(defparameter +error-limit+ 1000
  "How much of a refused answer's body the refusal quotes.")

;;; --- the reports ----------------------------------------------------------------
;;; A row is what the original's two tables say about one read: :KEY the name a
;;; caller asks for, :PATH where it sits under the project (or, without
;;; :PROJECT, under the API root), :WINDOW a measurement read that takes
;;; windowDays, :SETUP an Agent Picks read that takes a setup, :ID where one
;;; read is about a single thing -- :PATH in the path as {id}, a string as that
;;; query parameter -- and :ID-DOC what that id is, in the words a model needs
;;; to find one in a listing.

(defparameter *reports*
  (list
   (list :key :connection :path "/connection" :project nil)
   (list :key :visibility-summary :path "/visibility/summary" :window t)
   (list :key :visibility-diff :path "/visibility/diff" :window t)
   (list :key :competitive-position :path "/competitive/position")
   (list :key :citation-domains :path "/citations/top-domains")
   (list :key :citation-pages :path "/citations/top-pages")
   (list :key :visibility-responses :path "/visibility/responses" :window t)
   (list :key :visibility-response :path "/visibility/responses/{id}" :id :path
         :id-doc "the executionId of one answer, as visibility-responses lists it")
   (list :key :content-gaps :path "/content-gaps/list")
   (list :key :content-gap-evidence :path "/content-gaps/evidence" :id "gapId"
         :id-doc "the gap's id, as content-gaps lists it")
   (list :key :traffic-summary :path "/traffic/summary" :window t)
   (list :key :traffic-platforms :path "/traffic/platforms" :window t)
   (list :key :ai-referrals :path "/traffic/referrals" :window t)
   (list :key :404-demand :path "/traffic/404-demand" :window t)
   (list :key :actions :path "/actions/list")
   (list :key :action-brief :path "/actions/{id}/brief" :id :path
         :id-doc "the action's id, as actions lists it")
   (list :key :audit-summary :path "/audit/summary")
   (list :key :audit-findings :path "/audit/findings")
   (list :key :outcomes-ledger :path "/outcomes/ledger")
   (list :key :agent-picks-summary :path "/agent-picks/summary" :setup t)
   (list :key :agent-picks-cells :path "/agent-picks/cells" :setup t)
   (list :key :agent-picks-history :path "/agent-picks/history" :setup t)
   (list :key :agent-picks-runs :path "/agent-picks/runs" :setup t)
   (list :key :agent-picks-run-trace :path "/agent-picks/runs/{id}/trace" :id :path
         :id-doc "the run's id, as agent-picks-runs lists it")
   (list :key :agent-picks-actions :path "/agent-picks/actions" :setup t)
   (list :key :agent-picks-test :path "/agent-picks/test" :id "test_id" :setup t
         :id-doc "the test's id, as agent-picks-history lists it")
   (list :key :agent-picks-schedule :path "/agent-picks/schedule" :setup t)
   (list :key :agent-picks-catalogue :path "/setup/catalogue/agent-picks"))
  "Every read this cell makes, one row each: the original's two tables, as data.")

(defparameter +report-options+
  '("connection" "visibility-summary" "visibility-diff" "competitive-position"
    "citation-domains" "citation-pages" "visibility-responses" "visibility-response"
    "content-gaps" "content-gap-evidence" "traffic-summary" "traffic-platforms"
    "ai-referrals" "404-demand" "actions" "action-brief" "audit-summary"
    "audit-findings" "outcomes-ledger" "agent-picks-summary" "agent-picks-cells"
    "agent-picks-history" "agent-picks-runs" "agent-picks-run-trace"
    "agent-picks-actions" "agent-picks-test" "agent-picks-schedule"
    "agent-picks-catalogue")
  "The same names as the tool advertises them; a test holds the two lists equal.")

(defun reports ()
  "The report names this cell reads, in the order the manual lists them."
  (mapcar (lambda (row) (getf row :key)) *reports*))

(defun report-key (name)
  "NAME -- a keyword, a symbol or a string, in either case, with a dash or an
underscore -- as the keyword *REPORTS* is keyed by."
  (intern (substitute #\- #\_ (string-upcase (string name))) :keyword))

(defun find-report (name)
  "NAME's row, or a refusal naming every report there is."
  (or (find (report-key name) *reports* :key (lambda (row) (getf row :key)) :test #'eq)
      (fail "no report ~(~a~): this cell reads ~{~(~a~)~^, ~}" name (reports))))

;;; --- small words ------------------------------------------------------------------

(defun nonblank (value)
  "VALUE as a trimmed non-empty string, or NIL."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

(defun clipped (text)
  "TEXT at most +ERROR-LIMIT+ characters, with a note when it was cut."
  (let ((text (or text "")))
    (if (<= (length text) +error-limit+)
        text
        (format nil "~a [~d more characters]" (subseq text 0 +error-limit+)
                (- (length text) +error-limit+)))))

(defun clipped-body (text)
  "TEXT at most +BODY-LIMIT+ characters, with a note when it was cut."
  (let ((text (or text "")))
    (if (<= (length text) +body-limit+)
        text
        (format nil "~a~%[and ~d more characters]" (subseq text 0 +body-limit+)
                (- (length text) +body-limit+)))))

(defun url-encode (text)
  "TEXT as one path segment or query value."
  (quri:url-encode (princ-to-string text) :encoding :utf-8))

(defun base-url ()
  "The API root the section names, without its trailing slashes."
  (string-right-trim "/" (or (nonblank (setting :base-url)) +default-base-url+)))

;;; --- the address a read asks ------------------------------------------------------

(defun with-id (path id)
  "PATH with its {id} segment replaced by ID, already encoded."
  (let ((at (search "{id}" path)))
    (unless at (fail "the report table puts no {id} in ~a" path))
    (concatenate 'string (subseq path 0 at) id (subseq path (+ at 4)))))

(defun query-string (pairs)
  "PAIRS, a list of (NAME . VALUE), as a query string; a NIL value is left out."
  (format nil "~{~a~^&~}"
          (loop for (name . value) in pairs
                when value collect (format nil "~a=~a" name (url-encode value)))))

(defun report-url (row &key project window cursor setup id)
  "ROW's address on the API, with the parameters this call carries."
  (let* ((id-name (getf row :id))
         (path (getf row :path))
         (path (if (eq id-name :path) (with-id path (url-encode id)) path))
         (path (if (getf row :project t)
                   (format nil "/projects/~a~a" (url-encode project) path)
                   path))
         (query (query-string
                 (append (when (and (eq (getf row :window) t) window)
                           (list (cons "windowDays" (princ-to-string window))))
                         (when (and (eq (getf row :setup) t) setup)
                           (list (cons "setup" setup)))
                         (when cursor (list (cons "cursor" cursor)))
                         (when (and (stringp id-name) id)
                           (list (cons id-name id)))))))
    (format nil "~a~a~@[?~a~]" (base-url) path (and (plusp (length query)) query))))

;;; --- the one seam ------------------------------------------------------------------

(defun http (url &key headers timeout)
  "The cell's one network call; a test stubs this name. => (values BODY STATUS),
every status a value."
  (nlk:http :get url :headers headers :timeout (or timeout 30)))

(defun ask (url key &key timeout)
  "DevTune's answer to URL, with the project API key in the bearer header the
original's mcp.json sent. => (values BODY STATUS)."
  (http url
        :headers (list (cons "accept" "application/json")
                       (cons "authorization" (format nil "Bearer ~a" key)))
        :timeout timeout))

(defun answer (body status what)
  "BODY when DevTune answered 2xx; otherwise the refusal its status is, carrying
DevTune's own sentence and what the two skills say a refusal means."
  (let ((text (or body "")))
    (cond ((< status 400) text)
          ((<= status 499)
           (fail "DevTune answered HTTP ~d for ~a: ~a~%A 403 or a refused read means the key lacks a scope, or the account a feature: read (devtune:report :connection) and tell the operator which one.~@[~%Several Agent Picks setups exist: call again with setup set to one of the ids in details.setups.~]"
                 status what (clipped text)
                 (cl:search "agent_picks_setup_required" text)))
          (t (fail "DevTune answered HTTP ~d for ~a: ~a" status what (clipped text))))))

(defun decode (text)
  "TEXT as the JSON object DevTune answers, or a refusal quoting it."
  (let ((payload (ignore-errors (nlk:decode-json text))))
    (if (hash-table-p payload)
        payload
        (fail "DevTune answered something that is not a JSON object: ~a" (clipped text)))))

;;; --- which project a read is about --------------------------------------------------

(defvar *project-id* nil
  "The project id GET /connection answered for the running key, or NIL: resolved
on the first read, cleared by the cell's start and stop.")

(defun project-id-of (payload)
  "The project id PAYLOAD carries, under any spelling the connection answer has
used, or NIL."
  (flet ((text (object) (nonblank (nlk:json-value object :string "id"))))
    (or (nonblank (nlk:json-value payload :string "projectId"))
        (nonblank (nlk:json-value payload :string "project_id"))
        (let ((project (nlk:json-value payload :object "project")))
          (and project (or (text project)
                           (nonblank (nlk:json-value project :string "projectId")))))
        (let* ((projects (nlk:json-value payload :array "projects"))
               (first (and projects (plusp (length projects)) (aref projects 0))))
          (and first (or (text first)
                         (nonblank (nlk:json-value first :string "projectId")))))
        (text payload))))

(defun connection-payload ()
  "GET /connection: the project the key reaches, and whether it is paused."
  (multiple-value-bind (body status)
      (ask (format nil "~a/connection" (base-url)) (setting :api-key)
           :timeout (setting :timeout-seconds))
    (decode (answer body status "(devtune:report :connection)"))))

(defun project ()
  "The project a read asks about: the section's project_id, else the one the
key's connection names."
  (or (nonblank (setting :project-id))
      *project-id*
      (setf *project-id*
            (or (project-id-of (connection-payload))
                (fail "the connection answer names no project id: set project_id in the devtune section, or use a key that reaches a project")))))

;;; --- what a read answers ----------------------------------------------------------

(defun report-text (row body &key project window cursor setup id)
  "What a read answers: one line naming the report and what it asked, then the
JSON DevTune returned, as it returned it."
  (format nil "devtune: ~(~a~)~@[ in project ~a~]~@[ (windowDays ~d)~]~@[ (cursor ~a)~]~@[ (setup ~a)~]~@[ (id ~a)~]~2%~a"
          (getf row :key) project window cursor setup id (clipped-body body)))

;;; --- the verb ------------------------------------------------------------------

(define-verb report (name &key window-days cursor setup id)
  "Read one DevTune report and answer it as DevTune sent it. NAME is a report
this cell lists (devtune:reports): :connection, :visibility-summary,
:competitive-position, :citation-domains, :traffic-summary, :actions or
:action-brief, :outcomes-ledger, :agent-picks-summary, and so on.
WINDOW-DAYS is 30 or 90 for a measurement read and 7, 30, 90 or 365 for a
results one; the section's window_days answers when this names none. CURSOR
is the pagination.nextCursor of the page before. SETUP picks one Agent Picks
setup by id or category slug. ID is what a single-thing report is about: an
executionId, an actionId, a gapId, a run id or a test id, as a listing gives
it. => the JSON DevTune returned, under one line naming the read."
  (let* ((row (find-report name))
         (key (getf row :key))
         (cursor (nonblank cursor))
         (setup (nonblank setup))
         (id (nonblank id))
         (window (and (eq (getf row :window) t) (or window-days (setting :window-days)))))
    (when (and (eq (getf row :id) :path) (null id))
      (fail "the ~(~a~) report needs an id: ~a" key (getf row :id-doc)))
    (let* ((project (and (getf row :project t) (project)))
           (url (report-url row :project project :window window :cursor cursor
                                :setup setup :id id)))
      (multiple-value-bind (body status)
          (ask url (setting :api-key) :timeout (setting :timeout-seconds))
        (report-text row (answer body status (format nil "(devtune:report :~(~a~))" key))
                     :project project :window window :cursor cursor :setup setup :id id)))))

;;; --- what /devtune with no report answers ------------------------------------------

(defun connection-line ()
  "The project the key reaches, whether it is paused, and the reports it can
read: what /devtune with no report answers."
  (let* ((payload (connection-payload))
         (id (project-id-of payload))
         (paused (nlk:json-value payload :boolean "paused")))
    (setf *project-id* (or id *project-id*))
    (format nil "devtune: connected to project ~a~:[~; (paused: readable, not changeable, until someone resumes it in DevTune)~]~2%Reports: ~{~(~a~)~^, ~}~2%Type /devtune REPORT to read one, e.g. /devtune visibility-summary."
            (or id "unknown") paused (reports))))

;;; --- the tool the model calls -----------------------------------------------------

(nlk:define-contract devtune-read-request
  (:doc "Read one DevTune report: what AI answer engines say about a product and
who they cite instead, AI crawler and referral traffic, what to fix next, whether
a shipped fix moved the numbers, and which tools coding agents pick when asked to
add a capability. Reads only: nothing is created, changed or spent.")
  ("report" :text :required t :options +report-options+
   :doc "the report to read; devtune:reports lists them, and each one is a read of its own")
  ("window_days" :integer :slot window-days
   :doc "30 or 90 for a measurement read (7, 30, 90 or 365 for a results one); the section's window_days answers when this is absent")
  ("cursor" :text
   :doc "the pagination.nextCursor of the page before, passed back with every other parameter unchanged")
  ("setup" :text
   :doc "one Agent Picks setup, by id or category slug, when a project has several")
  ("id" :text
   :doc "the id a single-thing report is about: an executionId, an actionId, a gapId, a run id or a test id, as a listing gives it"))

(defun read-tool (request)
  "The devtune_read tool: the members one call carries, answered as the verb
answers them."
  (report (devtune-read-request-report request)
          :window-days (devtune-read-request-window-days request)
          :cursor (devtune-read-request-cursor request)
          :setup (devtune-read-request-setup request)
          :id (devtune-read-request-id request)))
