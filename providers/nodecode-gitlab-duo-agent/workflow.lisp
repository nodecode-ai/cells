;;;; workflow.lisp --- the gitlab-duo-agent lane: a Duo workflow run as one round.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): ai/src/providers/gitlab-duo-workflow.ts
;;;; (the REST setup, the socket loop, the checkpoint fold, the restarts, the
;;;; session kept across a tool call) and catalog/src/discovery/
;;;; gitlab-duo-workflow.ts (which namespace a workflow runs in, which models
;;;; it offers).
;;;;
;;;; One round, as the lane contract has it (NLE::DEFINE-PROVIDER-LANE): the
;;;; context goes in, (values MESSAGE USAGE FINISH-REASON REQUEST-JSON) comes
;;;; out, every part streamed to ON-PART on the way. For a fresh round that is:
;;;;
;;;;   the namespace   the configured one, else the configured project's root,
;;;;                   else the root of the session directory's GitLab remote,
;;;;                   else the first top-level group (Duo-enabled first);
;;;;                   kept per account and directory, found again once when
;;;;                   it stops working
;;;;   its settings    the agent platform, MCP and experiment flags turned on
;;;;                   once per account (best effort: it needs a maintainer)
;;;;   a project       the configured one, the remote's, else the latest one
;;;;                   the token can develop in: the ambient flow needs one
;;;;   direct access   a workflow token, and the service host when GitLab
;;;;                   hands the socket to the Duo Workflow Service directly
;;;;   a workflow      created empty; its goal rides the socket
;;;;   the models      the namespace's pinned model outranks the picker's
;;;;   the socket      the startRequest, then frames until a tool call (the
;;;;                   socket is kept for the tool's result), the end, or a
;;;;                   failure
;;;;
;;;; A round that ended on a tool call left its workflow waiting on the
;;;; socket: the next round, finding that call's result in its history,
;;;; answers it there (actionResponse) and goes on reading. A user message
;;;; after the result, or a result that never came, abandons the workflow for
;;;; a fresh one whose goal carries the whole conversation.
;;;;
;;;; Restarts on a fresh workflow, each bounded as omp bounds it: the socket
;;;; silent for the idle window (once), the service's step limit (4), a
;;;; workflow that stopped advancing (2), the service's catch-all failure (1).
;;;; An approval the service asks for is granted on a new socket.

(in-package #:nodecode-gitlab-duo-agent)

(defparameter *idle-seconds* 90
  "How long the socket may say nothing before the round gives up on it.")

(defparameter +rest-seconds+ 30
  "The connect and read deadline of one REST setup call.")

(defparameter +max-step-limit-restarts+ 4)
(defparameter +max-generic-retries+ 1)
(defparameter +max-stall-restarts+ 2)
(defparameter +max-attempts+ 12)

(defparameter +stall-message+
  "GitLab Duo Agent stopped making progress (the workflow's visible history did not advance after multiple restarts).")

;;; --- REST -------------------------------------------------------------------------

(defun body-string (body)
  "BODY, as dexador answered it, as a string."
  (typecase body
    (string body)
    ((vector (unsigned-byte 8)) (sb-ext:octets-to-string body :external-format :utf-8))
    (null "")
    (t (princ-to-string body))))

(defun gitlab (method url token &key body)
  "(values JSON STATUS HEADERS TEXT) of one GitLab request with TOKEN; STATUS
is NIL when the request never got an answer."
  (handler-case
      (multiple-value-bind (raw status headers)
          (dex:request url :method method
                           :headers `(("Authorization" . ,(format nil "Bearer ~a" token))
                                      ("Content-Type" . "application/json")
                                      ("Accept" . "application/json"))
                           :content (and body (nlk:encode-json-object body))
                           :connect-timeout +rest-seconds+ :read-timeout +rest-seconds+)
        (let ((text (body-string raw)))
          (values (and (plusp (length text)) (ignore-errors (nlk:decode-json text))) status headers text)))
    (dex:http-request-failed (condition)
      (let ((text (body-string (dex:response-body condition))))
        (values (and (plusp (length text)) (ignore-errors (nlk:decode-json text)))
                (dex:response-status condition) (dex:response-headers condition) text)))
    (error (condition)
      (values nil nil nil (princ-to-string condition)))))

(defun ok-p (status)
  (and (integerp status) (<= 200 status 299)))

(defun api (base path)
  "PATH under the instance BASE, its install path kept."
  (concatenate 'string (string-right-trim "/" base) path))

(defun graphql (base token query variables)
  "The data of one GraphQL QUERY, or NIL."
  (multiple-value-bind (json status) (gitlab :post (api base "/api/graphql") token
                                             :body (nlk:json-object "query" query "variables" variables))
    (and (ok-p status) json)))

(defun text-id (value)
  "VALUE, a string or a number, as a trimmed non-empty id, else NIL."
  (let ((text (typecase value
                (string (nlk:trimmed value))
                (number (princ-to-string value)))))
    (and text (plusp (length text)) text)))

;;; --- which namespace ----------------------------------------------------------------

(defun explicit-root (value)
  "The root namespace VALUE, a project or a namespace, names explicitly, or NIL."
  (when (hash-table-p value)
    (or (text-id (gethash "root_namespace_id" value))
        (text-id (gethash "rootNamespaceId" value))
        (let ((root (or (record value "root_namespace") (record value "rootNamespace")
                        (record value "root_ancestor") (record value "rootAncestor"))))
          (if root
              (or (text-id (gethash "id" root)) (text-id (gethash "full_path" root)) (text-id (gethash "fullPath" root)))
              (let ((namespace (record value "namespace")))
                (and namespace (explicit-root namespace))))))))

(defun root-id (value)
  "The root namespace a group or project VALUE belongs to, its own id the last resort."
  (when (hash-table-p value)
    (or (text-id (gethash "root_namespace_id" value))
        (text-id (gethash "rootNamespaceId" value))
        (let ((root (or (record value "root_namespace") (record value "rootNamespace")
                        (record value "root_ancestor") (record value "rootAncestor"))))
          (and root (or (text-id (gethash "id" root)) (text-id (gethash "full_path" root))
                        (text-id (gethash "fullPath" root)))))
        (let ((namespace (record value "namespace")))
          (and namespace (or (root-id namespace) (text-id (gethash "id" namespace))
                             (text-id (gethash "full_path" namespace)) (text-id (gethash "fullPath" namespace)))))
        (text-id (gethash "id" value)) (text-id (gethash "full_path" value)) (text-id (gethash "fullPath" value)))))

(defun namespace-path (value)
  (and (hash-table-p value)
       (or (text-id (gethash "full_path" value)) (text-id (gethash "fullPath" value)) (text-id (gethash "path" value)))))

(defun override-candidate (base token namespace-id)
  "The configured NAMESPACE-ID as a candidate, its root and path read from the group when it reads."
  (let ((rest-id (or (ppcre:register-groups-bind (n) ("^gid://gitlab/(?:Group|Namespace)/(\\d+)$" namespace-id) n)
                     (and (every #'digit-char-p namespace-id) namespace-id))))
    (or (and rest-id
             (multiple-value-bind (json status) (gitlab :get (api base (format nil "/api/v4/groups/~a" (path-segment rest-id))) token)
               (and (ok-p status)
                    (list :root (or (root-id json) namespace-id) :path (namespace-path json) :source :override))))
        (list :root namespace-id :source :override))))

(defun project-root (base token project)
  "The root namespace PROJECT, an id or a path, belongs to: from its REST
record, else GraphQL's rootAncestor by its full path."
  (multiple-value-bind (json status) (gitlab :get (api base (format nil "/api/v4/projects/~a" (path-segment project))) token)
    (let* ((rest (and (ok-p status) json))
           (root (explicit-root rest))
           (full-path (or (and rest (or (text-id (gethash "path_with_namespace" rest)) (text-id (gethash "fullPath" rest))))
                          (and (find #\/ project) project))))
      (or root
          (and full-path
               (explicit-root
                (record (record (graphql base token
                                         "query nodecode_gitlabDuoWorkflowProjectRootNamespace($fullPath: ID!) {
  project(fullPath: $fullPath) {
    namespace {
      id
      rootAncestor { id }
    }
  }
}"
                                         (nlk:json-object "fullPath" full-path))
                                "data")
                        "project")))))))

(defun git-config-text (directory)
  "The .git/config of the repository DIRECTORY is in, walking up; a linked
worktree's remotes are its common directory's."
  (loop for dir = (uiop:ensure-directory-pathname directory) then (uiop:pathname-parent-directory-pathname dir)
        for dot-git = (merge-pathnames ".git" (uiop:ensure-directory-pathname dir))
        do (let ((config (probe-file (merge-pathnames "config" (uiop:ensure-directory-pathname dot-git)))))
             (cond ((and config (not (uiop:directory-pathname-p config)) (uiop:directory-exists-p dot-git))
                    (return (ignore-errors (uiop:read-file-string config))))
                   ((uiop:file-exists-p dot-git)
                    (let* ((pointer (ignore-errors (uiop:read-file-string dot-git)))
                           (git-dir (and pointer (ppcre:register-groups-bind (dir) ("(?im)^gitdir:\\s*(.+?)\\s*$" pointer) dir))))
                      (when git-dir
                        (let* ((git-dir (uiop:ensure-directory-pathname
                                         (if (uiop:absolute-pathname-p git-dir) git-dir (merge-pathnames git-dir dir))))
                               (common (ignore-errors (nlk:trimmed (uiop:read-file-string (merge-pathnames "commondir" git-dir))))))
                          (return (or (and common
                                           (ignore-errors
                                            (uiop:read-file-string
                                             (merge-pathnames "config" (uiop:ensure-directory-pathname
                                                                        (if (uiop:absolute-pathname-p common)
                                                                            common
                                                                            (merge-pathnames common git-dir)))))))
                                      (ignore-errors (uiop:read-file-string (merge-pathnames "config" git-dir)))))))))))
        until (equal dir (uiop:pathname-parent-directory-pathname dir))))

(defun remote-urls (config-text)
  "The url of every [remote \"...\"] section of CONFIG-TEXT."
  (let ((in-remote nil) (urls '()))
    (dolist (line (uiop:split-string config-text :separator '(#\Newline)) (nreverse urls))
      (let ((section (ppcre:register-groups-bind (name) ("^\\s*\\[([^\\]]+)\\]" line) name)))
        (cond (section (setf in-remote (ppcre:scan "^remote\\s+\"[^\"]+\"$" (nlk:trimmed section))))
              (in-remote (let ((url (ppcre:register-groups-bind (url) ("^\\s*url\\s*=\\s*(.+?)\\s*$" line) url)))
                           (when url (push url urls)))))))))

(defun remote-project-path (url base)
  "The project path of the remote URL when it is on BASE's host, else NIL."
  (multiple-value-bind (host path port-free)
      (if (ppcre:scan "^[a-zA-Z][a-zA-Z0-9+.-]*://" url)
          (let ((uri (quri:uri url)))
            (values (format nil "~a~@[:~a~]" (quri:uri-host uri)
                            (and (quri:uri-port uri)
                                 (not (eql (quri:uri-port uri) (quri.port:scheme-default-port (quri:uri-scheme uri))))
                                 (quri:uri-port uri)))
                    (quri:uri-path uri)
                    (string-equal (quri:uri-scheme uri) "ssh")))
          (ppcre:register-groups-bind (host path) ("^(?:[^@]+@)?([^:]+):(.+)$" url)
            (values host path t)))
    (when (and host path)
      (let* ((base-uri (quri:uri base))
             (base-host (format nil "~a~@[:~a~]" (quri:uri-host base-uri)
                                (and (quri:uri-port base-uri)
                                     (not (eql (quri:uri-port base-uri)
                                               (quri.port:scheme-default-port (quri:uri-scheme base-uri))))
                                     (quri:uri-port base-uri))))
             (base-path (string-trim "/" (or (quri:uri-path base-uri) "")))
             (same-host (if port-free
                            (string-equal (subseq host 0 (position #\: host))
                                          (subseq base-host 0 (position #\: base-host)))
                            (string-equal host base-host))))
        (when same-host
          (let ((project (string-left-trim "/" path)))
            (when (and (plusp (length base-path))
                       (or (string= project base-path) (uiop:string-prefix-p (format nil "~a/" base-path) project)))
              (setf project (subseq project (length base-path))))
            (setf project (string-trim "/" project))
            (when (and (> (length project) 4) (string-equal ".git" (subseq project (- (length project) 4))))
              (setf project (subseq project 0 (- (length project) 4))))
            (and (find #\/ project) project)))))))

(defun group-candidates (base token)
  "Every top-level group the token belongs to, Duo-enabled ones first, the
pages GitLab names followed (50 at most)."
  (let ((found '()) (page "1"))
    (loop repeat 50
          while page
          do (multiple-value-bind (json status headers)
                 (gitlab :get (api base (format nil "/api/v4/groups?top_level_only=true&per_page=100&order_by=name&sort=asc&page=~a"
                                                (form-encode page)))
                         token)
               (unless (and (ok-p status) (vectorp json) (not (stringp json))) (return))
               (loop for group across json
                     for root = (root-id group)
                     when root
                       do (push (list :root root :path (namespace-path group) :source :group
                                      :preferred (or (eq t (gethash "duo_features_enabled" group))
                                                     (eq t (gethash "duo_core_features_enabled" group))))
                                found))
               (setf page (let ((next (and (hash-table-p headers) (gethash "x-next-page" headers))))
                            (and (stringp next) (plusp (length (nlk:trimmed next))) (nlk:trimmed next))))))
    (stable-sort (nreverse found) (lambda (a b) (and (getf a :preferred) (not (getf b :preferred)))))))

(defun select-namespace (base token resolve &key namespace-id project cwd enrich)
  "The first candidate RESOLVE accepts, in omp's order: the configured
namespace (read from GitLab when ENRICH), the configured project's root, the
session directory's remote's root, the top-level groups. NIL when none."
  (or (and namespace-id
           (funcall resolve (if enrich
                                (override-candidate base token namespace-id)
                                (list :root namespace-id :source :override))))
      (and project
           (let ((root (project-root base token project)))
             (and root (funcall resolve (list :root root :project-path (and (find #\/ project) project)
                                              :source :project)))))
      (let ((remote (and cwd (some (lambda (url) (remote-project-path url base))
                                   (remote-urls (or (git-config-text cwd) ""))))))
        (and remote
             (let ((root (project-root base token remote)))
               (and root (funcall resolve (list :root root :project-path remote :source :remote))))))
      (some resolve (group-candidates base token))))

;;; --- the models a namespace offers ----------------------------------------------------

(defun available-models (base token root)
  "The namespace ROOT's aiChatAvailableModels, or NIL."
  (record (record (graphql base token +available-models-query+
                           (nlk:json-object "rootNamespaceId" (graphql-namespace-id root)))
                  "data")
          "aiChatAvailableModels"))

(defun model-refs (available)
  "((REF . NAME) ...) AVAILABLE offers: the pinned model alone, else the
selectable ones, else the default."
  (flet ((ref (value)
           (let ((ref (text-id (nlk:json-value value :any "ref"))))
             (and ref (cons ref (or (text-id (nlk:json-value value :any "name")) ref))))))
    (let ((pinned (ref (record available "pinnedModel")))
          (selectable (remove nil (map 'list #'ref (nlk:json-array available "selectableModels")))))
      (cond (pinned (list pinned))
            (selectable selectable)
            (t (let ((default (ref (record available "defaultModel")))) (and default (list default))))))))

(defun discover-models (base token &key namespace-id project cwd)
  "The models the first namespace with any offers: omp's model discovery."
  (select-namespace base token
                    (lambda (candidate)
                      (let ((refs (model-refs (available-models base token (getf candidate :root)))))
                        (and refs refs)))
                    :namespace-id namespace-id :project project :cwd cwd))

;;; --- the account's state ---------------------------------------------------------------

(defvar *accounts* (make-hash-table :test 'equal :synchronized t)
  "Account key -> (:selection PLIST :settings-p BOOL): the namespace found and
the settings ensured, per token, instance and session directory.")

(defun account-key (token base cwd)
  (format nil "~a ~a ~a" (nlk:short-digest token 16) base (or cwd "")))

(defun account (token base cwd)
  (alexandria:ensure-gethash (account-key token base cwd) *accounts* (list :selection nil :settings-p nil)))

(defun ensure-settings (base token rest-namespace)
  "Turn on the group's agent platform, MCP and experiment flags; => whether
the answer was definitive (anything but a network failure or a 5xx)."
  (multiple-value-bind (json status)
      (gitlab :put (api base (format nil "/api/v4/groups/~a" (path-segment rest-namespace))) token :body (settings-body))
    (declare (ignore json))
    (and (integerp status) (< status 500))))

(defun discover-project (base token rest-namespace)
  "(:ID :PATH) of the latest project in REST-NAMESPACE, else of any, the
token can develop in; NIL when there is none."
  (dolist (path (list (format nil "/api/v4/groups/~a/projects?include_subgroups=true&per_page=1&min_access_level=30&order_by=last_activity_at&sort=desc"
                              (path-segment rest-namespace))
                      "/api/v4/projects?membership=true&per_page=1&min_access_level=30&order_by=last_activity_at&sort=desc"))
    (multiple-value-bind (json status) (gitlab :get (api base path) token)
      (let* ((first (and (ok-p status) (vectorp json) (not (stringp json)) (plusp (length json)) (aref json 0)))
             (id (and first (text-id (gethash "id" first))))
             (project-path (and first (text-id (gethash "path_with_namespace" first)))))
        (when (and id project-path)
          (return (list :id id :path project-path)))))))

(defun numeric-project-id (base token project-path)
  "The numeric id of PROJECT-PATH, or NIL."
  (multiple-value-bind (json status) (gitlab :get (api base (format nil "/api/v4/projects/~a" (path-segment project-path))) token)
    (and (ok-p status) (text-id (nlk:json-value json :any "id")))))

(defun refusal (what status json text)
  "A provider error for the setup call WHAT that GitLab refused with STATUS."
  (let ((message (or (nlk:json-value json :text "message") (nlk:json-value json :text "error"))))
    (make-condition 'nle::provider-error
                    :status status :scope :request
                    :detail (cond ((null status) (format nil "GitLab Duo Workflow ~a failed: ~a" what text))
                                  (message (format nil "GitLab Duo Workflow ~a failed with HTTP ~a: ~a" what status message))
                                  (t (format nil "GitLab Duo Workflow ~a failed with HTTP ~a" what status))))))

(defun direct-access (base token root project definition)
  "(:TOKEN :BASE :HEADERS :SERVICE-P): the workflow's credential, and the
Duo Workflow Service host when GitLab hands the socket to it."
  (multiple-value-bind (json status headers text)
      (gitlab :post (api base "/api/v4/ai/duo_workflows/direct_access") token
              :body (direct-access-body root project :definition definition))
    (declare (ignore headers))
    (unless (ok-p status) (error (refusal "direct_access" status json text)))
    (let* ((rails (record json "gitlab_rails"))
           (service (record json "duo_workflow_service"))
           (workflow-token (or (nlk:json-value rails :text "token") (nlk:json-value service :text "token")
                               (nlk:json-value json :text "duo_workflow_access_token")
                               (nlk:json-value json :text "workflow_token") (nlk:json-value json :text "token")
                               (nlk:json-value json :text "access_token") (nlk:json-value json :text "jwt")))
           (service-base (nlk:json-value service :text "base_url"))
           (service-p (and (null (nlk:json-value rails :text "token")) service-base t)))
      (unless workflow-token
        (error 'nle::provider-error :scope :request
                                    :detail "GitLab Duo Workflow direct_access did not return credentials"))
      (list :token workflow-token
            :base (and service-p (string-right-trim "/" (if (ppcre:scan "(?i)^https?://" service-base)
                                                            service-base
                                                            (concatenate 'string "https://" service-base))))
            :headers (and service-p (loop for name being the hash-keys of (or (record service "headers")
                                                                              (make-hash-table :test 'equal))
                                            using (hash-value value)
                                          when (stringp value) collect (cons name value)))
            :service-p service-p))))

(defun create-workflow (base token namespace project definition)
  "The id of a new, empty workflow."
  (multiple-value-bind (json status headers text)
      (gitlab :post (api base "/api/v4/ai/duo_workflows/workflows") token
              :body (create-body namespace project :definition definition))
    (declare (ignore headers))
    (unless (ok-p status) (error (refusal "create" status nil text)))
    (or (text-id (or (gethash "id" json) (gethash "workflow_id" json) (gethash "workflowId" json)))
        (error 'nle::provider-error :scope :request
                                    :detail (format nil "GitLab Duo Workflow create response missing workflow id (HTTP ~a)" status)))))

(defun stop-workflow (base token workflow-id)
  "Ask GitLab to stop WORKFLOW-ID; never signals."
  (ignore-errors
   (gitlab :patch (api base (format nil "/api/v4/ai/duo_workflows/workflows/~a" (path-segment workflow-id))) token
           :body (nlk:json-object "status_event" "stop"))))

;;; --- the socket --------------------------------------------------------------------------

(defstruct (socket (:constructor make-socket (sender closer)))
  "One workflow socket: SENDER takes a text frame, CLOSER ends it, and every
event arrives in MAILBOX as (:message TEXT), (:close CODE REASON) or (:error TEXT)."
  sender closer
  (mailbox (sb-concurrency:make-mailbox :name "nodecode-gitlab-duo-agent socket")))

(defun open-websocket (url headers)
  "A workflow socket to URL with the handshake HEADERS, connected."
  (let* ((ws (wsd:make-client url :additional-headers headers))
         (socket (make-socket (lambda (text) (wsd:send ws text))
                              (lambda () (nlk:sever-websocket ws)))))
    (let ((mailbox (socket-mailbox socket)))
      (wsd:on :message ws (lambda (message)
                            (sb-concurrency:send-message
                             mailbox (list :message (if (stringp message)
                                                        message
                                                        (sb-ext:octets-to-string (coerce message '(vector (unsigned-byte 8)))
                                                                                 :external-format :utf-8))))))
      (wsd:on :close ws (lambda (&key code reason)
                          (sb-concurrency:send-message mailbox (list :close code reason))))
      (wsd:on :error ws (lambda (error)
                          (sb-concurrency:send-message mailbox (list :error (princ-to-string error))))))
    (handler-case (sb-sys:with-deadline (:seconds *idle-seconds*) (wsd:start-connection ws))
      (sb-sys:deadline-timeout ()
        (ignore-errors (nlk:sever-websocket ws))
        (error 'nle::provider-error :scope :request
                                    :detail (format nil "GitLab Duo Workflow WebSocket did not open in ~a s" *idle-seconds*)))
      (error (condition)
        (ignore-errors (nlk:sever-websocket ws))
        (error 'nle::provider-error :scope :request
                                    :detail (format nil "GitLab Duo Workflow WebSocket error: ~a" condition))))
    socket))

(defvar *socket-factory* 'open-websocket
  "How a workflow socket is opened: a function of (URL HEADERS) answering a
SOCKET. A test binds a scripted one.")

(defun socket-send (socket object)
  "Send OBJECT as one JSON text frame."
  (funcall (socket-sender socket) (nlk:encode-json-object object)))

(defun socket-end (socket)
  "Close SOCKET, never signalling."
  (ignore-errors (funcall (socket-closer socket))))

(defun next-event (socket)
  "The next event on SOCKET, or NIL after *IDLE-SECONDS* of silence. A
cancelled turn unwinds from here."
  (let ((deadline (+ (get-internal-real-time) (* *idle-seconds* internal-time-units-per-second))))
    (loop
      (when nle::*current-durable-turn*
        (nlk:ensure-turn-not-cancelled nle::*current-durable-turn*))
      (let ((left (/ (- deadline (get-internal-real-time)) internal-time-units-per-second)))
        (when (<= left 0) (return nil))
        (let ((event (sb-concurrency:receive-message (socket-mailbox socket) :timeout (min 1 left))))
          (when event (return event)))))))

;;; --- one round's fold --------------------------------------------------------------------

(defstruct (session (:constructor make-session (workflow-id start socket base token)))
  "A workflow left waiting on its socket for a tool's result."
  workflow-id start socket base token
  (pending nil)
  (contents (make-hash-table :test 'equal))
  (signatures (make-hash-table :test 'equal))
  (boundary-length nil))

(defvar *sessions* (make-hash-table :test 'equal :synchronized t)
  "Session key -> SESSION: the workflows waiting on a tool, per instance,
model and conversation.")

(defun session-key (base model session-id)
  (format nil "~a ~a ~a" base model (or session-id "")))

(defstruct (fold (:constructor make-fold (asm contents signatures)))
  "What one round has streamed and learned."
  asm contents signatures
  (session nil)
  (active-key nil)
  (blocks 0)
  (ended-text nil)
  (ended-thinking nil)
  (last-length nil)
  (approval nil)
  (failure nil)
  (usage nil)
  (finish nil))

(defun end-blocks (fold)
  "Close the text and thinking spans: what streams next is a new block."
  (let ((asm (fold-asm fold)))
    (when (nle::lane-assembly-text-id asm) (setf (fold-ended-text fold) t))
    (when (nle::lane-assembly-reasoning-id asm) (setf (fold-ended-thinking fold) t))
    (nle::assembly-close-spans asm :order '(:text :reasoning))))

(defun emit-segment (fold kind text)
  "Stream TEXT as the answer (KIND :text) or as reasoning (:thinking): a new
block after a closed one starts on a blank line, the one string a chat
message carries for each."
  (let ((asm (fold-asm fold)))
    (ecase kind
      (:text
       (unless (nle::lane-assembly-text-id asm)
         (incf (fold-blocks fold))
         (when (fold-ended-text fold) (setf text (format nil "~%~%~a" text))))
       (nle::assembly-text-delta asm (format nil "duo-text-~d" (fold-blocks fold)) text :close-reasoning t))
      (:thinking
       (unless (nle::lane-assembly-reasoning-id asm)
         (incf (fold-blocks fold))
         (when (fold-ended-thinking fold) (setf text (format nil "~%~%~a" text))))
       (nle::assembly-reasoning-delta asm (format nil "duo-reasoning-~d" (fold-blocks fold)) text :close-text t)))))

(defun fold-checkpoint (fold entries length usage)
  "Stream what the checkpoint snapshot ENTRIES says that this conversation
has not heard yet. Every checkpoint repeats the whole log, so an agent
message streams only what grew since it was last seen, and one seen at the
same turn position under another id streams nothing."
  (when usage (setf (fold-usage fold) usage))
  (setf (fold-last-length fold) length)
  (let ((turn 0)
        (contents (fold-contents fold))
        (signatures (fold-signatures fold)))
    (dolist (entry entries)
      (if (eq (first entry) :boundary)
          (progn (end-blocks fold) (incf turn))
          (destructuring-bind (kind key content) entry
            (let* ((previous (gethash key contents))
                   (hash (nlk:short-digest content 16))
                   (signature (format nil "~d ~(~a~) ~a" turn kind hash))
                   (content-signature (format nil "~d content ~a" turn hash))
                   (duplicate (and (null previous)
                                   (or (gethash signature signatures) (gethash content-signature signatures))))
                   (rewrote (and previous (not (uiop:string-prefix-p previous content)) (string/= previous content)))
                   (delta (cond ((or duplicate rewrote) "")
                                (previous (subseq content (length previous)))
                                (t content))))
              (setf (gethash key contents) content
                    (gethash signature signatures) t
                    (gethash content-signature signatures) t)
              (when (plusp (length delta))
                (when (and (fold-active-key fold) (string/= (fold-active-key fold) key) (null previous))
                  (end-blocks fold))
                (emit-segment fold kind delta)
                (setf (fold-active-key fold) key))))))))

(defun stalled-p (fold)
  "Whether this tool boundary's checkpoint is byte for byte the size of the
last one's: the service re-sending a state that did not advance."
  (let ((session (fold-session fold))
        (length (fold-last-length fold)))
    (when (and session length)
      (prog1 (eql length (session-boundary-length session))
        (setf (session-boundary-length session) length)))))

(defun fold-message (fold text)
  "Fold one socket message TEXT; => :continue, or how the socket's run ends:
:terminal, :failed, :approval, :action, :step-limit, :retryable-error, :stalled."
  (let ((event (ignore-errors (nlk:decode-json text))))
    (unless (hash-table-p event) (return-from fold-message :continue))
    (let ((status (status-of event)))
      (multiple-value-bind (entries length usage found) (checkpoint-of event)
        (when found (fold-checkpoint fold entries length usage)))
      (cond ((member status '("PLAN_APPROVAL_REQUIRED" "TOOL_CALL_APPROVAL_REQUIRED") :test #'equal)
             (setf (fold-approval fold) status)
             :approval)
            ((member status '("INPUT_REQUIRED" "FINISHED") :test #'equal)
             (setf (fold-finish fold) "stop")
             :terminal)
            ((member status '("FAILED" "STOPPED") :test #'equal)
             (let ((message (or (record-string event "error") (record-string event "message") status)))
               (setf (fold-failure fold) message)
               (cond ((and (equal status "FAILED") (step-limit-p message)) :step-limit)
                     ((and (equal status "FAILED") (generic-failure-p message)) :retryable-error)
                     (t :failed))))
            (t (let ((action (action-of event)))
                 (cond ((null action) :continue)
                       ((stalled-p fold) :stalled)
                       (t (destructuring-bind (request-id name args) action
                            (multiple-value-bind (tool arguments) (action-tool-call name args)
                              (end-blocks fold)
                              (nle::open-tool-buffer (fold-asm fold) 0 :id request-id :name tool
                                                                       :arguments (nlk:encode-json-object arguments))
                              (setf (fold-finish fold) "tool_calls")
                              (when (fold-session fold)
                                (setf (session-pending (fold-session fold)) request-id))))
                          :action))))))))

(defun run-socket (socket fold &key start resume)
  "Send START (a startRequest) or RESUME (actionResponses) on SOCKET and fold
what comes back until the run ends; => how it ended, as FOLD-MESSAGE says,
or :closed, or :timeout. The socket is closed unless the run ends on a tool
call, whose result goes back on it."
  (if resume
      (dolist (response resume) (socket-send socket response))
      (socket-send socket (nlk:json-object "startRequest" start)))
  (loop
    (let ((event (next-event socket)))
      (case (first event)
        ((nil) (socket-end socket) (return :timeout))
        (:close (return (if (fold-approval fold) :approval :closed)))
        (:error (socket-end socket)
                (error 'nle::provider-error
                       :detail (format nil "GitLab Duo Workflow WebSocket error: ~a" (second event))))
        (:message (let ((result (fold-message fold (second event))))
                    (case result
                      (:continue)
                      (:action (return :action))
                      (t (socket-end socket) (return result)))))))))

;;; --- the lane --------------------------------------------------------------------------

(defun end-sessions ()
  "Close every workflow waiting on a tool and ask GitLab to stop it: a
sign-out, a stop."
  (let ((held (loop for session being the hash-values of *sessions* collect session)))
    (clrhash *sessions*)
    (dolist (session held)
      (socket-end (session-socket session))
      (stop-workflow (session-base session) (session-token session) (session-workflow-id session)))))

(defun tool-result (messages request-id)
  "(values MESSAGE INDEX) of the tool result for REQUEST-ID in MESSAGES, the last one."
  (let ((index (position-if (lambda (message)
                              (and (equal "tool" (nlk:json-value message :string "role"))
                                   (equal request-id (nlk:json-value message :string "tool_call_id"))))
                            messages :from-end t)))
    (values (and index (aref messages index)) index)))

(defun steered-p (messages index)
  "Whether a user spoke after the tool result at INDEX: the service has no
way to hear it in a running workflow."
  (find-if (lambda (message) (member (nlk:json-value message :string "role") '("user" "developer") :test #'equal))
           messages :start (1+ index)))

(defun round-message (fold)
  "The round as the chat-shaped assistant message every lane answers."
  (let* ((asm (fold-asm fold))
         (content (get-output-stream-string (nle::lane-assembly-content asm))))
    (nlk:json-object
     "role" "assistant"
     "content" (if (string= content "") :null content)
     :when (nle::lane-assembly-reasoning-seen-p asm) "reasoning_content"
     (get-output-stream-string (nle::lane-assembly-reasoning asm))
     :when (nle::lane-assembly-tool-buffers asm) "tool_calls"
     (map 'vector (lambda (pair &aux (buf (cdr pair)))
                    (nle::chat-tool-call-object (or (getf buf :id) "") (getf buf :name) (getf buf :arguments)))
          (sort (copy-list (nle::lane-assembly-tool-buffers asm)) #'< :key #'car)))))

(defun round-usage (fold)
  "The usage the round reports: the agent's context occupancy as its input."
  (let ((usage (fold-usage fold)))
    (when usage
      (let ((accumulated (nle::lane-assembly-usage (fold-asm fold))))
        (setf (nle::provider-usage-input-tokens accumulated) (car usage)
              (nle::provider-usage-total-tokens accumulated) (car usage))
        accumulated))))

(defun failure (fold overflow &optional message)
  "The provider error a failed workflow ends the round with: the goal's
overflow when it was past the soft budget (the core evicts on it), else the
service's own words, which the core does not retry: omp has retried already."
  (if overflow
      (make-condition 'nle::provider-error :status 400 :detail overflow)
      (make-condition 'nle::provider-error :status 422
                                           :detail (or message (fold-failure fold) "GitLab Duo Workflow failed"))))

(defun prepare (base token selection &key definition project-path project-id cwd context messages model)
  "Everything a fresh workflow in the namespace SELECTION needs: settings,
project, direct access, the workflow, the start request."
  (let* ((root (getf selection :root))
         (rest-namespace (rest-namespace-id root))
         (create-namespace (or (getf selection :path) rest-namespace))
         (account (account token base cwd)))
    (unless (getf account :settings-p)
      (when (ensure-settings base token rest-namespace)
        (setf (getf (gethash (account-key token base cwd) *accounts*) :settings-p) t)))
    (let* ((discovered (and (null project-path) (null project-id)
                            (if (getf selection :project-path)
                                (list :path (getf selection :project-path))
                                (discover-project base token rest-namespace))))
           (id-path-p (and project-id (find #\/ project-id)))
           (path (or project-path (and id-path-p project-id) (getf discovered :path)))
           (numeric (or (and project-id (not id-path-p) project-id) (getf discovered :id)))
           (rest-project (or project-path project-id (getf discovered :path)))
           (socket-project (or numeric (and path (numeric-project-id base token path))))
           (connection (direct-access base token root rest-project definition))
           (workflow-id (create-workflow base token create-namespace rest-project definition))
           (available (available-models base token root))
           (model-ref (or (text-id (nlk:json-value (record available "pinnedModel") :any "ref")) model))
           (config (nle::compiled-turn-context-provider-config context))
           (tools (and (not (equal (nle::effective-provider-config-tool-choice config) "none"))
                       (nle::compiled-turn-context-tools context))))
      (list :root root :rest-namespace rest-namespace :create-namespace create-namespace
            :rest-project rest-project :socket-project socket-project :connection connection
            :workflow-id workflow-id :model-ref model-ref
            :start (start-request workflow-id :system (nle::compiled-turn-context-system-prompt context)
                                              :messages messages :tools tools :model-ref model-ref
                                              :project-id socket-project :namespace-id rest-namespace
                                              :definition definition)))))

(defun open-workflow-socket (base setup)
  "The socket SETUP's workflow runs on."
  (let ((connection (getf setup :connection)))
    (funcall *socket-factory*
             (socket-url (or (getf connection :base) base)
                         :project-id (getf setup :socket-project)
                         :namespace-id (getf setup :rest-namespace)
                         :model-ref (getf setup :model-ref)
                         :definition (nlk:json-value (getf setup :start) :string "workflowDefinition")
                         :service-p (getf connection :service-p))
             (socket-headers (getf connection :token) base
                             :project-id (getf setup :socket-project)
                             :namespace-id (getf setup :rest-namespace)
                             :extra (getf connection :headers)))))

(defun call-duo-workflow-streaming (context &key (on-part nle::*turn-part-fn*))
  "One round on the Duo Workflow Service: resume the workflow waiting on
this round's tool result, else run a fresh one with the conversation as its
goal; => (values MESSAGE USAGE FINISH-REASON REQUEST-JSON)."
  (let* ((config (nle::compiled-turn-context-provider-config context))
         (token (nle::effective-provider-config-api-key config))
         (base (string-right-trim "/" (nle::effective-provider-config-endpoint config)))
         (model (nle::effective-provider-config-model config))
         (session-id (nle::compiled-turn-context-session-id context))
         (messages (nle::request-messages context))
         (key (session-key base model session-id))
         (session (gethash key *sessions*))
         (asm (nle::make-lane-assembly :on-part on-part))
         (fold (make-fold asm
                          (if session (session-contents session) (make-hash-table :test 'equal))
                          (if session (session-signatures session) (make-hash-table :test 'equal))))
         (request nil))
    (nle::emit-stream-part on-part :stream-start)
    (labels ((drop (&optional stop)
               (let ((held (gethash key *sessions*)))
                 (remhash key *sessions*)
                 (when held
                   (socket-end (session-socket held))
                   (when stop (stop-workflow base token (session-workflow-id held))))))
             (answer ()
               (nle::assembly-close-spans asm :order '(:text :reasoning :tools))
               (nle::flush-thinking-tag asm)
               (let ((message (round-message fold))
                     (usage (round-usage fold)))
                 (nle::emit-stream-part on-part :finish)
                 (return-from call-duo-workflow-streaming
                   (values message usage (or (fold-finish fold) "stop")
                           (sb-ext:string-to-octets (nlk:encode-json-object request) :external-format :utf-8))))))
      ;; --- a workflow waiting on this round's tool result -----------------------
      (when session
        (multiple-value-bind (result index) (tool-result messages (session-pending session))
          (if (and result (not (steered-p messages index)))
              (let* ((text (part-text (gethash "content" result)))
                     (response (action-response (session-pending session) text (uiop:string-prefix-p "ERROR" text)))
                     (outcome nil))
                (setf request response
                      (session-pending session) nil
                      (fold-session fold) session)
                (handler-bind ((serious-condition (lambda (condition)
                                                    (declare (ignore condition))
                                                    (drop t))))
                  (setf outcome (run-socket (session-socket session) fold :resume (list response))))
                (case outcome
                  (:action (answer))
                  (:stalled (drop t))   ; the fresh workflow below carries the result in its goal
                  (:failed (drop) (error (failure fold nil)))
                  (:terminal (drop) (answer))
                  (t (drop (member outcome '(:closed :timeout))) (answer))))
              ;; a steer, or a call whose result never came: the service
              ;; cannot hear either in the running workflow
              (drop t))))
      ;; --- a fresh workflow ----------------------------------------------------
      (let* ((definition (or (configured :workflow-definition "GITLAB_DUO_WORKFLOW_DEFINITION") +workflow-definition+))
             (namespace-id (configured :namespace-id "GITLAB_DUO_NAMESPACE_ID"))
             ;; the section's project is an id, or a path when it has a slash;
             ;; omp's variables name each, and discovery reads the id first
             (section-project (let ((value (setting :project)))
                                (and (stringp value) (plusp (length (nlk:trimmed value))) (nlk:trimmed value))))
             (project-path (or (and section-project (find #\/ section-project) section-project)
                               (nle::credential-env "GITLAB_DUO_PROJECT_PATH")))
             (project-id (or (and section-project (not (find #\/ section-project)) section-project)
                             (nle::credential-env "GITLAB_DUO_PROJECT_ID")))
             (project (or project-id project-path))
             (explicit (or namespace-id project))
             (cwd (nlk:find-session-cwd session-id))
             (account (account token base cwd))
             (cached (and (not explicit) (getf account :selection)))
             (resolve (lambda (candidate) (and (getf candidate :root) candidate)))
             (setup nil))
        (flet ((select ()
                 (or (select-namespace base token resolve :namespace-id namespace-id :project project
                                                         :cwd cwd :enrich t)
                     (error 'nle::provider-error
                            :status 422 :scope :request
                            :detail "GitLab Duo Workflow runtime namespace resolution failed: Unable to find a GitLab Duo Workflow namespace. Set GITLAB_DUO_NAMESPACE_ID to a root namespace or GITLAB_DUO_PROJECT_ID to a GitLab project.")))
               (prepared (selection)
                 (prepare base token selection :definition definition :project-path project-path
                                               :project-id project-id :cwd cwd :context context
                                               :messages messages :model model)))
          (setf setup (if cached
                          (handler-case (prepared cached)
                            (error ()
                              ;; the namespace this account used stopped working: find it once more
                              (setf (getf (gethash (account-key token base cwd) *accounts*) :selection) nil)
                              (let ((selection (select)))
                                (prog1 (prepared selection)
                                  (setf (getf (gethash (account-key token base cwd) *accounts*) :selection) selection)))))
                          (let ((selection (select)))
                            (prog1 (prepared selection)
                              (unless explicit
                                (setf (getf (gethash (account-key token base cwd) *accounts*) :selection) selection)))))))
        (let* ((workflow-id (getf setup :workflow-id))
               (start (getf setup :start))
               (bytes (length (sb-ext:string-to-octets (nlk:json-value start :string "goal") :external-format :utf-8)))
               (overflow (and (>= bytes +goal-soft-bytes+) (overflow-message bytes)))
               (result :closed)
               (settled nil)
               (timeout-restarted nil)
               (step-restarts 0)
               (generic-retries 0)
               (stall-restarts 0))
          (when (>= bytes +goal-hard-bytes+)
            ;; not sent: the transport fails a goal this size nearly always
            (stop-workflow base token workflow-id)
            (error (failure fold overflow)))
          (flet ((fresh ()
                   (stop-workflow base token workflow-id)
                   (setf workflow-id (create-workflow base token (getf setup :create-namespace)
                                                      (getf setup :rest-project) definition)
                         start (nlk:copy-json-object start "workflowID" workflow-id))))
            (unwind-protect
                 (progn
                   (loop repeat +max-attempts+
                         do (let* ((socket (open-workflow-socket base (list* :start start setup)))
                                   (held (make-session workflow-id start socket base token)))
                              (setf (session-contents held) (fold-contents fold)
                                    (session-signatures held) (fold-signatures fold)
                                    (fold-session fold) held
                                    (gethash key *sessions*) held
                                    request (nlk:json-object "startRequest" start))
                              (setf result (run-socket socket fold :start start))
                              (cond ((eq result :approval)
                                     (setf start (approval-request start)
                                           (fold-approval fold) nil))
                                    ((and (eq result :timeout) (not timeout-restarted))
                                     (setf timeout-restarted t)
                                     (fresh))
                                    ((and (eq result :step-limit) (< step-restarts +max-step-limit-restarts+))
                                     (incf step-restarts)
                                     (fresh))
                                    ((and (eq result :stalled) (< stall-restarts +max-stall-restarts+))
                                     (incf stall-restarts)
                                     (fresh))
                                    ((and (eq result :retryable-error) (< generic-retries +max-generic-retries+))
                                     (incf generic-retries)
                                     (setf (fold-failure fold) nil)
                                     (fresh))
                                    (t (return)))))
                   (setf settled t))
              ;; Every way out but a tool call or the service's own end leaves
              ;; the workflow running with no one to answer it: stop it.
              (unless (and settled (member result '(:action :terminal)))
                (remhash key *sessions*)
                (socket-end (and (fold-session fold) (session-socket (fold-session fold))))
                (stop-workflow base token workflow-id)))
            (case result
              (:action (answer))
              (:terminal (remhash key *sessions*) (answer))
              ((:failed :retryable-error :step-limit) (error (failure fold overflow)))
              (:stalled (error (failure fold overflow +stall-message+)))
              (t (answer)))))))))
