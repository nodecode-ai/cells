;;;; verbs.lisp --- the hosted core tools as Lisp verbs, one function a tool.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; tools.py registered the 24 tools of its shipped tools.json with one
;;;; handler between them; here each is a function whose keyword arguments
;;;; are that tool's own input schema (the dotted name is the tool, the kebab
;;;; argument is the camelCase wire field). Hermes renamed the dots to
;;;; underscores because a function name cannot carry one; Lisp has no such
;;;; limit, so the verbs are the tool names with the dots as package-qualified
;;;; hunks: unbrowse.browse.act is (unbrowse:browse-act ...).
;;;;
;;;; Every one answers a string and refuses with UNBROWSE-ERROR. CALL-TOOL
;;;; still reaches any tool the workspace's tools/list carries, the learned and
;;;; indexed ones among them, by its dotted name.

(in-package #:nodecode-unbrowse)

(defun strings (value)
  "VALUE, a string or a list of them, as the JSON array the wire takes, or NIL."
  (typecase value
    (null nil)
    (string (vector value))
    (vector value)
    (list (coerce value 'vector))
    (t (vector value))))

;;; --- reading a page, and what Unbrowse knows --------------------------------

(define-verb scrape (url &key formats only-main-content render)
  "Read one page as clean markdown: (unbrowse:scrape \"https://example.com\")."
  (call-tool "unbrowse.scrape"
              (nlk:json-object "url" url
                                :opt "formats" (strings formats)
                                :opt "onlyMainContent" only-main-content
                                :opt "render" render)))

(define-verb map (url &key search limit)
  "List a site's URLs from its sitemaps: (unbrowse:map \"https://example.com\" :search \"docs\")."
  (call-tool "unbrowse.map"
              (nlk:json-object "url" url
                                :opt "search" search
                                :opt "limit" limit)))

(define-verb sites (&key site)
  "What Unbrowse knows about each website this workspace has touched, as one line each."
  (call-tool "unbrowse.sites" (nlk:json-object :opt "site" site)))

(define-verb discover (query)
  "Search the workspace's own space for a capability that answers QUERY:
(unbrowse:discover \"book a table at a restaurant\")."
  (call-tool "unbrowse.discover" (nlk:json-object "query" query)))

;;; --- runs ------------------------------------------------------------------

(define-verb run (&key capability task input url idempotency-key interaction-mode account-refs)
  "Start a durable run by capability id or by a natural-language TASK.
INPUT is what the capability's own schema names, a JSON object; URL is the page; IDEMPOTENCY-KEY
makes the same intended change one run."
  (call-tool "unbrowse.run"
              (nlk:json-object :opt "capability" capability
                                :opt "task" task
                                :opt "input" input
                                :opt "targetUrl" url
                                :opt "idempotencyKey" idempotency-key
                                :opt "interactionMode" interaction-mode
                                :opt "accountRefs" (strings account-refs))))

(define-verb inspect (run-id)
  "A run's status, its open requirements, its verified result and the effects it knows of."
  (call-tool "unbrowse.inspect" (nlk:json-object "runId" run-id)))

(define-verb resume (run-id &key answers responses expected-state-revision idempotency-key)
  "Answer a run's open requirements and continue the same run. ANSWERS is a JSON object of
field to value; a choice takes the option's own value."
  (call-tool "unbrowse.resume"
              (nlk:json-object "runId" run-id
                                :opt "answers" answers
                                :opt "responses" responses
                                :opt "expectedStateRevision" expected-state-revision
                                :opt "idempotencyKey" idempotency-key)))

(define-verb cancel (run-id)
  "Cancel a run; the receipt says which effects were dispatched, uncertain or unstarted."
  (call-tool "unbrowse.cancel" (nlk:json-object "runId" run-id)))

;;; --- learning --------------------------------------------------------------

(define-verb learn (&key goal har traces title)
  "Compile browser traces or a HAR of a task done by hand into a reusable one-call capability."
  (call-tool "unbrowse.learn"
              (nlk:json-object :opt "goal" goal
                                :opt "har" har
                                :opt "traces" traces
                                :opt "title" title)))

(define-verb forget (capability)
  "Delete one of this workspace's own learned capabilities, by id."
  (call-tool "unbrowse.forget" (nlk:json-object "capability" capability)))

(define-verb index (url &key focus max-capabilities public)
  "Index a whole website on purpose: a background job learns its read-only routes. Follow it with
(unbrowse:index-status :job-id ...)."
  (call-tool "unbrowse.index"
              (nlk:json-object "url" url
                                :opt "focus" focus
                                :opt "maxCapabilities" max-capabilities
                                :opt "public" public)))

(define-verb index-status (&key job-id wait-seconds)
  "An index job's progress: queued, running, done or failed, with the tools it added."
  (call-tool "unbrowse.index.status"
              (nlk:json-object :opt "jobId" job-id
                                :opt "waitSeconds" wait-seconds)))

;;; --- the cloud browser -----------------------------------------------------

(define-verb browse-open (url &key task)
  "Open a cloud browser on URL when no capability fits yet; it answers the page and its @refs."
  (call-tool "unbrowse.browse.open"
              (nlk:json-object "url" url :opt "task" task)))

(define-verb browse-snapshot (session-id)
  "The page's text and its interactive elements with @refs, which change after every action."
  (call-tool "unbrowse.browse.snapshot" (nlk:json-object "sessionId" session-id)))

(define-verb browse-act (session-id action &key ref value name vault credential key ms)
  "Click, fill, select, check, press or wait on a @ref, or autofill a login. For a fill, NAME says
what the value means (\"date\", \"query\")."
  (call-tool "unbrowse.browse.act"
              (nlk:json-object "sessionId" session-id
                                "action" action
                                :opt "ref" ref
                                :opt "value" value
                                :opt "name" name
                                :opt "vault" vault
                                :opt "credential" credential
                                :opt "key" key
                                :opt "ms" ms)))

(define-verb browse-finish (session-id &key goal title)
  "End a browser session: the page it ended on, and every recorded session compiled into routes."
  (call-tool "unbrowse.browse.finish"
              (nlk:json-object "sessionId" session-id
                                :opt "goal" goal
                                :opt "title" title)))

(define-verb browse-close (session-id &key discard)
  "Close a browser session. It is still indexed unless DISCARD."
  (call-tool "unbrowse.browse.close"
              (nlk:json-object "sessionId" session-id
                                :opt "discard" (and discard t))))

;;; --- saved logins ----------------------------------------------------------

(define-verb credentials-list (&key origin)
  "The logins this user has saved in Unbrowse: site, label and a masked username."
  (call-tool "unbrowse.credentials.list" (nlk:json-object :opt "origin" origin)))

(define-verb credentials-request (origin &key fields reason)
  "Ask the user to save a login without seeing it: a one-time link to give them."
  (call-tool "unbrowse.credentials.request"
              (nlk:json-object "origin" origin
                                :opt "fields" (strings fields)
                                :opt "reason" reason)))

(define-verb credentials-status (request-id &key wait-seconds)
  "Whether a saved-login request is pending, fulfilled, declined or expired."
  (call-tool "unbrowse.credentials.status"
              (nlk:json-object "requestId" request-id
                                :opt "waitSeconds" wait-seconds)))

;;; --- money -----------------------------------------------------------------

(define-verb credits (&key pack)
  "This workspace's credits: the free allowance left, the paid balance, and a checkout link when
PACK names one."
  (call-tool "unbrowse.credits" (nlk:json-object :opt "pack" pack)))

(define-verb usage ()
  "This workspace's metered usage for the month: verified calls billed and what ran free."
  (call-tool "unbrowse.usage" (nlk:json-object)))

;;; --- the live canvas -------------------------------------------------------

(define-verb canvas-read ()
  "Read the signed-in user's live canvas, its cards and their revisions."
  (call-tool "unbrowse.canvas.read" (nlk:json-object)))

(define-verb canvas-put (id kind title &key action data expected-revision parent-id text)
  "Insert or update one card on the live canvas. KIND is note, plan, results, draft or active;
DATA is the card's own JSON."
  (call-tool "unbrowse.canvas.put"
              (nlk:json-object "id" id
                                "kind" kind
                                "title" title
                                :opt "action" action
                                :opt "data" data
                                :opt "expectedRevision" expected-revision
                                :opt "parentId" parent-id
                                :opt "text" text)))
