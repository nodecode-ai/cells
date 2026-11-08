;;;; client.lisp --- one WeKnora deployment: its JSON calls and its answer stream.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; WeKnora (github.com/Tencent/WeKnora) answers JSON under one envelope --
;;;; {success, data, error, message, total, page, page_size} -- and composes an
;;;; answer as a text/event-stream of `data: {...}' events. This file is the
;;;; whole of the wire: the reads, the two retrieval POSTs, one session
;;;; create, and the chat POST that answers. Nothing here writes to a
;;;; knowledge base.
;;;;
;;;; The key travels as X-API-Key and a platform-scoped one also as
;;;; X-Tenant-ID. A refusal names the status and WeKnora's own reason and
;;;; never the key: nothing in this file prints a header back.
;;;;
;;;; With resource_urls=public WeKnora rewrites cited resource:// figures into
;;;; directly loadable http(s) URLs. A knowledge-base-restricted key answers
;;;; that with 403, so the first such refusal puts the client in handle mode
;;;; for the rest of the start instead of paying the same round trip per call.

(in-package #:nodecode-weknora)

;;; --- the deployment --------------------------------------------------------

(defvar *resource-blocked* nil
  "True once this start's credential refused resource_urls=public.")

(defvar *visible-scope* :unresolved
  "Every knowledge base id the credential can see, resolved once per start and
shared between search and ask: an unscoped call is then one list call and not
one per question. :UNRESOLVED until the first call that needs it.")

(defun api-root ()
  "The deployment's API root, or a refusal naming the setting to write."
  (or (setting :base-url)
      (fail "no WeKnora deployment is set: put base_url - the API root of your WeKnora, ~
a bare origin gaining /api/v1, so \"https://kb.example.com\" or \"http://localhost:8080\" - ~
in the weknora section of the config, then (restart-cells \"nodecode-weknora\")")))

(defun api-root-tail-p (text)
  "Whether TEXT ends with an /api/vN version segment."
  (let ((at (cl:search "/api/v" text :from-end t)))
    (and at
         (> (length text) (+ at 6))
         (every #'digit-char-p (subseq text (+ at 6))))))

(defun normalize-base-url (raw)
  "RAW as the API root a call is made against: a bare origin gains /api/v1 and a
trailing slash goes. A value that is not an absolute http(s) address refuses at
the start (NLK:CONFIG-REFUSAL), not inside the first tool call."
  (let* ((text (string-right-trim "/" (nlk:trimmed raw)))
         (scheme (find-if (lambda (prefix) (uiop:string-prefix-p prefix text))
                           '("https://" "http://"))))
    (unless (and scheme (> (length text) (length scheme)) (ignore-errors (quri:uri text)))
      (nlk:config-error "weknora.base_url is ~s: give the deployment's API root as an ~
http:// or https:// address, such as \"https://kb.example.com\" or \"http://localhost:8080\"" raw))
    (if (api-root-tail-p text) text (concatenate 'string text "/api/v1"))))

(defun headers ()
  "The headers every call carries: the key when one is set, the workspace when
the key is platform-scoped."
  (let ((key (setting :api-key))
        (tenant (setting :tenant-id)))
    (append '(("content-type" . "application/json")
              ("accept" . "application/json"))
            (when key (list (cons "x-api-key" key)))
            (when tenant (list (cons "x-tenant-id" tenant))))))

(defun public-mode-p ()
  "Whether calls ask WeKnora for directly loadable file URLs."
  (and (equal "public" (setting :resource-urls))
       (not *resource-blocked*)))

(defun url-for (path &optional query)
  "The deployment's root and PATH, with QUERY -- an alist of (KEY . VALUE) --
and the resource mode rider, url-encoded."
  (let ((pairs (append (loop for (key . value) in query
                              when value
                                collect (cons key (princ-to-string value)))
                        (when (public-mode-p)
                          '(("resource_urls" . "public"))))))
    (format nil "~a~a~@[?~a~]" (api-root) path
            (and pairs (quri:url-encode-params pairs)))))

(defun http (method url &rest arguments)
  "The cell's one network call, NLK:HTTP; a test stubs this name."
  (apply #'nlk:http method url arguments))

;;; --- what a refusal says ----------------------------------------------------

(defun snippet (text)
  "TEXT as one bounded line: a body that was not JSON is usually a proxy's page."
  (nlk:one-line text :cap 300))

(defun reason-of (text)
  "What WeKnora said went wrong in TEXT: its error message, its message, or the
body itself when a proxy answered instead."
  (let ((json (ignore-errors (nlk:decode-json (or text "")))))
    (flet ((sentence (object)
             (nlk:json-value object :string "message")))
      (cond
        ((hash-table-p json)
         (let ((error (gethash "error" json)))
           (cond ((stringp error) error)
                 ((hash-table-p error) (or (sentence error) (snippet text)))
                 (t (or (sentence json) (snippet text))))))
        ((stringp json) (snippet json))
        (t (snippet text))))))

(defun public-forbidden-p (text status)
  "Whether TEXT at STATUS is a knowledge-base-restricted key refusing public URLs."
  (and (eql status 403)
       (not *resource-blocked*)
       (equal "public" (setting :resource-urls))
       (cl:search "resource_urls=public" (or text ""))))

(defun raw-json (method path &optional query body)
  "METHOD PATH as one JSON exchange. => (values TEXT STATUS), every status a
value, a transport that never arrived a refusal naming what failed."
  (handler-case
      (multiple-value-bind (text status)
          (http method (url-for path query)
                :headers (headers)
                :content (and body (nlk:encode-json-object body))
                :timeout (setting :request-timeout))
        (values (if (stringp text) text (princ-to-string (or text ""))) status))
    (error (condition)
      (fail "~a ~a failed: ~a" method path (nlk:one-line condition :cap 200)))))

(defun json-call (method path &optional query body)
  "One JSON call, retried once in handle mode after a public-mode 403.
=> the decoded envelope; a refused status, a body that was not JSON and a
transport that never arrived all refuse with WEKNORA-ERROR."
  (multiple-value-bind (text status) (raw-json method path query body)
    (when (public-forbidden-p text status)
      (setf *resource-blocked* t)
      (multiple-value-bind (retry-text retry-status) (raw-json method path query body)
        (setf text retry-text status retry-status)))
    (unless (and (integerp status) (<= 200 status 299))
      (fail "~a ~a failed with HTTP ~a~@[: ~a~]" method path status (reason-of text)))
    (let ((envelope (handler-case (nlk:decode-json text) (error () nil))))
      (if (hash-table-p envelope)
          envelope
          (fail "~a ~a returned a non-JSON body: ~a" method path (snippet text))))))

;;; --- the reads ---------------------------------------------------------------

(defun list-knowledge-bases ()
  "Every knowledge base this credential can retrieve from. => a vector of records."
  (nlk:json-array (json-call :get "/knowledge-bases") "data"))

(defun knowledge-base-ids ()
  "The id of each knowledge base this credential can see, in the order listed."
  (loop for record across (list-knowledge-bases)
        for id = (nlk:json-value record :string "id")
        when id collect id))

(defun visible-ids ()
  "Every knowledge base id the credential can see, resolved once per start."
  (if (eq *visible-scope* :unresolved)
      (setf *visible-scope* (knowledge-base-ids))
      *visible-scope*))

(defun search-passages (query knowledge-base-ids knowledge-ids)
  "Hybrid (vector + keyword) retrieval. => the hit records, best first."
  (nlk:json-array
   (json-call :post "/knowledge-search" nil
              (nlk:json-object "query" query
                               :when (plusp (length knowledge-base-ids))
                               "knowledge_base_ids" (coerce knowledge-base-ids 'vector)
                               :when (plusp (length knowledge-ids))
                               "knowledge_ids" (coerce knowledge-ids 'vector)))
   "data"))

(defun find-documents (keyword limit)
  "The documents whose title or file name matches KEYWORD, across every
knowledge base the credential can see. => a vector of records."
  (nlk:json-array
   (json-call :get "/knowledge/search"
              (list (cons "keyword" keyword) (cons "limit" limit)))
   "data"))

(defun document (knowledge-id)
  "One document's metadata -- the summary WeKnora generates is its description --
or NIL when the deployment will not serve it."
  (nlk:json-value (json-call :get (format nil "/knowledge/~a"
                                           (quri:url-encode knowledge-id :encoding :utf-8)))
                  :object "data"))

(defun chunks (knowledge-id page page-size)
  "One page of a document's stored chunks.
=> (values CHUNKS TOTAL PAGE PAGE-SIZE), the paging falling back to what was
asked for when the deployment omits the envelope's own numbers."
  (let ((envelope (json-call :get (format nil "/chunks/~a"
                                           (quri:url-encode knowledge-id :encoding :utf-8))
                              (list (cons "page" page) (cons "page_size" page-size)))))
    (let ((items (nlk:json-array envelope "data")))
      (values items
              (or (nlk:json-value envelope :integer "total") (length items))
              (or (nlk:json-value envelope :integer "page") page)
              (or (nlk:json-value envelope :integer "page_size") page-size)))))

(defun create-session (title)
  "A conversation container to hold one or more asked questions. => its id."
  (let ((id (nlk:json-value (json-call :post "/sessions" nil
                                        (nlk:json-object "title" title))
                             :string "data" "id")))
    (or id (fail "POST /sessions returned no session id"))))

;;; --- the answer stream -------------------------------------------------------

(defun text-stream (stream)
  "STREAM as characters, UTF-8 read one octet at a time when it is a byte stream."
  (if (and (streamp stream)
           (ignore-errors (subtypep (stream-element-type stream) 'integer)))
      (flexi-streams:make-flexi-stream stream :external-format :utf-8)
      stream))

(defun stream-text (body)
  "BODY -- a failed call's answer stream, or the text an exchange answered --
as a string, so a refusal can name what WeKnora said."
  (if (streamp body)
      (with-output-to-string (out)
        (handler-case
            (loop for line = (read-line (text-stream body) nil nil)
                  while line do (write-line line out))
          (error () nil)))
      (or body "")))

(defun chat-body (query knowledge-base-ids agent-id web-search)
  "The body of one chat POST: the RAG pipeline reads the scope from it, a
custom agent resolves its own server-side, so only the RAG one is scoped here."
  (nlk:json-object "query" query
                    "channel" "api"
                    :when (plusp (length knowledge-base-ids))
                    "knowledge_base_ids" (coerce knowledge-base-ids 'vector)
                    :when agent-id "agent_id" agent-id
                    :when agent-id "agent_enabled" t
                    :when web-search "web_search_enabled" t))

(defun open-answer (path body)
  "POST PATH with BODY and answer the event stream. => (values STREAM STATUS
TEXT): TEXT is what a refused status said. A knowledge-base-restricted
credential's 403 on public URLs is retried once in handle mode."
  (flet ((dial ()
           (handler-case
               (http :post (url-for path)
                     :headers (append (headers) '(("accept" . "text/event-stream")))
                     :content (nlk:encode-json-object body)
                     :timeout (setting :chat-timeout)
                     :stream t)
             (error (condition)
               (fail "POST ~a failed: ~a" path (nlk:one-line condition :cap 200))))))
    (multiple-value-bind (stream status) (dial)
      (if (and (integerp status) (<= 200 status 299))
          (values stream status nil)
          (let ((text (stream-text stream)))
            (if (public-forbidden-p text status)
                (progn
                  (setf *resource-blocked* t)
                  (multiple-value-bind (stream status) (dial)
                    (if (and (integerp status) (<= 200 status 299))
                        (values stream status nil)
                        (values nil status (stream-text stream)))))
                (values nil status text)))))))

(defun read-answer (path stream session-id)
  "Assemble PATH's event stream on STREAM. => a plist (:ANSWER :REFERENCES
:TOOL-CALLS :SESSION-ID). A `complete' event ends it; a stream that ends
without one refuses, so a truncated answer is never handed on as a whole one."
  (let ((text (make-string-output-stream))
        (references '())
        (tool-calls '())
        (resolved session-id)
        (completed nil))
    (flet ((take (payload)
             ;; One event: the answer grows, the citations and the tool names
             ;; it used are kept, and `complete' is what ends the read.
             (let ((event (handler-case (nlk:decode-json payload) (error () nil))))
               (when (hash-table-p event)
                 (let ((answered-in (nlk:json-value event :string "session_id")))
                   (when answered-in (setf resolved answered-in)))
                 (let ((kind (nlk:json-value event :string "response_type")))
                   (cond
                     ((equal kind "answer")
                      (let ((content (nlk:json-value event :string "content")))
                        (when content (write-string content text))))
                     ((equal kind "references")
                      (setf references (coerce (nlk:json-array event "knowledge_references") 'list)))
                     ((equal kind "tool_call")
                      (loop for call across (nlk:json-array event "tool_calls")
                            for name = (or (nlk:json-value call :string "function" "name")
                                           (nlk:json-value call :string "name"))
                            when (and name (not (member name tool-calls :test #'equal)))
                              do (push name tool-calls)))
                     ((equal kind "error")
                      (fail "POST ~a streamed an error: ~a" path
                            (or (nlk:json-value event :string "content") "unknown error")))
                     ((equal kind "complete")
                      (setf completed t)))))
               ;; NLK:READ-EVENTS stops at the first true answer its handler
               ;; gives, so only a completed answer ends the read: an answer
               ;; chunk's text is not the signal, and returning it would cut
               ;; the stream at the first event that carried any.
               (and completed t))))
      (unwind-protect
           (handler-case
               (nlk:read-events (text-stream stream) #'take
                                :limit 4000000 :idle (setting :chat-timeout))
             (weknora-error (condition) (error condition))
             (error (condition)
               (fail "POST ~a stream failed: ~a" path (nlk:one-line condition :cap 200))))
        (when (streamp stream) (ignore-errors (close stream :abort t))))
      (let ((answer (get-output-stream-string text)))
        (unless completed
          (fail "POST ~a ended before WeKnora completed the answer; ~d character(s) had streamed"
                path (length answer)))
        (list :answer answer
              :references references
              :tool-calls (nreverse tool-calls)
              :session-id resolved)))))

(defun answer (query knowledge-base-ids agent-id session-id web-search)
  "WeKnora's own composed answer for QUERY: its RAG pipeline when AGENT-ID is
NIL, its ReAct one when it is not. => a plist as READ-ANSWER answers."
  (let ((path (format nil "/~a/~a"
                      (if agent-id "agent-chat" "knowledge-chat")
                      (quri:url-encode session-id :encoding :utf-8))))
    (multiple-value-bind (stream status text)
        (open-answer path (chat-body query knowledge-base-ids agent-id web-search))
      (unless (and (integerp status) (<= 200 status 299))
        (fail "POST ~a failed with HTTP ~a~@[: ~a~]" path status (reason-of text)))
      (read-answer path stream session-id))))
