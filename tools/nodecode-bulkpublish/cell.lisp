;;;; cell.lisp --- the two tools, /bulkpublish, the manual, the section.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin this cell rewrites ships no code: a manifest, one MCP server
;;;; entry and thirty-three prose skills. The server's own tools are reached
;;;; here by name -- bulkpublish_tools lists them, bulkpublish_call runs one
;;;; with the arguments the server documents -- and what the skills taught is
;;;; the (help :bulkpublish) manual: draft, then publish; a write confirmed
;;;; once; media in three calls; the platform keys; approvals; analytics.
;;;;
;;;; One guard is this cell's own: the plugin marked the server untrusted so
;;;; its harness asked before every call that changes an account. Here that
;;;; ask is one argument -- a write called without confirm answers what it
;;;; would send instead of sending it -- and approve_writes in the section
;;;; turns even that off.

(in-package #:nodecode-bulkpublish)

;;; --- the manual ---------------------------------------------------------------

(defparameter +manual+
  "BulkPublish publishes and schedules posts on 15 social networks -- X, Instagram, Facebook,
LinkedIn, Threads, Bluesky, Mastodon, TikTok, YouTube, Pinterest, Google Business, Discord,
Telegram, Tumblr and Snapchat -- through the nodecode-bulkpublish cell.
Tools: bulkpublish_tools lists the server's tools, all of them or the ones a word matches;
bulkpublish_call runs one by name with its arguments object.
Eval: (bulkpublish:tools :query \"media\"), (bulkpublish:call \"get_post\" \"postId\" \"abc\"),
  (bulkpublish:channels) for the accounts connected to the key.
/bulkpublish channels|tools|quota, and /bulkpublish call get_post {\"postId\": \"abc\"}, ask by hand.
A post is drafted before it publishes: create_post makes it, publish_post sends it now, update_post
  changes it, list_posts and get_post read it back. Call get_post first and show the user the change.
A write -- a tool named publish_, reply_, delete_, approve_, retry_, update_, schedule_, cancel_,
  send_ or bulk_ -- answers what it would send until the same call carries \"confirm\": true.
  approve_writes false in the section drops that ask.
Media: upload_media takes a public url. A local file is three calls: create_media_upload
  (contentType, sizeBytes), a PUT of the bytes to the uploadUrl it answers, then
  finalize_media_upload (r2Key, fileName, mimeType, sizeBytes); the id it answers goes in
  mediaFileIds. filePath only works when the server runs on this machine.
Platform types ride in postTypeOverrides: Instagram needs reel or feed_video for video and
  feed_photo for a picture; Facebook, X, LinkedIn, Pinterest, TikTok, YouTube and Threads each
  default to their first type, which is wrong for video more often than not; Google Business is
  the key gmb. Character limits start at 280 (X) and reach 63,206 (Facebook).
Approvals: create_post with requestApproval true waits for approve_post; a 403 APPROVAL_REQUIRED
  means the role cannot publish that post directly.
Numbers: get_analytics takes from and to as ISO dates; get_quota_usage says what the plan has
  left. A list is the account as it is now; nothing here is cached.
A refusal is ERROR: BULKPUBLISH-ERROR with what the server said."
  "What (help :bulkpublish) answers while the cell runs.")

;;; --- the boundary contracts ----------------------------------------------------

(nlk:define-contract bulkpublish-tools
  (:doc "List the tools BulkPublish's server offers, one a line: its name, then what it does. Use it first, or after a refusal names an argument you have not read; then call one with bulkpublish_call. Query keeps the tools whose name or description carries a word, such as media, analytics, rss or inbox.")
  ("query" :text :doc "only the tools whose name or description carries this word or phrase")
  ("limit" :integer :default 40 :doc "how many to list, 1 to 200"))

(nlk:define-contract bulkpublish-call
  (:doc "Run one of BulkPublish's tools by its own name, with the arguments object that tool documents -- list_channels, create_post, publish_post, upload_media, get_analytics and the rest, as bulkpublish_tools lists them. A tool that publishes, replies, deletes, approves, retries, updates, schedules, cancels or runs a bulk action is a write: called without confirm it answers what it would send, and the same call with confirm true sends it. Publishing reaches real people on the user's accounts.")
  ("tool" :text :required t :doc "the tool's name, exactly as bulkpublish_tools lists it")
  ("arguments" :any :default nil
   :doc "the arguments as one JSON object, keys as that tool documents, e.g. {\"channelId\": \"...\", \"content\": \"...\"}; leave it out for a tool that takes none")
  ("confirm" :boolean :doc "true to send a write: a publish, reply, delete, approve, retry, update, schedule, cancel or bulk call"))

;;; --- a tool the server lists ---------------------------------------------------

(defparameter +write-prefixes+ '("publish_" "reply_" "delete_" "approve_" "retry_" "update_"
                                 "schedule_" "cancel_" "send_" "bulk_")
  "The prefixes of the server's tools whose call changes an account other people see. A call to
one of them answers what it would send until confirm is true -- the approval prompt the plugin
declared, as one argument.")

(defun tool-line (tool)
  "TOOL as the one line a listing gives it: its name, then what it does."
  (format nil "~a - ~a"
          (or (nlk:json-value tool :string "name") "?")
          (nlk:one-line (or (nlk:json-value tool :string "description") "") :cap 200)))

(defun list-tools (&key query limit)
  "The server's tools, one a line, at most LIMIT of them, only those QUERY matches."
  (running-settings)
  (let* ((tools (tool-catalog))
         (needle (and query (string-downcase (nlk:trimmed query))))
         (matching (if (and needle (plusp (length needle)))
                       (remove-if-not (lambda (tool)
                                        (search needle (string-downcase (tool-line tool))))
                                      tools)
                       tools))
         (limit (max 1 (min (or limit 40) 200)))
         (shown (subseq matching 0 (min limit (length matching)))))
    (if (null matching)
        (format nil "bulkpublish: none of the ~d tools the server lists~@[ carries ~s~]"
                (length tools) query)
        (format nil "bulkpublish: ~d of ~d tools~@[ carrying ~a~]; one a line:~%~{~a~%~}"
                (length shown) (length tools) query (mapcar #'tool-line shown)))))

(defun write-tool-p (name)
  "Whether NAME is a tool whose call changes an account."
  (some (lambda (prefix) (uiop:string-prefix-p prefix name)) +write-prefixes+))

(defun arguments-text (arguments)
  "ARGUMENTS as the JSON the server would receive, clipped."
  (if (and (hash-table-p arguments) (plusp (hash-table-count arguments)))
      (nlk:clip (nlk:encode-json-object arguments) 400 :ellipsis "...")
      "no arguments"))

(defun guarded (body)
  "BODY's string; a refusal becomes the tool answer the model repairs from,
naming this cell so the transcript shows which call failed."
  (handler-case (funcall body)
    (bulkpublish-error (condition)
      (nle:failure (format nil "ERROR: BULKPUBLISH-ERROR: ~a" condition)))
    (error (condition)
      (nle:failure (format nil "ERROR: BULKPUBLISH-ERROR: ~a" condition)))))

(defun tools-tool (request)
  (guarded (lambda ()
             (list-tools :query (bulkpublish-tools-query request)
                         :limit (bulkpublish-tools-limit request)))))

(defun run-tool (request)
  "bulkpublish_call: a write without confirm answers what it would send."
  (guarded
   (lambda ()
     (let ((name (bulkpublish-call-tool request))
           (arguments (bulkpublish-call-arguments request))
           (confirm (bulkpublish-call-confirm request)))
       (if (and (setting :approve-writes) (write-tool-p name) (not confirm))
           (format nil "~a changes the account. It would send ~a~%Call bulkpublish_call once more with the same tool and arguments and \"confirm\": true to send it."
                   name (arguments-text arguments))
           (call-tool name arguments))))))

;;; --- the verbs ------------------------------------------------------------------

(defun pairs-object (pairs)
  "PAIRS, alternating argument names and values, as one JSON object. A name is
a string, or a keyword or symbol whose downcased name is the argument's."
  (when (oddp (length pairs))
    (fail "arguments come in name and value pairs; ~d left one over" (length pairs)))
  (let ((object (nlk:make-json-object)))
    (loop for (name value) on pairs by #'cddr
          do (setf (gethash (if (stringp name) name (string-downcase (symbol-name name)))
                             object)
                   value))
    object))

(defun tools (&key query limit)
  "The server's tools, one a line; QUERY keeps what its name or description carries."
  (running-settings)
  (list-tools :query query :limit limit))

(defun call (tool &rest pairs)
  "Call TOOL with PAIRS, a name and a value each:
(bulkpublish:call \"get_post\" \"postId\" \"abc\") answers what the server said."
  (running-settings)
  (call-tool (string tool) (and pairs (pairs-object pairs))))

(defun channels ()
  "The channels connected to the account, as the server lists them."
  (running-settings)
  (call-tool "list_channels" nil))

;;; --- /bulkpublish ---------------------------------------------------------------

(defun status-line ()
  "One line: where the server is, what is known of it, how a write is taken."
  (format nil "bulkpublish: 15 networks through ~a~@[ (~d tools listed)~]; key ~a; writes ~a; ~ds a call"
          (url-host (endpoint))
          (and *session* (getf *session* :tools) (length (getf *session* :tools)))
          (if (api-key) "set" "missing")
          (if (setting :approve-writes) "ask for confirm" "go without confirm")
          (setting :timeout)))

(defun panel ()
  "What bare /bulkpublish answers besides its line: one ask-row a habit."
  (nle:list-dialog
   :title "BulkPublish" :summary (status-line)
   :rows (list (nle:ask-row "The channels connected" "Nothing" "/bulkpublish channels"
                             :placeholder "the accounts this key can post to" :verb "list")
               (nle:ask-row "What the server can do" "Word" "/bulkpublish tools"
                             :placeholder "media, analytics, rss, inbox" :verb "list")
               (nle:ask-row "Call one tool" "Name and JSON" "/bulkpublish call"
                             :placeholder "get_post {\"postId\": \"...\"}" :verb "call"))))

(defun split-verb (args)
  "ARGS as (values HEAD REST): the first word, and the trimmed remainder."
  (let* ((text (nlk:trimmed (or args "")))
         (space (position #\Space text)))
    (values (if space (subseq text 0 space) text)
            (if space (nlk:trimmed (subseq text (1+ space))) ""))))

(defun asked (body)
  "BODY's answer, or its refusal as text -- what a command shows."
  (handler-case (funcall body)
    (error (condition) (format nil "ERROR: BULKPUBLISH-ERROR: ~a" condition))))

(defun arguments-of (tail)
  "TAIL as a JSON object of arguments => (values OBJECT PROBLEM-TEXT)."
  (if (zerop (length tail))
      (values nil nil)
      (let ((object (object-of tail)))
        (if object
            (values object nil)
            (values nil (format nil "bulkpublish: ~a is not a JSON object of arguments, as in get_post {\"postId\": \"...\"}"
                                (nlk:clip tail 80 :ellipsis "...")))))))

(defun call-typed (line)
  "One `call NAME [JSON]' typed at the command: the tool's name and its arguments."
  (multiple-value-bind (name tail) (split-verb line)
    (if (zerop (length name))
        "bulkpublish: call what? /bulkpublish call get_post {\"postId\": \"...\"}"
        (multiple-value-bind (arguments problem) (arguments-of tail)
          (or problem (asked (lambda () (call-tool name arguments))))))))

(defun run-command (args session-id)
  "One /bulkpublish invocation => the answer's text, and its panel beside it."
  (declare (ignore session-id))
  (multiple-value-bind (head rest) (split-verb args)
    (cond ((or (zerop (length head)) (string-equal head "status"))
           (values (status-line) (panel)))
          ((string-equal head "channels") (values (asked #'channels) (panel)))
          ((string-equal head "quota")
           (values (asked (lambda () (call-tool "get_quota_usage" nil))) (panel)))
          ((string-equal head "tools")
           (values (asked (lambda () (list-tools :query rest))) (panel)))
          ((string-equal head "call") (values (call-typed rest) (panel)))
          (t (values (format nil "bulkpublish: ~a is not one of its choices (channels, tools, quota, call, status), or type /bulkpublish channels as bulkpublish: channels" head)
                     (panel))))))

;;; --- the one declaration ---------------------------------------------------------

(nle:define-cell bulkpublish
  (:section ("bulkpublish")
    (:guide "make a key at https://app.bulkpublish.com/developer; endpoint is the server's MCP URL, https://mcp.bulkpublish.com/mcp/full unless a proxy stands in the way; set api_key or export the variable api_key_env names; approve_writes asks once before a call that changes an account; timeout bounds one call")
    ("endpoint" :string :required t
     :doc "the server's MCP URL, https://mcp.bulkpublish.com/mcp/full unless a proxy stands in the way")
    ("api_key_env" :env :default "BULKPUBLISH_API_KEY"
                   :doc "the environment variable holding the BulkPublish API key")
    ("api_key" :secret :doc "the BulkPublish API key itself, kept in a 0600 file; api_key_env names one instead")
    ("approve_writes" :boolean :default t
                      :doc "answer what a publishing call would send until it carries confirm, the approval the plugin declared")
    ("timeout" :integer :default 60 :min 5
               :doc "seconds one call to the server may take"))
  (:tool "bulkpublish_tools" (nlk:contract 'bulkpublish-tools) 'tools-tool)
  (:tool "bulkpublish_call" (nlk:contract 'bulkpublish-call) 'run-tool)
  (:command "bulkpublish" 'run-command
            :description "Publish and schedule on 15 social networks"
            :argument-hint "[channels|tools|quota|call TOOL JSON]")
  (:help :bulkpublish
         "bulkpublish: 15 social networks through BulkPublish - bulkpublish:tools, bulkpublish:call, bulkpublish:channels"
         +manual+)
  (:start (lambda () (setf *session* nil))))
