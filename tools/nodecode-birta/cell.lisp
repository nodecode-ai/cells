;;;; cell.lisp --- the primer, the section, the /birta command, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin was an MCP server entry plus sixteen skills. The entry is the
;;;; two tools tools.lisp declares, and the skills are +PRIMER+, which the
;;;; model reads with (help :birta) and whose one line rides every request.
;;;; Nothing hooks the provider path: no model is served here.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "birta": {"token": "...", "token_env": "BIRTA_TOKEN",
;;;;             "url": "https://birta.dev/mcp", "timeout_seconds": 120,
;;;;             "max_chars": 6000}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-birta)

(defparameter +primer+
  "Birta is a hosted workspace where people and agents make pages, sites, reports, decks and
documents, publish them at a link, and review them together on the page. Every call goes to
birta.dev and acts on the Birta projects you can open there: the draft and the published versions
live there, not in the user's folder.

Call birta_tools once at the start of a Birta task. It answers the live tool list, each tool with
the arguments it takes, and those descriptions are the authority. Then call one with birta_call,
as birta_call {tool: \"list_projects\", arguments: {}}.

Publishing and review:
  1. Find or make the project: list_projects, then create_project with the name the user chose,
     omitting the address unless they picked one.
  2. Look before editing: list_versions, and continue the existing draft rather than overwriting
     another participant's.
  3. Draft the page as a project file, index.html or index.md as the entry page. The draft is a
     workspace: its address is never the one you give the user.
  4. Publish with publish_version; that version becomes current at the project address. If the
     user wants to look first, publish; if they asked not to, leave the work in the draft and say
     what it holds.
  5. Give the project address back, the same across publications and rollbacks, and say who can
     open it (project_settings, set_project_access).
  6. Read remarks with list_version_comments, apply them in the source, answer in the thread with
     reply_to_thread, and publish again. Threads keep their numbers across versions, so \"do
     number 3\" means thread 3; rollback makes an earlier version current again.

Access and forms: share_project invites people by email as editors, commenters or viewers;
change_role and revoke_access adjust that. create_form declares a lead form before its markup is
written, and list_leads reads its submissions: private data, summarized for the owner only and
never put on the page. connect_domain answers the DNS records to pass on exactly, then
verify_domain; stats answers visitor totals; set_site_mark shows or hides the made-with mark.

Making the page itself: a deck is one HTML page of slides, one idea each, its titles stating the
point; a report leads with the conclusion and draws its charts to scale; a proposal bounds the
scope, the price and one next step; a landing page carries one offer, its proof and one action; a
spec states the problem, the decision and what was rejected. Write as a person would for this
reader: concrete, front-loaded, no filler. Keep element ids stable across versions so reviewers'
remarks stay attached, and check the page on a phone and in dark mode. Diagrams show the real
mechanism; charts start from real numbers.

Never ask the user to paste a token in chat, and never claim a publish succeeded without the
tool's answer naming the version. The operator runs /birta from the keyboard: no argument reports
the connection, /birta tools lists what the server offers, and /birta <tool> <json> calls one."
  "What (help :birta) answers while the cell runs: the plugin's sixteen skills, condensed.")

;;; --- the operator's command --------------------------------------------------

(defun status-text ()
  "What this cell is connected to, without ever printing the token."
  (format nil "birta: ~a, token ~a, session ~a~%Tools: birta_tools (what the server offers), birta_call (call one); (help :birta) is the model's guide.~%Every call acts on the Birta projects you can open at birta.dev."
          (endpoint)
          (if (token) "set" "MISSING")
          (if *opened* "open" "not opened yet")))

(defun arguments-from (text)
  "TEXT after a tool name, as the JSON object it must be."
  (if (zerop (length text))
      (nlk:json-object)
      (let ((value (handler-case (nlk:decode-json text) (error () nil))))
        (if (hash-table-p value)
            value
            (fail "birta: the arguments after the tool name must be a JSON object, as in /birta create_project {\"name\": \"site\"}")))))

(defun command-answer (thunk)
  "THUNK's text, or the refusal it signalled as text: a command answers, it
does not throw."
  (handler-case (funcall thunk)
    (birta-error (condition) (format nil "~a" condition))))

(defun run-command (args session-id)
  "One /birta invocation: no argument reports the connection, `tools' lists
what the server offers, and `<tool> <json>' calls one."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (space (position #\Space text))
         (name (if space (subseq text 0 space) text))
         (rest (if space (nlk:trimmed (subseq text (1+ space))) "")))
    (cond ((zerop (length text)) (status-text))
          ((string-equal name "tools")
           (command-answer (lambda () (format-tool-list (list-tools) rest))))
          (t (command-answer (lambda () (format-answer (call-tool name (arguments-from rest)))))))))

;;; --- the cell ----------------------------------------------------------------

(nle:define-cell birta
  (:section ("birta")
    (:guide "paste the bearer token Birta's MCP server accepts, or name the variable holding it; url only for a proxy of your own; next step: /birta shows the connection")
    ("url" :string
     :doc "the MCP endpoint: https://birta.dev/mcp unless you go through a proxy of your own")
    ("token" :secret
     :doc "the bearer token Birta's MCP server accepts, from the sign-in at birta.dev")
    ("token_env" :env
     :doc "the environment variable holding the token, when it is not written here (BIRTA_TOKEN is read either way)")
    ("timeout_seconds" :integer :default 120 :min 1
     :doc "one MCP exchange's ceiling: a publish or a report can take a while")
    ("max_chars" :integer :default 6000 :min 200
     :doc "how much of one tool answer a result carries"))
  (:help :birta
         "birta: publish pages, sites and documents at a link, and apply the remarks left on them - (help :birta)"
         +primer+)
  (:command "birta" 'run-command
            :description "Birta: the connection, its tools, or one call"
            :argument-hint "[tools | TOOL [JSON]]"
            :session nil)
  (:tool "birta_tools" (nlk:contract 'list-request) #'tool-list)
  (:tool "birta_call" (nlk:contract 'call-request) #'tool-call)
  (:start (lambda ()
            (check-config)
            (nle:on-stop #'close-session))))
