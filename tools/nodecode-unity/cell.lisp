;;;; cell.lisp --- the primer, the section, the /unity command, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin was a portable manifest, an mcp.json entry and a 5400-line
;;;; Python bridge between Hermes and MCP for Unity. The entry is this cell's
;;;; four tools; the bridge's plan/apply discipline is unity_plan and
;;;; unity_apply, and its guidance for the model is +PRIMER+.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "unity": {"url": "http://127.0.0.1:8080/mcp", "timeout_seconds": 120,
;;;;             "plan_ttl_minutes": 10, "max_chars": 6000}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-unity)

(defparameter +primer+
  "A Unity project open in the editor, through the nodecode-unity cell: four tools that
speak MCP to the MCP for Unity package in that editor. Nothing here needs Unity Hub,
uv, uvx or Python; the editor does need the MCP for Unity package, started on its HTTP
transport (Window > MCP for Unity shows the address; the section's url names it).
  unity_tools   the editor's own tools and what each does. query filters by word.
  unity_call    one editor tool with its arguments: tool, and arguments as one JSON
                object. Reads and one-off calls. A call whose paths fail the check is
                refused with nothing sent.
  unity_plan    a whole change as steps (tool and arguments each). It checks every path,
                answers the plan in words with a plan_id, and sends nothing. Show the
                plan to the user before you apply it.
  unity_apply   run the newest plan, once, within plan_ttl_minutes: steps in order, one
                at a time, and the first failure stops it and says which step. Later
                steps are not sent.
How to work: read with unity_tools and unity_call, then put a change of more than one
call into one unity_plan - scripts, objects and the save together - show it, and apply
it. Say a step worked only when the reply's first line says done; FAILED at step N of M
means exactly that. Paths must stay inside the project: no .., no hidden file, no key or
credential file, and no absolute path. A step that deletes Assets, Packages or
ProjectSettings, or that sends File/Quit, is refused. Play mode and tests run the
project's own scripts, so say so before you plan one. The operator can run /unity."
  "What (help :unity) answers while the cell runs: the plugin's skill in one topic.")

;;; --- what the cell is connected to --------------------------------------------------

(defun url-host (url)
  "URL's host, without a scheme, a port or a path."
  (let* ((start (if (search "://" url) (+ 3 (search "://" url)) 0))
         (rest (subseq url start))
         (end (or (position-if (lambda (character) (find character "/?#")) rest) (length rest)))
         (authority (subseq rest 0 end)))
    (if (and (plusp (length authority)) (char= (char authority 0) #\[))
        (subseq authority 0 (min (length authority)
                                  (1+ (or (position #\] authority) (1- (length authority))))))
        (subseq authority 0 (or (position #\: authority) (length authority))))))

(defun loopback-url-p (url)
  (member (string-downcase (url-host url)) '("127.0.0.1" "localhost" "[::1]" "::1") :test #'string=))

(defun check-config ()
  "Refuse a section that could not reach an editor on this machine."
  (let ((url (nonempty (setting :url))))
    (unless url
      (error 'nlk:config-refusal
             :detail (concatenate 'string
                                   "unity: set \"url\" to the MCP for Unity endpoint, "
                                   "http://127.0.0.1:<port>/mcp; Window > MCP for Unity in the editor shows the port")))
    (unless (loopback-url-p url)
      (error 'nlk:config-refusal
             :detail (format nil "unity: ~a is not on this machine: MCP for Unity listens on loopback, so the url is http://127.0.0.1:<port>/mcp"
                             url)))))

(defun status-text ()
  "What this cell is connected to, and what it answers, without a call."
  (handler-case
      (let ((tools (editor-tools :force t)))
        (format nil "unity: ~a, ~d tools (~{~a~^, ~})~%Plan a change with unity_plan, show it, then unity_apply."
                (endpoint) (length tools) (subseq (mapcar #'tool-name tools) 0 (min 8 (length tools)))))
    (unity-error (condition)
      (format nil "unity: ~a, not reachable: ~a~%Open the project in Unity and start Window > MCP for Unity."
              (endpoint) (unity-error-detail condition)))))

(defun run-command (args session-id)
  "One /unity invocation: with no argument, what the cell is connected to;
with one, the editor tools that word names."
  (declare (ignore session-id))
  (let ((word (nlk:trimmed (or args ""))))
    (if (zerop (length word))
        (status-text)
        (tool-tools (decode-tools-call (nlk:make-json-object "query" word))))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell unity
  (:section ("unity")
    (:guide "open the project in Unity, add the MCP for Unity package, start it on its HTTP transport and paste its address as url (http://127.0.0.1:<port>/mcp); next step: /unity shows what the editor answers")
    ("url" :string :required t
     :doc "the MCP for Unity HTTP endpoint inside the open editor, e.g. http://127.0.0.1:8080/mcp")
    ("timeout_seconds" :integer :default 120 :min 5
     :doc "one call's ceiling: a compile, a play session or a test run in the editor can take minutes")
    ("plan_ttl_minutes" :integer :default 10 :min 1
     :doc "how long a plan stays runnable after unity_plan answers it")
    ("max_chars" :integer :default 6000 :min 200
     :doc "how much of one answer the model reads"))
  (:help :unity
         "unity: the open Unity editor - list its tools, call one, and run a change as one approved plan"
         +primer+)
  (:command "unity" 'run-command
            :description "Unity: the editor's tools, or those matching a word"
            :argument-hint "WORD"
            :session nil)
  (:tool "unity_tools" (nlk:contract 'tools-call) #'tool-tools)
  (:tool "unity_call" (nlk:contract 'editor-call) #'tool-call)
  (:tool "unity_plan" (nlk:contract 'plan-call) #'tool-plan)
  (:tool "unity_apply" (nlk:contract 'apply-call) #'tool-apply)
  (:start (lambda ()
            (check-config))))
