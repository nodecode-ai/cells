;;;; cell.lisp --- the manual, /gurusup-brain, the section, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original ships two things: mcp.json, which points a client at
;;;; Brain's remote MCP server, and skills/using-brain/SKILL.md, which tells
;;;; the model to call Brain's init tool first and to base what it says about
;;;; the company on the evidence Brain returns. Here the server is wire.lisp,
;;;; and the skill is (help :gurusup-brain), the manual topic every request's
;;;; help section names. Nothing hooks the provider path: Brain is data, not a
;;;; model.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "gurusup-brain": {"token": "...", "endpoint": "https://mcp.brain.gurusup.com/mcp"}
;;;; A vetoed section ("enabled": false) installs nothing, and a section with
;;;; no token refuses at start, naming where to sign in.

(in-package #:nodecode-gurusup-brain)

(defparameter +manual+
  "GuruSup Brain is the company's memory, unified across the sources the account connects (Slack,
HubSpot, Notion, uploaded documents): decisions, people, customers and projects, with the source
behind each answer. It is reached through the nodecode-gurusup-brain cell, over Brain's own MCP
server:
  gurusup_brain_init    Brain's contract for this account. Call it first, and follow it.
  gurusup_brain_tools   what this account may call, one tool a line.
  gurusup_brain_call    one of those tools by name, with its arguments; some write to the
                         workspace or message a teammate, and the init contract says which.
Also, through eval: (gurusup-brain:init), (gurusup-brain:tool-list) and
(gurusup-brain:call \"search\" (nlk:json-object \"query\" \"acme renewal\")).
How to work: init first. Base anything said about the company on the evidence Brain returns, and
name the source it came from. When Brain returns no evidence, or too little, say so instead of
guessing -- an empty answer is an answer, and a made-up one is worse than none. Answers respect the
account's permissions, so what is missing may be what this account may not read."
  "What (help :gurusup-brain) answers while the cell runs.")

;;; --- /gurusup-brain ------------------------------------------------------------

(defun status ()
  "Where the token comes from and where the cell talks, in one line."
  (format nil "gurusup-brain: ~a; endpoint ~a" (token-source) (endpoint)))

(defun panel (summary)
  "The dialog bare /gurusup-brain answers: SUMMARY over the two free checks."
  (nle:list-dialog
   :title "GuruSup Brain" :summary summary
   :rows (list (list :label "What Brain offers"
                     :run (list :enter "/gurusup-brain tools")
                     :detail "the tools this account may call; costs nothing")
               (list :label "Brain's contract"
                     :run (list :enter "/gurusup-brain init")
                     :detail "its init tool, which says how to ask; costs nothing"))))

(defun run-slash (args session-id)
  "/gurusup-brain typed alone: where the token comes from, and its panel;
/gurusup-brain tools lists what the account may call, /gurusup-brain init
shows the contract."
  (declare (ignore session-id))
  (let ((verb (string-downcase (nlk:trimmed (or args "")))))
    (cond ((equal verb "") (let ((text (status))) (values text (panel text))))
          ((equal verb "tools") (tool-list))
          ((equal verb "init") (init))
          (t (values (format nil "gurusup-brain: ~a is not one of its choices" verb)
                     (panel (status)))))))

;;; --- the section and the tools --------------------------------------------------

(nle:define-cell gurusup-brain
  (:section ("gurusup-brain")
    (:guide "sign in to Brain at https://gurusup.com/brain/mcp and put the token that issues in token (or name its variable with token_env, or export GURUSUP_BRAIN_TOKEN); endpoint and timeout_seconds are optional")
    ("token" :secret
     :doc "the Brain token; empty reads token_env, else GURUSUP_BRAIN_TOKEN")
    ("token_env" :env
     :doc "the environment variable holding the token")
    ("endpoint" :string :default "https://mcp.brain.gurusup.com/mcp"
     :doc "the MCP endpoint; Brain's public server by default")
    ("timeout_seconds" :integer :default 45 :min 5
     :doc "one call's ceiling"))
  (:tool "gurusup_brain_init" *init-schema* 'tool-init)
  (:tool "gurusup_brain_tools" *tools-schema* 'tool-tools)
  (:tool "gurusup_brain_call" *call-schema* 'tool-call)
  (:help :gurusup-brain
   "gurusup-brain: your company's knowledge from Slack, HubSpot and Notion, over Brain's MCP server"
   +manual+)
  (:command "gurusup-brain" 'run-slash
   :description "GuruSup Brain: the token it uses; its tools and its contract"
   :session nil)
  (:start #'check-config))
