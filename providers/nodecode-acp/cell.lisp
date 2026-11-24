;;;; cell.lisp --- the cell: a provider whose models are agents.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; acp/<agent> is picked like any model, in /models, the page's picker or
;;;; a start_turn's target. Its lane is never asked for a round: the turn
;;;; loop's own entry is hooked, and a turn on acp runs in the agent
;;;; (turn.lisp) while every other turn runs as it did. The hooks:
;;;;
;;;;   AGENT-LOOP                      a turn on acp is one session/prompt
;;;;   :CREDENTIAL                     acp needs no key: each agent signs in
;;;;                                   with its own CLI
;;;;   CONFIGURED-PROVIDER-INVENTORY   /models lists acp/<agent> for each one
;;;;   MODEL-LISTED-P                  and a pick of one is an id, never a
;;;;                                   catalog's display name
;;;;   TOOL-ARGUMENTS-ACTIVITY         an agent's call in the shell's own
;;;;                                   words: Read, Edited, Ran, Searched,
;;;;                                   led by the agent's own (its note)
;;;;   ROW-TOOL-TITLE, -COMMANDS       the agent's own title where those words
;;;;                                   have none; a command as `$ command'
;;;;
;;;; Config, a top-level `acp' section:
;;;;   "acp": {"agents": {"claude-code": {"mode": "acceptEdits"},
;;;;                      "opencode": {"command": "opencode", "args": ["acp"]}},
;;;;           "ask_seconds": 600, "idle_minutes": 30}

(in-package #:nodecode-acp)

(defun read-settings (values table)
  "The section's declared VALUES, with the agents: the presets and the
section's `agents' object laid over them, refused here when malformed."
  (multiple-value-bind (agents present) (nlk:section-value table "agents")
    (when (and present (not (hash-table-p agents)) (not (eq agents :null)))
      (refuse "acp: agents must be an object of agent names"))
    (list* :agents (configured-agents (and present (hash-table-p agents) agents))
           values)))

(defun acp-turn-p ()
  "Whether the running turn's target is an agent."
  (equal nle::*provider* +provider+))

(defun keep-input (next input &rest keys)
  "RUN advice: the turn's INPUT, kept for the agent's prompt (the turn loop
is handed the session alone, its history not yet read)."
  (let ((*input* input))
    (apply next input keys)))

(defun turn-loop (next session &rest keys)
  "AGENT-LOOP advice: a turn on acp runs in its agent."
  (if (acp-turn-p)
      (run-turn session)
      (apply next session keys)))

(defun credential (op next)
  "The :CREDENTIAL answer for acp: none is needed, each agent's login is its
own CLI's."
  (if (equal (getf op :provider) +provider+)
      (nle:make-credential +provider+ :oauth)
      (funcall next op)))

(defun model-rows ()
  "The /models rows acp offers: one per agent."
  (loop for agent in (setting :agents)
        collect (list :provider-id +provider+
                      :provider-display "Agents (ACP)"
                      :model-id (agent-name agent)
                      :context-window nil)))

(defun listed (next provider model)
  "MODEL-LISTED-P advice: an agent's name is an id acp lists, taken on sight --
so `claude-code', which a catalog also knows as a model's display name, is
never refused as one."
  (or (and (equal provider +provider+) (stringp model) (find-agent model) t)
      (funcall next provider model)))

(defun inventory (next &rest arguments)
  "CONFIGURED-PROVIDER-INVENTORY advice: acp among the providers, each agent
among the models."
  (multiple-value-bind (ids models) (apply next arguments)
    (values (sort (adjoin +provider+ ids :test #'string-equal) #'string<)
            (append models (model-rows)))))

;;; --- an agent's call in the shell's words -------------------------------------

(defun call-object (arguments)
  "An acp call's ARGUMENTS as an object, or NIL while they stream."
  (let ((value (and (stringp arguments) (ignore-errors (nlk:decode-json arguments)))))
    (and (hash-table-p value) value)))

(defun text-of (object key)
  (let ((value (gethash key object))) (and (stringp value) (plusp (length value)) value)))

(defun call-entries (call)
  "The entries CALL's kind and input make in the shell's own words: a read
reads, an edit edits, a search searches, a fetch fetches, and a command reads
as a bash call with that command does -- by its family where one names it
(ls lists), else by its line (echo, false). NIL for a call that names none."
  (let ((kind (text-of call "kind"))
        (path (text-of call "path"))
        (command (text-of call "command"))
        (pattern (text-of call "pattern")))
    (cond ((and (equal kind "read") path) (list (list :read path nil)))
          ((and (member kind '("edit" "delete" "move") :test #'equal) path)
           (list (list (if (eq (gethash "new" call) t) :write :edit) path nil)))
          ((and (equal kind "search") pattern) (list (list :search pattern nil)))
          ((and (equal kind "fetch") (text-of call "url")) (list (list :fetch (text-of call "url") nil)))
          ;; A web search is a fetch with a query and no page.
          ((and (equal kind "fetch") pattern) (list (list :search pattern nil)))
          (command
           (or (nle::shell-command-entries command)
               (let ((line (string-trim '(#\Space #\Tab #\Newline #\Return) command)))
                 (and (plusp (length line)) (list (list :command line nil)))))))))

(defun activity (next name arguments &rest keys &key metadata error-p &allow-other-keys)
  "TOOL-ARGUMENTS-ACTIVITY advice: an acp call's activity -- its entries in the
shell's words (CALL-ENTRIES) plus every file its result changed, and the
agent's own words for it (its `description') as its note, which a row, the
band and a digest read first."
  (if (not (equal name "acp"))
      (apply next name arguments keys)
      (let ((call (call-object arguments)))
        (when call
          (let ((entries (call-entries call))
                (command (text-of call "command"))
                (words (text-of call "description")))
            (let ((diff (and (hash-table-p metadata) (gethash "workspace_diff" metadata))))
              (when (hash-table-p diff)
                (loop for file across (or (gethash "files" diff) #())
                      for changed = (and (hash-table-p file) (text-of file "path"))
                      when (and changed (notany (lambda (entry) (equal (second entry) changed)) entries))
                        do (setf entries (append entries (list (list :edit changed nil)))))))
            (when (or entries words)
              (nle::activity-from-entries entries
                                          :note words
                                          :head (and command (not (text-of call "path")) command)
                                          :error-p error-p)))))))

(defun title (next row activity)
  "ROW-TOOL-TITLE advice: an acp call the shell has no words for is titled
by its agent's own title for it."
  (or (funcall next row activity)
      (and (equal (nle::transcript-row-tool-name row) "acp")
           (let ((call (call-object (nle::transcript-row-tool-args row))))
             (and call (text-of call "title"))))))

(defun commands (next row)
  "ROW-TOOL-COMMANDS advice: the command an acp call ran, as its card's
`$ ' line."
  (if (equal (nle::transcript-row-tool-name row) "acp")
      (let* ((call (call-object (nle::transcript-row-tool-args row)))
             (command (and call (text-of call "command"))))
        (and command (vector command)))
      (funcall next row)))

;;; --- the lane ------------------------------------------------------------------

(defun no-round (context &key on-part)
  "The acp lane's stream: never asked for a turn's round, which the agent
runs; anything else that would ask it -- a title, a summary -- is told why not."
  (declare (ignore context on-part))
  (error 'nle::provider-config-error
         :detail "acp agents answer only a session's turns, in the agent: pick a model for this"))

(defun register-lane ()
  (nle::register-provider-lane
   (nle::make-provider-lane :name +provider+ :stream-symbol 'no-round
                            :family :openai-completions
                            :default-endpoint "http://127.0.0.1/acp" :path "")))

(defun unregister-lane ()
  (setf nle::*provider-lanes*
        (remove +provider+ nle::*provider-lanes* :key #'nle::provider-lane-name :test #'equal)))

(defun start ()
  "The lane, the idle reaper, and every agent stopped when the cell stops."
  (register-lane)
  (let ((reaper (nlk:worker-start "acp idle" #'reap-idle :wake 60)))
    (nle:on-stop (lambda ()
                   (nlk:worker-stop reaper)
                   (stop-links)
                   (unregister-lane)))))

(nle:define-cell acp
  (:section ("acp")
    (:guide "agents.<name> adds an agent or changes a preset (claude-code, codex, gemini): command, args, env, title, mode; \"enabled\": false takes one off")
    ("start_seconds" :integer :default 120 :min 1
     :doc "how long an agent may take to start and open its session (npx fetches an adapter on first use)")
    ("ask_seconds" :integer :default 600 :min 1
     :doc "how long an agent's permission card waits for an answer before the agent is told no")
    ("idle_minutes" :integer :default 30 :min 1
     :doc "stop an agent no turn has used this long; its session is loaded again at the next turn"))
  (:settings #'read-settings)
  (:start #'start)
  (:hook :credential #'credential)
  (:hook 'nle::run #'keep-input)
  (:hook 'nle::agent-loop #'turn-loop)
  (:hook 'nle::configured-provider-inventory #'inventory)
  (:hook 'nle::model-listed-p #'listed)
  (:hook 'nle::tool-arguments-activity #'activity)
  (:hook 'nle::row-tool-title #'title)
  (:hook 'nle::row-tool-commands #'commands))
