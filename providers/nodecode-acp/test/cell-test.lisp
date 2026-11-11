;;;; cell-test.lisp --- the acp cell against a scripted ACP agent.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every agent here is test/fake-agent.py: a python3 program that speaks the
;;;; Agent Client Protocol on stdio and answers by its prompt's first word
;;;; (write, run, describe, plan, wait, count, crash, anything else echoed), so a turn
;;;; runs through the real wire, the real process and the real turn loop with
;;;; no network and no model. Its prompt count lives in a temp state file,
;;;; which is how a test sees that a restarted agent loaded the same session.

(in-package #:nodecode.test)

(define-test-slice "acp" "ACP-CELL-" :start nodecode-acp::start-cell)

(define-cell-lifecycle-tests "acp"
  (:hooks 'nle::agent-loop :credential 'nle::configured-provider-inventory 'nle::model-listed-p
          'nle::tool-arguments-activity 'nle::row-tool-title 'nle::row-tool-commands)
  (:refused ("agents" 5) ("ask_seconds" 0) ("idle_minutes" "soon")))

(defun acp-fake-path ()
  (namestring (asdf:system-relative-pathname "nodecode-acp" "test/fake-agent.py")))

(defun acp-fake-agents (state &rest more)
  "An `agents' object holding the fake agent, its prompt count kept in STATE,
and MORE (name object) pairs."
  (apply #'nlk:make-json-object
         "fake" (nlk:json-object "command" "python3" "args" (vector (acp-fake-path))
                                 "title" "Fake" "env" (nlk:json-object "FAKE_ACP_STATE" (namestring state)))
         more))

(defun call-with-acp-ask (stub thunk)
  "THUNK with the core's ASK-OPERATOR answered by STUB, put back after."
  ;; By name, so this reads on a core released before the question board.
  (let* ((symbol (intern "ASK-OPERATOR" '#:nodecode.evolved))
         (saved (and (fboundp symbol) (fdefinition symbol))))
    (setf (fdefinition symbol) stub)
    (unwind-protect (funcall thunk)
      (if saved (setf (fdefinition symbol) saved) (fmakunbound symbol)))))

(defmacro with-acp-session ((session-id directory &key (agent "fake") (state (gensym "STATE"))) &body body)
  "BODY with the cell started on the fake agent, a temp store holding
SESSION-ID on DIRECTORY (a fresh temp folder) aimed at acp/AGENT, and STATE
the fake's prompt-count file."
  `(with-temp-workspace (,directory)
     (with-temp-file (,state)
       (with-temp-store ()
         (nlk:create-session :id ,session-id :cwd (namestring ,directory))
         (nlk:record-session-model-selection ,session-id :provider "acp" :model ,agent)
         (with-cell-stop ((acp-start "agents" (acp-fake-agents ,state)))
           (nlk:with-cleanup ((nodecode-acp::stop-links "test cleanup"))
             ,@body))))))

(defun acp-turn (session-id input)
  "Run INPUT as a turn of SESSION-ID."
  (nle:run input :session (nle:intern-session session-id)))

(defun acp-said (session-id)
  "Every assistant message SESSION-ID's turns committed, oldest first."
  (loop for event in (session-events session-id :kind "turn.assistant_message_completed")
        collect (nlk:json-value (nlk:event-payload event) :string "message" "content")))

(defun acp-ends (session-id)
  "The kinds of SESSION-ID's turn ends, oldest first."
  (loop for event in (session-events session-id)
        when (member (nlk:event-kind event) '("turn.completed" "turn.failed" "turn.cancelled") :test #'equal)
          collect (nlk:event-kind event)))

;;; --- the agents and the catalog -------------------------------------------------

(deftest acp-cell-lists-the-presets-and-the-section-agents ()
  (with-temp-file (state)
    (with-cell-stop ((acp-start "agents" (acp-fake-agents state "gemini" (nlk:json-object "enabled" nil)
                                                          "codex" (nlk:json-object "args" #("my-codex")))))
      (let ((names (mapcar #'nodecode-acp::agent-name (nodecode-acp::setting :agents))))
        (is (member "claude-code" names :test #'equal) "the Claude Code preset")
        (is (member "fake" names :test #'equal) "a section agent")
        (is (not (member "gemini" names :test #'equal)) "\"enabled\": false takes a preset off")
        (let ((codex (nodecode-acp::find-agent "codex")))
          (is (equal '("my-codex") (nodecode-acp::agent-args codex)) "a section entry changes a preset's args")
          (is (equal "npx" (nodecode-acp::agent-command codex)) "and keeps what it does not name")))
      (let ((rows (remove-if-not (lambda (row) (member (getf row :model-id) '("fake" "claude-code") :test #'equal))
                                 (nodecode-acp::model-rows))))
        (is (equal '("acp" "acp") (mapcar (lambda (row) (getf row :provider-id)) rows)) "/models lists acp/<agent>"))
      (with-stubbed-fdefinition (nle::models-catalog-table ()
                                 (nlk:json-object "relay" (nlk:json-object "models" (nlk:json-object "cc-1" (nlk:json-object "name" "claude-code")))))
        (is (equal "claude-code" (nle::check-model-id "acp" "claude-code")) "an agent's name is an id, even where a catalog calls a model that"))
      (is (not (nle::model-listed-p "acp" "nobody")) "a name no agent has is not")
      (is (eq :next (nodecode-acp::credential '(:provider "openai") (constantly :next))) "other providers keep their key")
      (is (nodecode-acp::credential '(:provider "acp") (constantly nil)) "acp needs none"))))

(deftest acp-cell-refuses-a-malformed-agent ()
  (signals-error error (acp-start "agents" (nlk:json-object "x" (nlk:json-object "args" "not a list")))))

;;; --- a turn ---------------------------------------------------------------------------

(deftest acp-cell-runs-a-turn-in-the-agent ()
  (with-acp-session ("acp-hello" dir)
    (acp-turn "acp-hello" "hello there")
    (is (equal '("You said: hello there") (acp-said "acp-hello")) "the agent's words are the answer")
    (let ((message (nlk:event-payload (session-event "acp-hello" :kind "turn.assistant_message_completed"))))
      (is (equal "Thinking about hello" (nlk:json-value message :string "message" "reasoning_content")) "its thinking rides as reasoning"))
    (let ((usage (nlk:event-payload (session-event "acp-hello" :kind "turn.usage"))))
      (is (eql 1000 (nlk:json-value usage :integer "input-tokens")) "the agent's token counts")
      (is (equal "acp" (nlk:json-value usage :string "provider")))
      (is (nlk:json-value usage :boolean "cost_known") "and what the agent says it cost"))
    (is (equal '("turn.completed") (acp-ends "acp-hello")))
    ;; A turn RUN began before the cell's hook was in reads its words from
    ;; the turn loop's messages: the newest user message's text, or its parts'.
    (let ((session (nle:intern-session "acp-hello")))
      (setf (nle::session-messages session)
            (list (nlk:json-object "role" "user" "content" "first")
                  (nlk:json-object "role" "assistant" "content" "ok")
                  (nlk:json-object "role" "user" "content" "hello there")))
      (is (equal "hello there" (nodecode-acp::last-user-text session)) "the newest user message")
      (setf (nle::session-messages session)
            (list (nlk:json-object "role" "user" "content"
                                   (vector (nlk:json-object "type" "text" "text" "look")
                                           (nlk:json-object "type" "image_url")))))
      (is (equal "look" (nodecode-acp::last-user-text session)) "its text parts")
      (setf (nle::session-messages session) nil))))

(deftest acp-cell-shows-the-agents-plan-in-its-thinking ()
  (with-acp-session ("acp-plan" dir)
    (acp-turn "acp-plan" "plan")
    (let ((message (nlk:event-payload (session-event "acp-plan" :kind "turn.assistant_message_completed"))))
      (is (equal "Planned." (nlk:json-value message :string "message" "content")))
      (let ((plan (format nil "Thinking about plan~%~%Plan:~%- [x] Read the code~%- [~~] Change it"))
            (thinking (or (nlk:json-value message :string "message" "reasoning_content") "")))
        (is (search plan thinking) "the plan is its own checklist under the thinking")))))

(deftest acp-cell-shows-a-command-the-agent-ran ()
  (with-acp-session ("acp-run" dir)
    (acp-turn "acp-run" "run echo hi-from-the-agent")
    (let ((result (session-event "acp-run" :type 'nlk:tool-result-event)))
      (is result "the agent's call is a tool row")
      (is (search "hi-from-the-agent" (or (and result (nlk:tool-result-event-result result)) "")) "with its output"))
    (is (equal '("turn.completed") (acp-ends "acp-run")))))

(deftest acp-cell-words-a-command-the-shell-has-no-family-for ()
  "A command reads by its family where one names it (ls lists), else by its
line (echo, false) -- running and failed alike, never as `acp'."
  (with-acp-session ("acp-echo" dir)
    (acp-turn "acp-echo" "run echo rotation ok")
    (acp-turn "acp-echo" "run false")
    (acp-turn "acp-echo" "run ls -la")
    (let ((calls (mapcar #'nlk:tool-call-event-arguments
                         (session-events "acp-echo" :type 'nlk:tool-call-event))))
      (is (= 3 (length calls)) "each command is a call")
      (loop for (command error-p expected) in '(("echo rotation ok" nil "echo rotation ok")
                                                ("false" t "false")
                                                ("ls -la" nil "Listed"))
            for arguments in calls
            for words = (nle::tool-arguments-activity "acp" arguments :error-p error-p)
            for title = (and words (nle::activity-title words))
            for verb = (nle::tool-activity-verb "acp" arguments)
            do (is (and title (search expected title)) (format nil "~a reads as ~s: ~s" command expected title))
               (is (not (search "acp" verb)) (format nil "~a runs as its words, not as running acp: ~s" command verb))))))

(deftest acp-cell-words-a-call-as-its-agent-describes-it ()
  "A call carries its agent's own words as its description, which its row and
its running verb read first: a command the first line of the description its
input gives, a call the shell has no words for (a subagent) its title."
  (with-acp-session ("acp-said" dir)
    (acp-turn "acp-said" "describe echo hello")
    (let ((calls (mapcar #'nlk:tool-call-event-arguments
                         (session-events "acp-said" :type 'nlk:tool-call-event))))
      (is (= 2 (length calls)) "a command and a subagent")
      (destructuring-bind (&optional command subagent) calls
        (let ((said (and command (nlk:json-value (nlk:decode-json command) :string "description")))
              (task (and subagent (nlk:json-value (nlk:decode-json subagent) :string "description"))))
          (is (equal "Say hello" said) "the command carries its description's first line")
          (is (equal "Check the callers" task) "the subagent carries its title"))
        (let ((words (nle::tool-arguments-activity "acp" command)))
          (is (equal "Say hello" (and words (nle::activity-note words))) "as its note")
          (is (equal "Say hello" (nle::tool-activity-verb "acp" command)) "which it runs as"))
        (let ((words (nle::tool-arguments-activity "acp" subagent)))
          (is (equal "Check the callers" (and words (nle::activity-title words))) "and reads as it"))))))

(deftest acp-cell-asks-before-the-agent-writes ()
  (with-acp-session ("acp-write" dir)
    (let ((asked nil))
      (call-with-acp-ask
       (lambda (title &rest keys) (setf asked (list* title keys)) "Yes")
       (lambda () (with-workspace-captures (t) (acp-turn "acp-write" "write note.txt kept words"))))
      (is (and asked (search "Fake wants to write note.txt" (first asked))) "the card names the agent and the call")
      (is (equal '("Yes" "Yes, always" "No") (getf (rest asked) :choices)) "its choices are the agent's options")
      (is (equal "Create note.txt" (getf (rest asked) :detail)) "its detail says what changes")
      (is (equal (format nil "kept words~%") (uiop:read-file-string (merge-pathnames "note.txt" dir))) "Yes: the agent wrote it")
      (let ((result (session-event "acp-write" :type 'nlk:tool-result-event)))
        (is (nlk:json-value (nlk:tool-result-event-metadata result) :object "workspace_diff") "the row carries the diff")))))

(deftest acp-cell-no-answer-is-the-agents-own-no ()
  (with-acp-session ("acp-deny" dir)
    (call-with-acp-ask (lambda (title &rest keys) (declare (ignore title keys)) (values nil :timeout))
                       (lambda () (acp-turn "acp-deny" "write never.txt words")))
    (is (not (probe-file (merge-pathnames "never.txt" dir))) "nothing written")
    (is (search "No" (format nil "~{~a~}" (acp-said "acp-deny"))) "the agent hears no")
    (is (equal '("turn.completed") (acp-ends "acp-deny")))))

(deftest acp-cell-stop-cancels-the-prompt-and-keeps-what-was-said ()
  (with-acp-session ("acp-stop" dir)
    (let* ((turn (nlk:admit-turn "acp-stop" "stop-cmd" "wait"))
           (worker (nlk:spawn "acp-stop-worker"
                     (handler-case (nle:run "wait" :session (nle:intern-session "acp-stop")
                                                 :command-id "stop-cmd" :admitted-turn turn)
                       (nlk:turn-cancelled-condition () :cancelled)))))
      (is-eventually (gethash "acp-stop" nodecode-acp::*links*) "the agent is up" :seconds 30)
      (sleep 0.5)
      (let ((asked (get-internal-real-time)))
        (nlk:request-cancel-turn "acp-stop" (nlk:durable-turn-turn-id turn))
        (bt2:join-thread worker)
        (is (< (/ (- (get-internal-real-time) asked) internal-time-units-per-second) 5) "the stop is quick"))
      (is (equal '("turn.cancelled") (acp-ends "acp-stop")))
      (is (search "Working on it" (format nil "~{~a~}" (acp-said "acp-stop"))) "what the agent said before it is kept"))))

(deftest acp-cell-a-crash-fails-the-turn-and-the-next-loads-the-session ()
  (with-acp-session ("acp-crash" dir)
    (acp-turn "acp-crash" "count")
    (is (eq :failed (handler-case (acp-turn "acp-crash" "crash") (error () :failed))) "a crash fails the turn")
    (let* ((failed (session-event "acp-crash" :kind "turn.failed"))
           (detail (or (and failed (nlk:json-value (nlk:event-payload failed) :string "detail")) "")))
      (is (search "crashed on purpose" detail) "the failure quotes what the agent wrote"))
    (acp-turn "acp-crash" "count")
    (nodecode-acp::stop-links "idle")
    (acp-turn "acp-crash" "count")
    (let ((said '("This is prompt 1 of this session." "About to crash" "This is prompt 3 of this session."
                  "This is prompt 4 of this session.")))
      (is (equal said (acp-said "acp-crash")) "after a crash and after a stop the agent goes on in the same session"))
    (is (equal '("turn.completed" "turn.failed" "turn.completed" "turn.completed") (acp-ends "acp-crash")))))
