;;;; support.lisp --- the import lane's runner, its scratch box, and the fixtures.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; These register into the SAME nodecode.test registry (the core DEFTEST,
;;;; with its hermetic machine-state posture) under the IMPORT- name prefix
;;;; the whole import group shares; RUN-IMPORT-TESTS runs that slice.
;;;;
;;;; Every test runs over a scratch BOX — a $HOME of its own
;;;; (NLK:*AGENT-HOME*) carrying one or more foreign homes — and a scratch
;;;; organism home: a scratch shared config, auth.json and secrets folder,
;;;; the folders a plan would install recorded instead of installed, no
;;;; foreign gateway running unless the test says so, the takeover's stop
;;;; counted instead of run, and no watch thread — a test drives WATCH-STEP
;;;; itself.
;;;;
;;;; Four of the fixtures are worlds nobody here has. They are written from
;;;; each harness's own public documentation and read through the shape
;;;; readers with no world-specific code at all, which is the whole claim
;;;; this folder makes: a world is a row in a table, and a home nobody on
;;;; this box owns is still read. Every secret in them is the literal
;;;; string FAKE-not-a-key-<something>.

(in-package #:nodecode.test)

(define-test-slice "import" "IMPORT-")

(defvar *import-running* '()
  "What the running probe answers.")

(defvar *import-refreshed* '()
  "The folders the plan asked to restart so they read the config it wrote,
newest first.")

(defvar *import-stopped* 0
  "How many times a takeover stopped a foreign gateway.")

(defmacro with-import-runtime ((home box) &body body)
  "Run BODY with the folder installed over the scratch organism home HOME,
its cell folder and knowledge cell under HOME/cells/, and BOX as the $HOME
its worlds are looked for under."
  `(with-temp-directory (,home "import-home")
     (with-temp-directory (,box "import-box")
       (with-temp-file (nodecode.evolved::*shared-config-path* :type "jsonc" :contents "{}" :setf t)
        (with-saved-globals ((nlk::*cells-directory* (merge-pathnames "cells/" ,home))
                             (nlk::*layer-name* "tester")
                             (nlk::*layer-revision* nil))
         (clrhash nlk::*knowledge-memo*)
         (let ((nlk::*agent-home* (uiop:ensure-directory-pathname ,box))
               ;; A models.dev catalog of three providers, pinned: a gate never reaches the network.
               (nle::*models-catalog-path*
                 (namestring (import-write ,home "models.dev.json"
                                           "{\"anthropic\": {\"id\": \"anthropic\", \"env\": [\"ANTHROPIC_API_KEY\"],
                  \"npm\": \"@ai-sdk/anthropic\", \"api\": null,
                  \"models\": {\"claude-opus-5\": {\"id\": \"claude-opus-5\", \"tool_call\": true, \"release_date\": \"2026-07-24\"},
                               \"claude-opus-5-5\": {\"id\": \"claude-opus-5-5\", \"tool_call\": true, \"release_date\": \"2026-09-22\"},
                               \"claude-sonnet-5\": {\"id\": \"claude-sonnet-5\", \"tool_call\": true, \"release_date\": \"2026-06-29\"}}},
   \"openai\": {\"id\": \"openai\", \"env\": [\"OPENAI_API_KEY\"],
               \"npm\": \"@ai-sdk/openai\", \"api\": null,
               \"models\": {\"gpt-5.5\": {\"id\": \"gpt-5.5\"}, \"gpt-5.6\": {\"id\": \"gpt-5.6\"}}},
   \"openrouter\": {\"id\": \"openrouter\", \"env\": [\"OPENROUTER_API_KEY\"],
                   \"npm\": \"@ai-sdk/openai-compatible\",
                   \"api\": \"https://openrouter.ai/api/v1\",
                   \"models\": {\"anthropic/claude-opus-5-5\": {\"id\": \"anthropic/claude-opus-5-5\", \"tool_call\": true, \"release_date\": \"2026-09-22\"}}}}")))
               (nle::*models-catalog-memo* nil)
               (nle::*models-catalog-probe* nil)
               (nik::*vocabulary* nil)
               (nle::*auth-file-path* (merge-pathnames "auth.json" ,home))
               (nle::*secrets-directory* (merge-pathnames "secrets/" ,home))
               (nik::*ensure-cell*
                 (lambda (name &key fresh)
                   (when fresh (push name *import-refreshed*))
                   t))
               (nik::*running-probe* (lambda (units) (declare (ignore units)) *import-running*))
               (nik::*gateway-stop*
                 (lambda (unit)
                   (incf *import-stopped*)
                   (setf *import-running* '())
                   (format nil "~a stopped and disabled" unit)))
               (nik::*start-watch* (constantly nil))
               (*import-refreshed* '())
               (*import-running* '())
               (*import-stopped* 0))
           (nlk:with-cleanup ((nik::stop-cell))
             (nik:start-cell (nlk:json-object "import" (nlk:make-json-object)))
             ;; The scratch organism home, which is not $HOME here: the box is.
             (setf (getf nik::*import* :home) (uiop:ensure-directory-pathname ,home))
             ,@body)))))))

(defmacro with-cron (() &body body)
  "BODY with the cron cell started without its ticker, stopped on unwind."
  `(let ((nodecode-cron::*ticker* nil))
     (nlk:with-cleanup ((nodecode-cron::stop-cell))
       (nodecode-cron:start-cell (nlk:json-object "cron" (nlk:make-json-object)))
       ,@body)))

(defun kept-text (name)
  "The text of the definition NAME the fixture's knowledge cell keeps, or NIL."
  (nlk:read-text (merge-pathnames (format nil "tester-memory/~a.lisp" name) nlk::*cells-directory*)))

(defun import-write (root relative text)
  "TEXT into ROOT/RELATIVE, directories made. => the path."
  (write-temp-file (merge-pathnames relative (uiop:ensure-directory-pathname root)) text))

;;; --- the fixture secrets ------------------------------------------------------
;;; Every one of these is long enough to pass the credential shape guard and
;;; says what it is, so a fixture that leaks into a report is obvious.

(defparameter +fixture-key+ "FAKE-not-a-key-0123456789abcdef")
(defparameter +fixture-key-2+ "FAKE-not-a-key-fedcba9876543210")
(defparameter +fixture-token+ "FAKE-not-a-token-123456-abcdefgh")
(defparameter +fixture-refresh+ "FAKE-not-a-refresh-token-abcdefgh")

;;; --- fixture: a Hermes-shaped home --------------------------------------------

(defparameter +fixture-t0+ 1782252303
  "2026-06-23T22:05:03Z, the fixture's first session's start.")

(defun make-hermes-fixture (box &aux (root (merge-pathnames ".hermes/" (uiop:ensure-directory-pathname box))))
  "A Hermes home under BOX/.hermes/. => the home as a directory pathname."
  (import-write root "config.yaml" (format nil "model:
  default: gpt-5.5
  provider: custom:subrouter-1
providers: {}
toolsets:
- hermes-cli
telegram:
  reactions: true
discord:
  require_mention: true
  reactions: false
custom_providers:
- name: subrouter-1
  base_url: https://ai.example.test/v1
  api_key: ~a
  api_mode: chat_completions
  models:
    gpt-5.5:
      name: gpt-5.5
mcp_servers:
  github:
    command: npx
    args: [\"-y\", \"@modelcontextprotocol/server-github\"]
    env:
      GITHUB_PERSONAL_ACCESS_TOKEN: FAKE-not-a-token-github
      LOG_LEVEL: info
    cwd: /tmp
  docs:
    url: https://docs.example.test/mcp
    headers:
      Authorization: Bearer FAKE-not-a-token-docs
      X-Client: hermes
" +fixture-key+))
  (import-write root ".env" (format nil "# keys
OPENROUTER_API_KEY=~a
TELEGRAM_BOT_TOKEN=~a
TELEGRAM_ALLOWED_USERS=8071918233,@someone
TELEGRAM_HOME_CHANNEL=-100123:77
export DISCORD_BOT_TOKEN=\"~a\"
DISCORD_ALLOWED_USERS=42
" +fixture-key-2+ +fixture-token+ +fixture-token+))
  (import-write root "SOUL.md" "Speak plainly. Never flatter.")
  (import-write root "memories/MEMORY.md"
                (format nil "Holographic is the memory provider; built-in memory remains active.~%§~%The repo builds with `just build`; tests take a minute.~%"))
  (import-write root "memories/USER.md" (format nil "Prefers short answers.~%"))
  (import-write root "skills/productivity/notes/SKILL.md"
                (format nil "---~%name: notes~%description: Keeps notes.~%---~%~%# Notes~%"))
  (import-write root "skills/productivity/notes/scripts/run.py" "print('hi')")
  (import-write root "skills/.archive/old/SKILL.md" (format nil "---~%name: old~%---~%"))
  (import-write root "profiles/coder/config.yaml" (format nil "model:~%  default: gpt-5.5~%"))
  (import-write root "profiles/.deleted/gone/config.yaml" (format nil "model: {}~%"))
  (import-write root "cron/jobs.json"
                "{\"jobs\": [
 {\"id\": \"morning\", \"name\": \"morning\", \"prompt\": \"Summarize overnight mail\", \"schedule\": {\"kind\": \"cron\", \"expr\": \"0 9 * * 1-5\"}, \"enabled\": true, \"model\": \"gpt-5.5\"},
 {\"id\": \"tick\", \"name\": \"tick\", \"prompt\": \"Check the queue\", \"schedule\": {\"kind\": \"interval\", \"minutes\": 30}, \"enabled\": false},
 {\"id\": \"script-only\", \"name\": \"script-only\", \"script\": \"echo hi\", \"schedule\": {\"kind\": \"cron\", \"expr\": \"* * * * *\"}}
]}")
  ;; A session store: two answered prompts around an aside and a tool round, an unanswered one, a chat's.
  (let ((db (sqlite:connect (namestring (merge-pathnames "state.db" root)))))
    (nlk:with-cleanup ((sqlite:disconnect db))
      (sqlite:execute-non-query
       db "create table sessions (id text primary key, source text not null, title text, model text, started_at real not null, ended_at real, cwd text)")
      (sqlite:execute-non-query
       db "create table messages (id integer primary key autoincrement, session_id text not null, role text not null, content text, tool_calls text, timestamp real not null)")
      (flet ((session (id source title model started)
               (sqlite:execute-non-query
                db "insert into sessions (id, source, title, model, started_at, cwd) values (?, ?, ?, ?, ?, ?)"
                id source title model started "/home/op/project"))
             (message (session role content at &optional tool-calls)
               (sqlite:execute-non-query
                db "insert into messages (session_id, role, content, tool_calls, timestamp) values (?, ?, ?, ?, ?)"
                session role content tool-calls at)))
        (session "20260623_150503_55c2a9" "cli" "Casual Greeting" "gpt-5.5"
                 (float +fixture-t0+ 1d0))
        (message "20260623_150503_55c2a9" "user"
                 "[Note: model was just switched from  to gpt-5.5 via /model]"
                 (+ +fixture-t0+ 0.5d0))
        (message "20260623_150503_55c2a9" "user" "yo" (+ +fixture-t0+ 1))
        (message "20260623_150503_55c2a9" "assistant" "Hey! What's up?" (+ +fixture-t0+ 3))
        (message "20260623_150503_55c2a9" "user" "what memory options" (+ +fixture-t0+ 7))
        (message "20260623_150503_55c2a9" "assistant" nil (+ +fixture-t0+ 8) "[{\"id\": \"c1\"}]")
        (message "20260623_150503_55c2a9" "tool" "{\"success\": true}" (+ +fixture-t0+ 9))
        (message "20260623_150503_55c2a9" "assistant" "You have two options." (+ +fixture-t0+ 12))
        (session "20260623_151443_bacfba" "cli" nil "gpt-5.5"
                 (float (+ +fixture-t0+ 600) 1d0))
        (message "20260623_151443_bacfba" "user" "hello?" (+ +fixture-t0+ 601))
        (session "20260623_151650_574118" "telegram" "One and the other" "claude-opus-5"
                 (float (+ +fixture-t0+ 700) 1d0))
        (message "20260623_151650_574118" "user" "a" (+ +fixture-t0+ 701))
        (message "20260623_151650_574118" "assistant" "b" (+ +fixture-t0+ 702)))))
  root)

;;; --- fixture: Openclaw, from its own docs -------------------------------------
;;; docs/concepts/agent-workspace.md (the workspace layout),
;;; src/config/types.auth.ts (auth profiles keyed `<provider>:<label>', a
;;; type of api or oauth) and docs/gateway/config-channels (a bot under
;;; channels.<platform>: its token, and who may talk as `allowFrom'). Nobody
;;; on the box this was written on has Openclaw; every fact below is read by
;;; the same shape readers.

(defun make-openclaw-fixture (box &key (token +fixture-token+) discord
                              &aux (root (merge-pathnames ".openclaw/" (uiop:ensure-directory-pathname box))))
  "An Openclaw home under BOX/.openclaw/, its Telegram bot TOKEN's and, when
DISCORD names a token, a Discord bot beside it."
  (import-write
   root "openclaw.json"
   (format nil "{
  \"agents\": {
    \"main\": {
      \"model\": \"anthropic/claude-opus-5\",
      \"auth\": {
        \"profiles\": {
          \"anthropic:default\": {\"type\": \"api\", \"key\": \"~a\"},
          \"anthropic:max\": {\"type\": \"oauth\", \"refresh\": \"~a\"}
        }
      }
    }
  },
  \"providers\": {
    \"acmerouter\": {
      \"baseUrl\": \"https://router.example.test/v1\",
      \"apiKey\": \"~a\",
      \"npm\": \"@ai-sdk/openai-compatible\"
    }
  },
  \"channels\": {
    \"telegram\": {\"enabled\": true, \"botToken\": \"~a\", \"dmPolicy\": \"allowlist\", \"allowFrom\": [\"tg:9001\"]}~@[,
    \"discord\": {\"enabled\": true, \"token\": \"~a\", \"dmPolicy\": \"allowlist\", \"allowFrom\": [\"discord:9002\"]}~]
  },
  \"mcpServers\": {
    \"weather\": {\"command\": \"uvx\", \"args\": [\"weather-mcp\"]}
  }
}
" +fixture-key+ +fixture-refresh+ +fixture-key-2+ token discord))
  (import-write root "workspace/AGENTS.md" (format nil "# House rules~%~%Answer in one paragraph.~%"))
  (import-write root "workspace/SOUL.md" (format nil "Dry, exact, no filler.~%"))
  (import-write root "workspace/IDENTITY.md" (format nil "You are the operator's second pair of hands.~%"))
  (import-write root "workspace/memory/2026-09-01.md"
                (format nil "The staging box reboots on Sundays.~%"))
  (import-write root "workspace/skills/weather-brief/SKILL.md"
                (format nil "---~%name: weather-brief~%description: A morning weather line.~%---~%~%# Weather brief~%"))
  root)

;;; --- fixture: Qwen Code --------------------------------------------------------
;;; Its settings file lists every endpoint under `modelProviders.<wire>' --
;;; the wire (`openai') is the protocol, not a vendor -- each entry naming the
;;; environment variable its key is read from, and the keys sit in `env'.

(defun make-qwen-fixture (box &key (key-name "DASHSCOPE_API_KEY")
                              &aux (root (merge-pathnames ".qwen/" (uiop:ensure-directory-pathname box))))
  "A Qwen home under BOX/.qwen/: a hosted endpoint whose key is read from
KEY-NAME and a local one, both under `modelProviders.openai'."
  (import-write root "settings.json"
                (format nil "{
  \"modelProviders\": {\"openai\": [
    {\"id\": \"qwen3-coder-plus\", \"name\": \"Qwen\", \"baseUrl\": \"https://dashscope.aliyuncs.com/compatible-mode/v1\", \"envKey\": \"~a\"},
    {\"id\": \"llama3\", \"name\": \"Local\", \"baseUrl\": \"http://localhost:11434/v1\", \"envKey\": \"OLLAMA_KEY\"}]},
  \"env\": {\"~a\": \"~a\", \"OLLAMA_KEY\": \"ollama-local-1\"},
  \"security\": {\"auth\": {\"selectedType\": \"openai\"}},
  \"model\": {\"name\": \"qwen3-coder-plus\"}
}
" key-name key-name +fixture-key+))
  root)

;;; --- fixture: Command Code, from the pi provider readme ------------------------

(defun make-commandcode-fixture (box &aux (root (merge-pathnames ".commandcode/" box)))
  (import-write root "auth.json"
                (format nil "{\"commandcode\": {\"type\": \"api\", \"key\": \"~a\"}}~%" +fixture-key+))
  root)

;;; --- fixture: Windsurf memories ------------------------------------------------

(defun make-windsurf-fixture (box &aux (root (merge-pathnames ".codeium/windsurf/" box)))
  (import-write root "memories/global_rules.md"
                (format nil "Always run the linter before committing.~%"))
  (import-write root "memories/2026-08-30-deploys.md"
                (format nil "Deploys go out at 16:00 UTC.~%"))
  root)

;;; --- fixture: Codex, which writes TOML -----------------------------------------

(defun make-codex-fixture (box &aux (root (merge-pathnames ".codex/" (uiop:ensure-directory-pathname box))))
  (import-write root "config.toml"
                (format nil "model = \"gpt-5.6\"~%model_provider = \"acmeprivate\"~%
[model_providers.acmeprivate]~%name = \"Acme Private\"~%base_url = \"https://private.example.test/v1\"~%env_key = \"ACMEPRIVATE_API_KEY\"~%wire_api = \"chat\"~%
[mcp_servers.docs]~%command = \"uvx\"~%args = [\"docs-mcp\"]~%"))
  (import-write root "auth.json"
                (format nil "{\"ACMEPRIVATE_API_KEY\": \"~a\", \"auth_mode\": \"apikey\"}~%" +fixture-key+))
  (import-write root "AGENTS.md" (format nil "# Codex rules~%~%Small diffs.~%"))
  root)

;;; --- reading a plan back --------------------------------------------------------

(defun hermes-plan (home &rest keys)
  "The plan over the box's Hermes home, into the scratch organism HOME."
  (apply #'nik:make-plan :source "hermes" :settings (list :home home) keys))

(defun import-items (plan kind)
  "PLAN's items of KIND, oldest first."
  (remove kind (nik:plan-items plan) :key #'nik:item-kind :test-not #'string=))

(defun import-statuses (plan kind)
  (mapcar #'nik:item-status (import-items plan kind)))

(defun import-item (plan kind needle)
  "The item of KIND whose destination or source carries NEEDLE."
  (find-if (lambda (item)
             (or (and (nik:item-destination item) (search needle (nik:item-destination item)))
                 (and (nik:item-source item) (search needle (nik:item-source item)))))
           (import-items plan kind)))

(defun import-config ()
  (nle:read-shared-config))

(defun import-auth (&aux (path nle::*auth-file-path*))
  (and (probe-file path) (shasht:read-json (uiop:read-file-string path))))

(defun fact-ids (facts kind)
  (sort (mapcar #'nik:fact-id (remove kind facts :key #'nik:fact-kind :test-not #'eq))
        #'string<))

(defun home-facts-of (source)
  "Every fact SOURCE holds, the conversations left alone."
  (nik:read-facts (nik::open-home source) :sessions nil))

;;; --- fixture: this organism's own retired carriers ------------------------------

(defun make-nodecode-fixture (box &aux (root (merge-pathnames ".nodecode/" (uiop:ensure-directory-pathname box))))
  "A Nodecode home from before knowledge cells: memory folders with their hot
index and sightings, and a SKILL.md library with its usage ledger, an
archived skill and a proposed one."
  (import-write root "memory/MEMORY.md"
                (format nil "# Memory (user)~%~%## Feedback~%- [quiet-output](quiet-output.md) — the operator wants quiet output~%"))
  (import-write root "memory/quiet-output.md"
                (format nil "---~%name: quiet-output~%description: The operator wants quiet output, said \"twice\".~%type: feedback~%scope: user~%created: 2026-09-01T10:00:00.000Z~%updated: 2026-09-20T10:00:00.000Z~%sources: [s-ONE, s-TWO]~%---~%~%Said on 2026-09-01; a log line counts as noise.~%"))
  (import-write root "memory/cold-fact.md"
                (format nil "---~%name: cold-fact~%description: An old fact nobody lists.~%type: reference~%scope: user~%updated: 2026-08-01T10:00:00.000Z~%---~%~%Kept for search.~%"))
  (import-write root "memory/projects/-tmp-proj/repo-builds.md"
                (format nil "---~%name: repo-builds~%description: The repository builds with just.~%type: project~%scope: project /tmp/proj~%updated: 2026-09-21T10:00:00.000Z~%---~%"))
  (import-write root "memory/projects/-tmp-proj/quiet-output.md"
                (format nil "---~%name: quiet-output~%description: This project's own quiet rule.~%type: feedback~%scope: project /tmp/proj~%---~%"))
  (import-write root "memory/.experience/sightings.jsonl"
                (format nil "{\"kind\":\"helped\",\"line\":\"quiet-output\",\"session\":\"s-ONE\",\"at\":\"2026-09-22T10:00:00.000Z\",\"quote\":\"kept it quiet\"}~%{\"kind\":\"gap\",\"gap\":\"x\",\"session\":\"s-ONE\",\"at\":\"2026-09-22T10:00:00.000Z\",\"quote\":\"a gap\"}~%"))
  (import-write root "memory/.experience/proposals/p-1.json" "{}")
  (import-write root "skills/red-baseline/SKILL.md"
                (format nil "---~%name: red-baseline~%description: Split red tests from the baseline.~%origin: reflection~%---~%# Red baseline~%~%1. Run the suite.~%"))
  (import-write root "skills/red-baseline/references/notes.md" (format nil "Read the FAIL lines.~%"))
  (import-write root "skills/red-baseline/scripts/run.sh" "echo hi")
  (import-write root "skills/.archive/old/SKILL.md" (format nil "---~%name: old~%description: Gone.~%---~%"))
  (import-write root "skills/.skills/usage.jsonl"
                (format nil "{\"at\":\"2026-09-23T10:00:00.000Z\",\"name\":\"red-baseline\",\"kind\":\"view\",\"session\":\"s-TWO\"}~%"))
  (import-write root "skills/.skills/proposed/maybe/SKILL.md" (format nil "---~%name: maybe~%description: Not yet.~%---~%"))
  root)
