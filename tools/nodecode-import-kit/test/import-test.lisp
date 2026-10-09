;;;; import-test.lisp --- the plan over a box, dry and applied.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(nlk:access (item nik::item) (stop nik::item))

(deftest import-clock-reads-seconds-as-utc ()
  (is (equal "1970-01-01T00:00:00.000Z" (nlk:iso-time 0)))
  (is (equal "2026-06-23T22:05:07.000Z" (nlk:iso-time 1782252307)))
  (is (equal "2026-06-23T22:05:07.250Z" (nlk:iso-time 1782252307.25d0)))
  (is (equal "2026-06-23T22:05:07.000Z" (nik::iso-from-any "2026-06-23T22:05:07")))
  (is (equal "2026-06-23T22:05:07.000Z" (nik::iso-from-any 1782252307000)))
  (is (null (nik::iso-from-any "soon"))))

(deftest import-plan-reads-a-home-dry (with-import-runtime (home box))
  ;; A dry run names every piece and writes nothing: the config stays as it
  ;; was, no auth.json, no SOUL.md, no skills folder — and the keys and
  ;; tokens the home carries are in the plan like everything else, never
  ;; shown.
  (make-hermes-fixture box)
  (let ((plan (hermes-plan home)))
    (is (equal '("providers.subrouter-1" "auth.json api_keys.subrouter-1"
                 "auth.json api_keys.openrouter" "general.default_model")
               (sort (mapcar #'nik:item-destination (import-items plan "providers"))
                     #'< :key (lambda (text)
                                (position-if (lambda (needle) (search needle text))
                                             '("providers." "subrouter" "openrouter" ""))))))
    (is (every (lambda (status) (equal status "imported")) (import-statuses plan "providers")))
    (is (null (import-item plan "providers" "providers.openrouter")))
    (is (search "gpt-5.5 on subrouter-1"
                (nik:item-reason (import-item plan "providers" "general.default_model"))))
    (is (equal '("imported") (import-statuses plan "instructions")) "one persona")
    (is (equal '("imported" "imported" "imported") (import-statuses plan "memory")))
    (is (equal '("imported") (import-statuses plan "skills")) "one skill")
    (is (null (import-item plan "skills" ".archive")) "a hidden folder is not walked")
    (is (equal '("imported") (import-statuses plan "profiles")))
    (is (search "nodecode -p coder import hermes --source"
                (nik:item-reason (import-item plan "profiles" "profiles/coder"))))
    (is (equal '("imported" "imported" "skipped" "skipped")
               (sort (copy-list (import-statuses plan "mcp")) #'string<)) "two servers and the secrets left out of them")
    (is (search "cwd dropped" (nik:item-reason (import-item plan "mcp" "github"))))
    (is (equal '("imported" "imported") (import-statuses plan "channels")) "telegram and discord")
    (is (search "token in a 0600 file"
                (nik:item-reason (import-item plan "channels" "channels.telegram"))))
    (is (equal '("imported" "imported" "skipped") (import-statuses plan "cron")))
    (is (= 2 (count "imported" (import-statuses plan "sessions") :test #'string=)))
    (let ((report (nik:report-text plan)))
      (is (search "dry run" report))
      (is (not (search +fixture-key+ report)) "no key is ever printed")
      (is (not (search +fixture-token+ report)) "and no bot token")
      (is (not (search "FAKE-not-a-token-github" report)) "nor an MCP server's env"))
    (is (null (probe-file (merge-pathnames "auth.json" home))) "nothing written")
    (is (null (probe-file (merge-pathnames "SOUL.md" home))))
    (is (null (uiop:directory-exists-p (nlk:knowledge-directory))) "no knowledge cell either")))

(deftest import-apply-lands-each-piece-through-its-own-seam (with-import-runtime (home box))
  (make-hermes-fixture box)
  (with-cron ()
   (with-temp-store ()
    (let ((plan (nik:apply-plan (hermes-plan home))))
      (is (zerop (cdr (assoc "error" (nik::summary-counts plan) :test #'string=))))
      (is-shape (import-config)
        ((:text "providers" "subrouter-1" "base_url") "https://ai.example.test/v1")
        ((:text "providers" "subrouter-1" "sdk") "openai-completions")
        ((:text "general" "default_model" "provider") "subrouter-1")
        ((:text "mcp" "servers" "github" "command") "npx")
        ((:text "mcp" "servers" "github" "env" "LOG_LEVEL") "info")
        ((:any "channels" "telegram" "allowed_users") equalp #("8071918233" "someone"))
        ((:any "channels" "telegram" "reactions") eq t))
      (let ((auth (import-auth)))
        (is (equal +fixture-key+ (nlk:json-value auth :text "api_keys" "subrouter-1" "key")))
        (is (equal +fixture-key-2+ (nlk:json-value auth :text "api_keys" "openrouter" "key"))))
      ;; Under the heading that names the home it came from, as each home's rules land.
      (is (equal (format nil "## hermes — SOUL.md~%~%Speak plainly. Never flatter.~%")
                 (uiop:read-file-string (merge-pathnames "SOUL.md" home))))
      (is-present (skill (kept-text "notes")) "the skill is a definition in the knowledge cell"
        (is (search "(define-skill notes" skill))
        (is (search ";; origin: import" skill))
        (is (search ";; support files not carried: scripts/run.py (11 bytes)" skill) "a script is named, not carried"))
      (let ((secret (merge-pathnames "secrets/channels.telegram.token_file" home)))
        (is (probe-file secret) "the bot token is a file of its own")
        (is (equal +fixture-token+ (string-trim '(#\Newline) (uiop:read-file-string secret)))))
      (is (member "nodecode-channel-telegram" *import-refreshed* :test #'equal))
      (is (= 3 (length (directory (merge-pathnames "tester-memory/hermes-*.lisp" nlk::*cells-directory*)))))
      (is (search "— session hermes import"
                  (nlk::git-in-layer (nlk:knowledge-directory) "log" "--format=%s" "-1")) "kept under the import's name")
      (is (= 2 (length (symbol-value (find-symbol "*JOBS*" "NODECODE-CRON")))))
      (is (nlk:session-exists-p "hermes-20260623_150503_55c2a9"))
      (is (probe-file (nik::plan-report-path plan)) "the report is on disk")))))

(deftest import-leaves-an-mcp-servers-secrets-where-they-are (with-import-runtime (home box))
  ;; The mcp folder reads a server's env and headers from config.jsonc as
  ;; written, a file anyone on the box can read, and takes no reference to a
  ;; 0600 file: a secret in either has no safe place to go. It stays in its
  ;; home, the report names it, and what is not a secret comes over.
  (make-hermes-fixture box)
  (let* ((plan (hermes-plan home :only "mcp"))
         (left (remove "skipped" (import-items plan "mcp") :key #'nik:item-status :test-not #'string=))
         (github (find "mcp.servers.github" left :key #'nik:item-destination :test #'equal))
         (docs (find "mcp.servers.docs" left :key #'nik:item-destination :test #'equal)))
    (is (search "env GITHUB_PERSONAL_ACCESS_TOKEN" (nik:item-reason github)) "the variable, by name")
    (is (search "headers Authorization" (nik:item-reason docs)) "the header, by name")
    (is (not (search "LOG_LEVEL" (nik:item-reason github))) "and only the secrets")
    (is (search "config.jsonc" (nik:item-reason github)) "and where to add it by hand")
    ;; That path written as the operator writes it, from ~: the report said
    ;; /home/<them>/.nodecode/config.jsonc (2026-09-30). A path under the home
    ;; that names nothing, read by this plan alone.
    (let* ((nle::*shared-config-path* (merge-pathnames ".nl-import-absent/config.jsonc" (user-homedir-pathname)))
           (nle::*shared-config-memo* nil)
           (reason (nik:item-reason (find "mcp.servers.github"
                                          (remove "skipped" (import-items (hermes-plan home :only "mcp") "mcp")
                                                  :key #'nik:item-status :test-not #'string=)
                                          :key #'nik:item-destination :test #'equal))))
      (is-carrying (text reason)
        "in ~/.nl-import-absent/config.jsonc yourself"
        (:absent (namestring (user-homedir-pathname)))))
    (nik:apply-plan plan)
    (is-shape (import-config)
      ((:text "mcp" "servers" "github" "command") "npx")
      ((:text "mcp" "servers" "github" "env" "LOG_LEVEL") "info")
      ((:text "mcp" "servers" "docs" "url") "https://docs.example.test/mcp")
      ((:text "mcp" "servers" "docs" "headers" "X-Client") "hermes"))
    (let ((config (uiop:read-file-string nle::*shared-config-path*))
          (report (uiop:read-file-string (nik::plan-report-path plan))))
      (dolist (secret '("FAKE-not-a-token-github" "FAKE-not-a-token-docs"))
        (is (not (search secret config)) "no secret in the shared config")
        (is (not (search secret report)) "or in the report")))))

(deftest import-lands-an-mcp-server-off-and-says-what-it-would-run (with-import-runtime (home box))
  ;; vr-116: every imported server started at the next launch, before any
  ;; task, and npx and uvx fetched and ran their packages unasked. A server
  ;; lands off; the report says what it would run, where that comes from, and
  ;; how the operator starts it.
  (make-hermes-fixture box)
  (let* ((plan (hermes-plan home :only "mcp"))
         (github (import-item plan "mcp" "mcp.servers.github"))
         (docs (import-item plan "mcp" "mcp.servers.docs")))
    (is (search "npx -y @modelcontextprotocol/server-github (fetched from npm); lands off: /mcp on github starts it"
                (nik:item-reason github)))
    (is (search "https://docs.example.test/mcp; lands off: /mcp on docs starts it" (nik:item-reason docs)))
    (nik:apply-plan plan)
    (dolist (name '("github" "docs"))
      (let ((entry (nlk:json-value (import-config) :object "mcp" "servers" name)))
        (is (and entry (not (nlk:config-boolean entry "enabled" t))) name))))
  ;; A secret rides a server's arguments as often as its env: the report says
  ;; the command up to the package a runner fetches, masked, and counts the
  ;; rest. Windows spells a runner `npx.cmd' or `cmd /c npx'.
  (dolist (case '(("npx" #("-y" "mcp-remote" "https://mcp.example/sse" "--header" "Authorization: Bearer sk-live-SECRET123")
                   "npx -y mcp-remote and 3 more arguments (fetched from npm)")
                  ("docker" #("run" "-i" "--rm" "-e" "GITHUB_PERSONAL_ACCESS_TOKEN=ghp_SECRET0123456789" "ghcr.io/github/github-mcp-server")
                   "docker run and 5 more arguments (fetched from a container registry)")
                  ("mcp-server-postgres" #("postgresql://admin:SECRETPW@db.internal/prod")
                   "mcp-server-postgres with 1 argument")
                  ("uvx" #("--from" "git+https://bot:SECRETPW@git.example/mcp.git" "mcp-git")
                   "uvx --from git+https://[set]@git.example/mcp.git and 1 more argument (fetched from PyPI)")
                  ("uvx" #("--api-key" "SECRET-sk-proj-0123456789" "--token=SECRET" "pkg")
                   "uvx --api-key [set] and 2 more arguments (fetched from PyPI)")
                  ("uvx" #("--token=SECRET" "-y" "pkg" "sk-proj-SECRET0123456789")
                   "uvx --token=[set] -y pkg and 1 more argument (fetched from PyPI)")
                  ("cmd" #("/c" "npx" "-y" "@upstash/context7-mcp") "cmd /c npx -y @upstash/context7-mcp (fetched from npm)")
                  ("C:\\nodejs\\npx.cmd" #("-y" "@upstash/context7-mcp")
                   "C:\\nodejs\\npx.cmd -y @upstash/context7-mcp (fetched from npm)")))
    (destructuring-bind (command args line) case
      (is (equal line (nik::server-line (nlk:json-object "command" command "args" args))) line)))
  ;; a url's password and a secret in its query
  (is (equal "https://[set]@mcp.example/sse?token=[set]"
             (nik::server-line (nlk:json-object "url" "https://bot:SECRETPW@mcp.example/sse?token=SECRET")))))

(deftest import-plan-leaves-a-kind-out-and-says-it-is-waiting (with-import-runtime (home box))
  ;; The first frame brings everything but the conversations. The plan says
  ;; so in its answer, so the closing line can tell the operator where they
  ;; went instead of leaving them to wonder.
  (make-hermes-fixture box)
  (let* ((plan (hermes-plan home :without "sessions"))
         (value (nik:plan-json plan)))
    (is (null (import-items plan "sessions")) "no conversation is read")
    (is (import-items plan "providers") "and everything else still is")
    (is (eq t (gethash "sessions_left" value)) "the answer says they are waiting"))
  (let ((value (nik:plan-json (hermes-plan home :only "providers,sessions" :without "sessions"))))
    (is (eq t (gethash "sessions_left" value)) "WITHOUT outranks ONLY"))
  (let ((value (nik:plan-json (hermes-plan home))))
    (is (eq :false (gethash "sessions_left" value)))))

(deftest import-reads-a-world-nobody-here-has (with-import-runtime (home box))
  ;; Openclaw, written from its own docs: an auth profile keyed
  ;; `<provider>:<label>', an OAuth profile beside it, a custom provider, a
  ;; bot token, a model pick, a workspace of instructions and a memory day
  ;; file, and a skill. Not one line of this folder knows the word
  ;; `openclaw' — it is a row in a table, and everything else is a shape.
  (make-openclaw-fixture box)
  (let ((facts (home-facts-of "openclaw")))
    (is (equal '("acmerouter" "anthropic") (fact-ids facts :provider)))
    (is (equal '("anthropic") (fact-ids facts :oauth)) "the grant is named and left")
    (is (equal '("weather") (fact-ids facts :mcp)))
    (is (equal '("weather-brief") (fact-ids facts :skill)))
    (is (equal '("AGENTS" "IDENTITY" "SOUL") (fact-ids facts :instructions)))
    (is (= 1 (length (fact-ids facts :memory))) "and its memory day file")
    (is (equal '("telegram") (fact-ids facts :channel)))
    (let ((model (first (remove :model facts :key #'nik:fact-kind :test-not #'eq))))
      (is (equal "anthropic" (getf (nik:fact-value model) :provider)))
      (is (equal "claude-opus-5" (getf (nik:fact-value model) :model))))))

(deftest import-reads-command-code-and-windsurf (with-import-runtime (home box))
  ;; Two more worlds nobody here has: one that keeps a single key in
  ;; auth.json, and one whose whole contribution is a folder of remembered
  ;; entries -- and its global_rules.md among them, which is its rules for
  ;; every session, not one more thing remembered (an-116).
  (make-commandcode-fixture box)
  (make-windsurf-fixture box)
  (is (equal '("commandcode") (fact-ids (home-facts-of "commandcode") :provider)))
  (let ((facts (home-facts-of "windsurf")))
    (is (= 1 (length (fact-ids facts :memory))) "the dated memory file")
    (is (equal '("GLOBAL_RULES") (fact-ids facts :instructions)) "the rules are instructions")))

(deftest import-lands-rules-in-agents-md-and-a-persona-in-soul-md (with-import-runtime (home box))
  ;; am-101: a harness's rules for every session -- Claude Code's CLAUDE.md,
  ;; Codex's AGENTS.md, Windsurf's global_rules.md -- land in this home's
  ;; AGENTS.md, which every session reads; a persona (SOUL.md, IDENTITY.md)
  ;; in its SOUL.md, the channels' voice. Both went to SOUL.md, which no
  ;; session in a terminal reads, while the report said imported.
  (import-write box ".claude/settings.json" "{}")
  (import-write box ".claude/CLAUDE.md" (format nil "# House rules~%~%Use just.~%"))
  (make-codex-fixture box)
  (make-windsurf-fixture box)
  (make-openclaw-fixture box)
  (let* ((rules (namestring (merge-pathnames "AGENTS.md" home)))
         (soul (namestring (merge-pathnames "SOUL.md" home)))
         (plan (nik:make-plan :settings (list :home home) :only "instructions"))
         (items (import-items plan "instructions")))
    ;; One item a home's file, the rules ahead of the persona.
    (is (equal (list rules soul)
               (remove-duplicates (mapcar #'nik:item-destination items) :test #'equal :from-end t)))
    (is (every (lambda (item) (equal "imported" (nik:item-status item))) items))
    ;; the report names AGENTS.md, written from the home
    (is (search (format nil "→ ~a —" (nlk:home-abbreviated rules)) (nik:report-text plan)))
    ;; xa-201: the persona was reported as imported instructions, and no
    ;; terminal session reads SOUL.md; its line says who does.
    (is-carrying (text (nik:item-reason (find soul items :key #'nik:item-destination :test #'equal)))
      "the voice your chat bots speak in" "a terminal session does not read SOUL.md")
    (nik:apply-plan plan)
    (is-carrying (text (uiop:read-file-string rules))
      "Use just." "Small diffs." "Always run the linter" "Answer in one paragraph."
      "## claude — CLAUDE.md" (:absent "Dry, exact" "no persona among the rules"))
    (is-carrying (text (uiop:read-file-string soul))
      "Dry, exact" "second pair of hands" (:absent "Use just." "no rules in the voice")))
  ;; An AGENTS.md already here is the operator's: it stays, first, and the
  ;; rules land after it, each under its own heading.
  (with-temp-directory (fresh "import-home-2")
    (import-write fresh "AGENTS.md" "mine")
    (let ((plan (nik:make-plan :settings (list :home fresh) :only "instructions")))
      (is (every (lambda (status) (equal status "imported")) (import-statuses plan "instructions")))
      (nik:apply-plan plan))
    (is-carrying (text (uiop:read-file-string (merge-pathnames "AGENTS.md" fresh)))
      (is (uiop:string-prefix-p "mine" text) "what was here stays, first")
      "## codex — AGENTS.md" "Small diffs.")))

(deftest import-keeps-the-rules-within-what-a-session-reads (with-import-runtime (home box))
  ;; xh-201, xn-202: a session refuses the home's AGENTS.md whole past the
  ;; core's limit (context.lisp OPERATOR-RULES-SECTION), and the import joined
  ;; every home's rules into it unsized and said imported, so no session read
  ;; any of them. What fits lands; a file that would take AGENTS.md past the
  ;; limit is left out, named, with its size and what to do.
  (let ((limit nle::+instructions-byte-limit+))
    (import-write box ".claude/settings.json" "{}")
    (import-write box ".claude/CLAUDE.md"
                  (format nil "# Claude rules~%~%~a~%" (make-string (floor limit 2) :initial-element #\c)))
    (make-codex-fixture box)
    (import-write box ".codex/AGENTS.md"
                  (format nil "# Codex rules~%~%~a~%" (make-string (floor limit 2) :initial-element #\x)))
    (let* ((plan (nik:make-plan :source '("claude" "codex") :settings (list :home (nlk:home))
                                :only "instructions"))
           (items (import-items plan "instructions"))
           (kept (find "imported" items :key #'nik:item-status :test #'equal))
           (left (find "skipped" items :key #'nik:item-status :test #'equal)))
      (is (= 2 (length items)) "each fits alone, the two together do not")
      (is-present kept "one lands"
        (is-carrying (text (nik:item-reason kept))
          "with it AGENTS.md is"
          (is (search (format nil "of the ~a a session reads" (nlk:size-text limit)) text))))
      (is-present left "the other is left out, and said"
        (is-carrying (text (nik:item-reason left))
          "left out: with it" "AGENTS.md would be"
          (is (search (format nil "past the ~a a session reads" (nlk:size-text limit)) text))
          ;; The file to trim, by its path, and the command that tries again.
          (is (search (format nil "/~a or what is already there, and run `nodecode import ~a'"
                              (nik:item-source left) (nik::item-world left))
                      text))))
      (nik:apply-plan plan)
      (is (<= (length (uiop:read-file-string (nlk:home "AGENTS.md"))) limit))
      (is-present (text (cdr (nle::operator-rules-section nil))) "and a session reads what landed"
        (is (search (format nil "## ~a — ~a" (nik::item-world kept) (nik:item-source kept)) text)))))
  ;; A file past the limit on its own says that.
  (import-write box ".claude/CLAUDE.md" (make-string (* 2 nle::+instructions-byte-limit+) :initial-element #\c))
  (let ((item (first (import-items (nik:make-plan :source "claude" :settings (list :home home)
                                                  :only "instructions")
                                   "instructions"))))
    (is (equal "skipped" (nik:item-status item)))
    (is-carrying (text (nik:item-reason item))
      (is (search (format nil ".claude/CLAUDE.md is ~a" (nlk:size-text (* 2 nle::+instructions-byte-limit+))) text))
      "trim it and run `nodecode import claude'")
    (is (null (probe-file (merge-pathnames "AGENTS.md" home))) "nothing written for it"))
  ;; A persona is held to what a chat bot reads (channels/kit/soul.lisp):
  ;; past it, no bot speaks in it, so it is not reported as their voice.
  (import-write box ".hermes/SOUL.md" (make-string (* 2 nlk::+max-harness-bytes+) :initial-element #\s))
  (let ((item (first (import-items (nik:make-plan :source "hermes" :settings (list :home home)
                                                  :only "instructions")
                                   "instructions"))))
    (is (equal "skipped" (nik:item-status item)))
    (is-carrying (text (nik:item-reason item))
      ".hermes/SOUL.md is" (:absent "the voice your chat bots speak in" "not called their voice")
      (is (search (format nil "past the ~a a chat bot reads" (nlk:size-text nlk::+max-harness-bytes+)) text)))
    (is (null (probe-file (merge-pathnames "SOUL.md" home))) "nothing written for it"))
  ;; A home read at a path is no world the import verb takes by name: it is
  ;; named again by its path.
  (import-write box "backup/.claude/CLAUDE.md"
                (make-string (* 2 nle::+instructions-byte-limit+) :initial-element #\c))
  (is (search "run `nodecode import --source "
              (nik:item-reason
               (first (import-items (nik:make-plan :source (namestring (merge-pathnames "backup/.claude/" box))
                                                   :settings (list :home home) :only "instructions")
                                    "instructions"))))))

(deftest import-adds-a-later-homes-rules-under-their-own-heading (with-import-runtime (home box))
  ;; xa-205: a later import found AGENTS.md here and could only leave its
  ;; rules out or, under --overwrite, replace the file whole -- the first
  ;; home's rules gone without a word. Each home's rules are a section under
  ;; its own heading: a later import adds its section after what is here, a
  ;; re-run finds them already there, and --overwrite replaces that one
  ;; section, and says so.
  (make-codex-fixture box)
  (flet ((run (source &rest keys)
           (let ((plan (apply #'nik:make-plan :source source :settings (list :home home)
                              :only "instructions" keys)))
             (nik:apply-plan plan)
             (first (import-items plan "instructions"))))
         (rules () (uiop:read-file-string (merge-pathnames "AGENTS.md" home))))
    (is (search "added under `## codex — AGENTS.md'" (nik:item-reason (run "codex"))))
    ;; A home read at a path lands under its path, as the operator writes
    ;; it, a section of its own too.
    (let ((backup (merge-pathnames "backup/.claude/" box)))
      (import-write backup "CLAUDE.md" (format nil "Path rules.~%"))
      (is (search (format nil "added under `## ~a — CLAUDE.md'"
                          (nlk:home-abbreviated (uiop:native-namestring backup)))
                  (nik:item-reason (run (namestring backup)))))
      ;; Under the operator's home that path reads `~/...', a heading still.
      (is (nik::rules-heading-p "## ~/backup/.claude — CLAUDE.md")))
    (import-write box ".claude/settings.json" "{}")
    ;; A heading of the rules' own that reads like a world's is no section.
    (import-write box ".claude/CLAUDE.md" (format nil "# House rules~%~%Use just.~%~%## Claude — style~%~%Short lines.~%"))
    (let ((item (run "claude")))
      (is (equal "imported" (nik:item-status item)) "a later home's rules are added, not a conflict")
      (is (search "added under `## claude — CLAUDE.md'" (nik:item-reason item))))
    (is-carrying (text (rules)) "Small diffs." "Use just." "Path rules.")
    (is (equal "skipped" (nik:item-status (run "claude"))) "a re-run finds its rules already here")
    ;; The section after the one --overwrite replaces is another home's, and stays.
    (import-write box ".codex/AGENTS.md" (format nil "# Codex rules~%~%Big diffs.~%"))
    (is (search "replaced its own section `## codex — AGENTS.md'"
                (nik:item-reason (run "codex" :overwrite t))))
    (is-carrying (text (rules))
      "Big diffs." ("Path rules." "the home read at a path keeps its section") "Use just."
      (:absent "Small diffs."))
    (import-write home "AGENTS.md" (ppcre:regex-replace-all "\\n" (rules) (format nil "~c~%" #\Return)))
    (is (equal "skipped" (nik:item-status (run "claude"))) "and in a file saved with CRLF endings")
    (import-write box ".claude/CLAUDE.md" (format nil "# House rules~%~%Use make.~%"))
    (let ((item (run "claude")))
      (is (equal "conflict" (nik:item-status item)) "changed rules wait for --overwrite")
      (is (search "--overwrite replaces that section alone" (nik:item-reason item))))
    (is (search "replaced its own section `## claude — CLAUDE.md'"
                (nik:item-reason (run "claude" :overwrite t))))
    (is-carrying (text (rules))
      "Big diffs." "Path rules." "Use make."
      (:absent "Use just." "the old section is gone") (:absent "Short lines." "all of it")
      ;; In the order they came.
      (is (< (search "## codex" text) (search "Path rules." text) (search "## claude" text))))))

(deftest import-reads-a-home-written-in-toml (with-import-runtime (home box))
  (make-codex-fixture box)
  (let* ((facts (home-facts-of "codex"))
         (plan (nik:make-plan :source "codex" :settings (list :home home))))
    (is (equal '("acmeprivate")
               (remove-duplicates (fact-ids facts :provider) :test #'equal)))
    (is (equal '("docs") (fact-ids facts :mcp)))
    (is (equal '("AGENTS") (fact-ids facts :instructions)))
    (nik:apply-plan plan)
    (is-shape (import-config)
      ((:text "providers" "acmeprivate" "base_url") "https://private.example.test/v1")
      ((:text "general" "default_model" "provider") "acmeprivate"))
    (is (equal +fixture-key+
               (nlk:json-value (import-auth) :text "api_keys" "acmeprivate" "key")))))

(deftest import-merges-every-home-on-the-box-newest-first (with-import-runtime (home box))
  ;; A real box carries several homes and the useful facts are spread over
  ;; them. Naming no world reads all of them; a provider the newest world
  ;; names wins, and everything else is a union.
  (make-commandcode-fixture box)
  (make-openclaw-fixture box)
  (let ((plan (nik:make-plan :settings (list :home home))))
    (is (< 1 (length (nik::plan-homes plan))) "both homes are read")
    (let ((keys (remove-if-not (lambda (item)
                                 (search "auth.json api_keys." (or item.destination "")))
                               (import-items plan "providers"))))
      (is (= 3 (length keys)))
      (dolist (id '("acmerouter" "anthropic" "commandcode"))
        (is (find-if (lambda (item) (search id item.destination)) keys)
            (format nil "~a's key lands" id))))
    (is (search "Openclaw" (nik::plan-source-line plan)))))

(deftest import-never-clobbers-a-fact-already-here (with-import-runtime (home box))
  (make-hermes-fixture box)
  (write-temp-file nle::*shared-config-path*
                   "{\"providers\": {\"subrouter-1\": {\"sdk\": \"anthropic\"}},
                     \"general\": {\"default_model\": {\"provider\": \"x\", \"model\": \"y\"}},
                     \"mcp\": {\"servers\": {\"github\": {\"command\": \"mine\"}}},
                     \"channels\": {\"telegram\": {\"enabled\": true}}}")
  (import-write home "SOUL.md" "mine")
  (let ((plan (hermes-plan home)))
    (is (equal "conflict" (nik:item-status (import-item plan "providers" "providers.subrouter-1"))))
    (is (equal "conflict" (nik:item-status (import-item plan "providers" "general.default_model"))))
    (is (equal "conflict" (nik:item-status (import-item plan "mcp" "github"))))
    (is (equal "conflict" (nik:item-status (import-item plan "channels" "channels.telegram"))))
    ;; A persona here is the operator's voice, and a bot speaks in one.
    (is (equal "conflict" (nik:item-status (first (import-items plan "instructions")))))
    (is (search "--overwrite puts this one in its place"
                (nik:item-reason (first (import-items plan "instructions")))))
    (is (search "--overwrite" (nik:item-reason (import-item plan "mcp" "github")))))
  (let ((plan (hermes-plan home :overwrite t)))
    (is (equal "imported" (nik:item-status (import-item plan "mcp" "github"))))
    (is (search "in place of your own" (nik:item-reason (first (import-items plan "instructions")))))))

(deftest import-takes-a-running-gateway-over-by-default (with-import-runtime (home box))
  (make-hermes-fixture box)
  (setf *import-running* (list "systemd user unit hermes-gateway.service is active"))
  (let* ((plan (hermes-plan home))
         (stop (find "hermes-gateway.service" (import-items plan "channels")
                     :key #'nik:item-source :test #'equal)))
    (is stop "the takeover is an item of its own")
    (is (search "systemctl --user enable --now hermes-gateway.service"
                stop.reason))
    (is (eq stop (find-if (lambda (item) (equal "channels" item.kind)) (nik:plan-items plan))))
    (nik:apply-plan plan)
    (is (= 1 *import-stopped*) "the unit is stopped once")
    (is (eq t (nlk:json-value (import-config) :any "channels" "telegram" "enabled"))))
  (setf *import-running* (list "systemd user unit hermes-gateway.service is active")
        *import-stopped* 0)
  (nik:apply-plan (hermes-plan home :overwrite t :takeover nil))
  (is (zerop *import-stopped*) "--keep-running stops nothing")
  (is (not (nlk:config-boolean (nlk:json-value (import-config) :any "channels" "telegram")
                               "enabled" t)))
  (is (member "telegram" (nik::held-platforms (list :home home))
              :test #'equal)))

(deftest import-apply-over-the-wire-with-keep-running-stops-nothing (with-import-runtime (home box))
  ;; The first frame applies with keep_running set: a live foreign gateway is
  ;; never stopped or disabled, its bot lands off and held for the watch to hand
  ;; over, so one token is never polled by two gateways.
  (make-hermes-fixture box)
  (setf *import-running* (list "systemd user unit hermes-gateway.service is active"))
  (let ((value (nik::run-method "apply" (nlk:json-object "source" "hermes" "keep_running" t))))
    (is (zerop *import-stopped*) "no stop, so no disable after it")
    (is (equal "hermes-gateway.service" (gethash "takeover" value)) "it still names the unit that keeps running")
    (is (find "channels.telegram" (gethash "bots" value) :test #'equal) "and the bots it polls")
    (is (not (nlk:config-boolean (nlk:json-value (import-config) :any "channels" "telegram") "enabled" t)) "the bot is off while the other gateway polls its token")
    (is (member "telegram" (nik::held-platforms (list :home home)) :test #'equal))
    ;; The answer carries the report and nothing posts it: the wizard writes
    ;; it into its own transcript. Posted, it was a toast cut at four rows
    ;; (ip-01).
    (is (search "[providers]" (gethash "report" value)))
    (is (notany (lambda (entry) (search "[providers]" (first entry))) (nlk:notice-log)))))

(deftest import-holds-an-openclaw-bot-while-its-own-gateway-runs (with-import-runtime (home box))
  ;; Only the Hermes row named a gateway, so an Openclaw home was never asked
  ;; about: its Telegram bot landed on and polled beside `openclaw gateway', and
  ;; Telegram answers two pollers on one token with 409 (an-102, 2026-09-30).
  ;; The probe answers only for the unit it is asked about, as the box does.
  (make-openclaw-fixture box)
  (let ((nik::*running-probe*
          (lambda (units)
            (and (member "openclaw-gateway.service" units :test #'equal) *import-running*))))
    (setf *import-running* (list "pid 4242: openclaw gateway --port 18789"))
    (let ((plan (nik:make-plan :source "openclaw" :settings (list :home home) :takeover nil)))
      (nik:apply-plan plan)
      (is (zerop *import-stopped*))
      ;; Off while the other gateway polls its token, held so it comes on once
      ;; that gateway stops, and the report names the process that holds it.
      (is (not (nlk:config-boolean (nlk:json-value (import-config) :any "channels" "telegram")
                                   "enabled" t)))
      (is (member "telegram" (nik::held-platforms (list :home home)) :test #'equal))
      (is (search "another gateway: running — pid 4242: openclaw gateway --port 18789"
                  (nik::report-text plan))))
    ;; `nodecode import openclaw' with its unit up takes the bot over, as it does Hermes's.
    (setf *import-running* (list "systemd user unit openclaw-gateway.service is active"))
    (let ((plan (nik:make-plan :source "openclaw" :settings (list :home home) :overwrite t)))
      (is (equal "openclaw-gateway.service" (gethash "takeover" (nik::plan-json plan))))
      (nik:apply-plan plan)
      (is (= 1 *import-stopped*) "the unit is stopped once")
      (is (eq t (nlk:json-value (import-config) :any "channels" "telegram" "enabled")))
      (is (null (nik::held-platforms (list :home home)))))))

(deftest import-hands-a-held-bot-over-once-the-other-gateway-stops (with-import-runtime (home box))
  (let ((settings (list :home home)))
    (write-temp-file nle::*shared-config-path*
                     "{\"channels\": {\"telegram\": {\"enabled\": false, \"allowed_users\": [\"1\"]}}}")
    (nik::write-held settings '("telegram"))
    (setf *import-running* (list "systemd user unit hermes-gateway.service is active"))
    (is (eq :waiting (nik::watch-step settings)) "while it runs, nothing moves")
    (setf *import-running* '())
    (is (eq :gone (nik::watch-step settings)) "one look with it gone is not a stop")
    (is (eq :handed (nik::watch-step settings)) "two in a row is")
    (is (eq t (nlk:json-value (import-config) :any "channels" "telegram" "enabled")))
    (is (null (nik::held-platforms settings)) "and the held record is spent")
    (is (eq :idle (nik::watch-step settings)) "nothing held, nothing to do")))

(deftest import-knows-a-gateway-by-its-command-not-a-word-in-it ()
  ;; xh-202: any command line carrying the program's name and `gateway' held a
  ;; bot off, so a terminal tailing the unit's log was the gateway. The command
  ;; decides: the title the gateway gives itself, or the program -- itself, the
  ;; script, module or code an interpreter runs, or the name the kernel keeps
  ;; -- whose first word is its verb. A string is a line as `pgrep' joins it,
  ;; a list the words /proc holds.
  (let* ((code (concatenate 'string "import os, sys, runpy; os.environ.pop('PYTHONHOME', None); "
                            "sys.path.insert(0, '/home/op/.hermes/hermes-agent'); import hermes_bootstrap; "
                            "runpy.run_module('hermes_cli.main', run_name='__main__', alter_sys=True)"))
         ;; Hermes' store launcher (hermes_cli/_launchers.py runtime_command).
         (store (list "/home/op/.hermes/store/bin/python3" "-I" "-c" code "gateway" "run" "--replace"))
         (chat (list "/home/op/.hermes/store/bin/python3" "-I" "-c" code "chat" "-q" "is the gateway up")))
    (flet ((gateway-p (argv unit &optional comm)
             (nik::gateway-command-p argv (format nil "~a-gateway.service" unit) comm)))
      (dolist (argv '("openclaw gateway --port 18789"
                      "openclaw --profile work gateway run"
                      "node /usr/local/bin/openclaw gateway --port 18789"
                      "/usr/bin/node --max-old-space-size=4096 /usr/lib/node_modules/openclaw/dist/index.js gateway"
                      "openclaw-gateway"))
        (is (gateway-p argv "openclaw") argv))
      ;; The title, as the 15 characters of it the kernel keeps.
      (is (gateway-p '("openclaw") "openclaw" "openclaw-gatewa"))
      (dolist (argv (list "/home/op/.hermes/hermes-agent/venv/bin/python -m hermes_cli.main gateway run"
                          "python -X dev -m hermes_cli.main gateway run"
                          "python3 /home/op/.local/bin/hermes gateway run --replace"
                          "hermes -p work gateway"
                          "python3 /opt/hermes-agent/gateway/run.py"
                          "hermes-gateway"
                          store
                          (format nil "~{~a~^ ~}" store)))
        (is (gateway-p argv "hermes") argv))
      ;; The program by the name it gives itself, whatever code launched it.
      (is (gateway-p (list "/usr/bin/python3" "-I" "-c" "exec(a_launcher_this_reader_does_not_know)" "gateway" "run")
                     "hermes" "hermes"))
      ;; Each as if the kernel named it `hermes', as a Hermes chat is named: the name alone is no gateway.
      (dolist (argv (list "journalctl --user -fu openclaw-gateway"
                          "journalctl --user -fu hermes-gateway.service"
                          "tail -f /home/op/.openclaw/logs/gateway.log"
                          "vim /home/op/.hermes/gateway.json"
                          "bash drive.sh openclaw gateway"
                          "openclaw logs --follow"
                          "openclaw gateway status"
                          "hermes chat -q restart the gateway please"
                          "/home/op/.hermes/hermes-agent/venv/bin/python -m hermes_cli.main chat"
                          chat
                          (format nil "~{~a~^ ~}" chat)))
        (is (notany (lambda (unit) (gateway-p argv unit "hermes")) '("openclaw" "hermes"))
            argv)))))

(defun import-report-words (home)
  "The words that say the report is a file under HOME's import folder."
  (format nil "the report is in ~a/report-"
          (nlk:home-abbreviated (namestring (merge-pathnames "import/" home)))))

(deftest import-slash-previews-then-applies (with-import-runtime (home box))
  (make-commandcode-fixture box)
  (let ((nik::*import* (list :home home)))
    (let ((line (nik::run-slash "commandcode")))
      (is (search "(dry run)" line) "a bare /import previews")
      (is (search "--yes applies" line))
      (is (null (probe-file (merge-pathnames "auth.json" home))) "and writes nothing")
      ;; The notice is the summary and where the report is, never the report
      ;; cut at a toast's four rows, and the hint that stays says where too,
      ;; the home written `~' (ip-01).
      (is-carrying (text (first (first (nlk:notice-log))))
        "(dry run)" (:absent "[providers]")
        (is (search (import-report-words home) text) text))
      (is (search (import-report-words home) line) line))
    (let ((line (nik::run-slash "commandcode --yes")))
      (is (search "imported" line))
      (is (equal +fixture-key+ (nlk:json-value (import-auth) :text "api_keys" "commandcode" "key")))
      (is-carrying (text (first (first (nlk:notice-log))))
        " imported · " (:absent "(dry run)")
        (is (search (import-report-words home) text) text))
      (is (search (import-report-words home) line) line))
    (is (search "not a world this build knows"
                (princ-to-string (signals-error nik:import-error (nik::run-slash "nosuchworld")))))))

(deftest import-verify-asks-the-target-once-and-keeps-the-key-out-of-the-refusal ()
  (with-stubbed-fdefinitions ((nle:complete (system user &key session-id max-tokens fallback-p)
                                (is (null fallback-p) "no fallback: the imported target alone")
                                "ok"))
    (multiple-value-bind (ok reason) (nik::verify-target)
      (is (eq t ok) "an answer is an answer")
      (is (null reason))))
  ;; The key the check sent is out of the words, whole and in part; a model
  ;; id is not a key, and stays: a shape rule took `claude-sonnet-4.5' out
  ;; of the walk's closing line (2026-09-30).
  (let ((nle::*api-key* "FAKEnotakey0123456789abcdef"))
    (with-stubbed-fdefinitions ((nle:complete (system user &key session-id max-tokens fallback-p)
                                  (error 'nle::provider-error
                                         :status 401 :provider "openrouter" :model "anthropic/claude-sonnet-4.5"
                                         :detail "{\"error\":{\"message\":\"Incorrect API key provided: FAKEnotakey0123456789abcdef for anthropic/claude-sonnet-4.5\"}}")))
      (multiple-value-bind (ok reason code status) (nik::verify-target)
        (is (null ok) "a refusal is a refusal")
        (is (equal "auth" code))
        (is (eql 401 status))
        (is-carrying (text reason)
          "Incorrect API key provided: [redacted]" "anthropic/claude-sonnet-4.5"
          (:absent "FAKEnotakey") (:absent "provider request failed") (:absent "/connect"))))
    (with-stubbed-fdefinitions ((nle:complete (system user &key session-id max-tokens fallback-p)
                                  (error "rejected FAKEnotakey012 (claude-opus-5-5)")))
      (is-carrying (text (nth-value 1 (nik::verify-target)))
        "rejected [redacted] (claude-opus-5-5)" (:absent "FAKEnotakey"))))
  ;; A !command's key is what it prints, which its text does not hold: the
  ;; chain probed answered the command, and a refusal quoting the key the
  ;; check sent kept it (2026-09-30).
  (let ((nle::*api-key* "!printf %s%s KEYhead0 tail0123456789"))
    (with-stubbed-fdefinitions ((nle:complete (system user &key session-id max-tokens fallback-p)
                                  (error 'nle::provider-error
                                         :status 401
                                         :detail "{\"error\":{\"message\":\"Invalid key KEYhead0tail0123456789\"}}")))
      (is-carrying (text (nth-value 1 (nik::verify-target)))
        "Invalid key [redacted]" (:absent "KEYhead0tail") (:absent "tail0123456789"))))
  (with-stubbed-fdefinitions ((nle:complete (system user &key session-id max-tokens fallback-p)
                                (error 'nle::provider-connection-refused
                                       :detail "nothing listens at 127.0.0.1:1 — is the server running?")))
    (multiple-value-bind (ok reason code) (nik::verify-target)
      (is (null ok))
      (is (search "nothing listens at 127.0.0.1:1" reason))
      (is (equal "connection_refused" code) "a refusal of the port is named by its class, not its words"))))

(deftest import-scan-answers-the-pasteable-report (with-import-runtime (home box))
  (make-hermes-fixture box)
  (let ((nik::*import* (list :home home)))
    (let ((value (nik::run-method "scan" (nlk:json-object "source" "hermes"))))
      (is (hash-table-p value))
      (is (plusp (gethash "imported" (gethash "summary" value))))
      (is (equalp #("hermes") (gethash "worlds" value)))
      (is (not (search +fixture-key+ (gethash "report" value))))
      (is (vectorp (gethash "unmapped" value))))))

(deftest import-never-repoints-a-built-in-provider (with-import-runtime (home box))
  ;; Qwen Code lists every OpenAI-compatible endpoint under
  ;; modelProviders.openai: the wire's name, not a vendor's. Read as a
  ;; provider it repointed the built-in OpenAI at the first endpoint it met
  ;; and filed the endpoint's key under the variable's name. Each endpoint
  ;; is a provider of its own, with its key, and OpenAI is left as it was.
  (make-qwen-fixture box)
  (let ((plan (nik:make-plan :source "qwen" :settings (list :home home))))
    (dolist (item (import-items plan "providers"))
      (is (not (search "openai" (or (nik:item-destination item) ""))) "nothing is written for OpenAI"))
    (nik:apply-plan plan)
    (is (null (nth-value 1 (nlk:json-value (import-config) :any "providers" "openai"))))
    (is-shape (import-config)
      ((:text "providers" "dashscope-aliyuncs" "base_url") "https://dashscope.aliyuncs.com/compatible-mode/v1")
      ((:text "providers" "dashscope-aliyuncs" "sdk") "openai-completions")
      ((:text "providers" "localhost-11434" "base_url") "http://localhost:11434/v1"))
    (let ((auth (import-auth)))
      (is (equal +fixture-key+ (nlk:json-value auth :text "api_keys" "dashscope-aliyuncs" "key")))
      (is (equal "ollama-local-1" (nlk:json-value auth :text "api_keys" "localhost-11434" "key")))
      (dolist (id '("openai" "dashscope" "ollama"))
        (is (null (nth-value 1 (nlk:json-value auth :any "api_keys" id)))
            (format nil "no key is filed under ~a" id))))))

(deftest import-gives-a-key-to-the-endpoint-that-names-its-variable (with-import-runtime (home box))
  ;; OPENAI_API_KEY is OpenAI's variable, and a Qwen home that names it for
  ;; another endpoint means that endpoint's key: it lands there, and OpenAI,
  ;; which the home never gave a key, keeps none.
  (make-qwen-fixture box :key-name "OPENAI_API_KEY")
  (nik:apply-plan (nik:make-plan :source "qwen" :settings (list :home home)))
  (let ((auth (import-auth)))
    (is (equal +fixture-key+ (nlk:json-value auth :text "api_keys" "dashscope-aliyuncs" "key")))
    (is (null (nth-value 1 (nlk:json-value auth :any "api_keys" "openai"))))))

(deftest import-follows-a-moved-endpoint-with-the-model-that-named-it (with-import-runtime (home box))
  ;; A home that points its `openai' at a proxy and picks `openai/gpt-5.5'
  ;; means the proxy: the endpoint lands under its own id and the pick goes
  ;; with it, so the default model is the one that worked there.
  (import-write box ".config/opencode/opencode.json"
                (format nil "{\"model\": \"openai/gpt-5.5\", \"provider\": {\"openai\": {\"options\": {\"baseURL\": \"https://llm.corp.example.test/v1\", \"apiKey\": \"~a\"}}}}"
                        +fixture-key+))
  (let ((plan (nik:make-plan :source "opencode" :settings (list :home home))))
    (is (search "gpt-5.5 on llm-corp-example"
                (nik:item-reason (import-item plan "providers" "general.default_model"))))
    (is (import-item plan "providers" "providers.llm-corp-example"))
    (is (null (import-item plan "providers" "providers.openai")))))

(deftest import-places-a-bare-model-on-the-one-provider-that-serves-it (with-import-runtime (home box))
  ;; am-04: Claude Code writes `opus' beside the Anthropic key it keeps, Codex
  ;; `gpt-5.6' beside its OpenAI key. Each was dropped as naming no provider,
  ;; `which no provider here can reach', under the very line that landed the key.
  (flet ((pick (world)
           (import-item (nik:make-plan :source world :settings (list :home home) :only "providers")
                        "providers" "general.default_model")))
    (import-write box ".claude/settings.json"
                  (format nil "{\"model\": \"opus\", \"env\": {\"ANTHROPIC_API_KEY\": \"~a\"}}" +fixture-key+))
    ;; A line of models by its word: the newest the catalog lists, and the report says what it read.
    (is (equal "claude-opus-5-5 on anthropic, for Claude Code's opus" (nik:item-reason (pick "claude"))))
    (import-write box ".codex/config.toml" (format nil "model = \"gpt-5.6\"~%"))
    (import-write box ".codex/auth.json" (format nil "{\"OPENAI_API_KEY\": \"~a\"}" +fixture-key-2+))
    (let ((plan (nik:make-plan :source "codex" :settings (list :home home) :only "providers")))
      (is (equal "gpt-5.6 on openai, the one imported provider that serves it"
                 (nik:item-reason (import-item plan "providers" "general.default_model"))))
      (nik:apply-plan plan)
      (is-shape (import-config)
        ((:text "general" "default_model" "provider") "openai")
        ((:text "general" "default_model" "model") "gpt-5.6")))
    ;; What no imported key serves is said so, never as a provider it names.
    (import-write box ".claude/settings.json"
                  (format nil "{\"model\": \"opusplan\", \"env\": {\"ANTHROPIC_API_KEY\": \"~a\"}}" +fixture-key+))
    (let ((item (pick "claude")))
      (is (equal "skipped" (nik:item-status item)))
      (is (equal "Claude Code's opusplan names no provider, and no key imported here serves it: pick a model in /models"
                 (nik:item-reason item))))
    ;; Two imported providers that both serve it: neither is guessed.
    (import-write box ".claude/settings.json"
                  (format nil "{\"model\": \"opus\", \"env\": {\"ANTHROPIC_API_KEY\": \"~a\", \"OPENROUTER_API_KEY\": \"~a\"}}"
                          +fixture-key+ +fixture-key-2+))
    (is (search "each serve it: pick one in /models" (nik:item-reason (pick "claude"))))))

(deftest import-keeps-one-bot-per-token (with-import-runtime (home box))
  ;; Two harnesses' Telegram bots are two bots when their tokens differ, and
  ;; one section holds one: the newest home's is kept, the other is named and
  ;; left out, and an allowlist or an owner of one is never the other's.
  (make-hermes-fixture box)
  (make-openclaw-fixture box :token "FAKE-not-a-token-openclaw-987654"
                             :discord "FAKE-not-a-token-openclaw-discord")
  (flet ((items (plan platform)
           (remove-if-not (lambda (item) (equal (format nil "channels.~a" platform) (nik:item-destination item)))
                          (import-items plan "channels"))))
    (let ((plan (nik:make-plan :settings (list :home home) :only "channels")))
      (dolist (platform '("telegram" "discord"))
        (is (equal '("conflict" "imported") (sort (mapcar #'nik:item-status (items plan platform)) #'string<))
          (format nil "one ~a section, one bot named and left out" platform)))
      (is (find "hermes and openclaw both have a Telegram bot; kept hermes, left openclaw's out"
                (items plan "telegram") :key #'nik:item-reason :test #'equal))
      (nik:apply-plan plan)
      (is-shape (import-config)
        ((:any "channels" "telegram" "allowed_users") equalp #("8071918233" "someone"))
        ((:any "channels" "telegram" "owner") equalp #("8071918233" "someone"))
        ((:any "channels" "discord" "allowed_users") equalp #("42"))
        ((:any "channels" "discord" "owner") equalp #("42")))
      (is (equal +fixture-token+
                 (string-trim '(#\Newline)
                              (uiop:read-file-string (merge-pathnames "secrets/channels.telegram.token_file" home)))) "the token is the kept bot's")
      (is (not (search "9001" (uiop:read-file-string nle::*shared-config-path*))) "and nobody from the other bot is on it"))
    ;; A section already here outranks both, and neither is named as kept.
    (write-temp-file nle::*shared-config-path* "{\"channels\": {\"telegram\": {\"enabled\": true}}}")
    (let ((plan (nik:make-plan :settings (list :home home) :only "channels")))
      (is (equal '("conflict" "conflict") (mapcar #'nik:item-status (items plan "telegram"))))
      (is (every (lambda (item) (search "section already here" (nik:item-reason item)))
                 (items plan "telegram"))))))

(deftest import-merges-a-bot-two-homes-share (with-import-runtime (home box))
  ;; The same token is the same bot, whichever harness holds it: one section,
  ;; and its allowlist and owners are the union of both homes' -- each
  ;; already trusted those people with that very bot.
  (make-hermes-fixture box)
  (make-openclaw-fixture box)
  (let ((plan (nik:make-plan :settings (list :home home) :only "channels")))
    (is (equal '("imported")
               (mapcar #'nik:item-status
                       (remove-if-not (lambda (item) (equal "channels.telegram" (nik:item-destination item)))
                                      (import-items plan "channels")))))
    (nik:apply-plan plan)
    (is-shape (import-config)
      ((:any "channels" "telegram" "allowed_users") equalp #("8071918233" "someone" "9001"))
      ((:any "channels" "telegram" "owner") equalp #("8071918233" "someone" "9001")))))

(deftest import-reads-an-openclaw-bot-as-its-docs-write-it (with-import-runtime (home box))
  ;; xn-201: Openclaw keeps a bot under channels.<platform>, its token in the
  ;; file `tokenFile' names (which wins) or inline, and who may talk as
  ;; `allowFrom' (direct messages) and `groupAllowFrom' (senders in the groups
  ;; it lists), user ids a `tg:' may lead (docs/channels/telegram/access-control.md).
  ;; Only `allowedUsers' was read, so the bot was dropped as having no allowlist.
  (flet ((plan-item ()
           (import-item (nik:make-plan :source "openclaw" :settings (list :home home)
                                       :only "channels" :overwrite t)
                        "channels" "channels.telegram")))
    (import-write box ".openclaw/credentials/telegram.token" (format nil "~a~%" +fixture-token+))
    (import-write box ".openclaw/openclaw.json"
                  "{\"channels\": {\"telegram\": {\"enabled\": true, \"dmPolicy\": \"allowlist\",
  \"botToken\": \"123:inline\", \"tokenFile\": \"~/.openclaw/credentials/telegram.token\",
  \"allowFrom\": [\"tg:9001\", \"accessGroup:operators\"], \"groupAllowFrom\": [\"9003\", \"9001\"]}}}")
    (let ((plan (nik:make-plan :source "openclaw" :settings (list :home home) :only "channels")))
      ;; A sender Openclaw lets in only in a group is not let in here, where a listed user talks anywhere.
      (is (search "1 user, 0 chats, token in a 0600 file; groupAllowFrom left out"
                  (nik:item-reason (import-item plan "channels" "channels.telegram"))))
      (nik:apply-plan plan)
      (is-shape (import-config)
        ((:any "channels" "telegram" "allowed_users") equalp #("9001"))
        ((:any "channels" "telegram" "owner") equalp #("9001"))
        ((:any "channels" "telegram" "enabled") eq t))
      ;; The file's token, not the inline one, lands where an inline one does: a 0600 file of this home's.
      (is (equal +fixture-token+
                 (nlk:trimmed (uiop:read-file-string
                               (merge-pathnames "secrets/channels.telegram.token_file" home))))))
    ;; `*' lets anybody in, which is no allowlist: still refused.
    (import-write box ".openclaw/openclaw.json"
                  (format nil "{\"channels\": {\"telegram\": {\"botToken\": \"~a\", \"dmPolicy\": \"open\", \"allowFrom\": [\"*\"]}}}"
                          +fixture-token+))
    (let ((item (plan-item)))
      (is (equal "error" (nik:item-status item)))
      (is (search "no allowlist in the home" (nik:item-reason item))))
    ;; With direct messages off there, allowFrom names only group senders: no allowlist either.
    (import-write box ".openclaw/openclaw.json"
                  (format nil "{\"channels\": {\"telegram\": {\"botToken\": \"~a\", \"dmPolicy\": \"disabled\", \"allowFrom\": [\"9001\"]}}}"
                          +fixture-token+))
    (let ((item (plan-item)))
      (is (equal "error" (nik:item-status item)))
      (is (search "allowFrom left out" (nik:item-reason item))))
    ;; A token file that does not read, a link among them, gives no token, and its path is named.
    (sb-posix:symlink (namestring (merge-pathnames ".openclaw/credentials/telegram.token" box))
                      (namestring (merge-pathnames ".openclaw/credentials/linked.token" box)))
    (dolist (path '("~/.openclaw/credentials/missing.token" "~/.openclaw/credentials/linked.token"))
      (import-write box ".openclaw/openclaw.json"
                    (format nil "{\"channels\": {\"telegram\": {\"botToken\": \"~a\", \"tokenFile\": \"~a\", \"allowFrom\": [\"9001\"]}}}"
                            +fixture-token+ path))
      (let ((item (plan-item)))
        (is (equal "skipped" (nik:item-status item)) path)
        (is (search (file-namestring path) (nik:item-reason item)) path))))
  ;; A bot switched off there lands off, and is not held for the watch to turn on.
  (import-write box ".openclaw/openclaw.json"
                (format nil "{\"channels\": {\"telegram\": {\"enabled\": false, \"botToken\": \"~a\", \"allowFrom\": [\"9001\"]}}}"
                        +fixture-token+))
  (let* ((nik::*running-probe* (lambda (units) (declare (ignore units)) (list "pid 4242: openclaw gateway")))
         (plan (nik:make-plan :source "openclaw" :settings (list :home home) :only "channels"
                              :overwrite t :takeover nil)))
    (is (search "off in openclaw, so it lands off"
                (nik:item-reason (import-item plan "channels" "channels.telegram"))))
    (nik:apply-plan plan)
    (is (not (nlk:config-boolean (nlk:json-value (import-config) :any "channels" "telegram") "enabled" t)))
    (is (null (nik::held-platforms (list :home home))))))
;;; --- memories and skills: definitions -----------------------------------------------

(deftest import-nodecode-world-converts-both-carriers-and-seeds-the-ledger (with-import-runtime (home box))
  (make-nodecode-fixture box)
  (let ((plan (nik:make-plan :source "nodecode")))
    (is (equal '("conflict" "imported" "imported" "imported")
               (sort (copy-list (import-statuses plan "memory")) #'string<)) "a second memory of one name is named")
    (is (search "a memory named quiet-output came from quiet-output.md first"
                (nik:item-reason (find "conflict" (import-items plan "memory") :key #'nik:item-status :test #'equal))))
    (is (equal '("imported") (import-statuses plan "skills")) "neither the archived skill nor a proposed one")
    (is (equal "Split red tests from the baseline. (not carried: scripts/run.sh (7 bytes))"
               (nik:item-reason (import-item plan "skills" "red-baseline"))) "a skill's line is its description")
    (is (equal '("imported") (import-statuses plan "usage")))
    (nik:apply-plan plan))
  (is-present (memory (kept-text "quiet-output")) "a memory is a define-memory"
    (is (search "(define-memory quiet-output" memory))
    (is (search "\"The operator wants quiet output, said \\\"twice\\\".\"" memory) "the description escaped")
    (is (search ":type :feedback" memory))
    (is (search ":sources (\"s-ONE\" \"s-TWO\")" memory))
    (is (search ";; created 2026-09-01T10:00:00.000Z · updated 2026-09-20T10:00:00.000Z" memory))
    (is (search ";; Said on 2026-09-01; a log line counts as noise." memory)))
  (is (search ":project \"/tmp/proj\"" (kept-text "repo-builds")) "a project's memory keeps its root")
  (is-present (skill (kept-text "red-baseline")) "a skill is a prose define-skill"
    (is (search ";; origin: reflection" skill))
    (is (search ";; --- references/notes.md ---" skill))
    (is (search ";; Read the FAIL lines." skill))
    (is (search "support files not carried: scripts/run.sh (7 bytes)" skill)))
  (is (null (kept-text "old")))
  (is (null (kept-text "maybe")))
  (flet ((uses (name)
           (loop for line in (nlk:knowledge-ledger-lines)
                 when (equal name (nlk:json-value line :string "name"))
                   collect (list (nlk:json-value line :string "kind") (nlk:json-value line :string "at")))))
    (is (find '("keep" "2026-09-20T10:00:00.000Z") (uses "quiet-output") :test #'equal) "kept at its own date")
    (is (find "view" (uses "quiet-output") :key #'first :test #'equal) "listed hot, so viewed now")
    (is (find '("helped" "2026-09-22T10:00:00.000Z") (uses "quiet-output") :test #'equal) "and its sighting")
    (is (not (find "view" (uses "cold-fact") :key #'first :test #'equal)) "a cold one is not viewed")
    (is (find '("view" "2026-09-23T10:00:00.000Z") (uses "red-baseline") :test #'equal) "the library's ledger")
    (is (not (find "keep" (uses "red-baseline") :key #'first :test #'equal)) "a skill the ledger knows is not kept again"))
  (is (equal '("skipped") (import-statuses (nik:make-plan :source "nodecode" :only "usage") "usage")) "the ledger comes over once")
  (is (equal "skipped" (nik:item-status (import-item (nik:make-plan :source "nodecode" :only "memory") "memory" "quiet-output")))))

(deftest import-nodecode-world-unpins-the-retired-indexes (with-import-runtime (home box))
  ;; Every session the retired cells pinned keeps its rows until they are taken
  ;; out, teaching verbs that are gone beside the index that replaced them.
  (make-nodecode-fixture box)
  (with-temp-store ()
    (nlk:create-session :id "s-old" :cwd "/tmp")
    (nlk:set-harness-section "s-old" "memory-index" "(memory:read \"name\") opens one.")
    (nlk:set-harness-section "s-old" "skills-index" "(skills:view \"name\") before following one.")
    (nlk:set-harness-section "s-old" "soul" "be kind")
    (let ((plan (nik:make-plan :source "nodecode" :only "standing")))
      (is (equal "2 pins of memory-index and skills-index on 1 session, unpinned"
                 (nik:item-reason (first (import-items plan "standing")))))
      (nik:apply-plan plan))
    (is (equal '(("soul" . "be kind")) (nlk:list-harness-sections "s-old")) "the retired pins go, the rest stay")
    (is (equal '("skipped") (import-statuses (nik:make-plan :source "nodecode" :only "standing") "standing")))))

(deftest import-claude-memory-reads-frontmatter-and-resolves-its-project (with-import-runtime (home box))
  (import-write box ".claude.json" "{\"projects\": {\"/tmp/proj.x\": {}}}")
  (import-write box ".claude/settings.json" "{}")
  (import-write box ".claude/projects/-tmp-proj-x/memory/MEMORY.md"
                (format nil "- [grok-reasons](grok-reasons.md) — grok bills reasoning~%"))
  (import-write box ".claude/projects/-tmp-proj-x/memory/grok-reasons.md"
                (format nil "---~%name: grok-reasons~%description: grok bills reasoning it never shows~%metadata:~%  type: reference~%---~%~%Seen on both lanes.~%"))
  (let ((facts (home-facts-of "claude")))
    (is-present (value (nik::fact-value (find "grok-reasons" (remove :memory facts :key #'nik:fact-kind :test-not #'eq)
                                              :key #'nik:fact-id :test #'equal))) "one fact"
      (is (equal "reference" (getf value :type)))
      (is (equal "/tmp/proj.x" (getf value :project)) "its folder resolves through ~/.claude.json")
      (is (equal "grok bills reasoning it never shows" (getf value :description))))
    (is (equal '("grok-reasons") (fact-ids facts :hot)) "its MEMORY.md is the hot index")))

(deftest import-definition-text-reads-back-whole (with-import-runtime (home box))
  (let* ((description "A \"quoted\" path C:\\x; not a comment")
         (lines (list "first (with parens)" "" "a ;; second comment" "\"quoted\" too"))
         (text (nik::definition-text "define-memory" "kn-round" description lines
                                     :type :lesson :sources '("s-1")))
         (form (let ((*package* (find-package '#:nodecode.evolved)) (*read-eval* nil))
                 (read-from-string text))))
    (is (equal description (third form)) "the description reads back as written")
    (is (equal '(:type :lesson :sources ("s-1")) (cdddr form)) "the options, and the prose is comments")
    (nik::keep-definition "test" "kn-round" text)
    (is (equal (format nil "~{~a~^~%~}" lines) (nlk::knowledge-prose (nlk:knowledge-entry-named "kn-round" nil))))))
