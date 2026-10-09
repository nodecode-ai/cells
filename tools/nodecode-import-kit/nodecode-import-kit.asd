;;;; nodecode-import-kit.asd --- another coding agent's home brought into this one.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional quicklisp/ASDF cell, NOT part of the organism core: nothing in
;;;; src/src names this system by more than the folder name the setup
;;;; wizard's import stage installs. It ships with Nodecode at
;;;; src/cells/import/kit/, the group for reading a foreign agent home — one
;;;; folder, one `just import-*' lane there. The folder loader (kernel cells.lisp) loads it at boot
;;;; when its directory sits under ~/.nodecode/cells/; the gateway calls
;;;; START-CELL after recovery, which reads the `import' config section and
;;;; registers /import.
;;;;
;;;; What it does: reads any coding-agent home on the box — Hermes, Codex,
;;;; Claude Code, OpenCode, Cursor, Gemini, Qwen, pi, Crush, Cline,
;;;; Windsurf, Kiro, dotagents, Openclaw, Command Code, or a directory named
;;;; on the command line — and lands what maps into this organism through
;;;; the seams that own each piece: providers and the default model into the
;;;; shared config, API keys into auth.json, standing instructions into
;;;; SOUL.md, memory entries and skills as definitions in the knowledge cell,
;;;; with their use, MCP servers and channel sections into the config, cron
;;;; jobs through the cron folder, and conversations into the store as
;;;; recorded exchanges that keep their own clock.
;;;;
;;;; The architecture is the point: readers per artifact SHAPE, worlds as
;;;; DATA. shape.lisp classifies a decoded tree — credentials, OAuth grants,
;;;; MCP maps, model picks, cron jobs, bot tokens — whatever format it came
;;;; from (json, yaml.lisp, toml.lisp); home.lisp carries the conventions
;;;; that say which files to open; the manifest table itself is the core's
;;;; (NLK:*AGENT-WORLDS*, waist/worlds.lisp), data with no code in it, so
;;;; the first frame can ask what is on the box before this folder loads.
;;;; Adding a world is adding a row there. The credential
;;;; vocabulary is models.dev's own (catalog.lisp), so a provider added
;;;; upstream is read here with nothing recompiled.
;;;;
;;;; Preview first, always: a plan is a list of items — imported, skipped,
;;;; conflict, error — and a dry run is the plan without its actions. A
;;;; foreign gateway that polls the same bots is stopped and disabled so
;;;; they answer from here; with --keep-running they land off, and a watch
;;;; turns them on once that gateway stops.
;;;;
;;;; Dependencies: nodecode (the store, the config writers, the JSON codec,
;;;; cl-sqlite through it) and bordeaux-threads (the watch). The cron, mcp
;;;; and channel folders are reached by name at run time
;;;; when a home carries something for them, and installed first when they
;;;; are not there. The .asd sits INSIDE its folder (ADR-0229): install by
;;;; putting -- or symlinking -- the directory under ~/.nodecode/cells/.
;;;; Presence is enabled; the package is named after the system, which is
;;;; how the loader finds START-CELL.

(defsystem "nodecode-import-kit"
  :description "Bring another coding agent's home into this one"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode" "bordeaux-threads")
  :serial t
  :components ((:file "package")
               (:file "seams")
               (:file "yaml")
               (:file "toml")
               (:file "catalog")
               (:file "shape")
               (:file "home")
               (:file "sessions")
               (:file "takeover")
               (:file "plan")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-import-kit/test"))))

(defsystem "nodecode-import-kit/test"
  :description "Import tests. Registered into the shared nodecode.test registry under the import lane's IMPORT- name prefix, which RUN-IMPORT-TESTS filters by."
  :license "MIT"
  :depends-on ("nodecode-import-kit" "nodecode-cron" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "yaml-test")
               (:file "toml-test")
               (:file "shape-test")
               (:file "world-test")
               (:file "import-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-import-tests)))
