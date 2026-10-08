;;;; cell.lisp --- the primer, the settings, the one declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two hooks and a manual:
;;;;
;;;;   WRITE-FILE-TEXT   the core's one atomic writer, under EDIT and
;;;;                     WRITE-FILE: a file a server serves is recorded under
;;;;                     the session it was written for
;;;;   :TOOL             after an eval, the diagnostics of the files it wrote
;;;;                     are appended to its result, within wait_ms
;;;;   (help :lsp)       the verbs, while the cell runs
;;;;
;;;; Config, a sibling top-level key:
;;;;   "lsp": {"diagnostics_on_write": true, "wait_ms": 3000, "idle_minutes": 15,
;;;;           "servers": {"pyright": {"disabled": true},
;;;;                       "my-server": {"command": "my-ls", "args": ["--stdio"],
;;;;                                     "file_types": [".xyz"], "root_markers": [".git"]}}}
;;;; A server starts on demand, from the project's own bins or PATH, the first
;;;; time a file it serves is written or asked about, and stops after
;;;; idle_minutes unused or when the cell stops.

(in-package #:nodecode-lsp)

(defparameter +primer+
  "Language servers are available through the nodecode-lsp cell: Lisp functions in the lsp:
package, called through eval. Each returns a string. A server starts by itself the first time a
file of its language is written or asked about (rust-analyzer, pyright, gopls, clangd,
typescript-language-server and fifty more, whichever are installed), in the project root its
markers name.
After an eval that wrote files with edit or write-file, its result ends with what the servers
say about them: \"LSP diagnostics (1 error, 2 warnings):\" and one line per problem,
path:line:col [severity] [source] message (code). Nothing is added when they are clean. Fix
what it reports before moving on; \"still checking\" means the server was slow, so ask again.
  (lsp:diagnostics \"src/a.rs\")       re-reads the file and reports every diagnostic, or OK;
      a list of paths reports each.
  (lsp:definition \"src/a.rs\" \"parse\")   where parse is defined: path:line:col and the line.
  (lsp:references \"src/a.rs\" \"parse\")   every use, the declaration included.
  (lsp:hover \"src/a.rs\" \"parse\")        its type and docs.
      The symbol is found in the file by name: its first mention, \"parse#2\" the second,
      :line 40 on that line (use it when the first mention is a comment or an import).
  (lsp:symbols \"src/a.rs\")           the file's outline; (lsp:symbols \"src/a.rs\" :query \"Parser\")
      searches the whole project.
  (lsp:rename \"src/a.rs\" \"parse\" \"parse_all\")   renames it everywhere the server knows and
      writes the files; :apply nil only lists what would change. Prefer it to editing by hand.
  (lsp:status)   the servers running, their roots and any failure;  (lsp:restart [\"name\"]).
  (lsp:request \"rust-analyzer\" \"rust-analyzer/expandMacro\" :params p)   any other method, the
      reply as JSON; p a JSON string or a plist (:text-document (:uri ...)); :path a file it serves.
A verb waits at most 8 s: \"still indexing; try again\" means the project is loading.
ERROR: LSP-ERROR names the problem, a missing server included; do not retry one that is not
installed."
  "What (help :lsp) answers while the cell runs.")

(defun read-settings (values table)
  "The settings the cell runs on: what the `lsp' declaration derived
(VALUES), plus the servers: omp's defaults with the section's `servers'
object laid over them, refused here when malformed."
  (multiple-value-bind (servers present) (nlk:section-value table "servers")
    (list :diagnostics-on-write (getf values :diagnostics-on-write)
          :wait-ms (getf values :wait-ms)
          :idle-minutes (getf values :idle-minutes)
          :servers (configured-specs (and present servers)))))

(defun start ()
  "What no clause covers: the idle reaper, and every server and record
taken down on stop."
  (let ((reaper (nlk:worker-start "lsp idle" #'reap-idle :wake 60)))
    (nle:on-stop (lambda ()
                   (nlk:worker-stop reaper)
                   (stop-all-servers)
                   (forget-written)))))

(nle:define-cell lsp
  (:section ("lsp")
    (:guide "servers start on demand from the project's bins or PATH; servers.<name> overrides one of the 55 omp defines or adds one (command, args, file_types, root_markers, settings, init_options, disabled)")
    ("diagnostics_on_write" :boolean :default t
     :doc "append the language servers' diagnostics to an eval that wrote files")
    ("wait_ms" :integer :default 3000 :min 0
     :doc "how long an eval that wrote files waits for those diagnostics")
    ("idle_minutes" :integer :default 15 :min 1
     :doc "stop a language server unused this long"))
  (:settings #'read-settings)
  (:help :lsp "lsp: diagnostics after a write, definition, references, hover, symbols, rename - (help :lsp)" +primer+)
  (:hook 'nle::write-file-text #'note-write)
  (:hook :tool #'diagnose-writes)
  (:start #'start))
