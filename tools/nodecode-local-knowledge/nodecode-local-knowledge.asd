;;;; nodecode-local-knowledge.asd --- the local tree, as artifacts a question routes to.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `local-knowledge' section, registers the five tools cell.lisp
;;;; lists and puts the workflow on the manual as (help :local-knowledge).
;;;;
;;;; Rewritten from stepanov1975's hermes-local-knowledge (see NOTICE). The
;;;; original walked a Hermes home for skills, cron jobs and MCP servers and
;;;; kept its telemetry in SQLite; this walks any roots the section names and
;;;; keeps its record in one tab-separated file. Standard library only: the
;;;; image already has the filesystem, and no dependency is vendored.

(defsystem "nodecode-local-knowledge"
  :description "Index local skills, scripts, runbooks and docs so a question routes to the file to read first"
  :author "stepanov1975 (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "index")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-local-knowledge/test"))))

(defsystem "nodecode-local-knowledge/test"
  :description "Local-knowledge cell tests. Registered into the shared nodecode.test registry; RUN-LOCAL-KNOWLEDGE-TESTS filters by the LOCAL-KNOWLEDGE-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-local-knowledge" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-local-knowledge-tests)))
