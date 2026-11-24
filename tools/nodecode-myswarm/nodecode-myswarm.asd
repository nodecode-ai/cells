;;;; nodecode-myswarm.asd --- an agents.md read four ways, and taken in two.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `myswarm' section, registers the six tools, puts /myswarm and
;;;; the manual topic on, and hands the model the primer.
;;;;
;;;; A Nodecode rewrite of tomkubik/hermes-myswarm, a Hermes Agent plugin that
;;;; listed, diagrammed, exported (with best-effort PII pruning) and imported
;;;; community agents.md swarm configurations. See NOTICE.
;;;;
;;;; No key and no network: it reads and writes files, and asks no model.

(defsystem "nodecode-myswarm"
  :description "List, diagram, export (best-effort PII pruning) and import agents.md swarm configurations"
  :license "MIT"
  :author "tomkubik (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode" "cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "parse")
               (:file "parse2")
               (:file "parse3")
               (:file "render")
               (:file "scrub")
               (:file "files")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-myswarm/test"))))

(defsystem "nodecode-myswarm/test"
  :description "myswarm tests. Registered into the shared nodecode.test registry; RUN-MYSWARM-TESTS filters by the MYSWARM-CELL- name prefix."
  :license "MIT"
  :author "tomkubik (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode-myswarm" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-myswarm-tests)))
