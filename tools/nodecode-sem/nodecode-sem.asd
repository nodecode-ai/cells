;;;; nodecode-sem.asd --- Ataraxy Labs' Sem and Weave, as a Nodecode tool cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: the folder loader loads it at
;;;; boot, and the gateway calls START-CELL, which reads the `sem' section and
;;;; puts the four tools, the /sem command and the (help :sem) topic on the
;;;; organism that cell.lisp lists.
;;;;
;;;; Ported from the Hermes agent plugin of Ataraxy-Labs/sem (two prose skills
;;;; driving the `sem' and `weave' CLIs). It shells out to the CLIs the user
;;;; installs themselves, through nlk:run-bounded: no Node, no Python, no
;;;; bundled copy of either CLI.

(defsystem "nodecode-sem"
  :description "Sem and Weave: entity-level code discovery and review, and branch merge previews, through the local CLIs"
  :license "MIT"
  :author "Ataraxy-Labs (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cli")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-sem/test"))))

(defsystem "nodecode-sem/test"
  :description "Sem cell tests: every CLI call stubbed, no process started, no repository read."
  :license "MIT"
  :author "Ataraxy-Labs (original), Nodecode (rewrite)"
  :depends-on ("nodecode-sem" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-sem-tests)))
