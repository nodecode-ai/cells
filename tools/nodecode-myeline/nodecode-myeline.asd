;;;; nodecode-myeline.asd --- Myeline: memory with receipts, as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `myeline' section, registers the four cortex_* tools and
;;;; /myeline, and puts (help :myeline) on the manual.
;;;;
;;;; A Nodecode rewrite of Sandbox-Group-LLC/myeline-hermes (see NOTICE).

(defsystem "nodecode-myeline"
  :description "Myeline: memory with receipts - facts carry their source, nothing goes live until a human approves it, and a retraction keeps its reason"
  :license "MIT"
  :version "0.1.0"
  :author "Sandbox-Group-LLC (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-myeline/test"))))

(defsystem "nodecode-myeline/test"
  :description "Myeline tests. Registered into the shared nodecode.test registry; RUN-MYELINE-TESTS filters by the MYELINE-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "Sandbox-Group-LLC (original), Nodecode (rewrite)"
  :depends-on ("nodecode-myeline" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-myeline-tests)))
