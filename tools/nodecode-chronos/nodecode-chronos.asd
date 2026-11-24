;;;; nodecode-chronos.asd --- the workspace, checkpointed and put back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `chronos' section, hooks the :tool point, registers the four
;;;; chronos_* tools, puts /chronos in the one slash catalog and the manual on
;;;; (help :chronos).

(defsystem "nodecode-chronos"
  :description "Checkpoint the workspace before a tool call and roll it back after"
  :author "DevTarlow (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "git")
               (:file "engine")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-chronos/test"))))

(defsystem "nodecode-chronos/test"
  :description "Chronos tests. Registered into the shared nodecode.test registry; RUN-CHRONOS-TESTS filters by the CHRONOS-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-chronos" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-chronos-tests)))
