;;;; nodecode-recovery-peer.asd --- one bounded second opinion when a tool call stalls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of rezaaibox-ui/nexus-recovery-peer. The folder loader loads
;;;; it at boot when its directory sits under ~/.nodecode/cells/, and the
;;;; gateway calls START-CELL after recovery, which reads the `recovery-peer'
;;;; config section, hooks the :TOOL point, and registers /recovery-peer and
;;;; the recovery_peer tool. Every dependency is already in the serving image.

(defsystem "nodecode-recovery-peer"
  :description "One bounded second opinion from another model when a tool call keeps failing"
  :license "MIT"
  :author "rezaaibox-ui (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-recovery-peer/test"))))

(defsystem "nodecode-recovery-peer/test"
  :description "Recovery peer tests. Registered into the shared nodecode.test registry; RUN-RECOVERY-PEER-TESTS filters by the RECOVERY-PEER-CELL- name prefix."
  :license "MIT"
  :author "rezaaibox-ui (original), Nodecode (rewrite)"
  :depends-on ("nodecode-recovery-peer" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-recovery-peer-tests)))
