;;;; nodecode-mind-kit.asd --- meters, one aggregate, one read-only gate.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; An optional cell, not part of the organism core: the folder loader loads
;;;; it at boot when it sits under ~/.nodecode/cells/, and the gateway calls
;;;; START-CELL, which reads the `mind_kit' section and registers two tools,
;;;; /mind and the manual topic (help :mind-kit). brain/ beside this file is
;;;; the kit's 25 subsystem example files, copied into the agent directory by
;;;; (mind-kit:seed); nothing in it is compiled.

(defsystem "nodecode-mind-kit"
  :description "Meters for a long-running agent: 25 subsystem state files, one folded brain-state.json and a read-only decision gate"
  :license "MIT"
  :version "0.1.0"
  :author "ousiaresearch (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-mind-kit/test"))))

(defsystem "nodecode-mind-kit/test"
  :description "Mind kit tests: seeding, the fold, the gate's reason codes and the lifecycle. Registered into the shared nodecode.test registry; RUN-MIND-KIT-TESTS filters by the MIND-KIT-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-mind-kit" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-mind-kit-tests)))
