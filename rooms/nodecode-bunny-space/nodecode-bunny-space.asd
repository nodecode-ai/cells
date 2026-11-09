;;;; nodecode-bunny-space.asd --- Bunny Space: a bot in the owner's rooms, and the agent behind it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of jasm001/hermes-plugin-bunny-space (see NOTICE). Optional
;;;; ASDF cell, not part of the organism core: the folder loader loads it at
;;;; boot when its directory sits under ~/.nodecode/cells/; the gateway calls
;;;; START-CELL after recovery, which reads the `channels.bunny_space'
;;;; section, starts one outbound pull lap, and registers /bunny and the
;;;; bunny_send tool.

(defsystem "nodecode-bunny-space"
  :description "Bunny Space: talk to nodecode through a Bunny Space bot"
  :license "MIT"
  :version "0.1.0"
  :author "jasm001 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-bunny-space/test"))))

(defsystem "nodecode-bunny-space/test"
  :description "Bunny Space tests. Registered into the shared nodecode.test registry; RUN-BUNNY-SPACE-TESTS filters by the BUNNY-SPACE-CELL- name prefix."
  :license "MIT"
  :author "jasm001 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-bunny-space" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-bunny-space-tests)))
