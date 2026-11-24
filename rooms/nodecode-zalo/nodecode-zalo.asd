;;;; nodecode-zalo.asd --- Zalo: a bot on the Zalo Bot Platform, and the agent behind it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of moken627-hub/hermes-plugin-zalo (see NOTICE). Optional
;;;; ASDF cell, not part of the organism core: the folder loader loads it at
;;;; boot when its directory sits under ~/.nodecode/cells/; the gateway calls
;;;; START-CELL after recovery, which reads the `channels.zalo' section,
;;;; starts one long-poll lap, and registers /zalo and the zalo_send tool.

(defsystem "nodecode-zalo"
  :description "Zalo: talk to nodecode through a Zalo bot"
  :license "MIT"
  :version "0.1.0"
  :author "moken627-hub (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-zalo/test"))))

(defsystem "nodecode-zalo/test"
  :description "Zalo tests. Registered into the shared nodecode.test registry; RUN-ZALO-TESTS filters by the ZALO-CELL- name prefix."
  :license "MIT"
  :author "moken627-hub (original), Nodecode (rewrite)"
  :depends-on ("nodecode-zalo" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-zalo-tests)))
