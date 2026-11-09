;;;; nodecode-town.asd --- the town: one resident per session, a building per tool.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional cell, NOT part of the organism core: nothing in src/ names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `town' section, hooks the :frame and :tool points, registers
;;;; /town and serves its page and snapshot on the gateway's own port.

(defsystem "nodecode-town"
  :description "A pixel town of the running sessions: one resident per session, a building per tool, a page to watch"
  :license "MIT"
  :author "sxuff (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "page")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-town/test"))))

(defsystem "nodecode-town/test"
  :description "Town tests. Registered into the shared nodecode.test registry; RUN-TOWN-TESTS filters by the TOWN-CELL- name prefix."
  :license "MIT"
  :author "sxuff (original), Nodecode (rewrite)"
  :depends-on ("nodecode-town" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-town-tests)))
