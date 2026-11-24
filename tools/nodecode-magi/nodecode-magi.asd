;;;; nodecode-magi.asd --- pending memory review and the durable store.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `magi' section and registers the tools, /magi and the
;;;; manual topic.

(defsystem "nodecode-magi"
  :description "Review pending memory writes and edit the durable memory store"
  :license "MIT"
  :author "MarcosAlves90 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-magi/test"))))

(defsystem "nodecode-magi/test"
  :description "Magi tests. Registered into the shared nodecode.test registry; RUN-MAGI-TESTS filters by the MAGI-CELL- name prefix."
  :license "MIT"
  :author "MarcosAlves90 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-magi" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-magi-tests)))
