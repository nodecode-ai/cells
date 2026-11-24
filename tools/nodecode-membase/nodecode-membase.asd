;;;; nodecode-membase.asd --- Membase long-term memory as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `membase' section, registers membase_search,
;;;; membase_save and membase_profile, puts /membase and (help :membase) up,
;;;; and hooks the :round point so a turn's own input is recalled into its
;;;; request. Every dependency is already in the serving image.

(defsystem "nodecode-membase"
  :description "Membase long-term memory: search, save and profile a user's hosted Memories before each turn"
  :author "unibaseio (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-membase/test"))))

(defsystem "nodecode-membase/test"
  :description "Membase tests. Registered into the shared nodecode.test registry; RUN-MEMBASE-TESTS filters by the MEMBASE-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-membase" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-membase-tests)))
