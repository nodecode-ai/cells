;;;; nodecode-aside.asd --- the signed-in Aside browser, as tools a model calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the Hermes Agent plugin leonphull/hermes-aside-browser (see
;;;; NOTICE). Optional ASDF cell, not part of the organism core: the folder
;;;; loader loads it at boot when its directory sits under ~/.nodecode/cells/,
;;;; and the gateway calls START-CELL after recovery, which reads the `aside'
;;;; section, registers aside_status, aside_exec, aside_repl, aside_session
;;;; and aside_memory, the /aside command, the tool gate and (help :aside).

(defsystem "nodecode-aside"
  :description "Aside Browser: drives the user's signed-in Aside browser through the aside CLI"
  :license "MIT"
  :author "leonphull (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-aside/test"))))

(defsystem "nodecode-aside/test"
  :description "Aside Browser tests. Registered into the shared nodecode.test registry; RUN-ASIDE-TESTS filters by the ASIDE-CELL- name prefix."
  :license "MIT"
  :author "leonphull (original), Nodecode (rewrite)"
  :depends-on ("nodecode-aside" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-aside-tests)))
