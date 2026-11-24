;;;; nodecode-handydandy.asd --- a long session says so, and its work outlives it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/src
;;;; names this system. The folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/; the gateway calls START-CELL after
;;;; recovery, which reads the `handydandy' section, hooks the :ROUND point so
;;;; a session past its size limit is told once, registers the handoff tool,
;;;; /handydandy and puts the manual on (help :handydandy).
;;;;
;;;; A rewrite of percyhendrux123-prog's hermes-handydandy; NOTICE says what
;;;; changed. Every dependency is already in the serving image.

(defsystem "nodecode-handydandy"
  :description "Warns when a session grows too long and files a handoff for a fresh one"
  :license "MIT"
  :version "0.1.0"
  :author "percyhendrux123-prog (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-handydandy/test"))))

(defsystem "nodecode-handydandy/test"
  :description "HandyDandy tests. Registered into the shared nodecode.test registry; RUN-HANDYDDANDY-TESTS filters by the HANDYDDANDY-CELL- name prefix."
  :license "MIT"
  :author "percyhendrux123-prog (original), Nodecode (rewrite)"
  :depends-on ("nodecode-handydandy" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-handydandy-tests)))
