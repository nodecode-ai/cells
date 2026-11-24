;;;; nodecode-crew.asd --- a board of cards, each done when its proof command exits 0.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/src
;;;; names this system. The folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/; the gateway calls START-CELL after
;;;; recovery, which reads the `crew' section, registers the five crew_*
;;;; tools, /crew and the manual on (help :crew). A card is one readable
;;;; file under the home; every dependency is already in the serving image.

(defsystem "nodecode-crew"
  :description "Cards with a contract that are done only when their proof command exits 0"
  :license "Apache-2.0"
  :version "0.1.0"
  :author "macd2 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-crew/test"))))

(defsystem "nodecode-crew/test"
  :description "Crew tests. Registered into the shared nodecode.test registry; RUN-CREW-TESTS filters by the CREW-CELL- name prefix."
  :license "Apache-2.0"
  :depends-on ("nodecode-crew" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-crew-tests)))
