;;;; nodecode-hermesmade.asd --- the community bot shelf at hermesmade.lol.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `hermesmade' section, registers the three tools, /hermesmade
;;;; and (help :hermesmade). Every dependency is already in the serving image.

(defsystem "nodecode-hermesmade"
  :description "The community bot shelf: preview a bot, add it, share your own"
  :author "Noshkoto (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode" "cl-ppcre" "chipz" "salza2")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "pack")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-hermesmade/test"))))

(defsystem "nodecode-hermesmade/test"
  :description "Hermesmade cell tests. Registered into the shared nodecode.test registry; RUN-HERMESMADE-TESTS filters by the HERMESMADE-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-hermesmade" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-hermesmade-tests)))
