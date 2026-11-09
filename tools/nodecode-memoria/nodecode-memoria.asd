;;;; nodecode-memoria.asd --- Memoria Cloud memory as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `memoria' section, registers the six memory tools, puts the
;;;; manual on (help :memoria) and makes auto recall answer each turn.
;;;;
;;;; A rewrite of matrixorigin/memoria's Hermes plugin (see NOTICE).

(defsystem "nodecode-memoria"
  :description "Memoria Cloud memory: scoped recall, explicit save/correct/forget tools and a profile page"
  :license "Apache-2.0"
  :version "0.1.0"
  :author "matrixorigin (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "client")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-memoria/test"))))

(defsystem "nodecode-memoria/test"
  :description "Memoria tests. Registered into the shared nodecode.test registry; RUN-MEMORIA-TESTS filters by the MEMORIA-CELL- name prefix."
  :license "Apache-2.0"
  :author "matrixorigin (original), Nodecode (rewrite)"
  :depends-on ("nodecode-memoria" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-memoria-tests)))
