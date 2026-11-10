;;;; nodecode-scar.asd --- the rm shape an operator denied, stopped when it
;;;; comes back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A peripheral cell: one START-CELL, one :TOOL hook, one advice on the
;;;; question seam, four tools and one slash command. The folder loader
;;;; (kernel cells.lisp) loads this system at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; and the two hooks read this home's scar.json on every call.

(defsystem "nodecode-scar"
  :description "Scar: the rm shape an operator denied is stopped when it comes back"
  :license "MIT"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode" "cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "shape")
               (:file "ledger")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-scar/test"))))

(defsystem "nodecode-scar/test"
  :description "Scar tests. Registered into the shared nodecode.test registry; RUN-SCAR-TESTS filters by the SCAR-CELL- name prefix."
  :license "MIT"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :depends-on ("nodecode-scar" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-scar-tests)))
