;;;; nodecode-kilo.asd --- Kilo Gateway, signed in with a device code, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `kilo' section and installs the hooks and the /kilo command
;;;; cell.lisp lists.
;;;;
;;;; Ported from oh-my-pi's kilo provider (see NOTICE). Every dependency rides
;;;; with nodecode.

(defsystem "nodecode-kilo"
  :description "Kilo Gateway: sign in with a device code, and serve its models"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "signin")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-kilo/test"))))

(defsystem "nodecode-kilo/test"
  :description "Kilo tests. Registered into the shared nodecode.test registry; RUN-KILO-TESTS filters by the KILO-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-kilo" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-kilo-tests)))
