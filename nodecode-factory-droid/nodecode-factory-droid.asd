;;;; nodecode-factory-droid.asd --- Factory Droid, Factory's model subscription, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `factory-droid' section, installs the hooks cell.lisp lists and
;;;; registers /factory-droid, the sign-in.
;;;;
;;;; Ported from oh-my-pi's factory-droid provider (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-factory-droid"
  :description "Factory Droid: Factory's model subscription as a Nodecode provider, signed in with a WorkOS device code"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "wire")
               (:file "signin")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-factory-droid/test"))))

(defsystem "nodecode-factory-droid/test"
  :description "Factory Droid tests. Registered into the shared nodecode.test registry; RUN-FACTORY-DROID-TESTS filters by the FACTORY-DROID-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-factory-droid" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-factory-droid-tests)))
