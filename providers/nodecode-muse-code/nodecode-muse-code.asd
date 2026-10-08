;;;; nodecode-muse-code.asd --- Muse Code, Meta's Muse subscription, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `muse-code' section, installs the hooks cell.lisp lists and
;;;; registers /muse-code, the sign-in.
;;;;
;;;; Ported from oh-my-pi's muse-code provider (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-muse-code"
  :description "Muse Code: Meta's Muse subscription as a Nodecode provider, signed in with a device code"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "signin")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-muse-code/test"))))

(defsystem "nodecode-muse-code/test"
  :description "Muse Code tests. Registered into the shared nodecode.test registry; RUN-MUSE-CODE-TESTS filters by the MUSE-CODE-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-muse-code" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-muse-code-tests)))
