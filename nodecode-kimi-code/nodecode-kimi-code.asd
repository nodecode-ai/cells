;;;; nodecode-kimi-code.asd --- Kimi Code, Moonshot's coding subscription, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `kimi-code' section, installs the hooks cell.lisp lists and
;;;; registers /kimi-code, the sign-in.
;;;;
;;;; Ported from oh-my-pi's kimi-code provider (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-kimi-code"
  :description "Kimi Code: Moonshot's coding subscription as a Nodecode provider, signed in with a device code"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "signin")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-kimi-code/test"))))

(defsystem "nodecode-kimi-code/test"
  :description "Kimi Code tests. Registered into the shared nodecode.test registry; RUN-KIMI-CODE-TESTS filters by the KIMI-CODE-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-kimi-code" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-kimi-code-tests)))
