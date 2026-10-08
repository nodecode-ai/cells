;;;; nodecode-openrouter.asd --- OpenRouter, with its browser sign-in and its request quirks, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `openrouter' section and installs the hooks and the command
;;;; cell.lisp lists.
;;;;
;;;; Ported from oh-my-pi's openrouter provider (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-openrouter"
  :description "OpenRouter, with its browser sign-in, as a Nodecode provider"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "signin")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-openrouter/test"))))

(defsystem "nodecode-openrouter/test"
  :description "OpenRouter tests. Registered into the shared nodecode.test registry; RUN-OPENROUTER-TESTS filters by the OPENROUTER-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-openrouter" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-openrouter-tests)))
