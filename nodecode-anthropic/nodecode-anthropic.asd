;;;; nodecode-anthropic.asd --- an Anthropic Claude Pro/Max sign-in for the anthropic provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `anthropic' section and installs the hooks and the /anthropic
;;;; command cell.lisp lists.
;;;;
;;;; Ported from oh-my-pi's anthropic sign-in and its Claude Code transport
;;;; (see NOTICE). Every dependency rides with nodecode.

(defsystem "nodecode-anthropic"
  :description "Anthropic (Claude Pro/Max): sign in, and serve the anthropic provider on the subscription when no key is set"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "signin")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-anthropic/test"))))

(defsystem "nodecode-anthropic/test"
  :description "anthropic tests. Registered into the shared nodecode.test registry; RUN-ANTHROPIC-TESTS filters by the ANTHROPIC-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-anthropic" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-anthropic-tests)))
