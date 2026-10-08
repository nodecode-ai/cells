;;;; nodecode-github-copilot.asd --- GitHub Copilot, signed in with GitHub, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `github-copilot' section, installs the hooks cell.lisp lists
;;;; and registers /github-copilot.
;;;;
;;;; Ported from oh-my-pi's github-copilot provider (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-github-copilot"
  :description "GitHub Copilot: a GitHub device sign-in, and Copilot's Messages, chat and Responses wires as one provider"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "signin")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-github-copilot/test"))))

(defsystem "nodecode-github-copilot/test"
  :description "GitHub Copilot tests. Registered into the shared nodecode.test registry; RUN-GITHUB-COPILOT-TESTS filters by the GITHUB-COPILOT-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-github-copilot" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-github-copilot-tests)))
