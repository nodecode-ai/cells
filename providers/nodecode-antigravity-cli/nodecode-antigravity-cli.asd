;;;; nodecode-antigravity-cli.asd --- the Antigravity CLI, on a subscription, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `antigravity-cli' section, registers the antigravity-cli lane
;;;; and installs the hooks cell.lisp lists.
;;;;
;;;; Rewritten from soyelmismo's hermes-antigravity-subscription (see NOTICE).
;;;; Every dependency rides with nodecode.

(defsystem "nodecode-antigravity-cli"
  :description "Antigravity CLI: Gemini and Claude models through the agy CLI on an Antigravity subscription"
  :license "MIT"
  :author "soyelmismo (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it; the login is the CLI's
  :properties ((:serves (:provider "antigravity-cli" :name "Antigravity CLI" :sign-in "cli")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-antigravity-cli/test"))))

(defsystem "nodecode-antigravity-cli/test"
  :description "Antigravity CLI tests. Registered into the shared nodecode.test registry; RUN-ANTIGRAVITY-CLI-TESTS filters by the ANTIGRAVITY-CLI-CELL- name prefix."
  :license "MIT"
  :author "soyelmismo (original), Nodecode (rewrite)"
  :depends-on ("nodecode-antigravity-cli" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-antigravity-cli-tests)))
