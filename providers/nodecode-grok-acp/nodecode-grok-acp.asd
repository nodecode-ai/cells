;;;; nodecode-grok-acp.asd --- the grok CLI, on a subscription, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `grok-acp' section, registers the grok-acp lane and installs
;;;; the hooks cell.lisp lists.
;;;;
;;;; Rewritten from nicolasdmolina's hermes-grok-acp (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-grok-acp"
  :description "Grok through the grok CLI on a subscription: its own login, one ACP session per turn"
  :license "MIT"
  :author "nicolasdmolina (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it; the login is the CLI's
  :properties ((:serves (:provider "grok-acp" :name "Grok (grok CLI)" :sign-in "cli")))
  :serial t
  :components ((:file "package")
               (:file "acp")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-grok-acp/test"))))

(defsystem "nodecode-grok-acp/test"
  :description "Grok-acp tests. Registered into the shared nodecode.test registry; RUN-GROK-ACP-TESTS filters by the GROK-ACP-CELL- name prefix."
  :license "MIT"
  :author "nicolasdmolina (original), Nodecode (rewrite)"
  :depends-on ("nodecode-grok-acp" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-grok-acp-tests)))
