;;;; nodecode-anyrouter.asd --- AnyRouter, its gateway and its tool-capable catalog, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `anyrouter' section and installs the hooks and the command
;;;; cell.lisp lists.
;;;;
;;;; Rewritten from AnyRouterHQ's hermes-provider-anyrouter (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-anyrouter"
  :description "AnyRouter: its gateway and its tool-capable model catalog, as a Nodecode provider"
  :license "MIT"
  :version "0.1.0"
  :author "AnyRouterHQ (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it, then asks for its key
  :properties ((:serves (:provider "anyrouter" :name "AnyRouter" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-anyrouter/test"))))

(defsystem "nodecode-anyrouter/test"
  :description "AnyRouter tests. Registered into the shared nodecode.test registry; RUN-ANYROUTER-TESTS filters by the ANYROUTER-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-anyrouter" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-anyrouter-tests)))
