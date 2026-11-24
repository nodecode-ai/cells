;;;; nodecode-lowrouter.asd --- LowRouter, an EU-hosted model router, as a provider.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `lowrouter' section and installs the hooks cell.lisp declares.
;;;;
;;;; A rewrite of the LowRouter provider plugin for Hermes (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-lowrouter"
  :description "LowRouter: its EU-hosted, OpenAI-compatible routes as a provider, filtered to the ones a turn can run on"
  :license "Apache-2.0"
  :author "carboniferio (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it, then asks for its key
  :properties ((:serves (:provider "lowrouter" :name "LowRouter" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-lowrouter/test"))))

(defsystem "nodecode-lowrouter/test"
  :description "LowRouter tests. Registered into the shared nodecode.test registry; RUN-LOWROUTER-TESTS filters by the LOWROUTER-CELL- name prefix."
  :license "Apache-2.0"
  :author "carboniferio (original), Nodecode (rewrite)"
  :depends-on ("nodecode-lowrouter" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-lowrouter-tests)))
