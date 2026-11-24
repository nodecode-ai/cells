;;;; nodecode-netmind.asd --- NetMind, an OpenAI-compatible inference service, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `netmind' section and installs the hooks cell.lisp lists.
;;;;
;;;; Rewritten from NetMind-AI's Hermes provider plugin (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-netmind"
  :description "NetMind: an OpenAI-compatible inference service and its bundled roster, as a Nodecode provider"
  :license "MIT"
  :version "0.1.0"
  :author "NetMind-AI (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it, then asks for its key
  :properties ((:serves (:provider "netmind" :name "NetMind" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-netmind/test"))))

(defsystem "nodecode-netmind/test"
  :description "NetMind tests. Registered into the shared nodecode.test registry; RUN-NETMIND-TESTS filters by the NETMIND-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-netmind" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-netmind-tests)))
