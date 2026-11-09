;;;; nodecode-requesty.asd --- Requesty, its LLM gateway and managed routing policies, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `requesty' section and installs the hooks and the command
;;;; cell.lisp lists.
;;;;
;;;; Rewritten from requestyai's hermes-provider-requesty (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-requesty"
  :description "Requesty: an LLM gateway with managed routing policies, as a Nodecode provider"
  :license "MIT"
  :version "0.1.0"
  :author "requestyai (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it, then asks for its key
  :properties ((:serves (:provider "requesty" :name "Requesty" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-requesty/test"))))

(defsystem "nodecode-requesty/test"
  :description "Requesty tests. Registered into the shared nodecode.test registry; RUN-REQUESTY-TESTS filters by the REQUESTY-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-requesty" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-requesty-tests)))
