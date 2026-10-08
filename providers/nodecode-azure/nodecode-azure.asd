;;;; nodecode-azure.asd --- Azure OpenAI as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `azure' section and installs the hooks cell.lisp
;;;; lists.
;;;;
;;;; Ported from oh-my-pi's azure provider (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-azure"
  :description "Azure OpenAI: the Responses API on an Azure resource, with its api-version and api-key"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-azure/test"))))

(defsystem "nodecode-azure/test"
  :description "Azure OpenAI tests. Registered into the shared nodecode.test registry; RUN-AZURE-TESTS filters by the AZURE-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-azure" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-azure-tests)))
