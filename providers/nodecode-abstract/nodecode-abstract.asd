;;;; nodecode-abstract.asd --- Abstract, GLM/DeepSeek/Kimi over the Responses API, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `abstract' section and installs the hooks cell.lisp lists.
;;;;
;;;; Rewritten from abstractpbc's hermes-abstract-provider (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-abstract"
  :description "Abstract: GLM-5.3, DeepSeek-V4-Flash and Kimi-K3 over the Responses API"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it, then asks for its key
  :properties ((:serves (:provider "abstract" :name "Abstract" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-abstract/test"))))

(defsystem "nodecode-abstract/test"
  :description "Abstract tests. Registered into the shared nodecode.test registry; RUN-ABSTRACT-TESTS filters by the ABSTRACT-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-abstract" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-abstract-tests)))
