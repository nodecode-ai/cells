;;;; nodecode-aixy.asd --- Aixy, an inference gateway, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `aixy' section and installs the hooks cell.lisp lists.
;;;;
;;;; Rewritten from aixy-tech's Hermes setup plugin (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-aixy"
  :description "Aixy: an inference gateway as a Nodecode provider, with its live model catalog"
  :license "MIT"
  :version "0.1.0"
  :author "aixy-tech (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it, then asks for its key
  :properties ((:serves (:provider "aixy" :name "Aixy" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-aixy/test"))))

(defsystem "nodecode-aixy/test"
  :description "Aixy tests. Registered into the shared nodecode.test registry; RUN-AIXY-TESTS filters by the AIXY-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-aixy" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-aixy-tests)))
