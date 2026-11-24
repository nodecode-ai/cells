;;;; nodecode-scaleway.asd --- Scaleway Generative APIs as a Nodecode provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `scaleway' section and installs the hooks cell.lisp lists.
;;;;
;;;; Rewritten from the Hermes plugin scaleway-provider (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-scaleway"
  :description "Scaleway Generative APIs: its European OpenAI-compatible models as a Nodecode provider"
  :license "MIT"
  :author "scaleway (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it, then asks for its key
  :properties ((:serves (:provider "scaleway" :name "Scaleway" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-scaleway/test"))))

(defsystem "nodecode-scaleway/test"
  :description "Scaleway tests. Registered into the shared nodecode.test registry; RUN-SCALEWAY-TESTS filters by the SCALEWAY-CELL- name prefix."
  :license "MIT"
  :author "scaleway (original), Nodecode (rewrite)"
  :depends-on ("nodecode-scaleway" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-scaleway-tests)))
