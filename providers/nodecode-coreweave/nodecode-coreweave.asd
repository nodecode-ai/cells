;;;; nodecode-coreweave.asd --- CoreWeave Inference, its endpoint and its models, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `coreweave' section and installs the hooks and the command
;;;; cell.lisp lists.
;;;;
;;;; Rewritten from coreweave's serverless-inference-for-hermes (see NOTICE).
;;;; Every dependency rides with nodecode.

(defsystem "nodecode-coreweave"
  :description "CoreWeave Inference: its endpoint and its live model catalog, as a Nodecode provider"
  :license "MIT"
  :version "0.1.0"
  :author "coreweave (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it, then asks for its key
  :properties ((:serves (:provider "coreweave" :name "CoreWeave Inference" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-coreweave/test"))))

(defsystem "nodecode-coreweave/test"
  :description "CoreWeave Inference tests. Registered into the shared nodecode.test registry; RUN-COREWEAVE-TESTS filters by the COREWEAVE-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-coreweave" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-coreweave-tests)))
