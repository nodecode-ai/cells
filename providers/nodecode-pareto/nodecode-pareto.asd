;;;; nodecode-pareto.asd --- Pareto Inference as a Nodecode provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `pareto' section and installs the hooks cell.lisp lists.
;;;;
;;;; A rewrite of pareto-inference's hermes-pareto-provider (see NOTICE).
;;;; Every dependency rides with nodecode.

(defsystem "nodecode-pareto"
  :description "Pareto Inference: GLM, DeepSeek and MiMo Flash, pay per token, on the chat wire"
  :license "MIT"
  :author "pareto-inference (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it
  ;; installs it, then asks for its key
  :properties ((:serves (:provider "pareto" :name "Pareto Inference" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-pareto/test"))))

(defsystem "nodecode-pareto/test"
  :description "Pareto Inference tests. Registered into the shared nodecode.test registry; RUN-PARETO-TESTS filters by the PARETO-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-pareto" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-pareto-tests)))
