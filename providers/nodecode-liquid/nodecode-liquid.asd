;;;; nodecode-liquid.asd --- Liquid Inference, an auction exchange for LLM
;;;; inference, as a Nodecode provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `liquid' section and installs the hooks cell.lisp lists.
;;;;
;;;; A Nodecode rewrite of architect-xyz's hermes-liquid-inference (see
;;;; NOTICE). Every dependency rides with nodecode.

(defsystem "nodecode-liquid"
  :description "Liquid Inference: the auction exchange for LLM inference as a Nodecode provider"
  :license "MIT"
  :author "architect-xyz (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it, then asks for its key
  :properties ((:serves (:provider "liquid" :name "Liquid Inference" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-liquid/test"))))

(defsystem "nodecode-liquid/test"
  :description "Liquid Inference tests. Registered into the shared nodecode.test registry; RUN-LIQUID-TESTS filters by the LIQUID-CELL- name prefix."
  :license "MIT"
  :author "architect-xyz (original), Nodecode (rewrite)"
  :depends-on ("nodecode-liquid" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-liquid-tests)))
