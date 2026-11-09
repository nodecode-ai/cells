;;;; nodecode-hopper.asd --- Hopper: your own curated OpenRouter list, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `hopper' section and installs the hooks and the command
;;;; cell.lisp lists.
;;;;
;;;; A rewrite of mykeura's Hopper plugin for Hermes (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-hopper"
  :description "Hopper: a curated list of OpenRouter models, as a Nodecode provider"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it
  ;; installs it, then asks for its key
  :properties ((:serves (:provider "hopper" :name "Hopper" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-hopper/test"))))

(defsystem "nodecode-hopper/test"
  :description "Hopper tests. Registered into the shared nodecode.test registry; RUN-HOPPER-TESTS filters by the HOPPER-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-hopper" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-hopper-tests)))
