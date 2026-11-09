;;;; nodecode-empiriolabs.asd --- EmpirioLabs AI as a provider: its live chat catalog.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `empiriolabs' section and installs the hooks cell.lisp lists.
;;;;
;;;; A rewrite of EmpirioLabs-AI's Hermes plugin (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-empiriolabs"
  :description "EmpirioLabs AI: its live chat catalog as a Nodecode provider, tool calling and reasoning declared"
  :license "MIT"
  :author "EmpirioLabs-AI (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it, then asks for its key
  :properties ((:serves (:provider "empiriolabs" :name "EmpirioLabs AI" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-empiriolabs/test"))))

(defsystem "nodecode-empiriolabs/test"
  :description "EmpirioLabs AI tests. Registered into the shared nodecode.test registry; RUN-EMPIRIOLABS-TESTS filters by the EMPIRIOLABS-CELL- name prefix."
  :license "MIT"
  :author "EmpirioLabs-AI (original), Nodecode (rewrite)"
  :depends-on ("nodecode-empiriolabs" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-empiriolabs-tests)))
