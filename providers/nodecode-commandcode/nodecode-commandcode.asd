;;;; nodecode-commandcode.asd --- Command Code, over its CLI's /alpha/generate wire, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `commandcode' section, registers the commandcode lane and
;;;; installs the hooks cell.lisp lists.
;;;;
;;;; Rewritten from NanamiMio/hermes-commandcode-oauth (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-commandcode"
  :description "Command Code: its CLI's /alpha/generate wire as a Nodecode provider lane"
  :license "MIT"
  :author "NanamiMio (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it, then asks for its key
  :properties ((:serves (:provider "commandcode" :name "Command Code" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "wire")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-commandcode/test"))))

(defsystem "nodecode-commandcode/test"
  :description "Command Code tests. Registered into the shared nodecode.test registry; RUN-COMMANDCODE-TESTS filters by the COMMANDCODE-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-commandcode" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-commandcode-tests)))
