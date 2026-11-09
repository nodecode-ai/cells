;;;; nodecode-impossibl.asd --- Impossibl, one gateway for many labs, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `impossibl' section and installs the hooks cell.lisp lists.
;;;;
;;;; A rewrite of impossiblco/hermes-impossibl (see NOTICE). Every dependency
;;;; rides with nodecode.

(defsystem "nodecode-impossibl"
  :description "Impossibl: one OpenAI-compatible key for models from many labs, listed by chat and tool support"
  :license "MIT"
  :author "impossiblco (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it, then asks for its key
  :properties ((:serves (:provider "impossibl" :name "Impossibl" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-impossibl/test"))))

(defsystem "nodecode-impossibl/test"
  :description "Impossibl tests. Registered into the shared nodecode.test registry; RUN-IMPOSSIBL-TESTS filters by the IMPOSSIBL-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-impossibl" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-impossibl-tests)))
