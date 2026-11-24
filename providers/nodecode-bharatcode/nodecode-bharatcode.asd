;;;; nodecode-bharatcode.asd --- BharatCode, its OpenAI-compatible API, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `bharatcode' section and installs the hooks cell.lisp lists.
;;;;
;;;; A rewrite of thenabbu's bharatcode-hermes (see NOTICE). Every dependency
;;;; rides with nodecode.

(defsystem "nodecode-bharatcode"
  :description "BharatCode: an OpenAI-compatible model provider with a free student tier"
  :license "MIT"
  :author "thenabbu (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it
  ;; installs it, then asks for its key
  :properties ((:serves (:provider "bharatcode" :name "BharatCode" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-bharatcode/test"))))

(defsystem "nodecode-bharatcode/test"
  :description "BharatCode tests. Registered into the shared nodecode.test registry; RUN-BHARATCODE-TESTS filters by the BHARATCODE-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-bharatcode" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-bharatcode-tests)))
