;;;; nodecode-anyapi.asd --- AnyAPI.ai, one key for many vendors' models, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `anyapi' section and installs the hooks cell.lisp lists.
;;;;
;;;; A rewrite of es697/hermes-provider-anyapi (see NOTICE). Every dependency
;;;; rides with nodecode.

(defsystem "nodecode-anyapi"
  :description "AnyAPI.ai: one key for many vendors' models, listed by the routes that take tool calls"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it, then asks for its key
  :properties ((:serves (:provider "anyapi" :name "AnyAPI.ai" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-anyapi/test"))))

(defsystem "nodecode-anyapi/test"
  :description "AnyAPI tests. Registered into the shared nodecode.test registry; RUN-ANYAPI-TESTS filters by the ANYAPI-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-anyapi" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-anyapi-tests)))
