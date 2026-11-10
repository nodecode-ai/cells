;;;; nodecode-llmapi.asd --- LLM API as a Nodecode provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `llmapi' section and installs what cell.lisp declares.
;;;;
;;;; A rewrite of llmapi-ai's hermes-provider-llmapi (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-llmapi"
  :description "LLM API: Claude, GPT, Gemini, Kimi and more on one key, each on the wire it works best on"
  :license "MIT"
  :author "llmapi-ai (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it
  ;; installs it, then asks for its key
  :properties ((:serves (:provider "llmapi" :name "LLM API" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-llmapi/test"))))

(defsystem "nodecode-llmapi/test"
  :description "LLM API tests. Registered into the shared nodecode.test registry; RUN-LLMAPI-TESTS filters by the LLMAPI-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-llmapi" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-llmapi-tests)))
