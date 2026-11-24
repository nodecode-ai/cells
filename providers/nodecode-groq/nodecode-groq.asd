;;;; nodecode-groq.asd --- Groq as a Nodecode provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `groq' section and installs the hooks cell.lisp lists.
;;;;
;;;; A rewrite of AhmetArif0's hermes-groq-provider (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-groq"
  :description "Groq: fast inference for open models, each model sent only the reasoning effort it accepts"
  :license "MIT"
  :author "AhmetArif0 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it
  ;; installs it, then asks for its key
  :properties ((:serves (:provider "groq" :name "Groq" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-groq/test"))))

(defsystem "nodecode-groq/test"
  :description "Groq tests. Registered into the shared nodecode.test registry; RUN-GROQ-TESTS filters by the GROQ-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-groq" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-groq-tests)))
