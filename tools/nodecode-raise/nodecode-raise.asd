;;;; nodecode-raise.asd --- a draft-only fundraising desk: rank on cited evidence, gate every draft.
;;;;
;;;; SPDX-License-Identifier: MIT

(defsystem "nodecode-raise"
  :description "Draft-only fundraising desk: ranks investors on cited evidence and gates every draft"
  :license "MIT"
  :author "GenesisEngine-Triolith (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode" "cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "store")
               (:file "rules")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-raise/test"))))

(defsystem "nodecode-raise/test"
  :description "Raise tests: the pipeline file, the ranking, the gates and the lint, over a scratch home."
  :license "MIT"
  :depends-on ("nodecode-raise" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
             (uiop:symbol-call '#:nodecode.test '#:run-raise-tests)))
