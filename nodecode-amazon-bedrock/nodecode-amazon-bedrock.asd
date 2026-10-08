;;;; nodecode-amazon-bedrock.asd --- Amazon Bedrock as a provider, on a lane of its own.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `amazon-bedrock' section, registers the Bedrock lane and
;;;; installs the hooks cell.lisp lists.
;;;;
;;;; Ported from oh-my-pi's amazon-bedrock provider (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-amazon-bedrock"
  :description "Amazon Bedrock: the Converse Stream wire, SigV4 signing and the AWS event stream, on a lane of its own"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "sigv4")
               (:file "eventstream")
               (:file "credentials")
               (:file "wire")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-amazon-bedrock/test"))))

(defsystem "nodecode-amazon-bedrock/test"
  :description "Amazon Bedrock tests. Registered into the shared nodecode.test registry; RUN-AMAZON-BEDROCK-TESTS filters by the AMAZON-BEDROCK-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-amazon-bedrock" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-amazon-bedrock-tests)))
