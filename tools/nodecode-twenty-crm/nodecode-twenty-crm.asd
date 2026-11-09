;;;; nodecode-twenty-crm.asd --- Twenty CRM's REST, GraphQL and schema tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `twenty-crm' section, registers the four tools and puts the
;;;; primer on (help :twenty-crm).
;;;;
;;;; Rewritten from TREE-Ind's Hermes Agent plugin of the same name; see
;;;; NOTICE. Every dependency rides with nodecode: nlk:http speaks to the
;;;; instance, nlk:decode-json reads its answers.

(defsystem "nodecode-twenty-crm"
  :description "Twenty CRM: describe the workspace, call REST and GraphQL, introspect the schema"
  :author "TREE-Ind (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-twenty-crm/test"))))

(defsystem "nodecode-twenty-crm/test"
  :description "Twenty CRM tests. Registered into the shared nodecode.test registry; RUN-TWENTY-CRM-TESTS filters by the TWENTY-CRM-CELL- name prefix."
  :author "TREE-Ind (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-twenty-crm" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-twenty-crm-tests)))
