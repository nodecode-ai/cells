;;;; nodecode-pep.asd --- Python Enhancement Proposals, as a tool and a command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of hermes-context-refs' pep-ref (see NOTICE). A cell, NOT part
;;;; of the organism core: the folder loader loads it at boot when its
;;;; directory sits under ~/.nodecode/cells/, and the gateway calls START-CELL
;;;; after recovery, which reads the `pep' section, puts (help :pep) on the
;;;; manual and registers the pep_reference tool and /pep.
;;;;
;;;; Every dependency rides with nodecode: the network is nlk:http, the JSON
;;;; reader nlk:decode-json, the HTML reader is this folder's own.

(defsystem "nodecode-pep"
  :description "PEP references: a Python Enhancement Proposal's status and one section, from peps.python.org"
  :license "MIT"
  :author "EloquentBrush0x (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "html")
               (:file "pep")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-pep/test"))))

(defsystem "nodecode-pep/test"
  :description "PEP reference tests. Registered into the shared nodecode.test registry; RUN-PEP-TESTS filters by the PEP-CELL- name prefix."
  :license "MIT"
  :author "EloquentBrush0x (original), Nodecode (rewrite)"
  :depends-on ("nodecode-pep" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-pep-tests)))
