;;;; nodecode-glorya.asd --- Glorya's hosted ad and page studio, as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `glorya' section, registers the seven tools and /glorya, puts
;;;; the primer on (help :glorya), and reminds the model of the jobs this chat
;;;; has open.
;;;;
;;;; Rewritten from Glorya-AI's Hermes Agent plugin of the same name; see
;;;; NOTICE. Every dependency rides with nodecode: NLK:HTTP speaks to the
;;;; Glorya MCP server the operator names and nothing else.

(defsystem "nodecode-glorya"
  :description "Glorya: image ads, video ads, landing pages and websites, as tools and /glorya"
  :author "Glorya-AI (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "wire")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-glorya/test"))))

(defsystem "nodecode-glorya/test"
  :description "Glorya tests, in the shared nodecode.test registry; RUN-GLORIA-TESTS filters by the GLORIA-CELL- name prefix."
  :author "Glorya-AI (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-glorya" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-glorya-tests)))
