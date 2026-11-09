;;;; nodecode-tinyfish.asd --- TinyFish web search, fetch and web agent, as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `tinyfish' section, registers the five tools cell.lisp lists,
;;;; puts /tinyfish up and adds (help :tinyfish).
;;;;
;;;; Rewritten from tinyfish-io's Hermes plugin (see NOTICE). Every
;;;; dependency rides with nodecode: QURI comes in with dexador, which
;;;; NLK:HTTP already uses.

(defsystem "nodecode-tinyfish"
  :description "TinyFish web search, page fetch and goal-based web agent, over the TinyFish API"
  :author "tinyfish-io (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-tinyfish/test"))))

(defsystem "nodecode-tinyfish/test"
  :description "TinyFish tests. Registered into the shared nodecode.test registry; RUN-TINYFISH-TESTS filters by the TINYFISH-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-tinyfish" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-tinyfish-tests)))
