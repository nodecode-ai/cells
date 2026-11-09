;;;; nodecode-stacktree.asd --- Stacktree pages as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `stacktree' section, registers the six stacktree_* tools, /stacktree
;;;; and the (help :stacktree) topic.
;;;;
;;;; A Nodecode rewrite of stevysmith/stacktree-skill (see NOTICE).

(defsystem "nodecode-stacktree"
  :description "Stacktree: publish agent-built HTML to a private link that updates in place"
  :license "MIT"
  :version "0.1.0"
  :author "stevysmith (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-stacktree/test"))))

(defsystem "nodecode-stacktree/test"
  :description "Stacktree tests. Registered into the shared nodecode.test registry; RUN-STACKTREE-TESTS filters by the STACKTREE-CELL- name prefix."
  :license "MIT"
  :author "stevysmith (original), Nodecode (rewrite)"
  :depends-on ("nodecode-stacktree" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-stacktree-tests)))
