;;;; nodecode-grounder.asd --- Grounder's licensed web search as one model tool.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `grounder' section, registers the one tool the original gave its
;;;; harness (grounder_search), puts /grounder and the manual topic on, and
;;;; hands the model a primer.
;;;;
;;;; The original is a Hermes Agent plugin: a Python web-search backend that
;;;; POSTs each search to Grounder and answers rows of title, url and
;;;; description. This cell makes the same request through the image's own HTTP
;;;; call -- no Python, nothing fetched at run time, no dependency of its own --
;;;; and answers the same fields as lines a model reads.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting -- or symlinking --
;;;; the directory under ~/.nodecode/cells/.

(defsystem "nodecode-grounder"
  :description "Grounder: licensed web search across many providers, deduplicated, as one model tool"
  :license "Apache-2.0"
  :version "0.1.0"
  :author "grounder-labs (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "search")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-grounder/test"))))

(defsystem "nodecode-grounder/test"
  :description "Grounder tests. Registered into the shared nodecode.test registry; RUN-GROUNDER-TESTS filters by the GROUNDER-CELL- name prefix."
  :license "Apache-2.0"
  :version "0.1.0"
  :author "grounder-labs (original), Nodecode (rewrite)"
  :depends-on ("nodecode-grounder" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-grounder-tests)))
