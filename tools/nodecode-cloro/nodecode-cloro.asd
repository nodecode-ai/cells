;;;; nodecode-cloro.asd --- cloro's live Google results as one model tool.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `cloro' section, registers the one tool this cell gives the
;;;; model (cloro_search), puts /cloro and the manual topic on, and hands the
;;;; model a primer.
;;;;
;;;; The original is a Hermes Agent plugin: a web-search backend that POSTs
;;;; the query and the country to cloro's Google endpoint and hands the
;;;; harness the organic results. This cell makes the same request through
;;;; the image's own HTTP call -- no Python, nothing fetched at run time, no
;;;; dependency of its own -- and answers the same fields as lines a model
;;;; reads.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting -- or symlinking --
;;;; the directory under ~/.nodecode/cells/.

(defsystem "nodecode-cloro"
  :description "cloro: live Google results as one model tool, with your own key"
  :license "MIT"
  :version "0.1.0"
  :author "cloro-dev (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cloro")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-cloro/test"))))

(defsystem "nodecode-cloro/test"
  :description "cloro tests. Registered into the shared nodecode.test registry; RUN-CLORO-TESTS filters by the CLORO-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "cloro-dev (original), Nodecode (rewrite)"
  :depends-on ("nodecode-cloro" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-cloro-tests)))
