;;;; nodecode-brave-search.asd --- Brave Search as one model tool.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `brave-search' section, registers the one tool the original
;;;; plugin gave its harness (brave_web_search), puts /brave and the manual
;;;; topic on, and hands the model a primer.
;;;;
;;;; The original is a Hermes Agent plugin: a Python tool that GETs Brave's
;;;; web-search endpoint and hands back a JSON envelope. This cell makes the
;;;; same request through the image's own HTTP call -- no Python, nothing
;;;; fetched at run time, no dependency of its own -- and answers the same
;;;; fields as lines a model reads.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting -- or symlinking --
;;;; the directory under ~/.nodecode/cells/.

(defsystem "nodecode-brave-search"
  :description "Brave Search: live web results as one model tool, with your own key"
  :license "MIT"
  :version "0.1.0"
  :author "rabilrbl (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "search")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-brave-search/test"))))

(defsystem "nodecode-brave-search/test"
  :description "Brave Search tests. Registered into the shared nodecode.test registry; RUN-BRAVE-SEARCH-TESTS filters by the BRAVE-SEARCH-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "rabilrbl (original), Nodecode (rewrite)"
  :depends-on ("nodecode-brave-search" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-brave-search-tests)))
