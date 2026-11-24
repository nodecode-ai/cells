;;;; nodecode-aurora.asd --- the Aurora design system as an offline tool and a checker.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `aurora' section, registers the two tools the original plugin
;;;; gave its harness (aurora_search, aurora_validate), puts /aurora and the
;;;; manual topic on, and hands the model the doctrine its four skills carried.
;;;;
;;;; The original is a Hermes Agent plugin: two Python tools over a bundled
;;;; census snapshot, plus four skills. This cell reads the same snapshot
;;;; (data/censo.json, unchanged, shipped beside it) with the image's own
;;;; JSON reader and scores a query with the original's own scale -- no
;;;; Python, no network, no subprocess, nothing fetched at run time.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting -- or symlinking --
;;;; the directory under ~/.nodecode/cells/.

(defsystem "nodecode-aurora"
  :description "Aurora design system: page markup looked up offline, and a page checked against the census"
  :license "MIT"
  :author "Ntizar (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "census")
               (:file "check")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-aurora/test"))))

(defsystem "nodecode-aurora/test"
  :description "Aurora tests. Registered into the shared nodecode.test registry; RUN-AURORA-TESTS filters by the AURORA-CELL- name prefix."
  :license "MIT"
  :author "Ntizar (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode-aurora" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-aurora-tests)))
