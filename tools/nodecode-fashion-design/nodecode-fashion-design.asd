;;;; nodecode-fashion-design.asd --- a fashion design toolkit for the model.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode cell, NOT part of the organism core: nothing in Nodecode names
;;;; this system. A rewrite of priticolours/fashion-design's Hermes plugin (see
;;;; NOTICE). The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `fashion-design' section and registers the ten tools, the
;;;; /fashion-design command and the manual on (help :fashion-design).
;;;;
;;;; Every dependency is already in the serving image, and nothing here
;;;; reaches the network: the ten verbs read the tables in data.lisp.

(defsystem "nodecode-fashion-design"
  :description "Draft design briefs, tech packs, size sheets, BOMs, yardage and cost estimates"
  :license "MIT"
  :author "priticolours (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "data")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-fashion-design/test"))))

(defsystem "nodecode-fashion-design/test"
  :description "Fashion-design tests. Registered into the shared nodecode.test registry; RUN-FASHION-DESIGN-TESTS filters by the FASHION-DESIGN-CELL- name prefix."
  :license "MIT"
  :author "priticolours (original), Nodecode (rewrite)"
  :depends-on ("nodecode-fashion-design" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-fashion-design-tests)))
