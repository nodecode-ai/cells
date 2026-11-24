;;;; nodecode-flipletters.asd --- FlipLetters split-flap board videos, as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `flipletters' section, registers the fourteen tools and
;;;; /flipletters, and puts (help :flipletters) on the manual.
;;;;
;;;; A Nodecode rewrite of Terahertz-Inc/flipletters-mcp (see NOTICE).

(defsystem "nodecode-flipletters"
  :description "FlipLetters split-flap and flip-dot board videos: validate, render and share flipcode boards"
  :license "MIT"
  :version "0.1.0"
  :author "Terahertz-Inc (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-flipletters/test"))))

(defsystem "nodecode-flipletters/test"
  :description "FlipLetters cell tests. Registered into the shared nodecode.test registry; RUN-FLIPLETTERS-TESTS filters by the FLIPLETTERS-CELL- name prefix."
  :license "MIT"
  :author "Terahertz-Inc (original), Nodecode (rewrite)"
  :depends-on ("nodecode-flipletters" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-flipletters-tests)))
