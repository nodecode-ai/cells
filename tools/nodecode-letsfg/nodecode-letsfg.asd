;;;; nodecode-letsfg.asd --- LetsFG flights and hotels, as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A peripheral cell, NOT part of the organism core: nothing in Nodecode
;;;; names this system. The folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/; the gateway calls START-CELL after
;;;; recovery, which reads the `letsfg' section, registers the five
;;;; model-facing tools cell.lisp declares, puts (help :letsfg) on the manual
;;;; with the travel-booking rules the original plugin shipped as a skill, and
;;;; registers /letsfg. Every byte it sends goes to the hosted LetsFG MCP
;;;; endpoint through NLK:HTTP.

(defsystem "nodecode-letsfg"
  :description "LetsFG flights and hotels: search and book real flights and hotel rooms through the hosted LetsFG service"
  :license "MIT"
  :version "0.1.0"
  :author "LetsFG (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-letsfg/test"))))

(defsystem "nodecode-letsfg/test"
  :description "LetsFG tests. Registered into the shared nodecode.test registry; RUN-LETSFG-TESTS filters by the LETSFG-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-letsfg" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-letsfg-tests)))
