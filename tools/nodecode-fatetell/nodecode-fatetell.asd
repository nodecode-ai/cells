;;;; nodecode-fatetell.asd --- FateTell's BaZi charts and almanac as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `fatetell' section and registers the eight tools, /fatetell and
;;;; (help :fatetell).
;;;;
;;;; A Nodecode rewrite of RussGuo/fatetell-mcp (see NOTICE).

(defsystem "nodecode-fatetell"
  :description "FateTell BaZi charts, the Chinese almanac and saved FateTell charts, as tools"
  :license "MIT"
  :version "0.1.0"
  :author "RussGuo (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-fatetell/test"))))

(defsystem "nodecode-fatetell/test"
  :description "FateTell cell tests. Registered into the shared nodecode.test registry; RUN-FATETELL-TESTS filters by the FATETELL-CELL- name prefix."
  :license "MIT"
  :author "RussGuo (original), Nodecode (rewrite)"
  :depends-on ("nodecode-fatetell" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-fatetell-tests)))
