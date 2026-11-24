;;;; nodecode-mpp.asd --- payment-aware HTTP fetching as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `mpp' section, registers mpp_fetch and /mpp, and puts
;;;; (help :mpp) on the manual.
;;;;
;;;; A Nodecode rewrite of tempoxyz/hermes-mpp (see NOTICE).

(defsystem "nodecode-mpp"
  :description "Payment-aware HTTP: answer an MPP 402 challenge and retry the request"
  :license "MIT"
  :version "0.1.0"
  :author "tempoxyz (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-mpp/test"))))

(defsystem "nodecode-mpp/test"
  :description "MPP fetch tests. Registered into the shared nodecode.test registry; RUN-MPP-TESTS filters by the MPP-CELL- name prefix."
  :license "MIT"
  :author "tempoxyz (original), Nodecode (rewrite)"
  :depends-on ("nodecode-mpp" "nodecode/test")
  :serial t
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-mpp-tests)))
