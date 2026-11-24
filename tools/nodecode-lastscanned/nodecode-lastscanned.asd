;;;; nodecode-lastscanned.asd --- parcel and container tracking as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `lastscanned' section, registers track_package, track_packages,
;;;; get_rates and list_carriers, adds /lastscanned and puts
;;;; (help :lastscanned) on the manual.
;;;;
;;;; A Nodecode rewrite of dragonhuntr/lastscanned-plugin (see NOTICE).

(defsystem "nodecode-lastscanned"
  :description "LastScanned: parcel and container tracking across 549 carriers, and USPS, UPS and FedEx rate quotes"
  :license "MIT"
  :version "0.1.0"
  :author "dragonhuntr (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-lastscanned/test"))))

(defsystem "nodecode-lastscanned/test"
  :description "The LastScanned cell's tests. Registered into the shared nodecode.test registry; RUN-LASTSCANNED-TESTS filters by the LASTSCANNED-CELL- name prefix."
  :license "MIT"
  :author "dragonhuntr (original), Nodecode (rewrite)"
  :depends-on ("nodecode-lastscanned" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-lastscanned-tests)))
