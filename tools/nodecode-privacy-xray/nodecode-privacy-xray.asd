;;;; nodecode-privacy-xray.asd --- where a session's data actually goes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `privacy-xray' section, hooks the request and credential
;;;; points, registers the privacy_xray tool and /xray, and puts
;;;; (help :privacy-xray) on the manual.

(defsystem "nodecode-privacy-xray"
  :description "Where your data goes: a local ledger of every model request, a config audit, canaries and a leak map"
  :license "MIT"
  :author "MadeByBrain (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-privacy-xray/test"))))

(defsystem "nodecode-privacy-xray/test"
  :description "Privacy x-ray tests. Registered into the shared nodecode.test registry; RUN-PRIVACY-XRAY-TESTS filters by the PRIVACY-XRAY-CELL- name prefix."
  :license "MIT"
  :author "MadeByBrain (original), Nodecode (rewrite)"
  :depends-on ("nodecode-privacy-xray" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-privacy-xray-tests)))
