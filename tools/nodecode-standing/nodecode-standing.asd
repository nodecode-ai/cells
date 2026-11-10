;;;; nodecode-standing.asd --- earned trust: levels, proofs, and a local record.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional cell, NOT part of the organism core: nothing in src/ names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `standing' section, keeps the record in the nodecode home,
;;;; hooks the :TOOL point, and registers standing_proof, /standing and
;;;; (help :standing).
;;;;
;;;; A Nodecode rewrite of percyhendrux123-prog's hermes-standing; see
;;;; NOTICE. The .asd sits INSIDE its folder: presence is enabled.

(defsystem "nodecode-standing"
  :description "Earned trust: levels gate a call, and a proof it runs itself raises them"
  :license "MIT"
  :author "percyhendrux123-prog (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-standing/test"))))

(defsystem "nodecode-standing/test"
  :description "Standing tests. Registered into the shared nodecode.test registry; RUN-STANDING-TESTS filters by the STANDING- name prefix."
  :license "MIT"
  :author "percyhendrux123-prog (original), Nodecode (rewrite)"
  :depends-on ("nodecode-standing" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-standing-tests)))
