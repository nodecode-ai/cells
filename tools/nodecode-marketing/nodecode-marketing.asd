;;;; nodecode-marketing.asd --- Digital Marketing Pro's 207 skills, on demand.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `marketing' section, registers the two tools and /marketing, and
;;;; puts the primer on (help :marketing).
;;;;
;;;; Rewritten from indranilbanerjee's Hermes Agent plugin of the same name;
;;;; see NOTICE. skills/ is that plugin's three skills/ folders, whose prose
;;;; this cell serves; every dependency rides with nodecode.

(defsystem "nodecode-marketing"
  :description "Digital Marketing Pro: 207 marketing skills, searched and read on demand"
  :author "indranilbanerjee (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-marketing/test"))))

(defsystem "nodecode-marketing/test"
  :description "Marketing tests. Registered into the shared nodecode.test registry; RUN-MARKETING-TESTS filters by the MARKETING-CELL- name prefix."
  :author "indranilbanerjee (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-marketing" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-marketing-tests)))
