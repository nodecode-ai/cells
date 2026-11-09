;;;; nodecode-health-data.asd --- wearable health data as three tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `healthdata' section, registers the three tools and puts the
;;;; primer on (help :health-data).
;;;;
;;;; Rewritten from Infinite-Labs-AI's Hermes Agent plugin of the same name;
;;;; see NOTICE. Every dependency rides with nodecode: nlk:http speaks to
;;;; Oura's API, and the store is JSON lines under the organism's home.

(defsystem "nodecode-health-data"
  :description "Wearable health data: sync Oura, answer sleep and readiness questions"
  :author "Infinite-Labs-AI (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-health-data/test"))))

(defsystem "nodecode-health-data/test"
  :description "Health data tests. Registered into the shared nodecode.test registry; RUN-HEALTH-DATA-TESTS filters by the HEALTH-DATA-CELL- name prefix."
  :author "Infinite-Labs-AI (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-health-data" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-health-data-tests)))
