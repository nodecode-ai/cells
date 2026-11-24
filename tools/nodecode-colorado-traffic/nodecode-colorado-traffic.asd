;;;; nodecode-colorado-traffic.asd --- live Colorado highway events from CDOT's COtrip feed.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of drkpxl/colorado-traffic, a Hermes Agent skill. Optional
;;;; ASDF cell, not part of the organism core: the folder loader loads it at
;;;; boot when its directory sits under ~/.nodecode/cells/, and the gateway
;;;; calls START-CELL after recovery, which reads the `colorado-traffic'
;;;; section, registers the cotrip_events tool and /traffic, and puts the
;;;; manual on (help :colorado-traffic). No key, no dependency outside the
;;;; serving image.

(defsystem "nodecode-colorado-traffic"
  :description "Live Colorado highway events from CDOT COtrip: now, upcoming and Waze, by corridor"
  :license "MIT"
  :author "drkpxl (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cotrip")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-colorado-traffic/test"))))

(defsystem "nodecode-colorado-traffic/test"
  :description "COtrip tests. Registered into the shared nodecode.test registry; RUN-COLORADO-TRAFFIC-TESTS filters by the COLORADO-TRAFFIC-CELL- name prefix."
  :license "MIT"
  :author "drkpxl (original), Nodecode (rewrite)"
  :depends-on ("nodecode-colorado-traffic" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-colorado-traffic-tests)))
