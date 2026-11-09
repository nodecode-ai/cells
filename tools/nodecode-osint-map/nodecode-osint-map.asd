;;;; nodecode-osint-map.asd --- public situational feeds and passive recon, as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `osint-map' section and registers the five osint_* tools and
;;;; /osint.
;;;;
;;;; A Nodecode rewrite of Adolanium's hermes-osint-map (see NOTICE).

(defsystem "nodecode-osint-map"
  :description "OSINT Map: public situational feeds and passive recon as five tools, with no key"
  :license "MIT"
  :version "0.1.0"
  :author "Adolanium (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "sources")
               (:file "panels")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-osint-map/test"))))

(defsystem "nodecode-osint-map/test"
  :description "OSINT Map tests. Registered into the shared nodecode.test registry; RUN-OSINT-MAP-TESTS filters by the OSINT-MAP-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "Adolanium (original), Nodecode (rewrite)"
  :depends-on ("nodecode-osint-map" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-osint-map-tests)))
