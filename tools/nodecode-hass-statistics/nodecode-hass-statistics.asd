;;;; nodecode-hass-statistics.asd --- Home Assistant Recorder statistics as two tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `hass-statistics' section, registers the two tools and puts the
;;;; primer on (help :hass-statistics).
;;;;
;;;; Rewritten from goncami's Hermes Agent plugin of the same name; see
;;;; NOTICE. The instance's statistics live on its WebSocket API, so the cell
;;;; dials it with websocket-driver -- the RFC-6455 client the runtime
;;;; already carries (as nodecode-gemini-live does) -- and nlk:http is not
;;;; used at all.

(defsystem "nodecode-hass-statistics"
  :description "Home Assistant statistics: list statistic ids, fetch their history"
  :author "goncami (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-hass-statistics/test"))))

(defsystem "nodecode-hass-statistics/test"
  :description "Home Assistant statistics tests. Registered into the shared nodecode.test registry; RUN-HASS-STATISTICS-TESTS filters by the HASS-STATISTICS-CELL- name prefix."
  :author "goncami (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-hass-statistics" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-hass-statistics-tests)))
