;;;; nodecode-watchline.asd --- matched events from a Watchline pull channel.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core. The folder loader loads it at boot
;;;; when its directory sits under ~/.nodecode/cells/; the gateway calls
;;;; START-CELL after recovery, which reads the `watchline' section and starts
;;;; a worker that polls the Watchline pull channel, puts each matched event
;;;; into its session as a prompt and acknowledges it once the turn is
;;;; admitted. Also registers the watchline_pending tool, /watchline and the
;;;; :watchline manual topic. A rewrite of qordinate-ai's Hermes plugin of the
;;;; same name; see NOTICE. Every dependency rides with nodecode: NLK:HTTP
;;;; speaks to the Watchline API.

(defsystem "nodecode-watchline"
  :description "Watchline: matched events from a pull channel, delivered into a session"
  :author "qordinate-ai (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-watchline/test"))))

(defsystem "nodecode-watchline/test"
  :description "Watchline tests. Registered into the shared nodecode.test registry; RUN-WATCHLINE-TESTS filters by the WATCHLINE- name prefix."
  :author "qordinate-ai (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-watchline" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-watchline-tests)))
