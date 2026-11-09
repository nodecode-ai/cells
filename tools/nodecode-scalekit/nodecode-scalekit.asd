;;;; nodecode-scalekit.asd --- Scalekit connectors: any app's API, as four tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `scalekit' section and registers the four tools cell.lisp lists,
;;;; and puts the plugin's rules on the manual as (help :scalekit).
;;;;
;;;; A rewrite of scalekit-inc's hermes-tools (see NOTICE): the original was
;;;; a Hermes plugin of four tools over Scalekit's REST API, and this cell
;;;; calls the same endpoints with nlk:http.

(defsystem "nodecode-scalekit"
  :description "Scalekit connectors: approve a connection, call a connector's tools, and proxy its API, as four tools the model calls"
  :author "scalekit-inc (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-scalekit/test"))))

(defsystem "nodecode-scalekit/test"
  :description "Scalekit cell tests. Registered into the shared nodecode.test registry; RUN-SCALEKIT-TESTS filters by the SCALEKIT-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-scalekit" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-scalekit-tests)))
