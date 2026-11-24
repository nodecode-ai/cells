;;;; nodecode-otel.asd --- OpenTelemetry spans for what a session did.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Optional ASDF cell, not part of the organism core: the folder loader
;;;; loads it at boot when its directory sits under ~/.nodecode/cells/, and the
;;;; gateway calls START-CELL after recovery. See README.md beside this file.

(defsystem "nodecode-otel"
  :description "OpenTelemetry spans for tool calls, model rounds and marks, to your own OTLP backend"
  :license "Apache-2.0"
  :author "briancaffey (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "span")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-otel/test"))))

(defsystem "nodecode-otel/test"
  :description "OTel tests. Registered into the shared nodecode.test registry; RUN-OTEL-TESTS filters by the OTEL-CELL- name prefix."
  :license "Apache-2.0"
  :depends-on ("nodecode-otel" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-otel-tests)))
