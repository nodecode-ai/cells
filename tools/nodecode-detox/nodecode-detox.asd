;;;; nodecode-detox.asd --- the tool-output firewall, as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF system, NOT part of the organism core: nothing in src/
;;;; names it. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `detox' section, hooks the :TOOL point, registers the
;;;; three retrieval tools and /detox, and puts the manual on (help :detox).
;;;;
;;;; Every dependency is already in the serving image.

(defsystem "nodecode-detox"
  :description "Compact reports for huge tool outputs, the full text kept under the home, and exact lines back on demand."
  :license "MIT"
  :author "kellycryptos (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "store")
               (:file "redact")
               (:file "report")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-detox/test"))))

(defsystem "nodecode-detox/test"
  :description "Detox tests: the interception, the report, retrieval, the refusals."
  :license "MIT"
  :depends-on ("nodecode-detox" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-detox-tests)))
