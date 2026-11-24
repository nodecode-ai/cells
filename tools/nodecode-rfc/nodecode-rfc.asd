;;;; nodecode-rfc.asd --- an IETF RFC's record from the RFC Editor, as a tool.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core. The folder loader loads it when
;;;; its directory sits under ~/.nodecode/cells/; the gateway calls START-CELL
;;;; after recovery, which reads the `rfc' section and puts the rfc tool,
;;;; /rfc and (help :rfc) in place.

(defsystem "nodecode-rfc"
  :description "IETF RFC records from the RFC Editor: title, status, authors, abstract and relations, as a tool"
  :license "MIT"
  :author "EloquentBrush0x (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "record")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-rfc/test"))))

(defsystem "nodecode-rfc/test"
  :description "RFC cell tests. Registered into the shared nodecode.test registry; RUN-RFC-TESTS filters by the RFC-CELL- name prefix."
  :license "MIT"
  :author "EloquentBrush0x (original), Nodecode (rewrite)"
  :depends-on ("nodecode-rfc" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-rfc-tests)))
