;;;; nodecode-ethics-filter.asd --- the Ethics Filter as a Nodecode tool.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/, and the gateway calls START-CELL after recovery; the
;;;; `ethics-filter' section says which constitution a call uses by default,
;;;; how strict the verdict is, and where the audit trail is kept. It is a
;;;; rewrite of RegeneratusLabs's Ethics Filter plugin for Hermes Agent -- see
;;;; NOTICE.

(defsystem "nodecode-ethics-filter"
  :description "Six-module ethics engine: any decision comes back GREEN, AMBER or RED with the reasoning"
  :license "MIT"
  :author "RegeneratusLabs (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "engine")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-ethics-filter/test"))))

(defsystem "nodecode-ethics-filter/test"
  :description "Tests for the ethics-filter cell; RUN-ETHICS-FILTER-TESTS filters the shared registry by the ETHICS-FILTER- prefix."
  :license "MIT"
  :author "RegeneratusLabs (original), Nodecode (rewrite)"
  :depends-on ("nodecode-ethics-filter" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-ethics-filter-tests)))
