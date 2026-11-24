;;;; nodecode-fallback.asd --- heuristic model failover for Nodecode.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `fallback' section, puts (fallback:resolve ...) on the manual
;;;; and registers the tools and the /fallback command cell.lisp lists.
;;;;
;;;; A rewrite of tomkubik/hermes-fallback's model-switching heuristics (see
;;;; NOTICE). One dependency, the core itself.

(defsystem "nodecode-fallback"
  :description "Heuristic model failover: rank your providers' models and keep Nodecode's failover chain filled"
  :license "MIT"
  :version "0.1.0"
  :author "tomkubik (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "facts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-fallback/test"))))

(defsystem "nodecode-fallback/test"
  :description "Fallback tests. Registered into the shared nodecode.test registry; RUN-FALLBACK-TESTS filters by the FALLBACK-CELL- name prefix."
  :license "MIT"
  :author "tomkubik (original), Nodecode (rewrite)"
  :depends-on ("nodecode-fallback" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-fallback-tests)))
