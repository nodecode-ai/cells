;;;; nodecode-insight.asd --- Insight: a definition of a term, with its sources.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `insight' section, registers the define_term tool and the
;;;; /insight command, and puts the primer on (help :insight).
;;;;
;;;; Rewritten from BrokeSkill's Hermes Agent plugin of the same name; see
;;;; NOTICE. Every dependency rides with nodecode: nlk:http speaks to the
;;;; operator's provider and to a SearXNG instance, quri encodes the query.

(defsystem "nodecode-insight"
  :description "Insight: define a term from your own provider, with the sources it was given"
  :author "BrokeSkill (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-insight/test"))))

(defsystem "nodecode-insight/test"
  :description "Insight tests. Registered into the shared nodecode.test registry; RUN-INSIGHT-TESTS filters by the INSIGHT-CELL- name prefix."
  :author "BrokeSkill (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-insight" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-insight-tests)))
