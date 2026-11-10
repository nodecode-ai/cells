;;;; nodecode-dogmatic-skill-router.asd --- the one skill that fits the turn, named
;;;; before the model call.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `dogmatic-skill-router' section, puts (help :dogmatic-skill-router) on the
;;;; manual, registers /dogmatic-skill-router, and advises the ingress and the
;;;; harness-section read.
;;;;
;;;; A rewrite of DoGMaTiiC's hermes-jev skill router (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-dogmatic-skill-router"
  :description "Names the one skill of yours that fits the turn, before the model call"
  :author "DoGMaTiiC (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "roster")
               (:file "wire")
               (:file "router")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-dogmatic-skill-router/test"))))

(defsystem "nodecode-dogmatic-skill-router/test"
  :description "Skill router tests. Registered into the shared nodecode.test registry; RUN-DOGMATIC-SKILL-ROUTER-TESTS filters by the DOGMATIC-SKILL-ROUTER-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-dogmatic-skill-router" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-dogmatic-skill-router-tests)))
