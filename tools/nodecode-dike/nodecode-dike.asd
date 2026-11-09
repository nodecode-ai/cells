;;;; nodecode-dike.asd --- Chilean-law article citations, checked against a local statute pack.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `dike' section, registers the dike_verificar_citas tool and
;;;; /dike, and puts (help :dike) on the manual.
;;;;
;;;; A rewrite of Grizaceo/hermes-dike (see NOTICE). Every dependency rides
;;;; with nodecode; the statute tables under data/ are its own.

(defsystem "nodecode-dike"
  :description "Chilean-law citations checked against a local, versioned statute pack"
  :license "MIT"
  :author "Grizaceo (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "corpus")
               (:file "cite")
               (:file "scan")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-dike/test"))))

(defsystem "nodecode-dike/test"
  :description "Dike tests. Registered into the shared nodecode.test registry; RUN-DIKE-TESTS filters by the DIKE-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-dike" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-dike-tests)))
