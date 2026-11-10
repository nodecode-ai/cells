;;;; nodecode-household-tiers.asd --- per-person tool tiers for one shared bot.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `household-tiers' section, puts (help :household-tiers) on the
;;;; manual, registers /household-tiers and the household_tiers tool, and
;;;; hooks the :tool point, so each conversation's tier decides which tools
;;;; its turns may call, and when.
;;;;
;;;; A port of chevlume's Hermes plugin household-tiers (see NOTICE).

(defsystem "nodecode-household-tiers"
  :description "Household Tiers: per-conversation tool tiers, quiet hours and an audit for one shared bot"
  :license "MIT"
  :author "chevlume (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-household-tiers/test"))))

(defsystem "nodecode-household-tiers/test"
  :description "Household Tiers tests. Registered into the shared nodecode.test registry; RUN-HOUSEHOLD-TIERS-TESTS filters by the HOUSEHOLD-TIERS-CELL- name prefix."
  :license "MIT"
  :author "chevlume (original), Nodecode (rewrite)"
  :depends-on ("nodecode-household-tiers" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-household-tiers-tests)))
