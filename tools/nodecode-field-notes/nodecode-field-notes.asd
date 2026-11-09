;;;; nodecode-field-notes.asd --- Field Notes: pitfalls, local patches and a check of each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `field_notes' section, registers the four fieldnote_* tools and
;;;; /fieldnotes, and puts (help :field-notes) on the manual.
;;;;
;;;; A Nodecode rewrite of itpartypattaya/hermes-field-notes (see NOTICE).

(defsystem "nodecode-field-notes"
  :description "Field Notes: pitfalls, local patches and a read-only check that each patch survived"
  :license "MIT"
  :version "0.1.0"
  :author "itpartypattaya (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-field-notes/test"))))

(defsystem "nodecode-field-notes/test"
  :description "Field Notes tests. Registered into the shared nodecode.test registry; RUN-FIELD-NOTES-TESTS filters by the FIELD-NOTES-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "itpartypattaya (original), Nodecode (rewrite)"
  :depends-on ("nodecode-field-notes" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-field-notes-tests)))
