;;;; nodecode-memory-shaper.asd --- a keyword index in front of the model.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A rewrite of hermes-dynamic-memory's memory-shaper plugin as a Nodecode
;;;; cell: one line per subject in a flat index the model reads every turn,
;;;; the sentences in topic files on disk, and the entry's shape checked by
;;;; the tool that writes it.

(defsystem "nodecode-memory-shaper"
  :description "A flat keyword index in front of the model, the detail in topic files"
  :license "Apache-2.0"
  :version "0.1.0"
  :author "ockentap (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-memory-shaper/test"))))

(defsystem "nodecode-memory-shaper/test"
  :description "Memory Shaper tests. Registered into the shared nodecode.test registry; RUN-MEMORY-SHAPER-TESTS filters by the MEMORY-SHAPER-CELL- name prefix."
  :license "Apache-2.0"
  :author "ockentap (original), Nodecode (rewrite)"
  :depends-on ("nodecode-memory-shaper" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-memory-shaper-tests)))
