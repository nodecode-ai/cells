;;;; nodecode-orcaslicer.asd --- OrcaSlicer's presets, its CLI, and a printer's door.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `orcaslicer' section, registers the seven tools, puts the
;;;; primer on (help :orcaslicer) and registers /orca.
;;;;
;;;; Rewritten from mazzanfar's Hermes Agent plugin of the same name; see
;;;; NOTICE. Every dependency rides with nodecode: uiop runs the installed
;;;; OrcaSlicer, nlk:http speaks to a printer, ppcre reads the credential
;;;; name pattern, and shasht reads and writes the presets and job records.

(defsystem "nodecode-orcaslicer"
  :description "OrcaSlicer: search the installed presets, slice a model, read the job, and check or load a printer"
  :author "mazzanfar (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-orcaslicer/test"))))

(defsystem "nodecode-orcaslicer/test"
  :description "OrcaSlicer tests. Registered into the shared nodecode.test registry; RUN-ORCASLICER-TESTS filters by the ORCASLICER-CELL- name prefix."
  :author "mazzanfar (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-orcaslicer" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-orcaslicer-tests)))
