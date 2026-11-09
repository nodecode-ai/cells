;;;; nodecode-beam.asd --- Beam transfers and Rooms as tools the model calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: the folder loader loads it at boot
;;;; when its directory sits under ~/.nodecode/cells/; the gateway calls
;;;; START-CELL after recovery, which reads the `beam' section and registers
;;;; the three tools and /beam cell.lisp declares.
;;;;
;;;; Rewritten from Beam-Network/beam-hermes (see NOTICE): its Beam API calls
;;;; and its `beam' CLI calls, without the Python SDK or the detached workers.

(defsystem "nodecode-beam"
  :description "Beam transfers and Rooms: the account, transfers, and the beam CLI's Room verbs"
  :license "MIT"
  :author "Beam-Network (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "beam")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-beam/test"))))

(defsystem "nodecode-beam/test"
  :description "Beam tests. Registered into the shared nodecode.test registry; RUN-BEAM-TESTS filters by the BEAM-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-beam" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-beam-tests)))
