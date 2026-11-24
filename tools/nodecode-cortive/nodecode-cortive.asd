;;;; nodecode-cortive.asd --- the local Cortive library as the agent's recall.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `cortive' section, registers the three tools the original
;;;; plugin gave its harness, hooks the :TOOL point for its guard, puts
;;;; /cortive and the manual topic on.
;;;;
;;;; The original is a Hermes Agent memory provider for Cortive, a private
;;;; knowledge base for macOS. This cell reads the same library file with the
;;;; image's own SQLite binding (cl-sqlite, which Nodecode already loads) and
;;;; answers the same three tools as text.

(defsystem "nodecode-cortive"
  :description "Cortive memory: recall captured pages, PDFs and screenshots from a local Cortive library"
  :license "MIT"
  :version "0.1.0"
  :author "KadenDevAi (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "library")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-cortive/test"))))

(defsystem "nodecode-cortive/test"
  :description "Cortive memory tests. Registered into the shared nodecode.test registry; RUN-CORTIVE-TESTS filters by the CORTIVE-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "KadenDevAi (original), Nodecode (rewrite)"
  :depends-on ("nodecode-cortive" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-cortive-tests)))
