;;;; nodecode-neuralmind.asd --- NeuralMind's code memory, added to every turn.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `neuralmind' section, puts /neuralmind and (help :neuralmind)
;;;; up, and hooks the :ROUND point so a turn's own prompt is recalled into
;;;; its request, and the :TOOL point so a write is recorded.
;;;;
;;;; The one thing outside this folder is the neuralmind program itself, which
;;;; the user installs (pip install neuralmind) and builds a project with.
;;;; See NOTICE: this folder is a rewrite of dfrostar/neuralmind's Hermes
;;;; plugin, neuralmind/hermes_plugin.

(defsystem "nodecode-neuralmind"
  :description "NeuralMind code memory: the project's related files, symbols and decisions added to each turn"
  :license "MIT"
  :version "0.1.0"
  :author "dfrostar (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-neuralmind/test"))))

(defsystem "nodecode-neuralmind/test"
  :description "NeuralMind tests. Registered into the shared nodecode.test registry; RUN-NEURALMIND-TESTS filters by the NEURALMIND-CELL- name prefix."
  :license "MIT"
  :author "dfrostar (original), Nodecode (rewrite)"
  :depends-on ("nodecode-neuralmind" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-neuralmind-tests)))
