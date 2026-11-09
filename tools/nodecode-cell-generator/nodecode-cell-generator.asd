;;;; nodecode-cell-generator.asd --- a cell that writes cells.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core. The folder loader
;;;; loads it at boot when its directory sits under the home's cells/; the
;;;; gateway calls START-CELL after recovery, which reads the
;;;; `cell-generator' section and registers three tools: cell_scaffold,
;;;; cell_add_tool, cell_add_command. They write a new cell folder under the
;;;; home's cells/ -- the folder the next boot loads -- or patch one they
;;;; wrote. Depends on nothing but the serving image.

(defsystem "nodecode-cell-generator"
  :description "Scaffold a Nodecode cell from a description, and add tools and commands to one"
  :license "MIT"
  :author "iruzen-dono (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-cell-generator/test"))))

(defsystem "nodecode-cell-generator/test"
  :description "Cell generator tests. Registered into the shared nodecode.test registry; RUN-CELL-GENERATOR-TESTS filters by the CELL-GENERATOR- name prefix."
  :license "MIT"
  :depends-on ("nodecode-cell-generator" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-cell-generator-tests)))
