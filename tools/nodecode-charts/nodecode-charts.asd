;;;; nodecode-charts.asd --- draw a verified PNG chart from a table.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in the
;;;; release names this system. The folder loader loads it at boot when this
;;;; directory sits under ~/.nodecode/cells/, or the gateway installs it live
;;;; when `nodecode add nc://charts' is accepted. It reads the `charts'
;;;; section, contributes three tools (chart_render, chart_validate,
;;;; chart_types), one command (/charts) and one manual topic (help :charts),
;;;; and starts no thread. The PNG encoder is its own; salza2, which the
;;;; serving image already loads for its own deflate, is the one dependency
;;;; beyond nodecode.

(defsystem "nodecode-charts"
  :description "Bar, line, stacked and share charts as checked PNG images"
  :license "MIT"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode" "salza2")
  :serial t
  :components ((:file "package")
               (:file "numfmt")
               (:file "table")
               (:file "plan")
               (:file "png")
               (:file "render")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-charts/test"))))

(defsystem "nodecode-charts/test"
  :description "Charts tests, in the shared nodecode.test registry; RUN-CHARTS-TESTS filters by the CHARTS- name prefix."
  :license "MIT"
  :depends-on ("nodecode-charts" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-charts-tests)))
