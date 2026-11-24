;;;; nodecode-dyscalculia.asd --- cited dyscalculia statistics, as six model tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `dyscalculia' section, registers the six tools the original
;;;; plugin's hosted MCP server offered, puts /dyscalculia and the manual
;;;; topic on, and hands the model the skill's own guidance as the primer.
;;;;
;;;; The original is a Hermes Agent plugin, configuration only: one hosted MCP
;;;; server and one skill. This cell speaks the same MCP endpoint from Lisp
;;;; through the image's own HTTP call -- no Python, no Node, nothing fetched
;;;; at run time, no dependency of its own.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting -- or symlinking --
;;;; the directory under ~/.nodecode/cells/.

(defsystem "nodecode-dyscalculia"
  :description "Dyscalculia statistics: cited prevalence, references, chart data and screening-test facts from dyscalculia.io"
  :license "MIT"
  :version "0.1.0"
  :author "Dyscalculia-io (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "mcp")
               (:file "verbs")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-dyscalculia/test"))))

(defsystem "nodecode-dyscalculia/test"
  :description "Dyscalculia cell tests: offline, the one HTTP seam stubbed."
  :license "MIT"
  :version "0.1.0"
  :author "Dyscalculia-io (original), Nodecode (rewrite)"
  :depends-on ("nodecode-dyscalculia" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-dyscalculia-tests)))
