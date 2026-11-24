;;;; nodecode-devtune.asd --- DevTune's reports as one model tool.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `devtune' section, registers the one tool the original plugin
;;;; offered its harness (devtune_read), puts /devtune and the manual topic
;;;; on, and hands the model a primer.
;;;;
;;;; The original is a Hermes Agent plugin: no code at all, an MCP server
;;;; entry and two skill prompts. This cell makes the same reads against the
;;;; same REST API with the image's own HTTP call -- no MCP client, no
;;;; Python, nothing fetched at run time, no dependency of its own.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting -- or symlinking --
;;;; the directory under ~/.nodecode/cells/.

(defsystem "nodecode-devtune"
  :description "DevTune: what AI answers say about a product, and which tools coding agents pick"
  :license "MIT"
  :version "0.1.0"
  :author "devtunehq (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "api")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-devtune/test"))))

(defsystem "nodecode-devtune/test"
  :description "DevTune tests. Registered into the shared nodecode.test registry; RUN-DEVTUNE-TESTS filters by the DEVTUNE-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "devtunehq (original), Nodecode (rewrite)"
  :depends-on ("nodecode-devtune" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-devtune-tests)))
