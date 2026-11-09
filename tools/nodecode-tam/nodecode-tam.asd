;;;; nodecode-tam.asd --- TAM (total-agent-memory) as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `tam' section, registers the two tools the original plugin gave
;;;; its harness (tam_recall, tam_save), puts /tam and the manual topic on.
;;;;
;;;; The original is a Hermes Agent memory provider: it spawns the `tam'
;;;; binary and speaks MCP to it over stdio, or posts MCP over HTTP to a TAM
;;;; server. This cell speaks the same MCP surface to a TAM server over HTTP --
;;;; the same tools, the same records -- through the image's own HTTP call, so
;;;; it needs no Python and nothing of the original's runtime.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting -- or symlinking --
;;;; the directory under ~/.nodecode/cells/.

(defsystem "nodecode-tam"
  :description "TAM memory: recall and save facts in a total-agent-memory store shared with every agent"
  :license "MIT"
  :version "0.1.0"
  :author "vbcherepanov (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "mcp")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-tam/test"))))

(defsystem "nodecode-tam/test"
  :description "TAM tests. Registered into the shared nodecode.test registry; RUN-TAM-TESTS filters by the TAM-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "vbcherepanov (original), Nodecode (rewrite)"
  :depends-on ("nodecode-tam" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-tam-tests)))
