;;;; nodecode-sigrex.asd --- Sigrex trading automation as model tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `sigrex' section, registers the three tools the original
;;;; plugin gave its harness through Sigrex's MCP server, puts /sigrex and the
;;;; manual topic on, and hands the model the doctrine its skills carried.
;;;;
;;;; The original is a Hermes Agent plugin with no code at all: an mcp.json
;;;; pointing at https://mcp.sigrex.io and thirteen markdown skills. This cell
;;;; talks to the same platform over its documented, signed REST API -- the
;;;; requests the MCP tools make -- and carries the same rules as its primer.
;;;; No Python, nothing fetched at run time, no dependency of its own beyond
;;;; the image's own SHA-256.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting -- or symlinking --
;;;; the directory under ~/.nodecode/cells/.

(defsystem "nodecode-sigrex"
  :description "Sigrex: read and run your trading automation from the conversation"
  :license "MIT"
  :version "0.1.0"
  :author "sigrexio (original), Nodecode (rewrite)"
  :depends-on ("nodecode" "ironclad/digest/sha256")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "verbs")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-sigrex/test"))))

(defsystem "nodecode-sigrex/test"
  :description "Sigrex tests. Registered into the shared nodecode.test registry; RUN-SIGREX-TESTS filters by the SIGREX-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "sigrexio (original), Nodecode (rewrite)"
  :depends-on ("nodecode-sigrex" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-sigrex-tests)))
