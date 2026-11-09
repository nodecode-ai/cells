;;;; nodecode-crosodo.asd --- US cottage-food law, by state and county, as tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `crosodo' section, registers the six tools, puts /crosodo and
;;;; the manual topic on.
;;;;
;;;; The original is a Hermes Agent plugin: a portable manifest pointing an
;;;; MCP client at one streamable-HTTP endpoint. This cell is that client --
;;;; the image's own HTTP call, JSON-RPC over POST, no Python, no MCP
;;;; runtime, nothing fetched at run time -- answering the same fields as
;;;; text a model reads.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting -- or symlinking --
;;;; the directory under ~/.nodecode/cells/.

(defsystem "nodecode-crosodo"
  :description "Crosodo: US cottage-food law by state and county, as model tools"
  :license "MIT"
  :version "0.1.0"
  :author "Terahertz-Inc (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-crosodo/test"))))

(defsystem "nodecode-crosodo/test"
  :description "Crosodo tests. Registered into the shared nodecode.test registry; RUN-CROSODO-TESTS filters by the CROSODO-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "Terahertz-Inc (original), Nodecode (rewrite)"
  :depends-on ("nodecode-crosodo" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-crosodo-tests)))
