;;;; nodecode-astro.asd --- exact astrology charts and planetary timing, as tools the model calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `astro' section, registers the four tools cell.lisp lists and
;;;; the /astro command, and puts the skill's rules on the manual as
;;;; (help :astro).
;;;;
;;;; Rewritten from aidatatools-dev's astro-agents plugin; see NOTICE. The
;;;; original was an MCP server declaration and one skill; this cell speaks
;;;; that server's streamable HTTP itself, through nlk:http.

(defsystem "nodecode-astro"
  :description "Astro Agents: exact Western and Vedic charts and planetary timing over MCP"
  :author "aidatatools-dev (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-astro/test"))))

(defsystem "nodecode-astro/test"
  :description "Astro cell tests. Registered into the shared nodecode.test registry; RUN-ASTRO-TESTS filters by the ASTRO-CELL- name prefix."
  :author "aidatatools-dev (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-astro" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-astro-tests)))
