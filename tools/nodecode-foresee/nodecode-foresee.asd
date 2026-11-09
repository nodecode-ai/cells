;;;; nodecode-foresee.asd --- Foresee insurance quotes, as two tools the model calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `foresee' section, registers the two tools cell.lisp lists and
;;;; puts the plugin's skills on the manual as (help :foresee).
;;;;
;;;; A rewrite of Foresee-Tech's foresee plugin (see NOTICE): the original
;;;; shipped Foresee's hosted MCP server through a portable manifest. Every
;;;; tool it reached was that server's, so this cell speaks MCP's streamable
;;;; HTTP itself and needs no local server and no Node.

(defsystem "nodecode-foresee"
  :description "Foresee home and auto insurance quotes across carriers, with optional live premiums read from the carriers' own sites"
  :author "Foresee-Tech (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-foresee/test"))))

(defsystem "nodecode-foresee/test"
  :description "Foresee cell tests. Registered into the shared nodecode.test registry; RUN-FORESEE-TESTS filters by the FORESEE-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-foresee" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-foresee-tests)))
