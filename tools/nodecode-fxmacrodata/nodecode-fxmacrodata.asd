;;;; nodecode-fxmacrodata.asd --- FXMacroData's macro and FX research data as 100 tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `fxmacrodata' section and registers the 100 tools operations.lisp
;;;; and api.lisp carry.
;;;;
;;;; One seam reaches outside: NLK:HTTP, to api.fxmacrodata.com over REST and
;;;; mcp.fxmacrodata.com over MCP, with an optional key in a header. Nothing
;;;; is installed, and no dependency is added beyond what nodecode already
;;;; loads.

(defsystem "nodecode-fxmacrodata"
  :description "FXMacroData: macro and FX research data as 100 tools -- indicator history, release calendars, rates, positioning, commodities"
  :license "MIT"
  :version "0.1.0"
  :author "fxmacrodata (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "operations")
               (:file "api")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-fxmacrodata/test"))))

(defsystem "nodecode-fxmacrodata/test"
  :description "FXMacroData tests. Registered into the shared nodecode.test registry; RUN-FXMACRODATA-TESTS filters by the FXMACRODATA-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "fxmacrodata (original), Nodecode (rewrite)"
  :depends-on ("nodecode-fxmacrodata" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-fxmacrodata-tests)))
