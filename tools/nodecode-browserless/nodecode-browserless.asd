;;;; nodecode-browserless.asd --- Browserless cloud pages as Nodecode tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `browserless' section and registers the three page tools
;;;; cell.lisp lists. Every request goes to the Browserless endpoint the
;;;; section names; nothing is installed, and no browser is started here.
;;;;
;;;; A rewrite of browserless/browserless-mcp's Hermes plugin (see NOTICE).
;;;; Every dependency rides with nodecode.

(defsystem "nodecode-browserless"
  :description "Browserless: render pages and take screenshots in a stealth cloud browser"
  :license "MIT"
  :author "browserless (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "client")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-browserless/test"))))

(defsystem "nodecode-browserless/test"
  :description "Browserless tests. Registered into the shared nodecode.test registry; RUN-BROWSERLESS-TESTS filters by the BROWSERLESS-CELL- name prefix."
  :license "MIT"
  :author "browserless (original), Nodecode (rewrite)"
  :depends-on ("nodecode-browserless" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-browserless-tests)))
