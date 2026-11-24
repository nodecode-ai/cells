;;;; nodecode-bankbridge.asd --- BankBridge's read-only bank data as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `bankbridge' section, registers the twelve tools, /bankbridge
;;;; and the original plugin's 19 money reviews, and puts the manual on
;;;; (help :bankbridge).
;;;;
;;;; Rewritten from bankbridge-money's Hermes Agent plugin of the same subject;
;;;; see NOTICE. Every dependency rides with nodecode: nlk:http is the one
;;;; HTTP client, and the MCP client over Streamable HTTP is this folder's own.

(defsystem "nodecode-bankbridge"
  :description "BankBridge: your own bank, card and brokerage accounts, read-only"
  :author "bankbridge-money (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "commands")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-bankbridge/test"))))

(defsystem "nodecode-bankbridge/test"
  :description "BankBridge tests. Registered into the shared nodecode.test registry; RUN-BANKBRIDGE-TESTS filters by the BANKBRIDGE-CELL- name prefix."
  :author "bankbridge-money (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-bankbridge" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-bankbridge-tests)))
