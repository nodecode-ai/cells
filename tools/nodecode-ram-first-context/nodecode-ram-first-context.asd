;;;; nodecode-ram-first-context.asd --- read-only keyword recall over a Mnemosyne database.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in src/ names this system.
;;;; The folder loader loads it at boot when this directory sits under
;;;; ~/.nodecode/cells/, or the gateway installs it live when
;;;; `nodecode add nc://ram-first-context' is accepted. No key, no network, no
;;;; shell: the search is SQL over the SQLite file the section names.
;;;;
;;;; A Nodecode rewrite of ether-btc/ram-first-context, a Hermes Agent plugin.
;;;; See NOTICE.

(defsystem "nodecode-ram-first-context"
  :description "RAM-first context: read-only keyword recall over a live Mnemosyne SQLite memory database"
  :license "MIT"
  :author "ether-btc (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "recall")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-ram-first-context/test"))))

(defsystem "nodecode-ram-first-context/test"
  :description "RAM-first context tests. Registered into the shared nodecode.test registry; RUN-RAM-FIRST-CONTEXT-TESTS filters by the RAM-FIRST-CONTEXT name prefix."
  :license "MIT"
  :depends-on ("nodecode-ram-first-context" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-ram-first-context-tests)))
