;;;; nodecode-tab-watchdog.asd --- close the stale tabs a browser was driven into leaving.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `tab_watchdog' section, puts (help :tab-watchdog) on the
;;;; manual, registers the tabs_inspect and tabs_sweep tools and the /tabs
;;;; command, and starts the sweep worker when auto is on.
;;;;
;;;; A rewrite of web3blind/hermes-chrome-tab-watchdog (see NOTICE).

(defsystem "nodecode-tab-watchdog"
  :description "Close stale blank, idle and duplicate Chrome tabs"
  :license "MIT"
  :author "web3blind (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-tab-watchdog/test"))))

(defsystem "nodecode-tab-watchdog/test"
  :description "Tab watchdog tests. Registered into the shared nodecode.test registry; RUN-TAB-WATCHDOG-TESTS filters by the TAB-WATCHDOG-CELL- name prefix."
  :license "MIT"
  :author "web3blind (original), Nodecode (rewrite)"
  :depends-on ("nodecode-tab-watchdog" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-tab-watchdog-tests)))
