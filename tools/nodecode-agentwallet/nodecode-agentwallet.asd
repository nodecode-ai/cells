;;;; nodecode-agentwallet.asd --- OpenClawCash managed wallets, as five tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `agentwallet' section, registers the five tools cell.lisp lists
;;;; and puts the original's guidance on the manual as (help :agentwallet).
;;;;
;;;; A rewrite of openclawcash's agentwalletapi-hermes (see NOTICE): the
;;;; plugin was a thin Python bridge to OpenClawCash's agent REST API. This
;;;; cell is the same bridge in Lisp, over nlk:http, with no Python.

(defsystem "nodecode-agentwallet"
  :description "OpenClawCash managed EVM and Solana wallets: list, get, rename, create and send transfers, as five tools the model calls"
  :author "openclawcash (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-agentwallet/test"))))

(defsystem "nodecode-agentwallet/test"
  :description "Wallet cell tests. Registered into the shared nodecode.test registry; RUN-AGENTWALLET-TESTS filters by the AGENTWALLET-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-agentwallet" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-agentwallet-tests)))
