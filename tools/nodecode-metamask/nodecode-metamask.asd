;;;; nodecode-metamask.asd --- the MetaMask agent wallet, as tools the model calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `metamask' section and registers the ten tools cell.lisp lists,
;;;; the /metamask command and (help :metamask).
;;;;
;;;; A rewrite of akugone's hermes-metamask-wallet (see NOTICE): the plugin
;;;; was a Python bridge to the MetaMask agent wallet CLI, `mm --json'. This
;;;; cell is the same bridge in Lisp, over the same program on PATH, with no
;;;; Python -- and MetaMask still holds the keys.

(defsystem "nodecode-metamask"
  :description "MetaMask agent wallet: balances, prices, history, quotes, transfers and signatures through mm"
  :author "akugone (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-metamask/test"))))

(defsystem "nodecode-metamask/test"
  :description "Metamask tests. Registered into the shared nodecode.test registry; RUN-METAMASK-TESTS filters by the METAMASK-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-metamask" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-metamask-tests)))
