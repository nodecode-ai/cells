;;;; nodecode-zerosignal.asd --- ZeroSignal, through its local proxy, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `zerosignal' section and installs the hooks cell.lisp lists.
;;;;
;;;; A rewrite of TxnLab's hermes-zerosignal-provider (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-zerosignal"
  :description "ZeroSignal: private pay-per-request inference through the local zs-proxy"
  :license "MIT"
  :author "TxnLab (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it
  ;; installs it, then asks for a key -- any non-empty value, which the proxy
  ;; ignores, so the funded wallet is the credential
  :properties ((:serves (:provider "zerosignal" :name "ZeroSignal" :sign-in "key")))
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-zerosignal/test"))))

(defsystem "nodecode-zerosignal/test"
  :description "ZeroSignal tests. Registered into the shared nodecode.test registry; RUN-ZEROSIGNAL-TESTS filters by the ZEROSIGNAL-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-zerosignal" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-zerosignal-tests)))
