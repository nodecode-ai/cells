;;;; nodecode-apple.asd --- Apple Foundation Models (on-device), through omp's Swift bridge.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `apple' section and, on a Mac with Apple silicon, registers the
;;;; apple lane; anywhere else it says so once and does nothing.
;;;;
;;;; Ported from oh-my-pi's apple provider (see NOTICE). Every dependency
;;;; rides with nodecode; the bridge is Swift, built on the Mac from bridge/.

(defsystem "nodecode-apple"
  :description "Apple Foundation Models: the on-device model, through omp's Swift bridge, as a provider lane (macOS only)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "schema")
               (:file "wire")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-apple/test"))))

(defsystem "nodecode-apple/test"
  :description "Apple Foundation Models tests. Registered into the shared nodecode.test registry; RUN-APPLE-TESTS filters by the APPLE-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-apple" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-apple-tests)))
