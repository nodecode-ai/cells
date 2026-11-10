;;;; nodecode-caduceus.asd --- a private health vault, and four tools over it.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core. The folder loader loads it at
;;;; boot when its directory sits under ~/.nodecode/cells/; the gateway calls
;;;; START-CELL after recovery, which reads the `caduceus' section (the one
;;;; required setting is the vault folder), registers the four tools, the
;;;; /caduceus command and (help :caduceus).
;;;;
;;;; Rewritten from electricsheephq's Hermes Agent plugin of the same name;
;;;; see NOTICE. Everything is local: the vault is files under one folder the
;;;; user names, no file is ever uploaded, and nothing here dials anything.

(defsystem "nodecode-caduceus"
  :description "Caduceus: a private local vault for DNA raw data, gene-drug answers, Apple Health trends and health notes"
  :author "electricsheephq (original), Nodecode (rewrite)"
  :license "Apache-2.0"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "store")
               (:file "genome")
               (:file "health")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-caduceus/test"))))

(defsystem "nodecode-caduceus/test"
  :description "Caduceus tests: registered into the shared nodecode.test registry; RUN-CADUCEUS-TESTS filters by the CADUCEUS-CELL- name prefix."
  :author "electricsheephq (original), Nodecode (rewrite)"
  :license "Apache-2.0"
  :depends-on ("nodecode-caduceus" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-caduceus-tests)))
