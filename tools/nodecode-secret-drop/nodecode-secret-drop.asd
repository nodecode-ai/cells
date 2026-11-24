;;;; nodecode-secret-drop.asd --- a secret dropped into the environment, not the chat.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `secret-drop' section, loads the store, puts each value into
;;;; this image's environment, advises the wire fold, and puts /secret-drop,
;;;; (help :secret-drop), list_secrets and request_secret up.

(defsystem "nodecode-secret-drop"
  :description "Drop a secret into the environment without it entering the conversation, and redact exact copies of it"
  :author "Goitonthefloor (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-secret-drop/test"))))

(defsystem "nodecode-secret-drop/test"
  :description "Secret drop tests. Registered into the shared nodecode.test registry; RUN-SECRET-DROP-TESTS filters by the SECRET-DROP-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-secret-drop" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-secret-drop-tests)))
