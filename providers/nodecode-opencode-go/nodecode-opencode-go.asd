;;;; nodecode-opencode-go.asd --- OpenCode Go, OpenCode's model subscription, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `opencode-go' section and installs the hooks cell.lisp lists.
;;;;
;;;; Follows oh-my-pi's opencode-go provider (see NOTICE). Every dependency
;;;; rides with nodecode.

(defsystem "nodecode-opencode-go"
  :description "OpenCode Go: OpenCode's model subscription as a Nodecode provider"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-opencode-go/test"))))

(defsystem "nodecode-opencode-go/test"
  :description "OpenCode Go tests. Registered into the shared nodecode.test registry; RUN-OPENCODE-GO-TESTS filters by the OPENCODE-GO-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-opencode-go" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-opencode-go-tests)))
