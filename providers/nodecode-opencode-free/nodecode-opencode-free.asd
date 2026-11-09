;;;; nodecode-opencode-free.asd --- OpenCode's free models through the OpenCode CLI.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core. The folder loader loads it when
;;;; its directory sits under ~/.nodecode/cells/; the gateway calls
;;;; START-CELL after recovery, which reads the `opencode-free' section and
;;;; installs the lane and hooks cell.lisp lists.

(defsystem "nodecode-opencode-free"
  :description "OpenCode Free: OpenCode's free models through the OpenCode CLI's ACP server"
  :license "MIT"
  :author "cryptobredda (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking
  ;; OpenCode Free installs it, and the CLI's own login is the credential
  :properties ((:serves (:provider "opencode-free" :name "OpenCode Free" :sign-in "external")))
  :serial t
  :components ((:file "package")
               (:file "acp")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-opencode-free/test"))))

(defsystem "nodecode-opencode-free/test"
  :description "OpenCode Free tests. Registered into the shared nodecode.test registry; the slice filters by the OPENCODE-FREE-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-opencode-free" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-opencode-free-tests)))
