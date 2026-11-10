;;;; nodecode-kiro-acp.asd --- the Kiro CLI (AWS) as a provider, over ACP.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `kiro-acp' section, registers the kiro-acp lane and installs
;;;; the hooks cell.lisp lists.
;;;;
;;;; Rewritten from tobenwarrior's hermes-kiro-provider (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-kiro-acp"
  :description "Kiro CLI (AWS) driven over ACP stdio: its own login, one agent session per turn"
  :license "MIT"
  :author "tobenwarrior (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  ;; what /models offers this folder by before it is installed: picking it installs it; the login is the CLI's
  :properties ((:serves (:provider "kiro-acp" :name "Kiro (AWS)" :sign-in "cli")))
  :serial t
  :components ((:file "package")
               (:file "acp")
               (:file "provider")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-kiro-acp/test"))))

(defsystem "nodecode-kiro-acp/test"
  :description "Kiro-acp tests. Registered into the shared nodecode.test registry; RUN-KIRO-ACP-TESTS filters by the KIRO-ACP-CELL- name prefix."
  :license "MIT"
  :author "tobenwarrior (original), Nodecode (rewrite)"
  :depends-on ("nodecode-kiro-acp" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-kiro-acp-tests)))
