;;;; nodecode-lsp.asd --- language servers as a cell: diagnostics after a write,
;;;; definition, references, hover, symbols and rename as Lisp verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `lsp' section and installs the hooks cell.lisp lists.
;;;;
;;;; Ported from oh-my-pi's lsp module (see NOTICE). Every dependency rides
;;;; with nodecode; the language servers themselves are the operator's.

(defsystem "nodecode-lsp"
  :description "Language servers for Nodecode: diagnostics on write, navigation and rename verbs"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "rpc")
               (:file "text")
               (:file "servers")
               (:file "client")
               (:file "verbs")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-lsp/test"))))

(defsystem "nodecode-lsp/test"
  :description "LSP cell tests. Registered into the shared nodecode.test registry; RUN-LSP-TESTS filters by the LSP-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-lsp" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-lsp-tests)))
