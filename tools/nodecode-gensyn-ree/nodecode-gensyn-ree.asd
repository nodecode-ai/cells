;;;; nodecode-gensyn-ree.asd --- Gensyn REE receipts, checked with the official image.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of TakeshiTGAL's hermes-gensyn-ree plugin
;;;; (github.com/TakeshiTGAL/hermes-gensyn-plugins, packages/ree). See NOTICE.
;;;;
;;;; Three files beside this one: PACKAGE.LISP, the package and its one
;;;; condition; REE.LISP, the pinned image, the docker argv and the receipt
;;;; comparison; CELL.LISP, the three verbs and the one declaration.

(defsystem "nodecode-gensyn-ree"
  :description "Gensyn REE receipts: the hashes, a CPU re-execution, and one small model run"
  :license "MIT"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "ree")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-gensyn-ree/test"))))

(defsystem "nodecode-gensyn-ree/test"
  :description "REE cell tests. Registered into the shared nodecode.test registry; RUN-GENSYN-REE-TESTS filters by the GENSYN-REE-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-gensyn-ree" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-gensyn-ree-tests)))
