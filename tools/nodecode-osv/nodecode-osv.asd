;;;; nodecode-osv.asd --- CVE, GHSA and OSV records from OSV.dev, for the model.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of the osv-ref plugin for Hermes Agent
;;;; (github.com/EloquentBrush0x/hermes-context-refs). See NOTICE.
;;;;
;;;; Two files beside this one: OSV.LISP, the network seam and the rendering,
;;;; and CELL.LISP, the one declaration. The .asd sits INSIDE its folder:
;;;; install by putting -- or symlinking -- this directory under
;;;; ~/.nodecode/cells/. Presence is enabled; the package is named after the
;;;; system, which is how the loader finds START-CELL.

(defsystem "nodecode-osv"
  :description "CVE, GHSA and OSV vulnerability records from OSV.dev"
  :license "MIT"
  :author "EloquentBrush0x (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "osv")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-osv/test"))))

(defsystem "nodecode-osv/test"
  :description "OSV cell tests. Registered into the shared nodecode.test registry; RUN-OSV-TESTS filters by the OSV-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-osv" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-osv-tests)))
