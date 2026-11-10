;;;; nodecode-bumblebee.asd --- known-compromised packages among those installed here.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of the bumblebee-guard plugin for Hermes Agent
;;;; (github.com/kahnanX/bumblebee-guard), commit 8cdf7e0. See NOTICE.
;;;;
;;;; The original wrapped a separately installed scanner bridge and ran its
;;;; shell script. This rewrite reads the package metadata itself, in Lisp,
;;;; and makes the one network call the original's bridge made: OSV.dev.

(defsystem "nodecode-bumblebee"
  :description "Known-compromised npm and PyPI releases among the packages installed in a folder"
  :license "MIT"
  :author "kahnanX (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "scan")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-bumblebee/test"))))

(defsystem "nodecode-bumblebee/test"
  :description "Bumblebee tests. Registered into the shared nodecode.test registry; RUN-BUMBLEBEE-TESTS filters by the BUMBLEBEE-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-bumblebee" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-bumblebee-tests)))
