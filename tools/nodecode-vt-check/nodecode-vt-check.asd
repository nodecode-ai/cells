;;;; nodecode-vt-check.asd --- VirusTotal reputation for downloads and links, for the model.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of the vt-check plugin for Hermes Agent
;;;; (github.com/Aravinth-Earth/vt-check). See NOTICE.
;;;;
;;;; Two files beside this one: VT.LISP, the network seam, the hashing and the
;;;; archive forensics, and CELL.LISP, the one declaration. The .asd sits
;;;; INSIDE its folder: install by putting -- or symlinking -- this directory
;;;; under ~/.nodecode/cells/. Presence is enabled; the package is named after
;;;; the system, which is how the loader finds START-CELL.

(defsystem "nodecode-vt-check"
  :description "VirusTotal reputation for downloads, links, domains and IPs"
  :license "MIT"
  :author "Aravinth-Earth (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode" "chipz" "flexi-streams" "ironclad/digest/sha256" "ironclad/digest/sha1" "ironclad/digest/md5")
  :serial t
  :components ((:file "package")
               (:file "vt")
               (:file "archive") (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-vt-check/test"))))

(defsystem "nodecode-vt-check/test"
  :description "vt-check cell tests. Registered into the shared nodecode.test registry; RUN-VT-CHECK-TESTS filters by the VT-CHECK- name prefix."
  :license "MIT"
  :depends-on ("nodecode-vt-check" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-vt-check-tests)))
