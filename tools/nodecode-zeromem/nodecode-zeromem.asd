;;;; nodecode-zeromem.asd --- every turn verbatim in one local file, recalled.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core. The folder loader
;;;; loads it at boot when its directory sits under ~/.nodecode/cells/; the
;;;; gateway calls START-CELL after recovery, which reads the `zeromem'
;;;; section, keeps what this session says in one file under the home,
;;;; registers the three zeromem_ tools and /zeromem, and puts past sessions'
;;;; words into a turn's first request.
;;;;
;;;; A rewrite of ptaranat/zeromem (MIT) in Common Lisp: see README.md and
;;;; NOTICE beside this file.

(defsystem "nodecode-zeromem"
  :description "Every turn verbatim in one local file, recalled with provenance"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "store")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-zeromem/test"))))

(defsystem "nodecode-zeromem/test"
  :description "Zeromem tests. Registered into the shared nodecode.test registry; RUN-ZEROMEM-TESTS filters by the ZEROMEM-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-zeromem" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-zeromem-tests)))
