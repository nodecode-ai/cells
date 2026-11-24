;;;; nodecode-postkitty.asd --- one post in, every platform out, as five tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `postkitty' section, registers the five tools cell.lisp lists
;;;; and puts the original's skill on the manual as (help :postkitty).
;;;;
;;;; A rewrite of mertkayacs/postkitty (see NOTICE): the platform table,
;;;; the counting rules and the five tool bodies are reimplemented here, and
;;;; ffmpeg does the rendering the original did with OpenCV and Pillow.

(defsystem "nodecode-postkitty"
  :description "One draft, photo or video into checked, upload-ready files for eight social platforms, as five tools"
  :author "mertkayacs (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "data")
               (:file "job")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-postkitty/test"))))

(defsystem "nodecode-postkitty/test"
  :description "postkitty tests. Registered into the shared nodecode.test registry; RUN-POSTKITTY-TESTS filters by the POSTKITTY-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-postkitty" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-postkitty-tests)))
