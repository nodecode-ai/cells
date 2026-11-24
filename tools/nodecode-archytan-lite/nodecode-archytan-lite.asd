;;;; nodecode-archytan-lite.asd --- a policy gate on every tool call.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `archytan' section, hooks the :TOOL point, puts (help :archytan)
;;;; on the manual and registers /archytan.
;;;;
;;;; A rewrite of High-ArchyTech-Solutions's hermes-archytan-lite (see NOTICE).
;;;; Every dependency rides with nodecode.

(defsystem "nodecode-archytan-lite"
  :description "Archytan Lite: every tool call checked against a policy before it runs"
  :author "High-ArchyTech-Solutions (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-archytan-lite/test"))))

(defsystem "nodecode-archytan-lite/test"
  :description "Archytan Lite tests. Registered into the shared nodecode.test registry; RUN-ARCHYTAN-LITE-TESTS filters by the ARCHYTAN-LITE-CELL- name prefix."
  :author "High-ArchyTech-Solutions (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-archytan-lite" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-archytan-lite-tests)))
