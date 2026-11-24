;;;; nodecode-chmod-ref.asd --- a file mode, a chmod change or a umask, explained.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `chmod-ref' section, registers the chmod_explain tool and
;;;; /chmod, and puts (help :chmod-ref) on the manual.
;;;;
;;;; The bits are worked out in MODES.LISP, which imports nothing: no
;;;; network, no subprocess, no file read. Every dependency rides with
;;;; nodecode.

(defsystem "nodecode-chmod-ref"
  :description "Explain a file mode, a chmod change or a umask in octal, ls -l and plain words"
  :license "MIT"
  :author "olric3015 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "modes")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-chmod-ref/test"))))

(defsystem "nodecode-chmod-ref/test"
  :description "chmod-ref tests. Registered into the shared nodecode.test registry; RUN-CHMOD-REF-TESTS filters by the CHMOD-REF-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-chmod-ref" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-chmod-ref-tests)))
