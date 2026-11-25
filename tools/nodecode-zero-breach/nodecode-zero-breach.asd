;;;; nodecode-zero-breach.asd --- one machine checked for the traces of known
;;;; mercenary spyware, with the gaps kept apart from the clean answer: four
;;;; calls and a /zero-breach command over the vendored scanner.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `zero-breach' section and registers zerobreach_scan,
;;;; zerobreach_phone, zerobreach_last, zerobreach_describe, /zero-breach and
;;;; (help :zero-breach).
;;;;
;;;; The scanner itself is archivegenocide's, vendored unmodified under
;;;; vendor/ at the commit VENDOR.md names (Apache-2.0, see NOTICE). It needs
;;;; nothing but Python 3.8 or newer and its own standard library.

(defsystem "nodecode-zero-breach"
  :description "Check this machine for the traces of known mercenary spyware, with the gaps kept as gaps"
  :license "MIT (the Lisp); the vendored scanner under vendor/ is Apache-2.0"
  :author "archivegenocide (the scanner), Nodecode (the cell)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "engine")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-zero-breach/test"))))

(defsystem "nodecode-zero-breach/test"
  :description "Zero Breach tests. Registered into the shared nodecode.test registry; RUN-ZERO-BREACH-TESTS filters by the ZERO-BREACH-CELL- name prefix."
  :license "MIT"
  :author "Nodecode"
  :depends-on ("nodecode-zero-breach" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-zero-breach-tests)))
