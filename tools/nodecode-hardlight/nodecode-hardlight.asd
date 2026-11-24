;;;; nodecode-hardlight.asd --- Hardlight UI, as one cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A tool cell, NOT part of the organism core: nothing in Nodecode names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `hardlight' section, registers the five hardlight_* tools
;;;; and /hardlight, and puts the workflow on (help :hardlight).
;;;;
;;;; Ported from nosliwhtes's nosliwhtes/hardlight-ui (see NOTICE). The
;;;; guides and the style atlas ride with the cell under data/, unchanged;
;;;; the five tools are reimplemented in Lisp and reach nothing outside.

(defsystem "nodecode-hardlight"
  :description "Frontend design guidance with reference grammars and local checks for contrast, navigation and source"
  :license "MIT"
  :author "nosliwhtes (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "data")
               (:file "contrast")
               (:file "navigation")
               (:file "direction")
               (:file "inspect")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-hardlight/test"))))

(defsystem "nodecode-hardlight/test"
  :description "Hardlight tests. Registered into the shared nodecode.test registry; RUN-HARDLIGHT-TESTS filters by the HARDLIGHT- name prefix."
  :license "MIT"
  :author "nosliwhtes (original), Nodecode (rewrite)"
  :depends-on ("nodecode-hardlight" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-hardlight-tests)))
