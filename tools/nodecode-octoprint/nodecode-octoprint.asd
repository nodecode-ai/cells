;;;; nodecode-octoprint.asd --- one OctoPrint printer's job, over its REST API.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `octoprint' section, registers the three printer tools and
;;;; /octoprint, and puts the manual on (help :octoprint).

(defsystem "nodecode-octoprint"
  :description "OctoPrint printer: job progress, temperatures, webcam still, pause, resume, cancel"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "watch")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-octoprint/test"))))

(defsystem "nodecode-octoprint/test"
  :description "OctoPrint tests. Registered into the shared nodecode.test registry; RUN-OCTOPRINT-TESTS filters by the OCTOPRINT-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-octoprint" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-octoprint-tests)))
