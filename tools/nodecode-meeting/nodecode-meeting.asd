;;;; nodecode-meeting.asd --- Meeting Overlay as four model tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `meeting' section, registers the four tools, puts /meeting and
;;;; the manual topic on, and hands the model a primer.
;;;;
;;;; The original is a macOS desktop plugin: a whisper daemon writing
;;;; ~/.hermes/meeting-transcript/rolling.txt and full.txt, a launchd agent
;;;; beside it, a phone PWA, and a pane that polls all of it. The transcript
;;;; files are the surface a cell can carry, so this one reads and writes
;;;; exactly those two and the records the original saved to a vault. No
;;;; Python, no whisper, no launchd, nothing downloaded at run time.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting - or symlinking - the
;;;; directory under ~/.nodecode/cells/.

(defsystem "nodecode-meeting"
  :description "Meeting Overlay: the live meeting transcript and the records saved from it"
  :license "MIT"
  :version "0.1.0"
  :author "taxbax (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-meeting/test"))))

(defsystem "nodecode-meeting/test"
  :description "Meeting Overlay tests. Registered into the shared nodecode.test registry; RUN-MEETING-TESTS filters by the MEETING-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "taxbax (original), Nodecode (rewrite)"
  :depends-on ("nodecode-meeting" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-meeting-tests)))
