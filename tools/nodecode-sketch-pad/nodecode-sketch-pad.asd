;;;; nodecode-sketch-pad.asd --- a drawn PNG becomes a file the organism can look at.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in Nodecode's
;;;; source names this system. The folder loader loads it at boot when its
;;;; directory sits under ~/.nodecode/cells/, and the gateway calls START-CELL
;;;; after recovery, which reads the `sketch-pad' section, serves the pad page
;;;; on 127.0.0.1, registers the two tools, /sketch and (help :sketch).
;;;;
;;;; It is a rewrite of aydnOktay's hermes-sketch-pad for Hermes Agent -- see
;;;; NOTICE. Every dependency named below is already in the serving image.

(defsystem "nodecode-sketch-pad"
  :description "Sketch pad: draw with the mouse in a browser tab, and the PNG is saved where the model can look at it"
  :license "MIT"
  :author "aydnOktay (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode" "clack" "clack-handler-hunchentoot" "hunchentoot"
               "flexi-streams" "usocket" "cl-base64" "bordeaux-threads")
  :serial t
  :components ((:file "package")
               (:file "store")
               (:file "pad")
               (:file "server")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-sketch-pad/test"))))

(defsystem "nodecode-sketch-pad/test"
  :description "Sketch-pad tests. Registered into the shared nodecode.test registry; RUN-SKETCH-PAD-TESTS filters by the SKETCH-PAD- name prefix."
  :license "MIT"
  :author "aydnOktay (original), Nodecode (rewrite)"
  :depends-on ("nodecode-sketch-pad" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-sketch-pad-tests)))
