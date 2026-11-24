;;;; nodecode-vision-inline.asd --- the session model reads the image itself.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in src/ names this system.
;;;; The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/, or the gateway installs it live when
;;;; `nodecode add nc://vision-inline' is accepted. The gateway calls
;;;; START-CELL after recovery, which reads the `vision-inline' section and
;;;; registers the see_image tool, /vision and the manual topic.
;;;;
;;;; A Nodecode rewrite of BrokeSkill/hermes-vision-inline (see NOTICE).

(defsystem "nodecode-vision-inline"
  :description "Vision Inline: the session model reads an image itself when it can, else your captioner reads it"
  :license "MIT"
  :version "0.1.0"
  :author "BrokeSkill (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-vision-inline/test"))))

(defsystem "nodecode-vision-inline/test"
  :description "Vision Inline tests. Registered into the shared nodecode.test registry; RUN-VISION-INLINE-TESTS filters by the VISION-INLINE-CELL- name prefix."
  :license "MIT"
  :author "BrokeSkill (original), Nodecode (rewrite)"
  :depends-on ("nodecode-vision-inline" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-vision-inline-tests)))
