;;;; nodecode-gmi-video.asd --- GMI Cloud text- and image-to-video generation.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `gmi-video' section, registers the two tools cell.lisp lists and
;;;; puts (help :gmi-video) on the manual. Rewritten from zkasuran's
;;;; hermes-gmi-media gmi-video plugin (see NOTICE): its provider becomes the
;;;; gmi_video tool and the gmi-video:generate verb, and its picker's model
;;;; list the gmi_video_models tool.

(defsystem "nodecode-gmi-video"
  :description "GMI Cloud text- and image-to-video: make a video from a prompt or a public image URL"
  :license "Apache-2.0"
  :version "0.1.0"
  :author "zkasuran (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-gmi-video/test"))))

(defsystem "nodecode-gmi-video/test"
  :description "GMI Cloud video tests. Registered into the shared nodecode.test registry; RUN-GMI-VIDEO-TESTS filters by the GMI-VIDEO-CELL- name prefix."
  :license "Apache-2.0"
  :version "0.1.0"
  :depends-on ("nodecode-gmi-video" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-gmi-video-tests)))
