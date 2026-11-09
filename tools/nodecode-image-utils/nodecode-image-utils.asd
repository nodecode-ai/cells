;;;; nodecode-image-utils.asd --- six local image tools, over the engine's own codecs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; An optional ASDF cell: nothing in src/ names this system. The folder
;;;; loader loads it at boot when this directory sits under ~/.nodecode/cells/,
;;;; or the gateway installs it live when `nodecode add nc://image-utils' is
;;;; accepted. The gateway calls START-CELL after recovery; it reads the
;;;; `image-utils' config section (max_pixels, jpeg_quality) and registers six
;;;; model tools and the /image-utils command: image_info, image_resize,
;;;; image_crop, image_rotate, image_convert and image_optimize. No HTTP, and
;;;; no subprocess unless the machine offers a transcoder for a format the
;;;; engine's own readers do not open.

(defsystem "nodecode-image-utils"
  :description "Inspect, resize, crop, rotate, convert and shrink local image files, in-process"
  :license "MIT"
  :author "MazenMkhinini (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-image-utils/test"))))

(defsystem "nodecode-image-utils/test"
  :description "Tests for the image-utils cell; RUN-IMAGE-UTILS-TESTS runs the IMAGE-UTILS- slice."
  :license "MIT"
  :author "MazenMkhinini (original), Nodecode (rewrite)"
  :depends-on ("nodecode-image-utils" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-image-utils-tests)))
