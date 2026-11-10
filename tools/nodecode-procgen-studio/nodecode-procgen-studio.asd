;;;; nodecode-procgen-studio.asd --- image assets drawn as code, rendered locally.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode cell, NOT part of the organism core: nothing in Nodecode names
;;;; this system. A rewrite of iamlukethedev's hermes-procgen-studio (see
;;;; NOTICE). The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `procgen-studio' section and registers the four tools, the
;;;; /procgen-studio command and the manual on (help :procgen-studio).
;;;;
;;;; Nothing here reaches the network. A pixel-grid source is drawn by
;;;; render.lisp alone, a PNG written byte by byte; an SVG source is
;;;; rasterised by the user's own program on PATH, found once.

(defsystem "nodecode-procgen-studio"
  :description "Draw image assets as code -- SVG and pixel grids -- rendered locally to PNG with a seed"
  :license "MIT"
  :author "iamlukethedev (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "presets")
               (:file "style")
               (:file "render")
               (:file "store")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-procgen-studio/test"))))

(defsystem "nodecode-procgen-studio/test"
  :description "Procgen Studio tests. Registered into the shared nodecode.test registry; RUN-PROCGEN-STUDIO-TESTS filters by the PROCGEN-STUDIO-CELL- name prefix."
  :license "MIT"
  :author "iamlukethedev (original), Nodecode (rewrite)"
  :depends-on ("nodecode-procgen-studio" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-procgen-studio-tests)))
