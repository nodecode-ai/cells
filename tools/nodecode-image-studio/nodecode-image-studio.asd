;;;; nodecode-image-studio.asd --- images from FAL and Vertex AI, in one gallery.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `image-studio' section, registers the generate_image and
;;;; image_gallery tools and the /image-studio command, and puts the primer on
;;;; (help :image-studio).
;;;;
;;;; Rewritten from phdigital01's Hermes Agent plugin of the same name; see
;;;; NOTICE. Every dependency rides with nodecode: nlk:http speaks to the
;;;; provider, and cl-base64 carries the image bytes a data URI holds.

(defsystem "nodecode-image-studio"
  :description "FAL and Vertex AI images: generate_image, image_gallery and /image-studio"
  :author "phdigital01 (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-image-studio/test"))))

(defsystem "nodecode-image-studio/test"
  :description "Image Studio tests. Registered into the shared nodecode.test registry; RUN-IMAGE-STUDIO-TESTS filters by the IMAGE-STUDIO-CELL- name prefix."
  :author "phdigital01 (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-image-studio" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-image-studio-tests)))
