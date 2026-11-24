;;;; nodecode-gmi-image.asd --- GMI Cloud's image models, as a cell.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `gmi_image' section, registers the gmi_image tool and the
;;;; /gmi-image command, and puts the primer on (help :gmi-image).
;;;;
;;;; Rewritten from zkasuran's Hermes Agent plugin of the same name; see
;;;; NOTICE. Every dependency rides with nodecode: nlk:http speaks to GMI.

(defsystem "nodecode-gmi-image"
  :description "GMI Cloud images: Seedream, Gemini and the rest of GMI's live catalog, as the gmi_image tool and /gmi-image"
  :author "zkasuran (original), Nodecode (rewrite)"
  :license "Apache-2.0"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-gmi-image/test"))))

(defsystem "nodecode-gmi-image/test"
  :description "gmi-image tests. Registered into the shared nodecode.test registry; RUN-GMI-IMAGE-TESTS filters by the GMI-IMAGE-CELL- name prefix."
  :author "zkasuran (original), Nodecode (rewrite)"
  :license "Apache-2.0"
  :depends-on ("nodecode-gmi-image" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-gmi-image-tests)))
