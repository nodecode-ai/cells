;;;; nodecode-imagegen.asd --- images through an OpenAI Images-compatible gateway.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `imagegen' section, registers the generate_image tool and the
;;;; /image command, and puts the primer on (help :imagegen).
;;;;
;;;; Rewritten from duu261's Hermes Agent plugin of the same name; see NOTICE.
;;;; Every dependency rides with nodecode: nlk:http speaks to the gateway,
;;;; cl-base64 decodes what comes back.

(defsystem "nodecode-imagegen"
  :description "Images through any OpenAI Images-compatible gateway: generate_image and /image"
  :author "duu261 (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-imagegen/test"))))

(defsystem "nodecode-imagegen/test"
  :description "imagegen tests. Registered into the shared nodecode.test registry; RUN-IMAGEGEN-TESTS filters by the IMAGEGEN-CELL- name prefix."
  :author "duu261 (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-imagegen" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-imagegen-tests)))
