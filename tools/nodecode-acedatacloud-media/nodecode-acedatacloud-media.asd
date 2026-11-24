;;;; nodecode-acedatacloud-media.asd --- Ace Data Cloud's media API, as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `acedatacloud-media' section, registers the five tools and the
;;;; /media command, and puts the primer on (help :acedatacloud-media).
;;;;
;;;; Rewritten from AceDataCloud's Hermes Agent plugin of the same name; see
;;;; NOTICE. Every dependency rides with nodecode: NLK:HTTP speaks to
;;;; api.acedata.cloud and nothing else.

(defsystem "nodecode-acedatacloud-media"
  :description "Ace Data Cloud media: acedatacloud_image, _video, _audio, _task, _lookup and /media"
  :author "AceDataCloud (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-acedatacloud-media/test"))))

(defsystem "nodecode-acedatacloud-media/test"
  :description "acedatacloud-media tests, in the shared nodecode.test registry; RUN-ACEDATACLOUD-MEDIA-TESTS filters by the ACEDATACLOUD-MEDIA-CELL- name prefix."
  :author "AceDataCloud (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-acedatacloud-media" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-acedatacloud-media-tests)))
