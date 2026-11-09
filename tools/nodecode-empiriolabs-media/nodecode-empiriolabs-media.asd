;;;; nodecode-empiriolabs-media.asd --- image, video and speech from EmpirioLabs AI.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `empiriolabs-media' section, registers the image, video and
;;;; speech tools and /empiriolabs-media, and puts (help :empiriolabs-media)
;;;; on the manual.
;;;;
;;;; A Nodecode rewrite of the media backends in EmpirioLabs-AI/hermes-empiriolabs
;;;; (see NOTICE). Every dependency rides with nodecode: the wire is nlk:http.

(defsystem "nodecode-empiriolabs-media"
  :description "Image and video generation and text-to-speech through EmpirioLabs AI"
  :license "MIT"
  :version "0.1.0"
  :author "EmpirioLabs-AI (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "media")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-empiriolabs-media/test"))))

(defsystem "nodecode-empiriolabs-media/test"
  :description "EmpirioLabs AI media tests. Registered into the shared nodecode.test registry; RUN-EMPIRIOLABS-MEDIA-TESTS filters by the EMPIRIOLABS-MEDIA-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "EmpirioLabs-AI (original), Nodecode (rewrite)"
  :depends-on ("nodecode-empiriolabs-media" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-empiriolabs-media-tests)))
