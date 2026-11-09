;;;; nodecode-gemini-media.asd --- Gemini images (Nano Banana) and video (Omni).
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `gemini-media' section, registers the gemini_image,
;;;; gemini_video and gemini_models tools and the /gemini-media command, and
;;;; puts the manual on (help :gemini-media).
;;;;
;;;; Rewritten from bytecakelake's Hermes Agent plugin of the same subject;
;;;; see NOTICE. Every dependency rides with nodecode: NLK:HTTP speaks to
;;;; the Gemini API, cl-base64 carries the inline media both ways.

(defsystem "nodecode-gemini-media"
  :description "Gemini images (Nano Banana) and video (Omni): gemini_image, gemini_video and /gemini-media"
  :author "bytecakelake (original), Nodecode (rewrite)"
  :license "Apache-2.0"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-gemini-media/test"))))

(defsystem "nodecode-gemini-media/test"
  :description "gemini-media tests. Registered into the shared nodecode.test registry; RUN-GEMINI-MEDIA-TESTS filters by the GEMINI-MEDIA-CELL- name prefix."
  :author "bytecakelake (original), Nodecode (rewrite)"
  :license "Apache-2.0"
  :depends-on ("nodecode-gemini-media" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-gemini-media-tests)))
