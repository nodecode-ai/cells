;;;; nodecode-document-ocr.asd --- document OCR through your own endpoint.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `document_ocr' section and registers the two tools and
;;;; /document-ocr. Every request goes to the OpenAI-compatible endpoint the
;;;; section names.
;;;;
;;;; A Nodecode rewrite of lEWFkRAD's hermes-document-reader (see NOTICE).

(defsystem "nodecode-document-ocr"
  :description "Document OCR: reads page images and PDF scans into Markdown through your own OpenAI-compatible endpoint"
  :license "MIT"
  :version "0.1.0"
  :author "lEWFkRAD (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-document-ocr/test"))))

(defsystem "nodecode-document-ocr/test"
  :description "Document OCR tests. Registered into the shared nodecode.test registry; RUN-DOCUMENT-OCR-TESTS filters by the DOCUMENT-OCR-CELL- name prefix."
  :license "MIT"
  :author "lEWFkRAD (original), Nodecode (rewrite)"
  :depends-on ("nodecode-document-ocr" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-document-ocr-tests)))
