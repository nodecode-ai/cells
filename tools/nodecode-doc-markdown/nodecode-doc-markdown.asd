;;;; nodecode-doc-markdown.asd --- one local document to Markdown, for the model and the operator.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in Nodecode's
;;;; own sources names this system. The folder loader loads it at boot when
;;;; this directory sits under ~/.nodecode/cells/, or the gateway installs it
;;;; live when `nodecode add nc://doc-markdown' is accepted. The gateway calls
;;;; START-CELL after recovery, which declares the doc-markdown section, the
;;;; doc_convert tool and /doc; nothing here starts a thread or opens a port.

(defsystem "nodecode-doc-markdown"
  :description "One local PDF, DOCX, PPTX, XLSX, HTML, CSV, JSON, TXT or MD file to Markdown, as a tool"
  :license "MIT"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode" "flexi-streams")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-doc-markdown/test"))))

(defsystem "nodecode-doc-markdown/test"
  :description "Tests for the doc-markdown cell; RUN-DOC-MARKDOWN-TESTS filters the shared registry by the DOC-MARKDOWN prefix."
  :license "MIT"
  :depends-on ("nodecode-doc-markdown" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "convert-test")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-doc-markdown-tests)))
