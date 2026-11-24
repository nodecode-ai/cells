;;;; nodecode-markdown-new.asd --- Markdown.new: read, search, crawl and convert the web, as one tool.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `markdown-new' section, registers the markdown_new tool and the
;;;; /markdown-new command, and puts the service's manual on (help
;;;; :markdown-new).
;;;;
;;;; The original is nosliwhtes/hermes-markdown-new (see NOTICE): a Hermes
;;;; plugin whose one tool, markdown_new, shelled out to a companion Python
;;;; CLI that spoke to the public Markdown.new service. The service needs no
;;;; key, so this cell posts to its endpoints itself through nlk:http: no
;;;; Python, no CLI and no subprocess is involved.

(defsystem "nodecode-markdown-new"
  :description "Markdown.new: read, search, crawl and convert public web pages to Markdown, as a tool the model calls"
  :author "nosliwhtes (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-markdown-new/test"))))

(defsystem "nodecode-markdown-new/test"
  :description "Markdown.new cell tests. Registered into the shared nodecode.test registry; RUN-MARKDOWN-NEW-TESTS filters by the MARKDOWN-NEW-CELL- name prefix."
  :license "MIT"
  :author "nosliwhtes (original), Nodecode (rewrite)"
  :depends-on ("nodecode-markdown-new" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-markdown-new-tests)))
