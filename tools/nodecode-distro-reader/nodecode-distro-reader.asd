;;;; nodecode-distro-reader.asd --- Distro Reader: eight tools over Distro's reader.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `distro-reader' section, registers the eight tools cell.lisp
;;;; lists and puts the plugin's three skills on the manual as
;;;; (help :distro-reader).
;;;;
;;;; A rewrite of Distro-Media's hermes-plugin-distro-reader (see NOTICE): the
;;;; original shipped Distro's hosted MCP server through a portable manifest.
;;;; This cell speaks the same MCP endpoint directly, one JSON-RPC POST a
;;;; call, so it needs no MCP client, no Node and no local server. Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-distro-reader"
  :description "Distro Reader: search and read Distro's news, stories, books and podcasts, as eight tools"
  :author "Distro-Media (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-distro-reader/test"))))

(defsystem "nodecode-distro-reader/test"
  :description "Distro Reader tests. Registered into the shared nodecode.test registry; RUN-DISTRO-READER-TESTS filters by the DISTRO-READER-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-distro-reader" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-distro-reader-tests)))
