;;;; nodecode-botsee.asd --- BotSee's hosted MCP server as two model tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `botsee' section, registers the two tools the original plugin
;;;; gave its harness (list the server's tools, call one), puts /botsee and the
;;;; manual topic on, and hands the model a primer.
;;;;
;;;; The original is a Hermes Agent plugin: a portable manifest declaring
;;;; https://botsee.io/mcp as a streamable-HTTP MCP server plus a research
;;;; skill. This cell IS the MCP client the manifest borrowed -- initialize,
;;;; initialized, tools/list, tools/call, the bearer key, the session header,
;;;; the SSE body read -- written in Lisp over the image's own HTTP call: no
;;;; Python, nothing fetched at run time, no dependency of its own.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting -- or symlinking --
;;;; the directory under ~/.nodecode/cells/.

(defsystem "nodecode-botsee"
  :description "BotSee: AI-visibility research through its hosted MCP server, with your own key"
  :license "MIT"
  :version "0.1.0"
  :author "sidekickjanet (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "mcp")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-botsee/test"))))

(defsystem "nodecode-botsee/test"
  :description "BotSee tests. Registered into the shared nodecode.test registry; RUN-BOTSEE-TESTS filters by the BOTSEE-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "sidekickjanet (original), Nodecode (rewrite)"
  :depends-on ("nodecode-botsee" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-botsee-tests)))
