;;;; nodecode-meme-studio.asd --- make a meme with Meme Studio, from the conversation.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the Hermes plugin `meme-studio-mcp' by MrHydeDev: five
;;;; tools the model calls (`suggest_templates', `search_templates',
;;;; `get_template', `popular_templates', `create_meme') and one slash
;;;; command, /meme, against Meme Studio's hosted MCP server. The cell saves
;;;; the picture the server renders under output_dir and answers with the
;;;; file, its edit_url and the server's own warnings. No key is asked.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting the directory under
;;;; ~/.nodecode/cells/. Presence is enabled; the package is named after the
;;;; system, which is how the loader finds START-CELL.

(defsystem "nodecode-meme-studio"
  :description "Memes made with Meme Studio: pick a template, caption its slots, get the picture"
  :license "MIT"
  :author "MrHydeDev (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "mcp")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-meme-studio/test"))))

(defsystem "nodecode-meme-studio/test"
  :description "Meme Studio tests. They register into the shared nodecode.test registry; RUN-MEME-STUDIO-TESTS runs the MEME-STUDIO-CELL- slice."
  :license "MIT"
  :author "MrHydeDev (original), Nodecode (rewrite)"
  :depends-on ("nodecode-meme-studio" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-meme-studio-tests)))
