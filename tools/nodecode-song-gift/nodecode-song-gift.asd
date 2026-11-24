;;;; nodecode-song-gift.asd --- the Custom Song Gift catalog, as tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `song-gift' section and registers the four tools the original
;;;; plugin's server advertised (list_occasions, list_styles,
;;;; guide_story_and_lyrics, get_pricing_options), plus /song-gift and the
;;;; manual topic.
;;;;
;;;; The original is a portable Agent Plugin: a plugin.json, an mcp.json
;;;; pointing at a hosted MCP server, and one skill. This cell speaks MCP's
;;;; streamable HTTP to the same endpoint itself through nlk:http -- no MCP
;;;; transport, no Node, no Hermes runtime and nothing fetched at run time.

(defsystem "nodecode-song-gift"
  :description "Custom Song Gift: gift occasions, musical styles and lyric storytelling advice, as four tools"
  :license "MIT"
  :version "0.1.0"
  :author "GptsApp (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-song-gift/test"))))

(defsystem "nodecode-song-gift/test"
  :description "Custom Song Gift cell tests. Registered into the shared nodecode.test registry; RUN-SONG-GIFT-TESTS filters by the SONG-GIFT-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "GptsApp (original), Nodecode (rewrite)"
  :depends-on ("nodecode-song-gift" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-song-gift-tests)))
