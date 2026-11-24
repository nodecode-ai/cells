;;;; nodecode-cloudflare-web-search.asd --- Cloudflare Web Search as one model tool.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `cloudflare-web-search' section, registers the one search tool
;;;; the original gave its harness, puts /cloudflare-web-search and the manual
;;;; topic on, and hands the model a primer.
;;;;
;;;; The original is a Hermes Agent plugin: a Python web-search provider
;;;; that POSTs to Cloudflare's websearch endpoint and hands back a JSON
;;;; envelope. This cell makes the same request through the image's own HTTP
;;;; call -- no Python, nothing fetched at run time, no dependency of its own
;;;; -- and answers the same fields as lines a model reads.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting -- or symlinking --
;;;; the directory under ~/.nodecode/cells/.

(defsystem "nodecode-cloudflare-web-search"
  :description "Cloudflare Web Search: Ceramic, Exa or Linkup through your own Cloudflare account, as one model tool"
  :license "MIT"
  :version "0.1.0"
  :author "Adolanium (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "search")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-cloudflare-web-search/test"))))

(defsystem "nodecode-cloudflare-web-search/test"
  :description "Cloudflare Web Search tests. Registered into the shared nodecode.test registry; RUN-CLOUDFLARE-WEB-SEARCH-TESTS filters by the CLOUDFLARE-WEB-SEARCH-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "Adolanium (original), Nodecode (rewrite)"
  :depends-on ("nodecode-cloudflare-web-search" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-cloudflare-web-search-tests)))
