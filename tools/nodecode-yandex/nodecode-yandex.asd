;;;; nodecode-yandex.asd --- the Yandex Search API as two model tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `yandex' section, registers the two tools the original plugin
;;;; gave its harness (yandex web search, and yandex_generative_search), puts
;;;; /yandex and the manual topic on, and hands the model a primer.
;;;;
;;;; The original is a Hermes Agent plugin: a web-search backend plus one
;;;; tool, both over Yandex Cloud's Search API v2, in Python. This cell makes
;;;; the same two requests through the image's own HTTP call -- no Python,
;;;; nothing fetched at run time, no dependency of its own -- and answers the
;;;; same fields as lines a model reads.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting -- or symlinking --
;;;; the directory under ~/.nodecode/cells/.

(defsystem "nodecode-yandex"
  :description "Yandex Search API: web results and grounded answers as two model tools, with your own key"
  :license "MIT"
  :version "0.1.0"
  :author "akinfold (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "client")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-yandex/test"))))

(defsystem "nodecode-yandex/test"
  :description "Yandex Search API tests. Registered into the shared nodecode.test registry; RUN-YANDEX-TESTS filters by the YANDEX-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "akinfold (original), Nodecode (rewrite)"
  :depends-on ("nodecode-yandex" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-yandex-tests)))
