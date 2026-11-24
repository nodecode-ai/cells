;;;; nodecode-web-local.asd --- a page read here, as markdown, with no key.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `web-local' section, registers the local_extract tool and the
;;;; /web-local command, and puts the manual on (help :web-local).
;;;;
;;;; Nothing outside this folder is needed: the page is fetched with the
;;;; image's own nlk:http and reduced by cl-ppcre. See NOTICE: this folder is a
;;;; rewrite of enzo-adami/hermes-plugin-web-local.

(defsystem "nodecode-web-local"
  :description "Web Local: pages read here, as markdown, with no key and no service"
  :license "MIT"
  :version "0.1.0"
  :author "enzo-adami (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-web-local/test"))))

(defsystem "nodecode-web-local/test"
  :description "Web Local tests. Registered into the shared nodecode.test registry; RUN-WEB-LOCAL-TESTS filters by the WEB-LOCAL-CELL- name prefix."
  :license "MIT"
  :author "enzo-adami (original), Nodecode (rewrite)"
  :depends-on ("nodecode-web-local" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-web-local-tests)))
