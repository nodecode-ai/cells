;;;; nodecode-camofox.asd --- the Camofox anti-detection browser as browser tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in Nodecode
;;;; names this system. The folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/; the gateway calls START-CELL after
;;;; recovery, which reads the `camofox' section and registers the browser
;;;; tools cell.lisp lists. The server is the operator's own: this cell never
;;;; installs, starts or stops it, and every byte it sends goes to the address
;;;; the section names.
;;;;
;;;; A rewrite of the camofox plugin of NousResearch/hermes-official-plugins
;;;; (see NOTICE). Every dependency rides with nodecode.

(defsystem "nodecode-camofox"
  :description "Camofox: drive a self-hosted Camoufox browser server with browser tools"
  :license "MIT"
  :author "NousResearch (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "client")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-camofox/test"))))

(defsystem "nodecode-camofox/test"
  :description "Camofox tests. Registered into the shared nodecode.test registry; RUN-CAMOFOX-TESTS filters by the CAMOFOX-CELL- name prefix."
  :license "MIT"
  :author "NousResearch (original), Nodecode (rewrite)"
  :depends-on ("nodecode-camofox" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-camofox-tests)))
