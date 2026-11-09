;;;; nodecode-webly.asd --- publish a site or share files at a live Webly link.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `webly' section and registers the three tools and the /webly
;;;; command cell.lisp declares.
;;;;
;;;; A Nodecode rewrite of Webly-AI/webly-plugin's Hermes package (see NOTICE).
;;;; Every dependency rides with nodecode: the API calls are NLK:HTTP.

(defsystem "nodecode-webly"
  :description "Webly: publish a website or share files at a live webly.site link"
  :license "Apache-2.0"
  :author "Webly-AI (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "client")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-webly/test"))))

(defsystem "nodecode-webly/test"
  :description "Webly tests. Registered into the shared nodecode.test registry; RUN-WEBLY-TESTS filters by the WEBLY-CELL- name prefix."
  :license "Apache-2.0"
  :author "Webly-AI (original), Nodecode (rewrite)"
  :depends-on ("nodecode-webly" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-webly-tests)))
