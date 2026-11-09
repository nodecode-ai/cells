;;;; nodecode-talaria.asd --- the talaria-webui plugin's routes as four tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `talaria' section, registers the four tools and puts the primer
;;;; on (help :talaria).
;;;;
;;;; Rewritten from liamsmith86's Hermes Agent plugin of the same name; see
;;;; NOTICE. Every dependency rides with nodecode: nlk:http speaks to the
;;;; Hermes API server the plugin extends.

(defsystem "nodecode-talaria"
  :description "Talaria WebUI: search Hermes session history, read a reply's details and a session's context use"
  :author "liamsmith86 (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-talaria/test"))))

(defsystem "nodecode-talaria/test"
  :description "Talaria tests. Registered into the shared nodecode.test registry; RUN-TALARIA-TESTS filters by the TALARIA-CELL- name prefix."
  :author "liamsmith86 (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-talaria" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-talaria-tests)))
