;;;; nodecode-pulldown.asd --- public pages as bounded markdown, by the pulldown CLI.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `pulldown' section, registers the pulldown_fetch tool and the
;;;; /pulldown command, and puts the manual on (help :pulldown).
;;;;
;;;; The one thing outside this folder is the pulldown program itself, a Python
;;;; CLI the user installs with `pip install pulldown' (https://github.com/
;;;; anthony-maio/pulldown), which reads a page and prints it as JSON.
;;;; See NOTICE: this folder is a rewrite of anthony-maio/pulldown.

(defsystem "nodecode-pulldown"
  :description "Pulldown: a public web page as bounded Markdown, with optional Chromium rendering"
  :license "MIT"
  :version "0.1.0"
  :author "anthony-maio (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-pulldown/test"))))

(defsystem "nodecode-pulldown/test"
  :description "Pulldown tests. Registered into the shared nodecode.test registry; RUN-PULLDOWN-TESTS filters by the PULLDOWN-CELL- name prefix."
  :license "MIT"
  :author "anthony-maio (original), Nodecode (rewrite)"
  :depends-on ("nodecode-pulldown" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-pulldown-tests)))
