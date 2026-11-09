;;;; nodecode-wolfmarkdown.asd --- Markdown a user can keep: verified, not just printed.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when this directory sits
;;;; under ~/.nodecode/cells/, or the gateway installs it live when
;;;; `nodecode add nc://wolfmarkdown' is accepted. The gateway calls
;;;; START-CELL after recovery; it reads the `wolfmarkdown' config section,
;;;; registers three tools (markdown_verify, markdown_clean, markdown_outline)
;;;; and /markdown, and puts the manual on (help :markdown).

(defsystem "nodecode-wolfmarkdown"
  :description "Check, clean and prove Markdown files: structure and lint findings, plus integrity against the untouched source"
  :license "MIT"
  :author "WolfMarkTools (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "text")
               (:file "checks")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-wolfmarkdown/test"))))

(defsystem "nodecode-wolfmarkdown/test"
  :description "Tests for the wolfmarkdown cell; RUN-WOLFMARKDOWN-TESTS filters the shared registry by the WOLFMARKDOWN- prefix."
  :license "MIT"
  :depends-on ("nodecode-wolfmarkdown" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-wolfmarkdown-tests)))
