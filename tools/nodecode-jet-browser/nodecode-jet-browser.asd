;;;; nodecode-jet-browser.asd --- run Jet Browser's acceptance check.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A Nodecode rewrite of the Hermes integration in masakaai/jet-browser
;;;; (integrations/hermes, Apache-2.0): one tool, one command, one bounded
;;;; `docker run' of the project's digest-pinned image.
;;;;
;;;; The folder loader loads this system at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `jet-browser' section, registers JET_BROWSER_VERIFY and
;;;; /jetbrowser, and puts the manual on (help :jet-browser).

(defsystem "nodecode-jet-browser"
  :description "Jet Browser's acceptance check in a network-isolated container"
  :license "Apache-2.0"
  :version "0.1.0"
  :author "masakaai (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-jet-browser/test"))))

(defsystem "nodecode-jet-browser/test"
  :description "Jet Browser cell tests: the lifecycle, the tool, the command, the transport's refusals."
  :license "Apache-2.0"
  :author "masakaai (original), Nodecode (rewrite)"
  :depends-on ("nodecode-jet-browser" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-jet-browser-tests)))
