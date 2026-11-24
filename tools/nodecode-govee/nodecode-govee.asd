;;;; nodecode-govee.asd --- Govee lights as one tool and one command.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `govee' section and registers the one tool cell.lisp declares,
;;;; the /govee command and the manual topic (help :govee).
;;;;
;;;; A rewrite of promptclickrun's hermes-govee (see NOTICE): the original
;;;; shipped one tool and one skill over Govee's developer API. This cell keeps
;;;; that tool's actions, its capability checks and its state read-back, and
;;;; posts to the same four endpoints itself.

(defsystem "nodecode-govee"
  :description "Govee lights: discovery, state, scenes and control, each change checked by a state read-back"
  :author "promptclickrun (original), Nodecode (rewrite)"
  :license "Apache-2.0"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "lights")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-govee/test"))))

(defsystem "nodecode-govee/test"
  :description "Govee cell tests: the lifecycle, the one tool over a stubbed wire, and /govee."
  :license "Apache-2.0"
  :depends-on ("nodecode-govee" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
             (uiop:symbol-call '#:nodecode.test '#:run-govee-tests)))
