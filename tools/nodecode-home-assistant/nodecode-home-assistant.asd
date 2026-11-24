;;;; nodecode-home-assistant.asd --- Home Assistant devices as four tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `homeassistant' section, registers the four tools and puts the
;;;; primer on (help :home-assistant).
;;;;
;;;; Rewritten from NousResearch's Hermes Agent plugin of the same name; see
;;;; NOTICE. Every dependency rides with nodecode: nlk:http speaks to the
;;;; instance, ppcre reads the patterns and names.

(defsystem "nodecode-home-assistant"
  :description "Home Assistant: list entities, read a state, list services, call a service"
  :author "NousResearch (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-home-assistant/test"))))

(defsystem "nodecode-home-assistant/test"
  :description "Home Assistant tests. Registered into the shared nodecode.test registry; RUN-HOME-ASSISTANT-TESTS filters by the HOME-ASSISTANT-CELL- name prefix."
  :author "NousResearch (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-home-assistant" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-home-assistant-tests)))
