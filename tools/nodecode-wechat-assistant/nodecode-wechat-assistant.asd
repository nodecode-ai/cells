;;;; nodecode-wechat-assistant.asd --- local relationship memory for WeChat contacts.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A Nodecode rewrite of harrylabsj/hermes-plugin-wechat-social-assistant
;;;; (Apache-2.0).
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in Nodecode's
;;;; src names this system. The folder loader loads it at boot when its
;;;; directory sits under ~/.nodecode/cells/; the gateway calls START-CELL
;;;; after recovery, which reads the `wechat' section, registers six tools and
;;;; /wechat, puts the manual on (help :wechat), and keeps the memory in one
;;;; JSON file under the home (mode 0600) that nothing dials out of.

(defsystem "nodecode-wechat-assistant"
  :description "Local relationship memory for WeChat contacts"
  :license "Apache-2.0"
  :version "0.1.0"
  :author "harrylabsj (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-wechat-assistant/test"))))

(defsystem "nodecode-wechat-assistant/test"
  :description "Relationship memory tests. Registered into the shared nodecode.test registry; RUN-WECHAT-TESTS filters by the WECHAT-CELL- name prefix."
  :license "Apache-2.0"
  :author "harrylabsj (original), Nodecode (rewrite)"
  :depends-on ("nodecode-wechat-assistant" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-wechat-tests)))
