;;;; nodecode-prompt-enhance.asd --- a draft rewritten outside the chat.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in Nodecode
;;;; names this system. The folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/; the gateway calls START-CELL after
;;;; recovery, which reads the `prompt-enhance' section, registers the
;;;; prompt_* tools cell.lisp lists and puts (help :prompt-enhance) on the
;;;; manual. The rewrite is one chat-completions call to the endpoint the
;;;; section names; the library is a JSON file under the home.
;;;;
;;;; A rewrite of the prompt-enhance plugin of
;;;; apoapostolov/hermes-agent-awesome-plugins, which is apoapostolov's,
;;;; MIT, Copyright (c) 2026 Apostol Apostolov (see NOTICE and LICENSE).
;;;; Every dependency rides with nodecode.

(defsystem "nodecode-prompt-enhance"
  :description "Prompt Enhance: rewrite a draft with a saved enhancer, and keep a prompt library"
  :license "MIT"
  :author "apoapostolov (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "library")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-prompt-enhance/test"))))

(defsystem "nodecode-prompt-enhance/test"
  :description "Prompt Enhance tests. Registered into the shared nodecode.test registry; RUN-PROMPT-ENHANCE-TESTS filters by the PROMPT-ENHANCE-CELL- name prefix."
  :license "MIT"
  :author "apoapostolov (original), Nodecode (rewrite)"
  :depends-on ("nodecode-prompt-enhance" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-prompt-enhance-tests)))
