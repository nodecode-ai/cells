;;;; nodecode-enhance-prompt.asd --- rewrite a rough draft into an agent brief.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `enhance_prompt' section, registers the enhance_prompt tool and
;;;; the /enhance command, and puts the manual on (help :enhance-prompt).
;;;;
;;;; Rewritten from tuancookiez-hub's Hermes Agent plugin of the same name; see
;;;; NOTICE. Every dependency rides with nodecode: the rewrite is one side call
;;;; through NLK:COMPLETE, and CL-PPCRE reads the wrappers off the answer.

(defsystem "nodecode-enhance-prompt"
  :description "Enhance Prompt: rewrite a draft into a structured agent brief, in place"
  :author "tuancookiez-hub (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "prompts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-enhance-prompt/test"))))

(defsystem "nodecode-enhance-prompt/test"
  :description "Enhance Prompt tests. Registered into the shared nodecode.test registry; RUN-ENHANCE-PROMPT-TESTS filters by the ENHANCE-PROMPT-CELL- name prefix."
  :author "tuancookiez-hub (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-enhance-prompt" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-enhance-prompt-tests)))
