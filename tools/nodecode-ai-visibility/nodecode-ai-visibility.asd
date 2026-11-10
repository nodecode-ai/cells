;;;; nodecode-ai-visibility.asd --- AI visibility checks from qqq.ee, for the model.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of the qqq-ai-visibility plugin for Hermes Agent
;;;; (github.com/meelis545/qqq-ai-visibility-mcp). See NOTICE.
;;;;
;;;; Two files beside this one: WIRE.LISP, the one JSON-RPC call to the hosted
;;;; server and the reading of its answer, and CELL.LISP, the verbs, the three
;;;; tools and the one declaration. The .asd sits INSIDE its folder: install by
;;;; putting -- or symlinking -- this directory under ~/.nodecode/cells/.
;;;; Presence is enabled; the package is named after the system, which is how
;;;; the loader finds START-CELL.

(defsystem "nodecode-ai-visibility"
  :description "AI visibility: whether AI crawlers can read a site, and its brand mentions in AI answers"
  :license "MIT"
  :author "meelis545 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-ai-visibility/test"))))

(defsystem "nodecode-ai-visibility/test"
  :description "AI visibility cell tests. Registered into the shared nodecode.test registry; RUN-AI-VISIBILITY-TESTS filters by the AI-VISIBILITY-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-ai-visibility" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-ai-visibility-tests)))
