;;;; nodecode-perplexity.asd --- Perplexity web search, signed in with a Pro/Max account.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `perplexity' section, puts (perplexity:search ...) on the
;;;; manual and registers the /perplexity command cell.lisp lists.
;;;;
;;;; Ported from oh-my-pi's perplexity sign-in and the web search provider it
;;;; serves (see NOTICE). Every dependency rides with nodecode (dexador
;;;; brings cl-cookie, which keeps the sign-in's cookies).

(defsystem "nodecode-perplexity"
  :description "Perplexity web search: sign in with a Pro/Max account, or use a key, and search from eval"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "signin")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-perplexity/test"))))

(defsystem "nodecode-perplexity/test"
  :description "Perplexity tests. Registered into the shared nodecode.test registry; RUN-PERPLEXITY-TESTS filters by the PERPLEXITY-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-perplexity" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-perplexity-tests)))
