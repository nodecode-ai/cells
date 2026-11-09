;;;; nodecode-github-app.asd --- the seven GitHub tools, as one cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `github_app' section, registers the seven tools and the
;;;; /github-app command, and puts the manual on (help :github-app).
;;;;
;;;; Rewritten from himanusia's Hermes Agent plugin
;;;; himanusia/hermes-github-app-plugin, commit 2adfd020f93a5d71a563e94c88738fb8e64335f8
;;;; (MIT); see NOTICE. NLK:HTTP speaks to the API, PPCRE reads the two
;;;; patterns, and ironclad signs the App's RS256 JWT.

(defsystem "nodecode-github-app"
  :description "GitHub as one resolved identity: issues, comments, reviews and merges, with write actions gated"
  :author "himanusia (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode" "ironclad")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "wire")
               (:file "auth")
               (:file "client")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-github-app/test"))))

(defsystem "nodecode-github-app/test"
  :description "GitHub App tests. Registered into the shared nodecode.test registry; RUN-GITHUB-APP-TESTS filters by the GITHUB-APP-CELL- name prefix."
  :author "himanusia (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-github-app" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-github-app-tests)))
