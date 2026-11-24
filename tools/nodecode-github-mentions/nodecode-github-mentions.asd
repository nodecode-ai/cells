;;;; nodecode-github-mentions.asd --- GitHub @mentions as tasks the agent answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `github_mentions' section, registers the three tools and the
;;;; /mentions command, and puts the primer on (help :github-mentions).
;;;;
;;;; Rewritten from luantak's Hermes Agent plugin luantak/hermes-github-mentions,
;;;; commit d8a88220a547180663fcec76a069c8fbbf63bff9 (MIT); see NOTICE.
;;;; Every dependency rides with nodecode: nlk:http speaks to api.github.com,
;;;; ppcre reads the mention pattern.

(defsystem "nodecode-github-mentions"
  :description "GitHub @mentions as tasks: list the unread ones, read the thread, answer in it"
  :author "luantak (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-github-mentions/test"))))

(defsystem "nodecode-github-mentions/test"
  :description "GitHub mentions tests. Registered into the shared nodecode.test registry; RUN-GITHUB-MENTIONS-TESTS filters by the GITHUB-MENTIONS-CELL- name prefix."
  :author "luantak (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-github-mentions" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-github-mentions-tests)))
