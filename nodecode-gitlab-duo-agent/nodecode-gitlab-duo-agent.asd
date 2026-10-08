;;;; nodecode-gitlab-duo-agent.asd --- the GitLab Duo Agent Platform, over its own wire, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `gitlab-duo-agent' section, registers the gitlab-duo-agent
;;;; lane and installs the hooks and the command cell.lisp lists.
;;;;
;;;; Ported from oh-my-pi's gitlab-duo-agent provider (see NOTICE). Every
;;;; dependency rides with nodecode: dexador for GitLab's REST and GraphQL,
;;;; websocket-driver for the Duo Workflow socket.

(defsystem "nodecode-gitlab-duo-agent"
  :description "GitLab Duo Agent: the Duo Workflow Service as a Nodecode provider lane"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "wire")
               (:file "workflow")
               (:file "signin")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-gitlab-duo-agent/test"))))

(defsystem "nodecode-gitlab-duo-agent/test"
  :description "GitLab Duo Agent tests. Registered into the shared nodecode.test registry; RUN-GITLAB-DUO-AGENT-TESTS filters by the GITLAB-DUO-AGENT-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-gitlab-duo-agent" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-gitlab-duo-agent-tests)))
