;;;; nodecode-gitlab-duo.asd --- GitLab Duo (non-agentic chat), signed in with GitLab, as a provider.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `gitlab-duo' section and installs the hooks and the command
;;;; cell.lisp lists.
;;;;
;;;; Ported from oh-my-pi's gitlab-duo provider (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-gitlab-duo"
  :description "GitLab Duo Non-Agentic: Duo's chat models through GitLab's AI gateway, as a Nodecode provider"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "provider")
               (:file "signin")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-gitlab-duo/test"))))

(defsystem "nodecode-gitlab-duo/test"
  :description "GitLab Duo tests. Registered into the shared nodecode.test registry; RUN-GITLAB-DUO-TESTS filters by the GITLAB-DUO-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-gitlab-duo" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-gitlab-duo-tests)))
