;;;; nodecode-agentpurse.asd --- search, quote and buy inside an AgentPurse allowance.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. A rewrite of the Hermes plugin maaz-bin-hassan/agentpurse at
;;;; plugins/hermes, its twelve MCP tools as twelve Lisp verbs called through
;;;; eval, its /purse command as this cell's, and its human approval gate as a
;;;; question put to the person watching (NLK:ASK-OPERATOR) or a /purse approve
;;;; typed by them. Every exchange with agentpurse.vercel.app is NLK:DEX from
;;;; this folder; a test stubs the one function that dials.

(defsystem "nodecode-agentpurse"
  :description "Search, quote and buy inside an AgentPurse allowance, only after a person approves"
  :license "MIT"
  :author "maaz-bin-hassan (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "client")
               (:file "state")
               (:file "gate")
               (:file "verbs")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-agentpurse/test"))))

(defsystem "nodecode-agentpurse/test"
  :description "AgentPurse cell tests: offline, every HTTP exchange stubbed."
  :license "MIT"
  :depends-on ("nodecode-agentpurse" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-agentpurse-tests)))
