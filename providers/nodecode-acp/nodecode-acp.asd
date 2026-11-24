;;;; nodecode-acp.asd --- any Agent Client Protocol agent as a session's agent.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional quicklisp/ASDF cell, NOT part of the organism core: nothing in
;;;; src/ names this system. The folder loader (kernel cells.lisp) loads it at
;;;; boot when its directory sits under ~/.nodecode/cells/; the gateway calls
;;;; START-CELL after recovery, which reads the `acp' config section,
;;;; registers the acp lane and hooks the turn loop.
;;;;
;;;; A turn on acp/<agent> runs in that agent -- Claude Code, Codex, Gemini
;;;; CLI, or any program that speaks ACP (https://agentclientprotocol.com) on
;;;; stdio -- and the session shows its words, thinking, calls and the files
;;;; they changed; its permission asks are the page's question card, and Stop
;;;; cancels its prompt. Every dependency is already in the serving image.

(defsystem "nodecode-acp"
  :description "Claude Code, Codex, Gemini CLI or any ACP agent as a session's agent"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :properties ((:serves (:provider "acp" :name "Agents (Claude Code, Codex, Gemini CLI, any ACP agent)")))
  :serial t
  :components ((:file "package")
               (:file "rpc")
               (:file "agents")
               (:file "turn")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-acp/test"))))

(defsystem "nodecode-acp/test"
  :description "ACP tests. Registered into the shared nodecode.test registry; RUN-ACP-TESTS filters by the ACP-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-acp" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-acp-tests)))
