;;;; package.lisp --- the NODECODE-MYSWARM package: agents.md, read and shared.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of tomkubik/hermes-myswarm (see NOTICE). One package,
;;;; named after the system, which is how the folder loader finds START-CELL in
;;;; it, and nicknamed MYSWARM so an eval form reads (myswarm:list-agents).
;;;;
;;;; The pure half -- PARSING, the abbreviations, the diagrams, the scrubber --
;;;; is exported because it is where the behaviour is decided and what the tests
;;;; read; the tools and the command wrap it.

(defpackage #:nodecode-myswarm
  (:use #:cl)
  (:nicknames #:myswarm)
  (:export
   ;; reading an agents.md
   #:parse-agents #:agent #:agent-name #:agent-persona #:agent-goal
   #:agent-role #:agent-delegates #:agent-tools
   #:find-agents-file #:swarm-markdown
   ;; the four ways of showing one
   #:list-text #:topology-text #:copy-paste-text
   ;; the scrubber, and the import's reading of a stranger's file
   #:scrub-pii #:scan-text
   ;; what a user shares, and what they take in
   #:list-agents #:topology #:export-copy #:import-file #:revert #:status
   ;; the cell's own verbs
   #:run-slash #:start-cell #:stop-cell
   ;; the one condition
   #:myswarm-error))
~%
(in-package #:nodecode-myswarm)

;;; SETTING, FAIL and RUNNING-SETTINGS come from the NLE:DEFINE-CELL
;;; declaration in cell.lisp, which loads last; the files between read the
;;; section through SETTING and refuse through FAIL.
(nlk:define-peripheral myswarm :not-running t)
