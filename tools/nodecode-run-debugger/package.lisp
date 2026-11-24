;;;; package.lisp --- the RUNDEBUG package: the verbs, the condition, the words.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; One package, nicknamed RUNDEBUG so an eval form reads
;;;; (rundebug:timeline "run-1"): the same calls the tools make, answering
;;;; the same text. Every exported verb answers a STRING, or signals
;;;; RUN-DEBUGGER-ERROR with a detail the model can read.
;;;;
;;;; The settings this cell runs on derive from the one `run_debugger'
;;;; section declaration in cell.lisp (NLE:DEFINE-CELL), read with
;;;; (SETTING :max-bytes) and its neighbours.

(defpackage #:nodecode-run-debugger
  (:use #:cl)
  (:nicknames #:rundebug)
  (:export
   ;; the verbs, every one a string
   #:import-trace #:timeline #:explain #:replay #:status #:report
   ;; the run list, for a caller that wants the plists
   #:runs #:run
   ;; data, so a layer can reword it
   #:*primer* #:*rules*
   ;; the one condition
   #:run-debugger-error))

(in-package #:nodecode-run-debugger)

(nlk:define-peripheral run-debugger :not-running t)
