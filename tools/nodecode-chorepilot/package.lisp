;;;; package.lisp --- the CHOREPILOT package: the house's tools and /chorepilot.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; One package, nicknamed CHOREPILOT so an eval form reads
;;;; (chorepilot:...). Every refusal the model sees is CHOREPILOT-ERROR.
;;;; The settings the cell runs on derive from the one `chorepilot' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :url).

(defpackage #:nodecode-chorepilot
  (:use #:cl)
  (:nicknames #:chorepilot)
  (:export
   ;; the seven tools the house answers, as the model calls them
   #:status-tool #:chores-tool #:who-has-tool #:done-tool
   #:whoami-tool #:tools-tool #:call-tool
   ;; the slash command
   #:chorepilot-command
   ;; the client, for a layer that wants the house from eval
   #:house-tools #:call-house
   ;; the one condition
   #:chorepilot-error))

(in-package #:nodecode-chorepilot)

(nlk:define-peripheral chorepilot :not-running t)
