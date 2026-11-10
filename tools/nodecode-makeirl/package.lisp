;;;; package.lisp --- the MAKEIRL package: MakeIRL's tools and /makeirl.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed MAKEIRL so an eval form reads (makeirl:...).
;;;; Every refusal the model sees is MAKEIRL-ERROR. The settings the cell runs
;;;; on derive from the one `makeirl' section declaration in cell.lisp
;;;; (NLE:DEFINE-CELL), read with (SETTING :access-token).

(defpackage #:nodecode-makeirl
  (:use #:cl)
  (:nicknames #:makeirl)
  (:export
   ;; the two tools the model calls
   #:tools-tool #:call-tool
   ;; the slash command
   #:makeirl-command
   ;; the client, for a layer that wants MakeIRL from eval
   #:server-tools #:call
   ;; the one condition
   #:makeirl-error))

(in-package #:nodecode-makeirl)

(nlk:define-peripheral makeirl :not-running t)
