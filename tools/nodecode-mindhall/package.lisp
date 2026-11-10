;;;; package.lisp --- the MINDHALL package: five verbs, the primer, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed MINDHALL so an eval form reads
;;;; (mindhall:add-note ...). Every verb answers a STRING and every refusal
;;;; is MINDHALL-ERROR, rendered by the eval snippet as
;;;; "ERROR: MINDHALL-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `mindhall' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :store).

(defpackage #:nodecode-mindhall
  (:use #:cl)
  (:nicknames #:mindhall)
  (:export
   ;; the verbs, every one a string
   #:read-mind #:add-note #:update-work #:set-date #:remove-entry #:summary-text
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:mindhall-error))

(in-package #:nodecode-mindhall)

(nlk:define-peripheral mindhall :not-running t)
