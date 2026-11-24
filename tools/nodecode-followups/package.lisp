;;;; package.lisp --- the FOLLOWUPS package: the board's verbs, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed FOLLOWUPS so an eval form reads
;;;; (followups:add :subject "the sore throat" :why "care" :at "+18h").
;;;; Every exported verb answers a STRING and every refusal is
;;;; FOLLOWUPS-ERROR, rendered by the eval snippet as
;;;; "ERROR: FOLLOWUPS-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `followups' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :max-items).

(defpackage #:nodecode-followups
  (:use #:cl)
  (:nicknames #:followups)
  (:export
   ;; the board, every verb a string
   #:add #:board #:due #:checked #:retire #:outcome #:digest #:reminder
   ;; data, so a layer can reword it
   #:+categories+ #:*framing* #:*primer*
   ;; the one condition
   #:followups-error))

(in-package #:nodecode-followups)

(nlk:define-peripheral followups :not-running t :lock "followups" :recursive t)
