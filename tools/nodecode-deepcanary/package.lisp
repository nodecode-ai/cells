;;;; package.lisp --- the DEEPCANARY package: the inbox's verbs, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed DEEPCANARY so an eval form reads
;;;; (deepcanary:raise "TOOL_FAILURE_LOOP" :authority "runtime" ...).
;;;; Every exported verb answers a STRING and every refusal is
;;;; DEEPCANARY-ERROR, rendered by the eval snippet as
;;;; "ERROR: DEEPCANARY-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `deepcanary' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :LEVEL).

(defpackage #:nodecode-deepcanary
  (:use #:cl)
  (:nicknames #:deepcanary)
  (:export
   ;; the inbox, every verb a string
   #:raise #:inbox #:status #:acknowledge #:snooze #:mute #:feedback #:explain
   #:preview #:reminder
   ;; data, so a layer can reword it
   #:+reasons+ #:+authorities+ #:+levels+ #:*primer* #:*framing*
   ;; the one condition
   #:deepcanary-error))

(in-package #:nodecode-deepcanary)

(nlk:define-peripheral deepcanary :not-running t :lock "deepcanary" :recursive t)
