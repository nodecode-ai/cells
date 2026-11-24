;;;; package.lisp --- the WATCHLINE package: the verbs, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed WATCHLINE so an eval form reads
;;;; (watchline:pending). Every exported verb answers a STRING and every
;;;; refusal is WATCHLINE-ERROR, rendered by the eval snippet as
;;;; "ERROR: WATCHLINE-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `watchline' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :channel-id).

(defpackage #:nodecode-watchline
  (:use #:cl)
  (:nicknames #:watchline)
  (:export
   ;; verbs, every one a string
   #:pending
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:watchline-error))

(in-package #:nodecode-watchline)

(nlk:define-peripheral watchline :not-running t)
