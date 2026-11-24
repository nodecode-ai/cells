;;;; package.lisp --- the HANDOFF package: the store, the tool, the verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed HANDOFF so an eval form reads (handoff:handoff-text).
;;;; Every exported verb answers a STRING and every refusal is HANDOFF-ERROR,
;;;; which the eval snippet renders as "ERROR: HANDOFF-ERROR: <detail>".

(defpackage #:nodecode-handoff
  (:use #:cl)
  (:nicknames #:handoff)
  (:export
   ;; verbs, every one a string
   #:handoff-text #:handoffs-text
   ;; the one condition
   #:handoff-error))
