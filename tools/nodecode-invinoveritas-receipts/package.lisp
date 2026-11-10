;;;; package.lisp --- the INVINOVERITAS-RECEIPTS package: three verbs, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed INVINOVERITAS-RECEIPTS so an eval form reads
;;;; (invinoveritas-receipts:issue ...). Every exported verb answers a string
;;;; and every refusal is INVINOVERITAS-RECEIPTS-ERROR, rendered by the eval
;;;; snippet as "ERROR: INVINOVERITAS-RECEIPTS-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `invinoveritas-receipts' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :api-url).

(defpackage #:nodecode-invinoveritas-receipts
  (:use #:cl)
  (:nicknames #:invinoveritas-receipts)
  (:export
   ;; verbs, every one a string
   #:issue #:verify #:check
   ;; data, so a layer can reword it
   #:+question+ #:+options+ #:+verify-note+
   ;; the one condition
   #:invinoveritas-receipts-error))

(in-package #:nodecode-invinoveritas-receipts)

(nlk:define-peripheral invinoveritas-receipts :not-running t)
