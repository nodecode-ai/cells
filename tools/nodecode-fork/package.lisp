;;;; package.lisp --- the FORK package: one verb, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed FORK so an eval form reads (fork:run ...). The
;;;; verb answers data, one plist a task; every refusal is FORK-ERROR,
;;;; rendered by the eval snippet as "ERROR: FORK-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `fork' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :seconds).

(defpackage #:nodecode-fork
  (:use #:cl)
  (:nicknames #:fork)
  (:export
   ;; the verb
   #:run
   ;; data, so a layer can reword it
   #:*task-input*
   #:*earlier-input*
   #:*keep-input*
   #:*bisect-input*
   ;; the one condition
   #:fork-error))

(in-package #:nodecode-fork)

(nlk:define-peripheral fork :not-running t)
