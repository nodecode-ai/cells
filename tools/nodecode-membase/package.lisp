;;;; package.lisp --- the Membase package and its one condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed MEMBASE so an eval form reads
;;;; (membase:recall "what did we settle"): every exported verb answers a
;;;; STRING and every refusal is MEMBASE-ERROR, rendered by the tool or the
;;;; eval snippet as "ERROR: MEMBASE-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on are NOT declared here: they derive from
;;;; the one `membase' section declaration in cell.lisp (NLE:DEFINE-CELL),
;;;; read with (SETTING :api-key), and *MEMBASE* below is the variable they
;;;; land in.

(defpackage #:nodecode-membase
  (:use #:cl)
  (:nicknames #:membase)
  (:export
   ;; verbs, every one a string; the three the model calls are also tools
   #:recall #:remember #:profile
   ;; the one condition
   #:membase-error))

(in-package #:nodecode-membase)

(nlk:define-peripheral membase :not-running t)
