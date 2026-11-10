;;;; package.lisp --- one package, named after the system.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The loader finds START-CELL as the exported symbol of package
;;;; NODECODE-CRON-PLAN, and every contribution is filed under that name
;;;; downcased, which is this folder's name.

(defpackage #:nodecode-cron-plan
  (:use #:cl)
  (:nicknames #:cron-plan)
  (:export #:start-cell #:stop-cell #:plan))
