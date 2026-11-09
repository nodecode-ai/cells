;;;; package.lisp --- the WORKFLOWS package: the verbs, the primer, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every exported verb answers a STRING and every refusal is
;;;; WORKFLOWS-ERROR, rendered by the eval snippet as "ERROR: WORKFLOWS-ERROR:
;;;; <detail>". The settings this cell runs on derive from the one `workflows'
;;;; section declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING ...).

(defpackage #:nodecode-workflows
  (:use #:cl)
  (:nicknames #:workflows)
  (:export
   ;; verbs, every one a string
   #:start #:status #:runs #:stop #:answer #:validate #:show
   ;; the manual, and the one condition
   #:*primer* #:workflows-error))

(in-package #:nodecode-workflows)

(nlk:define-peripheral workflows :not-running t :lock "workflows")
