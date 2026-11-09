;;;; package.lisp --- the NODECODE-DASHSCOPE-IMAGE package: one tool, one
;;;; command, their condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it, and nicknamed DASHSCOPE so an eval form reads
;;;; (dashscope:generate-image "a red fox"). The tool is what the model
;;;; calls; a person can call the same function from eval, or type
;;;; /dashscope.

(defpackage #:nodecode-dashscope-image
  (:use #:cl)
  (:nicknames #:dashscope)
  (:export #:start-cell
           #:stop-cell
           #:generate-image
           #:dashscope-command
           #:dashscope-image-error
           #:dashscope-image-error-detail))

(in-package #:nodecode-dashscope-image)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and DASHSCOPE-IMAGE-ERROR, which a request that cannot go
;;; on signals, come from here: every refusal starts as one of these, and the
;;; tool answers it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral dashscope-image :not-running t)
