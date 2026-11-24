;;;; package.lisp --- the NODECODE-MINIMAX-IMAGE package: one tool, one
;;;; command, their condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it, and nicknamed MINIMAX-IMAGE so an eval form reads
;;;; (minimax-image:generate-image "a red fox"). The tool is what the model
;;;; calls; a person can call the same function from eval, or type
;;;; /minimax-image.

(defpackage #:nodecode-minimax-image
  (:use #:cl)
  (:nicknames #:minimax-image)
  (:export #:start-cell
           #:stop-cell
           #:generate-image
           #:minimax-image-command
           #:minimax-image-error
           #:minimax-image-error-detail))

(in-package #:nodecode-minimax-image)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and MINIMAX-IMAGE-ERROR, which a request that cannot go
;;; on signals, come from here: every refusal starts as one of these, and the
;;; tool answers it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral minimax-image :not-running t)
