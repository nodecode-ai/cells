;;;; package.lisp --- the IMAGE-STUDIO package: two tools, one command, one condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed IMAGE-STUDIO so an eval form reads
;;;; (image-studio:generate-image "a red fox in snow"). The tools are what the
;;;; model calls; a person can call the same function from eval, or type
;;;; /image-studio.

(defpackage #:nodecode-image-studio
  (:use #:cl)
  (:nicknames #:image-studio)
  (:export #:start-cell
           #:stop-cell
           #:generate-image
           #:gallery
           #:image-studio-command
           #:image-studio-error
           #:image-studio-error-detail))

(in-package #:nodecode-image-studio)

;;; SETTING, START-CELL and STOP-CELL come from the NLE:DEFINE-CELL
;;; declaration in cell.lisp, which loads last. FAIL and IMAGE-STUDIO-ERROR,
;;; which a request that cannot go on signals, come from here: every refusal
;;; starts as one of these, and a tool answers it as the call's failure
;;; rather than as an unhandled error.
(nlk:define-peripheral image-studio :not-running t)
