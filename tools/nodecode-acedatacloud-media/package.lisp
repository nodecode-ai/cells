;;;; package.lisp --- the NODECODE-ACEDATACLOUD-MEDIA package: five tools, one
;;;; command, and the condition every refusal signals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it. The tools are what the model calls; a person can call
;;;; the same functions from eval, or type /media.

(defpackage #:nodecode-acedatacloud-media
  (:use #:cl)
  (:export #:start-cell
           #:stop-cell
           #:acedatacloud-image
           #:acedatacloud-video
           #:acedatacloud-audio
           #:acedatacloud-task
           #:acedatacloud-lookup
           #:media-command
           #:acedatacloud-media-error
           #:acedatacloud-media-error-detail))

(in-package #:nodecode-acedatacloud-media)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and ACEDATACLOUD-MEDIA-ERROR, which a request that cannot
;;; go on signals, come from here: every refusal starts as one of these, and
;;; the tool answers it as the call's failure rather than as an unhandled
;;; error.
(nlk:define-peripheral acedatacloud-media :not-running t)
