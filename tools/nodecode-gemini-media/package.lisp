;;;; package.lisp --- the GEMINI-MEDIA package: three tools, one command, their condition.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed GEMINI-MEDIA so an eval form reads
;;;; (gemini-media:gemini-image "a red fox"). The tools are what the model
;;;; calls; a person can call the same functions from eval, or type
;;;; /gemini-media.

(defpackage #:nodecode-gemini-media
  (:use #:cl)
  (:nicknames #:gemini-media)
  (:export #:start-cell
           #:stop-cell
           #:gemini-image
           #:gemini-video
           #:list-media-models
           #:media-command
           #:gemini-media-error
           #:gemini-media-error-detail))

(in-package #:nodecode-gemini-media)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and GEMINI-MEDIA-ERROR, which a request that cannot go on
;;; signals, come from here: every refusal starts as one of these, and the
;;; tool answers it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral gemini-media :not-running t)
