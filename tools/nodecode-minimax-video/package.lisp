;;;; package.lisp --- the NODECODE-MINIMAX-VIDEO package: two tools, one
;;;; command, and the condition every refusal signals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it. The tools are what the model calls; a person can call
;;;; the same functions from eval, or type /video.

(defpackage #:nodecode-minimax-video
  (:use #:cl)
  (:export #:start-cell
           #:stop-cell
           #:generate-video
           #:video-status
           #:video-command
           #:minimax-video-error
           #:minimax-video-error-detail))

(in-package #:nodecode-minimax-video)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and MINIMAX-VIDEO-ERROR, which a request that cannot go on
;;; signals, come from here: every refusal starts as one of these, and the
;;; tool answers it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral minimax-video :not-running t)
