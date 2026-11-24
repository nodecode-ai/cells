;;;; package.lisp --- the NVIDIA-BROADCAST package: ten tools, their condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it, and nicknamed NVIDIA-BROADCAST so an eval form
;;;; reads (nvidia-broadcast:list-devices ...). The ten verbs are tools the
;;;; model calls; a person can call the same functions from eval.

(defpackage #:nodecode-nvidia-broadcast
  (:use #:cl)
  (:nicknames #:nvidia-broadcast)
  (:export #:start-cell
           #:stop-cell
           #:broadcast-state #:describe-effects #:set-effects
           #:list-devices #:set-active-device #:set-camera-resolution
           #:submit-file #:file-jobs #:wait-for-update #:control-jobs
           #:call-tool
           #:nvidia-broadcast-error #:nvidia-broadcast-error-detail))

(in-package #:nodecode-nvidia-broadcast)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and NVIDIA-BROADCAST-ERROR, which a request that cannot
;;; go on signals, come from here: every refusal starts as one of these, and
;;; the tools answer it as the call's failure rather than as an unhandled
;;; error.
(nlk:define-peripheral nvidia-broadcast :not-running t)
