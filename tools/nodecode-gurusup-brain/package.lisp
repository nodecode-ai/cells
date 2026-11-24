;;;; package.lisp --- the NODECODE-GURUSUP-BRAIN package: what the model and the operator reach.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed GURUSUP-BRAIN so an eval form reads
;;;; (gurusup-brain:call "search" ...). The three tools carry the server's
;;;; own prefix under the cell's name: gurusup_brain_init,
;;;; gurusup_brain_tools, gurusup_brain_call.

(defpackage #:nodecode-gurusup-brain
  (:use #:cl)
  (:nicknames #:gurusup-brain)
  (:export #:init #:tool-list #:call #:gurusup-brain-error))

(in-package #:nodecode-gurusup-brain)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; wire.lisp and tools.lisp read the section through it.
;;; FAIL and GURUSUP-BRAIN-ERROR, which a refused call signals, and the
;;; running settings, whose absence refuses, come from here too.
(nlk:define-peripheral gurusup-brain :not-running t)
