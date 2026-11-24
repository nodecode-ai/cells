;;;; package.lisp --- the BEAM package: three model verbs, a command and their condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed BEAM so an eval form reads (beam:...).

(defpackage #:nodecode-beam
  (:use #:cl)
  (:nicknames #:beam)
  (:export #:beam-error))

(in-package #:nodecode-beam)

;;; FAIL and BEAM-ERROR, which a call that cannot go on signals: the key, the
;;; network, the API's own answer, the CLI's exit. The verbs refuse while the
;;; cell is not running.
(nlk:define-peripheral beam :not-running t)
