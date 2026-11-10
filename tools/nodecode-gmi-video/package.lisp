;;;; package.lisp --- the GMI-VIDEO package: two tools' verbs and the section.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed GMI-VIDEO so an eval form reads
;;;; (gmi-video:generate "..."). The exported verbs are the original
;;;; provider's two public answers: a generation, and the models it can pick.

(defpackage #:nodecode-gmi-video
  (:use #:cl)
  (:nicknames #:gmi-video)
  (:export #:generate #:models #:gmi-video-error))

(in-package #:nodecode-gmi-video)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; the wire and the mapping read the section through it. FAIL and
;;; GMI-VIDEO-ERROR, which a request or a job that cannot go on signals, and
;;; RUNNING-SETTINGS, which refuses while the cell is not running, come from
;;; this one form too.
(nlk:define-peripheral gmi-video :not-running t)
