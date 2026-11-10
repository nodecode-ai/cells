;;;; package.lisp --- the QRBLOX package: the MCP bridge and its tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed QRBLOX so an eval form reads
;;;; (qrblox:codes) and (qrblox:call "qrblox.ping").

(defpackage #:nodecode-qrblox
  (:use #:cl)
  (:nicknames #:qrblox)
  (:export
   ;; the seven verbs, each answering the text a model reads
   #:ping #:codes #:create-code #:audio #:menu #:scans #:call
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:qrblox-error #:qrblox-error-detail))

(in-package #:nodecode-qrblox)

;;; QRBLOX-ERROR, its detail reader, FAIL, the settings variable *QRBLOX*, the
;;; not-running refusal and SETTING all come from here; cell.lisp declares the
;;; section that fills them in.
(nlk:define-peripheral qrblox :not-running t)
