;;;; package.lisp --- the NODECODE-DAVINCI-RESOLVE package: what the model calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed RESOLVE so an eval form reads
;;;; (resolve:status). Every failure the model sees is RESOLVE-ERROR, which
;;;; the eval snippet renders as "ERROR: RESOLVE-ERROR: <detail>".

(defpackage #:nodecode-davinci-resolve
  (:use #:cl)
  (:nicknames #:resolve)
  (:export
   ;; model-facing vocabulary: the live verbs, and the two pure ones
   #:status #:import-media #:add-marker #:render
   #:scan-media #:write-fcpxml #:write-marker-csv
   ;; the one condition
   #:resolve-error))

(in-package #:nodecode-davinci-resolve)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; bridge.lisp and export.lisp read the section through it.
(nlk:define-peripheral davinci-resolve :not-running t)
