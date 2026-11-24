;;;; package.lisp --- the EYES package: one verb over the ffmpeg this machine has.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of Agenziamaia's `eyes' plugin for Hermes Agent: a tool result
;;;; that names a freshly written image, video or sound file gets a measured
;;;; note appended to it before the model reads it. See NOTICE.
;;;;
;;;; The package is the folder's name, nicknamed EYES so an eval form reads
;;;; (eyes:measure "out.mp4"). Every refusal is EYES-ERROR, rendered by the
;;;; eval snippet as "ERROR: EYES-ERROR: <detail>".

(defpackage #:nodecode-eyes
  (:use #:cl)
  (:nicknames #:eyes)
  (:export
   ;; the one verb: a file in, the measured note out
   #:measure
   ;; data, so a layer can reword what the model reads
   #:*framing*
   ;; the one condition
   #:eyes-error))

(in-package #:nodecode-eyes)

(nlk:define-peripheral eyes :not-running t)
