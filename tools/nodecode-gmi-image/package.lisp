;;;; package.lisp --- the NODECODE-GMI-IMAGE package: one tool, one command,
;;;; their condition.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it, and nicknamed GMI-IMAGE so an eval form reads
;;;; (gmi-image:generate-image "a red fox"). The tool is what the model
;;;; calls; a person can call the same function from eval, or type
;;;; /gmi-image.

(defpackage #:nodecode-gmi-image
  (:use #:cl)
  (:nicknames #:gmi-image)
  (:export #:start-cell
           #:stop-cell
           #:generate-image
           #:gmi-image-command
           #:gmi-image-error
           #:gmi-image-error-detail))

(in-package #:nodecode-gmi-image)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and GMI-IMAGE-ERROR, which a request that cannot go on
;;; signals, come from here: every refusal starts as one of these, and the
;;; tool answers it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral gmi-image :not-running t)
