;;;; package.lisp --- the CARTOGRAPH package: two verbs, the one condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed CARTOGRAPH so an eval form reads
;;;; (cartograph:analyze "/path/to/repo"). Every exported verb answers a
;;;; STRING and every refusal is CARTOGRAPH-ERROR, rendered by the eval
;;;; snippet as "ERROR: CARTOGRAPH-ERROR: <detail>".
;;;;
;;;; The settings these verbs run on derive from the one `cartograph'
;;;; section declaration in cell.lisp (NLE:DEFINE-CELL), read with
;;;; (SETTING :max-files) and the rest.

(defpackage #:nodecode-cartograph
  (:use #:cl)
  (:nicknames #:cartograph)
  (:export
   ;; verbs, every one a string
   #:analyze #:context
   ;; the one condition
   #:cartograph-error))

(in-package #:nodecode-cartograph)

(nlk:define-peripheral cartograph :not-running t)
