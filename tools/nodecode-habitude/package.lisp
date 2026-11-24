;;;; package.lisp --- the HABITUDE package: four verbs, the one condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed HABITUDE so an eval form reads
;;;; (habitude:search-journal "writing routine"). Every exported verb answers
;;;; a STRING and every refusal is HABITUDE-ERROR, rendered by the eval
;;;; snippet as "ERROR: HABITUDE-ERROR: <detail>".
;;;;
;;;; The settings these verbs run on derive from the one `habitude' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :vault)
;;;; and the rest.

(defpackage #:nodecode-habitude
  (:use #:cl)
  (:nicknames #:habitude)
  (:export
   ;; verbs, every one a string
   #:search-journal #:read-excerpt #:index-status #:refresh-index
   ;; the one condition
   #:habitude-error))

(in-package #:nodecode-habitude)

(nlk:define-peripheral habitude :not-running t)
