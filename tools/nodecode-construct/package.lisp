;;;; package.lisp --- the CONSTRUCT package: the verbs a layer may call, the
;;;; data a layer may reword, and the one condition.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; One package, nicknamed CONSTRUCT so an eval form reads (construct:check-scene
;;;; ...). Every verb answers a string and every refusal is CONSTRUCT-ERROR,
;;;; rendered as "ERROR: CONSTRUCT-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `construct' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :segments).

(defpackage #:nodecode-construct
  (:use #:cl)
  (:nicknames #:construct)
  (:export
   ;; what a layer or a test calls
   #:start-cell #:construct-error
   #:project-dir #:scene-file #:safe-slug-p #:load-scene #:save-scene #:starter-scene
   #:check-scene #:problems-text #:compile-scene #:inspect-text #:scene-summary
   #:export-scene #:build-page #:shape-names #:circle-segments
   ;; data, so a layer can reword it
   #:*primer* #:*starter-help*))

(in-package #:nodecode-construct)

(nlk:define-peripheral construct :not-running t)
