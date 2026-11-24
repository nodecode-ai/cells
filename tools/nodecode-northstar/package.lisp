;;;; package.lisp --- the NORTHSTAR package: the audit, the findings, the refusal.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed NORTHSTAR so an eval form reads
;;;; (northstar:audit :scope "project"). Every exported verb answers a string
;;;; and every refusal is NORTHSTAR-ERROR, rendered by the eval snippet as
;;;; "ERROR: NORTHSTAR-ERROR: <detail>".

(defpackage #:nodecode-northstar
  (:use #:cl)
  (:nicknames #:northstar)
  (:export
   ;; verbs, every one a string
   #:audit #:status
   ;; a finding, read-only, so a test or a layer can look at one
   #:finding #:finding-category #:finding-severity #:finding-confidence
   #:finding-file #:finding-line #:finding-evidence #:finding-explanation
   #:finding-suggestion
   ;; data, so a layer can reword it
   #:*scopes* #:*project-names* #:*home-names*
   ;; the one condition
   #:northstar-error #:northstar-error-detail))

(in-package #:nodecode-northstar)

(nlk:define-peripheral northstar :not-running t)
