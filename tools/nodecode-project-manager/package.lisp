;;;; package.lisp --- the PROJECTS package: the verbs, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed PROJECTS so an eval form reads (projects:project).
;;;; Every exported verb answers a STRING and every refusal is
;;;; PROJECT-MANAGER-ERROR, rendered by the eval snippet as
;;;; "ERROR: PROJECT-MANAGER-ERROR: <detail>".
;;;;
;;;; The settings the cell runs on derive from the one `project-manager'
;;;; section declaration in cell.lisp (NLE:DEFINE-CELL), read with
;;;; (SETTING :WATCH-PATHS) and its siblings.

(defpackage #:nodecode-project-manager
  (:use #:cl)
  (:nicknames #:projects)
  (:export
   ;; verbs, every one a string
   #:projects #:project #:branches #:commits #:watch-paths
   ;; the one seam a test replaces, and the words
   #:*git* #:*primer*
   ;; the one condition
   #:project-manager-error))

(in-package #:nodecode-project-manager)

(nlk:define-peripheral project-manager :not-running t)
