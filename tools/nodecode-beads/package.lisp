;;;; package.lisp --- the BEADS package: six verbs over the bd CLI.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed BEADS so an eval form reads
;;;; (beads:frontier "/work/project"), (beads:show "/work/project" "bd-3"),
;;;; (beads:claim ...). Every exported verb answers a STRING -- what the model
;;;; or the operator reads -- and every refusal is BEADS-ERROR, rendered by the
;;;; eval snippet as "ERROR: BEADS-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `beads' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :bd),
;;;; (SETTING :store), (SETTING :actor) and (SETTING :timeout-seconds).

(defpackage #:nodecode-beads
  (:use #:cl)
  (:nicknames #:beads)
  (:export
   ;; verbs, every one a string
   #:smoke #:frontier #:show #:claim #:update #:comment
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:beads-error))

(in-package #:nodecode-beads)

(nlk:define-peripheral beads :not-running t)
