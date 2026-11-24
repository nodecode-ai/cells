;;;; package.lisp --- the town's package, and what an eval form drives.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed TOWN so an eval form reads (town:snapshot).
;;;; START-CELL is defined and exported by NLE:DEFINE-CELL in cell.lisp; the
;;;; settings this cell runs on are not declared here either -- they derive
;;;; from the one `town' section declaration there, read with (SETTING :salt),
;;;; and *TOWN* is the variable they land in.

(defpackage #:nodecode-town
  (:use #:cl)
  (:nicknames #:town)
  (:export
   ;; what a test or an eval form drives
   #:observe-frame #:observe-tool #:record-frame #:record-tool
   #:run-slash #:snapshot #:snapshot-route #:serve-page #:redirect-to-page
   #:residents #:journal #:received #:reset-town #:pseudonym
   #:place-for-tool #:safe-tool #:town-text #:page-text
   #:*residents-table* #:*journal* #:*received* #:*last-error* #:*last-session*))
