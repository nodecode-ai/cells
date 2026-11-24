;;;; package.lisp --- the FASHION-DESIGN package: ten verbs and their data.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed FASHION-DESIGN so an eval form reads
;;;; (fashion-design:tech-pack ...). Every exported verb takes the contract
;;;; record its tool receives and answers one JSON string, so a caller from
;;;; eval and a tool call from the model run the same code.

(defpackage #:nodecode-fashion-design
  (:use #:cl)
  (:nicknames #:fashion-design)
  (:export
   ;; the ten verbs, each taking the record its tool declared
   #:design-brief #:tech-pack #:size-sheet #:bill-of-materials #:fabric-yardage
   #:cost-sheet #:construction-guide #:color-palette #:measurement-chart
   #:collection-plan
   ;; data, so a layer can reword it
   #:+verbs+ #:*manual*))
