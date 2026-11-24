;;;; package.lisp --- the MOCKREVIEW package: five verbs, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed MOCKREVIEW so an eval form reads
;;;; (mockreview:feedback "r-1"). Every exported verb answers a STRING and
;;;; every refusal is MOCK-REVIEW-ERROR, rendered by the eval snippet as
;;;; "ERROR: MOCK-REVIEW-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `mock-review' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with
;;;; (SETTING :DIRECTORY).

(defpackage #:nodecode-mock-review
  (:use #:cl)
  (:nicknames #:mockreview)
  (:export
   ;; verbs, every one a string
   #:create #:add-round #:feedback #:ack #:list-reviews
   ;; the board's URL, so a layer can print it
   #:review-url
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:mock-review-error))

(in-package #:nodecode-mock-review)

(nlk:define-peripheral mock-review :not-running t :lock "mock-review")
