;;;; package.lisp --- the JOB-SEARCH package: two verbs, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed JOB-SEARCH so an eval form reads
;;;; (job-search:find-postings :keywords "senior designer") and
;;;; (job-search:posting :posting-id "jp_x" :source-url "..." :source "ashby").
;;;; Every exported verb answers a STRING and every refusal is
;;;; JOB-SEARCH-ERROR, rendered by the eval snippet as
;;;; "ERROR: JOB-SEARCH-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `job-search' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :command).

(defpackage #:nodecode-job-search
  (:use #:cl)
  (:nicknames #:job-search)
  (:export
   ;; verbs, every one a string
   #:find-postings #:posting
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:job-search-error))

(in-package #:nodecode-job-search)

(nlk:define-peripheral job-search :not-running t)
