;;;; package.lisp --- one package for the corpus and the cell.
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-mediarag
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:setting
           #:search-text #:add-note #:ingest-dir #:add-source #:status-text #:run-command
           #:read-corpus #:write-corpus #:chunk-text #:terms #:store-path #:index-file
           #:corpus-root #:list-files #:http-call))

(in-package #:nodecode-mediarag)

;; The condition, FAIL, *MEDIARAG*, RUNNING-SETTINGS and its refusal: what
;; DEFINE-CELL's SETTING reads and every path here refuses with.
(nlk:define-peripheral mediarag :not-running t)
