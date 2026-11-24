;;;; package.lisp --- PR Studio's one package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed PR-STUDIO so an eval form reads
;;;; (pr-studio:show 12 :repo "o/r"). It is used QUALIFIED only: INBOX,
;;;; SHOW, DIFF, THREADS, COMMENT and REVIEW are the six verbs, exported so
;;;; the operator can call them from eval as well as the model through its
;;;; tools.

(defpackage #:nodecode-pr-studio
  (:use #:cl)
  (:nicknames #:pr-studio)
  (:export
   ;; the cell's own
   #:start-cell #:stop-cell #:setting #:pr-studio-error
   ;; model-facing vocabulary
   #:inbox #:show #:diff #:threads #:comment #:review))

(in-package #:nodecode-pr-studio)
