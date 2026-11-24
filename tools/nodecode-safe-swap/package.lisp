;;;; package.lisp --- the cell's one package.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-safe-swap
  (:nicknames #:safe-swap)
  (:use #:cl)
  (:export #:plan #:receipts #:undo #:purge #:status
           #:analyse #:rewrite-command #:quarantine-dir
           #:analysis-rewrite #:analysis-skips #:analysis-kinds #:analysis-command
           #:safe-swap-error))

(in-package #:nodecode-safe-swap)

;; The cell's own condition (SAFE-SWAP-ERROR), its FAIL, *SAFE-SWAP*, and the
;; not-running refusal every verb opens with: declared before the files that use them.
(nlk:define-peripheral safe-swap :not-running t)
