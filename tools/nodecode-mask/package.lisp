;;;; package.lisp --- the cell's one package.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0

(defpackage #:nodecode-mask
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:setting
           #:mask-text #:mask-hits #:detect-hits #:resolve-overlaps
           #:mask-messages #:mask-wire #:restore-text #:reset-stripper
           #:decode-mask-test-request #:mask-test-request-text
           #:found-summary #:status-text))
