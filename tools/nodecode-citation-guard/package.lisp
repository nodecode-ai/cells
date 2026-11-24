;;;; package.lisp --- what this cell's own tests and its README may name.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-citation-guard
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:setting
           #:extract-citations #:citations-from #:check-all #:report-text
           #:check-citation #:check-raw #:check-status #:check-issues
           #:status-label #:problem-p #:resolve-path #:read-document
           #:*client* #:make-client
           #:verify-citations-text #:verify-citations-max-items
           #:verify-citations-check-retractions))
