;;;; package.lisp --- the cell's one package.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-security-patches
  (:use #:cl)
  (:nicknames #:security)
  (:export
   ;; the feed
   #:advisories #:advisory #:field #:feed-status #:refresh #:*feed-url*
   #:packaged-entries #:load-feed
   ;; the checks
   #:check #:report #:show #:run-slash
   ;; start (defined by NLE:DEFINE-CELL)
   #:start-cell #:stop-cell))
