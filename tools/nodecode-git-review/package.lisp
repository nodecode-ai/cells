;;;; package.lisp --- nodecode-git-review's one package.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-git-review
  (:use #:cl)
  (:nicknames #:git-review)
  (:export #:start-cell #:stop-cell #:setting #:review #:git-review-error))
