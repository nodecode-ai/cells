;;;; package.lisp --- nodecode-git-hook's one package.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-git-hook
  (:use #:cl)
  (:nicknames #:git-hook)
  (:export #:start-cell #:stop-cell #:setting #:git-hook-error))
