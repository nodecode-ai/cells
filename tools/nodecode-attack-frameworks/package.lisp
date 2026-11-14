;;;; package.lisp --- the cell's one package.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-attack-frameworks
  (:use #:cl)
  (:export #:start-cell #:stop-cell
           #:frameworks #:framework-name #:framework-repo #:framework-url
           #:framework-stars #:framework-licence #:framework-language
           #:framework-pushed #:framework-what #:framework-authorization
           #:framework-evidence #:framework-hunt
           #:frameworks-matching #:framework-by-name #:framework-block
           #:framework-list-text #:hunt-text #:summary-text #:source-line
           #:star-total #:star-text
           #:+frameworks+ #:+source+ #:+captured+ #:+thesis+
           #:+revocation+ #:+takeaway+ #:+rot+ #:+tell-text+))
