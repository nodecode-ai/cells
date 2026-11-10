;;;; package.lisp --- the one package, and what the other files read.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-hotwire
  (:use #:cl)
  (:export #:start-cell #:stop-cell
           #:hotwire-error #:hotwire-error-detail #:hotwire-error-line
           #:run-check #:run-simulate #:run-library #:run-slash
           #:run-explain #:run-bom #:run-tune #:run-yield #:run-export
           #:parse-netlist #:check-design #:bench #+nil #:make-bench
           #:sweep-of #:operating-point-of #:spec-result-text
           #:library-ids #:library-entry))
