;;;; package.lisp --- the one package, and what the tests read from it.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-redteam-gates
  (:use #:cl)
  (:nicknames #:redteam-gates)
  (:export #:+gate-modes+
           #:find-mode #:find-gate #:mode-ids #:gate-ids
           #:markdown-rows #:hex-present-p #:run-check #:run-gate
           #:gate-log-path #:append-gate-log #:log-rows
           #:verdict-text #:gates-list-text #:modes-text
           #:run-stage-gate #:run-gates-list #:run-slash
           #:start-cell #:stop-cell))
