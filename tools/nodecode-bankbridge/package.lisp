;;;; package.lisp --- the NODECODE-BANKBRIDGE package: one client for BankBridge's MCP server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, nicknamed BANKBRIDGE so an eval form reads
;;;; (bankbridge:accounts) or (bankbridge:call "get_spending_summary" ...).

(defpackage #:nodecode-bankbridge
  (:use #:cl)
  (:nicknames #:bankbridge)
  (:export #:start-cell
           #:stop-cell
           #:call
           #:status
           #:running-settings
           #:bankbridge-error #:bankbridge-error-detail))

(in-package #:nodecode-bankbridge)

;; BANKBRIDGE-ERROR, its detail reader, FAIL, the settings variable and the
;; not-running refusal come from here; cell.lisp declares the section that
;; fills them in.
(nlk:define-peripheral bankbridge :not-running t)
