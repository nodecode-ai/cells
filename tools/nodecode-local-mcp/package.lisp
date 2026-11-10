;;;; package.lisp --- the cell's one package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system; the loader finds START-CELL as
;;;; its exported symbol, and every contribution is filed under the name
;;;; downcased: nodecode-local-mcp.

(defpackage #:nodecode-local-mcp
  (:use #:cl)
  (:export #:local-mcp-error #:local-mcp-error-detail))

(in-package #:nodecode-local-mcp)

;; NAME-ERROR and FAIL, *LOCAL-MCP* with RUNNING-SETTINGS. No lock: every
;; call is one request in flight on the calling thread, and the connection
;; is only replaced by start and stop, which the gateway runs alone.
(nlk:define-peripheral local-mcp :not-running t)
