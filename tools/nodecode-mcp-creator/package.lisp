;;;; package.lisp --- the mcp-creator package: model-facing vocabulary, and the seam tests stub.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed MCP-CREATOR so an eval form reads
;;;; (mcp-creator:drift "files"). The model reaches the cell through the
;;;; mcp_creator_* tools; a person through /mcp-creator and (help :mcp-creator).
;;;;
;;;; *CONNECT* is the one door to a server: a function of a server plist
;;;; answering a connection, so every test stubs the transport without a
;;;; process. *SERVERS* is the mcp.servers map read once at start.

(defpackage #:nodecode-mcp-creator
  (:use #:cl)
  (:nicknames #:mcp-creator)
  (:export
   ;; the cell entry (DEFINE-CELL exports START-CELL too)
   #:stop-cell
   ;; the seam and the world it reads
   #:*connect* #:*servers* #:*config* #:*data-dir* #:*now*
   #:setting #:data-path #:build-path
   ;; the verbs behind the tools, callable from eval
   #:servers #:introspect #:mine #:convert #:scaffold #:validate-package
   #:install #:drift #:review
   ;; the nine tool bodies, callable as they are from eval and the tests
   #:run-servers #:run-introspect #:run-mine #:run-convert #:run-scaffold
   #:run-validate #:run-install #:run-drift #:run-review
   ;; conditions
   #:mcp-creator-error #:mcp-creator-error-detail #:fail))

(in-package #:nodecode-mcp-creator)

;; The condition, FAIL, the settings variable and RUNNING-SETTINGS, declared
;; once: DEFINE-CELL's SETTING reads through them.
(nlk:define-peripheral mcp-creator :not-running t)
