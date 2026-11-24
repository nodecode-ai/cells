;;;; package.lisp --- the NODECODE-EQUIPDASH package: a shop's day, through its MCP server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed EQUIPDASH so an eval form reads
;;;; (equipdash:list-tools).

(defpackage #:nodecode-equipdash
  (:use #:cl)
  (:nicknames #:equipdash)
  (:export #:start-cell
           #:stop-cell
           #:running-settings
           #:equipdash-error
           #:equipdash-error-detail
           #:equipdash_tools
           #:equipdash_call
           #:equipdash_day
           #:list-tools
           #:call-tool
           #:day
           #:run-command))

(in-package #:nodecode-equipdash)

;;; EQUIPDASH-ERROR, its detail reader, FAIL, the settings variable, the
;;; not-running refusal and SETTING all come from here; cell.lisp declares
;;; the section that fills them in.
(nlk:define-peripheral equipdash :not-running t)
