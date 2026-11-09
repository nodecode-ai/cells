;;;; package.lisp --- the NODECODE-BULKPUBLISH package: three verbs, two tools, one condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; BulkPublish is fifteen social networks behind one API, and the plugin
;;;; this cell rewrites reaches it through BulkPublish's hosted MCP server.
;;;; Every exported verb here answers a STRING a model or the operator reads;
;;;; every refusal is BULKPUBLISH-ERROR, which the eval snippet renders as
;;;; "ERROR: BULKPUBLISH-ERROR: <detail>".

(defpackage #:nodecode-bulkpublish
  (:use #:cl)
  (:nicknames #:bulkpublish)
  (:export
   ;; the verbs, every one a string
   #:tools #:call #:channels
   ;; the one condition
   #:bulkpublish-error))

(in-package #:nodecode-bulkpublish)

;; FAIL and BULKPUBLISH-ERROR, RUNNING-SETTINGS (the settings, or the refusal
;; a verb reads while the cell is not running). The values themselves come
;; from the NLE:DEFINE-CELL declaration in cell.lisp, read with (SETTING
;; :TIMEOUT) and the rest of the section.
(nlk:define-peripheral bulkpublish :not-running t)
