;;;; package.lisp --- the NODECODE-AGENTSTATUS-INDEX package: the MCP bridge and its tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed AGENTSTATUS-INDEX so an eval form reads
;;;; (agentstatus-index:search-index "...").

(defpackage #:nodecode-agentstatus-index
  (:use #:cl)
  (:nicknames #:agentstatus-index)
  (:export #:start-cell
           #:stop-cell
           #:search-index #:recommend #:record #:compare #:sections
           #:call-indexed #:tools-list
           #:running-settings
           #:agentstatus-index-error #:agentstatus-index-error-detail))

(in-package #:nodecode-agentstatus-index)

;;; AGENTSTATUS-INDEX-ERROR, its detail reader, FAIL, the settings variable,
;;; the not-running refusal and SETTING all come from here; cell.lisp declares
;;; the section that fills them in.
(nlk:define-peripheral agentstatus-index :not-running t)
