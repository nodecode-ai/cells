;;;; package.lisp --- the NODECODE-FXMACRODATA package: the tools and their condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, nicknamed FXMACRODATA so an eval form reads
;;;; (fxmacrodata:call-tool "fxmd_data_catalogue" ...).

(defpackage #:nodecode-fxmacrodata
  (:use #:cl)
  (:nicknames #:fxmacrodata)
  (:export #:fxmacrodata-error #:fxmacrodata-error-detail
           #:call-tool #:tool-names #:api-key #:base-url #:mcp-url #:usd-brief))

(in-package #:nodecode-fxmacrodata)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last, and so do the tools' registrations. FAIL, which a request that
;;; cannot go on signals, FXMACRODATA-ERROR, and the running settings variable
;;; *FXMACRODATA* come from here, so api.lisp can use them before the
;;; declaration loads.
(nlk:define-peripheral fxmacrodata :not-running t)

;;; --- the operation table, filled by operations.lisp --------------------------------

(defstruct (operation (:constructor make-operation
                                    (&key name method path description parameters schema mcp-name)))
  "One row of operations.lisp: what the provider is shown, and where the call goes."
  (name "" :type string)
  (method :get)
  (path "/" :type string)
  (description "" :type string)
  (parameters '() :type list)
  schema
  mcp-name)

(defun operation (&key name method (path "/") (description "") parameters schema mcp-name)
  "One operation, its JSON-Schema read once from the JSON text operations.lisp carries."
  (make-operation :name name :method method :path path :description description
                  :parameters parameters
                  :schema (and schema (nlk:decode-json schema))
                  :mcp-name mcp-name))


(defparameter +operations+ '()
  "Every operation the original registered; operations.lisp fills it at load.")

(defparameter +snapshot-schema+ "{}"
  "Indicator history without its paging members; operations.lisp fills it at load.")

(defparameter +brief-schema+ "{}"
  "The USD brief's one optional member; operations.lisp fills it at load.")
