;;;; package.lisp --- the MARKIFACT package: the model's cell and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed MARKIFACT so an eval form reads
;;;; (markifact:call-tool "find_operations" ...).

(in-package #:cl-user)

(defpackage #:nodecode-markifact
  (:use #:cl)
  (:nicknames #:markifact)
  (:export #:call-tool #:tools #:markifact-error #:sign-in))

(in-package #:nodecode-markifact)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which loads
;;; last; wire.lisp and signin.lisp read the section through it. FAIL, the
;;; condition, *MARKIFACT* and RUNNING-SETTINGS come from here.
(nlk:define-peripheral markifact :not-running t)

(defparameter +default-endpoint+ "https://api.markifact.com/mcp"
  "The vendor's one endpoint: every platform API call happens server-side of it.")

(defparameter +provider+ "markifact"
  "The id the sign-in is kept under: auth.json's oauth_tokens.markifact.")
