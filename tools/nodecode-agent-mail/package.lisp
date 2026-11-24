;;;; package.lisp --- the cell's own package, and the vocabulary every file shares.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, as a peripheral's always is: the
;;;; loader finds START-CELL there (NLE:DEFINE-CELL exports it), and every
;;;; contribution is filed under this package's name downcased,
;;;; "nodecode-agent-mail", which is this folder's name.

(defpackage #:nodecode-agent-mail
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:*agent-mail*
           #:agent-mail-error #:agent-mail-error-detail))

(in-package #:nodecode-agent-mail)

(nlk:define-peripheral agent-mail :not-running t)

(defparameter +tool-prefix+ "mail_"
  "Every tool this cell registers starts with it, so a prompt written for the
gateway's own MCP tools still reads right.")

(defparameter +max-body-chars+ 60000
  "A message body longer than this is cut before it reaches the model's
context; the tool says how much was left out.")

(defparameter +default-timeout+ 30
  "Seconds one exchange may take, unless the section says otherwise.")
