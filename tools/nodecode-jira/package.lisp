;;;; package.lisp --- the NODECODE-JIRA package: the tools and their wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it. No nickname: the package, the slash command and the
;;;; manual topic all carry the folder's own name.

(defpackage #:nodecode-jira
  (:use #:cl)
  (:export #:start-cell
           #:stop-cell
           #:assigned-issues #:search-issues #:issue-detail #:issue-transitions
           #:run-command
           #:running-settings
           #:jira-error #:jira-error-detail))

(in-package #:nodecode-jira)

;;; JIRA-ERROR, its detail reader, FAIL, the settings variable, the
;;; not-running refusal and SETTING all come from here; cell.lisp declares
;;; the section that fills them in.
(nlk:define-peripheral jira :not-running t)
