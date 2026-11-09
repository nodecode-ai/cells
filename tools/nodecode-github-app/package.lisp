;;;; package.lisp --- the GITHUB-APP package: seven tools, one command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed GITHUB-APP so an eval form reads
;;;; (github-app:list-issues ...). The seven verbs are tools the model calls,
;;;; under the names the original plugin gave them; a person can call the same
;;;; functions from eval.

(defpackage #:nodecode-github-app
  (:use #:cl)
  (:nicknames #:github-app)
  (:export #:start-cell
           #:stop-cell
           #:verify-identity #:create-issue #:comment-issue
           #:list-issues #:read-issue
           #:review-pull-request #:merge-pull-request
           #:run-slash
           #:github-app-error #:github-app-error-detail))

(in-package #:nodecode-github-app)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and GITHUB-APP-ERROR, which a call that cannot go on
;;; signals, come from here: every refusal starts as one of these, and the
;;; tools answer it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral github-app :not-running t)
