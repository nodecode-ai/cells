;;;; package.lisp --- the GITHUB-MENTIONS package: three tools, one command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed GITHUB-MENTIONS so an eval form reads
;;;; (github-mentions:list-mentions ...). The three verbs are tools the model
;;;; calls; a person can call the same functions from eval.

(defpackage #:nodecode-github-mentions
  (:use #:cl)
  (:nicknames #:github-mentions)
  (:export #:start-cell
           #:stop-cell
           #:list-mentions #:read-thread #:reply #:show-mentions
           #:github-mentions-error #:github-mentions-error-detail))

(in-package #:nodecode-github-mentions)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and GITHUB-MENTIONS-ERROR, which a call that cannot go on
;;; signals, come from here: every refusal starts as one of these, and the
;;; tools answer it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral github-mentions :not-running t)
