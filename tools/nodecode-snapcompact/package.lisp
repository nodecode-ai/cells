;;;; package.lisp --- the SNAPCOMPACT package: the renderer and its one condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed SNAPCOMPACT so an eval form reads
;;;; (snapcompact:frames-for "text"). The tool is what the model calls; a
;;;; person can call the same functions from eval, or type /snapcompact.

(defpackage #:nodecode-snapcompact
  (:use #:cl)
  (:nicknames #:snapcompact)
  (:export #:start-cell
           #:stop-cell
           #:snapcompact-tool
           #:snapcompact-command
           #:frames-for
           #:snapcompact-error
           #:snapcompact-error-detail))

(in-package #:nodecode-snapcompact)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. SNAPCOMPACT-ERROR and FAIL come from here, by way of
;;; NLK:DEFINE-PERIPHERAL: a request that cannot be carried out signals one,
;;; and the tool answers it as the call's failure rather than as an unhandled
;;; error. The macro also gives SETTING, RUNNING-SETTINGS and DEFINE-VERB.
;;; It is evaluated once, which is what keeps a reload from redefining them.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'define-verb)
    (nlk:define-peripheral snapcompact :not-running t)))
