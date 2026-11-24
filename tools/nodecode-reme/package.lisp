;;;; package.lisp --- the NODECODE-REME package: vocabulary and the condition.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; One package, nicknamed REME so an eval form reads (reme:recall ...), and
;;;; used qualified: its verbs are the cell's own words, not Lisp's.
;;;;
;;;; Every exported function returns a STRING and every refusal is
;;;; REME-ERROR, rendered by the eval snippet as "ERROR: REME-ERROR: <detail>".
;;;; The state the files share -- the running settings and the live tables the
;;;; frame observer and the verbs both read -- is declared here, ahead of every
;;;; use, so no file references a special variable a later file defines.

(defpackage #:nodecode-reme
  (:use #:cl)
  (:nicknames #:reme)
  (:export
   ;; model-facing vocabulary (see the primer in cell.lisp)
   #:recall
   #:health
   #:status
   ;; the one condition
   #:reme-error))

(in-package #:nodecode-reme)

(nlk:define-peripheral reme :not-running t :lock "reme" :recursive t)

;;; --- what the cell knows while it runs ------------------------------------------
;;; Live-only: emptied at every start and at every stop. Nothing here is a
;;; record; the memory itself lives in the ReMe service.

(defparameter +turn-ends+ '("turn.completed" "turn.failed" "turn.cancelled")
  "The frame kinds that end a turn; only `turn.completed` is recorded.")

(defvar *asks* (make-hash-table :test 'equal :synchronized t)
  "Session id -> the newest ask the :FRAME hook saw. A session started before
this cell started has none, and its ask is read from the event log instead.")

(defvar *answers* (make-hash-table :test 'equal :synchronized t)
  "Session id -> the assistant text of the turn in flight.")

(defvar *recalled* (make-hash-table :test 'equal :synchronized t)
  "Session id -> (QUERY . TEXT): the one recall already made for that ask, so a
turn's later rounds read it instead of searching again.")

(defvar *queue* '()
  "Completed turns waiting for the writer, newest first; the writer takes the
oldest. Only the lock's holder touches it.")

(defvar *last* nil
  "One line saying what the last recall or recording did, or NIL.")

(defvar *retry-at* 0
  "Universal time before which a service that just failed is not asked again:
reme.retry_seconds, so a service that is down costs one attempt, not one per
turn.")

(defvar *worker* nil
  "The writer thread while it runs, or NIL.")

(defvar *use-worker* t
  "Whether a start runs the writer thread; a test drives FLUSH-WRITES by hand.")

(defun clear-state ()
  "Empty every live table and drop the writer handle."
  (mapc #'clrhash (list *asks* *answers* *recalled*))
  (with-reme-lock
    (setf *queue* '()
          *last* nil
          *retry-at* 0)))
