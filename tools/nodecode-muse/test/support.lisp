;;;; support.lisp --- the slice, and a store of its own per test.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here stubs the cell: it is the real store, written and read back
;;;; from a scratch folder each test makes, so a test proves what an operator
;;;; gets rather than what a stand-in was handed.

(in-package #:nodecode.test)

(define-test-slice "muse" "MUSE-CELL-" :start nodecode-muse:start-cell)

(defun muse-scratch-root ()
  "A folder no other test has used, for one test's store."
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames (format nil "muse-cell-~36r/" (random (expt 36 6) (make-random-state t)))
                               (uiop:temporary-directory)))))
    (ensure-directories-exist dir)
    dir))

(defmacro with-muse-store (&body body)
  "BODY with the store's folder bound to a fresh one (the lifecycle fixture:
what is started and stopped is the cell's)."
  `(let ((nodecode-muse::*root* (muse-scratch-root)))
     ,@body))

(defmacro with-muse ((&rest config) &body body)
  "BODY with the cell started on the section CONFIG names (the defaults when
it names none), over the fixture's store, and stopped on unwind."
  `(with-cell-stop ((muse-start ,@config))
     ,@body))

(defun muse-tool (action &optional data)
  "ACTION through the registered muse_manage tool, as a model's call reaches
it: the contract decodes the arguments before the body sees them."
  (funcall (third (nle::find-tool "muse_manage"))
           (nlk:contract-decode (nlk:contract 'nodecode-muse::muse-call)
                                (if data
                                    (nlk:json-object "action" action "data" data)
                                    (nlk:json-object "action" action)))))
