;;;; support.lisp --- selffix test slice and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Tests register into the shared nodecode.test registry under the
;;;; SELFFIX-CELL- prefix. Every test runs over a fresh folder bound to
;;;; NODECODE-SELFFIX::*ROOT*, and stubs the backlog, the repro and the idle
;;;; gate through their specials, so no test reads the real store or spawns
;;;; a home.

(in-package #:nodecode.test)

(define-test-slice "selffix" "SELFFIX-CELL-" :start nodecode-selffix:start-cell)

(defun selffix-temp-root ()
  (let ((dir (merge-pathnames (format nil "selffix-test-~36r/" (random (expt 36 10) (make-random-state t)))
                              (uiop:temporary-directory))))
    (ensure-directories-exist dir)))

(defmacro with-selffix-root (() &body body)
  "BODY over a fresh, empty loop folder, removed after."
  (let ((root (gensym "ROOT")))
    `(let* ((,root (selffix-temp-root))
            (nodecode-selffix::*root* ,root))
       (unwind-protect (progn ,@body)
         (uiop:delete-directory-tree ,root :validate t :if-does-not-exist :ignore)))))

(defun selffix-item (key &key (source "turn.failed") (count 2))
  (list :source source :key key :count count :last "2026-11-03T01:00:00Z"
        :evidence (list (format nil "seen in s-work: ~a" key))))

(defun selffix-repro-stub (&key (fails t) (calls (list 0)) error)
  "A repro that writes a failing test under HOME; CALLS' car counts calls."
  (lambda (item &key home)
    (incf (car calls))
    (when error (error "~a" error))
    (let ((path (merge-pathnames "repro-test.lisp" (ensure-directories-exist home))))
      (with-open-file (out path :direction :output :if-exists :supersede)
        (format out ";; repro for ~a~%(assert nil)~%" (getf item :key)))
      (list :test path :sha256 (nodecode-selffix::file-sha256 path) :fails fails :output "assertion failed"))))
