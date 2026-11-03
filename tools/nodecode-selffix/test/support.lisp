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
            (nodecode-selffix::*root* ,root)
            (nodecode-selffix::*present* (constantly t))
            (nodecode-selffix::*async* nil))
       (unwind-protect (progn ,@body)
         (nodecode-selffix::drop-lock)
         (uiop:delete-directory-tree ,root :validate t :if-does-not-exist :ignore)))))

(defun selffix-item (key &key (source "turn.failed") (count 2))
  (list :source source :key key :count count :last "2026-11-03T01:00:00Z"
        :evidence (list (format nil "seen in s-work: ~a" key))))

(defmacro with-selffix-runner ((&key (answer 1) (calls '(list 0)) error (form "(+ 0 1)")
                                     (generations ''(1 2 3)))
                               &body body)
  "BODY with the repro form, the generations, the laying and the sealed ask
stubbed: every ask answers ANSWER (a number, or NIL for could not run), or
signals ERROR; CALLS' car counts the asks."
  (let ((counter (gensym "CALLS")))
    `(let* ((,counter ,calls)
            (nodecode-selffix::*repro-form* (lambda (item) (declare (ignore item)) ,form))
            (nodecode-selffix::*repro-deftest* (lambda (item) (declare (ignore item)) "(deftest selffix-found () (is (= 0 1)))"))
            (nodecode-selffix::*generations* (lambda () ,generations))
            (nodecode-selffix::*lay* (lambda (number home) (declare (ignore number home))))
            (nodecode-selffix::*ask* (lambda (text home)
                                       (declare (ignore text home))
                                       (incf (car ,counter))
                                       ,@(and error `((error "~a" ,error)))
                                       (values ,answer "stubbed"))))
       ,@body)))
