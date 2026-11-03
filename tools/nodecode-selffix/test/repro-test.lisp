;;;; repro-test.lisp --- a case's repro form answers 1 while the fault stands, 0 once it is gone.

(in-package #:nodecode.test)

(defun selffix-ask (text)
  "TEXT's answer here, read and evaluated the way the sealed process does it."
  (eval (let ((*read-eval* nil)) (read-from-string text))))

(defun selffix-no-method-case ()
  (with-selffix-root ()
    (nodecode-selffix::read-case
     (nodecode-selffix::note-case :no-turn (selffix-caught (lambda () (selffix-test-gf "a")))))))

(deftest selffix-cell-repro-no-method ()
  (let ((case (selffix-no-method-case)))
    (multiple-value-bind (text kind) (nodecode-selffix::repro-form case)
      (is (eq :check kind) "the check calls nothing")
      (is (not (search "selffix-test-gf \"a\"" text)) "the arguments are not passed, only their classes")
      (is (= 1 (selffix-ask text)) "no method for a string: the fault stands")
      (let ((method (eval '(defmethod selffix-test-gf ((x string)) x))))
        (unwind-protect (is (= 0 (selffix-ask text)) "a method for STRING: the fault is gone")
          (remove-method #'selffix-test-gf method)))
      (let ((around (eval '(defmethod selffix-test-gf :around ((x string)) (call-next-method)))))
        (unwind-protect (is (= 1 (selffix-ask text)) "an :around method alone is no primary method")
          (remove-method #'selffix-test-gf around))))
    (let ((not-generic (nodecode-selffix::repro-form (list* :function 'selffix-test-signaller case))))
      (is (= 2 (selffix-ask not-generic)) "a function that is not generic cannot be judged"))))

(deftest selffix-cell-repro-undefined-function ()
  (let* ((case (list :type 'undefined-function :function 'selffix-test-never-defined :args :unreadable))
         (text (nodecode-selffix::repro-form case)))
    (is (= 1 (selffix-ask text)))
    (setf (fdefinition 'selffix-test-never-defined) (lambda () t))
    (unwind-protect (is (= 0 (selffix-ask text)))
      (fmakunbound 'selffix-test-never-defined))))

(deftest selffix-cell-repro-call ()
  (let ((case (list :type 'type-error :function 'selffix-test-signaller :args '(42))))
    (multiple-value-bind (text kind) (nodecode-selffix::repro-form case)
      (is (eq :call kind))
      (is (= 1 (selffix-ask text)) "the recorded call signals the recorded type again"))
    (is (= 0 (selffix-ask (nodecode-selffix::repro-form (list* :args '("fine") case)))) "the call returns: gone")
    (is (= 2 (selffix-ask (nodecode-selffix::repro-form (list* :type 'program-error case)))) "another error is not judged")))

(deftest selffix-cell-repro-deftest-for-the-owning-cell ()
  (let* ((case (selffix-no-method-case))
         (form (let ((*package* (find-package :nodecode.test)) (*read-eval* nil))
                 (read-from-string (nodecode-selffix::repro-deftest case)))))
    (is (eq 'deftest (first form)))
    (is (search "SELFFIX-" (symbol-name (second form))))
    (is (search "SELFFIX-TEST-GF" (prin1-to-string form)))
    (let ((call (list :type 'type-error :function 'selffix-test-signaller :args '(42))))
      (is (null (nodecode-selffix::repro-deftest call)) "a call's arguments do not travel upstream")))
  (is (equal "nodecode-selffix" (nodecode-selffix::owning-cell 'nodecode-selffix::repro-form))))
