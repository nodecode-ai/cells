;;;; capture-test.lisp --- a failed turn's condition becomes a counted, readable case.

(in-package #:nodecode.test)

(defgeneric selffix-test-gf (x))

(defun selffix-test-signaller (x)
  (if (stringp x) x (error 'type-error :datum x :expected-type 'string)))

(defun selffix-caught (thunk)
  "THUNK's error, signalled under the AGENT-LOOP advice the way a turn's would be."
  (handler-case (progn (nodecode-selffix::note-frame thunk) nil)
    (error (condition) condition)))

(deftest selffix-cell-capture-no-method-case ()
  (with-selffix-root ()
    (let* ((condition (selffix-caught (lambda () (selffix-test-gf "a"))))
           (path (nodecode-selffix::note-case :no-turn condition))
           (case (nodecode-selffix::read-case path)))
      (is (typep condition 'sb-pcl::no-applicable-method-error) "the advice declines: the turn still fails")
      (is (eq 'sb-pcl::no-applicable-method-error (getf case :type)))
      (is (eq 'selffix-test-gf (getf case :function)) "the fault names the generic function, not a message")
      (is (equal '("a") (getf case :args)))
      (is (= 1 (length (getf case :classes))))
      (is (= 1 (getf case :count)))
      (nodecode-selffix::note-case :no-turn (selffix-caught (lambda () (selffix-test-gf "b"))))
      (let ((again (nodecode-selffix::read-case path)))
        (is (= 2 (getf again :count)) "a second sighting counts into the same case")
        (is (equal (getf case :first) (getf again :first))))
      (is (= 1 (length (directory (merge-pathnames "*.lisp" (nodecode-selffix::cases-dir)))))))))

(deftest selffix-cell-capture-signalling-frame ()
  (with-selffix-root ()
    (let* ((condition (selffix-caught (lambda () (selffix-test-signaller 42))))
           (case (nodecode-selffix::read-case (nodecode-selffix::note-case :no-turn condition))))
      (is (typep condition 'type-error))
      (is (eq 'selffix-test-signaller (getf case :function)) "the frame that signalled, noted before the stack unwound")
      (is (equal '(42) (getf case :args)))
      (is (search "42" (getf case :detail))))))

(deftest selffix-cell-capture-unreadable-args ()
  (with-selffix-root ()
    (let ((case (nodecode-selffix::read-case
                 (nodecode-selffix::note-case :no-turn (selffix-caught (lambda () (selffix-test-signaller (make-hash-table))))))))
      (is (eq :unreadable (getf case :args)) "a hash table does not read back whole")
      (is (null (nodecode-selffix::repro-form case)) "no arguments, no call repro: the case stays a note"))))

(deftest selffix-cell-capture-provider-failure-makes-no-case ()
  (with-selffix-root ()
    (let ((recorded nil)
          (condition (selffix-caught (lambda () (selffix-test-signaller 1)))))
      (nodecode-selffix::capture-failure (lambda (&rest args) (setf recorded args)) :no-turn condition
                                         :evidence (list :status 402))
      (is recorded "the failure is still recorded")
      (is (null (directory (merge-pathnames "*.lisp" (nodecode-selffix::cases-dir)))) "a provider's failure is no fault of the code")
      (nodecode-selffix::capture-failure (lambda (&rest args) (setf recorded args)) :no-turn
                                         (selffix-caught (lambda () (selffix-test-signaller 2))))
      (is (= 1 (length (directory (merge-pathnames "*.lisp" (nodecode-selffix::cases-dir)))))))))
