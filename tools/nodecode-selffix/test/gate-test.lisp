;;;; gate-test.lisp --- the idle gate's pure half, and the lock.

(in-package #:nodecode.test)

(deftest selffix-cell-gate-busy-from ()
  (let ((now (nlk:iso-universal "2026-11-03T12:00:00Z"))
        (counted (lambda (s) (not (equal s "cron-wake")))))
    (is (null (nodecode-selffix::busy-from '() '() '() now :counted counted)) "an empty log is idle")
    (is (search "s-work" (nodecode-selffix::busy-from '(("s-work" "2026-11-03T11:50:00.000Z")) '() '() now :counted counted)))
    (is (null (nodecode-selffix::busy-from '(("cron-wake" "2026-11-03T11:59:00.000Z")) '() '() now :counted counted)))
    (is (search "still running"
                (nodecode-selffix::busy-from '() '(("s-long" "t-1")) '() now :counted counted)))
    (is (null (nodecode-selffix::busy-from '() '(("s-long" "t-1")) '("t-1") now :counted counted)))))

(deftest selffix-cell-gate-lock ()
  (with-selffix-root ()
    (is (nodecode-selffix::take-lock) "the first taker holds it")
    (is (not (nodecode-selffix::take-lock)) "a second in the same process does not")
    (nodecode-selffix::drop-lock)
    (is (nodecode-selffix::take-lock) "free again after the drop")
    (nodecode-selffix::drop-lock)
    (is (probe-file (nodecode-selffix::lock-path)) "the lock file stays, so no taker locks a file the next one cannot see")))

(deftest selffix-cell-gate-lock-dies-with-its-holder ()
  (with-selffix-root ()
    (with-open-file (out (ensure-directories-exist (nodecode-selffix::lock-path))
                         :direction :output :if-exists :supersede)
      (write-line "2026-11-03T05:00:00Z" out))
    (is (nodecode-selffix::take-lock) "a lock file whose holder died blocks nothing")
    (nodecode-selffix::drop-lock)))

(defun selffix-child-takes-lock-p (path)
  "Whether a second SBCL process gets an fcntl write lock on PATH, or :NO-SBCL."
  (let ((sbcl (nodecode-selffix::on-path "sbcl")))
    (if (null sbcl)
        :no-sbcl
        (search "got"
                (uiop:run-program
                 (list sbcl "--non-interactive" "--no-sysinit" "--no-userinit" "--eval" "(require :sb-posix)"
                       "--eval"
                       (format nil "(let ((fd (sb-posix:open ~s sb-posix:o-wronly))) (princ (handler-case (progn (sb-posix:fcntl fd sb-posix:f-setlk (make-instance 'sb-posix:flock :type sb-posix:f-wrlck :whence sb-posix:seek-set :start 0 :len 0)) \"got\") (sb-posix:syscall-error () \"refused\"))))"
                               (namestring path)))
                 :output :string :ignore-error-status t)))))

(deftest selffix-cell-gate-lock-holds-across-processes ()
  (with-selffix-root ()
    (is (nodecode-selffix::take-lock))
    (let ((held (selffix-child-takes-lock-p (nodecode-selffix::lock-path))))
      (unless (eq held :no-sbcl)
        (is (not held) "another process is refused while this one holds it")))
    (nodecode-selffix::drop-lock)
    (let ((free (selffix-child-takes-lock-p (nodecode-selffix::lock-path))))
      (unless (eq free :no-sbcl)
        (is free "and gets it once this one drops it")))))
