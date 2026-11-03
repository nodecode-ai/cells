;;;; gate-test.lisp --- the idle gate's pure half, and the lock.

(in-package #:nodecode.test)

(deftest selffix-cell-gate-busy-from ()
  (let ((now (nlk:iso-universal "2026-11-03T12:00:00Z"))
        (counted (lambda (s) (not (equal s "cron-wake")))))
    (is (null (nodecode-selffix::busy-from '() '() '() now :counted counted)) "an empty log is idle")
    (is (search "s-work" (nodecode-selffix::busy-from '(("s-work" "2026-11-03T11:50:00.000Z")) '() '() now :counted counted))
        "a turn at work inside the window keeps the home busy")
    (is (null (nodecode-selffix::busy-from '(("cron-wake" "2026-11-03T11:59:00.000Z")) '() '() now :counted counted))
        "the wake's own session does not")
    (is (search "still running"
                (nodecode-selffix::busy-from '() '(("s-long" "t-1")) '() now :counted counted))
        "a turn that started and never ended keeps it busy")
    (is (null (nodecode-selffix::busy-from '() '(("s-long" "t-1")) '("t-1") now :counted counted))
        "one that ended does not")))

(deftest selffix-cell-gate-lock ()
  (with-selffix-root ()
    (let ((now (get-universal-time)))
      (is (nodecode-selffix::take-lock now) "the first taker holds it")
      (is (not (nodecode-selffix::take-lock now)) "a second does not")
      (nodecode-selffix::drop-lock)
      (is (nodecode-selffix::take-lock now) "free again after the drop")
      (let ((old (- (get-universal-time) nlk:+unix-epoch+ (* 3 3600))))
        (sb-posix:utimes (namestring (nodecode-selffix::lock-path)) old old))
      (is (nodecode-selffix::take-lock now) "a stale lock is taken over")
      (nodecode-selffix::drop-lock))))
