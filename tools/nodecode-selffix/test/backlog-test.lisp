;;;; backlog-test.lisp --- the backlog: what failed, grouped, most frequent first.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The pure ITEMS halves driven with data of the shapes the readers return,
;;;; then BACKLOG against a temp store holding a real failed turn, and a
;;;; source that cannot read reported as unread rather than empty.

(in-package #:nodecode.test)

(defun selffix-ledger-line (&rest pairs)
  (let ((line (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr do (setf (gethash key line) value))
    line))

(deftest selffix-cell-backlog-normalize ()
  (is (equal (nodecode-selffix::normalize-text
              "stream ended after 1532 bytes at /tmp/a/b.lisp in turn-1U6VMJF-68AE9QFR9L #<THREAD \"x\" {1202F2}>")
             (nodecode-selffix::normalize-text
              "stream   ended after 88 bytes at ~/.cache/c.lisp in turn-1U72X41-5ALM550GL4 #<THREAD \"y\" {9}>")))
  (is (equal "The function NODECODE.EVOLVED::ANTHROPIC-THINKING-EFFORT-P is undefined."
             (nodecode-selffix::normalize-text
              "The function NODECODE.EVOLVED::ANTHROPIC-THINKING-EFFORT-P is undefined.")) "a symbol name is kept")
  (is (= 20 (length (nodecode-selffix::normalize-text (make-string 50 :initial-element #\x) :cap 20))) "capped"))

(deftest selffix-cell-backlog-turn-failed-grouped ()
  (let* ((nodecode-selffix::*excluded-session-p* (lambda (id) (equal id "s-selffix")))
         (rows '(("s-a" "turn-1" "2026-11-01T10:00:00.000Z" "{\"condition-type\":\"PROVIDER-ERROR\",\"detail\":\"HTTP 529 after 3 tries\"}")
                 ("s-b" "turn-2" "2026-11-02T10:00:00.000Z" "{\"condition-type\":\"PROVIDER-ERROR\",\"detail\":\"HTTP 503 after 2 tries\"}")
                 ("s-c" "turn-3" "2026-11-03T09:00:00.000Z" "{\"condition-type\":\"TYPE-ERROR\",\"detail\":\"NIL is not a STRING\"}")
                 ("s-selffix" "turn-4" "2026-11-03T09:30:00.000Z" "{\"condition-type\":\"TYPE-ERROR\",\"detail\":\"NIL is not a STRING\"}")
                 ("s-d" "turn-5" "2026-10-01T09:00:00.000Z" "{\"condition-type\":\"TYPE-ERROR\",\"detail\":\"NIL is not a STRING\"}")))
         (items (nodecode-selffix::group-occurrences
                 (nodecode-selffix::turn-failed-items rows :since "2026-10-20T00:00:00.000Z"))))
    (is (= 2 (length items)) "one item per fault; the excluded session and the old row left out")
    (let ((top (first items)))
      (is (= 2 (getf top :count)) "the provider fault counted twice")
      (is (eq :turn-failed (getf top :source)))
      (is (equal "turn-failed PROVIDER-ERROR: HTTP N after N tries" (getf top :key)))
      (is (equal "2026-11-01T10:00:00.000Z" (getf top :first)))
      (is (equal "2026-11-02T10:00:00.000Z" (getf top :last)))
      (is (equal '("turn-2" "turn-1") (mapcar (lambda (e) (getf e :turn)) (getf top :evidence)))))
    (is (= 1 (getf (second items) :count)) "the type error once, inside the window")))

(deftest selffix-cell-backlog-evidence-capped ()
  (let* ((nodecode-selffix::*evidence-cap* 2)
         (rows (loop for i from 1 to 5
                     collect (list (format nil "s-~d" i) (format nil "turn-~d" i)
                                   (format nil "2026-11-0~dT00:00:00.000Z" i)
                                   "{\"condition-type\":\"ERROR\",\"detail\":\"boom\"}")))
         (item (first (nodecode-selffix::group-occurrences (nodecode-selffix::turn-failed-items rows)))))
    (is (= 5 (getf item :count)))
    (is (equal '("turn-5" "turn-4") (mapcar (lambda (e) (getf e :turn)) (getf item :evidence))))))

(deftest selffix-cell-backlog-ledger-sources ()
  (let* ((lines (list (selffix-ledger-line "kind" "harm" "name" "old-skill" "at" "2026-11-01T00:00:00.000Z" "quote" "it broke")
                      (selffix-ledger-line "kind" "harm" "name" "old-skill" "at" "2026-11-02T00:00:00.000Z" "quote" "again")
                      (selffix-ledger-line "kind" "helped" "name" "old-skill" "at" "2026-11-02T00:00:00.000Z")
                      (selffix-ledger-line "kind" "verification" "name" "page loads" "status" "failed" "at" "2026-11-02T00:00:00.000Z" "note" "404")
                      (selffix-ledger-line "kind" "verification" "name" "page loads" "status" "blocked" "at" "2026-11-03T00:00:00.000Z")
                      (selffix-ledger-line "kind" "verification" "name" "page loads" "status" "passed" "at" "2026-11-03T01:00:00.000Z")
                      (selffix-ledger-line "kind" "verification" "name" "tests" "status" "skipped" "at" "2026-11-03T01:00:00.000Z")))
         (harm (nodecode-selffix::group-occurrences (nodecode-selffix::harm-items lines)))
         (unmet (nodecode-selffix::group-occurrences (nodecode-selffix::verification-items lines))))
    (is (equal '(("harm old-skill" 2)) (mapcar (lambda (i) (list (getf i :key) (getf i :count))) harm)))
    (is (equal '(("verification page loads" 2)) (mapcar (lambda (i) (list (getf i :key) (getf i :count))) unmet)))
    (is (null (nodecode-selffix::harm-items lines :since "2026-11-05T00:00:00.000Z")) "the window holds")))

(deftest selffix-cell-backlog-qa-notes ()
  (let ((items (nodecode-selffix::group-occurrences
                (nodecode-selffix::qa-items
                 (list (selffix-ledger-line "tool" "eval" "symptom" "timeout" "at" "2026-11-01T00:00:00.000Z" "note" "hung")
                       (selffix-ledger-line "tool" "eval" "symptom" "timeout" "at" "2026-11-02T00:00:00.000Z" "note" "hung again")
                       (selffix-ledger-line "tool" "web" "symptom" "refused" "at" "2026-11-02T00:00:00.000Z"))))))
    (is (equal '(("qa eval timeout" 2) ("qa web refused" 1))
               (mapcar (lambda (i) (list (getf i :key) (getf i :count))) items)))))

(deftest selffix-cell-backlog-reads-the-store ()
  (with-temp-store ()
    (nlk:create-session :id "s-work")
    (let ((turn (nlk:admit-turn "s-work" (nlk::make-durable-id "command") "do the thing")))
      (nlk:fail-turn turn (make-condition 'simple-error :format-control "planted fault ~d" :format-arguments '(7))))
    (multiple-value-bind (items unread) (nodecode-selffix:backlog :sources '(:turn-failed))
      (is (null unread) "the store read")
      (is (= 1 (length items)))
      (is (search "planted fault N" (getf (first items) :key)) "the failed turn is an item"))))

(deftest selffix-cell-backlog-unread-is-not-empty ()
  (let ((nodecode-selffix::*backlog-sources*
          (list (cons :turn-failed (lambda (since) (declare (ignore since)) (error "store locked")))
                (cons :qa (lambda (since) (declare (ignore since)) nil)))))
    (multiple-value-bind (items unread) (nodecode-selffix:backlog)
      (is (null items))
      (is (equal '(:turn-failed) (mapcar #'car unread)) "the source that could not read is named")
      (is (search "store locked" (cdr (first unread)))))))

(deftest selffix-cell-backlog-leaves-out-the-wake ()
  (is (eq 'nodecode-cron::wake-session-p nodecode-selffix::*excluded-session-p*) "the wake the cycle runs on is not backlog by default")
  (let ((nodecode-selffix::*excluded-session-p* (lambda (id) (equal id "cron-wake")))
        (rows '(("cron-wake" "turn-1" "2026-11-03T00:00:00.000Z" "{\"condition-type\":\"ERROR\",\"detail\":\"x\"}"))))
    (is (null (nodecode-selffix::turn-failed-items rows)) "a failure in the excluded session is left out")))
