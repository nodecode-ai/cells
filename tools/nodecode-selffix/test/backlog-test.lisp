;;;; backlog-test.lisp --- the backlog: what failed, grouped, most frequent first.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The pure ITEMS halves driven with data of the shapes the readers return:
;;;; cases grouped by the exact (TYPE . FUNCTION), fixable only with readable
;;;; arguments; notes keyed without parsing message text. Then case files
;;;; read back from a temp folder, a turn a case covers counted once, BACKLOG
;;;; against a temp store holding a real failed turn, and a source that
;;;; cannot read reported as unread rather than empty.

(in-package #:nodecode.test)

(defun selffix-ledger-line (&rest pairs)
  (let ((line (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr do (setf (gethash key line) value))
    line))

(defun selffix-write-case (name form)
  "FORM written under cases/NAME.lisp as the capture writes it."
  (let ((path (nodecode-selffix::selffix-path (format nil "cases/~a.lisp" name))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
      (if (stringp form)
          (write-string form out)
          (with-standard-io-syntax
            (let ((*package* (find-package '#:keyword)))
              (prin1 form out)))))
    path))

(deftest selffix-cell-backlog-normalize ()
  (is (equal (nodecode-selffix::normalize-text
              "stream ended after 1532 bytes at /tmp/a/b.lisp in turn-1U6VMJF-68AE9QFR9L #<THREAD \"x\" {1202F2}>")
             (nodecode-selffix::normalize-text
              "stream   ended after 88 bytes at ~/.cache/c.lisp in turn-1U72X41-5ALM550GL4 #<THREAD \"y\" {9}>")))
  (is (= 20 (length (nodecode-selffix::normalize-text (make-string 50 :initial-element #\x) :cap 20))) "capped"))

(deftest selffix-cell-backlog-cases-grouped-by-condition ()
  (let ((items (nodecode-selffix::group-occurrences
                (nodecode-selffix::case-items
                 (list (cons #p"a.lisp" '(:type sb-pcl::no-applicable-method-error :function nodecode-selffix:backlog
                                          :args (:limit "x") :count 3 :first "2026-11-01T00:00:00.000Z"
                                          :last "2026-11-02T00:00:00.000Z" :generation 7 :turn "turn-1"))
                       (cons #p"b.lisp" '(:type sb-pcl::no-applicable-method-error :function nodecode-selffix:backlog
                                          :args (:days nil) :count 2 :first "2026-10-30T00:00:00.000Z"
                                          :last "2026-11-03T00:00:00.000Z" :generation 8 :turn "turn-2"))
                       (cons #p"c.lisp" '(:type type-error :function car :args :unreadable :count 1
                                          :first "2026-11-03T00:00:00.000Z" :last "2026-11-03T00:00:00.000Z"))
                       (cons #p"d.lisp" '(:type type-error :function car :args (1) :count 1
                                          :last "2026-09-01T00:00:00.000Z")))
                 :since "2026-10-20T00:00:00.000Z"))))
    (is (= 2 (length items)) "one item per (type . function); the case outside the window left out")
    (let ((top (first items)))
      (is (= 5 (getf top :count)) "two cases of one condition, their counts summed")
      (is (eq :case (getf top :source)))
      (is (equal (cons 'sb-pcl::no-applicable-method-error 'nodecode-selffix:backlog) (getf top :condition)) "the key is the symbols themselves")
      (is (getf top :fixable) "readable arguments: a repro form can call it")
      (is (equal '(:days nil) (getf top :args)) "the newest case's arguments")
      (is (eql 8 (getf top :generation)))
      (is (equal "2026-10-30T00:00:00.000Z" (getf top :first)) "the oldest first sighting of either")
      (is (equal "2026-11-03T00:00:00.000Z" (getf top :last))))
    (let ((note (second items)))
      (is (not (getf note :fixable)) "unreadable arguments: a note, never a fix candidate")
      (is (search "did not print readably" (getf note :title))))))

(deftest selffix-cell-backlog-cases-read-back ()
  (with-selffix-root ()
    (selffix-write-case "fp1" (list :type 'type-error :function 'car :args '(1) :count 2
                                    :first "2026-11-01T00:00:00.000Z" :last (nodecode-selffix::days-ago-iso 0)
                                    :turn "turn-1"))
    (selffix-write-case "fp2" "(:type nosuch-package-for-selffix::boom :function nil :count 1)")
    (selffix-write-case "fp3" "42")
    (selffix-write-case "fp4" "(:type type-error :function car :args #.(error \"read-eval\") :count 1)")
    (multiple-value-bind (items unread) (nodecode-selffix:backlog :sources '(:case))
      (is (= 1 (length items)) "the readable case")
      (is (equal (cons 'type-error 'car) (getf (first items) :condition)) "symbols read back as symbols")
      (is (= 2 (getf (first items) :count)))
      (is (= 3 (length unread)) "a gone package, a non-plist and #. each named, not dropped")
      (is (every (lambda (u) (eq :case (car u))) unread))
      (is (find-if (lambda (u) (search "fp2.lisp" (cdr u))) unread)))))

(deftest selffix-cell-backlog-a-case-covers-its-turn ()
  (let* ((rows '(("s-a" "turn-9" "2026-11-03T00:00:00.000Z" "{\"condition-type\":\"TYPE-ERROR\",\"detail\":\"1 is not a LIST\"}")
                 ("s-b" "turn-10" "2026-11-03T01:00:00.000Z" "{\"condition-type\":\"TYPE-ERROR\",\"detail\":\"2 is not a LIST\"}")))
         (nodecode-selffix::*excluded-session-p* (constantly nil))
         (nodecode-selffix::*backlog-sources*
           (list (cons :case (lambda (since) (declare (ignore since))
                               (nodecode-selffix::case-items
                                (list (cons #p"x.lisp" '(:type type-error :function car :args (1) :count 1
                                                        :last "2026-11-03T00:00:00.000Z" :turn "turn-9"))))))
                 (cons :turn-failed (lambda (since) (declare (ignore since))
                                      (nodecode-selffix::turn-failed-items rows))))))
    (let ((items (nodecode-selffix:backlog)))
      (is (equal '(("case COMMON-LISP::TYPE-ERROR COMMON-LISP::CAR" 1) ("turn-failed TYPE-ERROR" 1))
                 (sort (mapcar (lambda (i) (list (getf i :key) (getf i :count))) items) #'string< :key #'first)) "turn-9 is counted once, as its case; turn-10 stays a note"))
    (is (equal '(:case) (mapcar (lambda (i) (getf i :source)) (nodecode-selffix:backlog :fixable t))) "FIXABLE answers cases alone")))

(deftest selffix-cell-backlog-turn-failed-notes ()
  (let* ((nodecode-selffix::*excluded-session-p* (lambda (id) (equal id "s-selffix")))
         (rows '(("s-a" "turn-1" "2026-11-01T10:00:00.000Z" "{\"condition-type\":\"PROVIDER-ERROR\",\"detail\":\"HTTP 529 after 3 tries\"}")
                 ("s-b" "turn-2" "2026-11-02T10:00:00.000Z" "{\"condition-type\":\"PROVIDER-ERROR\",\"detail\":\"HTTP 503 after 2 tries\"}")
                 ("s-c" "turn-3" "2026-11-03T09:00:00.000Z" "{\"condition-type\":\"TYPE-ERROR\",\"detail\":\"NIL is not a STRING\"}")
                 ("s-selffix" "turn-4" "2026-11-03T09:30:00.000Z" "{\"condition-type\":\"TYPE-ERROR\",\"detail\":\"NIL is not a STRING\"}")
                 ("s-d" "turn-5" "2026-10-01T09:00:00.000Z" "{\"condition-type\":\"TYPE-ERROR\",\"detail\":\"NIL is not a STRING\"}")))
         (items (nodecode-selffix::group-occurrences
                 (nodecode-selffix::turn-failed-items rows :since "2026-10-20T00:00:00.000Z"))))
    (is (= 2 (length items)) "one note per condition type; the excluded session and the old row left out")
    (let ((top (first items)))
      (is (= 2 (getf top :count)) "two provider failures, whatever their text")
      (is (eq :turn-failed (getf top :source)))
      (is (equal "turn-failed PROVIDER-ERROR" (getf top :key)))
      (is (not (getf top :fixable)) "a turn row with no case is a note")
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
    (is (notany (lambda (i) (getf i :fixable)) (append harm unmet)) "notes")
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
      (is (equal "turn-failed SIMPLE-ERROR" (getf (first items) :key)) "the failed turn is a note by its type")
      (is (search "planted fault 7" (getf (first (getf (first items) :evidence)) :text)) "its text rides as evidence"))))

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
    (is (null (nodecode-selffix::turn-failed-items rows)) "a failure in the excluded session is left out")
    (is (null (nodecode-selffix::case-items
               (list (cons #p"w.lisp" '(:type type-error :function car :args (1) :session "cron-wake"))))) "and so is a case caught there")))
