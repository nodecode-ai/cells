;;;; cell-test.lisp --- the gate: grades, quarantine, release, the ledger, the seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: a kept prose skill is graded from the session's own
;;;; tool trail, a grade D leaves the index and comes back on release, an
;;;; operator's recorded rule is what the skill is measured against, a
;;;; passing command earns an A, the chained ledger notices an edited line,
;;;; and the hooks, the tool and /witness answer on a scratch folder.

(in-package #:nodecode.test)

(define-cell-lifecycle-tests "witness"
  (:config "dir" (namestring (temp-path "witness")) "mode" "observe")
  (:hooks :tool 'nlk:record-definitions)
  (:help :witness)
  (:command "witness")
  (:refused ("mode" "sideways") ("degraded_percent" -1))
  (:idle witness:witness-error (witness:report-text) (witness:show-text "nothing")))

(deftest witness-cell-quarantines-what-the-trail-contradicts ()
  (with-witness ()
    (witness-seed-turn "s1" '(("c1" "sh" "{\"command\":\"curl https://api.example/w\"}"
                               "HTTP 403 Forbidden" t)))
    (witness-keep "s1" "(define-skill weather-queries \"# Reliable retrieval. Validated, reliable. Run scripts/verify-api.sh\")")
    (let ((record (witness::record-for "weather-queries")))
      (is record "a kept prose skill is graded")
      (is (equal "D" (getf record :grade)))
      (is (equal "quarantined" (getf record :status)) "mode gate moves a grade-D skill")
      (is (probe-file (getf record :qpath)) "the file is beside its cell, in quarantine")
      (is (not (probe-file (getf record :rel))) "and no longer where it was")
      (is (search "unresolved-failure" (witness:show-text "weather-queries")))
      (is (search "QUARANTINED" (witness:report-text))))
    (is (search "Restored" (cell-entry "nodecode-witness" "witness" "release weather-queries" "s1")))
    (is (equal "trusted" (getf (witness::record-for "weather-queries") :status)) "released and trusted in one step")
    (is (probe-file (getf (witness::record-for "weather-queries") :rel)) "the file is back")))

(deftest witness-cell-measures-a-skill-against-the-operators-rule ()
  (with-witness ("mode" "observe")
    (witness-keep "s1" "(define-memory develop-only \"Only modify the develop branch; never push to main.\" :type :feedback)")
    (witness-seed-turn "s1" '(("c1" "sh" "{\"command\":\"git status\"}" "clean" nil)))
    (witness-keep "s1" "(define-skill ship-it \"# Ship it\n1. git add -A\n2. git push origin main\")")
    (is (search "omits-user-constraint" (witness:show-text "ship-it")) "the rule is read from the operator's own memories")
    (is (equal "active" (getf (witness::record-for "ship-it") :status)) "observe labels it and leaves the file in place")))

(deftest witness-cell-grades-a-passing-command-a ()
  (with-witness ("mode" "observe")
    (witness-seed-turn "s1" '(("c1" "sh" "{\"command\":\"pytest -q\"}" "12 passed" nil)))
    (witness-keep "s1" "(define-skill fix-flaky-tests \"Pin the seed, then rerun with pytest -q.\")")
    (is (equal "A" (getf (witness::record-for "fix-flaky-tests") :grade)))
    (is (search "fix-flaky-tests" (witness-call "report")))
    (is (search "grade A" (witness-call "show" "fix-flaky-tests")))
    (is (search "ledger intact" (witness-call "verify")))
    (is (search "no hook calls" (cell-entry "nodecode-witness" "witness" "trace" "s1")))
    (funcall (cell-hook "nodecode-witness" :tool)
             (list :name "sh" :arguments (nlk:json-object "command" "ls -la"))
             #'identity)
    (is (search "sh  keys=command" (witness-call "trace")) "the tool hook keeps names and keys, never values")))

(deftest witness-cell-ledger-chain-notices-an-line-it-did-not-write ()
  (with-witness ("mode" "observe")
    (witness-keep "s1" "(define-skill plain-skill \"A plain procedure for the test.\")")
    (multiple-value-bind (ok message) (witness:ledger-verify)
      (is ok message))
    (with-open-file (out (witness-ledger-path) :direction :output :if-exists :append
                                              :if-does-not-exist :create)
      (format out "{\"seq\":99,\"prev\":\"x\",\"hash\":\"y\",\"type\":\"grade\"}~%"))
    (multiple-value-bind (ok message) (witness:ledger-verify)
      (is (not ok) "an appended line breaks the chain")
      (is (search "broken" message)))))

(deftest witness-cell-a-procedure-skill-is-not-graded ()
  (with-witness ("mode" "observe")
    (witness-keep "s1" "(define-skill roll-dice (n) \"Roll n dice.\" (loop repeat n collect (1+ (random 6))))")
    (is (null (witness::record-for "roll-dice")) "only the prose shape is one file the gate can move")))
