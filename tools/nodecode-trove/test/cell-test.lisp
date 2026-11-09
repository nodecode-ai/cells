;;;; cell-test.lisp --- lifecycle, the six tools through dispatch, /trove.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, offline, over a private
;;;; in-memory log: the section (a healthy start, limits under their floors
;;;; refused), every tool over sessions of its own, /trove's panel and its
;;;; two verbs, and the one promise the original makes -- that a row cites a
;;;; seq whose body comes back whole. No test dials anything: the log is a
;;;; store of its own and nothing here reaches the network.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(define-test-slice "trove" "TROVE-CELL-" :start nodecode-trove:start-cell)

(defun trove-tool (name &rest pairs)
  "Call the registered tool NAME the way dispatch does: PAIRS as one JSON
object, through the tool's own contract, the body called with the record."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (let* ((schema (second entry))
           (arguments (apply #'nlk:make-json-object pairs)))
      (funcall (third entry)
               (if (typep schema 'nlk:contract)
                   (nlk:contract-decode schema arguments)
                   arguments)))))

(defun trove-say (session input answer)
  "One finished turn in SESSION: what was asked and what was answered."
  (nlk:create-session :id session :cwd (namestring (uiop:getcwd)))
  (nodecode.working-context-proof::enter session input)
  (nodecode.working-context-proof::answer answer)
  (nodecode.working-context-proof::finish))

(defmacro with-trove-log (&body body)
  "The cell started over a private log holding two sessions of its own."
  `(with-cell-stop ((trove-start))
     (nodecode.working-context-proof::with-database
      (lambda ()
        (trove-say "trove-a" "how do we deploy the cells?"
                   "run the release script, then add the cell")
        (trove-say "trove-b" "the build is green" "noted: nothing to fix")
        ,@body))))

;;; --- lifecycle ----------------------------------------------------------------

(define-cell-lifecycle-tests "trove"
  (:help :trove)
  (:command "trove")
  (:running (is (nle::find-tool "trove_search") "the search tool is registered")
            (is (nle::find-tool "trove_read") "the read tool")
            (is (nle::find-tool "trove_entry") "the entry tool")
            (is (= 20 (nodecode-trove::setting :limit)) "limit defaults to twenty rows")
            (is (= 200 (nodecode-trove::setting :clip)) "clip to two hundred characters"))
  (:stopped (is (null (nle::find-tool "trove_search")) "stop unregisters the tools"))
  (:refused ("limit" 0) ("clip" 10) ("sessions" 0)))

;;; --- the reads ------------------------------------------------------------------

(deftest trove-cell-search-greps-every-session-and-cites-it ()
  (with-trove-log
    (is-carrying (answer (trove-tool "trove_search" "query" "release script"))
                 ("trove-a" "the row names the session it came from")
                 ("run the release script" "and shows the matching body")
                 ("trove_entry" "and says how to read the entry whole"))
    (is (search "no entry"
                (trove-tool "trove_search" "query" "nothing said this"))
        "a query nothing matched is an answer, not an error")))

(deftest trove-cell-entry-returns-the-body-whole ()
  (with-trove-log
    (let* ((row (first (nlk:recall-search "release script" :limit 1)))
           (seq (getf row :seq)))
      (is (integerp seq) "the log answers the row's seq")
      (let ((answer (trove-tool "trove_entry" "seq" seq)))
        (is (search "run the release script, then add the cell" answer)
            "the body comes back whole")
        (is (search (format nil "[seq ~d" seq) answer)
            "and the answer cites the seq searched")))))

(deftest trove-cell-read-walks-one-session-in-order ()
  (with-trove-log
    (is-carrying (answer (trove-tool "trove_read" "session_id" "trove-a"))
                 ("how do we deploy the cells?" "the question is there, oldest first")
                 ("run the release script" "and the answer after it"))
    (let* ((rows (nlk:recall-log :session-id "trove-a" :limit 10))
           (newest (car (last rows))))
      (is (search "before=" (trove-tool "trove_read" "session_id" "trove-a" "limit" 1))
          "a cut answer says how to page older")
      (is-carrying (older (trove-tool "trove_read" "session_id" "trove-a"
                                     "before" (getf newest :seq) "match" "deploy"))
                   ("deploy" "before= reaches the older row")))
    (is (signals-error nodecode-trove:trove-error
          (trove-tool "trove_read" "session_id" "no-such-session"))
        "an unknown session refuses by name")))

(deftest trove-cell-a-missing-query-refuses-before-the-body ()
  (with-trove-log
    (is (signals-error nlk:contract-refusal (trove-tool "trove_search"))
        "trove_search with no query is refused by its contract")
    (is (signals-error nlk:contract-refusal (trove-tool "trove_entry" "seq" "twelve"))
        "a seq that is not an integer is refused too")))

(deftest trove-cell-sessions-and-status-describe-the-log ()
  (with-trove-log
    (is-carrying (directory (trove-tool "trove_sessions"))
                 ("trove-a" "the directory lists the first session")
                 ("trove-b" "and the second")
                 ("message" "with what each holds"))
    (is-carrying (status (trove-tool "trove_status"))
                 ("session" "the status counts the sessions")
                 ("event" "and the durable events")
                 ("limit=20" "and says what it reads under"))))

(deftest trove-cell-evidence-is-cited-and-budgeted ()
  (with-trove-log
    (is-carrying (digest (trove-tool "trove_evidence" "query" "the" "budget" 300))
                 ("Trove evidence" "the digest says what it is")
                 ("[trove-a" "and cites the session"))
    (is (<= (length (trove-tool "trove_evidence" "query" "the" "budget" 300)) 900)
        "a small budget keeps the digest near it")))

;;; --- /trove ----------------------------------------------------------------------

(deftest trove-cell-trove-opens-a-panel-and-answers-its-verbs ()
  (with-trove-log
    (multiple-value-bind (text dialog) (nodecode-trove::run-slash "" "trove-a")
      (is-carrying text ("session" "bare /trove says what the log holds"))
      (is (nle::dialog-json dialog) "and answers a panel")
      (is (= 3 (length (nle::json-array (nle::dialog-json dialog) "rows")))
          "one row to search, one per session"))
    (is-carrying (found (nodecode-trove::run-slash "deploy" "trove-a"))
                 ("deploy" "a query searches every session"))
    (is-carrying (read-back (nodecode-trove::run-slash "read trove-b" "trove-a"))
                 ("the build is green" "read SESSION reads that one"))
    (multiple-value-bind (text dialog) (nodecode-trove::run-slash "status" "trove-a")
      (is (search "limit=" text) "status is the status line")
      (is (null dialog) "and opens no panel"))))
