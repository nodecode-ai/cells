;;;; cell-test.lisp --- the lifecycle, the card, the ledger and the two commands.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved: the cell starts and stops, a task's keywords pick the lane
;;;; they name, a task with no hit falls back to the default lane at confidence
;;;; 0, a pinned lane is honoured and an unknown one refused, /rate labels the
;;;; newest unrated route and the report counts it, a correction files the
;;;; route under the lane the user says it was, and the ledger outlives a
;;;; restart because the file in the folder holds it. Nothing here touches the
;;;; network or a model: classification is keywords, the ledger a file in the
;;;; test's own home.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defparameter *canonical-arms*
  "build=strong/coder, review=review/model, read=long/context, quick=small/fast, plan=plan/model"
  "The models each lane names in these tests: the section's `models', as a user writes it.")

(defun evalroute-test-folder ()
  "The folder one test's ledger is kept in, inside the test's own home."
  (namestring (nlk:home "evalroute-test/")))

(defun evalroute-ledger-file ()
  (merge-pathnames "routes.jsonl" (nlk:home "evalroute-test/")))

(defun evalroute-empty-ledger ()
  "One test's ledger: the file removed, so the test starts from none."
  (when (probe-file (evalroute-ledger-file)) (delete-file (evalroute-ledger-file))))

(defun evalroute-rows ()
  "The rows the ledger holds, newest first, as the cell reads them."
  (nodecode-evalroute::read-rows))

(defun evalroute-start-test (&rest pairs)
  "The cell started on the section these tests share, with PAIRS over it."
  (apply #'evalroute-start
         (list* "models" *canonical-arms* "data_dir" (evalroute-test-folder) pairs)))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "evalroute" "EVALROUTE-CELL-" :start nodecode-evalroute:start-cell)

(define-cell-lifecycle-tests "evalroute"
  (:config "models" *canonical-arms* "data_dir" (evalroute-test-folder))
  (:help :evalroute)
  (:command "route")
  (:running (is (nle::find-tool "evalroute_route") "the tool is on the model")
            (is (nle::find-registered-command "rate") "and /rate is in the catalog")
            (is (equal (nodecode-evalroute::row-lane '(:rated-lane "build"))
                       "build") "a correction outranks the card"))
  (:stopped (is (null (nle::find-tool "evalroute_route")) "stop unregisters the tool")
            (is (null (nle::find-registered-command "rate")) "and takes /rate back"))
  (:refused ("default_lane" "sometimes") ("min_hits" 0))
  (:idle evalroute:evalroute-error
         (evalroute:report)
         (evalroute:lanes)
         (evalroute:status)))

(deftest evalroute-cell-a-missing-models-refuses-by-name ()
  (let ((detail (refusal-text nlk:config-refusal
                               (evalroute-start "data_dir" (evalroute-test-folder)))))
    (is-carrying detail
                 ("models" "the refusal names the member")
                 ("build=<model id>" "and the shape it takes")))
  (is (null (nle::find-tool "evalroute_route")) "a refused start installed nothing"))

;;; --- the card ------------------------------------------------------------------

(deftest evalroute-cell-a-build-task-routes-to-the-coding-arm ()
  (evalroute-empty-ledger)
  (with-cell-stop ((evalroute-start-test))
    (multiple-value-bind (text row)
        (evalroute:route "refactor the parser and fix the regression in the class module")
      (is-carrying text
                   ("build" "the lane its keywords pick")
                   ("strong/coder" "the model the section names for that lane")
                   ("effort high" "and the effort the table starts it at")
                   ("rules match, 6 keyword hits" "the evidence is named, not just the answer")
                   ("next: /model strong/coder, /think high" "the two switches to set")
                   ("route id: r-" "the id /rate takes"))
      (is (equal "build" (getf row :lane)) "the row is the lane the card names")
      (is (equal "high" (getf row :effort)) "at the effort the table starts it at"))
    (let ((rows (evalroute-rows)))
      (is (= 1 (length rows)) "one route is on the ledger")
      (is (equal "" (getf (first rows) :verdict)) "and it is unrated"))
    (is (probe-file (evalroute-ledger-file)) "the ledger is a file in the folder the section names")))

(deftest evalroute-cell-no-keyword-falls-back-to-the-default-lane ()
  (evalroute-empty-ledger)
  (with-cell-stop ((evalroute-start-test))
    (let ((text (evalroute:route "frobnicate the widget")))
      (is-carrying text
                   ("read" "the default lane")
                   ("long/context" "with the model the section names for it")
                   ("no keyword hit: the default lane at confidence 0.00"
                    "the card says it is a fallback, at confidence 0")))))

(deftest evalroute-cell-a-pinned-lane-is-honoured ()
  (evalroute-empty-ledger)
  (with-cell-stop ((evalroute-start-test))
    (let ((text (evalroute:route "frobnicate the widget" :lane "quick")))
      (is-carrying text
                   ("quick" "the pinned lane")
                   ("small/fast" "its model")
                   ("pinned to this lane with --lane" "and the card says it was pinned")))
    (let ((detail (refusal-text evalroute:evalroute-error
                                 (evalroute:route "frobnicate the widget" :lane "nope"))))
      (is-carrying detail
                   ("no lane" "an unknown lane is refused")
                   ("build, review, read, quick, plan" "and the table is named")))))

(deftest evalroute-cell-a-lane-without-a-model-names-where-to-put-one ()
  (evalroute-empty-ledger)
  (with-cell-stop ((evalroute-start-test "models" "build=strong/coder"))
    (let ((text (evalroute:route "frobnicate the widget")))
      (is-carrying text
                   ("no model named" "the lane has no model of its own")
                   ("read=<model id>" "and the card says where a model for this lane goes")
                   ("effort medium" "the card still names the effort")))))

(deftest evalroute-cell-the-tool-answers-the-card ()
  (evalroute-empty-ledger)
  (with-cell-stop ((evalroute-start-test))
    (let* ((entry (nle::find-tool "evalroute_route"))
           (answer (funcall (third entry)
                            (nodecode-evalroute::make-route-request
                             :task "audit this diff for a vulnerability" :lane nil))))
      (is (stringp answer) "the tool answers text")
      (is-carrying answer
                   ("review" "the lane its keywords pick")
                   ("review/model" "and the model the section names for it")))))

;;; --- the flywheel --------------------------------------------------------------

(deftest evalroute-cell-rate-labels-the-pending-route ()
  (evalroute-empty-ledger)
  (with-cell-stop ((evalroute-start-test))
    (evalroute:route "refactor the parser and fix the bug")
    (let ((line (evalroute:rate "pass" :note "shipped")))
      (is-carrying line
                   ("-> pass" "the label lands")
                   ("(shipped)" "with its note")
                   ("100% pass over 1 rated" "and the lane's rate is the answer")))
    (is (equal "pass" (getf (first (evalroute-rows)) :verdict)) "the row carries the verdict")
    (is (equal "shipped" (getf (first (evalroute-rows)) :note)) "and the note")
    (let ((report (evalroute:report)))
      (is-carrying report
                   ("1 pass" "the books count it")
                   ("100% pass over 1 rated" "as a pass rate")))))

(deftest evalroute-cell-rate-with-nothing-pending-refuses ()
  (evalroute-empty-ledger)
  (with-cell-stop ((evalroute-start-test))
    (let ((pending (refusal-text evalroute:evalroute-error (evalroute:rate "pass")))
          (word (refusal-text evalroute:evalroute-error (evalroute:rate "maybe"))))
      (is-carrying pending
                   ("no pending route" "rating with nothing routed says so")
                   ("/route a task first" "and names the step before it"))
      (is-carrying word ("rate it pass, fail or skip" "a word outside the three refuses")))))

(deftest evalroute-cell-a-lane-correction-files-the-row-where-the-user-says ()
  (evalroute-empty-ledger)
  (with-cell-stop ((evalroute-start-test))
    (evalroute:route "frobnicate the widget")
    (let ((line (evalroute:rate "fail" :lane "quick" :note "it was a rename")))
      (is-carrying line
                   ("filed as quick" "the correction is stated")
                   ("0% pass over 1 rated" "and the lane's books carry it")))
    (is (equal "quick" (getf (first (evalroute-rows)) :rated-lane)) "the row carries the correction")
    (let ((report (evalroute:report)))
      (is-carrying report
                   ("quick (small/fast @ low)  1 route, 0 pass, 1 fail"
                    "the lane the card did not pick now owns it")))))

(deftest evalroute-cell-the-ledger-outlives-the-cell ()
  (evalroute-empty-ledger)
  (with-cell-stop ((evalroute-start-test))
    (evalroute:route "refactor the parser")
    (evalroute:rate "pass"))
  (with-cell-stop ((evalroute-start-test))
    (let ((report (evalroute:report)))
      (is-carrying report ("1 pass" "a second start reads the routes the first one labelled"))
      (is (equal "pass" (getf (first (evalroute-rows)) :verdict)) "the row keeps its verdict"))))

(deftest evalroute-cell-max-routes-bounds-the-ledger ()
  (evalroute-empty-ledger)
  (with-cell-stop ((evalroute-start-test "max_routes" 2))
    (evalroute:route "refactor the parser")
    (evalroute:route "rename this")
    (evalroute:route "audit the diff")
    (let ((rows (evalroute-rows)))
      (is (= 2 (length rows)) "the ledger keeps the newest two")
      (is (equal "review" (getf (first rows) :lane)) "newest first"))))

;;; --- the commands --------------------------------------------------------------

(deftest evalroute-cell-route-command-reads-the-flags ()
  (evalroute-empty-ledger)
  (with-cell-stop ((evalroute-start-test))
    (let ((typed (nodecode-evalroute::run-route "frobnicate the widget" nil))
          (pinned (nodecode-evalroute::run-route "--lane quick frobnicate the widget" nil))
          (table (nodecode-evalroute::run-route "lanes" nil)))
      (is-carrying typed
                   ("read" "the words are the task")
                   ("no keyword hit" "and it falls back to the default lane"))
      (is-carrying pinned
                   ("quick" "--lane takes the next word")
                   ("pinned to this lane" "and the rest is the task"))
      (is-carrying table
                   ("build - implement" "lanes prints the table")
                   ("quick - a short answer" "one line per lane")))))

(deftest evalroute-cell-rate-command-takes-the-verdict-and-the-note ()
  (evalroute-empty-ledger)
  (with-cell-stop ((evalroute-start-test))
    (evalroute:route "refactor the parser")
    (let ((filed (nodecode-evalroute::run-rate "fail --lane build --note \"wrong scope\"" nil)))
      (is-carrying filed
                   ("-> fail" "the command files the same row the verb does")
                   ("(wrong scope)" "with the note --note carried")
                   ("filed as build" "and the correction")))
    (evalroute:route "refactor the other parser")
    (let ((bare (nodecode-evalroute::run-rate "pass it worked" nil)))
      (is-carrying bare
                   ("-> pass" "the verdict alone")
                   ("(it worked)" "words after it are the note when --note is absent")))))
