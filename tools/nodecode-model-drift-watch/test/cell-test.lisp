;;;; cell-test.lisp --- lifecycle, the grading rules, a baseline, a drop, and the verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here reaches a model or the network: the one call the cell makes
;;;; goes through *SAMPLE*, bound to a scripted answerer that reads the
;;;; question set and answers it right, wrong, or with an error. What is under
;;;; test is what the cell owns -- how an answer is cut out and graded, when a
;;;; run joins the baseline, and when a drop is called drift.

(in-package #:nodecode.test)

(define-test-slice "model-drift-watch" "MODEL-DRIFT-WATCH-CELL-"
  :start nodecode-model-drift-watch:start-cell)

(define-cell-lifecycle-tests "model-drift-watch"
  (:help :model-drift-watch)
  (:command "model-drift-watch")
  (:running
   (is (nle::find-tool "model_drift") "the model_drift tool is registered")
   (is (= 5 (nodecode-model-drift-watch::setting :baseline-runs)) "five runs make a baseline"))
  (:stopped
   (is (null (nle::find-tool "model_drift")) "and stop takes it back"))
  (:refused ("max_tokens" 0)
            ("baseline_runs" 1)
            ("alpha" "0.5")
            ("keep_runs" 2))
  (:idle nodecode-model-drift-watch:model-drift-watch-error
         (model-drift-watch:status)
         (model-drift-watch:check)))

;;; --- the scripted model -------------------------------------------------------------

(defun suite-question (user)
  "The question of the suite USER asks, or NIL."
  (find user nodecode-model-drift-watch::*suite*
        :key (lambda (question) (getf question :prompt)) :test #'string=))

(defun right-reply (user)
  "The answer USER is graded against, written the way a model would write it."
  (let* ((question (suite-question user))
         (answer (destructuring-bind (type &rest arguments) (getf question :grader)
                   (ecase type
                     (:number (format nil "~a" (first arguments)))
                     (:exact (format nil "~a" (first arguments)))
                     (:regex (string-trim "^$" (first arguments)))
                     (:words (format nil "~{~a~^ ~}"
                                     (append (make-list (1- (first arguments))
                                                        :initial-element "yes")
                                             (list (string-capitalize
                                                    (first (second arguments)))))))
                     (:json (format nil "[~{\"~a\"~^,~}]" (first arguments)))))))
    (if (eq :full (getf question :extract))
        answer
        (format nil "Working.~%ANSWER: ~a" answer))))

(defun wrong-reply (user)
  "An answer no question in the suite accepts."
  (declare (ignore user))
  "nope")

(defvar *model* #'right-reply
  "The answerer this test's model is.")

(defvar *asked* 0
  "How many questions the scripted model was asked.")

(defun scripted-model (system user max-tokens)
  "The one model call the cell makes, answered by *MODEL*."
  (declare (ignore system max-tokens))
  (incf *asked*)
  (funcall *model* user))

(defun drift-file ()
  "A run file of this test's own, in the scratch home."
  (let ((path (nlk:home "model-drift-watch-test/runs.jsonl")))
    (ensure-directories-exist path)
    (when (probe-file path) (delete-file path))
    path))

(defmacro with-drift ((&key (model '(function right-reply))) &body body)
  "BODY with the cell started on its defaults (or PAIRS), a run file of its
own, and the model answering as MODEL."
  `(let* ((*model* ,model)
          (*asked* 0)
          (path (drift-file)))
     (with-stubbed-fdefinitions
         ((nodecode-model-drift-watch::state-path () path)
          (nodecode-model-drift-watch::target-name () "fixture:model"))
       (let ((nodecode-model-drift-watch::*sample* #'scripted-model))
         (with-cell-stop ((model-drift-watch-start))
           (unwind-protect (progn ,@body)
             (when (probe-file path) (delete-file path))))))))

(defun drift-runs ()
  "The runs the cell recorded, oldest first."
  (nodecode-model-drift-watch::runs))

(defun drift-handler ()
  "/model-drift-watch's RUN, as the catalog's headless arm answers it."
  (nodecode.evolved::slash-command-headless (nle::find-registered-command "model-drift-watch")))

(defun drift-tool-text (arguments)
  "The model_drift tool called as a provider sends it, answered as text."
  (princ-to-string (nle::message-content (execute-wire-call "call-drift" "model_drift" arguments))))

;;; --- the grading rules --------------------------------------------------------------

(deftest model-drift-watch-cell-an-answer-is-cut-out-and-graded-by-rule ()
  (with-cell-stop ((model-drift-watch-start))
    (let ((arithmetic (find "r01-arith" nodecode-model-drift-watch::*suite*
                               :key (lambda (question) (getf question :id)) :test #'string=))
             (colors (find "i01-colors" nodecode-model-drift-watch::*suite*
                           :key (lambda (question) (getf question :id)) :test #'string=)))
         (is (equal "134022" (nodecode-model-drift-watch::extracted
                               arithmetic "4,837 x 29 is 140,273, less 6,251.~%ANSWER: 134022")) "the text after the last ANSWER: line is the answer")
         (is (equal "134,022" (nodecode-model-drift-watch::extracted
                                arithmetic "`ANSWER: 134,022`")) "a code fence is not part of the answer")
         (is (equal "134022" (let ((answer (nodecode-model-drift-watch::extracted
                                             arithmetic "ANSWER: 134,022")))
                                (format nil "~a" (nodecode-model-drift-watch::number-in answer)))) "a number with commas reads as a number")
         (is (nodecode-model-drift-watch::grade arithmetic "ANSWER: 134022") "the right number passes")
         (is (not (nodecode-model-drift-watch::grade arithmetic "ANSWER: 134,023")) "a near miss does not")
         (is (nodecode-model-drift-watch::grade colors "[ \"red\", \"green\", \"blue\" ]") "a JSON array passes in any order and spacing")
         (is (not (nodecode-model-drift-watch::grade colors "[\"red\", \"green\"]")) "a short array does not")
         (is (not (nodecode-model-drift-watch::grade colors "The primary colors are red, green and blue.")) "prose is not a JSON array"))))

(deftest model-drift-watch-cell-every-question-grades-its-own-reference ()
  (with-cell-stop ((model-drift-watch-start))
    (dolist (question nodecode-model-drift-watch::*suite*)
         (let ((reply (right-reply (getf question :prompt))))
           (multiple-value-bind (passed detail)
               (nodecode-model-drift-watch::grade
                question (nodecode-model-drift-watch::extracted question reply))
             (is passed (format nil "~a grades its own reference answer~@[: ~a~]"
                                (getf question :id) detail)))
           (multiple-value-bind (passed detail)
               (nodecode-model-drift-watch::grade
                question (nodecode-model-drift-watch::extracted question "nope"))
             (declare (ignore detail))
             (is (not passed) (format nil "~a rejects a wrong answer" (getf question :id))))))))

;;; --- a baseline, then a drop ---------------------------------------------------------

(deftest model-drift-watch-cell-five-clean-runs-make-the-baseline ()
  (with-drift ()
    (dotimes (n 5)
      (let ((report (model-drift-watch:check)))
        (is (search (format nil "baseline run ~d of 5" (1+ n)) report) "each clean run is one baseline run")
        (is (not (search "DRIFT" report)) "and nothing is called drift while the baseline is short")))
    (is (= 5 (length (drift-runs))) "five runs are recorded")
    (is (= 70 *asked*) "fourteen questions a run, five runs")
    (is (= 5 (length (nodecode-model-drift-watch::runs))) "and the run file holds them")))

(deftest model-drift-watch-cell-a-real-drop-is-called-drift ()
  (with-drift ()
    (dotimes (n 5) (model-drift-watch:check))
    (setf *model* #'wrong-reply)
    (let ((report (model-drift-watch:check)))
      (is (search "DRIFT" report) "a run that loses every question is drift")
      (is (search "A second run confirmed it" report) "and with confirm on it is confirmed first")
      (is (search "Regressed: " report) "the questions that fell are named"))
    (is (= 7 (length (drift-runs))) "six checks, and the confirmation run, are recorded")
    (is (search "DRIFT" (model-drift-watch:status)) "status reads the same verdict back")))

(deftest model-drift-watch-cell-a-small-drop-is-not-drift ()
  (with-drift ()
    (dotimes (n 5) (model-drift-watch:check))
    (let ((*model* (lambda (user) (if (string= user "Compute 4,837 × 29 − 6,251.") "ANSWER: 1" (right-reply user)))))
      (let ((report (model-drift-watch:check)))
        (is (search "no drift" report) "one question of fourteen is under the bar")
        (is (search "Regressed: r01-arith" report) "but the question that fell is named")))
    (is (= 6 (length (drift-runs))) "no confirmation run was spent")))

;;; --- a failed measurement, a changed model, and the free verbs -----------------------

(deftest model-drift-watch-cell-provider-failures-are-a-failed-measurement ()
  (with-drift (:model (lambda (user) (declare (ignore user)) (error "the provider is down")))
    (let ((report (model-drift-watch:check)))
      (is (search "could not measure" report) "an unusable run says so")
      (is (not (search "DRIFT" report)) "and is never drift")
      (is (search "first: the provider is down" report) "the first failure is quoted"))
    (is (search "baseline: 0 of 5" (model-drift-watch:status)) "a failed measurement does not join the baseline")))

(deftest model-drift-watch-cell-a-changed-target-starts-the-baseline-again ()
  (with-drift ()
    (dotimes (n 5) (model-drift-watch:check))
    (with-stubbed-fdefinitions ((nodecode-model-drift-watch::target-name () "fixture:other-model"))
      (let ((report (model-drift-watch:check)))
        (is (search "baseline run 1 of 5 for fixture:other-model" report) "the new model starts at its first run")
        (is (search "The watched model changed" report) "and the report says why")))
    (is (search "fixture:other-model" (model-drift-watch:status)) "status reports the newest model")))

(deftest model-drift-watch-cell-the-free-verbs-ask-no-question ()
  (with-drift ()
    (is (search "nothing measured yet" (model-drift-watch:status)) "status before a check")
    (let ((estimate (model-drift-watch:estimate)))
      (is (search "14 questions" estimate) "the estimate counts the questions")
      (is (search "at most" estimate) "and gives a ceiling in tokens"))
    (is (search "r01-arith" (model-drift-watch:questions)) "the question set is listed")
    (is (= 0 *asked*) "none of the three asked the model")
    (model-drift-watch:check)
    (is (search "deleted 1 recorded run(s)" (model-drift-watch:reset)) "reset drops the runs")
    (is (null (drift-runs)) "and the file is gone")))

;;; --- the command and the tool ---------------------------------------------------------

(deftest model-drift-watch-cell-the-command-and-the-tool-answer-the-report ()
  (with-drift ()
    (let ((handler (drift-handler)))
      (is (search "baseline run 1 of 5" (funcall handler "run" "s1")) "the command runs a check")
      (is (search "last check:" (funcall handler "" "s1")) "a bare line is status, and spends nothing")
      (is (search "deleted 1 recorded run(s)" (funcall handler "reset" "s1")) "reset clears the file")
      (is (search "Unknown option: nonsense" (funcall handler "nonsense" "s1")) "an unknown word says so")
      (is (search "baseline run 1 of 5 for fixture:model"
                  (drift-tool-text "{\"action\": \"run\"}")) "the tool runs a check and answers the report")
      (is (search "baseline: 1 of 5" (drift-tool-text "{\"action\": \"status\"}")) "status answers the baseline and spends nothing")
      (is (search "deleted 1 recorded run(s)" (drift-tool-text "{\"action\": \"reset\"}")) "reset through the tool clears the file"))))
