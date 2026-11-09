;;;; cell-test.lisp --- the effortless cell, offline.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every judge is a canned wire answer and every session seam a stub:
;;;; nothing here dials TypeSafe, a local server or a store. What a test
;;;; reads is the request the cell built, the verdict it took from the
;;;; answer, and the effort it wrote to the session.

(in-package #:nodecode.test)

(define-test-slice "effortless" "EFFORTLESS-CELL-" :start nodecode-effortless:start-cell)

;;; --- the lifecycle every cell owes ------------------------------------------

(define-cell-lifecycle-tests "effortless"
  (:config "judge_backend" "local" "apply" nil)
  (:help :effortless)
  (:command "effortless")
  (:refused ("judge_timeout_seconds" 0)))

;;; --- fixtures ----------------------------------------------------------------

(defparameter +eff-jev-answer+
  "{\"model\":\"jev-latest\",\"answers\":{\"effort\":{\"type\":\"choice\",\"choice\":\"high\",\"confidence\":0.82}}}"
  "System One naming a rung, with its confidence.")

(defparameter +eff-local-answer+
  (format nil "{\"choices\":[{\"message\":{\"content\":~s}}]}"
          "{\"effort\":\"low\",\"sure\":0.9,\"why\":\"one rename\"}")
  "A chat judge's reply, its verdict as JSON inside prose.")

(defmacro with-effortless-judge ((&key (answer '+eff-jev-answer+) (status 200)
                                       (session "eff-s1") (config '()))
                                  &body body)
  "Run BODY with the cell started on CONFIG, every judge answering ANSWER
with STATUS, and the session seams stubbed: REQUESTS each (METHOD URL . ARGS)
the cell sent, oldest first, and PICKED each (EFFORT SESSION) it set."
  `(with-cell-stop ((effortless-start ,@config))
     (let ((requests '()) (picked '()) (answer ,answer) (status ,status))
       (with-stubbed-fdefinitions
           ((nlk:http (method url &rest args)
                      (setf requests (append requests (list (list* method url args))))
                      (values answer status))
            (nle:select-effort (effort &key session-id)
                               (setf picked (append picked (list (list effort session-id))))
                               t)
            (nlk:session-model-effort (session-id) session-id nil)
            (nle:turn () (list :session-id ,session)))
         ,@body))))

(defun eff-header (request name)
  "The value REQUEST carries for header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun eff-body (request)
  "The decoded JSON body REQUEST sent."
  (nlk:decode-json (getf (cddr request) :content)))

;;; --- the heuristics the plugin ran before any judge -------------------------

(deftest effortless-cell-names-the-effort-in-the-message ()
  (is (equal "max" (nodecode-effortless::named-effort "please use max effort here")))
  (is (equal "high" (nodecode-effortless::named-effort "effort: high")))
  (is (equal "xhigh" (nodecode-effortless::named-effort "go at extra xhigh on this one")))
  (is (null (nodecode-effortless::named-effort "don't use max effort")))
  (is (null (nodecode-effortless::named-effort "refactor the parser"))))

(deftest effortless-cell-a-follow-up-keeps-the-effort ()
  (is (nodecode-effortless::follow-up-p "ok"))
  (is (nodecode-effortless::follow-up-p "go on"))
  (is (not (nodecode-effortless::follow-up-p "review the entire repository")))
  (is (equal "max" (nodecode-effortless::apply-bounds "xhigh" "low" "max" 1))
      "bias tips one rung up, ceiling holds")
  (is (equal "high" (nodecode-effortless::nearest-rung "xhigh" '("low" "high")))
      "an off-ladder rung falls to the nearest one below"))

;;; --- the judges ---------------------------------------------------------------

(deftest effortless-cell-asks-jev-and-sets-the-session ()
  (with-effortless-judge (:config ("judge_backend" "jev" "fallback_backend" "none"
                                    "typesafe_key" "ts-cfg"))
    (multiple-value-bind (line verdict)
        (nodecode-effortless::pick-effort "Refactor the entire auth system" :session-id "eff-s1")
      (declare (ignore verdict))
      (is (search "effort: high" line) "the verdict is the answer")
      (is (search "82% sure" line) "with the judge's confidence")
      (destructuring-bind (method url . args) (first requests)
        (is (eq :post method))
        (is (equal "https://api.typesafe.ai/v1/systemone" url))
        (is (equal "Bearer ts-cfg" (cdr (assoc "authorization" (getf args :headers)
                                                :test #'string-equal))))
        (let ((body (nlk:decode-json (getf args :content))))
          (is (equal "jev-latest" (nlk:json-value body :string "model")))
          (is (equal "Refactor the entire auth system"
                     (nlk:json-value body :string "state" "next_message"))
              "the next message is what the judge reads")
          (is (equal "choice" (nlk:json-value body :string "questions" "effort" "type")))))
      (is (equal '(("high" "eff-s1")) picked) "the verdict was written to the session"))))

(deftest effortless-cell-a-local-judge-answers-in-prose ()
  (with-effortless-judge (:answer +eff-local-answer+
                           :config ("judge_backend" "local" "fallback_backend" "none"
                                     "local_model" "tiny"))
    (let ((line (nodecode-effortless::pick-effort "rename this variable" :session-id "eff-s1")))
      (is (search "effort: low" line))
      (is (search "one rename" line) "the judge's own reason is kept")
      (destructuring-bind (method url . args) (first requests)
        (is (eq :post method))
        (is (equal "http://127.0.0.1:8080/v1/chat/completions" url))
        (is (string= "tiny" (nlk:json-value (nlk:decode-json (getf args :content))
                                             :string "model"))))
      (is (equal '(("low" "eff-s1")) picked)))))

(deftest effortless-cell-no-judge-keeps-the-session ()
  (with-effortless-judge (:answer "{\"answers\":{}}"
                           :config ("judge_backend" "jev" "fallback_backend" "none"
                                     "typesafe_key" "ts-cfg"))
    (let ((line (nodecode-effortless::pick-effort "something odd" :session-id "eff-s1")))
      (is (search "no verdict" line))
      (is (eq '() picked) "a judge that named no rung moves nothing"))))

(deftest effortless-cell-a-named-level-needs-no-judge ()
  (with-effortless-judge (:config ("judge_backend" "jev" "fallback_backend" "none"
                                    "typesafe_key" "ts-cfg"))
    (let ((line (nodecode-effortless::pick-effort "write this at max effort" :session-id "eff-s1")))
      (is (search "effort: max" line))
      (is (eq '() requests) "no judge was asked")
      (is (equal '(("max" "eff-s1")) picked)))))

;;; --- what the tool and the command answer ---------------------------------------

(deftest effortless-cell-the-tool-answers-for-the-turn ()
  (with-effortless-judge (:answer +eff-local-answer+
                           :config ("judge_backend" "local" "fallback_backend" "none"
                                     "local_model" "tiny"))
    (let ((line (nodecode-effortless::pick-tool
                 (nodecode-effortless::make-effort-pick
                  :message "go through my whole drive and clean it up" :apply-p t))))
      (is (search "effort: low" line))
      (is (equal '(("low" "eff-s1")) picked) "the turn's own session was set"))))

(deftest effortless-cell-the-command-reports-and-tests ()
  (with-effortless-judge (:answer +eff-local-answer+
                           :config ("judge_backend" "local" "fallback_backend" "none"
                                     "local_model" "tiny"))
    (let ((status (nodecode-effortless::run-slash "" "eff-s1")))
      (is (search "judge local" status) "bare /effortless names the judge in force"))
    (let ((test (nodecode-effortless::run-slash "test" "eff-s1")))
      (is (search "test prompt" test))
      (is (eq '() picked) "a test moves no session"))
    (let ((stats (nodecode-effortless::run-slash "stats" "eff-s1")))
      (is (search "effort" stats)))))
