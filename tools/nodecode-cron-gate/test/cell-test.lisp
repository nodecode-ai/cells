;;;; cell-test.lisp --- the cron-gate cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every exchange with TypeSafe is a stubbed dex:request (NLK:HTTP's own
;;;; call), every key a stubbed NLE::CREDENTIAL-ENV, every gate a file under
;;;; the run's scratch home: nothing touches the network, the environment or
;;;; the operator's files.

(in-package #:nodecode.test)

(define-test-slice "cron-gate" "CRON-GATE-CELL-" :start nodecode-cron-gate:start-cell)

(define-cell-lifecycle-tests "cron-gate"
  (:help :cron-gate)
  (:command "cron-gate")
  (:refused ("timeout_seconds" 0) ("max_state_chars" "lots"))
  (:idle cron-gate:cron-gate-error
         (cron-gate:gates)
         (cron-gate:check "heartbeat")))

;;; --- fixtures ---------------------------------------------------------------------

(defparameter +skip-answer+
  "{\"model\":\"jev-latest\",\"answers\":{\"useful\":{\"type\":\"noul\",\"noul\":0.05}},\"usage\":{\"input_tokens\":40,\"output_tokens\":0}}"
  "Jev, confident the run is pointless.")

(defparameter +wake-answer+
  "{\"model\":\"jev-latest\",\"answers\":{\"useful\":{\"type\":\"noul\",\"noul\":0.86}},\"usage\":{\"input_tokens\":40,\"output_tokens\":0}}"
  "Jev, confident the run would do work.")

(defparameter +junk-answer+
  "{\"model\":\"jev-latest\",\"answers\":{},\"usage\":{}}"
  "An answer that names no useful judgment.")

(defmacro with-cron-gate ((requests &key (answers '(list +skip-answer+)) (statuses '(list 200))
                                         (env '(("TYPESAFE_API_KEY" . "gate-env"))) config)
                          &body body)
  "BODY with the cell started on CONFIG, ENV as the environment, and Jev
answering ANSWERS with STATUSES in turn (the last repeated): REQUESTS each
(METHOD URL HEADERS CONTENT) sent, oldest first."
  `(with-cell-stop ((cron-gate-start ,@config))
     (let ((,requests '()) (answers ,answers) (statuses ,statuses))
       (with-stubbed-fdefinitions
           ((nle::credential-env (name) (cdr (assoc name ',env :test #'equal)))
            (dex:request (url &rest arguments)
                         (setf ,requests (append ,requests
                                                 (list (list (getf arguments :method) url
                                                             (getf arguments :headers)
                                                             (getf arguments :content)))))
                         (values (if (rest answers) (pop answers) (first answers))
                                 (if (rest statuses) (pop statuses) (first statuses))
                                 (make-hash-table :test 'equal))))
         ,@body))))

(defun gate-header (headers name)
  "The value HEADERS carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun gate-body (request)
  "REQUEST's decoded JSON body."
  (nlk:decode-json (fourth request)))

(defun gate-state (request)
  "The state REQUEST carried, decoded."
  (nlk:json-value (gate-body request) :string "state"))

(defun scratch-file (name text)
  "A file under the run's scratch home holding TEXT, by native namestring."
  (let ((path (nlk:home (format nil "scratch/~a" name))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string text out))
    (namestring path)))

;;; --- what a gate asks ----------------------------------------------------------------

(deftest cron-gate-cell-posts-the-question-over-the-snapshot ()
  (let ((notes (scratch-file "NOTES.md" "waiting on approval since Tuesday")))
    (with-cron-gate (requests :answers (list +skip-answer+))
      (cron-gate:gate "posts" :question "Running now would make real new progress."
                             :sources (list (list :file notes) (list :text "nightly synthesis"))
                             :skip-below 0.2 :max-skips 4)
      (multiple-value-bind (text decision) (cron-gate:check "posts")
        (is (eq :skip decision) "P(useful) 0.05 is under the 0.2 bar")
        (is (search "skip" text)))
      (destructuring-bind (method url headers content) (first requests)
        (declare (ignore content))
        (is (eq :post method))
        (is (equal "https://api.typesafe.ai/v1/systemone" url))
        (is (equal "Bearer gate-env" (gate-header headers "authorization")))
        (is (equal "application/json" (gate-header headers "content-type"))))
      (let* ((body (gate-body (first requests)))
             (state (gate-state (first requests)))
             (question (nlk:json-value body :object "questions" "useful")))
        (is (equal "jev-latest" (nlk:json-value body :string "model")))
        (is (equal "noul" (nlk:json-value question :string "type")) "one bool is the wire's noul")
        (is (equal "Running now would make real new progress."
                   (nlk:json-value question :string "instructions")))
        (is (search "waiting on approval since Tuesday" state) "the file is what Jev reads")
        (is (search "nightly synthesis" state))
        (is (search "Hours since the agent last ran" state) "and the time since the last wake")))))

(deftest cron-gate-cell-tails-a-file-and-caps-the-snapshot ()
  (let ((log (scratch-file "log.jsonl" (format nil "~{~a~^~%~}" (loop for i below 400 collect (format nil "line ~d of the log" i))))))
    (with-cron-gate (requests)
      (cron-gate:gate "tailed" :question "Worth a run?" :sources (list (list :file log :tail 60)))
      (cron-gate:check "tailed")
      (let ((state (gate-state (first requests))))
        (is (search "line 399 of the log" state) "the end of the file is kept")
        (is (not (search "line 0 of the log" state)) "the beginning is what :tail drops")
        (is (<= (length state) 6000) "the snapshot is capped at the section's max_state_chars")))))

(deftest cron-gate-cell-reads-a-localhost-url-and-names-any-other ()
  (with-cron-gate (requests :answers (list "{\"status\":\"green\"}" +skip-answer+))
    (cron-gate:gate "local" :question "Worth a run?"
                           :sources (list (list :url "http://127.0.0.1:8888/state")
                                          (list :url "https://example.test/state")))
    (cron-gate:check "local")
    (is (equal "http://127.0.0.1:8888/state" (second (first requests))) "the localhost URL is fetched")
    (is (eq :get (first (first requests))))
    (let ((state (gate-state (second requests))))
      (is (search "green" state) "its body is part of the snapshot")
      (is (search "(skipped: only localhost URLs are allowed)" state) "a remote URL is named, not fetched")
      (is (null (third requests)) "and nothing else was asked of the network"))))

(deftest cron-gate-cell-says-whether-paths-changed-since-the-last-wake ()
  (let ((changed (scratch-file "src.lisp" "(defun f () 1)")))
    (with-cron-gate (requests :answers (list +wake-answer+ +skip-answer+))
      (cron-gate:gate "watch" :question "Worth a run?" :sources (list (list :changed (list changed))))
      (cron-gate:check "watch")
      (is (search "true" (gate-state (first requests))) "nothing woke before, so everything is new")
      (cron-gate:check "watch")
      (is (search "false" (gate-state (second requests))) "the file is older than the last wake"))))

;;; --- the fail-open rules ----------------------------------------------------------------

(deftest cron-gate-cell-wakes-without-a-key-or-an-answer ()
  (with-cron-gate (requests :env ())
    (cron-gate:gate "keyless" :question "Worth a run?" :sources (list (list :text "x")))
    (multiple-value-bind (text decision) (cron-gate:check "keyless")
      (is (eq :wake decision) "no key wakes the run")
      (is (search "no key" text)))
    (is (null requests) "and nothing is sent"))
  (with-cron-gate (requests :answers (list "{\"detail\":\"busy\"}") :statuses (list 503))
    (cron-gate:gate "broken" :question "Worth a run?" :sources (list (list :text "x")))
    (multiple-value-bind (text decision) (cron-gate:check "broken")
      (is (eq :wake decision) "a 503 wakes the run")
      (is (search "HTTP 503" text))))
  (with-cron-gate (requests :answers (list +junk-answer+))
    (cron-gate:gate "silent" :question "Worth a run?" :sources (list (list :text "x")))
    (multiple-value-bind (text decision) (cron-gate:check "silent")
      (is (eq :wake decision) "an answer with no judgment in it wakes the run")
      (is (search "nothing for the useful question" text)))))

(deftest cron-gate-cell-wakes-anyway-after-the-skip-cap ()
  (with-cron-gate (requests :answers (list +skip-answer+))
    (cron-gate:gate "capped" :question "Worth a run?" :sources (list (list :text "x")) :max-skips 1)
    (is (eq :skip (nth-value 1 (cron-gate:check "capped"))) "the first skip stands")
    (multiple-value-bind (text decision) (cron-gate:check "capped")
      (is (eq :wake decision) "the second wakes, so a job cannot stall silently")
      (is (search "skip cap" text)))))

;;; --- what it remembers, and the tool -----------------------------------------------------

(deftest cron-gate-cell-counts-runs-and-skips ()
  (let ((name (format nil "counted-~a" (mod (get-universal-time) 1000000))))
    (with-cron-gate (requests :answers (list +skip-answer+ +wake-answer+ +skip-answer+))
      (cron-gate:gate name :question "Worth a run?" :sources (list (list :text "x")))
      (cron-gate:check name)
      (cron-gate:check name)
      (let ((listed (cron-gate:gates))
            (counted (format nil "~a: 2 runs, 1 skipped (50%)" name))
            (entry (cell-entry "nodecode-cron-gate" "cron-gate" "")))
        (is (search counted listed) "the log is the count")
        (is (search "useful=0.86" listed) "the last decision's note")
        (is (search (format nil "~a: 2 runs, 1 skipped" name) entry) "the command lists the same"))
      (let ((checked (cell-entry "nodecode-cron-gate" "cron-gate" name)))
        (is (search (format nil "~a: skip" name) checked) "and with a name it decides now")))))

(deftest cron-gate-cell-tool-defines-checks-and-forgets ()
  (with-cron-gate (requests)
    (let ((defined (nodecode-cron-gate::cron-gate-tool
                    (nlk:decode-json "{\"action\":\"define\",\"name\":\"tooled\",\"question\":\"Worth a run?\",\"sources\":[{\"text\":\"x\"}]}"))))
      (is (search "cron-gate tooled" defined) defined))
    (is (eq :skip (nth-value 1 (nodecode-cron-gate::cron-gate-tool
                                (nlk:decode-json "{\"action\":\"check\",\"name\":\"tooled\"}")))))
    (is (search "1 run" (nodecode-cron-gate::cron-gate-tool
                         (nlk:decode-json "{\"action\":\"status\",\"name\":\"tooled\"}"))))
    (is (search "forgotten" (nodecode-cron-gate::cron-gate-tool
                             (nlk:decode-json "{\"action\":\"forget\",\"name\":\"tooled\"}"))))
    (is (search "no such gate" (cron-gate:forget "tooled")))
    (let ((poked (nodecode-cron-gate::cron-gate-tool (nlk:decode-json "{\"action\":\"poke\"}"))))
      (is (search "is none" poked) "an unknown action is said, not signalled"))))

(deftest cron-gate-cell-refuses-a-gate-that-makes-no-judgment ()
  (with-cron-gate (requests)
    (flet ((refused (&rest arguments)
             (handler-case (progn (apply #'cron-gate:gate arguments) nil)
               (cron-gate:cron-gate-error (condition) (princ-to-string condition)))))
      (is (search "a gate name is" (refused "Not A Name" :question "?")) "a name of its own alphabet")
      (is (search "a gate needs a question" (refused "empty" :question "  ")))
      (is (search "probability from 0 to 1" (refused "wide" :question "?" :skip-below 3)))
      (is (search "whole number above 0" (refused "cap" :question "?" :max-skips 0)))
      (is (search "a source is" (refused "bare" :question "?" :sources (list (list :nonsense "x"))))))
    (is (null requests) "and nothing was sent")))
