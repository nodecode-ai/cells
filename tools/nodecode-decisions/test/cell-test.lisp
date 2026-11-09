;;;; cell-test.lisp --- the decisions cell, offline.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No endpoint is dialled and no thread is waited on: the one network call
;;;; is bound to a scripted reply and a background job runs on this thread, so
;;;; "the request carried the questions" and "the job completed" are facts a
;;;; test reads rather than a provider's mood.

(in-package #:nodecode.test)

(define-test-slice "decisions" "DECISIONS-CELL-" :start nodecode-decisions:start-cell)

;;; --- the lifecycle every cell owes ------------------------------------------

(define-cell-lifecycle-tests "decisions"
  (:config "api_key" "sk-test")
  (:help :decisions)
  (:command "decisions")
  (:refused ("timeout_seconds" 0)))

;;; --- the wire, scripted -----------------------------------------------------

(defvar *decisions-sent* nil
  "The keyword arguments the cell handed the network call, for a test to read.")

(defun decisions-question (instructions)
  "One predicate question, as a provider sends the tool's questions array."
  (nlk:make-json-object "type" "predicate" "instructions" instructions))

(defun decisions-reply-json (&optional (count 1))
  "A well-formed reply carrying COUNT answers and one member the cell does not know."
  (format nil "{\"answers\": [~{~a~^, ~}], \"model\": \"gpt-6-luna\"}"
          (loop repeat count
                collect "{\"type\": \"predicate\", \"name\": null, \"probability\": 0.75}")))

(defmacro with-decisions-answer ((body &key (status 200)) &body forms)
  "Run FORMS with the cell's one network call answering BODY at STATUS, and what it
was sent left in *DECISIONS-SENT*."
  `(let ((nodecode-decisions::*http*
           (lambda (method url &rest arguments)
             (declare (ignore method url))
             (setf *decisions-sent* arguments)
             (values ,body ,status nil nil)))
         (*decisions-sent* nil))
     ,@forms))

(defun decisions-call (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "decisions-call" name arguments))))

(defun decisions-submit (&rest pairs)
  "decisions_submit with PAIRS as its JSON body; => the result text."
  (decisions-call "decisions_submit"
                   (nlk:encode-json-object (apply #'nlk:make-json-object pairs))))

(defun decisions-question-text ()
  "The request body the cell sent, as text."
  (nlk:decode-octets (getf *decisions-sent* :content)))

(defun decisions-owner (id owner)
  "File job ID under OWNER, so a test can read another session's job."
  (setf (car (gethash id nodecode-decisions::*jobs*)) owner))

;;; --- a submission, foreground ------------------------------------------------

(deftest decisions-cell-a-foreground-submission-answers-probabilities ()
  (with-cell-stop ((decisions-start "api_key" "sk-test"))
    (with-decisions-answer ((decisions-reply-json 2))
      (let* ((text (decisions-submit "input" "a billing ticket from a customer"
                                      "questions" (vector (decisions-question "is it a support ticket?")
                                                           (decisions-question "is it urgent?"))
                                      "background" nil))
             (answer (nlk:decode-json text)))
        (is (equal "completed" (nlk:json-value answer :string "state")) "a foreground submission is finished when it answers")
        (let ((reply (nlk:json-value answer :object "answers")))
          (is (= 2 (length (nlk:json-array reply "answers"))) "every question has an answer")
          (is (equal "gpt-6-luna" (nlk:json-value reply :string "model")) "and the endpoint's whole reply is kept, a member the cell does not know included"))
        (let ((sent (decisions-question-text)))
          (is (search "\"model\":\"gpt-6-luna\"" sent) "the request names the model")
          (is (search "a billing ticket from a customer" sent) "and carries the input")
          (is (search "is it urgent?" sent) "and every question, as typed"))))))

;;; --- a submission, background ------------------------------------------------

(deftest decisions-cell-a-background-submission-is-a-job-to-read-back ()
  (with-cell-stop ((decisions-start "api_key" "sk-test"))
    (let ((nodecode-decisions::*spawn* #'funcall))
      (with-decisions-answer ((decisions-reply-json 1))
        (let* ((submitted (nlk:decode-json
                           (decisions-submit "input" "the deploy is slow"
                                             "questions" (vector (decisions-question "is it urgent?")))))
               (id (nlk:json-value submitted :string "job_id")))
          (is (plusp (length id)) "a background submission answers a job id")
          (let ((again (nlk:decode-json
                        (decisions-call "decisions_status"
                                        (nlk:encode-json-object (nlk:json-object "job_id" id))))))
            (is (equal "completed" (nlk:json-value again :string "state")) "and the job carries the answer once it lands")
            (is (nlk:json-value again :object "answers") "with the reply object"))
          (is (search id (cell-entry "nodecode-decisions" "decisions" "" "s1")) "/decisions lists the job it started"))))))

(deftest decisions-cell-a-session-reads-its-own-jobs-only ()
  (with-cell-stop ((decisions-start "api_key" "sk-test"))
    (let ((nodecode-decisions::*spawn* #'funcall))
      (with-decisions-answer ((decisions-reply-json 1))
        (let* ((submitted (nlk:decode-json
                           (decisions-submit "input" "the deploy is slow"
                                             "questions" (vector (decisions-question "is it urgent?")))))
               (id (nlk:json-value submitted :string "job_id")))
          (decisions-owner id "another-session")
          (is (search "no job"
                      (decisions-call "decisions_status"
                                      (nlk:encode-json-object (nlk:json-object "job_id" id)))) "a job another session started is not read here")
          (is (search "no job" (decisions-call "decisions_status"
                                                  (nlk:encode-json-object
                                                   (nlk:json-object "job_id" "0000")))) "nor is one that never existed"))))))

;;; --- the refusals a caller repairs from ---------------------------------------

(deftest decisions-cell-without-a-key-says-what-to-set ()
  (with-cell-stop ((decisions-start "api_key_env" "NODECODE_DECISIONS_TEST_UNSET_KEY"))
    (with-decisions-answer ((decisions-reply-json 1))
      (let ((text (decisions-submit "input" "hello"
                                     "questions" (vector (decisions-question "is it true?")))))
        (is (search "no API key" text) "a section with no key refuses the call")
        (is (search "api_key" text) "and names the setting that fills it")
        (is (null (getf *decisions-sent* :content)) "without a byte leaving")))))

(deftest decisions-cell-a-malformed-reply-is-refused ()
  (with-cell-stop ((decisions-start "api_key" "sk-test"))
    (with-decisions-answer ((decisions-reply-json 1))
      (let ((text (decisions-submit "input" "hello"
                                     "questions" (vector (decisions-question "is it true?")
                                                          (decisions-question "is it false?"))
                                     "background" nil)))
        (is (search "1 answers for 2 questions" text) "a reply that answers fewer questions than were asked is refused")))
    (with-decisions-answer ("{\"answers\": [{\"type\": \"predicate\", \"name\": null, \"probability\": 7}]}")
      (let ((text (decisions-submit "input" "hello"
                                     "questions" (vector (decisions-question "is it true?"))
                                     "background" nil)))
        (is (search "probability between 0 and 1" text) "a probability outside 0..1 is refused, not read as a confidence")))))

(deftest decisions-cell-an-http-status-says-what-to-do ()
  (with-cell-stop ((decisions-start "api_key" "sk-test"))
    (with-decisions-answer ("{}" :status 401)
      (let ((text (decisions-submit "input" "hello"
                                     "questions" (vector (decisions-question "is it true?"))
                                     "background" nil)))
        (is (search "401" text) "the status is named")
        (is (search "decisions.api_key" text) "with the setting that fixes a refused key")
        (is (search "not retried" text) "and that nothing was retried")))))

(deftest decisions-cell-the-running-ceiling-refuses-the-next-submission ()
  (with-cell-stop ((decisions-start "api_key" "sk-test"))
    (let ((nodecode-decisions::*spawn* (lambda (function) (declare (ignore function)) nil)))
      (with-decisions-answer ((decisions-reply-json 1))
        (dotimes (i 4)
          (decisions-submit "input" "hello"
                             "questions" (vector (decisions-question "is it true?"))))
        (is (search "already running"
                    (decisions-submit "input" "hello"
                                       "questions" (vector (decisions-question "is it true?")))) "the fifth submission at once is refused, as the original did")
        (is (= 4 (length nodecode-decisions::*order*)) "and four are running")))))
