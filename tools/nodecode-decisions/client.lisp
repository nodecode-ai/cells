;;;; client.lisp --- one predicate request, one bounded reply.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Small on purpose: one POST, no retry, one request shape and its reply
;;;; checked field by field. What comes back is the endpoint's own decoded
;;;; JSON object, extra members and all, because the callers of a decision ask
;;;; for more than the probabilities it carries.

(in-package #:nodecode-decisions)

;;; --- the two boundaries -----------------------------------------------------------

(nlk:define-contract decision-question
  (:doc "One yes/no question about the input: \"is this a support ticket?\", \"is it
urgent?\". Ask it in the affirmative; the answer is the probability that it is true.")
  ("instructions" :text :required t
   :doc "The question itself, in words, asked about the whole input"))

(nlk:define-contract decision-submission
  (:doc "Ask one to sixty-four yes/no questions about a text, and get back, for each, the
probability that it is true. Nothing is retried: a submission that fails says so, and a
retry is a second call. With background false it answers the probabilities themselves;
with background true (the default) it answers a job id at once and decisions_status reads
the answer when it lands.")
  ("input" :text :required t
   :doc "The text the questions are about; it is sent to the endpoint, so send only what
may leave this machine")
  ("questions" :array :required t :of decision-question
   :doc "One to sixty-four questions, in the order the answers should come back")
  ("background" :boolean :default t
   :doc "true (the default) answers a job id and leaves the request running; false waits
for the probabilities and answers them"))

(nlk:define-contract decision-status
  (:doc "One submission of this session, by the id decisions_submit answered. It answers
running, or completed with the endpoint's own reply (every member it sent, not only the
answers), or failed with the reason. Finished jobs are kept 32 deep, and a restart keeps
none.")
  ("job_id" :text :required t
   :doc "The job_id decisions_submit answered"))

;;; --- reading the section ----------------------------------------------------------

(defun nonblank (text)
  "TEXT trimmed, or NIL when it is not a string or holds nothing."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun endpoint-problem (endpoint)
  "Why ENDPOINT is not an absolute URL the cell may POST to, or NIL."
  (cond ((not (or (uiop:string-prefix-p "https://" endpoint)
                  (uiop:string-prefix-p "http://" endpoint)))
         "it has no http:// or https:// scheme")
        ((find #\? endpoint) "it carries a query")
        ((find #\# endpoint) "it carries a fragment")))

(defun read-settings (values table)
  "The declared VALUES, plus the key a call authenticates with, read here once.
The key is api_key written inline, else the variable api_key_env names; with
neither set the section still runs and each call says what to set."
  (declare (ignore table))
  (let* ((endpoint (or (nonblank (getf values :endpoint)) +default-endpoint+))
         (env (nonblank (getf values :api-key-env)))
         (inline (nonblank (getf values :api-key)))
         (timeout (getf values :timeout-seconds))
         (plist (copy-list values)))
    (when (endpoint-problem endpoint)
      (nlk:config-error "endpoint ~s is not a URL a decision may go to: ~a"
                        endpoint (endpoint-problem endpoint)))
    (setf (getf plist :endpoint) (string-right-trim "/" endpoint)
          (getf plist :model) (or (nonblank (getf values :model)) +default-model+)
          (getf plist :timeout-seconds) (min +max-timeout+ (or timeout 120))
          (getf plist :key) (or inline (and env (nle::credential-env env)))
          (getf plist :key-source) (if inline "api_key" (or env "api_key")))
    plist))

;;; --- the request -----------------------------------------------------------------

(defun request-json (settings input questions)
  "The one POST body: the model, the input, and one predicate question each, in order."
  (nlk:json-object "model" (getf settings :model)
                    "input" input
                    "questions"
                    (map 'vector (lambda (question)
                                   (nlk:json-object
                                    "type" "predicate"
                                    "instructions" (decision-question-instructions question)))
                         questions)))

(defun request-octets (settings input questions)
  "The request body as bytes, once it is inside the size the endpoint takes."
  (let* ((characters (+ (length input)
                         (loop for question across questions
                               sum (length (decision-question-instructions question)))))
         (octets (sb-ext:string-to-octets (nlk:encode-json-object (request-json settings input questions))
                                          :external-format :utf-8)))
    (when (> characters +max-request-bytes+)
      (fail "decisions: the input and questions are ~:d characters, over the ~:d a request takes: shorten them"
            characters +max-request-bytes+))
    (when (> (length octets) +max-request-bytes+)
      (fail "decisions: the request is ~:d bytes, over the ~:d an endpoint takes: shorten the input or the questions"
            (length octets) +max-request-bytes+))
    octets))

;;; --- the reply -------------------------------------------------------------------

(defun finite-number-p (value)
  "T unless VALUE is a float that is infinite or not a number."
  (if (floatp value)
      (and (= value value) (<= (- most-positive-double-float) value most-positive-double-float))
      t))

(defun finite-json-p (value)
  "T when VALUE holds no infinite or not-a-number float anywhere in it."
  (cond ((hash-table-p value)
         (loop for member being the hash-values of value always (finite-json-p member)))
        ((and (vectorp value) (not (stringp value)))
         (loop for item across value always (finite-json-p item)))
        (t (finite-number-p value))))

(defun checked-answer (answer index)
  "ANSWER, one predicate answer, once its shape and its probability hold."
  (unless (hash-table-p answer)
    (fail "decisions: answer ~d of the reply is not an object" index))
  (unless (equal "predicate" (nlk:json-value answer :string "type"))
    (fail "decisions: answer ~d of the reply is not a predicate answer" index))
  (when (nlk:json-value answer :string "name")
    (fail "decisions: answer ~d of the reply names a predicate, and only predicate-only questions were asked" index))
  (let ((probability (nlk:json-value answer :number "probability")))
    (unless (and probability (finite-number-p probability) (<= 0 probability 1))
      (fail "decisions: answer ~d of the reply has no probability between 0 and 1" index)))
  answer)

(defun checked-reply (text count)
  "TEXT, the endpoint's reply, decoded once it carries one predicate answer per question."
  (let ((reply (handler-case (nlk:decode-json text)
                 (error () (fail "decisions: the endpoint answered something that is not JSON")))))
    (unless (hash-table-p reply)
      (fail "decisions: the reply is not a JSON object"))
    (unless (finite-json-p reply)
      (fail "decisions: the reply carries an infinite or not-a-number number"))
    (let ((answers (nlk:json-array reply "answers")))
      (unless (= (length answers) count)
        (fail "decisions: the reply carries ~d answers for ~d question~:p" (length answers) count))
      (loop for answer across answers
            for index from 1
            do (checked-answer answer index)))
    reply))

;;; --- the one exchange -------------------------------------------------------------

(defun http-hint (status)
  "One line saying what an HTTP STATUS most often means here."
  (case status
    (401 "the key was refused: check decisions.api_key, or the variable decisions.api_key_env names")
    (403 "the key is not allowed to ask decisions")
    (404 "the endpoint or the model name is wrong: check decisions.endpoint and decisions.model")
    (429 "rate limited: ask fewer questions, or wait")
    (t "the endpoint refused the request")))

(defun require-key (settings)
  "SETTINGS' key, or a refusal naming the two settings one goes in. Read before a
job is admitted, so a section with no key refuses the call, not a background job."
  (or (getf settings :key)
      (fail "decisions: no API key: paste one into decisions.api_key, or set the variable decisions.api_key_env names~@[ (~a)~]"
            (getf settings :key-source))))

(defun post (settings body count)
  "BODY posted once to the section's endpoint; => the decoded reply object.
No retry, no redirect of its own, and every failure says the outcome is uncertain."
  (let ((key (require-key settings))
        (timeout (getf settings :timeout-seconds)))
    (multiple-value-bind (payload status headers truncated)
        (handler-case
            (funcall *http* :post (getf settings :endpoint)
                     :headers (list (cons "authorization" (format nil "Bearer ~a" key))
                                    (cons "content-type" "application/json")
                                    (cons "accept" "application/json")
                                    (cons "accept-encoding" "identity"))
                     :content body
                     :timeout timeout
                     :max-bytes +max-response-bytes+)
          (error (condition)
            (fail "decisions: no answer from the endpoint (~a); the outcome is uncertain and it was not retried"
                  (nlk:one-line (princ-to-string condition) :cap 200))))
      (declare (ignore headers))
      (when truncated
        (fail "decisions: the reply is over ~:d bytes" +max-response-bytes+))
      (if (eql 200 status)
          (checked-reply (if (stringp payload) payload (nlk:decode-octets payload)) count)
          (fail "decisions: HTTP ~d, ~a; not retried" status (http-hint status))))))
