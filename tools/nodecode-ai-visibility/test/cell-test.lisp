;;;; cell-test.lisp --- the AI visibility cell, end to end over a scripted seam.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one network call is NODECODE-AI-VISIBILITY::HTTP; WITH-SCRIPTED-SERVER
;;;; stubs that name, records every exchange, and answers a fixture -- so no
;;;; test opens a socket. The lifecycle tests are the machinery's own: a start
;;;; installs the manual topic, the three tools and /ai-visibility, a stop takes
;;;; them all back, a second start leaves one of everything, and a bad setting
;;;; refuses.

(in-package #:nodecode.test)

(define-test-slice "ai-visibility" "AI-VISIBILITY-CELL-" :start nodecode-ai-visibility:start-cell)

;;; --- the scripted seam ---------------------------------------------------------------

(defmacro with-scripted-server ((calls &key answer) &body body-forms)
  "Run BODY-FORMS with NODECODE-AI-VISIBILITY::HTTP recording each exchange
into CALLS as (:method :url :content ...) and answering (values TEXT STATUS):
ANSWER is a function of the recorded exchange."
  (let ((method (gensym "METHOD")) (url (gensym "URL")) (options (gensym "OPTIONS")))
    `(let ((,calls '()))
       (with-stubbed-fdefinition (nodecode-ai-visibility::http (,method ,url &rest ,options)
                                   (setf ,calls (append ,calls (list (list* :method ,method
                                                                            :url ,url ,options))))
                                   (funcall ,answer (car (last ,calls))))
         ,@body-forms))))

(defun answer-text (text &key (is-error nil))
  "TEXT as the JSON-RPC answer a tools/call gets."
  (nlk:encode-json-object
   (nlk:make-json-object
    "jsonrpc" "2.0" "id" 1
    "result" (nlk:make-json-object
              "content" (vector (nlk:make-json-object "type" "text" "text" text))
              "isError" is-error))))

(defun answer-events (text &key (is-error nil))
  "TEXT as the event-stream answer a Streamable HTTP server may give instead."
  (format nil "event: message~%data: ~a~%~%" (answer-text text :is-error is-error)))

(defun plain-answer (text &key (is-error nil))
  "A function answering TEXT as one JSON body, whatever was asked."
  (lambda (call) (declare (ignore call)) (values (answer-text text :is-error is-error) 200)))

(defun events-answer (text &key (is-error nil))
  "A function answering TEXT as one event stream, whatever was asked."
  (lambda (call) (declare (ignore call)) (values (answer-events text :is-error is-error) 200)))

(defun asked-about (call)
  "What CALL carried, as the body the server would read."
  (getf call :content))

;;; --- the lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "ai-visibility"
  (:help :ai-visibility)
  (:command "ai-visibility")
  (:refused ("timeout_seconds" 0) ("language" "de"))
  (:idle nodecode-ai-visibility:ai-visibility-error
         (nodecode-ai-visibility:check "example.com")))

;;; --- what the model reads ------------------------------------------------------------------

(deftest ai-visibility-cell-check-reads-the-servers-own-text ()
  (with-cell-stop ((ai-visibility-start))
    (with-scripted-server
        (calls :answer (events-answer "AI visibility: example.com - 6 of 9 checks passed. Full check: https://qqq.ee/en/ai-visibility-checker/?domain=example.com"))
      (let ((answer (nodecode-ai-visibility:check "example.com")))
        (is (search "6 of 9 checks passed" answer) "the server's own text is what the model reads")
        (is (search "https://qqq.ee/en/ai-visibility-checker/" answer) "its links survive whole")
        (is (= 1 (length calls)) "one exchange, and no more")
        (is (equal :post (getf (first calls) :method)) "the call is a POST")
        (is (string= "https://qqq.ee/mcp" (getf (first calls) :url)) "to the hosted endpoint")
        (let ((asked (asked-about (first calls))))
          (is (search "\"check_ai_visibility\"" asked) "it names the readiness tool")
          (is (search "\"example.com\"" asked) "and the domain asked about")
          (is (search "\"en\"" asked) "and the language, from the section's default"))))))

(deftest ai-visibility-cell-report-reads-a-plain-json-answer ()
  (with-cell-stop ((ai-visibility-start))
    (with-scripted-server
        (calls :answer (plain-answer "Report of 2026-11-04: named in 4 of 12 AI answers."))
      (let* ((arguments (nodecode-ai-visibility::decode-report-call
                          (nlk:json-object "domain" "https://www.example.com/blog?page=2")))
             (answer (nodecode-ai-visibility::report-tool arguments)))
        (is (search "named in 4 of 12 AI answers" answer) "a JSON answer reads the same as events")
        (is (search "\"get_public_report\"" (asked-about (first calls))) "it names the report tool")
        (is (search "\"www.example.com\"" (asked-about (first calls))) "a URL is sent as its bare domain")
        (is (not (search "page=2" (asked-about (first calls)))) "and its path and query are dropped")))))

(deftest ai-visibility-cell-an-error-answer-is-a-refusal-in-the-servers-words ()
  (with-cell-stop ((ai-visibility-start))
    (with-scripted-server
        (calls :answer (events-answer "Daily limit reached: five checks per address a day. Run the full check at https://qqq.ee/en/ai-visibility-checker/" :is-error t))
      (let ((refusal (refusal-text nodecode-ai-visibility:ai-visibility-error
                                    (nodecode-ai-visibility:check "example.com"))))
        (is (search "Daily limit reached" refusal) "the server's reason is the refusal")
        (is (search "https://qqq.ee/en/ai-visibility-checker/" refusal) "with the link a person can open")))))

(deftest ai-visibility-cell-the-section-language-is-what-goes-out ()
  (with-cell-stop ((ai-visibility-start "language" "et"))
    (with-scripted-server (calls :answer (plain-answer "Valmis: 6 / 9 kontrolli läbitud."))
      (is (search "kontrolli" (nodecode-ai-visibility:check "example.com")) "the Estonian reading is returned as it came")
      (is (search "\"et\"" (asked-about (first calls))) "the section's language is sent when the call names none"))))

(deftest ai-visibility-cell-a-domain-that-is-no-domain-never-reaches-the-wire ()
  (with-cell-stop ((ai-visibility-start))
    (with-scripted-server (calls :answer (plain-answer "unused"))
      (is (search "is not a domain"
                  (refusal-text nodecode-ai-visibility:ai-visibility-error
                                (nodecode-ai-visibility:check "not-a-domain"))) "a name with no dot is refused")
      (is (search "is an IP address"
                  (refusal-text nodecode-ai-visibility:ai-visibility-error
                                (nodecode-ai-visibility:check "10.0.0.7"))) "an address is refused")
      (is (search "public websites only"
                  (refusal-text nodecode-ai-visibility:ai-visibility-error
                                (nodecode-ai-visibility:check "localhost"))) "and this machine is refused")
      (is (null calls) "nothing reached the network"))))

(deftest ai-visibility-cell-the-command-routes-to-the-tool-asked-for ()
  (with-cell-stop ((ai-visibility-start))
    (with-scripted-server
        (calls :answer (lambda (call)
                          (let ((asked (asked-about call)))
                            (values (answer-text
                                     (cond ((search "\"get_public_report\"" asked) "the stored report")
                                           ((search "\"get_tool_info\"" asked) "what is measured")
                                           (t "the readiness check")))
                                    200))))
      (let ((report (cell-entry "nodecode-ai-visibility" "ai-visibility" "report example.com"
                                "ai-visibility-cell-report")))
        (is (search "the stored report" report) "report DOMAIN reads the stored report"))
      (let ((info (cell-entry "nodecode-ai-visibility" "ai-visibility" "info"
                              "ai-visibility-cell-info")))
        (is (search "what is measured" info) "info asks what is measured"))
      (let ((usage (cell-entry "nodecode-ai-visibility" "ai-visibility" ""
                               "ai-visibility-cell-usage")))
        (is (search "usage: /ai-visibility" usage) "alone, it says how it is used"))
      (let ((checked (cell-entry "nodecode-ai-visibility" "ai-visibility" "example.com"
                                 "ai-visibility-cell-check")))
        (is (search "the readiness check" checked) "a bare domain is the readiness check")))))
