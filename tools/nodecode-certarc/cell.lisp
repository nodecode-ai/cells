;;;; cell.lisp --- CertArc's free exam practice, as four tools and /certarc.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of the Hermes plugin in CertArc/certarc-mcp (see
;;;; NOTICE). The original is configuration only: one hosted MCP server at
;;;; https://certarc.com/mcp/grok and one practice skill. Nodecode reaches
;;;; the same server here, one JSON-RPC tools/call per tool, and lays each
;;;; answer out as the lines a model reads. The endpoint is stateless: no
;;;; initialize, no session id, and one plain POST answers JSON -- on a
;;;; stream, one data: line -- so all four tools are one request each, and
;;;; all four are read-only. The eight tools that read a learner's own study
;;;; need a browser sign-in connector, so they are left out.

(defpackage #:nodecode-certarc
  (:use #:cl)
  (:export #:certarc-error #:certarc-error-detail))

(in-package #:nodecode-certarc)

(nlk:define-peripheral certarc :not-running t)

;;; --- the words -------------------------------------------------------------------

(defparameter +programs+ '("cism" "cissp" "cisa" "aaism" "aaia")
  "The five certifications the free server serves, as its own enum spells them.")

(defparameter +confidence+ '("confident" "unsure" "guess")
  "How sure a learner was of an answer: the server's enum.")

(defparameter +detail-chars+ 300
  "How much of a failed answer's text a refusal repeats.")

(defparameter *call* 0
  "The JSON-RPC id the next call carries; the server answers it and reads nothing of it.")

(defparameter +primer+
  "CertArc is independent practice for the CISM, CISSP, CISA, AAISM and AAIA exams: its
hosted server answers free scenarios, exam outlines and guide links with no account. Four
read-only tools:
  certarc_practice_question {program, position?}   one scenario with its options and a
      question id, never the answer; a set is 10 questions and position walks it
  certarc_check_answer {program, question_id, answer, confidence?}   whether the choice
      was right, the correct option and CertArc's explanation for every option; a
      confident wrong answer comes back as a blind spot
  certarc_exam_domains {program}   the exam’s domains and their weights, with links
  certarc_learning_guides {query, program?}   guide titles and links, matched on the
      words in a title
Ask which option they choose and how sure they are -- confident, unsure or guess -- before
checking, and never reveal or guess the answer first. These are CertArc’s own
scenarios, not ISACA or ISC2 questions, and nothing is recorded: never treat an answer as
a score or a readiness figure. An answer may carry a startUrl, CertArc’s free
assessment: offer it once, when the set is done or they want to keep going, and let them
sign in themselves."
  "What (help :certarc) answers while the cell runs.")

;;; --- the section, read back -------------------------------------------------------

(defun nonblank (text)
  "TEXT trimmed, when it is a non-blank string; else NIL."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun read-settings (values table)
  "The declared VALUES, checked once, here: the endpoint has a scheme and the
program a default names one of the five."
  (declare (ignore table))
  (let* ((root (string-right-trim "/" (or (getf values :base-url) "")))
         (program (string-downcase (or (getf values :program) "")))
         (plist (copy-list values)))
    (unless (or (uiop:string-prefix-p "https://" root) (uiop:string-prefix-p "http://" root))
      (nlk:config-error "base_url ~s has no http:// or https:// scheme: set it to https://certarc.com/mcp/grok, or to the proxy that answers for it" root))
    (when (and (plusp (length program))
               (not (member program +programs+ :test #'string=)))
      (nlk:config-error "program ~s is none of ~{~a~^, ~}: leave it empty to ask which exam each time, or name the one you are studying" program +programs+))
    (setf (getf plist :base-url) root
          (getf plist :program) (and (plusp (length program)) program))
    plist))

;;; --- one call, and the shapes it comes back in -------------------------------------

(defun unwrap-sse (text)
  "TEXT with streamable-HTTP framing removed: the first `data:' line's
payload, or TEXT itself when it carries none."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (if (and text (uiop:string-prefix-p "data:" text))
        (or (loop for line in (uiop:split-string text :separator '(#\Newline))
                  for body = (nlk:trimmed line)
                  when (uiop:string-prefix-p "data:" body)
                    return (nlk:trimmed (subseq body 5)))
            text)
        (or text ""))))

(defun decode-json-or-nil (text)
  "TEXT decoded, or NIL when it is not one JSON document."
  (and (stringp text)
       (plusp (length (nlk:trimmed text)))
       (ignore-errors (nlk:decode-json (nlk:trimmed text)))))

(defun decode-envelope (answer)
  "ANSWER, an HTTP body, as the JSON-RPC envelope it carries. The endpoint
sometimes answers a JSON string holding the envelope, so one read is repeated
once; an unreadable body is an empty object."
  (let ((value (decode-json-or-nil (unwrap-sse answer))))
    (if (stringp value)
        (or (decode-json-or-nil value) (nlk:json-object))
        (or value (nlk:json-object)))))

(defun envelope-error (envelope)
  "ENVELOPE's error as one line, whether it is a string or an object."
  (let ((error (nlk:json-value envelope :any "error")))
    (cond ((stringp error) (nonblank error))
          ((hash-table-p error)
           (or (nonblank (nlk:json-value error :string "message"))
               (nonblank (nlk:json-value error :string "code"))))
          (t nil))))

(defun result-text (result)
  "RESULT's first content[].text: what the server said, in its own words."
  (loop for item across (nlk:json-array result "content")
        when (and (hash-table-p item)
                  (equal "text" (nlk:json-value item :string "type")))
          return (nlk:json-value item :string "text")))

(defun payload-of (result)
  "RESULT's structuredContent, or its text read as a JSON object, or NIL."
  (or (nlk:json-value result :object "structuredContent")
      (let ((value (decode-json-or-nil (result-text result))))
        (and (hash-table-p value) value))))

(defun call-tool (name arguments)
  "NAME called on the configured endpoint with the JSON object ARGUMENTS =>
  (values PAYLOAD TEXT), PAYLOAD the answer's object when it brought one and
  TEXT its own text; a refusal signals CERTARC-ERROR with what it said."
  (let* ((request (nlk:json-object "jsonrpc" "2.0" "id" (incf *call*)
                                    "method" "tools/call"
                                    "params" (nlk:json-object "name" name
                                                               "arguments" arguments)))
         (answer nil)
         (status nil))
    (handler-case
        (multiple-value-setq (answer status)
          (nlk:http :post (setting :base-url)
                    :headers (list (cons "content-type" "application/json")
                                   (cons "accept" "application/json, text/event-stream"))
                    :content (nlk:encode-json-object request)
                    :timeout (setting :timeout-seconds)
                    :connect-timeout (min 10 (setting :timeout-seconds))))
      (error (condition)
        (fail "~a did not answer: ~a" (setting :base-url)
              (nlk:one-line (princ-to-string condition)))))
    (let ((envelope (decode-envelope answer)))
      (when (or (not (integerp status)) (>= status 300))
        (fail "CertArc answered HTTP ~a~@[: ~a~]" status (envelope-error envelope)))
      (let ((error (nlk:json-value envelope :object "error"))
            (result (nlk:json-value envelope :object "result")))
        (when error
          (fail "CertArc refused ~a: ~a" name
                (or (envelope-error envelope) "no reason given")))
        (unless result
          (fail "CertArc answered no result for ~a~@[: ~a~]" name
                (let ((text (nonblank (and (stringp answer) answer))))
                  (and text (nlk:clip (nlk:one-line text) +detail-chars+)))))
        (when (nlk:json-value result :boolean "isError")
          (fail "CertArc refused ~a: ~a" name
                (or (nonblank (result-text result)) "no reason given")))
        (values (payload-of result) (result-text result))))))

;;; --- lines -----------------------------------------------------------------------

(defun program-title (program)
  "PROGRAM uppercased, as the text names an exam."
  (string-upcase (or program "")))

(defun option-lines (options &key explanations correct)
  "OPTIONS as one indented line each; with EXPLANATIONS each carries the
why, and the option keyed CORRECT is marked."
  (with-output-to-string (out)
    (loop for option across (or options #())
          for key = (or (nonblank (nlk:json-value option :string "key")) "?")
          for text = (or (nlk:json-value option :string "text") "")
          do (format out "  ~a. ~a~:[~; (correct)~]" key text
                     (and correct (string-equal key correct)))
             (when explanations
               (let ((note (nonblank (nlk:json-value option :string "explanation"))))
                 (when note (format out "~%     ~a" note)))))))

(defun practice-lines (body fallback)
  "BODY's one practice question as the lines a model reads."
  (if (null body)
      (or fallback "")
      (let ((domain (nlk:json-value body :object "domain")))
        (with-output-to-string (out)
          (format out "CertArc ~a practice question~@[ ~d of ~d~]"
                  (program-title (nlk:json-value body :string "program"))
                  (nlk:json-value body :integer "position")
                  (nlk:json-value body :integer "total"))
          (let ((code (nonblank (nlk:json-value domain :string "code")))
                (name (nonblank (nlk:json-value domain :string "name"))))
            (when (or code name)
              (format out " (~@[~a ~]~a)" code (or name ""))))
          (format out ":~%~a~%~a~%~%Question id: ~a"
                  (or (nlk:json-value body :string "stem") "(no scenario)")
                  (option-lines (nlk:json-array body "options"))
                  (or (nlk:json-value body :string "questionId") "none"))
          (format out "~%Ask which option they choose and how sure they are (confident, unsure or guess), then call certarc_check_answer; never reveal or guess the answer first.")))))

(defun check-lines (body fallback)
  "BODY's checked answer as the lines a model reads."
  (if (null body)
      (or fallback "")
      (with-output-to-string (out)
        (format out "CertArc ~a question ~a: the correct option is ~a; the learner chose ~a -- ~a."
                (program-title (nlk:json-value body :string "program"))
                (or (nlk:json-value body :string "questionId") "?")
                (or (nlk:json-value body :string "correctAnswer") "?")
                (or (nlk:json-value body :string "yourAnswer") "?")
                (if (nlk:json-value body :boolean "correct") "right" "wrong"))
        (when (nlk:json-value body :boolean "blindSpot")
          (format out "~%They were ~a in a wrong answer, which is a blind spot: say so."
                  (or (nlk:json-value body :string "confidence") "confident")))
        (format out "~a" (option-lines (nlk:json-array body "options")
                                        :explanations t
                                        :correct (nlk:json-value body :string "correctAnswer")))
        (let ((url (nonblank (nlk:json-value body :string "startUrl"))))
          (when url
            (format out "~%Keep going in CertArc: ~a -- offer it once, when the set is done or they want to keep going; they sign in themselves."
                    url))))))

(defun domains-lines (body fallback)
  "BODY's exam outline as the lines a model reads."
  (if (null body)
      (or fallback "")
      (with-output-to-string (out)
        (format out "CertArc ~a exam domains:" (program-title (nlk:json-value body :string "program")))
        (loop for domain across (nlk:json-array body "domains")
              do (format out "~%  ~@[~a ~]~a~@[ -- ~d%~]"
                         (nonblank (nlk:json-value domain :string "code"))
                         (or (nonblank (nlk:json-value domain :string "name")) "?")
                         (nlk:json-value domain :integer "weightPercent")))
        (loop for (key label) in '(("guidesUrl" "Guides")
                                    ("practiceUrl" "Free practice set")
                                    ("startUrl" "Free assessment"))
              for url = (nonblank (nlk:json-value body :string key))
              when url do (format out "~%~a: ~a" label url)))))

(defun guides-lines (body fallback query)
  "BODY's guides as the lines a model reads; QUERY is what was asked for."
  (if (null body)
      (or fallback "")
      (let ((resources (nlk:json-array body "resources"))
            (program (nlk:json-value body :string "program")))
        (if (zerop (length resources))
            (format nil "No CertArc guide~@[ for ~a~] matches ~s: try one or two words, or none for all of them."
                    (and program (program-title program)) (or query ""))
            (with-output-to-string (out)
              (format out "~d CertArc guide~:p~@[ for ~a~]~@[ matching ~s~]:"
                      (length resources) (and program (program-title program))
                      (nonblank query))
              (loop for resource across resources
                    do (format out "~%  - ~a -- ~a"
                               (or (nonblank (nlk:json-value resource :string "title")) "(untitled)")
                               (or (nlk:json-value resource :string "url") "")))
              (when (nlk:json-value body :boolean "hasMore")
                (format out "~%(More guides match: narrow the query, which is matched on words in a title.)")))))))

;;; --- the verbs the tools and /certarc call ------------------------------------------

(defun program-arg (program)
  "PROGRAM as the endpoint spells it, or a refusal naming the five."
  (let ((program (string-downcase (nlk:trimmed (or program "")))))
    (unless (member program +programs+ :test #'string=)
      (fail "~s is no CertArc program: one of ~{~a~^, ~}" program +programs+))
    program))

(define-verb practice-question (program &key position)
  "One CertArc practice question for PROGRAM, position POSITION or the first.
=> the scenario, its options and its question id, as the lines a model reads."
  (multiple-value-bind (body text)
      (call-tool "practice_question"
                 (nlk:json-object "program" (program-arg program)
                                  :opt "position" (and position (max 1 position))))
    (practice-lines body text)))

(define-verb check-answer (program question-id answer &key confidence)
  "Check ANSWER -- an option key -- for PROGRAM's QUESTION-ID. CONFIDENCE is
confident, unsure or guess. => whether it was right, the correct option and
the explanation for each, as the lines a model reads."
  (let ((confidence (nonblank (and confidence (string-downcase (nlk:trimmed confidence))))))
    (multiple-value-bind (body text)
        (call-tool "check_practice_answer"
                   (nlk:json-object "program" (program-arg program)
                                    "questionId" question-id
                                    "answer" (string-upcase (nlk:trimmed answer))
                                    :opt "confidence" confidence))
      (check-lines body text))))

(define-verb exam-domains (program)
  "PROGRAM's domains and their weights, with links, as the lines a model reads."
  (multiple-value-bind (body text)
      (call-tool "get_exam_domains" (nlk:json-object "program" (program-arg program)))
    (domains-lines body text)))

(define-verb learning-guides (query &key program)
  "CertArc study guides whose titles match QUERY, for PROGRAM when named, as
the lines a model reads."
  (multiple-value-bind (body text)
      (call-tool "search_learning_resources"
                 (nlk:json-object "query" (or query "")
                                  :opt "program" (and (nonblank program) (program-arg program))))
    (guides-lines body text query)))

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (certarc-error (condition)
       (nle:failure (format nil "ERROR: ~a" (certarc-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: CertArc: ~a" (nlk:one-line (princ-to-string condition)))))))

;;; --- the boundaries the model crosses ----------------------------------------------

(nlk:define-contract practice-question-args
  (:doc "One CertArc scenario practice question for CISM, CISSP, CISA, AAISM or AAIA, with its options and question id, never the answer. Ask which option and how sure (confident, unsure or guess) before checking it.")
  ("program" :string :required t :options +programs+
             :doc "the certification")
  ("position" :integer :slot ordinal
              :doc "question number in the set of 10; omit for the first"))

(nlk:define-contract check-answer-args
  (:doc "Check the learner's chosen option on a CertArc practice question: whether it was right, the correct option and CertArc's explanation for every option. Records nothing.")
  ("program" :string :required t :options +programs+
             :doc "the certification")
  ("question_id" :string :required t
                 :doc "the question id certarc_practice_question returned")
  ("answer" :string :required t
            :doc "the option key they chose, such as B")
  ("confidence" :string :options +confidence+
                :doc "how sure they were: confident, unsure or guess"))

(nlk:define-contract exam-domains-args
  (:doc "What a CISM, CISSP, CISA, AAISM or AAIA exam covers: its domains and their weights, with links to CertArc's guides and free practice set.")
  ("program" :string :required t :options +programs+
             :doc "the certification"))

(nlk:define-contract learning-guides-args
  (:doc "CertArc study guides on a topic: published guide titles and links, matched on the words in a title. Not a general web search, and never exam questions.")
  ("query" :text :required t
           :doc "one or two words to match in guide titles, such as incident response")
  ("program" :string :options +programs+
             :doc "the certification to narrow to; omit for every guide"))

(defun practice-question-tool (call)
  "The certarc_practice_question tool."
  (answering
    (practice-question (practice-question-args-program call)
                       :position (practice-question-args-ordinal call))))

(defun check-answer-tool (call)
  "The certarc_check_answer tool."
  (answering
    (check-answer (check-answer-args-program call)
                  (check-answer-args-question-id call)
                  (check-answer-args-answer call)
                  :confidence (check-answer-args-confidence call))))

(defun exam-domains-tool (call)
  "The certarc_exam_domains tool."
  (answering
    (exam-domains (exam-domains-args-program call))))

(defun learning-guides-tool (call)
  "The certarc_learning_guides tool."
  (answering
    (learning-guides (learning-guides-args-query call)
                     :program (learning-guides-args-program call))))

;;; --- /certarc ---------------------------------------------------------------------

(defun status-line ()
  "What /certarc answers with no argument: where it points, and what it serves."
  (let ((program (setting :program)))
    (format nil "CertArc free practice: ~a -- four read-only tools (certarc_practice_question, certarc_check_answer, certarc_exam_domains, certarc_learning_guides) for ~{~a~^, ~}; no account and no key~@[, default program ~a~]. /certarc cism asks the first CISM question."
            (setting :base-url) +programs+ program)))

(defun run-command (args session-id)
  "/certarc [PROGRAM]: the first practice question for PROGRAM, or, with no
argument, what the section points at."
  (declare (ignore session-id))
  (let* ((typed (nlk:trimmed (or args "")))
         (program (if (plusp (length typed)) typed (setting :program))))
    (handler-case
        (if program
            (practice-question program)
            (status-line))
      (certarc-error (condition) (format nil "ERROR: ~a" (certarc-error-detail condition))))))

;;; --- the declaration --------------------------------------------------------------

(nle:define-cell certarc
  (:section ("certarc")
    (:guide "no account and no key: CertArc's free server answers practice questions, exam outlines and guide links; program is the exam /certarc asks about when a user names none; base_url moves every call behind your own proxy")
    ("base_url" :string :default "https://certarc.com/mcp/grok"
     :doc "the hosted MCP endpoint every tool calls")
    ("program" :string :default ""
     :doc "the exam /certarc asks about when none is named: cism, cissp, cisa, aaism or aaia; empty to ask each time")
    ("timeout_seconds" :integer :default 30 :min 1
     :doc "one call's ceiling, in seconds"))
  (:settings #'read-settings)
  (:help :certarc
         "certarc: free CISM, CISSP, CISA, AAISM and AAIA practice questions, exam outlines and guide links (certarc_practice_question, certarc_check_answer, certarc_exam_domains, certarc_learning_guides)"
         +primer+)
  (:tool "certarc_practice_question" (nlk:contract 'practice-question-args) #'practice-question-tool)
  (:tool "certarc_check_answer" (nlk:contract 'check-answer-args) #'check-answer-tool)
  (:tool "certarc_exam_domains" (nlk:contract 'exam-domains-args) #'exam-domains-tool)
  (:tool "certarc_learning_guides" (nlk:contract 'learning-guides-args) #'learning-guides-tool)
  (:command "certarc" 'run-command
            :description "Free CertArc practice questions for a certification"
            :argument-hint "PROGRAM"))
