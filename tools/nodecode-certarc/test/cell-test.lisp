;;;; cell-test.lisp --- lifecycle, the four tools, /certarc, and the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, a member under its floor or a root without a scheme refused, a
;;;; program the endpoint does not serve refused), the four tools through
;;;; the seam the model calls them by -- their URL, their JSON-RPC body and
;;;; the lines they build from an answer, structured or as text -- the
;;;; server's own refusal read back as a repair, a framed and a
;;;; string-wrapped body decoded, and /certarc with and without a program.
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches CertArc.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *certarc-requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *certarc-answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defparameter +question-json+
  "{\"program\":\"cism\",\"position\":1,\"total\":10,\"questionId\":\"q-1\",\"domain\":{\"code\":\"D1\",\"name\":\"Information Security Governance\"},\"stem\":\"The compliance function has begun directing security controls. What is the MOST appropriate way to resolve this?\",\"options\":[{\"key\":\"A\",\"text\":\"Subordinate the security programme to compliance\"},{\"key\":\"B\",\"text\":\"Escalate to senior management to set each function's authority\"}],\"source\":\"CertArc public practice set (unscored)\"}"
  "One practice answer's structured content, shaped as the endpoint sends it.")

(defparameter +check-json+
  "{\"program\":\"cism\",\"questionId\":\"q-1\",\"yourAnswer\":\"A\",\"correctAnswer\":\"B\",\"correct\":false,\"confidence\":\"confident\",\"blindSpot\":true,\"options\":[{\"key\":\"A\",\"text\":\"Subordinate the security programme to compliance\",\"explanation\":\"Picks a winner without senior-management sanction.\"},{\"key\":\"B\",\"text\":\"Escalate to senior management to set each function's authority\",\"explanation\":\"Escalates to the layer that owns the authority decision.\"}],\"startUrl\":\"https://certarc.com/login?program=cism\"}"
  "One checked answer's structured content.")

(defparameter +domains-json+
  "{\"program\":\"aaia\",\"domains\":[{\"code\":\"D1\",\"name\":\"AI Governance and Risk\",\"weightPercent\":33},{\"code\":\"D2\",\"name\":\"AI Operations\",\"weightPercent\":46}],\"guidesUrl\":\"https://certarc.com/aaia/resources\",\"practiceUrl\":\"https://certarc.com/aaia/free-practice-questions\"}"
  "One exam outline's structured content.")

(defparameter +guides-json+
  "{\"program\":\"cism\",\"resources\":[{\"title\":\"CISM incident response trap\",\"url\":\"https://certarc.com/cism/incident-response-trap\"}],\"hasMore\":false}"
  "One guide search's structured content.")

(defmacro with-certarc-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *CERTARC-REQUESTS* and
answered by *CERTARC-ANSWER-FN*, so no test dials CertArc."
  `(let ((*certarc-requests* '())
         (*certarc-answer-fn* (lambda (method url args)
                                 (declare (ignore method url args))
                                 (error "unexpected CertArc request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *certarc-requests*)
                                (funcall *certarc-answer-fn* method url args))
       ,@body)))

(defun certarc-asked ()
  "The stubbed requests, oldest first."
  (reverse *certarc-requests*))

(defun certarc-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun certarc-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun certarc-answer (text &optional (status 200))
  "Answer every request with TEXT at STATUS."
  (setf *certarc-answer-fn* (lambda (method url args)
                               (declare (ignore method url args))
                               (values text status))))

(defun certarc-call (name payload)
  "The JSON-RPC envelope a tools/call of NAME is answered with, PAYLOAD both as
its text block and as its structuredContent, as the endpoint sends one."
  (declare (ignore name))
  (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"content\":[{\"type\":\"text\",\"text\":~s}],\"structuredContent\":~a}}"
          payload payload))

(defun certarc-tool (name arguments)
  "NAME called through the wire with the JSON object ARGUMENTS, as a provider
sends it -- the contract decoded, the hooks run; => the result text."
  (assert (nle::find-tool name) () "no tool named ~a" name)
  (nle::message-content (execute-wire-call "call-1" name (nlk:encode-json-object arguments))))

(defun certarc-arguments (request)
  "REQUEST's tools/call arguments, decoded."
  (nlk:json-value (nlk:json-value (certarc-request-body request) :object "params")
                   :object "arguments"))

;;; --- lifecycle -----------------------------------------------------------------

(define-test-slice "certarc" "CERTARC-CELL-")

(defun certarc-config (&rest pairs)
  "A whole shared config with one `certarc' section built from PAIRS."
  (nlk:json-object "certarc" (apply #'nlk:make-json-object pairs)))

(defun certarc-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-certarc:start-cell (apply #'certarc-config pairs)))

(define-cell-lifecycle-tests "certarc"
  (:fixture with-certarc-wire)
  (:help :certarc)
  (:command "certarc")
  (:running (is (nle::find-tool "certarc_practice_question") "certarc_practice_question is registered")
            (is (nle::find-tool "certarc_check_answer") "certarc_check_answer is registered")
            (is (nle::find-tool "certarc_exam_domains") "certarc_exam_domains is registered")
            (is (nle::find-tool "certarc_learning_guides") "certarc_learning_guides is registered")
            (is (equal "https://certarc.com/mcp/grok" (nodecode-certarc::setting :base-url)) "the vendor's free endpoint is the default")
            (is (null (nodecode-certarc::setting :program)) "no program is defaulted")
            (is (= 30 (nodecode-certarc::setting :timeout-seconds)) "one call to 30 seconds"))
  (:stopped (is (null (nle::find-tool "certarc_practice_question")) "stop unregisters the tools")
            (is (null (nle::find-tool "certarc_learning_guides")) "all four of them"))
  (:refused ("timeout_seconds" 0) ("base_url" "certarc.com") ("program" "security+"))
  (:idle nodecode-certarc:certarc-error (nodecode-certarc::run-command "" nil)))

(deftest certarc-cell-a-program-the-endpoint-does-not-serve-refuses ()
  (with-certarc-wire
    (setf *certarc-answer-fn* (lambda (method url args)
                                 (declare (ignore method args))
                                 (error "no request may reach ~a" url)))
    (is-carrying (reply (refusal-text nlk:config-refusal (certarc-start "program" "ccsp")))
                 ("program \"ccsp\" is none of cism, cissp, cisa, aaism, aaia")
                 ("leave it empty" "and what to do instead"))
    (is (null (nle::find-tool "certarc_exam_domains")) "a refused start installed nothing")))

;;; --- certarc_practice_question -------------------------------------------------

(deftest certarc-cell-a-practice-question-asks-and-reads ()
  (with-certarc-wire
    (with-cell-stop ((certarc-start))
      (certarc-answer (certarc-call "practice_question" +question-json+))
      (is-carrying
       (reply (certarc-tool "certarc_practice_question" (nlk:json-object "program" "cism")))
       ("CertArc CISM practice question 1 of 10 (D1 Information Security Governance):")
       ("The compliance function has begun directing security controls.")
       ("  A. Subordinate the security programme to compliance")
       ("  B. Escalate to senior management")
       ("Question id: q-1")
       ("never reveal or guess the answer first" "the protocol rides the answer")
       (:absent "correctAnswer" "the answer itself is not shown"))
      (let ((request (first (certarc-asked))))
        (is (= 1 (length (certarc-asked))) "one request, no handshake first")
        (is (eq :post (first request)))
        (is (equal "https://certarc.com/mcp/grok" (second request)) "the section's endpoint")
        (is (equal "application/json" (certarc-request-header request "content-type")))
        (is (equal "application/json, text/event-stream" (certarc-request-header request "accept")) "the streamable-HTTP accept the endpoint answers")
        (let ((body (certarc-request-body request))
              (arguments (certarc-arguments request)))
          (is (equal "2.0" (nlk:json-value body :string "jsonrpc")))
          (is (equal "tools/call" (nlk:json-value body :string "method")))
          (is (equal "practice_question" (nlk:json-value (nlk:json-value body :object "params")
                                                          :string "name")) "the server's own tool name")
          (is (equal "cism" (nlk:json-value arguments :string "program")))
          (is (null (nlk:json-value arguments :integer "position")) "no position unless the call names one"))))))

(deftest certarc-cell-a-position-under-one-starts-the-set ()
  (with-certarc-wire
    (with-cell-stop ((certarc-start))
      (certarc-answer (certarc-call "practice_question" +question-json+))
      (certarc-tool "certarc_practice_question" (nlk:json-object "program" "cissp" "position" 0))
      (let ((arguments (certarc-arguments (first (certarc-asked)))))
        (is (equal "cissp" (nlk:json-value arguments :string "program")))
        (is (= 1 (nlk:json-value arguments :integer "position")) "a position under one starts the set")))))

;;; --- certarc_check_answer -------------------------------------------------------

(deftest certarc-cell-a-check-names-the-blind-spot-and-explains ()
  (with-certarc-wire
    (with-cell-stop ((certarc-start))
      (certarc-answer (certarc-call "check_practice_answer" +check-json+))
      (is-carrying
       (reply (certarc-tool "certarc_check_answer"
                             (nlk:json-object "program" "cism" "question_id" "q-1"
                                              "answer" "a" "confidence" "confident")))
       ("CertArc CISM question q-1: the correct option is B; the learner chose A -- wrong.")
       ("They were confident in a wrong answer, which is a blind spot")
       ("  B. Escalate to senior management to set each function's authority (correct)")
       ("Escalates to the layer that owns the authority decision.")
       ("Keep going in CertArc: https://certarc.com/login?program=cism")
       (:absent "questionId" "the wire keys are not shown raw"))
      (let ((arguments (certarc-arguments (first (certarc-asked)))))
        (is (equal "check_practice_answer" (nlk:json-value (nlk:json-value (certarc-request-body (first (certarc-asked)))
                                                                            :object "params")
                                                           :string "name")))
        (is (equal "q-1" (nlk:json-value arguments :string "questionId")) "the server's own camelCase key")
        (is (equal "A" (nlk:json-value arguments :string "answer")) "an option key reads uppercased")
        (is (equal "confident" (nlk:json-value arguments :string "confidence")))))))

(deftest certarc-cell-a-check-without-confidence-sends-none ()
  (with-certarc-wire
    (with-cell-stop ((certarc-start))
      (certarc-answer (certarc-call "check_practice_answer" +check-json+))
      (certarc-tool "certarc_check_answer" (nlk:json-object "program" "cism" "question_id" "q-1" "answer" "B"))
      (multiple-value-bind (confidence present)
          (nlk:json-value (certarc-arguments (first (certarc-asked))) :string "confidence")
        (declare (ignore confidence))
        (is (null present) "an unasked confidence is absent, not empty")))))

;;; --- certarc_exam_domains and certarc_learning_guides ---------------------------

(deftest certarc-cell-the-outline-lists-weights-and-links ()
  (with-certarc-wire
    (with-cell-stop ((certarc-start))
      (certarc-answer (certarc-call "get_exam_domains" +domains-json+))
      (is-carrying (reply (certarc-tool "certarc_exam_domains" (nlk:json-object "program" "aaia")))
                   ("CertArc AAIA exam domains:")
                   ("  D1 AI Governance and Risk -- 33%")
                   ("  D2 AI Operations -- 46%")
                   ("Guides: https://certarc.com/aaia/resources")
                   ("Free practice set: https://certarc.com/aaia/free-practice-questions")
                   (:absent "startUrl" "a link the answer did not carry is not invented"))
      (is (equal "get_exam_domains" (nlk:json-value (nlk:json-value (certarc-request-body (first (certarc-asked)))
                                                                    :object "params")
                                                     :string "name"))))))

(deftest certarc-cell-a-guide-search-names-what-it-matched ()
  (with-certarc-wire
    (with-cell-stop ((certarc-start))
      (certarc-answer (certarc-call "search_learning_resources" +guides-json+))
      (is-carrying (reply (certarc-tool "certarc_learning_guides"
                                         (nlk:json-object "query" "incident" "program" "cism")))
                   ("1 CertArc guide for CISM matching \"incident\":")
                   ("  - CISM incident response trap -- https://certarc.com/cism/incident-response-trap")
                   (:absent "hasMore"))
      (let ((arguments (certarc-arguments (first (certarc-asked)))))
        (is (equal "incident" (nlk:json-value arguments :string "query")))
        (is (equal "cism" (nlk:json-value arguments :string "program")) "a named program narrows the search"))
      (certarc-answer (certarc-call "search_learning_resources"
                                     "{\"program\":\"cism\",\"resources\":[],\"hasMore\":false}"))
      (is-carrying (reply (certarc-tool "certarc_learning_guides" (nlk:json-object "query" "quantum")))
                   ("No CertArc guide for CISM matches \"quantum\"")
                   ("try one or two words")))))

;;; --- what a server says when it refuses ------------------------------------------

(deftest certarc-cell-a-refused-call-is-a-repair-not-a-crash ()
  (with-certarc-wire
    (with-cell-stop ((certarc-start))
      (certarc-answer "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"Unknown program: ccsp\"}],\"isError\":true}}")
      (let ((reply (certarc-tool "certarc_practice_question" (nlk:json-object "program" "cism"))))
        (is (search "ERROR: CertArc refused practice_question: Unknown program: ccsp" reply)
            reply))
      (certarc-answer "{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":{\"code\":-32602,\"message\":\"Invalid params\"}}" 400)
      (is-carrying (reply (certarc-tool "certarc_exam_domains" (nlk:json-object "program" "cism")))
                   ("ERROR: CertArc answered HTTP 400: Invalid params")
                   (:absent "unhandled" "a refusal never escapes as an error")))))

(deftest certarc-cell-a-framed-or-a-wrapped-body-decodes ()
  (with-certarc-wire
    (with-cell-stop ((certarc-start))
      (certarc-answer (concatenate 'string "data: " (certarc-call "practice_question" +question-json+)))
      (is-carrying (reply (certarc-tool "certarc_practice_question" (nlk:json-object "program" "cism")))
                   ("Question id: q-1" "a data: framed body reads like a plain one"))
      (certarc-answer (format nil "~s" (certarc-call "practice_question" +question-json+)))
      (is-carrying (reply (certarc-tool "certarc_practice_question" (nlk:json-object "program" "cism")))
                   ("Question id: q-1" "an envelope sent as a JSON string is read once more")))))

;;; --- /certarc -------------------------------------------------------------------

(deftest certarc-cell-the-command-says-where-it-points-or-asks ()
  (with-certarc-wire
    (with-cell-stop ((certarc-start))
      (is-carrying (reply (cell-entry "nodecode-certarc" "certarc" ""))
                   ("CertArc free practice: https://certarc.com/mcp/grok")
                   ("no account and no key")
                   ("/certarc cism asks the first CISM question"))
      (is (null (certarc-asked)) "a status read asks CertArc nothing")
      (certarc-answer (certarc-call "practice_question" +question-json+))
      (is-carrying (reply (cell-entry "nodecode-certarc" "certarc" "cism"))
                   ("CertArc CISM practice question 1 of 10")
                   ("Question id: q-1"))
      (is (equal "cism" (nlk:json-value (certarc-arguments (first (certarc-asked))) :string "program"))))))

(deftest certarc-cell-the-command-refuses-a-program-it-does-not-serve ()
  (with-certarc-wire
    (with-cell-stop ((certarc-start))
      (is-carrying (reply (cell-entry "nodecode-certarc" "certarc" "ccsp"))
                   ("ERROR: \"ccsp\" is no CertArc program")
                   ("one of cism, cissp, cisa, aaism, aaia"))
      (is (null (certarc-asked)) "nothing was asked"))))

;;; --- a call the model got wrong is refused before it leaves ----------------------

(deftest certarc-cell-a-call-without-its-program-refuses ()
  (with-certarc-wire
    (with-cell-stop ((certarc-start))
      (let ((reply (certarc-tool "certarc_practice_question" (nlk:json-object))))
        (is (search "program" reply) "the refusal names the member the call left out")
        (is (search "ERROR" reply) "and reads as every refusal does"))
      (is (null (certarc-asked)) "no request left for a call the contract refused"))))
