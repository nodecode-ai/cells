;;;; cell-test.lisp --- lifecycle, the eight tools and /postel, against a stubbed wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches Postel, needs a
;;;; key or touches the network. What is proved is the cell's own wiring: the
;;;; section (a healthy start, wrong values refused), the tools through the
;;;; seam the model calls them by -- their method, their url, their body, their
;;;; headers and their answer -- and the operator's command.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *reply* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-postel-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *REPLY*, so no test dials Postel."
  `(let ((*requests* '())
         (*reply* (lambda (method url args)
                    (declare (ignore method url args))
                    (error "unexpected postel request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *reply* method url args))
       ,@body)))

(defun postel-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun postel-reply (text &optional (status 200))
  (setf *reply* (lambda (method url args)
                  (declare (ignore method url args))
                  (values text status))))

(defun postel-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun postel-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun postel-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "postel-call" name arguments)))

;;; --- lifecycle -----------------------------------------------------------------

(define-test-slice "postel" "POSTEL-CELL-" :start nodecode-postel:start-cell)

(define-cell-lifecycle-tests "postel"
  (:fixture with-postel-wire)
  (:config "api_key" "pstl_test")
  (:help :postel)
  (:command "postel")
  (:running
   (is (nle::find-tool "postel_viral") "research is registered")
   (is (nle::find-tool "postel_publish") "and the one tool that reaches a network")
   (is (= 8 (count-if (lambda (entry) (eql 0 (search "postel_" (first entry)))) nle::*tools*)) "eight of them, and no others"))
  (:stopped
   (is (null (nle::find-tool "postel_viral")) "and stop takes every one back")
   (is (null (nle::find-tool "postel_save"))))
  (:refused ("timeout_seconds" 0)
            ("max_results" 0)
            ("max_chars" 10)))

;;; --- research ----------------------------------------------------------------------

(deftest postel-cell-with-no-key-every-tool-refuses-and-sends-nothing (with-postel-wire)
  (with-stubbed-fdefinition (uiop:getenv (name) nil)
    (with-cell-stop ((postel-start))
      (let ((answer (postel-call "postel_viral" "{\"query\": \"cold email\"}")))
        (is (search "ERROR: " answer) "a refusal is the tool result, not a condition up the stack")
        (is (search "POSTEL_API_KEY" answer) "naming the key it needs")
        (is (search "Settings" answer) "and where a key is made")
        (is (null (postel-asked)) "and nothing was sent")))))

(deftest postel-cell-research-asks-the-library-and-reads-its-posts (with-postel-wire)
  (with-cell-stop ((postel-start "api_key" "pstl_test"))
    (postel-reply
     (concatenate 'string
                  "{\"posts\": [{\"author\": \"someone\", \"network\": \"x\", \"views\": 120000, "
                  "\"outlierScore\": 3.4, \"url\": \"https://x.test/1\", "
                  "\"text\": \"the cold email that booked 40 calls\"}]}"))
    (let ((answer (postel-call "postel_viral" "{\"query\": \"cold email\", \"min_views\": 10000}")))
      (let* ((request (first (postel-asked)))
             (url (second request)))
        (is (eq :get (first request)) "one GET")
        (is (eql 0 (search "https://www.postel.app/api/v1/viral/posts?" url)) "to the library's own path")
        (is (search "query=cold" url) "with the query")
        (is (search "minViews=10000" url) "and the floor the call named")
        (is (search "limit=10" url) "and the section's count")
        (is (equal "Bearer pstl_test" (postel-request-header request "authorization")) "with the section's key"))
      (is (search "1 post from Postel's viral library" answer) "the count, said as a snapshot")
      (is (search "1. someone (x)" answer) "the numbered result")
      (is (search "120,000 views" answer) "with its numbers")
      (is (search "https://x.test/1" answer) "and its link"))))

(deftest postel-cell-a-network-outside-the-library-refuses-before-a-request (with-postel-wire)
  (with-cell-stop ((postel-start "api_key" "pstl_test"))
    (let ((answer (postel-call "postel_viral" "{\"query\": \"x\", \"network\": \"linkedin\"}")))
      (is (search "ERROR: " answer) "the library covers four networks and LinkedIn is not one")
      (is (search "network" answer) "the refusal names the member")
      (is (null (postel-asked)) "and nothing was sent"))))

(deftest postel-cell-an-empty-library-is-said-not-left-blank (with-postel-wire)
  (with-cell-stop ((postel-start "api_key" "pstl_test"))
    (postel-reply "{\"posts\": []}")
    (let ((answer (postel-call "postel_viral" "{\"query\": \"underwater basket weaving\"}")))
      (is (search "no post in Postel's library" answer) "nothing found is said")
      (is (search "broader words" answer) "with what to do next"))))

;;; --- the workspace -----------------------------------------------------------------

(deftest postel-cell-drafts-are-listed-with-their-state (with-postel-wire)
  (with-cell-stop ((postel-start "api_key" "pstl_test"))
    (postel-reply
     (concatenate 'string
                  "{\"posts\": [{\"id\": \"p1\", \"platform\": \"linkedin\", \"status\": \"draft\", "
                  "\"text\": \"a draft the user approved\"}]}"))
    (let ((answer (postel-call "postel_posts" "{\"status\": \"draft\"}")))
      (is (search "status=draft" (second (first (postel-asked)))) "the state rides the query")
      (is (search "p1" answer) "the id the user needs next")
      (is (search "linkedin" answer) "the network")
      (is (search "a draft the user approved" answer) "and the text"))))

(deftest postel-cell-a-saved-post-carries-what-was-confirmed (with-postel-wire)
  (with-cell-stop ((postel-start "api_key" "pstl_test"))
    (postel-reply "{\"id\": \"p9\", \"platform\": \"linkedin\", \"status\": \"draft\"}" 201)
    (let ((answer (postel-call "postel_save"
                                (concatenate 'string
                                             "{\"platform\": \"linkedin\", \"text\": \"my take\", "
                                             "\"first_comment\": \"the link: https://a.test\"}"))))
      (let* ((request (first (postel-asked)))
             (body (postel-request-body request)))
        (is (eq :post (first request)) "one POST")
        (is (equal "https://www.postel.app/api/v1/posts" (second request)) "to the posts path")
        (is (equal "linkedin" (nlk:json-value body :text "platform")) "the network")
        (is (equal "my take" (nlk:json-value body :text "text")) "the approved text")
        (is (equal "the link: https://a.test" (nlk:json-value body :text "firstComment")) "and the first comment")
        (is (null (nlk:json-value body :any "scheduledAt")) "with no time, so it stays a draft")
        (is (equal "application/json" (postel-request-header request "content-type")) "a JSON body"))
      (is (search "saved post p9 on linkedin" answer) "the answer names what Postel saved")
      (is (search "draft" answer) "in the state Postel sent"))))

(deftest postel-cell-a-post-with-neither-text-nor-thread-refuses (with-postel-wire)
  (with-cell-stop ((postel-start "api_key" "pstl_test"))
    (let ((answer (postel-call "postel_save" "{\"platform\": \"x\"}")))
      (is (search "ERROR: " answer) "the refusal is the tool result")
      (is (search "text, or thread" answer) "naming the two it takes")
      (is (null (postel-asked)) "and nothing was sent"))))

(deftest postel-cell-scheduling-and-publishing-are-their-own-paths (with-postel-wire)
  (with-cell-stop ((postel-start "api_key" "pstl_test"))
    (postel-reply "{\"id\": \"p9\", \"status\": \"scheduled\", \"scheduledAt\": \"2026-03-01T09:00:00Z\"}")
    (let ((answer (postel-call "postel_schedule"
                                "{\"id\": \"p9\", \"scheduled_at\": \"2026-03-01T09:00:00Z\"}")))
      (let* ((request (first (postel-asked)))
             (body (postel-request-body request)))
        (is (eql 0 (search "https://www.postel.app/api/v1/posts/p9/schedule" (second request))) "the draft's own schedule path")
        (is (equal "2026-03-01T09:00:00Z" (nlk:json-value body :text "scheduledAt")) "the confirmed time"))
      (is (search "scheduled post p9" answer) "and the answer says so")
      (is (search "2026-03-01T09:00:00Z" answer) "with the time Postel stored"))
    (postel-reply "{\"id\": \"p9\", \"status\": \"publishing\"}")
    (let ((answer (postel-call "postel_publish" "{\"id\": \"p9\"}")))
      (is (eql 0 (search "https://www.postel.app/api/v1/posts/p9/publish" (second (car *requests*)))) "and publishing is the publish path")
      (is (search "p9" answer) "answered with the post"))))

(deftest postel-cell-generating-spends-a-credit-and-says-so (with-postel-wire)
  (with-cell-stop ((postel-start "api_key" "pstl_test"))
    (postel-reply "{\"id\": \"p3\", \"platform\": \"x\", \"status\": \"draft\"}" 201)
    (let ((answer (postel-call "postel_generate" "{\"prompt\": \"what I learned shipping\"}")))
      (let* ((request (first (postel-asked)))
             (body (postel-request-body request)))
        (is (eql 0 (search "https://www.postel.app/api/v1/posts/generate" (second request))) "the generate path")
        (is (equal "what I learned shipping" (nlk:json-value body :text "prompt")) "the prompt")
        (is (equal "x" (nlk:json-value body :text "platform")) "and the default network"))
      (is (search "1 Postel AI credit" answer) "the cost is stated"))))

(deftest postel-cell-the-account-reads-the-plan-and-the-networks (with-postel-wire)
  (with-cell-stop ((postel-start "api_key" "pstl_test"))
    (setf *reply*
          (lambda (method url args)
            (declare (ignore method args))
            (if (search "/channels" url)
                (values "{\"channels\": [{\"platform\": \"x\", \"handle\": \"@someone\", \"connected\": true}]}" 200)
                (values "{\"workspace\": \"mine\", \"plan\": \"pro\", \"canPublish\": true, \"credits\": 12}" 200))))
    (let ((answer (postel-call "postel_account" "{}")))
      (is (= 2 (length (postel-asked))) "one request for the account, one for its networks")
      (is (search "workspace: mine" answer) "the workspace")
      (is (search "plan: pro" answer) "the plan")
      (is (search "can publish: yes" answer) "whether it can publish")
      (is (search "AI credits: 12" answer) "what is left to spend")
      (is (search "x @someone" answer) "and the network that is connected"))))

(deftest postel-cell-a-refused-key-is-the-model-s-repair (with-postel-wire)
  (with-cell-stop ((postel-start "api_key" "pstl_test"))
    (postel-reply "{\"error\": \"That API key is not valid.\", \"code\": \"invalid_key\"}" 401)
    (let ((answer (postel-call "postel_posts" "{}")))
      (is (search "ERROR: " answer) "the failure is the tool result")
      (is (search "HTTP 401" answer) "naming the status")
      (is (search "That API key is not valid." answer) "with Postel's own message")
      (is (search "postel section" answer) "and telling the model what to check"))))

;;; --- the operator's command ----------------------------------------------------------

(deftest postel-cell-the-command-reports-the-account-and-researches (with-postel-wire)
  (with-cell-stop ((postel-start "api_key" "pstl_test"))
    (setf *reply*
          (lambda (method url args)
            (declare (ignore method args))
            (cond ((search "/channels" url) (values "{\"channels\": []}" 200))
                  ((search "/viral/posts" url) (values "{\"posts\": []}" 200))
                  (t (values "{\"workspace\": \"mine\", \"plan\": \"trial\", \"canPublish\": true}" 200)))))
    (let ((status (nodecode-postel::run-command "" nil)))
      (is (search "workspace: mine" status) "no argument reports the account")
      (is (search "no network connected yet" status) "and says there is no network yet")
      (is (null (search "pstl_test" status)) "without ever printing the key"))
    (let ((answer (nodecode-postel::run-command "  cold email  " nil)))
      (is (search "no post in Postel's library" answer) "words are a research")
      (is (search "query=cold" (second (car *requests*))) "with the words on the url"))))

(deftest postel-cell-the-command-says-a-refusal-instead-of-signalling (with-postel-wire)
  (with-stubbed-fdefinition (uiop:getenv (name) nil)
    (with-cell-stop ((postel-start))
      (let ((answer (nodecode-postel::run-command "cold email" nil)))
        (is (search "POSTEL_API_KEY" answer) "the refusal comes back as text")
        (is (null (postel-asked)) "and nothing was sent")))))
