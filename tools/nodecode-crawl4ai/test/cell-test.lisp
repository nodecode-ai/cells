;;;; cell-test.lisp --- the Crawl4AI cell, end to end over a scripted server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here opens a socket: the cell's one network seam
;;;; (NODECODE-CRAWL4AI::*HTTP*) is bound to a fake server that answers the
;;;; routes each test scripts and records every request it took, so "the URL
;;;; never left" and "the token rode the request" are facts a test can read.
;;;; The tools are called through the real dispatch seam
;;;; (EXECUTE-WIRE-CALL), so the schema the provider is shown and the body
;;;; that runs are the same declaration.

(in-package #:nodecode.test)

(define-test-slice "crawl4ai" "CRAWL4AI-CELL-" :start nodecode-crawl4ai:start-cell)

;;; --- the fake server ---------------------------------------------------------

(defvar *requests* '()
  "Every request the fake server took, newest first: (METHOD URL CONTENT HEADERS).")

(defvar *calls* 0
  "A counter for wire call ids.")

(defvar *show-tool-answers* nil
  "Whether TOOL-TEXT echoes what each tool answered: bind it true to read a
failing test's log.")

(defun request-path (url)
  "URL without its scheme, host and query: the route it names."
  (let ((uri (ignore-errors (quri:uri url))))
    (if uri (or (quri:uri-path uri) "") url)))

(defun route-matches-p (fragment path)
  "Whether a request PATH is FRAGMENT."
  (equal fragment path))

(defun fake-server (routes)
  "A stand-in for NLK:HTTP over ROUTES, each (METHOD PATH BODY &optional
STATUS): the route whose path the request names answers. Every request lands
in *REQUESTS*."
  (setf *requests* '())
  (lambda (method url &key headers content &allow-other-keys)
    (push (list method url content headers) *requests*)
    (let* ((path (request-path url))
           (route (find-if (lambda (route)
                             (and (string-equal (string (first route)) (string method))
                                  (route-matches-p (second route) path)))
                           routes)))
      (if route
          (values (third route) (or (fourth route) 200) nil nil)
          (values "{\"detail\":\"no such route\"}" 404 nil nil)))))

(defmacro with-server ((&rest routes) &body body)
  "The cell over a server answering ROUTES. Each route is (METHOD PATH BODY
&optional STATUS), its elements evaluated where the test is written."
  `(let ((nodecode-crawl4ai::*http*
           (fake-server (list ,@(loop for route in routes collect `(list ,@route))))))
     ,@body))

(defmacro with-crawl4ai ((&rest pairs) &body body)
  "The cell started on the test server's address, plus PAIRS, and stopped after."
  `(with-cell-stop ((crawl4ai-start "url" "http://crawl4ai.test:11235" ,@pairs))
     ,@body))

(defun requests-to (fragment)
  "The requests whose URL carries FRAGMENT, oldest first."
  (reverse (remove fragment *requests* :test-not #'search :key #'second)))

(defun json-answer (&rest pairs)
  "PAIRS as the JSON text a route answers with."
  (nlk:encode-json-object (apply #'nlk:make-json-object pairs)))

(defun header (name request)
  "The request's NAME header, or NIL."
  (cdr (assoc name (fourth request) :test #'string-equal)))

(defun tool-text (name &rest arguments)
  "The text the tool called NAME answers, called as the provider's call is."
  (let ((text (princ-to-string
               (nle::message-content
                (execute-wire-call (format nil "crawl4ai-call-~d" (incf *calls*)) name
                                    (if arguments (apply #'json-answer arguments) "{}"))))))
    (when *show-tool-answers*
      (format t "~&; ~a => ~a~%" name text))
    text))

(defmacro with-fake-crawl4ai (&body body)
  "The cell over a server that answers no route: the shape of a test that
calls nothing but proves the wiring."
  `(with-server () ,@body))

;;; --- lifecycle ----------------------------------------------------------------

(define-cell-lifecycle-tests "crawl4ai"
  (:fixture with-fake-crawl4ai)
  (:config "url" "http://crawl4ai.test:11235" "api_token" "test-token")
  (:help :crawl4ai)
  (:command "crawl")
  (:running
   (is (nle::find-tool "crawl4ai_extract") "the tool is in the catalog")
   (is (equal "http://crawl4ai.test:11235" (nodecode-crawl4ai::base-url)) "the section's url"))
  (:stopped
   (is (null (nle::find-tool "crawl4ai_extract")) "stop takes the tool out of the catalog"))
  (:refused ("url" "crawl4ai.test")))

(deftest crawl4ai-cell-an-address-that-is-not-one-is-refused ()
  ;; The url is read at the start, so a typo lands on the folder's REFUSED
  ;; line and one standing notice instead of on every tool call.
  (with-server ()
    (is (search "crawl4ai.url is"
                (handler-case (progn (crawl4ai-start "url" "crawl4ai.test") "")
                  (nlk:config-refusal (condition) (nlk:config-refusal-detail condition)))))
    (is (null (nle::find-tool "crawl4ai_extract")) "and nothing was registered")))

;;; --- extracting ----------------------------------------------------------------

(deftest crawl4ai-cell-extract-posts-md-and-answers-the-markdown ()
  (with-session-store ("crawl4ai-extract")
    (with-server (("POST" "/md"
                    (json-answer "url" "https://example.com/"
                                 "markdown" "# Example Domain

This domain is for use in examples."
                                 "success" t)))
      (with-crawl4ai ("api_token" "test-token")
        (let ((text (tool-text "crawl4ai_extract" "urls" (vector "https://example.com"))))
          (is (search "## Example Domain" text) "the title is the page's first heading")
          (is (search "This domain is for use in examples." text) "and the markdown is whole")
          (is (search "https://example.com" text) "the address it read is named")
          (let ((request (first *requests*)))
            (is (equal "POST" (first request)) "one POST to the server")
            (is (equal "/md" (request-path (second request))) "to the /md route")
            (is (equal "Bearer test-token" (header "authorization" request)) "carrying the section's token as a bearer")
            (is (search "https://example.com" (third request)) "and the page in the body")))))))

(deftest crawl4ai-cell-extract-reads-the-nested-markdown-shape ()
  ;; Crawl4AI answers {"markdown": {"raw_markdown": ...}} on some builds; the
  ;; original read both shapes and so does this.
  (with-session-store ("crawl4ai-nested")
    (with-server (("POST" "/md"
                    (json-answer "url" "https://example.com/nested"
                                 "markdown" (nlk:make-json-object
                                             "raw_markdown" "# Nested

The nested text."
                                             "fit_markdown" "short")
                                 "success" t)))
      (with-crawl4ai ()
        (let ((text (tool-text "crawl4ai_extract" "urls" (vector "https://example.com/nested"))))
          (is (search "## Nested" text))
          (is (search "The nested text." text) "raw_markdown is what is read"))))))

(deftest crawl4ai-cell-extract-answers-one-line-per-failed-page ()
  (with-session-store ("crawl4ai-refused")
    (with-server (("POST" "/md" (json-answer "detail" "JWT expired") 401))
      (with-crawl4ai ("api_token" "old-token")
        (let ((text (tool-text "crawl4ai_extract" "urls" (vector "https://example.com/gone"))))
          (is (search "ERROR" text))
          (is (search "HTTP 401" text) "the status is named")
          (is (search "JWT expired" text) "and the server's own detail"))))))

(deftest crawl4ai-cell-extract-refuses-a-private-address-before-it-is-sent ()
  (with-session-store ("crawl4ai-private")
    (with-server ()
      (with-crawl4ai ()
        (let ((text (tool-text "crawl4ai_extract" "urls" (vector "http://127.0.0.1:8080/admin"))))
          (is (search "ERROR" text))
          (is (search "private or loopback" text) "the policy is named")
          (is (null *requests*) "and nothing was sent to the server"))))))

(deftest crawl4ai-cell-extract-with-no-url-says-which-member ()
  (with-session-store ("crawl4ai-nourl")
    (with-server ()
      (with-cell-stop ((crawl4ai-start))
        (let ((text (tool-text "crawl4ai_extract" "urls" (vector "https://example.com"))))
          (is (search "ERROR" text))
          (is (search "crawl4ai.url is not set" text) "the refusal names the member"))
        (is (null *requests*) "and nothing was sent")))))

;;; --- the command ---------------------------------------------------------------

(deftest crawl4ai-cell-slash-crawl-extracts-one-page ()
  (with-session-store ("crawl4ai-command")
    (with-server (("POST" "/md" (json-answer "markdown" "# From the command" "success" t)))
      (with-crawl4ai ()
        (multiple-value-bind (text dialog)
            (cell-entry "nodecode-crawl4ai" "crawl" "https://example.com/cmd")
          (is (search "From the command" text) "the page is the answer")
          (is (null dialog) "and no dialog when the address was typed"))
        (is (equal 1 (length (requests-to "/md"))) "one request")))))

(deftest crawl4ai-cell-slash-crawl-alone-asks-for-the-page ()
  (with-session-store ("crawl4ai-command-ask")
    (with-server ()
      (with-crawl4ai ()
        (multiple-value-bind (text dialog)
            (cell-entry "nodecode-crawl4ai" "crawl" "")
          (is (search "/crawl" text) "the answer says how to call it")
          (is (equal 1 (length (and dialog (nle::json-array dialog "rows")))) "and a dialog asks for the address")
          (is (null *requests*) "nothing is sent yet"))))))
