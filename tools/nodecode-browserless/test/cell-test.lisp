;;;; cell-test.lisp --- the browserless cell, end to end over a scripted Browserless.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here opens a socket: the cell's one network seam
;;;; (NODECODE-BROWSERLESS::*HTTP*) is bound to a fake endpoint that answers
;;;; the routes each test scripts and records every request it took, so "the
;;;; token rode the query" and "the page never left" are facts a test reads.
;;;; The tools are called through the real dispatch seam
;;;; (EXECUTE-WIRE-CALL), so the schema the provider is shown and the body
;;;; that runs are the same declaration.

(in-package #:nodecode.test)

(define-test-slice "browserless" "BROWSERLESS-CELL-" :start nodecode-browserless:start-cell)

;;; --- the fake endpoint -----------------------------------------------------

(defvar *requests* '()
  "Every request the fake endpoint took, newest first: (METHOD URL CONTENT).")

(defvar *calls* 0
  "A counter for wire call ids.")

(defun request-path (url)
  "URL without its scheme, host and query: the route it names."
  (let ((uri (ignore-errors (quri:uri url))))
    (if uri (or (quri:uri-path uri) "") url)))

(defun fake-endpoint (routes)
  "A stand-in for NLK:HTTP over ROUTES, each (METHOD PATH BODY &optional STATUS):
the first route whose method and path match answers. Every request lands in
*REQUESTS*."
  (setf *requests* '())
  (lambda (method url &key content &allow-other-keys)
    (push (list method url content) *requests*)
    (let ((route (find-if (lambda (route)
                            (and (string-equal (string (first route)) (string method))
                                 (equal (second route) (request-path url))))
                          routes)))
      (if route
          (values (third route) (or (fourth route) 200) nil nil)
          (values "{\"error\":\"no such route\"}" 404 nil nil)))))

(defmacro with-fake-browserless ((&rest routes) &body body)
  "The cell over a Browserless answering ROUTES. Each route is
(METHOD PATH BODY &optional STATUS), evaluated where the test is written."
  `(let ((nodecode-browserless::*http* (fake-endpoint (list ,@(loop for route in routes
                                                                    collect `(list ,@route))))))
     ,@body))

(defmacro with-token (&body body)
  "BODY with the token environment answering one fixed token."
  `(with-stubbed-fdefinition (nle::credential-env (name)
                              (and (equal name "BROWSERLESS_TOKEN") "test-token"))
     ,@body))

(defmacro with-browserless ((&rest pairs) &body body)
  "The cell started on PAIRS (token_env defaulted) and stopped after, over the
fake endpoint the caller bound."
  `(with-token
     (with-cell-stop ((browserless-start "token_env" "BROWSERLESS_TOKEN" ,@pairs))
       ,@body)))

(defun requests-to (path)
  "The requests whose route is PATH, oldest first."
  (reverse (remove path *requests* :test-not #'equal :key (lambda (request) (request-path (second request))))))

(defun query-argument (url name)
  "The value NAME carries in URL's query, or NIL."
  (cdr (assoc name (quri:uri-query-params (quri:uri url)) :test #'equal)))

(defun tool-text (name &rest members)
  "The text the tool called NAME answers, called as the provider's call is."
  (let ((text (princ-to-string
               (nle::message-content
                (execute-wire-call (format nil "browserless-call-~d" (incf *calls*)) name
                                    (if members
                                        (nlk:encode-json-object (apply #'nlk:make-json-object members))
                                        "{}"))))))
    text))

(defun tool-object (name &rest members)
  "TOOL-TEXT of NAME, decoded."
  (nlk:decode-json (apply #'tool-text name members)))

(defun page-octets (text)
  "TEXT's characters as the octet vector a binary route answers."
  (coerce (map 'list #'char-code text) '(vector (unsigned-byte 8))))

(defparameter +page+
  (concatenate
   'string
   "<html><head><title>Pricing</title><style>.a{color:#fff}</style>"
   "<script>window.nav=1;document.write('<p>not the page</p>')</script></head>"
   "<body><h1>Pricing</h1><p>The Pro plan is $20 a month.</p>"
   "<ul><li>Stealth&lt;b&gt; browser</li><li>Residential proxies</li></ul>"
   "<!-- a comment --><div>Contact sales</div></body></html>")
  "A page as /content answers it: scripts, a style, a comment and entities in it.")

;;; --- the lifecycle, from the one declaration -------------------------------

(define-cell-lifecycle-tests "browserless"
  (:fixture with-token)
  (:config "token_env" "BROWSERLESS_TOKEN")
  (:help :browserless)
  (:running
   (is (equal "https://production-sfo.browserless.io" (nodecode-browserless::base-url))
       "the default region endpoint")
   (loop for name in '("browserless_fetch" "browserless_screenshot" "browserless_pdf")
         do (is (nle::find-tool name) name)))
  (:refused ("token_env" 5) ("base_url" "production-sfo.browserless.io")))

(deftest browserless-cell-without-a-token-refuses ()
  ;; The one setting no tool can work without: the section names a token, and
  ;; a section that names none is refused at start, before anything installs.
  (let ((condition (signals-error nlk:config-refusal (browserless-start "token_env" ""))))
    (is (search "token" (princ-to-string condition)) "the refusal names what is missing")
    (is (search "token_env" (princ-to-string condition)) "and the setting that names it")
    (is (null (nle::find-tool "browserless_fetch")) "and installs nothing")))

;;; --- browserless_fetch ------------------------------------------------------

(deftest browserless-cell-fetches-a-page-as-its-words ()
  (with-fake-browserless (("POST" "/content" +page+))
    (with-browserless ()
      (let ((object (tool-object "browserless_fetch" "url" "https://shop.test/pricing")))
        (let ((content (nlk:json-value object :string "content")))
          (is (search "The Pro plan is $20 a month." content) "the page's words come through")
          (is (search "Pricing" content))
          (is (search "Stealth<b> browser" content) "an entity is written back as its character")
          (is (not (search "document.write" content)) "a script is not the page's words")
          (is (not (search "color:#fff" content)) "nor is a style")
          (is (not (search "a comment" content)) "nor a comment"))
        (is (eq nil (nlk:json-value object :boolean "truncated")) "the whole page fits")
        (is (equal "text" (nlk:json-value object :string "format")) "text unless asked otherwise"))
      (let* ((request (first (requests-to "/content")))
             (url (second request)))
        (is (equal :post (first request)))
        (is (equal "test-token" (query-argument url "token")) "the token rides the query")
        (is (equal "true" (query-argument url "stealth")) "stealth is on by default")
        (is (equal "30000" (query-argument url "timeout")))
        (let ((body (nlk:decode-json (third request))))
          (is (equal "https://shop.test/pricing" (nlk:json-value body :string "url"))))))))

(deftest browserless-cell-fetches-html-on-request ()
  (with-fake-browserless (("POST" "/content" +page+))
    (with-browserless ()
      (let ((object (tool-object "browserless_fetch" "url" "https://shop.test/raw" "format" "html")))
        (is (search "<script>" (nlk:json-value object :string "content")) "html is what the browser rendered")
        (is (equal "html" (nlk:json-value object :string "format")))))))

(deftest browserless-cell-caps-a-long-page-and-says-so ()
  (with-fake-browserless (("POST" "/content"
                            (concatenate 'string +page+
                                         (with-output-to-string (out)
                                           (dotimes (i 40)
                                             (write-string "<p>Another line of the page.</p>" out))))))
    (with-browserless ("content_chars" 500)
      (let ((object (tool-object "browserless_fetch" "url" "https://shop.test/pricing")))
        (is (eq t (nlk:json-value object :boolean "truncated")) "a page over the cap is cut")
        (is (< (length (nlk:json-value object :string "content")) 520) "cut, with the ellipsis")
        (is (search "of " (nlk:json-value object :string "note")) "and the note says how much there was")))))

(deftest browserless-cell-refuses-a-rejected-token ()
  (with-fake-browserless (("POST" "/content" "unauthorized" 401))
    (with-browserless ()
      (let ((text (tool-text "browserless_fetch" "url" "https://shop.test/pricing")))
        (is (search "ERROR" text))
        (is (search "rejected the API token" text) "the refusal names the token, not a stack")
        (is (search "HTTP 401" text))
        (is (search "token_env" text) "and the setting to check")))))

(deftest browserless-cell-explains-a-concurrency-limit ()
  (with-fake-browserless (("POST" "/content" "too many sessions" 429))
    (with-browserless ()
      (is (search "concurrency limit" (tool-text "browserless_fetch" "url" "https://shop.test/"))))))

;;; --- browserless_screenshot ---------------------------------------------------

(deftest browserless-cell-saves-a-screenshot ()
  (with-fake-browserless (("POST" "/screenshot" (page-octets "PNGDATA")))
    (with-browserless ()
      (let ((object (tool-object "browserless_screenshot" "url" "https://shop.test/pricing"
                                  "full_page" t)))
        (let ((path (nlk:json-value object :string "screenshot_path")))
          (is (search "browserless" (or path "")))
          (is (probe-file path) "the PNG is on disk")
          (is (search "(look " (nlk:json-value object :string "note")) "the answer says how to see it")
          (when (probe-file path) (ignore-errors (delete-file path)))))
        (let* ((request (first (requests-to "/screenshot")))
               (body (nlk:decode-json (third request))))
          (is (eq t (nlk:json-value body :boolean "options" "fullPage")) "full page was asked for")))))

(deftest browserless-cell-saves-a-pdf ()
  (with-fake-browserless (("POST" "/pdf" (page-octets "%PDF-1.4")))
    (with-browserless ()
      (let ((object (tool-object "browserless_pdf" "url" "https://shop.test/invoice" "landscape" t)))
        (let ((path (nlk:json-value object :string "pdf_path")))
          (is (search ".pdf" path))
          (is (probe-file path))
          (when (probe-file path) (ignore-errors (delete-file path))))
        (is (= 8 (nlk:json-value object :integer "bytes")) "the answer counts the file")))))

;;; --- /browserless --------------------------------------------------------------

(deftest browserless-cell-command-reports-its-state ()
  (with-fake-browserless ()
    (with-browserless ("proxy" "residential" "proxy_country" "us")
      (let ((answer (cell-entry "nodecode-browserless" "browserless" "")))
        (is (search "production-sfo.browserless.io" answer))
        (is (search "token: set" answer))
        (is (search "residential (us)" answer))
        (is (not (search "test-token" answer)) "the command never prints the token")
        (is (null *requests*) "and asks Browserless nothing")))))

(deftest browserless-cell-command-renders-one-page ()
  (with-fake-browserless (("POST" "/content" +page+))
    (with-browserless ()
      (let ((answer (cell-entry "nodecode-browserless" "browserless" "https://shop.test/pricing")))
        (is (search "The Pro plan is $20 a month." answer))
        (is (equal "test-token" (query-argument (second (first *requests*)) "token")))))))
