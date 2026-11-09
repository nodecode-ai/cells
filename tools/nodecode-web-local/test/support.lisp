;;;; support.lisp --- the web-local test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry as every cell's
;;;; (the core DEFTEST, with its hermetic machine-state posture) under a
;;;; WEB-LOCAL-CELL- name prefix; RUN-WEB-LOCAL-TESTS runs exactly that slice.
;;;;
;;;; The one outside thing this cell does is an HTTP exchange, so that is what
;;;; the tests stub: the special variable NODECODE-WEB-LOCAL::*HTTP* is rebound
;;;; to a Lisp function that answers the pages a test hands it and records every
;;;; address asked for. No test needs a network, and none touches one.

(in-package #:nodecode.test)

(define-test-slice "web-local" "WEB-LOCAL-CELL-" :start nodecode-web-local:start-cell)

(defvar *requests* '()
  "The addresses the stub was asked for, newest first.")

(defparameter +bridge+ "https://example.org/a-bridge"
  "The address the tests read.")

(defparameter +gone+ "https://example.org/gone"
  "An address the stub answers 404 for.")

(defparameter +plain+ "https://example.org/notes.txt"
  "An address the stub answers as text/plain.")

(defparameter +page-html+
  "<html><head><title>A Bridge in the Fog</title>
<meta name=\"description\" content=\"How one town rebuilt its crossing.\">
<meta name=\"author\" content=\"J. Doe\">
<meta property=\"og:site_name\" content=\"Example\">
<meta property=\"article:published_time\" content=\"2024-03-01\">
<script>var x = '<p>not prose</p>';</script></head>
<body><nav><a href=\"/sections\">Sections</a></nav>
<main><h1>A Bridge in the Fog</h1>
<p>The river <b>rose</b> &amp; the old crossing went with it.</p>
<pre><code>(bridge-rebuild)</code></pre>
<ul><li>Stone</li><li>Timber</li></ul>
<p>See <a href=\"/costs\">the costs</a>.</p></main></body></html>"
  "One page: a title, four meta tags, a script, navigation outside <main>, a
heading, a paragraph with an entity and an inline tag, a code block, a list and a
relative link.")

(defun web-local-octets (text)
  "TEXT as the octets a response carries."
  (sb-ext:string-to-octets text :external-format :utf-8))

(defun page-route (url &key (body +page-html+) (status 200) (content-type "text/html")
                        (truncated nil))
  "One stubbed answer: URL answered STATUS with BODY under CONTENT-TYPE."
  (list url (if (stringp body) (web-local-octets body) body)
        status (nlk:make-json-object "content-type" content-type) truncated))

(defun stub-http (routes)
  "A function the cell's *HTTP* can be: it answers the ROUTE for the address it
is asked for, and records every address."
  (lambda (url &key timeout max-bytes)
    (declare (ignore timeout max-bytes))
    (push url *requests*)
    (let ((route (assoc url routes :test #'string=)))
      (unless route (error "the tests stub no answer for ~a" url))
      (destructuring-bind (octets status headers truncated) (rest route)
        (values octets status headers truncated)))))

(defmacro with-http (routes &body body)
  "BODY with the network stubbed: ROUTES are PAGE-ROUTE forms."
  `(let ((nodecode-web-local::*http* (stub-http (list ,@routes)))
         (*requests* '()))
     ,@body))

(defun refused (url &rest keys)
  "What reading URL answers as a refusal, in the words the model would read."
  (handler-case (apply #'web-local:extract url keys)
    (error (condition) (princ-to-string condition))))
