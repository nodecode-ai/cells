;;;; cell-test.lisp --- lifecycle, the three sources, /umt, a failure's shape.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What reaches outside this image is NODECODE-UMT::*HTTP*, the one function
;;;; every call dials, and WITH-UMT stubs it: each test answers its own URLs
;;;; from a route table, so no test touches the network and none depends on
;;;; what a live API says today. What is proved is that each verb asks the
;;;; configured base for the documented URL, reads the documented shape, and
;;;; renders it -- and that a tool, a command and a refusal all come out as
;;;; text the model can read.

(in-package #:nodecode.test)

(define-test-slice "umt" "UMT-CELL-" :start nodecode-umt:start-cell
  :defaults (("timeout" 20)))

(define-cell-lifecycle-tests "umt"
  (:help :umt)
  (:command "umt")
  (:running (is (equal nodecode-umt::+manual+ (nle:help :umt)) "(help :umt) is the manual")
            (is (equal "https://hacker-news.firebaseio.com/v0" (nodecode-umt::hn-api-base))
                "the section's default base")
            (is (equal "cs.AI" (nodecode-umt::setting :default-category)))
            (is (third (nle::find-tool "hn_top_stories")) "the ranking tool is registered")
            (is (third (nle::find-tool "arxiv_paper")))
            (is (third (nle::find-tool "npm_package_versions"))))
  (:refused ("timeout" 0) ("hn_api_base" 7))
  (:idle umt:umt-error (nodecode-umt::top-stories) (nodecode-umt::search-papers "x"))
  (:stopped (is (null (nle::find-tool "hn_top_stories")) "stop unregisters the tools")
            (is (null (nle::find-tool "npm_package_metadata")))))

;;; --- the stubbed wire -----------------------------------------------------------

(defvar *umt-routes* '()
  "((URL-FRAGMENT . ANSWER) ...): ANSWER takes the url and answers (values TEXT STATUS).")

(defvar *umt-requests* '()
  "((METHOD URL) ...) the stubbed wire received, newest first.")

(defun umt-dial (method url &key headers content &allow-other-keys)
  "The stub standing in for NLK:HTTP: record the exchange, answer the first
route whose fragment URL carries, and an empty object when none does."
  (declare (ignore headers content))
  (push (list method url) *umt-requests*)
  (loop for (fragment . answer) in *umt-routes*
        when (search fragment url)
          do (return-from umt-dial (funcall answer url)))
  (values "{}" 200))

(defmacro with-umt ((&rest pairs) &body body)
  "BODY with the cell started on the section PAIRS, its one dial stubbed, and
nothing the test did not route answered."
  `(with-saved-globals (nle:*tools* nodecode-umt::*http*)
     (let ((*umt-routes* '())
           (*umt-requests* '()))
       (setf nodecode-umt::*http* #'umt-dial)
       (with-cell-stop ((umt-start ,@pairs))
         ,@body))))

(defun umt-answer (object)
  "OBJECT encoded, as one 200."
  (values (nlk:encode-json-object object) 200))

(defun umt-json (url)
  (declare (ignore url))
  (values "{}" 200))

(defun umt-route (fragment answer)
  "Answer every URL carrying FRAGMENT with ANSWER, a function of the url."
  (push (cons fragment answer) *umt-routes*))

(defun umt-requested ()
  "The exchanges the stub received, oldest first."
  (reverse *umt-requests*))

(defun umt-reported (condition) (format nil "~a" condition))

;;; --- Hacker News -------------------------------------------------------------------

(defun hn-story-object (id &rest pairs)
  "One Hacker News item as the Firebase API writes it."
  (let ((object (nlk:make-json-object "id" id "type" "story" "by" "someone" "score" 42
                                       "descendants" 7 "time" 1700000000)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key object) value))
    object))

(deftest umt-cell-top-stories-lists-the-ranking ()
  (with-umt ()
    (umt-route "topstories.json" (lambda (url) (declare (ignore url)) (values "[1,2,3]" 200)))
    (umt-route "/item/1.json"
               (lambda (url) (declare (ignore url))
                 (umt-answer (hn-story-object 1 "title" "A story about Lisp"
                                                 "url" "https://example.com/lisp"))))
    (umt-route "/item/2.json"
               (lambda (url) (declare (ignore url))
                 (umt-answer (hn-story-object 2 "type" "job" "title" "We are hiring"))))
    (umt-route "/item/3.json"
               (lambda (url) (declare (ignore url))
                 (umt-answer (hn-story-object 3 "deleted" t "title" "Gone"))))
    (let ((text (nodecode-umt::top-stories :limit 3)))
      (is (search "1. A story about Lisp" text) "the story, numbered")
      (is (search "42 points, 7 comments" text) "its numbers")
      (is (search "https://example.com/lisp" text) "its url")
      (is (search "hn item 1" text) "its id, for a thread")
      (is (search "2023-11-14" text) "its date, as an ISO stamp")
      (is (not (search "hiring" text)) "a job is not a story")
      (is (not (search "Gone" text)) "a deleted item is not a story"))
    (is (equal "https://hacker-news.firebaseio.com/v0/topstories.json"
               (second (first (umt-requested)))) "the section's base asked for the ranking")))

(deftest umt-cell-a-search-reads-the-algolia-index ()
  (with-umt ()
    (umt-route "/search?"
               (lambda (url) (declare (ignore url))
                 (umt-answer (nlk:json-object
                              "hits" (vector (nlk:json-object
                                               "objectID" "4242" "title" "Show HN: a Lisp"
                                               "url" "https://example.com/show" "author" "maker"
                                               "points" 99 "num_comments" 12
                                               "created_at" "2026-01-02T03:04:05Z"
                                               "story_text" "<p>It is conses &amp; closures.</p>"))))))
    (let ((text (nodecode-umt::search-stories "lisp" :limit 5)))
      (is (search "Hacker News stories matching \"lisp\"" text))
      (is (search "1. Show HN: a Lisp" text))
      (is (search "99 points, 12 comments" text))
      (is (search "It is conses & closures." text) "the story text's entities decoded, its tags gone"))
    (let ((url (second (first (umt-requested)))))
      (is (search "hn.algolia.com/api/v1/search?" url) "the search base")
      (is (search "tags=story" url))
      (is (search "query=lisp" url))
      (is (search "hitsPerPage=5" url)))))

(deftest umt-cell-a-thread-indents-its-replies ()
  (with-umt ()
    (umt-route "/item/7.json"
               (lambda (url) (declare (ignore url))
                 (umt-answer (hn-story-object 7 "title" "Ask HN: a question"
                                                 "kids" (vector 8)))))
    (umt-route "/item/8.json"
               (lambda (url) (declare (ignore url))
                 (umt-answer (nlk:json-object "id" 8 "type" "comment" "by" "answerer"
                                               "time" 1700000100 "text" "An answer"
                                               "kids" (vector 9)))))
    (umt-route "/item/9.json"
               (lambda (url) (declare (ignore url))
                 (umt-answer (nlk:json-object "id" 9 "type" "comment" "by" "second"
                                               "time" 1700000200 "text" "A follow-up"))))
    (let ((text (nodecode-umt::item-thread 7 :depth 2)))
      (is (search "Ask HN: a question" text))
      (is (search "answerer" text) "the first reply")
      (is (search "A follow-up" text) "its own reply, one level in")
      (is (search "    second" text) "indented under its parent"))))

(deftest umt-cell-a-ranking-tool-answers-through-its-contract ()
  (with-umt ()
    (umt-route "topstories.json" (lambda (url) (declare (ignore url)) (values "[1,1]" 200)))
    (umt-route "/item/1.json"
               (lambda (url) (declare (ignore url))
                 (umt-answer (hn-story-object 1 "title" "Only one"))))
    (let ((tool (third (nle::find-tool "hn_top_stories")))
          (contract (nlk:contract 'nodecode-umt::hn-top-stories)))
      (is (search "1. Only one" (funcall tool (nlk:contract-decode contract (nlk:make-json-object "limit" 1))))
          "the tool reads its contract and answers the verb's text")
      (is (search "1. Only one" (funcall tool (nlk:contract-decode contract (nlk:make-json-object))))
          "a call that names no limit takes the contract's default"))))

;;; --- arXiv ----------------------------------------------------------------------------

(defparameter +umt-arxiv-feed+
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<feed xmlns=\"http://www.w3.org/2005/Atom\">
  <title>ArXiv Query</title>
  <entry>
    <id>http://arxiv.org/abs/2405.00001v1</id>
    <updated>2024-05-01T12:00:00Z</updated>
    <published>2024-05-01T12:00:00Z</published>
    <title>A Test Paper
      About Lists</title>
    <summary>We show that lists are conses &amp; nothing more.</summary>
    <author><name>Ada Example</name></author>
    <author><name>Bea Example</name></author>
    <link href=\"http://arxiv.org/abs/2405.00001v1\" rel=\"alternate\" type=\"text/html\"/>
    <link title=\"pdf\" href=\"http://arxiv.org/pdf/2405.00001v1\" rel=\"related\" type=\"application/pdf\"/>
    <arxiv:primary_category term=\"cs.AI\" scheme=\"http://arxiv.org/schemas/atom\"/>
    <category term=\"cs.AI\" scheme=\"http://arxiv.org/schemas/atom\"/>
    <category term=\"cs.LO\" scheme=\"http://arxiv.org/schemas/atom\"/>
  </entry>
</feed>")

(deftest umt-cell-arxiv-search-renders-a-feed ()
  (with-umt ()
    (umt-route "arxiv.org/api/query"
               (lambda (url) (declare (ignore url)) (values +umt-arxiv-feed+ 200)))
    (let ((text (nodecode-umt::search-papers "lists" :max 1)))
      (is (search "A Test Paper About Lists" text) "the title, its whitespace collapsed")
      (is (search "2405.00001v1" text) "the identifier, without the abs url")
      (is (search "Ada Example, Bea Example" text) "its authors")
      (is (search "cs.AI, cs.LO (primary cs.AI)" text) "its categories")
      (is (search "http://arxiv.org/pdf/2405.00001v1" text) "its pdf")
      (is (search "lists are conses & nothing more." text) "the abstract, decoded"))
    (let ((url (second (first (umt-requested)))))
      (is (search "search_query=all%3Alists" url) "the query, as one all: term")
      (is (search "max_results=1" url)))))

(deftest umt-cell-arxiv-a-paper-nothing-answers-for-refuses ()
  (with-umt ()
    (umt-route "arxiv.org/api/query"
               (lambda (url) (declare (ignore url)) (values "<feed></feed>" 200)))
    (is (search "arXiv has no paper \"2405.99999\""
                (umt-reported (handler-case (nodecode-umt::paper "2405.99999")
                                (umt:umt-error (condition) condition))))
        "an empty feed is a refusal naming the identifier")))

;;; --- npm ---------------------------------------------------------------------------------

(defun npm-metadata-object ()
  (nlk:json-object
   "name" "express" "description" "Fast web framework" "license" "MIT"
   "homepage" "https://expressjs.com"
   "dist-tags" (nlk:json-object "latest" "4.19.2" "next" "5.0.0")
   "time" (nlk:json-object "created" "2010-01-01T00:00:00Z"
                            "4.19.2" "2024-03-20T00:00:00Z"
                            "5.0.0" "2024-01-01T00:00:00Z")
   "versions" (nlk:json-object "4.19.2" (nlk:json-object "deprecated" "use 5")
                                "5.0.0" (nlk:json-object))
   "repository" (nlk:json-object "url" "git+https://example.com/express.git")
   "maintainers" (vector (nlk:json-object "name" "one") (nlk:json-object "name" "two"))))

(deftest umt-cell-npm-metadata-names-what-the-registry-says ()
  (with-umt ()
    (umt-route "registry.npmjs.org/express"
               (lambda (url) (declare (ignore url)) (umt-answer (npm-metadata-object))))
    (let ((text (nodecode-umt::package-metadata "express")))
      (is (search "express - Fast web framework" text))
      (is (search "latest 4.19.2 (published 2024-03-20T00:00:00Z)" text))
      (is (search "license MIT; 2 versions" text))
      (is (search "dist-tags: latest=4.19.2, next=5.0.0" text))
      (is (search "repository: git+https://example.com/express.git" text))
      (is (search "maintainers: one, two" text)))
    (is (equal "https://registry.npmjs.org/express" (second (first (umt-requested))))
        "the section's registry, the name as one path segment")))

(deftest umt-cell-npm-versions-newest-first ()
  (with-umt ()
    (umt-route "registry.npmjs.org/express"
               (lambda (url) (declare (ignore url)) (umt-answer (npm-metadata-object))))
    (let ((text (nodecode-umt::package-versions "express")))
      (is (search "express: 2 versions, newest 4.19.2" text))
      (is (search "4.19.2 - 2024-03-20T00:00:00Z [deprecated]" text) "a deprecated version says so")
      (is (search "5.0.0 - 2024-01-01T00:00:00Z" text))
      (is (< (search "4.19.2 -" text) (search "5.0.0 -" text)) "the newer release first"))))

(deftest umt-cell-npm-search-reads-the-registry-index ()
  (with-umt ()
    (umt-route "/-/v1/search?"
               (lambda (url) (declare (ignore url))
                 (umt-answer (nlk:json-object
                              "objects" (vector (nlk:json-object
                                                 "package" (nlk:json-object
                                                            "name" "express" "version" "4.19.2"
                                                            "description" "Fast web framework"
                                                            "date" "2024-03-20T00:00:00Z"
                                                            "links" (nlk:json-object
                                                                     "npm" "https://www.npmjs.com/package/express"))
                                                 "score" (nlk:json-object "final" 0.9)))))))
    (let ((text (nodecode-umt::search-packages "web framework")))
      (is (search "1. express@4.19.2" text))
      (is (search "Fast web framework" text))
      (is (search "https://www.npmjs.com/package/express" text)))
    (let ((url (second (first (umt-requested)))))
      (is (search "text=web%20framework" url) "the query, percent-encoded")
      (is (search "size=10" url)))))

;;; --- the command, and a failure ------------------------------------------------------------

(deftest umt-cell-the-command-answers-a-source-by-hand ()
  (with-umt ()
    (umt-route "/search?"
               (lambda (url) (declare (ignore url))
                 (umt-answer (nlk:json-object
                              "hits" (vector (nlk:json-object
                                               "objectID" "77" "title" "From the command"
                                               "points" 5 "num_comments" 1
                                               "created_at" "2026-02-03T00:00:00Z"))))))
    (multiple-value-bind (text dialog) (nodecode-umt::run-slash "hn lisp" nil)
      (is (search "1. From the command" text) "/umt hn QUERY searches")
      (is (search "5 points, 1 comments" text))
      (is dialog "and answers a panel beside it"))
    (multiple-value-bind (text dialog) (nodecode-umt::run-slash "" nil)
      (is (search "umt: Hacker News at" text) "bare /umt says where the bases point")
      (is dialog))
    (is (search "umt: search npm for what?" (nodecode-umt::run-slash "npm" nil))
        "a verb with nothing after it asks without dialing")
    (is (search "is not one of its choices" (nodecode-umt::run-slash "spotify x" nil)))))

(deftest umt-cell-a-failed-status-answers-a-failure-the-model-reads ()
  (with-umt ()
    (umt-route "topstories.json"
               (lambda (url) (declare (ignore url)) (values "{\"error\":\"boom\"}" 500)))
    (let ((answer (funcall (third (nle::find-tool "hn_top_stories"))
                            (nlk:contract-decode (nlk:contract 'nodecode-umt::hn-top-stories)
                                                 (nlk:make-json-object "limit" 3)))))
      (is (search "ERROR: UMT-ERROR" answer) "the tool answers a failure, not a signal")
      (is (search "HTTP 500" answer) "with the status")
      (is (search "boom" answer) "and what the API said"))))
