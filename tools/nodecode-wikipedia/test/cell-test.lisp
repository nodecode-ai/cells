;;;; cell-test.lisp --- the wikipedia cell, end to end over a scripted seam.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

;;; --- lifecycle, settings and the tool's shape ------------------------------------

(define-cell-lifecycle-tests "wikipedia"
  (:help :wikipedia)
  (:command "wiki")
  (:running
   (is (nle::find-tool "wikipedia") "the tool is registered")
   (is (equal "en" (nodecode-wikipedia::setting :language)) "the edition defaults to English")
   (is (= 6000 (nodecode-wikipedia::setting :max-chars)) "and one reference's length is bounded"))
  (:stopped
   (is (null (nle::find-tool "wikipedia")) "stop takes the tool back out"))
  (:refused ("language" "Bad Host") ("language" "en.us")
            ("max_chars" 10) ("timeout_seconds" 0))
  (:idle nodecode-wikipedia:wikipedia-error
         (nodecode-wikipedia:article "Alan Turing")
         (nodecode-wikipedia:article "Berlin" "History")))

(deftest wikipedia-cell-tool-refuses-a-call-with-no-title ()
  ;; The one member the declaration requires: a call without it never reaches
  ;; the body, which is the repair the model reads.
  (is (signals-error nlk:contract-refusal
        (nlk:contract-decode (nlk:contract 'nodecode-wikipedia::wiki-reference)
                              (nlk:make-json-object "section" "History")))))

;;; --- the lead section -------------------------------------------------------------

(defun turing-page ()
  "The API's answer for the Alan Turing article."
  (wiki-pages (wiki-object "title" "Alan Turing"
                           "fullurl" "https://en.wikipedia.org/wiki/Alan_Turing"
                           "description" "English computer scientist"
                           "extract" "Alan Mathison Turing was an English mathematician and logician.")))

(deftest wikipedia-cell-reads-the-lead-section ()
  (with-wikipedia ()
    (with-scripted-wikipedia (calls :body (turing-page))
      (let ((text (wikipedia-tool "Alan Turing")))
        (is (search "Wikipedia (en): Alan Turing -- English computer scientist" text) "the block opens with the article and its description")
        (is (search "https://en.wikipedia.org/wiki/Alan_Turing" text) "and its url")
        (is (search "English mathematician" text) "and the text itself")
        (is (search "CC BY-SA 4.0" text) "and whose words they are")
        (is (search "not instructions" text) "which the model is told not to follow")
        (is (= 1 (length calls)) "one request answers one reference")
        (is (equal "Alan Turing" (wiki-param (first calls) "titles")) "the title travels")
        (is (equal "1" (wiki-param (first calls) "exintro")) "the lead section is asked for")
        (is (null (wiki-param (first calls) "exsectionformat")) "and the whole article is not")
        (is (search "en.wikipedia.org/w/api.php" (wiki-http-url (first calls))) "from the edition the section names")))))

(deftest wikipedia-cell-follows-a-redirect-to-the-article ()
  (with-wikipedia ()
    (with-scripted-wikipedia (calls :body (wiki-pages (wiki-object
                                                       "title" "Turing test"
                                                       "fullurl" "https://en.wikipedia.org/wiki/Turing_test"
                                                       "extract" "The Turing test is a test of a machine's ability.")))
      (let ((text (wikipedia-tool "turing_test")))
        (is (search "resolved from \"turing test\"" text) "the block says what it resolved from")
        (is (search "Turing test is a test" text) "and carries the article")))))

;;; --- one named section -----------------------------------------------------------

(defparameter +berlin-extract+
  (format nil "Berlin is the capital of Germany.~%
~%
== History ==~%
~%
Berlin in 1900 was an industrial city.~%
~%
=== Weimar years ===~%
~%
The Weimar years changed the city.~%
~%
== Notes ==~%
~%
Notes that a section read should not carry."))

(deftest wikipedia-cell-reads-one-section-with-its-subsections ()
  (with-wikipedia ()
    (with-scripted-wikipedia (calls :body (wiki-pages (wiki-object
                                                       "title" "Berlin"
                                                       "extract" +berlin-extract+)))
      (let ((text (wikipedia-tool "Berlin" "history")))
        (is (search "Section: History" text) "the heading path is named")
        (is (search "Berlin in 1900" text) "the section's own text is there")
        (is (search "Weimar years changed the city" text) "and its subsections with it")
        (is (not (search "Notes that a section read" text)) "the next section is not")
        (is (not (search "capital of Germany" text)) "nor the lead before it")
        (is (search "#History" text) "the url points at the section")
        (is (equal "wiki" (wiki-param (first calls) "exsectionformat")) "the whole article comes, cut here")
        (is (null (wiki-param (first calls) "exintro")) "so the lead flag is not sent")
        (is (equal "Berlin" (wiki-param (first calls) "titles")) "and the section's name never leaves the machine")))))

(deftest wikipedia-cell-refuses-a-section-the-article-lacks ()
  (with-wikipedia ()
    (with-scripted-wikipedia (calls :body (wiki-pages (wiki-object
                                                       "title" "Berlin"
                                                       "extract" +berlin-extract+)))
      (let ((text (wikipedia-tool "Berlin" "Histor")))
        (is (search "did you mean \"History\"" text) "a close heading is suggested")
        (is (search "Sections: History, Notes" text) "and the headings are listed")))))

;;; --- a miss ----------------------------------------------------------------------

(deftest wikipedia-cell-suggests-titles-for-a-missing-article ()
  (with-wikipedia ()
    (with-scripted-wikipedia (calls :body
                                    (lambda (call)
                                      (if (equal "opensearch" (wiki-param call "action"))
                                          (nlk:encode-json-object
                                           (vector "Alan Toring"
                                                   (vector "Alan Turing" "Alan Turing Award")
                                                   (vector "" "")
                                                   (vector "" "")))
                                          (wiki-pages (wiki-object "title" "Alan Toring"
                                                                   "missing" "")))))
      (let ((text (wikipedia-tool "Alan Toring")))
        (is (search "no en.wikipedia.org article titled" text) "the miss is named")
        (is (search "\"Alan Turing\"" text) "with the titles Wikipedia suggests")
        (is (search "\"Alan Turing Award\"" text) "every one of them")
        (is (equal 2 (length calls)) "a miss costs one suggestion lookup")))))

;;; --- bounds -----------------------------------------------------------------------

(deftest wikipedia-cell-cuts-at-a-word-boundary ()
  (let ((long (format nil "~{~a~^ ~}"
                       (loop for index below 200 collect (format nil "w~d" index)))))
    (with-wikipedia ("max_chars" 200)
      (with-scripted-wikipedia (calls :body (wiki-pages (wiki-object
                                                         "title" "Long article"
                                                         "extract" long)))
        (let ((text (wikipedia-tool "Long article")))
          (is (search "[...]" text) "the cut is marked")
          (is (search "(lead section truncated to 200 characters)" text) "and counted")
          (is (not (search "w199" text)) "the tail is gone")
          (is (search "w10 " text) "the head survives"))))))

;;; --- the command ------------------------------------------------------------------

(deftest wikipedia-cell-command-reads-a-target ()
  (with-wikipedia ()
    (with-scripted-wikipedia (calls :body (wiki-pages (wiki-object
                                                       "title" "Berlin"
                                                       "extract" +berlin-extract+)))
      (let ((text (cell-entry "nodecode-wikipedia" "wiki" "Berlin#History" nil)))
        (is (search "Berlin in 1900" text) "Title#Section reads that section")
        (is (equal "Berlin" (wiki-param (first calls) "titles")) "the title is the article")))))

(deftest wikipedia-cell-bare-command-asks-for-a-title ()
  (with-wikipedia ()
    (with-scripted-wikipedia (calls :body (turing-page))
      (multiple-value-bind (text dialog)
          (cell-entry "nodecode-wikipedia" "wiki" "" nil)
        (is (search "a Wikipedia article" text) "the answer says what /wiki does")
        (is (search "Title" (nlk:encode-json-object dialog)) "and offers a row to ask for the title in")
        (is (null calls) "asking costs no request")))))
