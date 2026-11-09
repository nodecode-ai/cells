;;;; cell-test.lisp --- the tool, the command, the verb, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's wiring against a stubbed network: that
;;;; its start installs the tool, the command and the manual and its stop takes
;;;; them back, that the tool reads every address it is given and answers each
;;;; page as the plugin answered it -- title, address, one line of metadata, the
;;;; description quoted, the article as markdown -- that a page which could not
;;;; be read is a line of its own and never takes the others with it, that the
;;;; command reads what the operator types, and that the refusals say why. No
;;;; test needs a network.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "web-local"
  (:help :web-local)
  (:command "web-local")
  (:running (is (= 30 (nodecode-web-local::setting :timeout-seconds)) "half a minute per page by default")
            (is (= 120 (nodecode-web-local::setting :budget-seconds)) "and two minutes for the call")
            (is (= 10485760 (nodecode-web-local::setting :max-bytes)) "with a 10MB page at most"))
  (:refused ("timeout_seconds" 0) ("budget_seconds" 0) ("max_bytes" 10))
  (:idle web-local:web-local-error (web-local:extract "https://example.org/a-bridge")))

;;; --- the tool ---------------------------------------------------------------------

(deftest web-local-cell-the-tool-reads-every-address-in-one-answer ()
  (with-cell-stop ((web-local-start))
    (with-http ((page-route +bridge+)
                (page-route +gone+ :status 404))
      (let* ((entry (nle::find-tool "local_extract"))
             (call (nlk:contract-decode (second entry)
                                         (nlk:make-json-object
                                          "urls" (vector +bridge+ +gone+))))
             (answer (funcall (third entry) call)))
        (is (search "# A Bridge in the Fog" answer) "the article is the answer, as a heading")
        (is (search "> How one town rebuilt its crossing." answer) "the description is quoted")
        (is (search "Example · by J. Doe · published 2024-03-01" answer) "one line says what the page says about itself")
        (is (search "[the costs](https://example.org/costs)" answer) "a link is markdown, resolved against the page")
        (is (search "- Stone" answer) "a list item is a line of its own")
        (is (search "The river rose & the old crossing" answer) "an entity is decoded and an inline tag is nothing")
        (is (search "```" answer) "a code block is fenced")
        (is (unchanged-metadata-p answer) "the script in the head is not prose")
        (is (search "ERROR: HTTP 404" answer) "a page that could not be read is a line of its own")
        (is (search +bridge+ answer) "and each answer names its own address")
        (is (equal (list +bridge+ +gone+) (reverse *requests*)) "both addresses were asked for, in order")))))

(defun unchanged-metadata-p (answer)
  "T when the page's script and navigation reached nothing."
  (and (null (search "not prose" answer))
       (null (search "Sections" answer))
       t))

(deftest web-local-cell-a-positive-clip-cuts-and-says-how-much-is-left ()
  (with-cell-stop ((web-local-start))
    (with-http ((page-route +bridge+))
      (let* ((entry (nle::find-tool "local_extract"))
             (call (nlk:contract-decode (second entry)
                                         (nlk:make-json-object "urls" (vector +bridge+)
                                                                "max_chars" 40)))
             (answer (funcall (third entry) call)))
        (is (search "clipped at 40 of" answer) "the cut is said out loud")
        (is (search "max_chars 0" answer) "with the way to read the rest")
        (is (eql 0 (search "A Bridge" answer)) "and the answer opens with the page")))))

(deftest web-local-cell-text-format-answers-without-markers ()
  (with-cell-stop ((web-local-start))
    (with-http ((page-route +bridge+))
      (let ((answer (web-local:extract +bridge+ :format "text")))
        (is (search "A Bridge in the Fog" answer) "the page is there")
        (is (null (search "# A Bridge in the Fog" answer)) "without its heading marker")
        (is (search "the costs (https://example.org/costs)" answer) "a link as the label and then its address")))))

;;; --- the command ------------------------------------------------------------------

(deftest web-local-cell-the-command-reads-a-page-and-asks-for-one-with-none ()
  (with-cell-stop ((web-local-start))
    (with-http ((page-route +bridge+))
      (let ((answer (cell-entry "nodecode-web-local" "web-local" +bridge+)))
        (is (search "# A Bridge in the Fog" answer) "the operator reads the page")
        (is (equal (list +bridge+) *requests*) "through the one network seam"))
      (let ((before *requests*))
        (is (search "type one or more URLs" (cell-entry "nodecode-web-local" "web-local" "")) "no argument asks for one")
        (is (equal before *requests*) "and reads nothing")))))

;;; --- every refusal ----------------------------------------------------------------

(deftest web-local-cell-every-way-a-page-fails-says-why ()
  (with-cell-stop ((web-local-start))
    (with-http ((page-route +bridge+ :content-type "application/pdf")
                (page-route +gone+ :status 404)
                (page-route +plain+ :body "A note, plainly." :content-type "text/plain"))
      (is (search "Unsupported content type 'application/pdf'" (web-local:extract +bridge+)) "a document this reader does not reduce says so")
      (is (search "ERROR: HTTP 404" (web-local:extract +gone+)) "a missing page is a fact")
      (is (search "A note, plainly." (web-local:extract +plain+)) "plain text is returned as it is"))
    (with-http ((page-route +bridge+ :truncated t))
      (is (search "exceeds the 10MB local extract limit" (web-local:extract +bridge+)) "a page past its cap says which cap"))
    (with-http ((page-route +bridge+ :body ""))
      (is (search "No readable content" (web-local:extract +bridge+)) "a page with no prose is named as such"))))

(deftest web-local-cell-an-address-that-is-no-page-is-refused ()
  (with-cell-stop ((web-local-start))
    (with-http ()
      (is (search "http://" (refused "example.org/a-bridge")) "a bare address is refused")
      (is (search "non-empty" (refused "   ")) "and so is an empty one")
      (is (search "private or internal" (refused "http://127.0.0.1/admin")) "a loopback address is blocked, as the plugin blocked it")
      (is (search "private or internal" (refused "http://192.168.1.1/router")) "and a private network one")
      (is (null *requests*) "nothing was asked for"))))

(deftest web-local-cell-one-call-takes-a-bounded-list ()
  (with-cell-stop ((web-local-start))
    (with-http ((page-route +bridge+))
      (let ((urls (make-list 9 :initial-element +bridge+)))
        (is (search "at most 8 addresses" (handler-case (web-local:extract-many urls) (error (condition) (princ-to-string condition)))) "nine addresses are one too many")
        (is (null *requests*) "and nothing was asked for")))))
