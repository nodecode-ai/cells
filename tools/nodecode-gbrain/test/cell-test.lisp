;;;; cell-test.lisp --- the cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every brain is a stubbed NLK:HTTP: nothing here dials 127.0.0.1:3131,
;;;; and the run has no network at all. What is proved is what the model and
;;;; the operator get: the pages ranked, the entity arm first, a page named
;;;; by both arms once, the bearer token on the wire, an event-stream body
;;;; read, a brain that holds nothing said so, a missing token and an
;;;; unreachable endpoint each one readable refusal -- and the lifecycle the
;;;; declaration derives (installed, idempotent, vetoable, refused).

(in-package #:nodecode.test)

(define-test-slice "gbrain" "GBRAIN-CELL-" :start nodecode-gbrain:start-cell)

(define-cell-lifecycle-tests "gbrain"
  (:config "url" "http://127.0.0.1:1/mcp" "max_pages" 3)
  (:hooks :round)
  (:help :gbrain)
  (:command "gbrain")
  (:running (is (= 3 (nodecode-gbrain::setting :max-pages)) "the section's cap is what runs")
            (is (equal nodecode-gbrain::*primer* (nle:help :gbrain)) "(help :gbrain) is the primer"))
  (:refused ("max_pages" 0))
  (:idle gbrain:gbrain-error (gbrain:recall "x")))

;;; --- fixtures -------------------------------------------------------------------

(defmacro with-gbrain ((&rest pairs) &body body)
  "BODY with the cell started on PAIRS, stopped however BODY leaves."
  `(with-cell-stop ((gbrain-start ,@pairs))
     ,@body))

(defun gbrain-reply (payload)
  "A JSON-RPC tools/call reply whose one text part is PAYLOAD, JSON text."
  (nlk:encode-json-object
   (nlk:json-object "jsonrpc" "2.0" "id" 3
                     "result" (nlk:json-object
                               "content" (vector (nlk:json-object "type" "text" "text" payload))))))

(defun gbrain-pages-json (pages)
  "PAGES, (SLUG TITLE SCORE) triples, as the JSON text a brain tool answers."
  (nlk:encode-json-object
   (nlk:json-object "pages"
                     (coerce (loop for (slug title score) in pages
                                   collect (nlk:json-object
                                            "slug" slug "title" title "confidence" score
                                            "synopsis" (format nil "~a, in one line" title)))
                             'vector))))

(defun gbrain-count (needle haystack)
  "How many times NEEDLE occurs in HAYSTACK."
  (loop with start = 0
        for at = (search needle haystack :start2 start)
        while at
        count 1
        do (setf start (+ at (length needle)))))

(defun gbrain-answer (content)
  "The reply for the arm CONTENT asks: the volunteered pages, or the queried ones."
  (if (search "volunteer_context" content)
      (gbrain-reply (gbrain-pages-json '(("people/ada" "Ada" 0.9))))
      (gbrain-reply (gbrain-pages-json '(("ops/build" "Build notes" 0.8)
                                          ("people/ada" "Ada again" 0.7))))))

;;; --- what the model gets ----------------------------------------------------------

(deftest gbrain-cell-ranks-the-entity-arm-first ()
  (with-gbrain ("url" "http://127.0.0.1:1/mcp" "token" "inert" "max_pages" 3)
    (let ((asked '()))
      (with-stubbed-fdefinition (nlk:http (method url &rest args)
        (let ((content (getf args :content)))
          (push (list method url (getf args :headers) content) asked)
          (values (gbrain-answer content) 200)))
        (let ((answer (gbrain:recall "what do we know about Ada")))
          (is (search "people/ada" answer) "the volunteered page is in the answer")
          (is (search "ops/build" answer) "and the topical one fills in")
          (is (< (search "people/ada" answer) (search "ops/build" answer)) "the entity hit comes first")
          (is (= 1 (gbrain-count "people/ada" answer)) "a page both arms name appears once")
          (is (= 2 (length asked)) "one volunteer call and one query call")
          (destructuring-bind (method url headers content) (second asked)
            (declare (ignore url content))
            (is (eq :post method) "the brain is posted to")
            (let ((authorization (cdr (assoc "Authorization" headers :test #'string-equal))))
              (is (equal "Bearer inert" authorization) "the bearer token goes on the wire"))))))))

(deftest gbrain-cell-reads-an-event-stream ()
  (with-gbrain ("url" "http://127.0.0.1:1/mcp" "token" "inert")
    (with-stubbed-fdefinition (nlk:http (method url &rest args)
      (declare (ignore method url args))
      (values (format nil "event: message~%data: ~a~%~%" (gbrain-answer "volunteer_context"))
              200))
      (is (search "people/ada" (gbrain:recall "Ada")) "a streamable-HTTP body is read as JSON"))))

(deftest gbrain-cell-says-when-the-brain-holds-nothing ()
  (with-gbrain ("url" "http://127.0.0.1:1/mcp" "token" "inert")
    (with-stubbed-fdefinition (nlk:http (method url &rest args)
      (declare (ignore method url args))
      (values (gbrain-reply "{\"pages\":[]}") 200))
      (is (search "no page" (gbrain:recall "nothing at all")) "an empty brain says so, it does not answer NIL")
      (is (search "Usage: /gbrain" (cell-entry "nodecode-gbrain" "gbrain" "")) "no argument says how to use it"))))

;;; --- the refusals -----------------------------------------------------------------

(deftest gbrain-cell-refuses-without-a-token ()
  (with-gbrain ("url" "http://127.0.0.1:1/mcp" "token_env" "GBRAIN_TEST_ABSENT_TOKEN" "token_file" "")
    (with-stubbed-fdefinition (nlk:http (method url &rest args)
      (declare (ignore method url args))
      (error "the brain must not be dialled without a token"))
      (is (search "token" (refusal-text gbrain:gbrain-error (gbrain:recall "x"))) "the setting to fill is named"))))

(deftest gbrain-cell-says-an-unreachable-brain ()
  (with-gbrain ("url" "http://127.0.0.1:1/mcp" "token" "inert")
    (with-stubbed-fdefinition (nlk:http (method url &rest args)
      (declare (ignore method url args))
      (error "connection refused"))
      (let ((said (refusal-text gbrain:gbrain-error (gbrain:recall "x"))))
        (is (search "did not answer" said) "the refusal says the brain did not answer")
        (is (search "127.0.0.1:1" said) "and names the endpoint it dialled")))))
