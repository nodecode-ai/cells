;;;; support.lisp --- the wikipedia test runner and its scripted seam.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; WIKIPEDIA-CELL- prefix, and RUN-WIKIPEDIA-TESTS runs exactly that
;;;; slice, so this system's test-op never re-runs the core suite.
;;;;
;;;; The one network call is NODECODE-WIKIPEDIA::HTTP; WITH-SCRIPTED-WIKIPEDIA
;;;; stubs that name, records every call and answers scripted JSON, so no
;;;; test ever opens a socket.

(in-package #:nodecode.test)

(define-test-slice "wikipedia" "WIKIPEDIA-CELL-" :start nodecode-wikipedia:start-cell)

;;; --- config and lifecycle fixtures ----------------------------------------------

(defmacro with-wikipedia ((&rest config-pairs) &body body)
  "Run BODY with the cell started on CONFIG-PAIRS and stopped on unwind."
  `(with-cell-stop ((wikipedia-start ,@config-pairs))
     ,@body))

;;; --- the scripted seam -----------------------------------------------------------

(defun wiki-json (&rest specs)
  "A JSON text body built from alternating key/value pairs."
  (nlk:encode-json-object (apply #'nlk:make-json-object specs)))

(defun wiki-object (&rest specs)
  "One JSON object built from alternating key/value pairs."
  (apply #'nlk:make-json-object specs))

(defun wiki-pages (&rest pages)
  "The API's answer for PAGES, as query.pages carries them."
  (wiki-json "query" (wiki-object "pages" (coerce pages 'vector))))

(defstruct (wiki-http (:copier nil))
  url params timeout)

(defun wiki-param (call name)
  "The value CALL asked for under NAME, or NIL."
  (cdr (assoc name (wiki-http-params call) :test #'string=)))

(defmacro with-scripted-wikipedia ((calls &key body) &body forms)
  "Run FORMS with the cell's one network call stubbed: every call recorded
into the list CALLS, oldest first, and BODY -- a JSON string, or a function
of the call answering one -- answered as HTTP 200."
  `(let ((,calls '()))
     (with-stubbed-fdefinition (nodecode-wikipedia::http (url params timeout)
                                  (let ((call (make-wiki-http :url url :params params
                                                              :timeout timeout)))
                                    (setf ,calls (append ,calls (list call)))
                                    (values (if (functionp ,body) (funcall ,body call) ,body)
                                            200)))
       ,@forms)))

;;; --- calling what the cell contributes -------------------------------------------

(defun wikipedia-tool (title &optional (section ""))
  "The registered `wikipedia' tool, called the way the engine calls it: the
declaration materializes the record, the body answers the text."
  (let ((entry (nle::find-tool "wikipedia")))
    (assert entry () "the wikipedia tool is not registered")
    (funcall (third entry)
             (nlk:contract-decode (second entry)
                                   (nlk:make-json-object "title" title "section" section)))))
