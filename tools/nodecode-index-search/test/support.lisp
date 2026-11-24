;;;; support.lisp --- the test slice, and a stubbed Meilisearch server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under an
;;;; INDEX-SEARCH-CELL- name prefix; RUN-INDEX-SEARCH-TESTS runs exactly that
;;;; slice.
;;;;
;;;; The one seam that reaches outside is NODECODE-INDEX-SEARCH::EXCHANGE, the
;;;; cell's only way to the network. WITH-WIRE replaces it with a canned
;;;; answer and records every call, so a test asserts on the request and
;;;; nothing ever dials. The watch file is a temporary file of the run's own,
;;;; so no test reads or writes the home's.

(in-package #:nodecode.test)

(define-test-slice "index-search" "INDEX-SEARCH-CELL-"
  :start nodecode-index-search:start-cell
  :defaults (("host" "http://127.0.0.1:7700") ("api_key" +key+)))

(defparameter +key+ "search-only-key"
  "The token a test's section carries.")

;;; --- the canned server ------------------------------------------------------------

(defvar *calls* '()
  "Every request the stub took, newest first: (:METHOD :URL :HEADERS :CONTENT).")

(defvar *answer* nil
  "A function of one call answering (values BODY STATUS), or NIL for health.")

(defun stub-exchange (method url &key headers content timeout)
  (declare (ignore timeout))
  (let ((call (list :method method :url url :headers headers :content content)))
    (push call *calls*)
    (if *answer*
        (funcall *answer* call)
        (values "{\"status\":\"available\"}" 200))))

(defmacro with-wire (&body body)
  "Run BODY with the cell's one network call stubbed and its calls recorded."
  `(let ((*calls* '())
         (*answer* nil))
     (with-stubbed-fdefinition (nodecode-index-search::exchange
                                (method url &key headers content timeout)
                                (stub-exchange method url :headers headers
                                                    :content content :timeout timeout))
       ,@body)))

;;; --- a cell of one test's own -----------------------------------------------------

(defun test-watch-file ()
  "A watch file no other test and no home shares."
  (merge-pathnames (format nil "index-search-~a.json" (gensym)) (uiop:temporary-directory)))

(defmacro with-index ((&rest pairs) &body body)
  "Run BODY with the cell started on PAIRS, its wire stubbed, its watch file a
temporary one, and everything stopped and deleted on unwind."
  `(with-wire
     (let ((nodecode-index-search::*state-file* (test-watch-file)))
       (unwind-protect
            (with-cell-stop ((index-search-start ,@pairs))
              ,@body)
         (ignore-errors (delete-file nodecode-index-search::*state-file*))))))

;;; --- calling what the cell contributes --------------------------------------------

(defun call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

(defun header (call name)
  "The value CALL sends in header NAME, or NIL."
  (cdr (assoc name (getf call :headers) :test #'string-equal)))

(defun sent-body (call)
  "CALL's body decoded, as the server would read it."
  (nlk:decode-json (or (getf call :content) "")))

;;; --- a page the server could answer -----------------------------------------------

(defun hit (&rest pairs)
  "One hit, built from alternating key/value PAIRS."
  (apply #'nlk:make-json-object pairs))

(defun index-page-text (hits total)
  "A search page carrying HITS and TOTAL, as the API writes it."
  (nlk:encode-json-object
   (nlk:json-object "hits" (coerce hits 'vector) "estimatedTotalHits" total)))

(defun answer-with (text &optional (status 200))
  "A stub answer answering TEXT at STATUS to every call."
  (setf *answer* (lambda (call) (declare (ignore call)) (values text status))))
