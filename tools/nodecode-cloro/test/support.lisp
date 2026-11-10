;;;; support.lisp --- the test slice: a stubbed cloro API.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; CLORO-CELL- name prefix; RUN-CLORO-TESTS runs exactly that slice.
;;;;
;;;; The one seam that reaches outside is NODECODE-CLORO::HTTP, the cell's
;;;; only way to the network. WITH-WIRE replaces it with a canned answer and
;;;; records every call, so a test asserts on the request and nothing ever
;;;; dials.

(in-package #:nodecode.test)

(define-test-slice "cloro" "CLORO-CELL-" :start nodecode-cloro:start-cell)

;;; --- the canned API -------------------------------------------------------------

(defparameter +key+ "cloro-test-key"
  "The key a test's section carries.")

(defparameter +answer+
  (nlk:encode-json-object
   (nlk:json-object
    "success" t
    "result"
    (nlk:json-object
     "organicResults"
     (vector
      (nlk:json-object
       "title" "Rust &amp; Cargo: 1.78.0 <b>is out</b>"
       "link" "https://blog.rust-lang.org/releases/1.78.0"
       "snippet" "<em>Announcing</em> Rust 1.78.0: the release notes&#x27; details &amp; more"
       "position" 1)
      ;; cloro lists a result whose link did not resolve; the original skipped
      ;; it, and so does this cell.
      (nlk:json-object
       "title" "Rust 1.78 without a resolved link"
       "position" 2)
      (nlk:json-object
       "title" "Rust  1.78 changelog"
       "link" "https://example.test/rust-1-78"
       "snippet" "The merged pull requests."
       "position" 3)))))
  "What the stubbed API answers: two resolved results and one that is not.")

(defparameter +empty+
  (nlk:encode-json-object
   (nlk:json-object "success" t
                    "result" (nlk:json-object "organicResults" (vector))))
  "What cloro answers when the page had no organic results.")

(defparameter +error+
  "{\"success\":false,\"error\":{\"code\":\"INVALID_API_KEY\",\"message\":\"The API key is invalid.\"}}"
  "What a refused key answers.")

;;; --- the stub -------------------------------------------------------------------

(defvar *calls* '()
  "Every request the stub took, newest first: (:method :url :headers :content).")

(defvar *answer* nil
  "A function of one call answering (values BODY STATUS), or NIL for +ANSWER+.")

(defun stub-http (method url &key headers content timeout)
  (declare (ignore timeout))
  (let ((call (list :method method :url url :headers headers :content content)))
    (push call *calls*)
    (if *answer* (funcall *answer* call) (values +answer+ 200))))

(defmacro with-wire (&body body)
  "Run BODY with the cell's one HTTP seam stubbed and its calls recorded."
  `(let ((*calls* '())
         (*answer* nil))
     (with-stubbed-fdefinition (nodecode-cloro::http
                                (method url &key headers content timeout)
                                (stub-http method url :headers headers
                                           :content content :timeout timeout))
       ,@body)))

;;; --- starting it, calling it -----------------------------------------------------

(defun cloro-run-start (&rest pairs)
  "START-CELL with a key unless PAIRS name one."
  (apply #'cloro-start
         (append (unless (loop for tail on pairs by #'cddr
                                 thereis (and (stringp (first tail))
                                              (string= "api_key" (first tail))))
                   (list "api_key" +key+))
                 (copy-list pairs))))

(defmacro with-cloro ((&rest pairs) &body body)
  "Run BODY with the cell started on PAIRS, its wire stubbed, stopped on unwind."
  `(with-wire
     (with-cell-stop ((cloro-run-start ,@pairs))
       ,@body)))

(defun cloro-call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

;;; --- reading the request a call made ---------------------------------------------

(defun header (call name)
  "The value CALL sends in header NAME, or NIL."
  (cdr (assoc name (getf call :headers) :test #'string-equal)))

(defun body-value (call key)
  "The member KEY of the JSON body CALL carried, or NIL."
  (let ((payload (ignore-errors (nlk:decode-json (or (getf call :content) "")))))
    (and (hash-table-p payload) (nlk:json-value payload :string key))))
