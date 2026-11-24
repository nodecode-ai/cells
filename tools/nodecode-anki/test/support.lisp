;;;; support.lisp --- the test slice: a stubbed AnkiConnect.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under an
;;;; ANKI-CELL- name prefix; RUN-ANKI-TESTS runs exactly that slice.
;;;;
;;;; The one seam that reaches outside is NODECODE-ANKI::HTTP, the cell's only
;;;; way to the network.  WITH-WIRE replaces it with a canned answer per
;;;; AnkiConnect action and records every call, so a test asserts on the
;;;; request and nothing ever dials.

(in-package #:nodecode.test)

(define-test-slice "anki" "ANKI-CELL-" :start nodecode-anki:start-cell)

;;; --- the canned collection ---------------------------------------------------------

(defparameter +url+ "http://127.0.0.1:8765"
  "The endpoint a test's section names.")

(defvar *calls* '()
  "Every request the stub took, newest first: (:url :headers :action :request).")

(defvar *answers* nil
  "An action, as AnkiConnect names it, to the JSON text it answers.")

(defvar *body* nil
  "When set, the one answer every request gets, whatever it asked.")

(defvar *status* 200
  "The HTTP status the stub answers with.")

(defun answer-as (action result)
  "Have the stub answer ACTION with RESULT, already JSON text."
  (setf (gethash action *answers*) (format nil "{\"result\": ~a, \"error\": null}" result)))

(defun answer-error (action message)
  "Have the stub refuse ACTION, as AnkiConnect refuses a bad action."
  (setf (gethash action *answers*)
        (format nil "{\"result\": null, \"error\": \"~a\"}" message)))

(defun stub-http (url &key content headers timeout)
  (declare (ignore timeout))
  (let* ((request (ignore-errors (nlk:decode-json (or content ""))))
         (action (and request (nlk:json-value request :string "action"))))
    (push (list :url url :headers headers :action action :request request) *calls*)
    (when *body* (return-from stub-http (values *body* *status*)))
    (values (or (gethash action *answers*)
                "{\"result\": null, \"error\": \"unknown action\"}")
            *status*)))

(defmacro with-wire (&body body)
  "Run BODY with the cell's one HTTP seam stubbed and its calls recorded."
  `(let ((*calls* '())
         (*answers* (make-hash-table :test #'equal))
         (*body* nil)
         (*status* 200))
     (with-stubbed-fdefinition (nodecode-anki::http (url &key content headers timeout)
                                (stub-http url :content content :headers headers :timeout timeout))
       ,@body)))

;;; --- starting it, calling it -----------------------------------------------------

(defmacro with-anki ((&rest pairs) &body body)
  "Run BODY with the cell started on PAIRS, its wire stubbed, stopped on unwind."
  `(with-wire
     (with-cell-stop ((anki-start ,@pairs))
       ,@body)))

(defun anki-call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

;;; --- reading the request a call made ---------------------------------------------

(defun call-of (action)
  "The newest request the stub took for ACTION."
  (find action *calls* :key (lambda (call) (getf call :action)) :test #'equal))

(defun calls-of (action)
  "Every request the stub took for ACTION, newest first."
  (remove action *calls* :key (lambda (call) (getf call :action)) :test-not #'equal))

(defun at (call &rest path)
  "CALL's request, walked down PATH (member names and array indices)."
  (let ((value (getf call :request)))
    (dolist (step path value)
      (setf value (cond ((null value) nil)
                        ((stringp step) (nlk:json-value value :any step))
                        ((and (integerp step) (vectorp value)) (aref value step))
                        (t nil))))))

(defun asked (call &rest path)
  "What CALL asked for at PATH, as text: a member or an array element."
  (let ((value (apply #'at call path)))
    (cond ((null value) nil)
          ((eq value :null) nil)
          (t (princ-to-string value)))))

(defun body-text (text)
  "TEXT decoded, for a test that wants to read an answer back."
  (nlk:decode-json text))

(defun member-of (text key)
  "KEY's value in the JSON answer TEXT."
  (nlk:json-value (body-text text) :any key))
