;;;; support.lisp --- the test slice: a stubbed DevTune API.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; DEVTUNE-CELL- name prefix; RUN-DEVTUNE-TESTS runs exactly that slice.
;;;;
;;;; The one seam that reaches outside is NODECODE-DEVTUNE::HTTP, the cell's
;;;; only way to the network. WITH-WIRE replaces it with a canned answer and
;;;; records every call, so a test asserts on the request and nothing ever
;;;; dials.

(in-package #:nodecode.test)

(define-test-slice "devtune" "DEVTUNE-CELL-" :start nodecode-devtune:start-cell)

;;; --- the canned API --------------------------------------------------------------

(defparameter +key+ "dtk_test_key"
  "The key a test's section carries.")

(defparameter +connection+
  "{\"projectId\":\"p_1234\",\"projectName\":\"example\",\"paused\":false,\"credential\":\"project key\"}"
  "What GET /connection answers.")

(defparameter +connection-paused+
  "{\"projectId\":\"p_1234\",\"projectName\":\"example\",\"paused\":true}"
  "What GET /connection answers for a paused project.")

(defparameter +summary+
  "{\"windowDays\":30,\"brandAwareness\":0.23,\"citationPresence\":0.41,\"shareOfVoice\":0.12,\"successes\":41,\"n\":100}"
  "What a measurement read answers: percentages from 0 to 100 and a rate from 0 to 1.")

(defparameter +ledger+
  "{\"outcomes\":[{\"actionId\":\"act_1\",\"outcome\":\"improved\",\"directional\":false}]}"
  "What GET /projects/{id}/outcomes/ledger answers.")

(defparameter +refused+
  "{\"error\":{\"code\":\"invalid_token\",\"message\":\"The API key is invalid.\"},\"requestId\":\"r_1\"}"
  "What a refused key answers.")

(defparameter +setup-required+
  "{\"error\":{\"code\":\"agent_picks_setup_required\",\"details\":{\"setups\":[{\"id\":\"s_1\",\"category\":\"auth\"}]}}}"
  "What an Agent Picks read answers when a project has several setups.")

;;; --- the stub -------------------------------------------------------------------

(defvar *calls* '()
  "Every request the stub took, newest first: (:url :headers).")

(defvar *answer* nil
  "A function of one url answering (values BODY STATUS), or NIL for the canned
answers below, which are chosen by path.")

(defun stub-http (url &key headers timeout)
  (declare (ignore timeout))
  (push (list :url url :headers headers) *calls*)
  (cond (*answer* (funcall *answer* url))
        ((cl:search "/connection" url) (values +connection+ 200))
        ((cl:search "/outcomes/ledger" url) (values +ledger+ 200))
        (t (values +summary+ 200))))

(defmacro with-wire (&body body)
  "Run BODY with the cell's one HTTP seam stubbed and its calls recorded."
  `(let ((*calls* '())
         (*answer* nil))
     (with-stubbed-fdefinition (nodecode-devtune::http
                                (url &key headers timeout)
                                (stub-http url :headers headers :timeout timeout))
       ,@body)))

;;; --- starting it, calling it -----------------------------------------------------

(defun devtune-run-start (&rest pairs)
  "START-CELL with a key unless PAIRS name one."
  (apply #'devtune-start
         (append (unless (member "api_key" pairs :test #'string=)
                   (list "api_key" +key+))
                 (copy-list pairs))))

(defmacro with-devtune ((&rest pairs) &body body)
  "Run BODY with the cell started on PAIRS, its wire stubbed, stopped on unwind."
  `(with-wire
     (with-cell-stop ((devtune-run-start ,@pairs))
       ,@body)))

(defun devtune-call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

;;; --- reading the request a call made ---------------------------------------------

(defun devtune-call-to (fragment)
  "The first request the stub took whose url carries FRAGMENT, or NIL."
  (find-if (lambda (call) (cl:search fragment (getf call :url))) *calls*))

(defun devtune-params-of (url)
  "URL's query string as an alist of decoded (NAME . VALUE) pairs."
  (loop with query = (and url (quri:uri-query (quri:uri url)))
        for pair in (and query (uiop:split-string query :separator '(#\&)))
        for equals = (position #\= pair)
        for name = (quri:url-decode (subseq pair 0 equals) :encoding :utf-8)
        for value = (quri:url-decode (subseq pair (1+ equals)) :encoding :utf-8)
        collect (cons name value)))

(defun devtune-param (call name)
  "The value CALL's url carries for NAME, or NIL when it does not carry it."
  (cdr (assoc name (devtune-params-of (getf call :url)) :test #'string=)))

(defun devtune-header (call name)
  "The value CALL sends in header NAME, or NIL."
  (cdr (assoc name (getf call :headers) :test #'string-equal)))
