;;;; support.lisp --- the TinyFish test slice, its stubbed wire and its quiet clock.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What reaches outside this image is NODECODE-TINYFISH::*HTTP*, the
;;;; function every call dials through, NODECODE-TINYFISH::*SLEEP*, the
;;;; wait between two polls, and NODECODE-TINYFISH::ENV-SECRET, the
;;;; environment read behind the key. WITH-TINYFISH stubs all three, so no
;;;; test dials TinyFish, no test waits on a clock, and no test depends on
;;;; what this machine's environment happens to hold.

(in-package #:nodecode.test)

(define-test-slice "tinyfish" "TINYFISH-CELL-" :start nodecode-tinyfish:start-cell
  :defaults (("api_key" "tf-test")
             ("search_url" "https://search.test")
             ("fetch_url" "https://fetch.test")
             ("agent_url" "https://agent.test")
             ("poll_seconds" 0)))

(defvar *tinyfish-requests* '()
  "((METHOD URL HEADERS BODY) ...) the stubbed wire received, newest first.")

(defvar *tinyfish-routes* '()
  "((URL-FRAGMENT . ANSWER) ...): ANSWER takes (URL BODY) and answers
\(values TEXT STATUS); a URL no route names answers an empty object, 200.")

(defun tinyfish-dial (method url &key headers content &allow-other-keys)
  "The stub standing in for NLK:HTTP: record the exchange, answer the first
route whose fragment URL carries."
  (push (list method url headers content) *tinyfish-requests*)
  (loop for (fragment . answer) in *tinyfish-routes*
        when (search fragment url)
          do (return-from tinyfish-dial (funcall answer url content)))
  (values "{}" 200))

(defun tinyfish-hush (seconds)
  "The stub standing in for SLEEP: the poll does not wait in a test."
  (declare (ignore seconds))
  nil)

(defmacro with-tinyfish ((&rest pairs) &body body)
  "BODY with the cell started on the section PAIRS (api_key and the three
hosts from the slice's defaults when PAIRS leave them out), its wire and
clock stubbed, and no environment the test did not name."
  `(with-saved-globals (nle:*tools* nodecode-tinyfish::*http* nodecode-tinyfish::*sleep*
                        nodecode-tinyfish::*last-error*)
     (setf *tinyfish-requests* '()
           *tinyfish-routes* '()
           nodecode-tinyfish::*http* #'tinyfish-dial
           nodecode-tinyfish::*sleep* #'tinyfish-hush)
     (with-stubbed-fdefinition (nodecode-tinyfish::env-secret (name)
                                (declare (ignore name))
                                nil)
       (with-cell-stop ((tinyfish-start ,@pairs))
         ,@body))))

(defun tinyfish-requests ()
  "The exchanges the stub received, oldest first."
  (reverse *tinyfish-requests*))

(defun tinyfish-body (request)
  "REQUEST's content, decoded."
  (let ((content (fourth request)))
    (and content (nlk:decode-json content))))

(defun tinyfish-header (request name)
  "The value REQUEST's headers carry for NAME, case-insensitively."
  (cdr (assoc name (third request) :test #'string-equal)))

(defun tinyfish-answering (fragment answer)
  "Answer every URL carrying FRAGMENT with ANSWER, a (values TEXT STATUS) form."
  (push (cons fragment answer) *tinyfish-routes*))

(defun tinyfish-json (object &optional (status 200))
  "A route answer carrying OBJECT as JSON."
  (lambda (url body)
    (declare (ignore url body))
    (values (nlk:encode-json-object object) status)))
