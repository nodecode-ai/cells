;;;; support.lisp --- the Fluxer test slice and its one stubbed wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What reaches outside this image is NODECODE-FLUXER::*HTTP*, the function
;;;; every call dials through: WITH-FLUXER stubs it, so no test ever dials
;;;; Fluxer and no test depends on the network. The lifecycle tests start the
;;;; whole cell.

(in-package #:nodecode.test)

(define-test-slice "fluxer" "FLUXER-CELL-" :start nodecode-fluxer:start-cell
  :defaults (("bot_token" "app.secret") ("base_url" "https://fluxer.test/v1")))

(defvar *fluxer-requests* '()
  "((METHOD URL HEADERS BODY) ...) the stubbed wire received, newest first.")

(defvar *fluxer-routes* '()
  "((URL-FRAGMENT . ANSWER) ...): ANSWER takes (METHOD URL BODY) and answers
\(values TEXT STATUS); a URL no route names answers \"{}\" 200.")

(defun fluxer-dial (method url &key headers content &allow-other-keys)
  "The stub standing in for NLK:HTTP: record the exchange, answer the first
route whose fragment URL carries."
  (push (list method url headers content) *fluxer-requests*)
  (loop for (fragment . answer) in *fluxer-routes*
        when (search fragment url)
          do (return-from fluxer-dial (funcall answer method url content)))
  (values "{}" 200))

(defmacro with-fluxer ((&rest pairs) &body body)
  "BODY with the cell started on the section PAIRS (bot_token and base_url
from the slice's defaults when PAIRS leave them out) and its wire stubbed."
  `(with-saved-globals (nle:*tools* nodecode-fluxer::*http* nodecode-fluxer::*last-error*
                        nodecode-fluxer::*fluxer*)
     (setf *fluxer-requests* '()
           *fluxer-routes* '()
           nodecode-fluxer::*http* #'fluxer-dial)
     (with-cell-stop ((fluxer-start ,@pairs))
       ,@body)))

(defun fluxer-requests ()
  "The exchanges the stub received, oldest first."
  (reverse *fluxer-requests*))

(defun fluxer-latest ()
  "The last exchange the stub received."
  (first *fluxer-requests*))

(defun fluxer-body (request)
  "REQUEST's content, decoded."
  (let ((content (fourth request)))
    (and content (nlk:decode-json content))))

(defun fluxer-header (request name)
  "The value REQUEST's headers carry for NAME, case-insensitively."
  (cdr (assoc name (third request) :test #'string-equal)))

(defun fluxer-answering (fragment answer)
  "Answer every URL carrying FRAGMENT with ANSWER, a (values TEXT STATUS) form."
  (push (cons fragment answer) *fluxer-routes*))

(defparameter *fluxer-tool-decoders*
  '(("fluxer_send_message" . nodecode-fluxer::decode-send-call)
    ("fluxer_read_messages" . nodecode-fluxer::decode-read-call)
    ("fluxer_list_channels" . nodecode-fluxer::decode-channels-call)
    ("fluxer_react" . nodecode-fluxer::decode-react-call)
    ("fluxer_pin" . nodecode-fluxer::decode-pin-call))
  "The contract decoder behind each tool: what dispatch runs before the body.")

(defun fluxer-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry)
             (funcall (or (cdr (assoc name *fluxer-tool-decoders* :test #'equal))
                          (error "no decoder for ~a" name))
                      arguments))))

(defun fluxer-answer (text &optional (status 200))
  "Answer every URL with TEXT and STATUS: one route that matches anything."
  (fluxer-answering "" (lambda (method url body)
                          (declare (ignore method url body))
                          (values text status))))
