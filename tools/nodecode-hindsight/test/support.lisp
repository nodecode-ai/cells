;;;; support.lisp --- the Hindsight test slice and its one stubbed wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What reaches outside this image is NODECODE-HINDSIGHT::*HTTP*, the
;;;; function every call dials through, and NODECODE-HINDSIGHT::SECRET, the
;;;; environment read behind the key: WITH-HINDSIGHT stubs both, so no test
;;;; ever dials Hindsight, and no test depends on what the machine's
;;;; environment happens to hold. The lifecycle test starts the whole cell.

(in-package #:nodecode.test)

(define-test-slice "hindsight" "HINDSIGHT-CELL-" :start nodecode-hindsight:start-cell
  :defaults (("api_url" "https://hindsight.test") ("api_key" "test-key")))

(defvar *hindsight-requests* '()
  "((METHOD URL HEADERS BODY) ...) the stubbed wire received, newest first.")

(defvar *hindsight-routes* '()
  "((URL-FRAGMENT . ANSWER) ...): ANSWER takes (URL BODY) and answers
\(values TEXT STATUS); a URL no route names answers \"{}\" 200.")

(defvar *hindsight-env* '()
  "The environment the stubbed SECRET reads, as an alist.")

(defun hindsight-dial (method url &key headers content &allow-other-keys)
  "The stub standing in for NLK:HTTP: record the exchange, answer the first
route whose fragment URL carries."
  (push (list method url headers content) *hindsight-requests*)
  (loop for (fragment . answer) in *hindsight-routes*
        when (search fragment url)
          do (return-from hindsight-dial (funcall answer url content)))
  (values "{}" 200))

(defmacro with-hindsight ((&rest pairs) &body body)
  "BODY with the cell started on the section PAIRS (api_url and api_key from
the slice's defaults when PAIRS leave them out), its wire stubbed, and no
environment the test did not name."
  `(with-saved-globals (nle:*tools* nodecode-hindsight::*http* nodecode-hindsight::*seen*
                        nodecode-hindsight::*retained* nodecode-hindsight::*recalled*
                        nodecode-hindsight::*last-error*)
     (setf *hindsight-requests* '()
           *hindsight-routes* '()
           *hindsight-env* '()
           nodecode-hindsight::*http* #'hindsight-dial)
     (with-stubbed-fdefinition (nodecode-hindsight::secret (name)
                                (cdr (assoc name *hindsight-env* :test #'equal)))
       (with-cell-stop ((hindsight-start ,@pairs))
         ,@body))))

(defun hindsight-requests ()
  "The exchanges the stub received, oldest first."
  (reverse *hindsight-requests*))

(defun hindsight-body (request)
  "REQUEST's content, decoded."
  (nlk:decode-json (fourth request)))

(defun hindsight-header (request name)
  "The value REQUEST's headers carry for NAME, case-insensitively."
  (cdr (assoc name (third request) :test #'string-equal)))

(defun hindsight-asking (fragment answer)
  "Answer every URL carrying FRAGMENT with ANSWER, a (values TEXT STATUS) form."
  (push (cons fragment answer) *hindsight-routes*))

(defun turn-id-of (turn)
  "The id of TURN, whether WITH-DURABLE-TURN bound its id or the turn itself."
  (if (stringp turn) turn (nlk:durable-turn-turn-id turn)))

(defun prompt-text (turn)
  "The last <system-reminder> text recorded on TURN, or NIL."
  (nlk:json-value (nlk:events :turn-id (turn-id-of turn) :kind nlk::+kind-turn-context-prompt+
                               :order :newest :as :payload)
                  :string "text"))
