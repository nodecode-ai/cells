;;;; support.lisp --- the Membase test slice and its one stubbed wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What reaches outside this image is NODECODE-MEMBASE::*HTTP*, the function
;;;; every call dials through, and NODECODE-MEMBASE::SECRET, the environment
;;;; read behind the key: WITH-MEMBASE stubs both, so no test ever dials
;;;; Membase, and no test depends on what the machine's environment holds.
;;;; The lifecycle test starts the whole cell.

(in-package #:nodecode.test)

(define-test-slice "membase" "MEMBASE-CELL-" :start nodecode-membase:start-cell
  :defaults (("base_url" "https://membase.test") ("api_key" "test-key")))

(defvar *membase-requests* '()
  "((METHOD URL HEADERS BODY) ...) the stubbed wire received, newest first.")

(defvar *membase-routes* '()
  "((URL-FRAGMENT . ANSWER) ...): ANSWER takes (URL BODY) and answers
\(values TEXT STATUS); a URL no route names answers \"{}\" 200.")

(defvar *membase-env* '()
  "The environment the stubbed SECRET reads, as an alist.")

(defun membase-dial (method url &key headers content &allow-other-keys)
  "The stub standing in for NLK:HTTP: record the exchange, answer the first
route whose fragment URL carries."
  (push (list method url headers content) *membase-requests*)
  (loop for (fragment . answer) in *membase-routes*
        when (search fragment url)
          do (return-from membase-dial (funcall answer url content)))
  (values "{}" 200))

(defmacro with-membase ((&rest pairs) &body body)
  "BODY with the cell started on the section PAIRS (base_url and api_key from
the slice's defaults when PAIRS leave them out), its wire stubbed, and no
environment the test did not name."
  `(with-saved-globals (nle:*tools* nodecode-membase::*http* nodecode-membase::*seen*
                        nodecode-membase::*captured* nodecode-membase::*recalled*
                        nodecode-membase::*recall-refused* nodecode-membase::*last-error*)
     (setf *membase-requests* '()
           *membase-routes* '()
           *membase-env* '()
           nodecode-membase::*http* #'membase-dial)
     (with-stubbed-fdefinition (nodecode-membase::secret (name)
                                (cdr (assoc name *membase-env* :test #'equal)))
       (with-cell-stop ((membase-start ,@pairs))
         ,@body))))

(defun membase-requests ()
  "The exchanges the stub received, oldest first."
  (reverse *membase-requests*))

(defun membase-body (request)
  "REQUEST's content, decoded."
  (nlk:decode-json (fourth request)))

(defun membase-header (request name)
  "The value REQUEST's headers carry for NAME, case-insensitively."
  (cdr (assoc name (third request) :test #'string-equal)))

(defun membase-asking (fragment answer)
  "Answer every URL carrying FRAGMENT with ANSWER, a (values TEXT STATUS) form."
  (push (cons fragment answer) *membase-routes*))

(defun membase-json (object)
  "OBJECT as one JSON body string."
  (nlk:encode-json-object object))

(defun turn-id-of (turn)
  "The id of TURN, whether WITH-DURABLE-TURN bound its id or the turn itself."
  (if (stringp turn) turn (nlk:durable-turn-turn-id turn)))

(defun prompt-text (turn)
  "The last <system-reminder> text recorded on TURN, or NIL."
  (nlk:json-value (nlk:events :turn-id (turn-id-of turn) :kind nlk::+kind-turn-context-prompt+
                               :order :newest :as :payload)
                  :string "text"))
