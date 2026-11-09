;;;; support.lisp --- the Index Network test slice and its one stubbed wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What reaches outside this image is NODECODE-INDEX-NETWORK::*HTTP*, the
;;;; function every call dials through, and NODECODE-INDEX-NETWORK::SECRET,
;;;; the environment read behind the key: WITH-INDEX stubs both, so no test
;;;; ever dials Index, and no test depends on what this machine's environment
;;;; happens to hold. The lifecycle test starts the whole cell.

(in-package #:nodecode.test)

(define-test-slice "index-network" "INDEX-NETWORK-CELL-" :start nodecode-index-network:start-cell
  :defaults (("api_url" "https://index.test") ("api_key" "test-key")))

(defvar *index-requests* '()
  "((METHOD URL HEADERS BODY) ...) the stubbed wire received, newest first.")

(defvar *index-routes* '()
  "((METHOD-FRAGMENT . ANSWER) ...): ANSWER takes (URL BODY) and answers
\(values TEXT STATUS); a route no entry names answers \"{}\" 200.")

(defvar *index-env* '()
  "The environment the stubbed SECRET reads, as an alist.")

(defun index-dial (method url &key headers content &allow-other-keys)
  "The stub standing in for NLK:HTTP: record the exchange, answer the first
route whose URL carries its fragment, 200 {} otherwise."
  (push (list method url headers content) *index-requests*)
  (loop for (fragment . answer) in *index-routes*
        when (search fragment url)
          do (return-from index-dial (funcall answer url content)))
  (values "{}" 200))

(defmacro with-index ((&rest pairs) &body body)
  "BODY with the cell started on the section PAIRS (api_url and api_key from
the slice's defaults when PAIRS leave them out), its wire stubbed, and no
environment the test did not name."
  `(with-saved-globals (nle:*tools* nodecode-index-network::*http*
                        nodecode-index-network::*last-error*)
     (setf *index-requests* '()
           *index-routes* '()
           *index-env* '()
           nodecode-index-network::*http* #'index-dial)
     (with-stubbed-fdefinition (nodecode-index-network::secret (name)
                                (cdr (assoc name *index-env* :test #'equal)))
       (with-cell-stop ((index-network-start ,@pairs))
         ,@body))))

(defun index-requests ()
  "The exchanges the stub received, oldest first."
  (reverse *index-requests*))

(defun index-body (request)
  "REQUEST's content, decoded."
  (nlk:decode-json (or (fourth request) "{}")))

(defun index-header (request name)
  "The value REQUEST's headers carry for NAME, case-insensitively."
  (cdr (assoc name (third request) :test #'string-equal)))

(defun index-answering (fragment answer)
  "Answer every URL carrying FRAGMENT with ANSWER, a (values TEXT STATUS) form."
  (push (cons fragment answer) *index-routes*))

(defun answering-json (value &key (status 200))
  "An ANSWER that sends VALUE as JSON with STATUS."
  (lambda (url body)
    (declare (ignore url body))
    (values (nlk:encode-json-object value) status)))

(defun answering-error (text &key (status 400))
  "An ANSWER that refuses with TEXT and STATUS."
  (lambda (url body)
    (declare (ignore url body))
    (values (nlk:encode-json-object (nlk:json-object "message" text)) status)))

(defun intent-row (id description &key (status "ACTIVE"))
  "One decoded intents/list row, as the API answers it."
  (nlk:json-object "id" id "description" description "status" status))

(defun opportunity-row (id name &key (status "pending"))
  "One decoded opportunities row, as the API answers it."
  (nlk:json-object "id" id "counterpartName" name "status" status))
