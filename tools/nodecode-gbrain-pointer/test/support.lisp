;;;; support.lisp --- the GBrain pointer test slice and its one stubbed wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What reaches outside this image is NODECODE-GBRAIN-POINTER::*HTTP*, the
;;;; function every dial goes through, and NODECODE-GBRAIN-POINTER::SECRET, the
;;;; environment read behind every endpoint and credential: WITH-GBRAIN stubs
;;;; both, so no test dials a brain and none depends on what this machine's
;;;; environment happens to hold. The lifecycle test starts the whole cell.

(in-package #:nodecode.test)

(define-test-slice "gbrain-pointer" "GBRAIN-POINTER-CELL-" :start nodecode-gbrain-pointer:start-cell
  :defaults (("mcp_url" "http://127.0.0.1:3131/mcp") ("access_token" "test-token")))

(defvar *gbrain-requests* '()
  "((METHOD URL HEADERS BODY) ...) the stubbed wire received, newest first.")

(defvar *gbrain-routes* '()
  "((URL-FRAGMENT . ANSWER) ...): ANSWER takes (URL BODY) and answers
\(values TEXT STATUS); a URL no route names answers an empty object, 200.")

(defun gbrain-dial (method url &key headers content &allow-other-keys)
  "The stub standing in for NLK:HTTP: record the exchange, answer the first
route whose fragment URL carries."
  (push (list method url headers content) *gbrain-requests*)
  (loop for (fragment . answer) in *gbrain-routes*
        when (search fragment url)
          do (return-from gbrain-dial (funcall answer url content)))
  (values "{}" 200))

(defmacro with-no-gbrain-env (&body body)
  "BODY with every GBrain environment read answering nothing, so no test depends
on the machine's environment for any of it."
  `(with-stubbed-fdefinition (nodecode-gbrain-pointer::secret (name) nil)
     ,@body))

(defmacro with-gbrain ((&rest pairs) &body body)
  "BODY with the cell started on the section PAIRS (mcp_url and access_token
from the slice's defaults when PAIRS leave them out), its wire stubbed, and no
environment the test did not name."
  `(with-saved-globals (nle:*tools*
                        nodecode-gbrain-pointer::*http*
                        nodecode-gbrain-pointer::*pointers*
                        nodecode-gbrain-pointer::*pending*
                        nodecode-gbrain-pointer::*minted*
                        nodecode-gbrain-pointer::*injected*
                        nodecode-gbrain-pointer::*last-error*)
     (setf *gbrain-requests* '()
           *gbrain-routes* '()
           nodecode-gbrain-pointer::*http* #'gbrain-dial)
     (with-no-gbrain-env
       (with-cell-stop ((gbrain-pointer-start ,@pairs))
         ,@body))))

(defun gbrain-requests ()
  "The exchanges the stub received, oldest first."
  (reverse *gbrain-requests*))

(defun gbrain-body (request)
  "REQUEST's content, decoded."
  (nlk:decode-json (fourth request)))

(defun gbrain-header (request name)
  "The value REQUEST's headers carry for NAME, case-insensitively."
  (cdr (assoc name (third request) :test #'string-equal)))

(defun gbrain-answering (fragment answer)
  "Answer every URL carrying FRAGMENT with ANSWER, a (values TEXT STATUS) form."
  (push (cons fragment answer) *gbrain-routes*))

(defun answer-with (text &optional (status 200))
  "A route answer carrying TEXT and STATUS."
  (lambda (url body)
    (declare (ignore url body))
    (values text status)))

(defun row-object (row)
  "One GBrain lean row as the brain sends it: ROW is a plist of :SLUG, :TITLE,
:SOURCE, :CHUNK and, when :INJECTION is non-NIL, the suspected-injection flag."
  (let ((object (nlk:json-object
                 "slug" (or (getf row :slug) "")
                 "title" (or (getf row :title) "")
                 "source_id" (or (getf row :source) "")
                 "chunk_text" (or (getf row :chunk) "x"))))
    (when (getf row :injection)
      (setf (gethash "injection_suspected" object) t))
    object))

(defun search-answer-body (rows)
  "The tools/call envelope a search answers for ROWS, as the JSON text GBrain
sends inside content[0].text."
  (nlk:encode-json-object
   (nlk:json-object
    "result"
    (nlk:json-object
     "content"
     (vector (nlk:json-object
              "type" "text"
              "text" (nlk:encode-json-object (coerce (mapcar #'row-object rows) 'vector))))))))

(defun sse-body (raw)
  "RAW as the Streamable HTTP body a brain may answer: one data: line."
  (format nil "event: message~%data: ~a~%~%" raw))
