;;;; support.lisp --- the WeKnora test slice: a stubbed wire, a canned deployment.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the same nodecode.test registry every cell's do
;;;; (the core DEFTEST, with its hermetic machine-state posture) under a
;;;; WEKNORA-CELL- name prefix; RUN-WEKNORA-TESTS runs exactly that slice.
;;;;
;;;; The one seam that reaches outside is NLK:HTTP, the only way this cell
;;;; touches the network. WITH-WIRE replaces it with a small WeKnora - two
;;;; knowledge bases, one document, one streamed answer - and records every
;;;; call, so a test asserts on the exchange and nothing ever dials.

(in-package #:nodecode.test)

(define-test-slice "weknora" "WEKNORA-CELL-" :start nodecode-weknora:start-cell)

;;; --- the canned deployment ------------------------------------------------------

(defparameter +base-url+ "https://kb.test"
  "The deployment a test's section names; the client appends /api/v1.")

(defparameter +knowledge-bases+
  "{\"success\":true,\"data\":[{\"id\":\"kb-1\",\"name\":\"Handboek\",\"description\":\"how we work\"},{\"id\":\"kb-2\",\"name\":\"Inkoop\",\"description\":\"\"}]}"
  "GET /knowledge-bases.")

(defparameter +passages+
  "{\"success\":true,\"data\":[{\"id\":\"ch-1\",\"content\":\"A receipt is filed within 30 days.\",\"knowledge_id\":\"doc-1\",\"chunk_index\":2,\"knowledge_title\":\"Receipts\",\"score\":0.8125},{\"id\":\"ch-2\",\"content\":\"The handbooks live in the Handboek folder.\",\"knowledge_id\":\"doc-1\",\"chunk_index\":3,\"knowledge_title\":\"Receipts\",\"score\":0.5}]}"
  "POST /knowledge-search.")

(defparameter +named-documents+
  "{\"success\":true,\"data\":[{\"id\":\"doc-9\",\"title\":\"Receipt procedure\",\"file_name\":\"receipts.pdf\",\"knowledge_base_id\":\"kb-1\",\"knowledge_base_name\":\"Handboek\"}]}"
  "GET /knowledge/search, the by-name enrichment. doc-9 is not a passage hit,
so it survives the scope filter; a document already among the hits is dropped.")

(defparameter +chunks+
  "{\"success\":true,\"data\":[{\"id\":\"ch-2\",\"content\":\"The second paragraph.\",\"chunk_index\":2},{\"id\":\"ch-1\",\"content\":\"The first paragraph.\",\"chunk_index\":1}],\"total\":5,\"page\":1,\"page_size\":2}"
  "GET /chunks/doc-1, out of order on the wire on purpose: the verb sorts.")

(defparameter +document+
  "{\"success\":true,\"data\":{\"id\":\"doc-1\",\"title\":\"Receipts\",\"file_name\":\"receipts.pdf\",\"description\":\"How expense claims work.\"}}"
  "GET /knowledge/doc-1.")

(defparameter +session+
  "{\"success\":true,\"data\":{\"id\":\"sess-1\"}}"
  "POST /sessions.")

(defparameter +answer-stream+
  (format nil "data: ~a~%~%data: ~a~%~%data: ~a~%~%~%"
          "{\"response_type\":\"answer\",\"content\":\"A receipt is filed within 30 days.\",\"session_id\":\"sess-1\"}"
          "{\"response_type\":\"references\",\"knowledge_references\":[{\"knowledge_id\":\"doc-1\",\"knowledge_title\":\"Receipts\",\"chunk_index\":2,\"content\":\"A receipt is filed within 30 days.\"}]}"
          "{\"response_type\":\"complete\"}")
  "The answer WeKnora streams for a question, as its chat route sends it.")

(defparameter +unfinished-stream+
  (format nil "data: ~a~%~%"
          "{\"response_type\":\"answer\",\"content\":\"A receipt is\"}")
  "The same stream cut before its complete event.")

;;; --- the wire --------------------------------------------------------------------

(defvar *calls* '()
  "Every (METHOD PATH BODY) the stubbed wire received, newest first.")

(defvar *overrides* '()
  "((PATH . (VALUES BODY STATUS)) ...) a test sets to script one route; the
newest entry for a path wins and the rest of the deployment answers as canned.")

(defun path-of (url)
  "URL's path under the deployment root, query string cut away."
  (let* ((at (or (search "/api/v1" url) -7))
         (rest (subseq url (min (length url) (+ at 7)))))
    (subseq rest 0 (or (position #\? rest) (length rest)))))

(defun script (path body &optional (status 200))
  "Answer PATH with BODY at STATUS for the rest of the test."
  (push (cons path (lambda () (values body status))) *overrides*))

(defun stub-wire (method url &rest arguments)
  "The stubbed NLK:HTTP: a small WeKnora, and a record of every call."
  (let* ((path (path-of url))
         (content (getf arguments :content))
         (stream (getf arguments :stream)))
    (push (list method path content url) *calls*)
    (let ((override (assoc path *overrides* :test #'equal)))
      (when override (return-from stub-wire (funcall (cdr override)))))
    (cond
      ((equal path "/knowledge-bases") (values +knowledge-bases+ 200))
      ((equal path "/knowledge-search") (values +passages+ 200))
      ((equal path "/knowledge/search") (values +named-documents+ 200))
      ((equal path "/knowledge/doc-1") (values +document+ 200))
      ((equal path "/chunks/doc-1") (values +chunks+ 200))
      ((equal path "/sessions") (values +session+ 200))
      ((and stream (search "-chat/sess-1" path))
       (values (make-string-input-stream +answer-stream+) 200))
      (stream (values (make-string-input-stream "") 200))
      (t (values "{\"success\":false,\"error\":{\"message\":\"no such route\"}}" 404)))))

(defmacro with-wire (&body body)
  "Run BODY with NLK:HTTP stubbed and the client's per-start cache cleared."
  `(with-saved-globals (*calls* *overrides* nodecode-weknora::*resource-blocked*
                        nodecode-weknora::*visible-scope*)
     (setf *calls* '()
           *overrides* '()
           nodecode-weknora::*resource-blocked* nil
           nodecode-weknora::*visible-scope* :unresolved)
     (with-stubbed-fdefinition (nlk:http (method url &rest arguments)
                                 (apply #'stub-wire method url arguments))
       ,@body)))

(defmacro with-weknora ((&rest pairs) &body body)
  "Run BODY with the cell started under PAIRS as the `weknora' section, its
wire stubbed, and stopped on unwind."
  `(with-wire
     (with-cell-stop ((weknora-start ,@pairs))
       ,@body)))

;;; --- asking the cell what a model would -------------------------------------------

(defun call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

(defun calls-to (path)
  "The calls the stub took on PATH, newest first."
  (remove path *calls* :key #'second :test-not #'equal))
