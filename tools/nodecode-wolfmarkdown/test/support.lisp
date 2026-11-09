;;;; support.lisp --- wolfmarkdown test runner and its two helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a WOLFMARKDOWN
;;;; prefix; RUN-WOLFMARKDOWN-TESTS runs exactly that slice. Nothing here
;;;; dials anything: the checks are functions over strings, and the two
;;;; stubs are a string (the document, or the untouched source) and a file
;;;; under the scratch home.

(in-package #:nodecode.test)

(define-test-slice "wolfmarkdown" "WOLFMARKDOWN" :start nodecode-wolfmarkdown:start-cell)

(defun wolfmarkdown-doc (&rest lines)
  "LINES joined as a document, one newline after each."
  (format nil "~{~a~%~}" lines))

(defun wolfmarkdown-answer (id name arguments)
  "The text a tool call NAME with ARGUMENTS answers: the wire, as a
provider's call arrives on it."
  (nle::message-content (execute-wire-call id name arguments)))

(defun wolfmarkdown-file (name text)
  "A file under the scratch home, named NAME and holding TEXT => its path."
  (let ((path (namestring (nlk:home (format nil "wolfmarkdown-test/~a" name)))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :external-format :utf-8)
      (write-string text out))
    path))

(defun wolfmarkdown-contents (path)
  "What the file at PATH holds."
  (uiop:read-file-string path))

