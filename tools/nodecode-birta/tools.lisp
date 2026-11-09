;;;; tools.lisp --- the two tools, their arguments, and what each answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin exposed Birta's whole tool surface through its hosted MCP
;;;; server: projects, versions, publishing, comments, access, forms, leads,
;;;; domains and stats, each a tool with its own schema. This cell keeps that
;;;; surface whole -- BIRTA_TOOLS lists what the connected server offers, with
;;;; the arguments each one takes, and BIRTA_CALL calls one by name -- rather
;;;; than hard-coding a fixed subset whose names could drift from the server's.
;;;;
;;;; Every tool answers text: the model reads it, and the addresses and
;;;; version numbers stay where it can cite them.

(in-package #:nodecode-birta)

;;; --- what the model is shown ------------------------------------------------

(nlk:define-contract list-request
  (:doc "List the Birta tools the connected server offers, with the arguments each one takes.
Call it once before the first Birta call: the server's own descriptions are the authority on
what a tool needs.")
  ("match" :string
   :doc "only the tools whose name or description carries this text"))

(nlk:define-contract call-request
  (:doc "Call one Birta tool by name, with the arguments its own schema asks for. Every call acts
on the Birta projects you can open, at birta.dev. Use birta_tools first when you do not know the
tool's arguments.")
  ("tool" :text :required t
   :doc "the tool's name, exactly as birta_tools lists it: list_projects, publish_version, ...")
  ("arguments" :object
   :doc "the arguments object the tool's schema asks for; leave it out for a tool that takes none"))

;;; --- reading what comes back ------------------------------------------------

(defun clip (text limit)
  "TEXT cut to LIMIT characters, saying how much was left."
  (cond ((null text) "")
        ((<= (length text) limit) text)
        (t (format nil "~a~%[... ~d more characters]"
                   (subseq text 0 limit) (- (length text) limit)))))

(defun tool-name (tool)
  "TOOL's name, as the server spells it."
  (or (nlk:json-value tool :text "name") "?"))

(defun tool-description (tool)
  "TOOL's description, its first line, at most 300 characters."
  (let ((text (nlk:json-value tool :text "description")))
    (when text
      (let* ((line (first (uiop:split-string text :separator '(#\Newline))))
             (trimmed (nlk:trimmed line)))
        (if (> (length trimmed) 300) (subseq trimmed 0 300) trimmed)))))

(defun tool-required (tool)
  "The argument names TOOL's schema marks required, null when it marks none."
  (let* ((schema (nlk:json-value tool :object "inputSchema"))
         (required (nlk:json-value schema :array "required")))
    (and required
         (remove-if-not #'stringp (coerce required 'list)))))

(defun tool-covers-p (tool match)
  "True when TOOL's name or description carries MATCH, case-insensitively."
  (and match
       (plusp (length match))
       (let ((haystack (string-downcase (format nil "~a ~a"
                                                (tool-name tool)
                                                (or (nlk:json-value tool :text "description") "")))))
         (not (null (search (string-downcase match) haystack))))))

(defun format-tool-list (tools match)
  "TOOLS -- the server's own list -- as the text the model reads."
  (let* ((kept (if (and match (plusp (length match)))
                   (remove-if-not (lambda (tool) (tool-covers-p tool match)) tools)
                   tools))
         (text (with-output-to-string (out)
                 (format out "birta: ~d tool~:p at ~a~@[ matching ~s~]~%"
                         (length kept) (endpoint) (and match (plusp (length match)) match))
                 (if (null kept)
                     (format out "none; call birta_tools with no match to see them all~%")
                     (dolist (tool kept)
                       (format out "~a - ~a~@[ (required: ~{~a~^, ~})~]~%"
                               (tool-name tool)
                               (or (tool-description tool) "no description")
                               (tool-required tool)))))))
    (clip text (setting :max-chars))))

(defun content-text (result)
  "RESULT's text content joined, or NIL when it carries none."
  (let ((pieces (loop for item across (or (nlk:json-value result :array "content") #())
                      for type = (nlk:json-value item :text "type")
                      for text = (nlk:json-value item :text "text")
                      when (and text (equal type "text")) collect text)))
    (and pieces (format nil "~{~a~^~%~}" pieces))))

(defun format-answer (result)
  "RESULT -- one tools/call answer -- as the text the model reads. A call the
server marks failed is the model's repair, so it reads ERROR: first."
  (let* ((text (content-text result))
         (body (or text
                   (let ((structured (nlk:json-value result :object "structuredContent")))
                     (and structured (nlk:encode-json-object structured)))
                   (and result (nlk:encode-json-object result))
                   "the call answered nothing"))
         (failed (nlk:json-value result :boolean "isError")))
    (clip (if failed (format nil "ERROR: ~a" body) body) (setting :max-chars))))

;;; --- the two tools ----------------------------------------------------------

(defun tool-list (call)
  "BIRTA_TOOLS: what the connected server offers."
  (format-tool-list (list-tools) (list-request-match call)))

(defun tool-call (call)
  "BIRTA_CALL: one named tool, with the arguments the model gave it."
  (format-answer (call-tool (call-request-tool call)
                             (call-request-arguments call))))
