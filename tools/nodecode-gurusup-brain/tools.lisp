;;;; tools.lisp --- the three tools the original manifest's server gives the model.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original ships no tool list: the manifest points at Brain's MCP
;;;; server and the skill says to call its `init' tool first and follow the
;;;; contract it returns. So the cell asks the server rather than assuming:
;;;; gurusup_brain_tools lists what this account may call, gurusup_brain_init
;;;; returns the contract, and gurusup_brain_call runs any of them by name.
;;;;
;;;; A call's refusal the server reports is the answer text, prefixed ERROR:,
;;;; which is the shape the model repairs from.

(in-package #:nodecode-gurusup-brain)

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the ERROR: line the model reads."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: gurusup-brain: ~a" condition)))))

(defun argument-text (args name)
  "ARGS' NAME as a trimmed non-empty string, or a refusal naming it."
  (let ((value (gethash name args)))
    (if (and (stringp value) (plusp (length (string-trim '(#\Space) value))))
        (string-trim '(#\Space) value)
        (fail "~a is required" (substitute #\_ #\- name)))))

(defun arguments-object (args)
  "ARGS' arguments member as the JSON object a tools/call carries: an object
as given, a JSON string decoded, and nothing at all for an absent one."
  (let ((value (gethash "arguments" args)))
    (cond ((hash-table-p value) value)
          ((and (stringp value) (plusp (length (string-trim '(#\Space) value))))
           (let ((decoded (handler-case (nlk:decode-json value) (error () nil))))
             (if (hash-table-p decoded)
                 decoded
                 (fail "arguments must be a JSON object"))))
          ((null value) (nlk:json-object))
          (t (fail "arguments must be a JSON object")))))

;;; --- the tools ----------------------------------------------------------------

(defun tool-init (args)
  (declare (ignore args))
  (answering (brain-init)))

(defun tool-tools (args)
  (declare (ignore args))
  (answering (tool-lines (session-tools (open-session)))))

(defun tool-call (args)
  (answering (let ((session (open-session)))
               (session-call session (string-downcase (argument-text args "tool"))
                             (arguments-object args)))))

;;; --- the schemas the model reads -----------------------------------------------

(defparameter *init-schema*
  (list :description (concatenate 'string
                                  "GuruSup Brain's own contract for this account, returned by its init "
                                  "tool. Call it before answering anything about the company: it says "
                                  "which sources Brain holds and how to ask. Costs nothing.")
        :parameters (nlk:json-object "type" "object" "properties" (nlk:json-object)))
  "The gurusup_brain_init tool.")

(defparameter *tools-schema*
  (list :description (concatenate 'string
                                  "What this GuruSup Brain account can call: one line per tool, with "
                                  "what it is for. Costs nothing, and names the tools "
                                  "gurusup_brain_call takes.")
        :parameters (nlk:json-object "type" "object" "properties" (nlk:json-object)))
  "The gurusup_brain_tools tool.")

(defparameter *call-schema*
  (list :description (concatenate 'string
                                  "Call one GuruSup Brain tool, as gurusup_brain_tools named it, and "
                                  "answer its text: what the company's connected sources say about a "
                                  "customer, a project, a person or a decision. Some tools write to "
                                  "the workspace or message a teammate; the init contract says which.")
        :parameters (nlk:json-object
                     "type" "object"
                     "properties" (nlk:json-object
                                   "tool" (nlk:json-object
                                           "type" "string"
                                           "description" "The Brain tool's name, as gurusup_brain_tools answered it.")
                                   "arguments" (nlk:json-object
                                                "type" "object"
                                                "description" (concatenate
                                                               'string
                                                               "That tool's arguments as an object, or its own "
                                                               "JSON string. Absent for a tool that takes none.")))
                     "required" #("tool")))
  "The gurusup_brain_call tool.")
