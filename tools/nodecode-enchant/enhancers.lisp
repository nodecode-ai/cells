;;;; enhancers.lisp --- the five enhancers, their instruction, and the limits.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original ships five enhancers, each a name and the instruction text a
;;;; rewrite runs with; it sends that body as the system message and the
;;;; draft as the user message, and every body ends in the same demand for the
;;;; rewritten prompt alone. The texts are data here, because they are the
;;;; behaviour: a layer may reword one and every rewrite follows.

(in-package #:nodecode-enchant)

(defparameter +enhancers+
  '(("clarify" . "Make the request unambiguous without adding requirements.")
    ("tighten" . "Remove filler while preserving intent and constraints.")
    ("grammar" . "Correct grammar and style without changing meaning.")
    ("expand" . "Expand into concrete ordered work.")
    ("summarize" . "Summarize the request and preserve decisions."))
  "The five enhancers the original seeds, in its order, name and instruction
unchanged: a name is what /enchant and enchant_propose take, and the
instruction is what the model is given.")

(defparameter +return-only+ "Return only the rewritten prompt."
  "What every instruction ends in, as the original's composer appended it.")

(defparameter +max-draft-chars+ 12000
  "The longest draft one rewrite takes, in characters: the original's ceiling
on the input, and on the answer it keeps.")

(defparameter +max-output-chars+ 12000
  "The longest rewrite kept, in characters; the rest is dropped.")

(defparameter +default-max-tokens+ 4096
  "One rewrite's completion ceiling, as the original asked for.")

(defun enhancer-names ()
  "The five names, in the original's order."
  (mapcar #'car +enhancers+))

(defun enhancer-body (name)
  "NAME's instruction text, or a refusal naming the five there are."
  (or (cdr (assoc (string-downcase (nlk:trimmed (or name ""))) +enhancers+
                   :test #'string=))
      (fail "~s is no enhancer: ~{~a~^, ~}" name (enhancer-names))))

(defun instruction (name)
  "The system message a rewrite with NAME runs with: its body, then the one
demand for the rewritten prompt alone."
  (format nil "~a~%~%~a" (enhancer-body name) +return-only+))

(defun draft-text (draft)
  "DRAFT trimmed, as the text one rewrite is about: a refusal when it is empty
or past the ceiling."
  (let ((draft (nlk:trimmed (or draft ""))))
    (cond ((string= "" draft) (fail "there is no draft to enchant"))
          ((> (length draft) +max-draft-chars+)
           (fail "the draft is ~d characters; one enchant takes at most ~d - cut it down first"
                 (length draft) +max-draft-chars+))
          (t draft))))
