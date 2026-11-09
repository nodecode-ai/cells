;;;; specialists.lisp --- the roster: built-in team, a user's own, and the
;;;; system prompt one brief is asked under.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The six built-in roles and the paragraph they all share are the
;;;; original's (specialists.py:10-70), character for character, because a
;;;; roster is a product decision and not an implementation detail: an answer
;;;; from "design" should be the answer the original's "design" gives.

(in-package #:nodecode-staff)

(defconstant +max-id+ 40 "A specialist id, in characters, as the original bounds it.")
(defconstant +max-title+ 80 "A specialist's title, in characters.")
(defconstant +max-focus+ 300 "A specialist's focus line, in characters.")
(defconstant +max-prompt+ 4000 "A specialist's own instructions, in characters.")

(defparameter +base-prompt+
  "You are part of a small staff team working for one user inside Nodecode. Be concrete, cite sources (URLs) when you state facts you looked up, flag uncertainty, and end with 'Next steps' as a short bullet list."
  "What every specialist's system prompt opens with: the original's team paragraph.")

(defparameter +default-specialists+
  '(("chief" "Chief of staff" "coordination, breaking work down, routing to specialists"
     "You are the chief of staff. Restate the goal in one line, break it into 3-7 concrete work items, assign each to the best specialist id from the roster, note dependencies and what 'done' looks like. Do not do the specialists' work yourself; produce the plan.")
    ("saas" "SaaS analyst" "SaaS products, pricing, positioning, go-to-market"
     "You are a SaaS strategy analyst. Cover market, competitors, pricing models, ICP, positioning and go-to-market channels. Prefer numbers and comparisons.")
    ("design" "Design researcher" "UI/UX, visual references, motion and interaction design"
     "You are a product design researcher. Give visual references, UI/UX patterns, motion and interaction ideas, and the trade-offs of each. Be specific about layout, typography, color and animation technique.")
    ("vision-agents" "Decision & vision-agent architect" "agent systems that perceive, decide and act; evaluation and guardrails"
     "You design agent systems that combine perception (vision/LLM) with decision logic. Describe architecture, decision primitives, confidence thresholds, human-in-the-loop escalation, evaluation and failure modes.")
    ("computer-vision" "Computer vision researcher" "CV applications: inspection, agriculture/livestock, drones, edge deployment"
     "You are an applied computer vision researcher. Cover use cases, models (detection, segmentation, tracking), datasets, hardware/edge constraints, accuracy reported in the field, vendors and open gaps.")
    ("x" "X (Twitter) researcher" "public conversation on X: who is talking, what is trending, sentiment"
     "You research the public conversation on X/Twitter. Summarise themes, notable accounts and posts (with links when available), sentiment and what changed recently. Never invent posts or handles; say so when you could not verify."))
  "The team a user gets before writing one of their own: (ID TITLE FOCUS PROMPT).")

;;; --- an id ---------------------------------------------------------------

(defun id-character-p (char)
  (or (alphanumericp char) (char= char #\-) (char= char #\_)))

(defun valid-id-p (id)
  "Whether ID is a specialist id: lower-case letters, digits, - and _, up to
+MAX-ID+, starting with a letter or a digit."
  (and (stringp id)
       (plusp (length id))
       (<= (length id) +max-id+)
       (alphanumericp (char id 0))
       (every #'id-character-p id)))

;;; --- one entry ------------------------------------------------------------

(defun field (object name)
  "NAME off OBJECT, a JSON object (a hash table, or an alist), as a string, or
NIL. A wrong type reads NIL."
  (cond
    ((hash-table-p object) (nlk:json-value object :string name))
    ((and (listp object) (assoc name object :test #'equal))
     (let ((value (cdr (assoc name object :test #'equal))))
       (and (stringp value) value)))
    (t nil)))

(defun clamp (text limit)
  "TEXT cut to LIMIT characters."
  (subseq text 0 (min (length text) limit)))

(defun clean-specialist (entry)
  "ENTRY as an (ID TITLE FOCUS PROMPT) list, or NIL when it names no usable id.
A bare string is a specialist with that one id and the default paragraph."
  (let ((id (string-downcase (trim (if (stringp entry) entry (field entry "id"))))))
    (when (valid-id-p id)
      (let* ((said (trim (field entry "title")))
             (title (if (plusp (length said)) said id))
             (focus (trim (field entry "focus")))
             (said-prompt (trim (field entry "prompt")))
             (prompt (if (plusp (length said-prompt)) said-prompt
                         (format nil "You are the ~a specialist." id))))
        (list id (clamp title +max-title+) (clamp focus +max-focus+) (clamp prompt +max-prompt+))))))

(defun clean-roster (entries)
  "ENTRIES as a roster: one entry per id, the first of a repeated id kept."
  (let ((seen '()) (roster '()))
    (dolist (entry entries (nreverse roster))
      (let ((clean (clean-specialist entry)))
        (when (and clean (not (member (first clean) seen :test #'string=)))
          (push (first clean) seen)
          (push clean roster))))))

(defun string-list (raw)
  "RAW as a list of entries to read a roster off: a JSON array, a list, one
JSON string holding an array, or one id."
  (cond
    ((vectorp raw) (coerce raw 'list))
    ((listp raw) raw)
    ((stringp raw)
     (let ((text (trim raw)))
       (cond
         ((zerop (length text)) '())
         ((char= #\[ (char text 0))
          (let ((decoded (ignore-errors (nlk:decode-json text))))
            (if (vectorp decoded) (coerce decoded 'list) (list text))))
         (t (list text)))))
    (t '())))

(defun specialists (&optional raw)
  "The roster the cell runs on: RAW (the `specialists' setting, undecoded) as
a list of (ID TITLE FOCUS PROMPT), or the built-in team when it names none."
  (let ((roster (clean-roster (string-list raw))))
    (or roster (mapcar (lambda (entry) (copy-list entry)) +default-specialists+))))

(defun find-specialist (roster id)
  "The roster's entry for ID, or NIL."
  (let ((key (string-downcase (trim id))))
    (find key roster :key #'first :test #'string=)))

(defun specialist-system-prompt (specialist roster)
  "SPECIALIST's system prompt, with the team it belongs to named after it, as
the original composes it (specialists.py:118-121)."
  (format nil "~a~%~%Role: ~a.~%~a~%~%Team roster: ~a."
          +base-prompt+
          (second specialist)
          (fourth specialist)
          (format nil "~{~a~^; ~}"
                  (mapcar (lambda (entry) (format nil "~a (~a)" (first entry)
                                                   (let ((focus (third entry)))
                                                     (if (plusp (length focus)) focus (second entry)))))
                          roster))))

(defun roster-line (roster)
  "ROSTER as one line a person reads: id, title and focus, one per item."
  (format nil "~{~a~^, ~}" (mapcar #'first roster)))
