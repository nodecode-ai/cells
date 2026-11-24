;;;; roster.lisp --- the skills the organism keeps, read as data.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; In Hermes a skill was a SKILL.md under a folder the plugin walked. In
;;;; Nodecode it is a prose (define-skill name "one sentence" ;; steps ...)
;;;; kept as one file in a knowledge cell, which the core reads as data and
;;;; never loads (NLK:KNOWLEDGE-ENTRIES). That is the roster: the same
;;;; things the index section names, read here with one description for the
;;;; ranking question and one excerpt for the re-read.
;;;;
;;;; A skill imported from a SKILL.md elsewhere keeps its frontmatter in the
;;;; file, so the excerpt strips it: the ranking reads the description, the
;;;; re-read reads the prose under it.

(in-package #:nodecode-dogmatic-skill-router)

(defparameter +index-chars+ 120
  "How much of a description the ranking question carries (INDEX_DESC_CHARS).")

(defparameter +max-name-chars+ 64
  "The longest skill name the wire is given (MAX_NAME_CHARS).")

(defparameter +blank-chars+ '(#\Space #\Tab #\Newline #\Return #\Page)
  "What a squash collapses.")

(defun squash (text)
  "TEXT on one line: whitespace runs collapsed to one space, both ends trimmed."
  (let ((chars '())
        (space nil))
    (loop for char across (or text "")
          do (if (member char +blank-chars+)
                 (setf space (and chars t))
                 (progn (when space (push #\Space chars))
                        (setf space nil)
                        (push char chars))))
    (coerce (nreverse chars) 'string)))

(defun clip (text chars)
  "TEXT at most CHARS long."
  (subseq text 0 (min (max chars 0) (length text))))

(defun frontmatter-end (text)
  "Where a leading --- block of TEXT ends, or NIL when it has none."
  (let ((at (search "---" text)))
    (when (and at (zerop (length (squash (subseq text 0 at)))))
      (let ((close (search "---" text :start2 (+ at 3))))
        (and close (min (length text) (+ close 3)))))))

(defun skill-text (entry)
  "ENTRY's own words: its span in its file, frontmatter stripped, one line."
  (handler-case
      (let* ((path (nlk:knowledge-entry-path entry))
             (text (uiop:read-file-string path))
             (start (min (or (nlk:knowledge-entry-start entry) 0) (length text)))
             (end (min (or (nlk:knowledge-entry-end entry) (length text)) (length text)))
             (body (subseq text start end)))
        (squash (subseq body (or (frontmatter-end body) 0))))
    (error () "")))

(defun safe-name (name)
  "NAME as the wire and the harness block carry it: no angle brackets, at
most +MAX-NAME-CHARS+ characters."
  (clip (remove-if (lambda (char) (member char '(#\< #\>))) (squash name))
        +max-name-chars+))

(defun make-skill (entry)
  "ENTRY, a knowledge entry, as the plist this cell routes over, or NIL when
it is not a prose skill."
  (when (equal "DEFINE-SKILL" (ignore-errors (nlk:knowledge-entry-operator entry)))
    (let ((name (safe-name (or (nlk:knowledge-entry-name entry) ""))))
      (when (plusp (length name))
        (list :name name
              :description (or (clip (squash (or (nlk:knowledge-entry-description entry) ""))
                                     +index-chars+)
                               name)
              :text (skill-text entry))))))

(defun roster ()
  "Every prose skill the organism keeps, by name: a list of
(:name :description :text) plists. An unreadable knowledge cell is an empty
roster, never an error: routing is advisory."
  (let ((seen (make-hash-table :test #'equal))
        (skills '()))
    (dolist (entry (ignore-errors (nlk:knowledge-entries))
             (sort skills #'string< :key (lambda (skill) (getf skill :name))))
      (let ((skill (make-skill entry)))
        (when (and skill (not (gethash (getf skill :name) seen)))
          (setf (gethash (getf skill :name) seen) t)
          (push skill skills))))))

(defun excerpt (skill chars)
  "SKILL's words, at most CHARS of them: what the re-read carries."
  (clip (or (getf skill :text) "") chars))
