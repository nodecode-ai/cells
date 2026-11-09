;;;; review.lisp --- the review: what changed between two texts, as groups.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A proposal is not applied whole. The original compares the draft with
;;;; the rewrite a token at a time - each run of whitespace, each run of other
;;;; characters - takes the longest common subsequence of the two, and cuts the
;;;; difference into groups: one group per run of changed tokens between
;;;; unchanged ones, each a character range of the original and the text that
;;;; goes in its place. A group the person leaves out is simply not composed
;;;; back in. Past 512 tokens on either side, or past 32 groups, the whole
;;;; rewrite is offered as one instead, because the table stops being cheap.
;;;;
;;;; Nothing here calls a model or touches a file: two strings in, a value out.

(in-package #:nodecode-enchant)

(defparameter +max-diff-tokens+ 512
  "How many tokens a side may have before the change is offered whole: the
original's ceiling on the diff, and on the table it walks.")

(defparameter +max-groups+ 32
  "How many groups one proposal may have before it is offered whole instead.")

;;; --- tokens -------------------------------------------------------------------

(defun whitespace-p (char)
  "T when CHAR is one of the characters a token boundary is made of."
  (and (member char '(#\Space #\Tab #\Newline #\Return #\Page) :test #'char=) t))

(defun tokens (text)
  "TEXT as the tokens a change is measured in: each run of whitespace, each
run of other characters, in order."
  (let ((n (length text)) (out '()) (start 0))
    (loop while (< start n)
          for space = (whitespace-p (char text start))
          for end = (or (position-if (lambda (char) (not (eq (whitespace-p char) space)))
                                     text :start start)
                        n)
          do (push (subseq text start end) out)
             (setf start end))
    (nreverse out)))

;;; --- the one edit path ---------------------------------------------------------

(defun lcs-actions (left right)
  "The edit path from LEFT to RIGHT the original walked: a list of
(:same . TOKEN), (:delete . TOKEN) and (:insert . TOKEN), in order."
  (let* ((n (length left))
         (m (length right))
         (table (make-array (list (1+ n) (1+ m)) :element-type 'fixnum :initial-element 0)))
    (loop for i from (1- n) downto 0
          do (loop for j from (1- m) downto 0
                   do (setf (aref table i j)
                            (if (string= (aref left i) (aref right j))
                                (1+ (aref table (1+ i) (1+ j)))
                                (max (aref table (1+ i) j) (aref table i (1+ j)))))))
    (let ((actions '()) (i 0) (j 0))
      (loop while (or (< i n) (< j m))
            do (cond ((and (< i n) (< j m) (string= (aref left i) (aref right j)))
                      (push (cons :same (aref left i)) actions)
                      (incf i)
                      (incf j))
                     ((and (< j m) (or (= i n) (>= (aref table i (1+ j)) (aref table (1+ i) j))))
                      (push (cons :insert (aref right j)) actions)
                      (incf j))
                     (t (push (cons :delete (aref left i)) actions)
                        (incf i))))
      (nreverse actions))))

(defun token-offsets (left)
  "Where each token of LEFT starts, in characters, and the end after the last."
  (let* ((n (length left))
         (offsets (make-array (1+ n) :element-type 'fixnum :initial-element 0)))
    (loop for i below n
          do (setf (aref offsets (1+ i)) (+ (aref offsets i) (length (aref left i)))))
    offsets))

(defun cut-groups (original actions left)
  "ACTIONS walked into the change groups of ORIGINAL, each (:id :start :end
:removed :replacement): a run of changed tokens, its character range in
ORIGINAL, the text it takes out and the text it puts in."
  (let ((offsets (token-offsets left)) (runs '()) (current nil) (at 0))
    (labels ((finish-run ()
               (when current
                 (push current runs)
                 (setf current nil))))
      (dolist (action actions)
        (ecase (car action)
          (:same (finish-run) (incf at))
          (:delete
           (unless current (setf current (list :start at :end at :replacement "")))
           (incf (getf current :end))
           (incf at))
          (:insert
           (unless current (setf current (list :start at :end at :replacement "")))
           (setf (getf current :replacement)
                 (concatenate 'string (getf current :replacement) (cdr action))))))
      (finish-run))
    (loop for run in (nreverse runs)
          for index from 1
          for start = (aref offsets (getf run :start))
          for end = (aref offsets (getf run :end))
          collect (list :id (format nil "change-~d" index)
                        :start start :end end
                        :removed (subseq original start end)
                        :replacement (getf run :replacement)))))

(defun change-groups (original proposed)
  "How PROPOSED differs from ORIGINAL, as the groups a person picks from.
=> (values GROUPS WHOLE-P): GROUPS empty when the two are equal, and WHOLE-P
T - GROUPS NIL - when the change is past what a group review is good for."
  (let* ((left (coerce (tokens original) 'vector))
         (right (coerce (tokens proposed) 'vector)))
    (if (or (> (length left) +max-diff-tokens+)
            (> (length right) +max-diff-tokens+))
        (values nil t)
        (let ((groups (cut-groups original (lcs-actions left right) left)))
          (if (> (length groups) +max-groups+)
              (values nil t)
              (values groups nil))))))

;;; --- taking the chosen ones --------------------------------------------------

(defun compose (original groups &optional ids)
  "ORIGINAL with every group in IDS put in its place - all of them when IDS
is NIL - and every other left out. The groups are the strategy's, in order;
a group overlapping one already taken is skipped, so a choice can never
duplicate text."
  (let ((chosen (if ids
                    (remove-if-not (lambda (group) (member (getf group :id) ids
                                                          :test #'string-equal))
                                   groups)
                    groups))
        (cursor 0)
        (out '()))
    (dolist (group chosen)
      (let ((start (getf group :start))
            (end (getf group :end)))
        (when (>= start cursor)
          (push (subseq original cursor start) out)
          (push (getf group :replacement) out)
          (setf cursor end))))
    (push (subseq original cursor) out)
    (apply #'concatenate 'string (nreverse out))))

;;; --- reading one out loud -----------------------------------------------------

(defun shorten (text limit)
  "TEXT, its first LIMIT characters with an ellipsis when it is longer."
  (if (> (length text) limit)
      (concatenate 'string (subseq text 0 limit) "...")
      text))

(defun group-line (group &key (limit 80))
  "One group as the line a person reads: its id, what it takes out, what it
puts in."
  (format nil "~a: ~s -> ~s"
          (getf group :id)
          (shorten (getf group :removed) limit)
          (shorten (getf group :replacement) limit)))
