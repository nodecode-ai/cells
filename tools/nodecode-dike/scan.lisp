;;;; scan.lisp --- the brief read as citations, and each one judged against the pack.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode-dike)

(defun names-in (folded)
  "The statutes FOLDED names, in order. A name the pack carries is looked up;
a ley whose number the pack does not carry is kept, so the answer can say the
brief cites outside the corpus instead of staying silent."
  (load-corpus)
  (let ((found '()) (n (length folded)))
    (dolist (key *mention-keys*)
      (unless (find-if #'digit-char-p key)
        (let ((i 0))
          (loop for at = (search key folded :start2 i)
                while at
                do (when (and (word-start-p folded at) (word-end-p folded (+ at (length key))))
                     (push (make-named at (+ at (length key)) (subseq folded at (+ at (length key)))
                                        (gethash key *mentions*))
                           found))
                   (setf i (1+ at))))))
    ;; the numbered statutes: ley 19.880, decreto ley 3.500, dfl 1
    (let ((i 0))
      (loop while (< i n)
            do (let ((span (cond ((and (word-start-p folded i) (match-at folded i "decreto ley")) 11)
                                   ((and (word-start-p folded i) (match-at folded i "ley")) 3)
                                   ((and (word-start-p folded i) (match-at folded i "dfl")) 3)
                                   ((and (word-start-p folded i) (match-at folded i "dl") (word-end-p folded (+ i 2))) 2)))
                     (kind (cond ((match-at folded i "decreto ley") :decreto-ley)
                                 ((match-at folded i "dfl") :dfl)
                                 ((match-at folded i "dl") :dl)
                                 ((match-at folded i "ley") :ley))))
                 (if (null span)
                     (incf i)
                     (let* ((j (skip-spaces folded (+ i span)))
                            (j (cond ((match-at folded j "n°") (skip-spaces folded (+ j 2)))
                                     ((match-at folded j "nº") (skip-spaces folded (+ j 2)))
                                     ((match-at folded j "nº.") (skip-spaces folded (+ j 3)))
                                     (t j)))
                            (end (digits-at folded j)))
                       (if (and (> end j) (<= (- end j) 12))
                           (let* ((digits (remove #\. (subseq folded j end)))
                                  (number (string-left-trim "0" digits))
                                  (designation (ecase kind
                                                 (:ley (format nil "ley ~a" number))
                                                 (:decreto-ley (format nil "decreto ley ~a" number))
                                                 ((:dl :dfl) (format nil "decreto ley ~a" number)))))
                             (push (make-named i end (subseq folded i end)
                                                (gethash designation *mentions*))
                                   found)
                             (setf i end))
                           (incf i)))))))
    (sort found #'< :key #'named-start)))

(defun citations-in (folded &optional (text folded))
  "Every article citation in FOLDED, in order: the original's grammar, read from
the prefix to the end of its enumeration. TEXT is the unfolded brief the raw
text of each citation is cut from."
  (let ((out '()) (n (length folded)) (i 0))
    (loop while (< i n)
          do (let ((after (article-prefix-end folded i)))
               (if (null after)
                   (incf i)
                   (let ((j after) (first t))
                     (loop
                       (multiple-value-bind (token end transitory)
                           (or (and first (transitory-at folded j))
                               (citation-number-at folded j))
                         (declare (ignore transitory))
                         (if (null token)
                             (return)
                             (progn (push (make-citation i end (subseq text i end) token nil) out)
                                    (setf first nil)
                                    (let ((next (separator-end folded end)))
                                      (if (null next)
                                          (progn (setf i (max (1+ i) end)) (return))
                                          (setf j next)))))))
                     (when (<= i after) (incf i))))))
    (nreverse out)))

(defun sentence-span (text position)
  "The sentence of TEXT holding POSITION, as its start and end: a full stop, a
semicolon, a blank line, or a line break closes one."
  (let ((start 0) (end (length text)))
    (loop for i downfrom (1- (min position (length text))) to 0
          when (and (member (char text i) '(#\. #\; #\Newline)) (char= (char text i) #\Newline))
            do (progn (setf start (1+ i)) (return))
          when (and (char= (char text i) #\.) (< (1+ i) (length text))
                    (member (char text (1+ i)) '(#\Space #\Tab #\Newline)))
            do (progn (setf start (min (length text) (+ i 2))) (return)))
    (loop for i from (min position (length text)) below (length text)
          when (char= (char text i) #\Newline)
            do (progn (setf end i) (return))
          when (and (char= (char text i) #\.) (< (1+ i) (length text))
                    (member (char text (1+ i)) '(#\Space #\Tab #\Newline)))
            do (progn (setf end (1+ i)) (return)))
    (values start end)))

(defun resolve-citation (citation names text)
  "Set CITATION's statute from the names its sentence carries. A sentence that
names one statute resolves to it; several resolve to the nearest one after the
citation, or before it when none follows, and a name that means more than one
statute is ambiguous."
  (multiple-value-bind (start end) (sentence-span text (citation-start citation))
    (let* ((here (remove-if-not (lambda (named) (and (>= (named-start named) start)
                                                    (< (named-start named) end)))
                                names))
           (after (find-if (lambda (named) (>= (named-start named) (citation-start citation))) here))
           (before (car (last (remove-if (lambda (named) (>= (named-start named) (citation-start citation)))
                                         here))))
           (chosen (or after before)))
      (cond ((null here) (list :state "UNRESOLVABLE" :reason "ninguna norma nombrada en la oración"))
            ((null (named-slugs chosen))
             (list :state "UNRESOLVABLE"
                   :reason (format nil "~a no está en el corpus" (named-designation chosen))
                   :designation (named-designation chosen)))
            ((rest (named-slugs chosen))
             (list :state "AMBIGUOUS" :reason "el nombre admite más de una norma"
                   :slugs (named-slugs chosen) :designation (named-designation chosen)))
            (t (list :state "RESOLVED" :slug (first (named-slugs chosen))
                     :designation (named-designation chosen)))))))

(defun in-force-p (article as-of)
  "IN_FORCE, NOT_YET_IN_FORCE or OUT_OF_FORCE for ARTICLE judged at AS-OF."
  (let ((from (article-from article)) (to (article-to article))
        (status (article-status article)))
    (cond ((string= status "DEROGADA") "OUT_OF_FORCE")
          ((and (plusp (length to)) (not (string< as-of to))) "OUT_OF_FORCE")
          ((and (plusp (length from)) (string< as-of from)) "NOT_YET_IN_FORCE")
          (t "IN_FORCE"))))
