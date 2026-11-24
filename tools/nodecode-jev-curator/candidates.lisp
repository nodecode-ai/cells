;;;; candidates.lisp --- deterministic lexical candidates; the judge sees only the residue.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's scoring, kept whole: name-token Jaccard, description-token
;;;; Jaccard, a term-frequency cosine over body plus description twice plus
;;;; name three times, and a small bonus when two names share their first
;;;; word. Each skill keeps its top-k neighbours, a pair once, and the list
;;;; stops at max_pairs, so a hundred-skill library never asks a judgement of
;;;; every pair.

(in-package #:nodecode-jev-curator)

(defparameter +min-similarity+ 0.12d0
  "Below this a pair is kept only when the two names share their first word.")

(defparameter +token-extra-chars+ "_-+."
  "Characters a term may hold besides letters and digits, as the original's
token pattern allows.")

(defstruct (features (:constructor make-features (name description counts prefix)))
  "One skill's lexical fingerprint."
  (name '() :read-only t)
  (description '() :read-only t)
  (counts nil :read-only t)
  (prefix "" :read-only t))

(defstruct (candidate (:constructor make-candidate (a b a-digest b-digest similarity signals)))
  "One pair worth judging: A and B sorted by name, their digests, the score."
  (a "" :read-only t)
  (b "" :read-only t)
  (a-digest "" :read-only t)
  (b-digest "" :read-only t)
  (similarity 0d0 :read-only t)
  (signals '() :read-only t))

(defun tokenize (text)
  "TEXT's terms: lowercased, split at everything that is not a letter, a
digit or one of +TOKEN-EXTRA-CHARS+, one-letter terms dropped."
  (let ((out '())
        (word (make-string-output-stream)))
    (labels ((flush ()
               (let ((token (get-output-stream-string word)))
                 (when (> (length token) 1) (push token out)))))
      (loop for char across (princ-to-string (or text ""))
            do (if (or (alphanumericp char) (find char +token-extra-chars+))
                   (write-char (char-downcase char) word)
                   (flush)))
      (flush))
    (nreverse out)))

(defun name-prefix (name)
  "NAME up to its first dash, lowercased: what two siblings share."
  (let ((dash (position #\- name)))
    (string-downcase (if dash (subseq name 0 dash) name))))

(defun features-of (skill)
  "SKILL's FINGERPRINT, weighted as the original weighs it."
  (let* ((name (tokenize (substitute #\Space #\- (skill-name skill))))
         (description (tokenize (skill-description skill)))
         (body (tokenize (skill-text skill)))
         (counts (make-hash-table :test #'equal)))
    (flet ((add (tokens weight)
             (dolist (token tokens) (incf (gethash token counts 0) weight))))
      (add body 1)
      (add description 2)
      (add name 3))
    (make-features name description counts (name-prefix (skill-name skill)))))

(defun token-set (tokens)
  "TOKENS as a hash table used as a set."
  (let ((set (make-hash-table :test #'equal)))
    (dolist (token tokens) (setf (gethash token set) t))
    set))

(defun jaccard (first second)
  "The Jaccard index of two term lists."
  (let ((a (token-set first))
        (b (token-set second))
        (both 0))
    (maphash (lambda (token value)
               (declare (ignore value))
               (when (gethash token b) (incf both)))
             a)
    (let ((union (- (+ (hash-table-count a) (hash-table-count b)) both)))
      (if (plusp union) (/ (float both 1d0) union) 0d0))))

(defun cosine (a b)
  "The cosine of two term-count tables."
  (let ((dot 0d0) (na 0d0) (nb 0d0))
    (maphash (lambda (token value)
               (incf dot (* value (gethash token b 0)))
               (incf na (* value value)))
             a)
    (maphash (lambda (token value)
               (declare (ignore token))
               (incf nb (* value value)))
             b)
    (if (and (plusp na) (plusp nb)) (/ dot (sqrt (* na nb))) 0d0)))

(defun similarity (a b)
  "How alike two fingerprints are, as (values SCORE SIGNALS)."
  (let* ((name-j (jaccard (features-name a) (features-name b)))
         (desc-j (jaccard (features-description a) (features-description b)))
         (both (cosine (features-counts a) (features-counts b)))
         (same-prefix (and (plusp (length (features-prefix a)))
                           (string= (features-prefix a) (features-prefix b))))
         (score (min 1d0 (+ (* 0.55d0 both) (* 0.25d0 desc-j) (* 0.15d0 name-j)
                            (if same-prefix 0.05d0 0d0))))
         (signals '()))
    (when same-prefix (push "name-prefix" signals))
    (when (>= name-j 0.5d0) (push "name-overlap" signals))
    (when (>= desc-j 0.35d0) (push "description-overlap" signals))
    (when (>= both 0.45d0) (push "content-overlap" signals))
    (values score (nreverse signals))))

(defun pair-key (a b)
  "The one key two names share, whichever order they are written in."
  (if (string< a b) (format nil "~a::~a" a b) (format nil "~a::~a" b a)))

(defun candidate-pairs (skills &key (top-k 5) (max-pairs 100) protected)
  "The pairs worth a judgement: each skill's TOP-K lexical neighbours,
deduplicated, best first, at most MAX-PAIRS. A protected or archived skill
stays in the inventory but is never proposed."
  (let* ((items (sort (remove-if (lambda (skill) (protected-p (skill-name skill) protected))
                                 (copy-list skills))
                       #'string< :key #'skill-name))
         (vector (coerce items 'vector))
         (features (map 'vector #'features-of vector))
         (neighbours (make-hash-table :test #'equal))
         (chosen (make-hash-table :test #'equal)))
    (flet ((push-neighbour (name other-name score signals)
             (push (list score other-name signals) (gethash name neighbours '()))))
      (loop for i from 0 below (length vector)
            do (loop for j from (1+ i) below (length vector)
                     do (multiple-value-bind (score signals) (similarity (aref features i) (aref features j))
                          (when (or (>= score +min-similarity+) (member "name-prefix" signals :test #'equal))
                            (push-neighbour (skill-name (aref vector i)) (skill-name (aref vector j)) score signals)
                            (push-neighbour (skill-name (aref vector j)) (skill-name (aref vector i)) score signals))))))
    (let ((by-name (make-hash-table :test #'equal)))
      (loop for skill across vector do (setf (gethash (skill-name skill) by-name) skill))
      (dolist (name (sort (loop for key being the hash-keys of neighbours collect key) #'string<))
        (let ((ranked (sort (copy-list (gethash name neighbours))
                            (lambda (a b)
                              (if (= (first a) (first b))
                                  (string< (second a) (second b))
                                  (> (first a) (first b)))))))
          (dolist (row (subseq ranked 0 (min (max 1 top-k) (length ranked))))
            (destructuring-bind (score other signals) row
              (let* ((first (skill-name (gethash name by-name)))
                     (second (skill-name (gethash other by-name)))
                     (a (if (string< first second) (gethash name by-name) (gethash other by-name)))
                     (b (if (string< first second) (gethash other by-name) (gethash name by-name))))
                (setf (gethash (pair-key (skill-name a) (skill-name b)) chosen)
                      (make-candidate (skill-name a) (skill-name b)
                                      (skill-digest a) (skill-digest b)
                                      (coerce score 'double-float) signals)))))))
      (let ((all (sort (loop for pair being the hash-values of chosen collect pair)
                       (lambda (a b)
                         (cond ((/= (candidate-similarity a) (candidate-similarity b))
                                (> (candidate-similarity a) (candidate-similarity b)))
                               ((not (string= (candidate-a a) (candidate-a b)))
                                (string< (candidate-a a) (candidate-a b)))
                               (t (string< (candidate-b a) (candidate-b b))))))))
        (subseq all 0 (min (max 1 max-pairs) (length all)))))))

(defun baseline-relation (pair)
  "The cheap guess a pair gets without a judgement, as the original proposes it."
  (let ((score (candidate-similarity pair)))
    (cond ((>= score 0.92d0) "duplicate")
          ((>= score 0.72d0) "same_class")
          (t "unrelated"))))
