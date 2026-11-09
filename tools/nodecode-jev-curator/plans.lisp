;;;; plans.lisp --- one judgement folded, the typed graph, and its merge plans.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The evidence policy is versioned here, never operator-configurable, as
;;;; the original keeps it: a judgement authorizes an absorption only when it
;;;; picks an absorbing relation with confidence 0.85 or more, the offered
;;;; options covered the pair at 0.70 or more, operational conflict is at
;;;; most 0.15, and the absorbed skill's rules survive in the canonical at
;;;; 0.90 or more. Absorption is star-shaped, never transitive: every
;;;; absorbed skill carries its own direct judgement against the plan's
;;;; canonical, so A~B plus B~C never merges C into A. A plan's id is a hash
;;;; of the content it was built from, and an apply re-checks every digest.

(in-package #:nodecode-jev-curator)

(defparameter +contract-version+ "skill-relations-v3"
  "The question contract's version: a judgement built under another one is
stale.")

(defparameter +graph-version+ "skill-graph-v1"
  "The evidence policy's version: it names every plan id.")

(defparameter +relations+
  '("duplicate" "a_subset_of_b" "b_subset_of_a" "same_class"
    "complementary" "conflict" "unrelated" "insufficient_evidence")
  "The eight labels one pair of skills can be judged with.")

(defparameter +absorbing-relations+ '("duplicate" "a_subset_of_b" "b_subset_of_a")
  "The only relations that let one skill's rules be carried by another.")

(defparameter +min-confidence+ 0.85d0
  "Confidence in an absorbing relation choice.")

(defparameter +min-coverage+ 0.70d0
  "That the offered options adequately describe the pair.")

(defparameter +max-conflict+ 0.15d0
  "Operational conflict tolerated between two skills.")

(defparameter +min-preservation+ 0.90d0
  "That the absorbed skill's rules survive in the canonical.")

(defparameter +max-plans+ 20
  "How many plans one scan proposes at most.")

(defstruct (judgment (:constructor make-judgment
                        (a b a-digest b-digest relation confidence coverage
                         preservation-a-in-b preservation-b-in-a conflict evidence)))
  "One judged pair, bound to the digests it was judged at."
  (a "" :read-only t)
  (b "" :read-only t)
  (a-digest "" :read-only t)
  (b-digest "" :read-only t)
  (relation "insufficient_evidence" :read-only t)
  (confidence 0d0 :read-only t)
  (coverage 0d0 :read-only t)
  (preservation-a-in-b 0d0 :read-only t)
  (preservation-b-in-a 0d0 :read-only t)
  (conflict 0d0 :read-only t)
  (evidence "whole" :read-only t))

(defstruct (edge (:constructor make-edge (key a b relation confidence coverage conflict
                                             authorized canonical absorbed refusals)))
  "One deduplicated judgement: AUTHORIZED is the only gate absorption has."
  (key "" :read-only t)
  (a "" :read-only t)
  (b "" :read-only t)
  (relation "" :read-only t)
  (confidence 0d0 :read-only t)
  (coverage 0d0 :read-only t)
  (conflict 0d0 :read-only t)
  (authorized nil :read-only t)
  (canonical "" :read-only t)
  (absorbed "" :read-only t)
  (refusals '() :read-only t))

(defstruct (merge-plan (:constructor make-merge-plan
                          (id canonical canonical-digest absorbed absorbed-digests
                           relation-keys status blockers)))
  "One star merge, or the explicit no-op a scan with nothing to merge answers."
  (id "" :read-only t)
  (canonical "" :read-only t)
  (canonical-digest "" :read-only t)
  (absorbed '() :read-only t)
  (absorbed-digests '() :read-only t)
  (relation-keys '() :read-only t)
  (status "noop" :read-only t)
  (blockers '() :read-only t))

;;; --- reading one answer ---------------------------------------------------------------

(defun answer-probability (answers key)
  "The probability of yes the wire answered under KEY, 0.0 when it is absent
or not a number."
  (let* ((question (nlk:json-value answers :object key))
         (value (or (nlk:json-value question :number "noul")
                    (nlk:json-value question :number "bool"))))
    (coerce (or value 0d0) 'double-float)))

(defun answer-choice (answers key)
  "The option the wire chose under KEY, and its confidence."
  (let ((question (nlk:json-value answers :object key)))
    (values (or (nlk:json-value question :string "choice")
                (nlk:json-value question :string "label")
                "insufficient_evidence")
            (coerce (or (nlk:json-value question :number "confidence") 0d0) 'double-float))))

(defun judgment-from-answers (a b answers)
  "A and B's JUDGMENT folded from one whole-pair ANSWER object. An option
outside the contract reads as insufficient evidence, never as a licence."
  (multiple-value-bind (relation confidence) (answer-choice answers "relation")
    (make-judgment (skill-name a) (skill-name b) (skill-digest a) (skill-digest b)
                   (if (member relation +relations+ :test #'string=)
                       relation
                       "insufficient_evidence")
                   (max 0d0 (min 1d0 confidence))
                   (answer-probability answers "coverage")
                   (answer-probability answers "a_in_b")
                   (answer-probability answers "b_in_a")
                   (answer-probability answers "conflict")
                   "whole")))

;;; --- the graph ------------------------------------------------------------------------

(defun pair-digests-match-p (judgment skills)
  "Whether the pair was judged at the digests the inventory holds now."
  (let ((a (gethash (judgment-a judgment) skills))
        (b (gethash (judgment-b judgment) skills)))
    (and a b
         (string= (skill-digest a) (judgment-a-digest judgment))
         (string= (skill-digest b) (judgment-b-digest judgment)))))

(defun absorbing-direction (judgment)
  "The (values CANONICAL ABSORBED PRESERVATION) an absorbing judgement names;
preservation is of the absorbed skill's rules inside the canonical."
  (cond ((string= (judgment-relation judgment) "a_subset_of_b")
         (values (judgment-b judgment) (judgment-a judgment)
                 (judgment-preservation-a-in-b judgment)))
        ((string= (judgment-relation judgment) "b_subset_of_a")
         (values (judgment-a judgment) (judgment-b judgment)
                 (judgment-preservation-b-in-a judgment)))
        (t (if (string< (judgment-a judgment) (judgment-b judgment))
               (values (judgment-a judgment) (judgment-b judgment)
                       (min (judgment-preservation-a-in-b judgment)
                            (judgment-preservation-b-in-a judgment)))
               (values (judgment-b judgment) (judgment-a judgment)
                       (min (judgment-preservation-b-in-a judgment)
                            (judgment-preservation-a-in-b judgment)))))))

(defun evaluate-judgment (judgment skills &optional protected)
  "JUDGMENT as an EDGE: authorized only when every gate in the evidence
policy passes, with the reason each refusal names."
  (let* ((key (pair-key (judgment-a judgment) (judgment-b judgment)))
         (refusals '())
         (canonical "") (absorbed ""))
    (unless (pair-digests-match-p judgment skills)
      (push "stale-hash" refusals))
    (dolist (name (list (judgment-a judgment) (judgment-b judgment)))
      (when (protected-p name protected) (push (format nil "protected:~a" name) refusals)))
    (cond ((string= (judgment-relation judgment) "insufficient_evidence")
           (push "insufficient-evidence" refusals))
          ((string= (judgment-relation judgment) "conflict")
           (push "conflict" refusals))
          ((member (judgment-relation judgment) +absorbing-relations+ :test #'string=)
           (when (< (judgment-confidence judgment) +min-confidence+) (push "low-confidence" refusals))
           (when (< (judgment-coverage judgment) +min-coverage+) (push "low-coverage" refusals))
           (when (> (judgment-conflict judgment) +max-conflict+) (push "conflict-score" refusals))
           (when (null refusals)
             (multiple-value-bind (name member preservation) (absorbing-direction judgment)
               (if (< preservation +min-preservation+)
                   (push "low-preservation" refusals)
                   (setf canonical name absorbed member))))))
    (make-edge key (judgment-a judgment) (judgment-b judgment)
               (judgment-relation judgment) (judgment-confidence judgment)
               (judgment-coverage judgment) (judgment-conflict judgment)
               (and (null refusals) (plusp (length canonical))) canonical absorbed
               (nreverse refusals))))

(defun plan-id (canonical canonical-digest absorbed-digests relation-keys)
  "A plan's id: the hash of exactly what it was built from."
  (format nil "merge-~a"
          (subseq (nlk:sha256-text
                   (format nil "~a~%~a~%~a~%~{~a~%~}~{~a~^~%~}"
                           +graph-version+ canonical canonical-digest
                           (loop for (name . digest) in (sort (copy-list absorbed-digests)
                                                               #'string< :key #'car)
                                 collect (format nil "~a=~a" name digest))
                           (sort (copy-list relation-keys) #'string<)))
                  7 19)))

(defun noop-plan (skills edges)
  "The plan a scan answers when nothing may merge, named by what it saw."
  (make-merge-plan
   (format nil "noop-~a"
           (subseq (nlk:sha256-text
                    (format nil "~a~%noop~%~{~a~%~}~{~a: ~a~^~%~}"
                            +graph-version+
                            (loop for skill in skills collect (format nil "~a=~a" (skill-name skill) (skill-digest skill)))
                            (loop for edge in edges collect (format nil "~a:~a" (edge-key edge) (edge-relation edge)))))
                   0 12))
   "" "" '() '() '() "noop" '()))

(defun build-plans (skills judgments &key (max-plans +max-plans+) protected)
  "The merge plans JUDGMENTS authorize over SKILLS, validated first; a scan
with nothing to merge answers one explicit no-op plan."
  (let ((items (make-hash-table :test #'equal))
        (best (make-hash-table :test #'equal)))
    (dolist (skill skills) (setf (gethash (skill-name skill) items) skill))
    (dolist (judgment (sort (copy-list judgments)
                             (lambda (a b)
                               (let ((ka (pair-key (judgment-a a) (judgment-b a)))
                                     (kb (pair-key (judgment-a b) (judgment-b b))))
                                 (if (string= ka kb)
                                     (> (judgment-confidence a) (judgment-confidence b))
                                     (string< ka kb))))))
      (let ((key (pair-key (judgment-a judgment) (judgment-b judgment))))
        (unless (gethash key best) (setf (gethash key best) judgment))))
    (let* ((edges (sort (loop for judgment being the hash-values of best
                               collect (evaluate-judgment judgment items protected))
                        #'string< :key #'edge-key))
           (claims (make-hash-table :test #'equal))
           (owner (make-hash-table :test #'equal))
           (plans '()))
      (dolist (edge edges)
        (when (edge-authorized edge)
          (push edge (gethash (edge-canonical edge) claims '()))))
      (let ((canonicals (sort (loop for key being the hash-keys of claims collect key) #'string<)))
        (dolist (canonical canonicals)
          (dolist (member (sort (mapcar #'edge-absorbed (gethash canonical claims)) #'string<))
            (unless (gethash member owner) (setf (gethash member owner) canonical))))
        (dolist (canonical canonicals)
          (let* ((mine (remove-if-not (lambda (edge) (string= (gethash (edge-absorbed edge) owner "") canonical))
                                       (gethash canonical claims)))
                 (absorbed (sort (mapcar #'edge-absorbed mine) #'string<)))
            (when absorbed
              (let ((blockers '()))
                (dolist (name (cons canonical absorbed))
                  (when (protected-p name protected)
                    (push (format nil "protected:~a" name) blockers)))
                (dolist (first (cons canonical absorbed))
                  (dolist (second (cons canonical absorbed))
                    (unless (string= first second)
                      (let ((edge (find (pair-key first second) edges :test #'string= :key #'edge-key)))
                        (when (and edge (or (string= (edge-relation edge) "conflict")
                                            (> (edge-conflict edge) +max-conflict+)))
                          (push (format nil "conflict:~a" (pair-key first second)) blockers))))))
                (when (gethash canonical owner)
                  (push (format nil "canonical-absorbed-by:~a" (gethash canonical owner)) blockers))
                (let* ((canonical-skill (gethash canonical items))
                       (digests (loop for name in absorbed
                                      collect (cons name (skill-digest (gethash name items))))
                              ))
                  (push (make-merge-plan
                         (plan-id canonical (skill-digest canonical-skill) digests
                                  (mapcar #'edge-key mine))
                         canonical (skill-digest canonical-skill) absorbed digests
                         (sort (mapcar #'edge-key mine) #'string<)
                         (if blockers "blocked" "validated")
                         (sort (delete-duplicates blockers :test #'string=) #'string<))
                        plans))))))
      (setf plans (sort plans
                        (lambda (a b)
                          (cond ((not (string= (merge-plan-status a) (merge-plan-status b)))
                                 (string= (merge-plan-status a) "validated"))
                                ((/= (length (merge-plan-absorbed a)) (length (merge-plan-absorbed b)))
                                 (> (length (merge-plan-absorbed a)) (length (merge-plan-absorbed b))))
                                (t (string< (merge-plan-canonical a) (merge-plan-canonical b)))))))
      (if plans (subseq plans 0 (min (max 1 max-plans) (length plans)))
          (list (noop-plan skills edges)))))))
