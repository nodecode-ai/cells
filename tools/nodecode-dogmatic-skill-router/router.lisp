;;;; router.lisp --- two stages, two thresholds, at most one skill name back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Request 1 ranks the whole roster with one Choice and scores the turn
;;;; with three yes/no questions (a gate: does this turn want a skill at
;;;; all?). Request 2 re-reads the top three with each candidate's own text
;;;; plus one absolute `fits' judgment per candidate. Two thresholds, at most
;;;; one name back; the question wording and thresholds follow the published
;;;; skill-suggestion cookbook, so an upstream change can be diffed.
;;;;
;;;; The model stays in charge: the line the turn is given says to ignore it
;;;; when it does not fit what was asked.

(in-package #:nodecode-dogmatic-skill-router)

(defparameter +shortlist-default+ 3
  "Candidates carried from request 1 into request 2 (SHORTLIST).")

(defparameter +excerpt-default+ 700
  "The skill text a candidate brings to request 2 (EXCERPT_CHARS).")

(defparameter +chunk+ 240
  "Skills per choice question; the API caps one at 255 options (CHUNK_CHOICES).")

(defparameter +suggest-chars-default+ 4000
  "Longer messages are left alone (suggest_chars).")

(defparameter +choice-instructions+
  "Which of these skills, if any, is the right one to load to help with the user's latest request?"
  "Request 1's question (CHOICE_INSTRUCTIONS).")

(defparameter +rerank-instructions+
  "Exactly one of these skills is the right one to load for the user's latest request. Which one? Read what each actually does, not just its name."
  "Request 2's question (RERANK_INSTRUCTIONS).")

(defparameter +gate-questions+
  '(("acts_on_user_system"
     . "Is the assistant being asked to act on the user's files, accounts, devices, or online services, rather than only to explain or advise?")
    ("would_follow_documented_procedure"
     . "Would a careful expert answering this consult a specific documented procedure or set of commands, rather than answering from general understanding?")
    ("prose_suffices"
     . "Could a knowledgeable generalist fully satisfy this request in prose, with no tools, no documentation, and no access to the user's files or accounts?"))
  "The gate: two questions point toward needing a skill, one points away (GATE_QUESTIONS).")

(defparameter +inverted+ '("prose_suffices")
  "The gate question whose yes means no skill is wanted (INVERTED).")

;;; --- what a turn is worth routing ------------------------------------------------

(defun skip-reason (text)
  "Why TEXT is left alone, or NIL when it is eligible (SKIP_REASON)."
  (let ((stripped (nlk:trimmed (or text ""))))
    (cond ((zerop (length stripped)) "empty")
          ((char= #\/ (char stripped 0)) "slash")
          ((> (length stripped) (setting :suggest-chars)) "too-long")
          ((search "skill_relevance" stripped) "already-routed"))))

(defun threshold (key)
  "The section's percentage KEY as a fraction, clamped to 0..1."
  (/ (float (max 0 (min 100 (setting key))) 1d0) 100d0))

;;; --- the choices both requests ask --------------------------------------------------

(defun choice-question (instructions criteria)
  "One choice question over CRITERIA, an option -> rubric table."
  (list :type :choice :instructions instructions :criteria criteria))

(defun ranking-questions (group chunked-p gate-p)
  "Request 1's questions over GROUP: one choice, and on the first chunk the
three gate questions."
  (let ((questions (make-hash-table :test #'equal))
        (criteria (make-hash-table :test #'equal)))
    (dolist (skill group)
      (setf (gethash (getf skill :name) criteria) (getf skill :description)))
    (when chunked-p
      (setf (gethash +none+ criteria) "None of these skills fit the request."))
    (setf (gethash "which" questions) (choice-question +choice-instructions+ criteria))
    (when gate-p
      (dolist (pair +gate-questions+)
        (setf (gethash (format nil "gate::~a" (car pair)) questions)
              (list :type :boolean :instructions (cdr pair)))))
    questions))

(defun rerank-questions (names skills)
  "Request 2's questions over NAMES: one choice carrying each candidate's
description and text, and one `fits' question each."
  (let ((questions (make-hash-table :test #'equal))
        (criteria (make-hash-table :test #'equal))
        (chars (setting :excerpt)))
    (dolist (name names)
      (let* ((skill (find name skills :test #'equal :key (lambda (skill) (getf skill :name))))
             (description (or (getf skill :description) name))
             (body (excerpt skill chars)))
        (setf (gethash name criteria)
              (if (plusp (length body)) (format nil "~a~%~%~a" description body) description))
        (setf (gethash (format nil "fits::~a" name) questions)
              (list :type :boolean
                    :instructions (format nil "Does the skill '~a' do the specific thing the user's request asks for? It is described as: ~a"
                                          name description)))))
    (setf (gethash +none+ criteria) "None of these skills fit the request.")
    (setf (gethash "which" questions) (choice-question +rerank-instructions+ criteria))
    questions))

;;; --- request 1 -------------------------------------------------------------------

(defun chunks (skills size)
  "SKILLS in groups of at most SIZE, in order."
  (loop for rest on skills by (lambda (rest) (nthcdr size rest))
        collect (subseq rest 0 (min size (length rest)))))

(defun gate-mean (answers)
  "The mean of the three gate judgments with `prose_suffices' inverted; a
missing one is no evidence, so 0.0 rather than a partial mean (GATE_MEAN)."
  (let ((values '()))
    (dolist (pair +gate-questions+ (if (= (length values) 3) (/ (reduce #'+ values) 3d0) 0d0))
      (let ((probability (answer-probability (answer-at answers (format nil "gate::~a" (car pair))))))
        (unless probability (return 0d0))
        (push (if (member (car pair) +inverted+ :test #'equal)
                  (- 1d0 probability)
                  probability)
              values)))))

(defun probability-pairs (answers)
  "ANSWERS' choice as (NAME . P) pairs, an unknown name included."
  (let ((probabilities (answer-probabilities (answer-at answers "which")))
        (pairs '()))
    (when (hash-table-p probabilities)
      (maphash (lambda (name value)
                 (when (realp value) (push (cons name (float value 1d0)) pairs)))
               probabilities))
    pairs))

(defun choice-probability (answers name)
  "The probability ANSWERS gave NAME in its choice, or 0."
  (or (cdr (assoc name (probability-pairs answers) :test #'equal)) 0d0))

(defun rank-probabilities (answers valid)
  "ANSWERS' choice as (NAME . P) pairs for the names this roster knows,
most probable first."
  (sort (remove-if-not (lambda (pair) (member (car pair) valid :test #'equal))
                        (delete +none+ (probability-pairs answers) :key #'car :test #'equal))
        #'> :key #'cdr))

(defun rank-wide (text skills)
  "Request 1: rank the whole roster, gate the turn, nominate a shortlist.
=> (:shortlist NAMES :gate G :calls N) or NIL (fail-open)."
  (let* ((groups (chunks skills +chunk+))
         (chunked-p (> (length groups) 1))
         (valid (mapcar (lambda (skill) (getf skill :name)) skills))
         (per-chunk '())
         (none-pressure '())
         (gate nil)
         (calls 0))
    (loop for group in groups
          for index from 0
          do (let* ((result (evaluate text (ranking-questions group chunked-p (zerop index))))
                    (answers (getf result :answers)))
               (incf calls)
               (unless answers (return-from rank-wide nil))
               (when (zerop index) (setf gate (gate-mean answers)))
               (push (rank-probabilities answers valid) per-chunk)
               (push (or (number-at (answer-at answers "which") +none+) 0d0) none-pressure)))
    (setf per-chunk (nreverse per-chunk) none-pressure (nreverse none-pressure))
    (let* ((tops (mapcar (lambda (ranked) (if ranked (cdr (first ranked)) -1d0)) per-chunk))
           (best (position (reduce #'max tops) tops))
           (shortlist '()))
      ;; The best chunk nominates first: with a small shortlist the later
      ;; chunk's winner would otherwise be cut by chunk order.
      (dolist (index (cons best (loop for i from 0 below (length per-chunk)
                                      unless (= i best) collect i)))
        (unless (and (/= index best) (>= (nth index none-pressure) +none-pressure+))
          (loop for pair in (nth index per-chunk)
                for name = (car pair)
                while (< (length shortlist) (max (setting :shortlist) 1))
                unless (member name shortlist :test #'equal)
                  do (setf shortlist (append shortlist (list name))))))
      (list :shortlist shortlist :gate (or gate 0d0) :calls calls))))

;;; --- requests 1 and 2 --------------------------------------------------------------

(defun decide (text)
  "The whole decision over the kept skills. => (:skill :gate :fits :probability
:calls) for the one skill that fits, or NIL when the turn wants none."
  (let ((skills (roster))
        (request (nlk:trimmed (or text ""))))
    (when (<= 2 (length skills))
      (let ((ranked (rank-wide request skills)))
        (when (and ranked
                   (>= (getf ranked :gate) (threshold :gate))
                   (getf ranked :shortlist))
          (let* ((names (getf ranked :shortlist))
                 (answers (getf (evaluate request (rerank-questions names skills)) :answers))
                 (chosen (and answers (answer-choice (answer-at answers "which")))))
            (when (and chosen (not (equal chosen +none+)) (member chosen names :test #'equal))
              (let ((fits (answer-probability (answer-at answers (format nil "fits::~a" chosen)))))
                (when (and fits (>= fits (threshold :fits)))
                  (list :skill chosen
                        :gate (getf ranked :gate)
                        :fits fits
                        :probability (choice-probability answers chosen)
                        :calls (1+ (getf ranked :calls))))))))))))
