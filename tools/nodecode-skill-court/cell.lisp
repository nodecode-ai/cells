;;;; cell.lisp --- the section, the four tools, /skill-court, and the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original puts every skill the agent wrote on trial: the same tasks
;;;; are run once with the skill and once without it, a blind judge or a
;;;; case's own check says which run won, and the skills that make a model
;;;; worse are hidden from it. This cell is the court's record and its
;;;; arithmetic: it keeps a skill's cases and the pairs of runs judged on
;;;; them, scores the evidence the way the original's scoring.py does, and
;;;; keeps the quarantine list. The runs themselves are the caller's -- the
;;;; agent in the session, or an operator from /skill-court -- which is what
;;;; replaces the original's Docker replay profile.

(in-package #:nodecode-skill-court)

;;; --- reading the section ------------------------------------------------------

(defun margin-value ()
  "The section's margin as a number; a quarter when it does not read as one."
  (let* ((*read-eval* nil)
         (value (ignore-errors (read-from-string (setting :margin)))))
    (if (and (realp value) (plusp value)) (coerce value 'double-float) 0.25)))

(defun notice-key (name)
  "The standing notice a quarantined skill speaks under."
  (format nil "nodecode-skill-court/~a" name))

(defun announce (name note)
  "Stand a notice naming the quarantined skill, so the model reads it."
  (when (setting :notify)
    (nle:notice (format nil "skill-court: ~a is quarantined~@[ -- ~a~]" name note)
                :level :warning :key (notice-key name))))

(defun hush (name)
  "Clear a quarantined skill's notice."
  (nle:notice nil :key (notice-key name)))

;;; --- one line per skill -------------------------------------------------------

(defun interval-text (result)
  "The success interval of RESULT, as a line, or an empty string."
  (if (getf result :ci)
      (format nil "95% CI ~@d%..~@d%"
              (percent (car (getf result :ci))) (percent (cdr (getf result :ci))))
      "no interval"))

(defun token-text (result)
  "The token change of RESULT, as a line, or an empty string."
  (if (getf result :token-change)
      (format nil "tokens ~a~@[ (95% ~@d%..~@d%)~]"
              (signed-percent (getf result :token-change))
              (when (getf result :token-ci)
                (percent (car (getf result :token-ci))))
              (when (getf result :token-ci)
                (percent (cdr (getf result :token-ci)))))
      "no token counts"))

(defun case-line (case)
  "One case as the line a report and /skill-court print."
  (let ((check (getf case :check)))
    (format nil "  ~a. ~a~@[ (pass when: ~a)~]"
            (getf case :id) (getf case :prompt)
            (and (stringp check) (plusp (length check)) check))))

(defun verdict-line (name result)
  "One skill's verdict as the line a report and /skill-court print."
  (if result
      (format nil "~a: ~a -- ~a (~d case~:p, ~d pair~:p; ~a; ~a)~@[ [quarantined]~]"
              name (getf result :verdict) (getf result :reason)
              (getf result :tasks) (getf result :pairs)
              (interval-text result) (token-text result)
              (quarantined name))
      (format nil "~a: no evidence yet~@[ [quarantined]~]" name (quarantined name))))

(defun standing ()
  "Every skill's verdict, the best first: what the original's report prints."
  (let ((rows (loop for name in (skill-names)
                    for result = (court-score name)
                    collect (cons name result))))
    (sort rows (lambda (a b)
                 (let ((left (getf (cdr a) :delta))
                       (right (getf (cdr b) :delta)))
                   (cond ((and left right (/= left right)) (> left right))
                         (left (not right))
                         ((and (null left) right) nil)
                         (t (string< (car a) (car b)))))))))

(defun report-text ()
  "The leaderboard: one line per skill, the best first."
  (let ((rows (standing)))
    (if rows
        (format nil "skill-court: ~d skill~:p~%~{~a~^~%\~}"
                (length rows)
                (mapcar (lambda (row) (verdict-line (car row) (cdr row))) rows))
        "skill-court: no skill has evidence yet -- add a case with skill_court_case")))

(defun one-skill-text (name)
  "One skill's verdict, its cases, and why it is quarantined, if it is."
  (let ((result (court-score name))
        (cases (court-cases name))
        (entry (quarantined name)))
    (format nil "~a~%~{~a~^~%\~}cases: ~d~@[~%~a~]"
            (verdict-line name result)
            (when entry
              (list (format nil "quarantined ~a: ~a"
                            (getf (cdr entry) :at) (or (getf (cdr entry) :note) "no note"))))
            (length cases)
            (when cases
              (format nil "~{~a~^~%\~}" (mapcar #'case-line cases))))))

;;; --- what the tools do --------------------------------------------------------

(defun clip (text limit)
  "TEXT cut to LIMIT characters, with an ellipsis when it was longer."
  (if (> (length text) limit)
      (format nil "~a..." (subseq text 0 (- limit 3)))
      text))

(defun case-record (skill prompt)
  "The record SKILL_COURT_CASE takes, for a caller inside this image."
  (nlk:contract-decode (nlk:contract 'court-case)
                        (nlk:make-json-object "skill" skill "prompt" prompt "check" "")))

(defun quarantine-record (skill &key (action "quarantine") (note ""))
  "The record SKILL_COURT_QUARANTINE takes, for a caller inside this image."
  (nlk:contract-decode (nlk:contract 'court-quarantine)
                        (nlk:make-json-object "skill" skill "action" action "note" note)))

(defun add-case (args)
  "The skill_court_case tool: add one case to a skill's court."
  (let ((skill (court-case-skill args))
        (prompt (court-case-prompt args))
        (check (or (court-case-check args) "")))
    (with-state
      (let* ((record (ensure-skill skill))
             (cases (getf record :cases))
             (id (format nil "~d" (1+ (length cases)))))
        (setf (getf record :cases)
              (append cases (list (list :id id :prompt prompt :check check :at (now-text)))))
        (save-state)
        (format nil "skill-court: case ~a on ~a: ~a (~d case~:p now)"
                id skill (clip prompt 70) (1+ (length cases)))))))

(defun record-pair (args)
  "The skill_court_record tool: one case's outcome in both arms."
  (let ((skill (court-record-skill args))
        (case (court-record-case-id args))
        (with (if (court-record-with-success args) :pass :fail))
        (without (if (court-record-without-success args) :pass :fail))
        (with-tokens (or (court-record-with-tokens args) 0))
        (without-tokens (or (court-record-without-tokens args) 0)))
    (with-state
      (let* ((record (ensure-skill skill))
             (cases (getf record :cases))
             (pairs (getf record :pairs)))
        (unless (find case cases :key (lambda (row) (getf row :id)) :test #'equal)
          (return-from record-pair
            (format nil "skill-court: ~a has no case ~a -- add it with skill_court_case"
                    skill case)))
        (setf (getf record :pairs)
              (append pairs (list (list :case case :with with :without without
                                        :with-tokens with-tokens
                                        :without-tokens without-tokens :at (now-text)))))
        (save-state)
        (format nil "skill-court: ~a case ~a -- with ~a, without ~a (~d pair~:p, ~d case~:p)\n~a"
                skill case (string-downcase (symbol-name with))
                (string-downcase (symbol-name without))
                (1+ (length pairs)) (length cases)
                (verdict-line skill (court-score skill)))))))

(defun report-tool (args)
  "The skill_court_report tool: one skill's verdict, or the leaderboard."
  (let ((skill (court-report-skill args)))
    (with-state
      (if (and skill (plusp (length skill)))
          (one-skill-text skill)
          (report-text)))))

(defun quarantine-tool (args)
  "The skill_court_quarantine tool: put a skill on the list, take it off,
or list what is on it."
  (let ((skill (court-quarantine-skill args))
        (action (or (court-quarantine-action args) "quarantine"))
        (note (or (court-quarantine-note args) "")))
    (with-state
      (cond
        ((string-equal action "list")
         (let ((entries (getf *state* :quarantine)))
           (if entries
               (format nil "skill-court, quarantined:~%~{~a~^~%\~}"
                       (mapcar (lambda (entry)
                                 (format nil "  ~a -- ~a (~a)"
                                         (car entry) (or (getf (cdr entry) :note) "no note")
                                         (getf (cdr entry) :at)))
                               entries))
               "skill-court: nothing is quarantined")))
        ((string-equal action "release")
         (if (release-skill skill)
             (progn (hush skill)
                    (format nil "skill-court: ~a is visible again" skill))
             (format nil "skill-court: ~a was not quarantined" skill)))
        ((string-equal action "quarantine")
         (quarantine-skill skill note)
         (announce skill note)
         (let ((result (court-score skill)))
           (format nil "skill-court: ~a is quarantined~@[ -- ~a~]~@[~%~a~]"
                   skill (and (plusp (length note)) note)
                   (and result (verdict-line skill result)))))
        (t (format nil "skill-court: ~s is not quarantine, release or list" action))))))

;;; --- the model's side --------------------------------------------------------

(nlk:define-contract court-case
  (:doc "Add one case to a skill's court: the task the two runs are given, once with the skill and once without it. A skill needs at least min_tasks cases judged in both arms before it can be helped or hurt. Write the task the way a user would ask it, and say in check what makes a run pass when reading the answer is not enough.")
  ("skill" :text :required t :doc "the skill's name, as the index lists it")
  ("prompt" :text :required t :doc "the task, as the user would ask it")
  ("check" :string :doc "how a run is judged a pass, when it is not the reading of the answer"))

(nlk:define-contract court-record
  (:doc "Record what one case did in both arms: the run with the skill, and the run without it. Call it once per pair, after you have run the case both ways; pass the tokens each run spent when you know them, because a skill that costs a quarter more without lifting success is a skill that hurts.")
  ("skill" :text :required t :doc "the skill's name")
  ("case" :text :required t :slot case-id :doc "the case id skill_court_case answered with")
  ("with_success" :boolean :required t :doc "did the run with the skill pass")
  ("without_success" :boolean :required t :doc "did the run without it pass")
  ("with_tokens" :integer :default 0 :doc "tokens the run with the skill spent, when counted")
  ("without_tokens" :integer :default 0 :doc "tokens the run without it spent, when counted"))

(nlk:define-contract court-report
  (:doc "The court's verdict on the evidence it holds: helps, hurts, neutral, or inconclusive with how many more cases it wants. With a skill, that skill's line, its interval, its cases and its quarantine; without one, the leaderboard, best first.")
  ("skill" :string :doc "one skill's verdict; without it, every skill's"))

(nlk:define-contract court-quarantine
  (:doc "Put a skill on the quarantine list, so the model is told which skill is out until it is fixed, or take it off, or list what is on it. Quarantine never edits the skill: it is one line the court keeps and stands as a notice.")
  ("skill" :text :required t :doc "the skill's name")
  ("action" :text :default "quarantine" :options '("quarantine" "release" "list")
            :doc "what to do with it")
  ("note" :string :doc "one line on why, kept with the record"))

;;; --- the operator's side ------------------------------------------------------

(defun split-word (text)
  "TEXT as its first word and the rest, trimmed; an empty text answers two
empty strings."
  (let* ((text (nlk:trimmed text))
         (space (position #\Space text)))
    (if space
        (values (subseq text 0 space) (nlk:trimmed (subseq text (1+ space))))
        (values text ""))))

(defun status-text ()
  "The court in one line: what it holds and how it decides."
  (let ((skills (skill-names)))
    (format nil "skill-court: ~d skill~:p, ~d case~:p, ~d pair~:p, ~d quarantined; min_tasks ~d, margin ~a, resamples ~d"
            (length skills)
            (loop for name in skills sum (length (court-cases name)))
            (loop for name in skills sum (length (court-pairs name)))
            (length (getf *state* :quarantine))
            (setting :min-tasks) (setting :margin) (setting :resamples))))

(defun run-slash (args session-id)
  "/skill-court typed alone answers status; with a word after it, one of
list, report NAME, cases NAME, task NAME TEXT, quarantine NAME [NOTE],
release NAME or reset NAME."
  (declare (ignore session-id))
  (handler-case
      (progn
        (ensure-state-loaded)
        (multiple-value-bind (head rest) (split-word (or args ""))
          (cond
            ((or (zerop (length head)) (string-equal head "status")) (status-text))
            ((string-equal head "list") (report-text))
            ((string-equal head "help")
             "skill-court: /skill-court status|list|report NAME|cases NAME|task NAME TEXT|quarantine NAME [NOTE]|release NAME|reset NAME")
            ((string-equal head "report")
             (if (plusp (length rest)) (one-skill-text rest) (report-text)))
            ((string-equal head "cases")
             (let ((cases (court-cases rest)))
               (if cases
                   (format nil "~a: ~d case~:p~%~{~a~^~%\~}"
                           rest (length cases) (mapcar #'case-line cases))
                   (format nil "skill-court: ~a has no cases" rest))))
            ((string-equal head "task")
             (multiple-value-bind (skill prompt) (split-word rest)
               (if (plusp (length skill))
                   (add-case (case-record skill prompt))
                   "skill-court: /skill-court task NAME TEXT")))
            ((string-equal head "quarantine")
             (multiple-value-bind (skill note) (split-word rest)
               (if (plusp (length skill))
                   (quarantine-tool (quarantine-record skill :note note))
                   "skill-court: /skill-court quarantine NAME [NOTE]")))
            ((string-equal head "release")
             (if (plusp (length rest))
                 (quarantine-tool (quarantine-record rest :action "release"))
                 "skill-court: /skill-court release NAME"))
            ((string-equal head "reset")
             (if (skill-record rest)
                 (progn
                   (setf (getf *state* :skills)
                         (remove rest (getf *state* :skills) :key #'car :test #'equal))
                   (save-state)
                   (format nil "skill-court: ~a's evidence is gone" rest))
                 (format nil "skill-court: ~a has no evidence" rest)))
            (t (one-skill-text head)))))
    (error (condition) (format nil "skill-court: ~a" condition))))

;;; --- the manual --------------------------------------------------------------

(defparameter +manual+
  "Skill Court asks whether a skill the agent wrote is worth what it costs. The court runs
nothing itself: it keeps a skill's cases, records what each case did with the skill and
without it, and scores the evidence the way the original does.
  skill_court_case      add one case (the task, and how a run passes)
  skill_court_record    one case's outcome in both arms, with the tokens each spent
  skill_court_report    helps, hurts, neutral or inconclusive, with its 95% interval
  skill_court_quarantine  put a skill out of service, take it back, or list the ones out
A pair is one case run twice. Repeated pairs of one case are not independent evidence: they
are averaged per case first, and every interval is a bootstrap over cases with a fixed seed,
so a report reads the same twice. A verdict needs min_tasks cases judged in both arms and
its whole 95% interval on one side; with three that means every case moved the same way.
A skill that costs more than `margin' in tokens without lifting success hurts; one that
lifts success and costs at least a fifth less helps. Quarantine never edits the skill: it
stands a warning notice naming it, for the model and for the operator, until it is released.
  /skill-court status                 what the court holds and how it decides
  /skill-court list                   every skill's verdict, best first
  /skill-court report NAME            one skill, its interval, its cases, its quarantine
  /skill-court cases NAME             the cases a skill is tried on
  /skill-court task NAME TEXT         add a case from the operator's side
  /skill-court quarantine NAME [NOTE] one skill out of service
  /skill-court release NAME           and back
  /skill-court reset NAME             forget a skill's evidence
The record is one file under the home, skill-court/state.sexp, written atomically: the cases,
the pairs and the quarantine list, and no word of a conversation. Configure the section
`skill-court' in ~/.nodecode/config.jsonc; every member is optional."
  "What (help :skill-court) answers while the cell runs.")

(nle:define-cell skill-court
  (:section ("skill-court")
    (:guide "every member is optional: min_tasks is the fewest cases judged in both arms before a verdict; margin is the relative token cost that counts as a difference; notify stands a notice naming each quarantined skill")
    ("min_tasks" :integer :default 3 :min 1
     :doc "cases with outcomes in both arms before a skill can be helped or hurt")
    ("margin" :string :default "0.25"
     :doc "relative token cost that counts as a difference; 0.25 is a quarter more")
    ("resamples" :integer :default 2000 :min 100
     :doc "bootstrap resamples behind every interval")
    ("seed" :integer :default 1
     :doc "the bootstrap's seed, so a report reads the same twice")
    ("notify" :boolean :default t
     :doc "stand a notice naming each quarantined skill"))
  (:help :skill-court
   "skill-court: put a written skill on trial -- cases, paired runs, a 95% verdict and a quarantine list; /skill-court for status and reports"
   +manual+)
  (:tool "skill_court_case" (nlk:contract 'court-case) 'add-case)
  (:tool "skill_court_record" (nlk:contract 'court-record) 'record-pair)
  (:tool "skill_court_report" (nlk:contract 'court-report) 'report-tool)
  (:tool "skill_court_quarantine" (nlk:contract 'court-quarantine) 'quarantine-tool)
  (:command "skill-court" 'run-slash
            :description "Skill Court: what a skill's evidence says, and the quarantine list"
            :argument-hint "status|list|report NAME|cases NAME|task NAME TEXT|quarantine NAME|release NAME|reset NAME")
  (:start (lambda ()
            (with-state
              (setf *state* (load-state))
              (dolist (entry (getf *state* :quarantine))
                (announce (car entry) (or (getf (cdr entry) :note) ""))))
            (nle:on-stop
             (lambda ()
               (dolist (entry (getf *state* :quarantine))
                 (hush (car entry))))))))
