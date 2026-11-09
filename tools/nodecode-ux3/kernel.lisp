;;;; kernel.lisp --- the UX3 decision kernel: route, evaluate, reduce.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's scripts/jev_gate.py and scripts/check_review.py, as
;;;; functions. Nothing here reads a file, a clock or the network: a verb takes
;;;; text and answers a plist, so a test can hold it to its own tables.
;;;;
;;;; Two of the original's rules are the whole of it. A review is three lanes
;;;; and never their average: one stop outranks any number of continues. And
;;;; the weakest flow is chosen deterministically, by severity, then the lowest
;;;; evidence tier, then evidence, user, business -- so two people reading the
;;;; same review reach the same next step.

(in-package #:nodecode-ux3)

;;; --- the vocabulary ---------------------------------------------------------

(defparameter +verdicts+ '("continue" "verify" "stop_reframe")
  "The three verdicts, weakest first. A lane's verdict is never averaged with another's.")

(defparameter +tiers+ '("t0" "t1" "t2" "t3" "t4")
  "Evidence tiers, weakest first: the string order is the strength order.")

(defparameter +tier-notes+
  '(("t0" . "internal belief, invented actor or generic trend, no traceable source")
    ("t1" . "a stated opinion, an isolated quote or a public signal")
    ("t2" . "a contextual interview, an observed workaround or a small test")
    ("t3" . "repeated behaviour, adoption, payment, retention or product logs")
    ("t4" . "durable retention, workflow dependency, sustainable economics or governance proof"))
  "What each tier is, in the original's words (its docs/UNCERTAINTY.md).")

(defparameter +next-steps+
  '(("continue" . "write a context pack inside the execution boundary: continue plus a valid pack is the one branch that authorizes implementation")
    ("verify" . "run the one proof step and nothing else; verify never authorizes general coding")
    ("stop_reframe" . "stop and return one better product question"))
  "The next step each verdict asks for (the original's Outcome table).")

(defparameter +severity+ '(("continue" . 0) ("verify" . 1) ("stop_reframe" . 2))
  "A lane's weight. stop_reframe is 2 so one stop outranks any number of continues.")

(defparameter +flow-priority+ '(("evidence" . 0) ("user" . 1) ("business" . 2))
  "The tie-break after severity and tier: evidence, then user, then business.")

(defparameter +flow-labelled+ '(("user" . "user_flow") ("evidence" . "evidence_flow")
                                 ("business" . "business_flow"))
  "How a flow is written in a review and in the report.")

;;; The original's keyword tables, unchanged (scripts/jev_gate.py:11-20): the
;;; baseline is a fast reading of one prompt, and it says so.

(defparameter +routine-keywords+
  '("typo" "css" "style" "format" "lint" "comment" "readme" "docs" "rename"
    "cleanup" "refactor test" "fix test"))

(defparameter +high-risk-keywords+
  '("drop table" "alter table" "migration" "secret" "auth" "payment" "billing"
    "token" "privacy" "delete user" "external api" "webhook" "telemetry" "export data"))

(defparameter +hedge-keywords+ '("maybe" "i think" "someone said")
  "A prompt that hedges its own evidence.")

(defparameter +business-risk-keywords+ '("bypass auth" "disable security" "free tier exploit"))
(defparameter +business-paid-keywords+ '("billing" "payment" "paid"))
(defparameter +user-universal-keywords+ '("everyone" "anyone" "all users"))

;;; --- reading the vocabulary -------------------------------------------------

(defun head (items count)
  "The first COUNT of ITEMS, or the whole of it."
  (if (> (length items) count) (subseq items 0 count) items))

(defun keyword-hits (text keywords)
  "The entries of KEYWORDS TEXT carries as substrings, in table order."
  (remove-if-not (lambda (keyword) (search keyword text)) keywords))

(defun prompt-text (prompt verb)
  "PROMPT as non-empty text, or a refusal naming VERB."
  (cond ((not (stringp prompt)) (fail "~a takes the prompt as a string; it read ~s" verb prompt))
        ((zerop (length (nlk:trimmed prompt)))
         (fail "~a needs a prompt: the direction, feature or experiment to judge" verb))
        (t prompt)))

(defun check-tier (tier &optional (verb "the gate"))
  "TIER as one of +TIERS+, lowercased, or a refusal naming VERB."
  (let ((name (and (stringp tier) (string-downcase (nlk:trimmed tier)))))
    (or (find name +tiers+ :test #'string=)
        (fail "~a reads an evidence tier as one of ~{~a~^, ~}; it read ~s" verb +tiers+ tier))))

(defun check-verdict (verdict &optional (verb "the gate"))
  "VERDICT as one of +VERDICTS+, lowercased, or a refusal naming VERB."
  (let ((name (and (stringp verdict) (string-downcase (nlk:trimmed verdict)))))
    (or (find name +verdicts+ :test #'string=)
        (fail "~a reads a verdict as one of ~{~a~^, ~}; it read ~s" verb +verdicts+ verdict))))

(defun severity (verdict)
  "How severe VERDICT is: 0 continue, 1 verify, 2 stop_reframe."
  (or (cdr (assoc verdict +severity+ :test #'string=))
      (fail "~s is not a verdict" verdict)))

(defun flow-name (flow)
  "The canonical flow a caller's spelling names: user, evidence or business.
Accepts a keyword, a symbol or a string, and the _flow spelling."
  (let* ((raw (cond ((symbolp flow) (symbol-name flow))
                    ((stringp flow) flow)
                    (t (fail "a lane names its flow as user, evidence or business; it read ~s" flow))))
         (name (string-downcase (nlk:trimmed raw)))
         (base (if (and (> (length name) 5) (string= "_flow" name :start2 (- (length name) 5)))
                   (subseq name 0 (- (length name) 5))
                   name)))
    (or (car (find base +flow-priority+ :key #'car :test #'string=))
        (fail "a lane names its flow as user, evidence or business; it read ~s" flow))))

(defun flow-label (flow)
  "FLOW as a review writes it: user_flow, evidence_flow, business_flow."
  (or (cdr (assoc flow +flow-labelled+ :test #'string=)) flow))

(defun worst-verdict (verdicts)
  "The most severe of VERDICTS: one stop outranks any number of continues."
  (let ((worst "continue"))
    (dolist (verdict verdicts worst)
      (when (> (severity verdict) (severity worst)) (setf worst verdict)))))

;;; --- a lane ---------------------------------------------------------------

(defun lane-flow (lane) (first lane))
(defun lane-verdict (lane) (second lane))
(defun lane-tier (lane) (third lane))

(defun lane-priority (lane)
  "FLOW's fixed place in the tie-break."
  (or (cdr (assoc (lane-flow lane) +flow-priority+ :test #'string=)) 9))

(defun lane-before-p (a b)
  "Whether lane A is the weaker of the two, by the deterministic selection:
most severe verdict, then the lowest evidence tier, then evidence, user,
business (scripts/check_review.py:22-31)."
  (let ((sa (severity (lane-verdict a))) (sb (severity (lane-verdict b))))
    (cond ((/= sa sb) (> sa sb))
          ((not (string= (lane-tier a) (lane-tier b))) (string< (lane-tier a) (lane-tier b)))
          (t (< (lane-priority a) (lane-priority b))))))

(defun weakest-lane (lanes)
  "The lane the whole review answers to."
  (first (sort (copy-list lanes) #'lane-before-p)))

(defun headline-tier (lanes)
  "The weakest tier any lane brings: a headline never claims more than its weakest lane."
  (reduce (lambda (a b) (if (string< a b) a b)) (mapcar #'lane-tier lanes)))

(defun parse-lane (lane position)
  "LANE, what a caller wrote, as (FLOW VERDICT TIER): (:evidence \"verify\" \"t2\"),
a list of three."
  (unless (and (listp lane) (= 3 (length lane)))
    (fail "lane ~d reads as (flow verdict evidence-tier), e.g. (:evidence \"verify\" \"t2\"); it read ~s"
          position lane))
  (list (flow-name (first lane))
        (check-verdict (second lane) (format nil "lane ~d" position))
        (check-tier (third lane) (format nil "lane ~d" position))))

;;; --- route ----------------------------------------------------------------

(defun route-mode (route)
  "The smallest responsible mode ROUTE asks for: routine work is a quick gate,
anything else a standard one, and a council is the operator's call."
  (if (string= route "routine") "quick" "standard"))

(define-verb route (prompt &key files-changed)
  "Where one task prompt routes in the original's baseline (scripts/jev_gate.py:23):
\"commitment\", \"routine\" or \"discovery\", with why, the mode it asks for and
the review requirement. Advisory: it says which review to run, not what to build."
  (when (and files-changed (or (not (integerp files-changed)) (minusp files-changed)))
    (fail "files_changed is a nonnegative integer; it read ~s" files-changed))
  (let* ((text (prompt-text prompt "ux3:route"))
         (lower (string-downcase text))
         (risky (keyword-hits lower +high-risk-keywords+))
         (routine (keyword-hits lower +routine-keywords+)))
    (multiple-value-bind (name why)
        (cond ((and files-changed (> files-changed 5))
               (values "commitment"
                       (format nil "~d files changed is past the five the baseline treats as a commitment"
                               files-changed)))
              (risky
               (values "commitment"
                       (format nil "the prompt names ~{~s~^, ~}: high risk, so it commits" (head risky 3))))
              (routine
               (values "routine"
                       (format nil "the prompt reads as routine work (~{~s~^, ~})" (head routine 3))))
              (t (values "discovery"
                         "nothing in the prompt is routine or high risk; the baseline reads it as discovery")))
      (list :route name :bypass nil :review-required t :mode (route-mode name) :why why))))

;;; --- evaluate -------------------------------------------------------------

(define-verb evaluate (prompt &key evidence-tier files-changed)
  "The original's three-lane baseline (scripts/jev_gate.py:44) over one prompt:
the route, each lane's verdict, one combined verdict, the weakest flow and the
next step. Advisory, and it says what it read and why."
  (let* ((text (prompt-text prompt "ux3:evaluate"))
         (lower (string-downcase text))
         (routed (route text :files-changed files-changed))
         (tier (check-tier (or evidence-tier (setting :evidence-tier)) "ux3:evaluate"))
         (hedges (keyword-hits lower +hedge-keywords+))
         (risky-business (keyword-hits lower +business-risk-keywords+))
         (paid (keyword-hits lower +business-paid-keywords+))
         (universal (keyword-hits lower +user-universal-keywords+))
         (notes '()))
    (flet ((note (format-control &rest args)
             (push (apply #'format nil format-control args) notes)))
      ;; a prompt that hedges its own evidence is tier t0 however it was graded
      (when hedges
        (setf tier "t0")
        (note "evidence: the prompt hedges (~{~s~^, ~}), so the lane reads as t0" (head hedges 3)))
      (let* ((evidence (cond ((string= tier "t0") "stop_reframe")
                             ((string= tier "t1") "verify")
                             (t "continue")))
             (business (cond (risky-business "stop_reframe")
                             (paid "verify")
                             (t "continue")))
             (user (if (and universal (search "commitment" lower)) "verify" "continue")))
        (case (severity evidence)
          (2 (note "evidence: tier t0 is internal belief; nothing observed yet can authorize action"))
          (1 (note "evidence: tier t1 supports at most a verify"))
          (t (note "evidence: tier ~a carries the lane (~a)" tier
                   (or (cdr (assoc tier +tier-notes+ :test #'string=)) ""))))
        (when risky-business
          (note "business: the prompt asks for a risk shortcut (~{~s~^, ~})" (head risky-business 3)))
        (when (and paid (null risky-business))
          (note "business: money changes hands (~{~s~^, ~}), so the exchange itself needs a verify"
                (head paid 3)))
        (when (string= user "verify")
          (note "user: ~{~s~^, ~} over a commitment is an untested claim" (head universal 3)))
        (let* ((lanes (list (list "user" user tier)
                            (list "evidence" evidence tier)
                            (list "business" business tier)))
               (combined (worst-verdict (mapcar #'lane-verdict lanes)))
               (weakest (flow-label (lane-flow (weakest-lane lanes)))))
          (list :route (getf routed :route)
                :mode (getf routed :mode)
                :review-required (getf routed :review-required)
                :advisory t
                :evidence-tier tier
                :tier-note (cdr (assoc tier +tier-notes+ :test #'string=))
                :lanes lanes
                :combined-verdict combined
                :weakest-flow weakest
                :next-step (cdr (assoc combined +next-steps+ :test #'string=))
                :notes (nreverse notes)
                :why (getf routed :why)
                :language (setting :working-language)))))))

;;; --- decide ---------------------------------------------------------------

(defun boundary-contradictions (may-do must-not-do)
  "The entries both lists carry, compared without case or surrounding space."
  (let ((allowed (mapcar (lambda (entry) (string-downcase (nlk:trimmed entry)))
                          (remove-if-not #'stringp may-do)))
        (forbidden (mapcar (lambda (entry) (string-downcase (nlk:trimmed entry)))
                           (remove-if-not #'stringp must-not-do))))
    (sort (remove-duplicates (intersection allowed forbidden :test #'string=) :test #'string=)
          #'string<)))

(define-verb decide (lanes &key may-do must-not-do)
  "Reduce three lane reviews to the one canonical verdict (scripts/check_review.py:59).
LANES is three (FLOW VERDICT TIER) lists, one per flow, each what that lane's
reviewer found. Answers the combined verdict, the weakest flow, the headline
tier, the next step, the lanes read back, and every problem the original's
validator would raise -- so a review is repaired before it authorizes anything."
  (let* ((parsed (loop for lane in lanes for position from 1 collect (parse-lane lane position)))
         (seen (mapcar #'lane-flow parsed))
         (missing (remove-if (lambda (flow) (member flow seen :test #'string=))
                             (list "evidence" "user" "business")))
         (twice (remove-duplicates
                 (remove-if-not (lambda (flow) (> (count flow seen :test #'string=) 1)) seen)
                 :test #'string=)))
    (when missing
      (fail "a review needs all three lanes; this one has no ~{~a~^, ~}"
            (mapcar #'flow-label missing)))
    (when twice
      (fail "a review names ~{~a~^, ~} twice; a lane is one verdict"
            (mapcar #'flow-label twice)))
    (let* ((combined (worst-verdict (mapcar #'lane-verdict parsed)))
           (weakest (flow-label (lane-flow (weakest-lane parsed))))
           (tier (headline-tier parsed))
           (problems '()))
      (when (and (string= tier "t0") (not (string= combined "stop_reframe")))
        (push "the headline tier t0 forces stop_reframe: internal belief with no traceable source cannot authorize anything else"
              problems))
      (when (and (string= tier "t1") (string= combined "continue"))
        (push "the headline tier t1 can never return continue: a stated opinion or an isolated signal supports at most a verify"
              problems))
      (let ((contradictions (boundary-contradictions may-do must-not-do)))
        (when contradictions
          (push (format nil "the execution boundary contradicts itself: ~{~s~^, ~} is in both may_do and must_not_do"
                        contradictions)
                problems)))
      (list :combined-verdict combined
            :weakest-flow weakest
            :evidence-tier tier
            :tier-note (cdr (assoc tier +tier-notes+ :test #'string=))
            :next-step (cdr (assoc combined +next-steps+ :test #'string=))
            :authorized (and (string= combined "continue") (null problems))
            :lanes parsed
            :problems (nreverse problems)))))

;;; --- prose ----------------------------------------------------------------

(defun lanes-text (stream lanes)
  "One line per lane: its name, verdict and tier."
  (dolist (lane lanes)
    (format stream "  ~a~8t~a~22t~a~%" (flow-label (first lane)) (second lane) (third lane))))

(defun gate-text (&optional evaluation)
  "EVALUATION, the plist ux3:evaluate answers, as prose a person and a model
both read."
  (unless (and (listp evaluation) (getf evaluation :lanes))
    (fail "gate-text takes the plist ux3:evaluate answers; it read ~s" evaluation))
  (with-output-to-string (out)
    (format out "UX3 gate: a ~a route, ~a gate (advisory: it bounds the next step, it does not build)~%"
            (getf evaluation :route) (getf evaluation :mode))
    (format out "~%")
    (lanes-text out (getf evaluation :lanes))
    (format out "~%headline tier ~a (~a)~%" (getf evaluation :evidence-tier)
            (or (getf evaluation :tier-note) ""))
    (format out "combined ~a, from the weakest lane (~a); verdicts are never averaged~%"
            (getf evaluation :combined-verdict) (getf evaluation :weakest-flow))
    (format out "next: ~a~%" (getf evaluation :next-step))
    (when (getf evaluation :notes)
      (format out "~%why:~%~{  - ~a~%~}" (getf evaluation :notes)))
    (format out "~%route: ~a~%" (getf evaluation :why))
    (format out "prose in ~a; flow, verdict and tier names stay canonical~%"
            (or (getf evaluation :language) "en"))))

(defun review-text (decision)
  "DECISION, the plist ux3:decide answers, as prose."
  (unless (and (listp decision) (getf decision :lanes))
    (fail "review-text takes the plist ux3:decide answers; it read ~s" decision))
  (with-output-to-string (out)
    (format out "UX3 review: ~a~%" (getf decision :combined-verdict))
    (lanes-text out (getf decision :lanes))
    (format out "~%headline tier ~a (~a); weakest flow ~a~%"
            (getf decision :evidence-tier) (or (getf decision :tier-note) "")
            (getf decision :weakest-flow))
    (format out "next: ~a~%" (getf decision :next-step))
    (format out "~a~%" (if (getf decision :authorized)
                             "the review authorizes implementation inside its execution boundary"
                             "the review authorizes nothing yet"))
    (when (getf decision :problems)
      (format out "~%problems:~%~{  - ~a~%~}" (getf decision :problems)))))
