;;;; cell-test.lisp --- the gates, the quote check, the committed verdict, the plan's numbers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing leaves the machine: a claim's page is a string bound to *FETCH*. What is
;;;; proved is the part the original enforced in code -- a check mark needs a quote that
;;;; is in the page, a phase does not end without its artifact, a verdict commits once and
;;;; changes only with newer evidence, and a plan's effort and calendar are the code's.

(in-package #:nodecode.test)

(define-test-slice "blueprint" "BLUEPRINT-CELL-" :start nodecode-blueprint:start-cell)

;;; --- fixtures ----------------------------------------------------------------------

(defparameter *pages* nil "URL -> page text, the pages the stub serves.")

(defun stub-fetch (url)
  (let ((page (cdr (assoc url *pages* :test #'equal))))
    (values page (if page 200 404))))

(defmacro with-pages ((&rest pairs) &body body)
  "BODY over a stubbed page reader: PAIRS of url and page text."
  `(let ((*pages* (list ,@(loop for (url page) on pairs by #'cddr collect `(cons ,url ,page)))))
     (let ((nodecode-blueprint::*fetch* #'stub-fetch)) ,@body)))

(defmacro with-blueprint ((&rest pairs) &body body)
  "BODY over a fresh home, the cell started on a literal section, stopped on unwind."
  `(with-temp-store ()
     (with-cell-stop ((blueprint-start ,@pairs)) ,@body)))

(defun call (function &rest pairs)
  "FUNCTION called with PAIRS as the model's JSON arguments."
  (funcall function (apply #'nlk:make-json-object pairs)))

(defun blueprint-page (title body)
  (format nil "<html><head><title>~a</title></head><body><article>~a</article></body></html>" title body))

(defparameter *club-page*
  (blueprint-page "Padel booking"
                  "Small padel clubs take bookings on WhatsApp. A club of 200 members takes about 40 bookings a week on the phone, and two clubs reported losing a court a week to a double booking."))

(defparameter *directory-page*
  (blueprint-page "Padel directories"
                  "There are eleven booking tools listed for padel clubs in the directory, and every one of them asks the club for a monthly fee."))

(defun blueprint-setting (key) (nodecode-blueprint::setting key))

(defun started ()
  "A project with a brief and a confirmed profile, in phase 1."
  (call #'blueprint:phase-tool "action" "start" "idea" "a WhatsApp bot that books padel courts")
  (call #'blueprint:phase-tool "action" "record" "kind" "brief" "content" "for small clubs; double bookings")
  (call #'blueprint:profile-tool "action" "set"
        "profile" (nlk:make-json-object "team" "solo" "agents" (vector "claude-code")
                                          "hours_per_day" 4))
  (call #'blueprint:profile-tool "action" "confirm"))

(defun at-phase (n)
  (nodecode-blueprint::set-phase (nodecode-blueprint::current-project) n))

(defun verdict-args ()
  (list "action" "commit"
        "verdict" "go_with_conditions"
        "thesis" "clubs will pay for a WhatsApp booking bot"
        "reference_class" (nlk:make-json-object "name" "Chrome extensions"
                                                 "base_rate" "87% under a thousand installs"
                                                 "source" "chrome_extensions")
        "p_success" 0.4
        "premortem" (vector (nlk:make-json-object "cause" "clubs do not answer" "p_given_failure" 0.5)
                             (nlk:make-json-object "cause" "WhatsApp bans the number" "p_given_failure" 0.3)
                             (nlk:make-json-object "cause" "nobody pays" "p_given_failure" 0.2))
        "must_be_true" (vector (nlk:make-json-object "assumption" "clubs answer in a day" "test" "ask five"
                                                       "cost" "one day" "deadline" "2027-01-31"
                                                       "kill_threshold" "fewer than two answers"))
        "tripwires" (vector (nlk:make-json-object "metric" "paying clubs" "threshold" "one"
                                                    "by" "2027-03-01"))
        "builder_fit" "solo, four hours a day"
        "estimate_realism" "two weeks, mostly the trial"
        "predictions" (vector (nlk:make-json-object "statement" "five clubs answer" "p" 0.6
                                                     "resolve_by" "2027-02-01" "how" "the reply log")
                              (nlk:make-json-object "statement" "one club pays" "p" 0.35
                                                     "resolve_by" "2027-04-01" "how" "the invoice")
                              (nlk:make-json-object "statement" "ten courts are booked" "p" 0.3
                                                     "resolve_by" "2027-05-01" "how" "the log"))))

(defun commit-verdict () (apply #'call #'blueprint:verdict-tool (verdict-args)))

(defun unit (id class p10 p50 p90 &rest extra)
  (apply #'nlk:make-json-object
         (append (list "id" id "title" (format nil "unit ~a" id) "class" class
                       "p10" p10 "p50" p50 "p90" p90)
                 extra)))

(defun plan-args (units &key (gates t))
  (list "action" "submit"
        "plan" (nlk:make-json-object
                 "shape" "single_artifact"
                 "units" units
                 "gates" (if gates
                               (vector (nlk:make-json-object "after" "U1" "check" "five clubs answer"
                                                              "kill_if" "fewer than two do"))
                               (vector))
                 "effort_driver" ""
                 "scope_reasons" (vector))))

(defun blueprint-plan (units &key (gates t))
  (apply #'call #'blueprint:plan-tool (plan-args units :gates gates)))

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "blueprint"
  (:config)
  (:help :blueprint)
  (:command "blueprint")
  (:running
   (is (nle::find-tool "blueprint_phase") "the phase tool is registered")
   (is (nle::find-tool "blueprint_research") "and research")
   (is (nle::find-tool "blueprint_verdict") "and the critic")
   (is (nle::find-tool "blueprint_plan") "and the plan")
   (is (nle::find-tool "blueprint_recheck") "and the re-check")
   (is (= 6 (count-if (lambda (entry) (eql 0 (search "blueprint_" (first entry)))) nle::*tools*)) "six of them, and no others")
   (is (equal (blueprint-setting :tier) "standard") "the tier defaults to standard"))
  (:stopped (is (null (nle::find-tool "blueprint_phase")) "stop takes every tool back"))
  (:refused ("min_quote_chars" 5))
  (:idle blueprint:blueprint-error (blueprint:status-text)))

;;; --- the phases and their gates -----------------------------------------------------

(deftest blueprint-cell-a-phase-does-not-end-without-its-artifact ()
  (with-blueprint ()
    (call #'blueprint:phase-tool "action" "start" "idea" "a WhatsApp bot that books padel courts")
    (let ((started (call #'blueprint:phase-tool "action" "status"))
          (stuck (call #'blueprint:phase-tool "action" "advance")))
      (is (search "1. Understanding" started) "a new project starts at phase 1")
      (is (search "profile" stuck) "advancing without the brief and the profile names both"))
    (call #'blueprint:phase-tool "action" "record" "kind" "brief" "content" "for small clubs")
    (is (search "builder profile" (call #'blueprint:phase-tool "action" "advance")) "the brief alone does not open the gate")
    (call #'blueprint:profile-tool "action" "set"
          "profile" (nlk:make-json-object "team" "solo" "agents" (vector "claude-code")
                                            "hours_per_day" 4))
    (is (search "confirmed" (call #'blueprint:profile-tool "action" "confirm")) "the profile confirms into the project")
    (is (search "2. Context" (call #'blueprint:phase-tool "action" "advance")) "and then the phase moves")))

(deftest blueprint-cell-variants-need-three-distinct-ones ()
  (with-blueprint ()
    (started)
    (at-phase 3)
    (let ((two (vector (nlk:make-json-object "name" "bot" "summary" "a bot")
                       (nlk:make-json-object "name" "app" "summary" "an app"))))
      (is (search "at least 3" (call #'blueprint:phase-tool "action" "record" "kind" "variants" "content" two)) "two variants do not open the gate"))
    (let ((three (vector (nlk:make-json-object "name" "bot" "summary" "a bot")
                         (nlk:make-json-object "name" "app" "summary" "an app")
                         (nlk:make-json-object "name" "desk" "summary" "a desk"))))
      (is (search "3. Generation" (call #'blueprint:phase-tool "action" "record" "kind" "variants" "content" three)) "three distinct variants open it"))))

(deftest blueprint-cell-a-choice-must-name-a-recorded-variant ()
  (with-blueprint ()
    (call #'blueprint:phase-tool "action" "start" "idea" "x")
    (at-phase 3)
    (call #'blueprint:phase-tool "action" "record" "kind" "variants"
          "content" (vector (nlk:make-json-object "name" "bot" "summary" "a bot")
                             (nlk:make-json-object "name" "app" "summary" "an app")
                             (nlk:make-json-object "name" "desk" "summary" "a desk")))
    (at-phase 4)
    (let ((wrong (call #'blueprint:phase-tool "action" "record" "kind" "choice"
                        "content" (nlk:make-json-object "variant" "nothing" "reason" "because"))))
      (is (search "one of the recorded variants" wrong) "a choice for a variant nobody recorded is refused"))
    (let ((right (call #'blueprint:phase-tool "action" "record" "kind" "choice"
                        "content" (nlk:make-json-object "variant" "app" "reason" "the clubs ask for it"))))
      (is (search "4. Deep dive" right) "and one that names a recorded variant opens the gate"))))

;;; --- the quote check ---------------------------------------------------------------

(deftest blueprint-cell-a-check-mark-needs-a-quote-in-the-page ()
  (with-pages ("https://clubs.example/padel" *club-page*)
    (with-blueprint ("tier" "quick")
      (call #'blueprint:phase-tool "action" "start" "idea" "a padel booking bot")
      (let ((made (call #'blueprint:research-tool "action" "claim" "text" "clubs lose courts"
                         "type" "traction" "url" "https://clubs.example/padel"
                         "quote" "two clubs reported losing a court a week to a double booking"))
            (lying (call #'blueprint:research-tool "action" "claim" "text" "a made-up claim"
                          "type" "technical" "url" "https://clubs.example/padel"
                          "quote" "this sentence is nowhere on the page at all")))
        (is (search "[partial]" made) "one site is a partial claim, not a check mark")
        (is (search "not in https://clubs.example/padel" lying) "a quote the page does not carry is refused")))))

(deftest blueprint-cell-two-independent-sites-make-a-check-mark ()
  (with-pages ("https://clubs.example/padel" *club-page*
               "https://dir.example/padel" *directory-page*
               "https://copy.example/padel" *club-page*)
    (with-blueprint ("tier" "quick")
      (call #'blueprint:phase-tool "action" "start" "idea" "a padel booking bot")
      (call #'blueprint:research-tool "action" "claim" "text" "clubs lose bookings" "type" "market_size"
            "url" "https://clubs.example/padel"
            "quote" "two clubs reported losing a court a week to a double booking")
      (call #'blueprint:research-tool "action" "source" "id" "C1" "url" "https://dir.example/padel"
            "quote" "every one of them asks the club for a monthly fee")
      (let ((claim (nodecode-blueprint::find-claim (nodecode-blueprint::current-project) "C1")))
        (is (equal 2 (nodecode-blueprint::claim-independent claim)) "two sites, two independent sources"))
      (call #'blueprint:research-tool "action" "source" "id" "C1" "url" "https://copy.example/padel"
            "quote" "Small padel clubs take bookings on WhatsApp")
      (let ((claim (nodecode-blueprint::find-claim (nodecode-blueprint::current-project) "C1")))
        (is (equal 2 (nodecode-blueprint::claim-independent claim)) "a republished page does not count twice")
        (is (equal "verified" (nodecode-blueprint::claim-label claim)) "and the claim carries a check mark")))))

(deftest blueprint-cell-an-absence-is-never-a-check-mark ()
  (with-blueprint ()
    (call #'blueprint:phase-tool "action" "start" "idea" "x")
    (let ((logged (call #'blueprint:research-tool "action" "absence" "text" "no padel bot on GitHub"
                        "searches" (vector "github padel booking bot" "padel whatsapp bot"))))
      (is (search "never a check mark" logged) "a logged absence says what it is"))
    (is (equal "absence" (nodecode-blueprint::claim-label (nodecode-blueprint::find-claim (nodecode-blueprint::current-project) "C1"))) "and its label is an absence")
    (is (search "not ready" (call #'blueprint:research-tool "action" "brief")) "the briefing refuses while coverage is short")))

;;; --- the verdict -------------------------------------------------------------------

(deftest blueprint-cell-a-verdict-commits-once-and-needs-an-outside-view ()
  (with-blueprint ()
    (started)
    (at-phase 5)
    (let ((thin (call #'blueprint:verdict-tool "action" "commit" "verdict" "go" "thesis" "it will work" "p_success" 0.4))
          (full (commit-verdict))
          (again (commit-verdict))
          (pushed (call #'blueprint:verdict-tool "action" "pushback" "argument" "but my friends would use it")))
      (is (search "reference_class" thin) "a verdict without a named reference class is refused")
      (is (search "P(success)" full) "a complete verdict commits")
      (is (search "already committed" again) "a second commit is refused: it was stored before it was shown")
      (is (search "The stored verdict stands" pushed) "pushback restates the stored verdict")
      (is (search "blueprint_research" pushed) "and names what evidence could change it"))))

;;; --- the plan's numbers -------------------------------------------------------------

(deftest blueprint-cell-a-plan-is-checked-and-its-numbers-are-the-code-s ()
  (with-blueprint ()
    (started)
    (at-phase 5)
    (commit-verdict)
    (at-phase 6)
    (let ((bare (blueprint-plan (vector (unit "U1" "A" 1 2 3 "ref_class" "scaffold") (unit "U2" "H" 1 2 3))))
          (high (blueprint-plan (vector (unit "U1" "A" 5 20 40 "ref_class" "scaffold" "acceptance" (vector "a booking"))
                                         (unit "U2" "H" 1 2 3))))
          (good (blueprint-plan (vector (unit "U1" "A" 0.5 1 3 "ref_class" "scaffold" "acceptance" (vector "a booking"))
                                         (unit "U2" "H" 1 2 4)
                                         (unit "U3" "W" 14 14 21 "start_now" t)))))
      (is (search "acceptance" bare) "an agent unit without acceptance criteria is refused")
      (is (search "above the scaffold P90" high) "agent hours above the class P90 need a multiplier or the right class")
      (is (search "# Plan:" good) "a plan that passes is rendered")
      (is (search "agent 1.0 h" good) "the agent effort is summed in code")
      (is (search "Calendar:" good) "and the calendar is computed, not written"))))

(deftest blueprint-cell-a-wait-that-depends-on-nothing-starts-today ()
  (with-blueprint ()
    (started)
    (at-phase 5)
    (commit-verdict)
    (at-phase 6)
    (let ((waiting (blueprint-plan (vector (unit "U1" "A" 0.5 1 3 "ref_class" "scaffold" "acceptance" (vector "a booking"))
                                            (unit "U2" "H" 1 2 4)
                                            (unit "U3" "W" 14 14 21))))
          (ungated (blueprint-plan (vector (unit "U1" "A" 0.5 1 3 "ref_class" "scaffold" "acceptance" (vector "a booking")))
                                   :gates nil)))
      (is (search "starts on day 1" waiting) "an independent wait without start_now is refused")
      (is (search "gates:" ungated) "a plan with no gate is refused"))))

(deftest blueprint-cell-a-critic-will-not-change-its-mind-for-free ()
  (with-blueprint ()
    (started)
    (at-phase 5)
    (commit-verdict)
    (let ((free (call #'blueprint:verdict-tool "action" "revise" "reason" "I disagree"))
          (base (call #'blueprint:verdict-tool "action" "base_rates")))
      (is (search "new_evidence" free) "a revision without new evidence is refused")
      (is (search "chrome_extensions" base) "the outside views are named for the critic"))))

;;; --- the command and the re-check ---------------------------------------------------

(deftest blueprint-cell-the-command-answers-the-phase-and-the-gate ()
  (with-blueprint ()
    (is (search "no project yet" (blueprint:run-command "" "session-1")) "with no project the command says how to start one")
    (is (search "1. Understanding" (blueprint:run-command "start a padel booking bot" "session-1")) "start binds a project to the chat")
    (is (search "Gate still needs" (blueprint:run-command "status" "session-1")) "status names what the gate still needs")
    (is (search "#1" (blueprint:run-command "list" "session-1")) "list names it")))

(deftest blueprint-cell-a-recheck-lists-the-predictions-still-to-score ()
  (with-blueprint ()
    (started)
    (at-phase 5)
    (commit-verdict)
    (let ((report (call #'blueprint:recheck-tool "action" "report"))
          (missed (call #'blueprint:recheck-tool "action" "score" "prediction" "P2" "result" "miss" "actual_hours" 3)))
      (is (search "P1 [verdict]" report) "the stored predictions come back with their ids")
      (is (search "today is" report) "and the report names today")
      (is (search "Brier" missed) "a scored prediction answers its Brier score")
      (is (search "miss" missed) "and what the outcome was"))))


