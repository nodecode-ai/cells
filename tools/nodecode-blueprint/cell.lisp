;;;; cell.lisp --- six gated phases from a raw idea to a plan you can build.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Rewritten from winchxyz's hermes-blueprint plugin; see NOTICE. The method
;;;; is prose in the manual and in each phase's rules; what the code keeps is
;;;; the part the original checked in code, so a chat model cannot skip it by
;;;; being confident:
;;;;
;;;;   - a check mark needs a quote that appears in a page this cell fetched
;;;;     itself, and two independent sites for a market size or traction claim;
;;;;     "nothing like this exists" is never checked, it is a logged absence;
;;;;   - a phase's gate is a stored artifact from the tool that produces it, so
;;;;     research, the critic's verdict and the plan cannot be free text;
;;;;   - the verdict is stored before it is shown, and changes only with
;;;;     evidence recorded after it;
;;;;   - a plan is A (agent hours), H (your hours) and W (named waits) units
;;;;     with P10/P50/P90, and the code computes effort and calendar itself.

(in-package #:nodecode-blueprint)

;;; --- the manual ---------------------------------------------------------------

(defparameter +primer+
  "Blueprint turns one raw idea into a plan you can build, in six gated phases:
1 Understanding: interview (who for, the problem, why now, constraints, what done means), record the
  brief, and confirm the builder profile (team, agents, hours per day) after the user agrees.
2 Context: research. blueprint_research plan lists what to search; claim stores a quote you read on a
  fetched page; source adds a second independent one; absence logs where you looked for a gap. A
tag is CHECKED (verified) only when the quote is in the page and enough independent sites say it;
  PARTIAL and LEAD otherwise. brief writes the briefing and refuses while too little is covered.
3 Generation: three really different variants, each with name and summary, recorded together.
4 Deep dive: score them against a fixed rubric, then record the choice with the reason.
5 Critique: blueprint_verdict base_rates names the outside view; commit stores verdict, p_success,
  premortem, tripwires and at least three scoreable predictions before you show them; pushback
  answers what the stored verdict says and what evidence would change it; revise needs C# claims
  recorded after the commit.
6 Plan: blueprint_plan units A (agent-bound active hours), H (your hours) or W (external wait days)
  with P10/P50/P90, a ref_class for agent work, and gates with a kill threshold. Code computes
  effort vs calendar; a single artifact over 8 agent hours needs an effort_driver.
Then blueprint_recheck report: due predictions, actual hours against estimates.
The tools: blueprint_phase (status, record, advance, start, list, stop), blueprint_profile,
blueprint_research, blueprint_verdict, blueprint_plan, blueprint_recheck. /blueprint runs the same
ones from the keyboard. A phase's gate is checked in code; (help :blueprint) is this text."
  "What (help :blueprint) answers while the cell runs.")

(defparameter +phases+
  '((1 . "Understanding") (2 . "Context") (3 . "Generation")
    (4 . "Deep dive") (5 . "Critique") (6 . "Plan"))
  "The six phases, by number.")

(defparameter +last-phase+ 6)

(defparameter +gates+
  '((1 (("brief" "blueprint_phase"
        "the problem brief: who it is for, the problem, why now, constraints, what done means")
       ("profile_used" "blueprint_profile"
        "the builder profile confirmed for this project")))
    (2 (("briefing" "blueprint_research"
        "the research briefing: source log, coverage and the not-verified block")))
    (3 (("variants" "blueprint_phase" "at least three really different variants")))
    (4 (("choice" "blueprint_phase" "the chosen variant and the reason")))
    (5 (("verdict" "blueprint_verdict" "the critic's verdict, committed before it was shown")))
    (6 (("plan" "blueprint_plan"
        "the validated plan: A/H/W units, P10/P50/P90, effort against calendar"))))
  "Each phase's exit gate: (KIND PRODUCER LABEL), so only the tool that carries the
quality harness can satisfy it.")

(defparameter +recordable+
  '(("brief" . 1) ("variants" . 3) ("deepdive" . 4) ("choice" . 4))
  "What blueprint_phase action=record may write, and in which phase.")

(defparameter +min-variants+ 3)

(defparameter +phase-rules+
  '((1 "Interview first: one or two questions per turn, no solutions yet. Pin down who it is for,\n   the problem, why now, the constraints, what done means. Show the builder profile, ask only for\n   missing fields, and confirm it after the user agrees. Then record the brief and advance.")
    (2 "Plan the research, then search, fetch and quote. A search snippet is a lead, never a claim.\n   Run disconfirmation searches and the competitor sweep (repos, registries, marketplaces, app\n   stores, product lists, forums). Absence is never a check mark: log it with action=absence.")
    (3 "Three really different variants: a different user, mechanism or business model, not one idea\n   in three sizes. Each needs a name and a summary (who for, why it could win, biggest risk),\n   grounded in the briefing's C# claims.")
    (4 "Score the variants one at a time against a fixed rubric, evidence before score, and work out\n   what must be true for the leading one. The user chooses: record the choice with the reason.")
    (5 "Outside view first: a reference class and a sourced base rate. Premortem shares are\n   conditional on failure and calibrated to this builder. Commit the verdict before you show it.\n   Pushback without new evidence changes nothing.")
    (6 "A/H/W units with P10/P50/P90. Agent work is hours, not weeks; months come only from named\n   waits. Include H units for review and real-use testing. Then blueprint_recheck report."))
  "The few rules that matter in each phase: what the state card and status carry.")

(defparameter +summary-text+
  "Blueprint: {PHASE}. Gate still needs: {MISSING}. Next: {NEXT}"
  "The one line the state card and /blueprint status open with.")

;;; --- answers, and how a refusal reaches the model ---------------------------------

(defun refuse (format-control &rest args)
  "The model's answer when a call cannot go on, as the call's failure."
  (nle:failure (format nil "ERROR: ~a" (apply #'format nil format-control args))))

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (blueprint-error (condition)
       (nle:failure (format nil "ERROR: ~a" (blueprint-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: blueprint: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun nonblank (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun numbered (items &optional (start 1))
  "ITEMS as lines 1., 2., ... one per line."
  (with-output-to-string (out)
    (loop for item in items for n from start
          do (format out "~d. ~a~%" n item))))

(defun bulleted (items)
  "ITEMS as - lines one per line."
  (with-output-to-string (out)
    (dolist (item items) (format out "- ~a~%" item))))

;;; --- the tables the checks start from ---------------------------------------------
;; Data, so a layer can reword or re-date them. The original reviewed them in 2026-11.

(defparameter +unit-classes+
  '(("scaffold" "scaffold, single-file tool, static page, plugin skeleton" 0.3 1.0 3.0)
    ("api_integration" "one documented external API, with tests" 0.5 1.5 4.0)
    ("polished_feature" "polished web toy or app feature with custom rendering" 1.0 3.0 8.0)
    ("core_engine" "game or app with physics, rendering or a solver core" 5.0 12.0 30.0)
    ("multi_component" "multi-component slice: backend + frontend + deploy" 10.0 30.0 80.0)
    ("money_code" "contracts or real-money code, excluding audits" 10.0 40.0 100.0)
    ("promo_video" "promo video or reel with an existing pipeline" 1.0 2.0 6.0)
    ("docs" "README, docs, catalog or store listing text" 0.3 1.0 2.5))
  "Agent-bound active hours per work unit, P10/P50/P90, with what the class is.")

(defparameter +multipliers+
  '(("unfamiliar_toolchain" 3.0 "game engines, modloaders, native mobile")
    ("unseen_codebase" 4.0 "a large existing codebase the agent has not seen")
    ("novel_problem" 5.0 "a novel algorithm or research question; widen P90"))
  "Named multipliers on an agent unit's P90 ceiling: (NAME FACTOR WHAT).")

(defparameter +reference-classes+
  '(("steam_games" "Steam games released in 2025"
     "about 20,000 releases; median revenue about $249; about 66% earned under $1,000")
    ("chrome_extensions" "Chrome Web Store extensions, 2019 snapshot"
     "87% under 1,000 installs; 50% under 16 installs")
    ("us_new_businesses" "US new employer establishments"
     "about 49% survive five years, 1994-2022 average")
    ("ai_dev_speed_belief" "experienced developers using AI tools on their own repos"
     "19% slower, after expecting 24% faster, and still believing they were 20% faster")
    ("agent_task_horizon" "agent task length, 50% time horizon"
     "about 50 minutes in 2025, doubling roughly every 7 months since 2019"))
  "Outside views a verdict may cite by key, with the rate in words. The dates and sources
are the original's, kept as they were: a layer or a later cell re-dates them.")

(defparameter +waits+
  '(("google_play_new_account" "12 testers opted in for 14 consecutive days before production")
    ("apple_review" "50% of submissions reviewed in under 24 h, 90% in under 48 h")
    ("steam_release" "$100 fee, 21-day wait after paying, Coming Soon page live 2 weeks")
    ("chrome_web_store" "most reviews within a few days, up to a few weeks")
    ("plugin_catalog_review" "a maintainer's review of a catalog pull request: days"))
  "Calendar-bound waits a solo builder hits, by key.")

(defparameter +claim-types+
  '(("market_size" . 2) ("traction" . 2) ("price" . 1) ("competitor_exists" . 1)
    ("technical" . 1) ("policy" . 1) ("other" . 1) ("competitor_absent" . 0))
  "Independent sources a check mark needs, by claim type; 0 never verifies (an absence).")

(defparameter +tiers+ '(("quick" . 3) ("standard" . 6) ("deep" . 10))
  "Claims a briefing needs, by research tier.")

(defparameter +shapes+
  '(("single_artifact" . "one plugin, CLI, bot, static site or one-screen tool")
    ("app" . "a polished app or game")
    ("multi_component" . "several components: backend, frontend, deploy")
    ("money_or_security" . "real money, contracts or security-critical code")
    ("physical_or_taste" . "physical prototypes, brand or taste loops")
    ("research" . "an open research question"))
  "The shapes a plan may declare.")

;;; --- the state: one file under this home ------------------------------------------

(defvar *state* nil
  "The loaded state while the cell runs: projects, profile, predictions, ledger.")

(defun state-path ()
  "Where the projects live: the section's state_file, else blueprint/state.sexp under the home."
  (let ((named (setting :state-file)))
    (if (and (stringp named) (plusp (length (nlk:trimmed named))))
        (pathname (nlk:trimmed named))
        (nlk:home "blueprint/state.sexp"))))

(defun empty-state ()
  (list :version 1 :next-id 0 :current nil :projects '() :predictions '()
        :profile (list :version 0 :fields '()) :actuals '()))

(defun load-state ()
  (let ((path (state-path)))
    (or (when (probe-file path)
          (handler-case
              (let ((*read-eval* nil) (*package* (find-package :keyword)))
                (let ((form (read-from-string (uiop:read-file-string path))))
                  (and (listp form) (getf form :version) form)))
            (error () nil)))
        (empty-state))))

(defun save-state ()
  (let ((path (state-path)))
    (ensure-directories-exist path)
    (nlk:write-file-atomically
     path
     (with-standard-io-syntax
       (let ((*print-pretty* nil) (*print-circle* nil) (*print-length* nil))
         (format nil "~s~%" *state*)))
     :mode #o600 :directory-mode #o700)
    *state*))

(defun require-running ()
  (unless (and (boundp '*state*) *state*)
    (fail "the blueprint cell is not running: turn it on in /cells, then (restart-cells \"nodecode-blueprint\")")))

(defun state ()
  (require-running)
  *state*)

(defun projects () (getf (state) :projects))

(defun now () (nlk:iso-now))

(defun new-id ()
  (let ((id (1+ (or (getf (state) :next-id) 0))))
    (setf (getf *state* :next-id) id)
    id))

(defun find-project (id)
  (find (princ-to-string id) (projects) :key (lambda (p) (princ-to-string (getf p :id)))
        :test #'equal))

(defun current-project (&optional (state (state)))
  "The project this home is working on, or NIL."
  (let ((id (getf state :current)))
    (and id (find-project id))))

(defun clipped-title (idea)
  (let ((idea (nlk:one-line (nlk:trimmed idea))))
    (if (> (length idea) 64) (subseq idea 0 64) idea)))

(defun start-project (idea)
  "Bind a new project to this home, at phase 1."
  (let ((idea (nonblank idea)))
    (unless idea (fail "a project starts from an idea: (blueprint_phase action=start idea=\"...\")"))
    (let* ((id (new-id))
           (project (list :id id :title (clipped-title idea) :idea idea :status "active"
                          :phase 1 :created (now) :artifacts '() :claims '() :next-claim 0
                          :log '() :session nil)))
      (setf (getf *state* :projects) (cons project (projects))
            (getf *state* :current) id)
      (save-state)
      project)))

;;; --- artifacts, and the gates they open -------------------------------------------

(defun artifacts (project) (getf project :artifacts))

(defun artifact (project kind)
  "The newest artifact of KIND, or NIL."
  (cdr (assoc kind (artifacts project) :test #'equal)))

(defun put-artifact (project kind producer payload)
  "Store PAYLOAD as PROJECT's newest artifact of KIND, replaced wholesale."
  (let ((entry (list :producer producer :payload payload :at (now) :seq (new-id))))
    (setf (getf project :artifacts)
          (cons (cons kind entry) (remove kind (artifacts project) :key #'car :test #'equal)))
    entry))

(defun artifact-count (project kind)
  (count kind (artifacts project) :key #'car :test #'equal))

(defun phase-name (phase)
  (format nil "~d. ~a" phase (or (cdr (assoc phase +phases+)) "?")))

(defun gate (phase)
  (let ((entry (cdr (assoc phase +gates+))))
    (if (and entry (listp (first entry)) (listp (first (first entry))))
        (first entry)
        entry)))

(defun missing (project phase)
  "What PHASE's gate still needs from PROJECT: one label a missing requirement."
  (loop for (kind producer label) in (gate phase)
        unless (let ((found (artifact project kind)))
                 (and found (equal producer (getf found :producer))))
          collect label))

(defun open-items (project)
  (missing project (getf project :phase)))

(defun set-phase (project phase &optional (reason "jump"))
  (setf (getf project :phase) phase
        (getf project :log) (cons (list :at (now) :phase phase :reason reason)
                                   (getf project :log)))
  project)

(defun advance (project)
  "Move on when the current phase's gate is met; finishing 6 marks the project planned."
  (let ((gap (open-items project)))
    (when gap
      (fail "phase ~a is not done: missing ~{~a~^; ~}"
            (phase-name (getf project :phase)) gap))
    (if (= (getf project :phase) +last-phase+)
        (progn (setf (getf project :status) "planned") project)
        (set-phase project (1+ (getf project :phase)) "advance"))))

(defun go-to (project target reason)
  "Jump to TARGET: backwards always, forwards only past every gate in between."
  (unless (and (integerp target) (<= 1 target +last-phase+))
    (fail "phase must be 1-~d" +last-phase+))
  (when (> target (getf project :phase))
    (loop for phase from (getf project :phase) below target
          for gap = (missing project phase)
          when gap do (fail "phase ~a is not done: missing ~{~a~^; ~}" (phase-name phase) gap)))
  (setf (getf project :status) "active")
  (set-phase project target (or reason "jump")))

;;; --- what blueprint_phase may record ----------------------------------------------

(defun variant-names (variants)
  (loop for variant in variants
        when (hash-table-p variant)
          collect (string-downcase (or (nlk:json-value variant :text "name") ""))))

(defun check-record (project kind content)
  "CONTENT for KIND, normalized, or a refusal saying what is missing."
  (unless (assoc kind +recordable+ :test #'equal)
    (fail "blueprint_phase records only ~{~a~^, ~}; research briefings, verdicts and plans come from their own tools"
          (mapcar #'car +recordable+)))
  (unless (= (getf project :phase) (cdr (assoc kind +recordable+ :test #'equal)))
    (fail "~a belongs to phase ~a; the project is in ~a" kind
          (phase-name (cdr (assoc kind +recordable+ :test #'equal)))
          (phase-name (getf project :phase))))
  (cond
    ((equal kind "variants")
     (let ((variants (as-list content)))
       (unless (and (>= (length variants) +min-variants+) (every #'hash-table-p variants))
         (fail "variants must be a list of at least ~d objects, each with name and summary"
               +min-variants+))
       (dolist (variant variants)
         (unless (and (nonblank (nlk:json-value variant :text "name"))
                      (nonblank (nlk:json-value variant :text "summary")))
           (fail "each variant needs a name and a summary: who it is for, why it could win, its biggest risk")))
       (let ((names (variant-names variants)))
         (unless (= (length names) (length (remove-duplicates names :test #'equal)))
           (fail "variant names must be distinct")))
       variants))
    ((equal kind "choice")
     (let* ((choice (as-object content))
            (variant (and choice (nonblank (nlk:json-value choice :text "variant"))))
            (reason (and choice (nonblank (nlk:json-value choice :text "reason"))))
            (recorded (artifact project "variants")))
       (unless (and variant reason)
         (fail "choice needs variant (a recorded variant's name) and reason (why it wins)"))
       (let ((names (and recorded (variant-names (getf recorded :payload)))))
         (unless (member (string-downcase variant) names :test #'equal)
           (fail "choice must name one of the recorded variants: ~{~a~^, ~}" (or names '("none")))))
       choice))
    (t
     (let ((text (if (stringp content) content (nlk:encode-json-object content))))
       (unless (nonblank text) (fail "~a is empty" kind))
       (list :text (nlk:trimmed text))))))

;;; --- json in, lisp values out ------------------------------------------------------

(defun as-object (value)
  "VALUE as a JSON object: itself, or a string holding one."
  (cond ((hash-table-p value) value)
        ((and (stringp value) (plusp (length (nlk:trimmed value)))
              (char= #\{ (char (nlk:trimmed value) 0)))
         (ignore-errors (nlk:decode-json value)))
        (t nil)))

(defun as-list (value)
  "VALUE as a list: a JSON array, a string holding one, or one value."
  (let ((value (if (and (stringp value) (plusp (length (nlk:trimmed value)))
                        (char= #\[ (char (nlk:trimmed value) 0)))
                   (ignore-errors (nlk:decode-json value))
                   value)))
    (cond ((null value) '())
          ((vectorp value) (coerce value 'list))
          ((listp value) value)
          (t (list value)))))

(defun get-text (object key)
  (and (hash-table-p object) (nonblank (nlk:json-value object :text key))))

(defun get-number (object key)
  "KEY of OBJECT as a number, or NIL; a string of digits is read."
  (let ((value (and (hash-table-p object) (nlk:json-value object :any key))))
    (cond ((numberp value) (float value))
          ((and (stringp value) (plusp (length (nlk:trimmed value))))
           (let ((read (ignore-errors (read-from-string (nlk:trimmed value)))))
             (and (numberp read) (float read))))
          (t nil))))

(defun get-list (object key)
  (and (hash-table-p object) (as-list (nlk:json-value object :any key))))

(defun get-strings (object key)
  (loop for value in (get-list object key)
        for text = (and (stringp value) (nonblank value))
        when text collect (nlk:trimmed text)))

(defun get-object (object key)
  (and (hash-table-p object)
       (let ((value (nlk:json-value object :any key)))
         (cond ((hash-table-p value) value)
               ((stringp value) (as-object value))
               (t nil)))))

;;; --- the builder profile ------------------------------------------------------------

(defparameter +profile-fields+
  '(("team" . "solo, pair or team(n)")
    ("agents" . "the coding agents used, as a list")
    ("parallel_agents" . "typical concurrent agent sessions")
    ("hours_per_day" . "engaged hours on an active day")
    ("days_per_week" . "active days per week")
    ("skills" . "where you verify fast")
    ("weak_spots" . "where verification is slow")
    ("hated_tasks" . "tasks that predict abandonment: marketing, support, ...")
    ("accounts_ready" . "github, app-store, play-console, steam, stripe, ...")
    ("distribution" . "audience assets and their sizes, past launches")
    ("budget_usd" . "{one_time, monthly}")
    ("goal" . "fun, learning, portfolio or income")
    ("jurisdiction" . "the country, for payments, tax and regulation"))
  "The profile's fields and what each holds.")

(defparameter +profile-string-fields+ '("team" "goal" "jurisdiction"))
(defparameter +profile-list-fields+
  '("agents" "skills" "weak_spots" "hated_tasks" "accounts_ready" "distribution"))
(defparameter +profile-number-fields+ '("parallel_agents" "hours_per_day" "days_per_week"))
(defparameter +profile-required+ '("team" "agents" "hours_per_day"))
(defparameter +profile-defaults+ '(("parallel_agents" . 1.0) ("days_per_week" . 5.0)))

(defun profile-store () (getf (state) :profile))

(defun profile-fields () (getf (profile-store) :fields))

(defun profile-value (key) (cdr (assoc key (profile-fields) :test #'equal)))

(defun profile-missing ()
  (loop for key in +profile-required+ unless (profile-value key) collect key))

(defun check-profile-field (key value)
  "VALUE for KEY, normalized, or a refusal."
  (cond
    ((member key +profile-string-fields+ :test #'equal)
     (or (nonblank value) (fail "~a should be a sentence of text" key)))
    ((member key +profile-list-fields+ :test #'equal)
     (let ((items (if (stringp value)
                      (loop for item in (uiop:split-string value :separator '(#\,))
                            for text = (nonblank item) when text collect (nlk:trimmed text))
                      (loop for item in (as-list value)
                            for text = (and (stringp item) (nonblank item))
                            when text collect (nlk:trimmed text)))))
       (or items (fail "~a is a list: ~a" key (cdr (assoc key +profile-fields+ :test #'equal))))))
    ((member key +profile-number-fields+ :test #'equal)
     (let ((number (if (numberp value) (float value)
                       (ignore-errors (read-from-string (princ-to-string value))))))
       (unless (and (numberp number) (plusp number))
         (fail "~a is a positive number (~a)" key (cdr (assoc key +profile-fields+ :test #'equal))))
       (when (and (equal key "hours_per_day") (> number 16))
         (fail "hours_per_day above 16 is not a sustainable plan"))
       (if (and (not (floatp value)) (= number (round number))) (round number) number)))
    ((equal key "budget_usd")
     (let ((object (as-object value)))
       (unless object (fail "budget_usd is an object: {one_time, monthly}"))
       (loop for name being the hash-keys of object using (hash-value amount)
             when (get-number object name) collect (cons name (get-number object name)))))
    (t (fail "unknown profile field ~s: ~{~a~^, ~}" key (mapcar #'car +profile-fields+)))))

(defun set-profile (object)
  "Merge OBJECT's fields into the stored profile."
  (unless (hash-table-p object) (fail "set needs profile: {field: value}, fields ~{~a~^, ~}"
                                       (mapcar #'car +profile-fields+)))
  (let ((clean '()))
    (loop for key being the hash-keys of object
          do (push (cons key (check-profile-field key (gethash key object))) clean))
    (unless clean (fail "set needs profile: {field: value}, fields ~{~a~^, ~}"
                        (mapcar #'car +profile-fields+)))
    (let ((store (profile-store)))
      (dolist (pair clean)
        (let ((existing (assoc (car pair) (getf store :fields) :test #'equal)))
          (if existing
              (setf (cdr existing) (cdr pair))
              (setf (getf store :fields) (cons pair (getf store :fields))))))
      (setf (getf *state* :profile)
            (list :version (1+ (or (getf store :version) 0)) :fields (getf store :fields))))
    (save-state)
    (profile-fields)))

(defun effective-profile (project)
  "The profile PROJECT confirmed, else the current one, with defaults filled in."
  (let* ((confirmed (and project (artifact project "profile_used")))
         (fields (copy-alist (if confirmed (getf (getf confirmed :payload) :fields)
                                 (profile-fields)))))
    (dolist (pair +profile-defaults+)
      (unless (assoc (car pair) fields :test #'equal)
        (setf fields (cons pair fields))))
    fields))

(defun profile-line (fields)
  (format nil "~{~a: ~a~^; ~}"
          (loop for (key . value) in fields
                append (list key (if (listp value)
                                     (format nil "~{~a~^, ~}" value)
                                     (princ-to-string value))))))

;;; --- evidence: what counts as a source ---------------------------------------------

(defvar *fetch* nil
  "The page reader a claim goes through: a function of a URL answering (values TEXT STATUS).
NIL reads the page with NLK:HTTP; a test binds one, so nothing leaves the machine.")

(defun normalize (text)
  "TEXT folded for a quote check: typographic quotes and dashes to ASCII, a markdown link to
its text, markup dropped, case and whitespace folded."
  (let* ((text (substitute (code-char 39) (code-char 8217) (or text "")))
         (text (substitute (code-char 39) (code-char 8216) text))
         (text (substitute (code-char 34) (code-char 8220) text))
         (text (substitute (code-char 34) (code-char 8221) text))
         (text (substitute #\- (code-char 8212) text))
         (text (substitute #\- (code-char 8211) text))
         (out (make-string-output-stream))
         (skip nil)
         (space nil)
         (wrote nil))
    (loop for char across text
          do (cond
               (skip (when (char= char #\)) (setf skip nil)))
               ((char= char #\]) (setf skip t) (setf space t))
               ((find char "[`*_#>|~")
                (setf space t))
               ((find char '(#\Space #\Tab #\Newline #\Return #\Linefeed))
                (setf space t))
               (t
                (when (and space wrote) (write-char #\Space out))
                (setf space nil wrote t)
                (write-char char out))))
    (string-downcase (string-trim " " (get-output-stream-string out)))))

(defun quote-in-p (quote content)
  "Whether QUOTE appears in CONTENT, folded, and is long enough to mean something."
  (let ((quote (string-trim " .\"'" (normalize quote))))
    (and (>= (length quote) (setting :min-quote-chars))
         (search quote (normalize content)))))

(defun domain-of (url)
  "URL's site: its host, www dropped, the last two labels (three for a second-level one)."
  (let* ((start (or (search "//" url) -1))
         (rest (subseq url (min (length url) (+ start 2))))
         (end (or (position-if (lambda (char) (find char "/?#")) rest) (length rest)))
         (host (string-downcase (string-trim "." (subseq rest 0 end)))))
    (when (and (> (length host) 4) (eql 0 (search "www." host))) (setf host (subseq host 4)))
    (let* ((labels (uiop:split-string host :separator '(#\.)))
           (n (length labels)))
      (cond ((>= n 3) (format nil "~{~a~^.~}" (last labels 2)))
            (t host)))))

(defun content-hash (text) (nlk:sha256-text (normalize text)))

(defun fetch-page (url)
  "URL's text and status, through *FETCH* when a test bound one."
  (if *fetch*
      (funcall *fetch* url)
      (nlk:http :get url :timeout (setting :timeout-seconds))))

(defun required-sources (type)
  "Independent sources TYPE needs for a check mark; NIL when it never verifies."
  (let ((need (assoc type +claim-types+ :test #'equal)))
    (unless need (fail "claim type must be one of ~{~a~^, ~}" (mapcar #'car +claim-types+)))
    (and (plusp (cdr need)) (cdr need))))

(defun claim-independent (claim)
  "The claim's independent sources: one per site, a page republished word for word counting once."
  (let ((domains '()) (hashes '()) (n 0))
    (dolist (source (getf claim :sources))
      (let ((domain (getf source :domain)) (hash (getf source :hash)))
        (unless (or (member domain domains :test #'equal) (member hash hashes :test #'equal))
          (push domain domains) (push hash hashes) (incf n))))
    n))

(defun claim-label (claim)
  (let ((need (required-sources (getf claim :type)))
        (n (claim-independent claim)))
    (cond ((null need) "absence")
          ((null claim) "unverified")
          ((>= n need) "verified")
          ((plusp n) "partial")
          (t "unverified"))))

(defun claim-tag (claim)
  (let ((label (if claim (claim-label claim) "unverified")))
    (cond ((equal label "verified") "[verified]")
          ((equal label "partial") "[partial]")
          ((equal label "absence") "[absence]")
          (t "[lead]"))))

(defun claims (project) (getf project :claims))

(defun find-claim (project id)
  (and id (find (string-upcase (nlk:trimmed (princ-to-string id))) (claims project)
                :key (lambda (claim) (getf claim :id)) :test #'equal)))

(defun add-claim (project claim)
  (push claim (getf project :claims))
  claim)

(defun claim-line (claim)
  (format nil "~a ~a~@[: ~a~]~%~@[   quote: \"~a\"~%~]~@[   source: ~{~a~^ , ~}~]"
          (getf claim :id) (claim-tag claim) (getf claim :text)
          (getf (first (getf claim :sources)) :quote)
          (mapcar (lambda (source) (getf source :url)) (getf claim :sources))))

(defun store-source (project claim url quote)
  "Fetch URL, check QUOTE against the page, and record it on CLAIM."
  (let ((url (nonblank url)) (quote (nonblank quote)))
    (unless (and url quote) (fail "a source needs url and quote: the sentence, copied as the page writes it"))
    (unless (search "http" url) (fail "url must be an http or https address"))
    (multiple-value-bind (text status) (fetch-page url)
      (unless (and (integerp status) (<= 200 status 299))
        (fail "~a answered HTTP ~a: a claim needs a page this cell read" url status))
      (unless (stringp text) (fail "~a answered no text" url))
      (let ((text (subseq text 0 (min (length text) (setting :max-fetch-chars)))))
        (unless (quote-in-p quote text)
          (fail "the quote is not in ~a as it now reads (at least ~d characters, folded for case and whitespace): copy the sentence, or log the gap with action=absence"
                url (setting :min-quote-chars)))
        (let ((source (list :url url :quote (nlk:trimmed quote) :domain (domain-of url)
                            :hash (content-hash text) :at (now))))
          (setf (getf claim :sources) (cons source (getf claim :sources)))
          (getf claim :id))))))

(defun next-claim-id (project)
  (let ((n (1+ (or (getf project :next-claim) 0))))
    (setf (getf project :next-claim) n)
    (format nil "C~d" n)))

(defun check-claim (project)
  "PROJECT's claims as the briefing reads them: the ones a check mark or a lead covers."
  (remove-if (lambda (claim) (equal (claim-label claim) "unverified")) (claims project)))

(defun coverage-report (project)
  "The briefing's coverage part: what the tier asked for, and what is there."
  (let* ((tier (setting :tier))
         (need (or (cdr (assoc tier +tiers+ :test #'equal)) 6))
         (verified (remove-if-not (lambda (c) (equal (claim-label c) "verified")) (claims project)))
         (partial (remove-if-not (lambda (c) (equal (claim-label c) "partial")) (claims project)))
         (domains (remove-duplicates
                   (loop for claim in (append verified partial)
                         append (mapcar (lambda (s) (getf s :domain)) (getf claim :sources)))
                   :test #'equal))
         (disconfirmed (find-if (lambda (c) (member (getf c :type) '("competitor_exists" "competitor_absent")
                                                    :test #'equal))
                                (claims project))))
    (list :tier tier :need need :have (+ (length verified) (length partial))
          :verified (length verified) :partial (length partial) :domains (length domains)
          :disconfirmed (and disconfirmed t))))

(defun briefing-gaps (project)
  "Why the briefing cannot be written yet, one line each."
  (let* ((coverage (coverage-report project))
         (gaps '()))
    (when (< (getf coverage :have) (getf coverage :need))
      (push (format nil "~d sourced claim~:p for the ~a tier, ~d of them quoted and checked (~d more)"
                    (getf coverage :need) (getf coverage :tier) (getf coverage :have)
                    (- (getf coverage :need) (getf coverage :have)))
            gaps))
    (when (< (getf coverage :domains) 2)
      (push "claims from at least two different sites, so one page cannot carry the briefing" gaps))
    (unless (getf coverage :disconfirmed)
      (push "a disconfirmation: a competitor claim (action=claim type=competitor_exists) or a logged absence (action=absence)" gaps))
    (nreverse gaps)))

(defun briefing-text (project)
  "PROJECT's research briefing: the claims with their tags, the source log, coverage, and
what was not verified."
  (let* ((coverage (coverage-report project))
         (all (reverse (claims project)))
         (verified (remove-if-not (lambda (c) (equal (claim-label c) "verified")) all))
         (rest (remove-if (lambda (c) (equal (claim-label c) "verified")) all))
         (unverified (remove-if-not (lambda (c) (equal (claim-label c) "unverified")) all)))
    (with-output-to-string (out)
      (format out "# Research briefing: ~a~%Idea: ~a~%~%" (getf project :title)
              (getf project :idea))
      (format out "## Claims a check mark stands on~%~%")
      (if verified
          (dolist (claim verified) (format out "~a~%~%" (claim-line claim)))
          (format out "none yet~%~%"))
      (when rest
        (format out "## Leads and partial claims~%~%")
        (dolist (claim rest) (format out "~a~%~%" (claim-line claim))))
      (when unverified
        (format out "## Not verified~%")
        (dolist (claim unverified)
          (format out "- ~a ~a: no page read yet~%" (getf claim :id) (getf claim :text)))
        (terpri out))
      (format out "## Source log~%")
      (dolist (claim all)
        (dolist (source (getf claim :sources))
          (format out "- ~a ~a (quote checked against the page)~%"
                  (getf claim :id) (getf source :url))))
      (format out "~%## Coverage~%tier ~a: ~d sourced claim~:p (~d checked, ~d partial) from ~d site~:p; disconfirmation: ~a.~%"
              (getf coverage :tier) (getf coverage :have) (getf coverage :verified)
              (getf coverage :partial) (getf coverage :domains)
              (if (getf coverage :disconfirmed) "logged" "none")))))

;;; --- the critic: outside view first, committed before it is shown --------------------

(defparameter +verdicts+ '("go" "go_with_conditions" "pivot" "kill"))

(defun today () (subseq (nlk:iso-now) 0 10))

(defun date-text (value field)
  "VALUE as YYYY-MM-DD, or a refusal naming FIELD."
  (let ((text (and value (nonblank (princ-to-string value)))))
    (unless (and text (>= (length text) 10)
                 (char= (char text 4) #\-) (char= (char text 7) #\-)
                 (every #'digit-char-p (concatenate 'string (subseq text 0 4) (subseq text 5 7)
                                                    (subseq text 8 10))))
      (fail "~a must be a date YYYY-MM-DD, not ~s" field value))
    (subseq text 0 10)))

(defun check-probability (value field problems)
  "VALUE as a probability in 0.02-0.98, pushed onto PROBLEMS when it is not one."
  (let ((p (if (numberp value) (float value)
               (ignore-errors (read-from-string (princ-to-string (or value "")))))))
    (cond ((not (and (numberp p) (<= 0.02 p 0.98)))
           (push (format nil "~a must be a probability between 0.02 and 0.98: certainty is not calibrated"
                         field) problems)
           nil)
          (t (round (* p 1000)) ))))

(defun base-rate-key (key)
  (assoc key +reference-classes+ :test #'equal))

(defun validate-verdict (project data)
  "DATA as a verdict, normalized, or a refusal listing every problem."
  (let ((problems '()))
    (unless (member (getf data :verdict) +verdicts+ :test #'equal)
      (push (format nil "verdict must be one of ~{~a~^, ~}" +verdicts+) problems))
    (unless (nonblank (getf data :thesis))
      (push "thesis: the bet in one sentence" problems))
    (let* ((rc (getf data :reference-class))
           (name (and rc (getf rc :name)))
           (rate (and rc (getf rc :base-rate)))
           (source (and rc (nonblank (getf rc :source)))))
      (cond ((not (and (nonblank name) (nonblank rate) source))
             (push "reference_class needs name, base_rate and source: name the class and its sourced rate before the premortem" problems))
            ((base-rate-key source)
             (setf (getf data :source-detail)
                   (format nil "~a: ~a" (second (base-rate-key source)) (third (base-rate-key source)))))
            ((let ((claim (find-claim project source)))
               (and claim (member (claim-label claim) '("verified" "partial") :test #'equal)))
             (let ((claim (find-claim project source)))
               (setf (getf data :source-detail) (format nil "~a: ~a" (getf claim :id) (getf claim :text)))))
            (t (push (format nil "reference_class.source ~s is neither a base-rate key (~{~a~^, ~}) nor a quoted claim C#"
                             source (mapcar #'car +reference-classes+)) problems))))
    (let ((p (check-probability (getf data :p-success) "p_success" problems)))
      (when p (setf (getf data :p-success) (/ p 1000.0))))
    (let ((premortem (getf data :premortem)))
      (unless (and (listp premortem) (>= (length premortem) 3))
        (push "premortem: at least three ranked causes of failure" problems))
      (let ((total 0.0))
        (loop for item in premortem for n from 0
              do (unless (nonblank (getf item :cause))
                   (push (format nil "premortem[~d] needs a cause" n) problems))
                 (let ((share (and (getf item :p-given-failure)
                                   (ignore-errors (read-from-string (princ-to-string (getf item :p-given-failure)))))))
                   (if (numberp share) (incf total share)
                       (push (format nil "premortem[~d].p_given_failure: this cause's share of failures, 0 to 1" n) problems))))
        (when (> total 1.05)
          (push "premortem shares are conditional on failure and must sum to at most 1" problems))))
    (unless (getf data :must-be-true)
      (push "must_be_true: at least one assumption with test, cost, deadline and kill_threshold" problems))
    (unless (getf data :tripwires)
      (push "tripwires: at least one dated kill or pivot threshold" problems))
    (dolist (key '("builder_fit" "estimate_realism"))
      (unless (nonblank (getf data (intern (string-upcase (substitute #\- #\_ key)) :keyword)))
        (push (format nil "~a is required" key) problems)))
    (let ((predictions (getf data :predictions)))
      (unless (and (listp predictions) (>= (length predictions) 3))
        (push "at least three scoreable predictions: the thesis test, the top premortem cause, time to first release, first traction threshold" problems))
      (loop for item in predictions for n from 0
            do (unless (nonblank (getf item :statement))
                 (push (format nil "predictions[~d] needs a statement that is true or false" n) problems))
               (unless (nonblank (getf item :how))
                 (push (format nil "predictions[~d].how: how the user will check it" n) problems))
               (let ((p (check-probability (getf item :p) (format nil "predictions[~d].p" n) problems)))
                 (when p (setf (getf item :p) (/ p 1000.0))))
               (let ((when (date-text (getf item :resolve-by) (format nil "predictions[~d].resolve_by" n))))
                 (when when
                   (setf (getf item :resolve-by) when)
                   (unless (string> when (today))
                     (push (format nil "predictions[~d].resolve_by must be in the future" n) problems))
                   (setf (getf item :resolve-by) when)))))
    (let ((probs (cons (or (getf data :p-success) 0.0)
                       (loop for item in (getf data :predictions)
                             when (numberp (getf item :p)) collect (getf item :p)))))
      (when (and probs (>= (reduce #'min probs) 0.5))
        (unless (nonblank (getf data :acquiescence-note))
          (push "every probability is at least 0.5: re-examine for acquiescence, then explain in acquiescence_note why that is right" problems))))
    (when problems (fail "the verdict is not ready: ~{~a~^; ~}" problems))
    data))

;;; --- the plan: A, H and W units, checked, with effort against calendar ----------------

(defparameter +max-a-unit-hours+ 16.0)
(defparameter +single-artifact-a-p50+ 8.0)
(defparameter +s2-a-p50+ 50.0)
(defparameter +month-days+ 30.0)

(defparameter +plan-rules+
  "S1. A single plugin, CLI, bot, static site or one-screen tool built by an agent is hours, not
    weeks: agent P50 at most 8 h, or name the unit that causes more (effort_driver).
S2. Agent effort over about 50 h needs scope_reasons, or re-scope.
S3. Months appear only in calendar time, and only from named waits.
S4. Every estimate is a range (p10 below p90); novel work needs p90 at least 1.5 x p50.
S5. Done includes verification: a plan with A units has H units for review and real-use testing.
S6. A W item that depends on nothing starts on day 1 (start_now).
S7. Where your ledger has three or more actuals for a class, its numbers replace the base rate.
Units: A agent-bound active hours (at most 16 each, split bigger ones) with ref_class and
acceptance criteria; H human-bound active hours of the user's time; W external waits in calendar
days. Every green-light criterion becomes a gate with a kill threshold."
  "The plan's rules, as the model reads them.")

(defun get-boolean (object key)
  (let ((value (and (hash-table-p object) (nlk:json-value object :any key))))
    (cond ((eq value t) t)
          ((stringp value) (and (member (string-downcase (nlk:trimmed value))
                                         '("true" "yes" "1") :test #'equal) t))
          ((numberp value) (not (zerop value)))
          (t nil))))

(defun unit-class-row (class) (assoc class +unit-classes+ :test #'equal))

(defun multiplier-factor (multipliers id)
  "MULTIPLIERS as (NAME . FACTOR) pairs; a named, reasoned one only."
  (let ((factor 1.0) (problems '()))
    (dolist (item multipliers)
      (let* ((object (as-object item))
             (name (or (and object (get-text object "name"))
                       (and (stringp item) (nonblank item))))
             (reason (and object (get-text object "reason")))
             (known (and name (assoc (string-downcase name) +multipliers+ :test #'equal))))
        (cond ((not known)
               (push (format nil "~a: each multiplier names one of ~{~a~^, ~}" id
                             (mapcar #'car +multipliers+)) problems))
              ((not reason)
               (push (format nil "~a: multiplier ~a needs a reason" id name) problems))
              (t (setf factor (* factor (second known)))))))
    (values factor problems)))

(defun plan-unit (object n)
  "OBJECT as a unit plist, and the problems its own rules found."
  (let* ((where (format nil "units[~d]" n))
         (id (or (get-text object "id") (format nil "U~d" (1+ n))))
         (class (string-upcase (or (get-text object "class") "")))
         (p10 (get-number object "p10"))
         (p50 (get-number object "p50"))
         (p90 (get-number object "p90"))
         (problems '()))
    (cond
      ((not (member class '("A" "H" "W") :test #'equal))
       (values nil (list (format nil "~a (~a): class is A (agent hours), H (your hours) or W (wait days)" id where))))
      ((not (and p10 p50 p90))
       (values nil (list (format nil "~a: p10, p50 and p90 are numbers" id))))
      ((not (<= p10 p50 p90))
       (values nil (list (format nil "~a: needs p10 at most p50 at most p90" id))))
      ((>= p10 p90)
       (values nil (list (format nil "~a: an estimate is a range (S4): p90 must be above p10" id))))
      (t
       (let ((unit (list :id id :title (or (get-text object "title") "") :class class
                         :ref-class (or (get-text object "ref_class") "")
                         :p10 p10 :p50 p50 :p90 p90
                         :depends-on (get-strings object "depends_on")
                         :start-now (get-boolean object "start_now")
                         :acceptance (get-strings object "acceptance")
                         :multipliers '() :novel (get-boolean object "novel")
                         :note (or (get-text object "note") ""))))
         (unless (plusp (length (getf unit :title)))
           (push (format nil "~a: needs a title" id) problems))
         (when (and (getf unit :novel) (< p90 (* 1.5 p50)))
           (push (format nil "~a: novel work needs p90 at least 1.5 x p50 (S4)" id) problems))
         (when (equal class "A")
           (when (> p50 +max-a-unit-hours+)
             (push (format nil "~a: ~,1f h is over ~,1f h; split it" id p50 +max-a-unit-hours+) problems))
           (let ((row (unit-class-row (getf unit :ref-class))))
             (cond
               ((not row)
                (push (format nil "~a: ref_class is one of ~{~a~^, ~}" id
                              (mapcar #'car +unit-classes+)) problems))
               (t
                (unless (getf unit :acceptance)
                  (push (format nil "~a: acceptance criteria, so done is checkable" id) problems))
                (setf (getf unit :multipliers)
                      (loop for item in (get-list object "multipliers")
                            collect (or (as-object item) item)))
                (multiple-value-bind (factor more) (multiplier-factor (getf unit :multipliers) id)
                  (setf problems (append problems more))
                  (when (> p50 (* (fourth row) factor))
                    (push (format nil "~a: P50 ~,1f h is above the ~a P90 (~,1f h); name a multiplier with a reason, split the unit, or pick the right class"
                                  id p50 (getf unit :ref-class) (fourth row)) problems)))))))
         (values unit problems))))))

(defun topo-order (units)
  "UNITS in dependency order, and the id a cycle runs through (NIL when there is none)."
  (let ((deps (make-hash-table :test #'equal))
        (state (make-hash-table :test #'equal))
        (order '())
        (cycle nil))
    (dolist (unit units) (setf (gethash (getf unit :id) deps) (getf unit :depends-on)))
    (labels ((visit (id)
               (cond ((eq (gethash id state) :done) t)
                     ((eq (gethash id state) :open) (setf cycle id) nil)
                     (t (setf (gethash id state) :open)
                        (dolist (dep (gethash id deps))
                          (unless (visit dep) (return-from visit nil)))
                        (setf (gethash id state) :done)
                        (push id order)
                        t))))
      (dolist (unit units)
        (unless cycle (visit (getf unit :id))))
      (values (nreverse order) cycle))))

(defun unit-days (unit key hours-per-day days-per-week)
  (if (equal (getf unit :class) "W")
      (getf unit key)
      (* (/ (getf unit key) hours-per-day) (/ 7.0 days-per-week))))

(defun compute-plan (units order profile)
  "Effort and calendar from the units, the builder's hours and the order: the code's numbers,
not the model's."
  (let* ((hours-per-day (float (or (cdr (assoc "hours_per_day" profile :test #'equal)) 5)))
         (days-per-week (float (or (cdr (assoc "days_per_week" profile :test #'equal)) 5)))
         (parallel (max 1.0 (float (or (cdr (assoc "parallel_agents" profile :test #'equal)) 1))))
         (speedup (min 2.0 (+ 1.0 (* 0.3 (- parallel 1.0)))))
         (week (/ 7.0 days-per-week))
         (by-id (make-hash-table :test #'equal))
         (out (list :effort-hours '() :calendar-days '())))
    (dolist (unit units) (setf (gethash (getf unit :id) by-id) unit))
    (dolist (key '(:p10 :p50 :p90))
      (let ((agent (reduce #'+ units :key (lambda (u) (if (equal (getf u :class) "A") (getf u key) 0))))
            (human (reduce #'+ units :key (lambda (u) (if (equal (getf u :class) "H") (getf u key) 0))))
            (finish (make-hash-table :test #'equal)))
        (push (list key :agent (round-to agent 1) :human (round-to human 1)
                    :total (round-to (+ agent human) 1))
              (getf out :effort-hours))
        (dolist (id order)
          (let* ((unit (gethash id by-id))
                 (start (reduce #'max (mapcar (lambda (dep) (gethash dep finish))
                                                (getf unit :depends-on))
                                  :initial-value 0.0)))
            (setf (gethash id finish) (+ start (unit-days unit key hours-per-day days-per-week)))))
        (let* ((path (reduce #'max (loop for value being the hash-values of finish collect value)
                             :initial-value 0.0))
               (workload (* (/ (+ (/ agent speedup) human) hours-per-day) week))
               (calendar (max path workload)))
          (push (list key :days (round-to calendar 1)) (getf out :calendar-days))
          (when (eq key :p50)
            (setf (getf out :critical-path-days) (round-to path 1)
                  (getf out :workload-days) (round-to workload 1))
            (let ((end (and (plusp (hash-table-count finish))
                            (reduce (lambda (a b)
                                      (if (>= (gethash a finish) (gethash b finish)) a b))
                                    (loop for id being the hash-keys of finish collect id)))))
              (let ((chain '()) (wait 0.0))
                (loop while end
                      do (let ((unit (gethash end by-id)))
                           (push end chain)
                           (when (equal (getf unit :class) "W") (incf wait (getf unit :p50)))
                           (let ((preds (remove-if-not (lambda (dep) (gethash dep finish))
                                                        (getf unit :depends-on))))
                             (setf end (and preds
                                            (reduce (lambda (a b)
                                                      (if (>= (gethash a finish) (gethash b finish)) a b))
                                                    preds))))))
                (setf (getf out :critical-path) chain
                      (getf out :wait-days-on-path) (round-to wait 1))))))))
    (setf (getf out :effort-hours) (nreverse (getf out :effort-hours))
          (getf out :calendar-days) (nreverse (getf out :calendar-days)))
    (setf (getf out :calendar-drivers)
          (mapcar (lambda (unit)
                    (format nil "~a ~a (~a, ~,1f d)" (getf unit :id) (getf unit :title)
                            (getf unit :class)
                            (unit-days unit :p50 hours-per-day days-per-week)))
                  (subseq (sort (copy-list units) #'>
                                :key (lambda (u) (unit-days u :p50 hours-per-day days-per-week)))
                          0 (min 3 (length units))))
          (getf out :start-today)
          (loop for unit in units
                when (or (getf unit :start-now)
                         (and (null (getf unit :depends-on)) (not (equal (getf unit :class) "A"))))
                  collect (getf unit :id))
          (getf out :assumptions)
          (list :hours-per-day hours-per-day :days-per-week days-per-week
                :parallel-agents parallel))
    out))

(defun round-to (number places)
  (let ((scale (expt 10.0 places)))
    (/ (fround (* number scale)) scale)))

(defun total-of (computed key bucket)
  (getf (cdr (assoc key (getf computed bucket))) (if (equal bucket :effort-hours) :total :days)))

(defun validate-plan (project plan)
  "PLAN as a plan plist and its computed totals, or a refusal listing every problem."
  (let ((problems '())
        (profile (effective-profile project))
        (shape (get-text plan "shape")))
     (unless (assoc shape +shapes+ :test #'equal)
      (push (format nil "shape is one of ~{~a~^, ~}" (mapcar #'car +shapes+)) problems))
    (let* ((objects (get-list plan "units"))
           (units '()))
      (loop for object in objects for n from 0
            do (multiple-value-bind (unit unit-problems) (plan-unit (as-object object) n)
                 (setf problems (append problems unit-problems))
                 (when unit (push unit units))))
      (setf units (nreverse units))
      (unless units (fail "a plan needs units: A agent hours, H your hours, W wait days"))
      (let ((ids (mapcar (lambda (u) (getf u :id)) units)))
        (unless (= (length ids) (length (remove-duplicates ids :test #'equal)))
          (push "unit ids must be distinct" problems))
        (dolist (unit units)
          (let ((unknown (remove-if (lambda (dep) (member dep ids :test #'equal))
                                    (getf unit :depends-on))))
            (when unknown
              (push (format nil "~a: depends on unknown units ~{~a~^, ~}" (getf unit :id) unknown)
                    problems)))
          (when (and (equal (getf unit :class) "W") (null (getf unit :depends-on))
                     (not (getf unit :start-now)))
            (push (format nil "~a: a wait that depends on nothing starts on day 1 (start_now) (S6)"
                          (getf unit :id)) problems)))
        (let ((unknown-dep (remove-if (lambda (unit) (member (getf unit :id) ids :test #'equal)) units)))
          (declare (ignore unknown-dep))))
      (multiple-value-bind (order cycle) (topo-order units)
        (when cycle (push (format nil "dependency cycle through ~a" cycle) problems))
        (let* ((gates (remove-if-not #'hash-table-p (get-list plan "gates")))
               (a-ids (loop for unit in units when (equal (getf unit :class) "A") collect (getf unit :id)))
               (a-p50 (reduce #'+ units :key (lambda (u) (if (equal (getf u :class) "A") (getf u :p50) 0))))
               (driver (or (get-text plan "effort_driver") ""))
               (reasons (get-strings plan "scope_reasons")))
          (unless (and gates (every (lambda (g) (and (get-text g "check") (get-text g "kill_if"))) gates))
            (push "gates: at least one {after, check, kill_if}; every green-light criterion becomes a gate with a kill threshold" problems))
          (when (and a-ids (not (find "H" units :key (lambda (u) (getf u :class)) :test #'equal)))
            (push "plans with agent work need H units for review and real-use testing (S5)" problems))
          (when (and (equal shape "single_artifact") (> a-p50 +single-artifact-a-p50+)
                     (not (member driver a-ids :test #'equal)))
            (push (format nil "S1: a single artifact with ~,1f agent hours (more than ~,1f); set effort_driver to the agent unit that causes it (~{~a~^, ~}) or split the scope"
                          a-p50 +single-artifact-a-p50+ a-ids) problems))
          (when (and (> a-p50 +s2-a-p50+) (null reasons))
            (push (format nil "S2: ~,1f agent hours needs scope_reasons, or re-scope" a-p50) problems))
          (when (and order (null cycle) (null problems))
            (let* ((computed (compute-plan units order profile))
                   (calendar (total-of computed :p50 :calendar-days)))
              (when (> calendar +month-days+)
                (let ((share (/ (getf computed :wait-days-on-path) calendar)))
                  (when (< share 0.5)
                    (push "S3: the calendar runs past a month mainly on active work; months come only from named waits. Re-check the A estimates or the hours per day" problems))))))
          (when problems
            (return-from validate-plan
              (format nil "the plan does not pass the checks: ~{~a~^; ~}" (nreverse problems))))
          (let* ((computed (compute-plan units order profile))
                 (clean (list :shape shape :units (mapcar (lambda (id) (find id units :key (lambda (u) (getf u :id)) :test #'equal)) order)
                              :gates (mapcar (lambda (g) (list :after (get-text g "after")
                                                               :check (get-text g "check")
                                                               :kill-if (get-text g "kill_if")))
                                             gates)
                              :effort-driver driver :scope-reasons reasons)))
             (values clean computed)))))))

(defun render-plan (project plan computed)
  (with-output-to-string (out)
    (format out "# Plan: ~a~%~%Shape: ~a (~a).~%" (getf project :title) (getf plan :shape)
            (or (cdr (assoc (getf plan :shape) +shapes+ :test #'equal)) ""))
    (format out "~%| Unit | Class | P10 / P50 / P90 | Depends on | Start today | Acceptance |~%|---|---|---|---|---|---|~%")
    (dolist (unit (getf plan :units))
      (format out "| ~a ~a | ~a | ~,1f / ~,1f / ~,1f ~a | ~a | ~a | ~a |~%"
              (getf unit :id) (getf unit :title) (getf unit :class)
              (getf unit :p10) (getf unit :p50) (getf unit :p90)
              (if (equal (getf unit :class) "W") "d" "h")
              (or (format nil "~{~a~^, ~}" (getf unit :depends-on)) "-")
              (if (member (getf unit :id) (getf computed :start-today) :test #'equal) "yes" "-")
              (or (format nil "~{~a~^; ~}" (getf unit :acceptance)) "-")))
    (format out "~%Effort: agent ~,1f h (P10-P90 ~,1f-~,1f h), your time ~,1f h.~%"
            (getf (cdr (assoc :p50 (getf computed :effort-hours))) :agent)
            (getf (cdr (assoc :p10 (getf computed :effort-hours))) :agent)
            (getf (cdr (assoc :p90 (getf computed :effort-hours))) :agent)
            (getf (cdr (assoc :p50 (getf computed :effort-hours))) :human))
    (format out "Calendar: ~,1f days (P10-P90 ~,1f-~,1f) at ~,1f h/day; critical path ~a.~%"
            (getf (cdr (assoc :p50 (getf computed :calendar-days))) :days)
            (getf (cdr (assoc :p10 (getf computed :calendar-days))) :days)
            (getf (cdr (assoc :p90 (getf computed :calendar-days))) :days)
            (getf (getf computed :assumptions) :hours-per-day)
            (or (format nil "~{~a~^ -> ~}" (getf computed :critical-path)) "-"))
    (format out "Top calendar drivers: ~{~a~^; ~}.~%" (getf computed :calendar-drivers))
    (format out "~%## Gates~%")
    (dolist (gate (getf plan :gates))
      (format out "- After ~a: ~a. Kill or pivot if ~a.~%"
              (or (getf gate :after) "start") (getf gate :check) (getf gate :kill-if)))
    (when (getf plan :scope-reasons)
      (format out "~%Why this is bigger than a quick build: ~{~a~^; ~}.~%" (getf plan :scope-reasons)))))

;;; --- the ledger and the re-check ----------------------------------------------------

(defun project-predictions (project)
  (remove-if-not (lambda (prediction) (eql (getf prediction :project) (getf project :id)))
                 (getf (state) :predictions)))

(defun prediction-line (prediction)
  (format nil "~a [~a] ~a (p ~,2f, resolve by ~a, how: ~a)~@[ -> ~a~]"
          (getf prediction :id) (getf prediction :origin) (getf prediction :statement)
          (getf prediction :p) (getf prediction :resolve-by) (getf prediction :how)
          (getf prediction :outcome)))

(defun brier (probability outcome)
  (let ((p (float probability)) (o (if outcome 1.0 0.0)))
    (round-to (expt (- p o) 2) 3)))

(defun ledger-rows ()
  "Per unit class, the actual active hours of finished A units: n, P10, P50 and P90."
  (let ((by-class (make-hash-table :test #'equal)))
    (dolist (row (getf (state) :actuals))
      (when (equal (getf row :class) "A")
        (push (float (getf row :actual)) (gethash (getf row :ref-class) by-class '()))))
    (loop for class being the hash-keys of by-class using (hash-value hours)
          collect (let ((sorted (sort (copy-list hours) #'<)))
            (list :class class :n (length sorted)
                  :p10 (quantile sorted 0.1) :p50 (quantile sorted 0.5)
                  :p90 (quantile sorted 0.9))))))

(defun quantile (sorted q)
  (let* ((n (length sorted))
         (position (* (1- n) q))
         (low (floor position))
         (high (min (1+ low) (1- n))))
    (round-to (+ (nth low sorted) (* (- (nth high sorted) (nth low sorted)) (- position low))) 2)))

(defun class-rates ()
  "The base-rate table, with a class's own ledger numbers where it has three or more actuals."
  (let ((ledger (ledger-rows)))
    (loop for row in +unit-classes+
          collect (let ((own (find (first row) ledger :key (lambda (r) (getf r :class)) :test #'equal)))
                    (if (and own (>= (getf own :n) 3))
                        (list (first row) (second row) (getf own :p10) (getf own :p50) (getf own :p90))
                        row)))))

(defun recheck-report (project)
  (let* ((plan (artifact project "plan"))
         (verdict (artifact project "verdict"))
         (predictions (project-predictions project))
         (due (remove-if (lambda (p) (getf p :outcome)) predictions)))
    (with-output-to-string (out)
      (format out "Project #~d \"~a\": ~a, ~d phase~:p done.~%"
              (getf project :id) (getf project :title) (getf project :status)
              (1- (getf project :phase)))
      (when plan
        (let ((computed (getf (getf plan :payload) :computed)))
          (format out "Plan: agent ~,1f h, your time ~,1f h, ~,1f days (P10-P90 ~,1f-~,1f).~%"
                  (getf (cdr (assoc :p50 (getf computed :effort-hours))) :agent)
                  (getf (cdr (assoc :p50 (getf computed :effort-hours))) :human)
                  (getf (cdr (assoc :p50 (getf computed :calendar-days))) :days)
                  (getf (cdr (assoc :p10 (getf computed :calendar-days))) :days)
                  (getf (cdr (assoc :p90 (getf computed :calendar-days))) :days))))
      (when verdict
        (format out "Verdict: ~a, P(success) ~,2f.~%" (getf (getf verdict :payload) :verdict)
                (getf (getf verdict :payload) :p-success)))
      (if predictions
          (progn (format out "Predictions:~%")
                 (dolist (prediction predictions) (format out "- ~a~%" (prediction-line prediction)))
                 (format out "~d still to score; today is ~a.~%" (length due) (today)))
          (format out "No predictions stored yet.~%"))
      (format out "Ledger: ~d actual hour record~:p.~%" (length (getf (state) :actuals)))
      (dolist (row (ledger-rows))
        (format out "- ~a: n = ~d, P10-P50-P90 ~,1f / ~,1f / ~,1f h~%"
                (getf row :class) (getf row :n) (getf row :p10) (getf row :p50) (getf row :p90))))))

(defun score-prediction (arguments)
  (let* ((project (current-project))
         (id (nonblank (get-text arguments "prediction")))
         (prediction (and id (find (string-upcase id) (project-predictions project)
                                   :key (lambda (p) (getf p :id)) :test #'equal)))
         (result (get-text arguments "result")))
    (unless prediction (fail "no prediction ~a in this project: blueprint_recheck action=report lists them" id))
    (unless (member (string-downcase (or result "")) '("hit" "miss" "1" "0" "true" "false") :test #'equal)
      (fail "result is hit or miss: did the statement come true by its resolution date?"))
    (let ((outcome (member (string-downcase result) '("hit" "1" "true") :test #'equal))
          (actual (get-number arguments "actual_hours")))
      (setf (getf prediction :outcome) (if outcome "hit" "miss"))
      (when actual
        (let ((class (or (get-text arguments "ref_class") (getf prediction :ref-class) "scaffold")))
          (push (list :class "A" :ref-class class :estimate (getf prediction :p50)
                      :actual actual :unit (getf prediction :id) :at (now))
                (getf *state* :actuals))))
      (save-state)
      (format nil "~a scored ~a. Brier (model): ~,3f.~@[~%Ledger now: ~{~a~^; ~}.~]"
              (getf prediction :id) (getf prediction :outcome)
              (brier (getf prediction :p) outcome)
              (and actual (mapcar (lambda (row)
                                     (format nil "~a n=~d" (getf row :class) (getf row :n)))
                                   (ledger-rows)))))))

;;; --- status, and the state card -----------------------------------------------------

(defparameter +next-steps+
  '((1 . "blueprint_phase(record brief) and blueprint_profile(confirm), then advance")
    (2 . "blueprint_research(plan, claim, source, absence), then brief, then advance")
    (3 . "blueprint_phase(record variants) with at least three really different ones, then advance")
    (4 . "blueprint_phase(record deepdive) then (record choice) naming the winner and why, then advance")
    (5 . "blueprint_verdict(commit) with the outside view, the premortem and three predictions, then advance")
    (6 . "blueprint_plan(submit or rules), then advance; then blueprint_recheck(report)"))
  "What the next call is, by phase.")

(defun next-step (project)
  (let ((phase (getf project :phase)))
    (if (equal (getf project :status) "planned")
        "blueprint_recheck(report): due predictions and actual hours against the estimate"
        (or (cdr (assoc phase +next-steps+)) "blueprint_phase(action=status)"))))

(defun status-text (&optional (project (current-project)))
  "PROJECT's phase, what its gate still needs, the few rules and the next call."
  (require-running)
  (unless project
    (fail "no project yet: start one with blueprint_phase(action=start, idea=...) or /blueprint start <your idea>"))
  (let* ((phase (getf project :phase))
         (gap (open-items project)))
    (format nil "~a~%~@[Gate still needs: ~{~a~^; ~}.~%~]~a~%~%Next: ~a~%"
            (format nil "Project #~d \"~a\": ~a"
                    (getf project :id) (getf project :title)
                    (if (equal (getf project :status) "planned")
                        "planned, all six phases done"
                        (phase-name phase)))
            gap
            (if (equal (getf project :status) "planned")
                "Re-checks: blueprint_recheck(report)."
                (or (cdr (assoc phase +phase-rules+)) ""))
            (next-step project))))

(defun card-text (project)
  "The short per-turn reminder: the phase, the one thing the gate lacks, the next call."
  (when project
    (let ((gap (open-items project)))
      (format nil "[blueprint] (a note from the cell, not the user) Project #~d \"~a\", ~a.~@[ Gate needs: ~a.~] Next: ~a. (help :blueprint) is the method."
              (getf project :id) (getf project :title)
              (if (equal (getf project :status) "planned") "planned" (phase-name (getf project :phase)))
              (and gap (format nil "~{~a~^; ~}" gap))
              (next-step project)))))

(defun standing-card (next session-id)
  "The harness sections plus this project's card, for the session it was started in (or any)."
  (let ((sections (funcall next session-id))
        (project (and (boundp '*state*) *state* (current-project))))
    (let ((card (and project
                      (or (null (getf project :session)) (equal (getf project :session) session-id))
                      (card-text project))))
      (if card
          (acons "blueprint" card (remove "blueprint" sections :key #'car :test #'equal))
          (remove "blueprint" sections :key #'car :test #'equal)))))

;;; --- the tools the model calls ------------------------------------------------------

(defparameter +phase-schema+
  (list :description
        (concatenate 'string
                     "Blueprint's six gated phases (Understanding, Context, Generation, Deep dive, "
                     "Critique, Plan). Actions: start (idea: bind a new project here); status (the "
                     "phase, what its gate still needs, the next call); record (kind + content: "
                     "brief in phase 1, variants in 3, deepdive and choice in 4 -- the content is "
                     "what the user agreed to, in their words); advance (when the gate is met); "
                     "phase (number: jump, forward only past every gate); list; stop.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties"
         (nlk:make-json-object
          "action" (nlk:make-json-object "type" "string"
                                          "enum" (vector "start" "status" "record" "advance"
                                                          "phase" "list" "stop")
                                          "description" "what to do")
          "idea" (nlk:make-json-object "type" "string"
                                        "description" "start: the raw idea, in the user's words")
          "kind" (nlk:make-json-object "type" "string"
                                        "enum" (vector "brief" "variants" "deepdive" "choice")
                                        "description" "record: which artifact")
          "content" (nlk:make-json-object "description"
                                           "record: the brief as text; variants as a list of {name, summary}; choice as {variant, reason}")
          "number" (nlk:make-json-object "type" "integer"
                                          "description" "phase: the phase to move to, 1-6")
          "reason" (nlk:make-json-object "type" "string"
                                          "description" "phase: why it moved"))
         "required" (vector "action"))))

(defun phase-tool (arguments)
  (answering
    (let ((action (string-downcase (or (get-text arguments "action") "status"))))
      (cond
        ((string= action "start")
         (let ((project (start-project (or (get-text arguments "idea")
                                           (get-text arguments "content")))))
           (setf (getf project :session) (ignore-errors (getf (nle:turn) :session-id)))
           (save-state)
           (format nil "~a~%~%Interview first: one or two questions per turn, no solutions yet. Then record the brief and confirm the builder profile."
                   (status-text project))))
        ((string= action "status") (status-text))
        ((string= action "record")
         (let* ((project (current-project))
                (kind (string-downcase (or (get-text arguments "kind") "")))
                (content (check-record project kind (nlk:json-value arguments :any "content"))))
           (put-artifact project kind "blueprint_phase" content)
           (save-state)
           (format nil "Recorded ~a for project #~d.~%~a" kind (getf project :id) (status-text project))))
        ((string= action "advance")
         (let ((project (current-project)))
           (advance project)
           (save-state)
           (format nil "~a" (status-text project))))
        ((string= action "phase")
         (let ((project (current-project)))
           (go-to project (or (get-number arguments "number") (getf project :phase))
                  (get-text arguments "reason"))
           (save-state)
           (status-text project)))
        ((string= action "list")
         (if (projects)
             (format nil "~{~a~^
~}"
                     (mapcar (lambda (project)
                               (format nil "#~d \"~a\": ~a, phase ~a"
                                       (getf project :id) (getf project :title)
                                       (getf project :status) (getf project :phase)))
                             (projects)))
             "no projects yet"))
        ((string= action "stop")
         (let ((project (current-project)))
           (when project (setf (getf project :status) "stopped"))
           (setf (getf *state* :current) nil)
           (save-state)
           (format nil "~@[~a stopped.~]Nothing is bound to this chat now: /blueprint resume <id> picks one up."
                   (and project (format nil "#~d \"~a\"" (getf project :id) (getf project :title))))))
        (t (fail "action is one of start, status, record, advance, phase, list, stop"))))))

(defparameter +profile-schema+
  (list :description
        (concatenate 'string
                     "Blueprint builder profile: who builds the idea (team, coding agents, hours per "
                     "day, skills, accounts, audience, budget) and the velocity ledger. Actions: get; "
                     "set (profile: fields to merge, e.g. {team, agents, hours_per_day}); confirm "
                     "(snapshot it into this project after the user agreed: phase 1 needs it); ledger "
                     "(actual hours per unit class against the base rates).")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties"
         (nlk:make-json-object
          "action" (nlk:make-json-object "type" "string"
                                          "enum" (vector "get" "set" "confirm" "ledger"))
          "profile" (nlk:make-json-object "type" "object"
                                           "description" "set: the fields to merge")
          "assumed" (nlk:make-json-object "type" "array"
                                           "items" (nlk:make-json-object "type" "string")
                                           "description" "confirm: fields filled by default, labelled as assumptions"))
         "required" (vector "action"))))

(defun profile-tool (arguments)
  (answering
    (let ((action (string-downcase (or (get-text arguments "action") "get"))))
      (cond
        ((string= action "get")
         (format nil "Profile (version ~d): ~a~%Missing: ~{~a~^, ~}~@[~%Fields: ~{~a~^; ~}~]"
                 (or (getf (profile-store) :version) 0)
                 (if (profile-fields) (profile-line (profile-fields)) "nothing set yet")
                 (or (profile-missing) '("nothing"))
                 (and (null (profile-fields))
                      (mapcar (lambda (pair) (format nil "~a (~a)" (car pair) (cdr pair)))
                              +profile-fields+))))
        ((string= action "set")
         (let ((fields (set-profile (get-object arguments "profile"))))
           (format nil "Profile (version ~d): ~a~%Missing: ~{~a~^, ~}"
                   (getf (profile-store) :version) (profile-line fields)
                   (or (profile-missing) '("nothing")))))
        ((string= action "confirm")
         (let ((project (current-project)))
           (unless project (fail "no project here to confirm the profile for: start one first"))
           (let ((missing (profile-missing)))
             (when missing
               (fail "ask the user for the missing fields first, in at most three questions, then set them: ~{~a~^, ~}"
                     missing)))
           (let ((assumed (get-strings arguments "assumed")))
             (put-artifact project "profile_used" "blueprint_profile"
                           (list :version (getf (profile-store) :version)
                                 :fields (copy-alist (profile-fields)) :assumed assumed))
             (save-state)
             (format nil "Profile confirmed for project #~d~@[, with ~{~a~^, ~} assumed~].~%~a"
                     (getf project :id) (and assumed assumed) (status-text project)))))
        ((string= action "ledger")
         (format nil "Unit classes (agent-bound active hours, P10/P50/P90):~%~{~a~^
~}" 
                 (mapcar (lambda (row)
                           (format nil "- ~a: ~a, ~,1f / ~,1f / ~,1f h"
                                   (first row) (second row) (third row) (fourth row) (fifth row)))
                         (class-rates))))
        (t (fail "action is one of get, set, confirm, ledger"))))))

(defun json->plist (value)
  "VALUE as a plist with keyword keys, all the way down: objects become plists, arrays lists."
  (cond ((stringp value) value)
        ((hash-table-p value)
         (loop for key being the hash-keys of value
               append (list (intern (string-upcase (substitute #\- #\_ key)) :keyword)
                            (json->plist (gethash key value)))))
        ((vectorp value) (mapcar #'json->plist (coerce value 'list)))
        (t value)))

(defparameter +research-schema+
  (list :description
        (concatenate 'string
                     "Blueprint research (phase 2). Actions: plan (what to search, by tier); claim "
                     "(text, type, url, quote: this cell fetches the page and stores the claim only "
                     "if the quote is in it); source (id, url, quote: a second independent source "
                     "for a claim); absence (text, searches: where you looked for a competitor or "
                     "a gap -- never a check mark); brief (write the briefing: refuses while too "
                     "little is covered); briefing (show it); sources (the source log).")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties"
         (nlk:make-json-object
          "action" (nlk:make-json-object "type" "string"
                                          "enum" (vector "plan" "claim" "source" "absence"
                                                          "brief" "briefing" "sources"))
          "text" (nlk:make-json-object "type" "string"
                                        "description" "claim: the claim in one sentence; absence: what was searched for")
          "type" (nlk:make-json-object "type" "string"
                                        "enum" (vector "market_size" "traction" "price"
                                                        "competitor_exists" "technical" "policy" "other")
                                        "description" "claim: what kind of claim it is")
          "url" (nlk:make-json-object "type" "string" "description" "the page you read it on")
          "quote" (nlk:make-json-object "type" "string"
                                         "description" "the sentence, copied as the page writes it, at least 20 characters")
          "id" (nlk:make-json-object "type" "string"
                                      "description" "source: the claim it belongs to, C#")
          "searches" (nlk:make-json-object "type" "array"
                                            "items" (nlk:make-json-object "type" "string")
                                            "description" "absence: the searches you ran"))
         "required" (vector "action"))))

(defun research-tool (arguments)
  (answering
    (let* ((action (string-downcase (or (get-text arguments "action") "plan")))
           (project (current-project)))
      (cond
        ((string= action "plan")
         (let ((tier (setting :tier)))
           (format nil "Research for \"~a\" (tier ~a, ~d sourced claims):~%~a~%~%For each: search, read the page, then action=claim with the url and the exact sentence. Market size and traction need two independent sites (action=source for the second). A search snippet is a lead, never a claim.~%"
                   (getf project :idea) tier (or (cdr (assoc tier +tiers+ :test #'equal)) 6)
                   (bulleted (list "who has this problem today, and what they use instead"
                                   "the market: size, growth, who pays, at what price"
                                   "the nearest competitors, by channel: repos, registries, marketplaces, app stores, product lists, forums"
                                   "what the platform or policy requires (accounts, review, fees)"
                                   "the technical unknown: the one thing that must work"
                                   "the disconfirmation: what would show this is not wanted")))))
        ((string= action "claim")
         (unless project (fail "no project yet: start one first"))
         (let* ((type (string-downcase (or (get-text arguments "type") "other")))
                (text (or (get-text arguments "text") (get-text arguments "quote")))
                (claim (list :id (next-claim-id project) :type type :text text :sources '()
                             :at (now) :contested (get-boolean arguments "contested"))))
           (unless (nonblank text) (fail "a claim needs text: the claim in one sentence"))
           (copy-list claim)
           (add-claim project claim)
           (store-source project claim (get-text arguments "url") (get-text arguments "quote"))
           (save-state)
           (format nil "~a~%~a" (claim-line claim)
                   (if (equal (claim-label claim) "verified")
                       "A check mark stands on a page this cell read."
                       "Not a check mark yet: another independent site (action=source) or a fuller quote."))))
        ((string= action "source")
         (let* ((claim (find-claim project (get-text arguments "id"))))
           (unless claim (fail "no claim ~a in this project" (get-text arguments "id")))
           (store-source project claim (get-text arguments "url") (get-text arguments "quote"))
           (save-state)
           (format nil "~a~%~a" (claim-line claim)
                   (format nil "~d independent site~:p of ~d needed."
                           (claim-independent claim) (or (required-sources (getf claim :type)) 0)))))
        ((string= action "absence")
         (unless project (fail "no project yet: start one first"))
         (let ((claim (list :id (next-claim-id project) :type "competitor_absent"
                            :text (or (get-text arguments "text") "searched and found nothing")
                            :searches (get-strings arguments "searches")
                            :sources '() :at (now))))
           (add-claim project claim)
           (save-state)
           (format nil "~a logged with ~d search~:p: an absence stays a lead, never a check mark."
                   (getf claim :id) (length (getf claim :searches)))))
        ((string= action "brief")
         (unless project (fail "no project yet: start one first"))
         (let ((gaps (briefing-gaps project)))
           (when gaps (fail "the briefing is not ready: ~{~a~^; ~}" gaps))
           (let ((text (briefing-text project)))
             (put-artifact project "briefing" "blueprint_research" (list :text text))
             (save-state)
             (format nil "~a~%Show this as it is. blueprint_phase(action=advance) leaves phase 2." text))))
        ((string= action "briefing")
         (let ((briefing (and project (artifact project "briefing"))))
           (unless briefing (fail "no briefing stored yet"))
           (getf (getf briefing :payload) :text)))
        ((string= action "sources")
         (let ((claims (reverse (claims project))))
           (if claims
               (format nil "~{~a~^
~}" (mapcar #'claim-line claims))
               "no claims recorded yet")))
        (t (fail "action is one of plan, claim, source, absence, brief, briefing, sources"))))))

(defparameter +verdict-schema+
  (list :description
        (concatenate 'string
                     "Blueprint critic (phase 5): outside view first, committed before it is shown. "
                     "Actions: base_rates (the named reference classes); commit (verdict, thesis, "
                     "reference_class {name, base_rate, source: a base-rate key or a checked C#}, "
                     "p_success 0.02-0.98, premortem [>=3 {cause, p_given_failure}], must_be_true, "
                     "tripwires, builder_fit, estimate_realism, predictions [>=3 {statement, p, "
                     "resolve_by, how}]); show; pushback (argument: what it answers, and what "
                     "evidence would change it); revise (new_evidence [C# claims recorded after "
                     "the verdict], reason, changes).")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties"
         (nlk:make-json-object
          "action" (nlk:make-json-object "type" "string"
                                          "enum" (vector "base_rates" "commit" "show"
                                                          "pushback" "revise"))
          "verdict" (nlk:make-json-object "type" "string"
                                           "enum" (vector "go" "go_with_conditions" "pivot" "kill"))
          "thesis" (nlk:make-json-object "type" "string" "description" "the bet, in one sentence")
          "reference_class" (nlk:make-json-object "type" "object"
                                                   "description" "{name, base_rate, source}")
          "p_success" (nlk:make-json-object "type" "number"
                                             "description" "P(the thesis holds), 0.02 to 0.98")
          "premortem" (nlk:make-json-object "type" "array"
                                             "items" (nlk:make-json-object "type" "object")
                                             "description" "at least three ranked causes of failure, with their share")
          "must_be_true" (nlk:make-json-object "type" "array"
                                                "items" (nlk:make-json-object "type" "object")
                                                "description" "assumptions with test, cost, deadline, kill_threshold")
          "tripwires" (nlk:make-json-object "type" "array"
                                             "items" (nlk:make-json-object "type" "object")
                                             "description" "dated kill or pivot thresholds")
          "builder_fit" (nlk:make-json-object "type" "string"
                                               "description" "how this builder's profile changes the risks")
          "estimate_realism" (nlk:make-json-object "type" "string"
                                                     "description" "hours against calendar: which waits set the pace")
          "predictions" (nlk:make-json-object "type" "array"
                                               "items" (nlk:make-json-object "type" "object")
                                               "description" "at least three scoreable predictions")
          "acquiescence_note" (nlk:make-json-object "type" "string"
                                                      "description" "required when every probability is at least 0.5")
          "argument" (nlk:make-json-object "type" "string" "description" "pushback: the user's argument")
          "changes" (nlk:make-json-object "type" "object" "description" "revise: the fields that change")
          "new_evidence" (nlk:make-json-object "type" "array"
                                                 "items" (nlk:make-json-object "type" "string")
                                                 "description" "revise: C# claims recorded after the verdict")
          "reason" (nlk:make-json-object "type" "string"))
         "required" (vector "action"))))

(defun add-predictions (project predictions origin)
  (let ((ids '()))
    (dolist (prediction predictions)
      (let ((id (format nil "P~d" (1+ (or (getf (state) :next-prediction) 0)))))
        (setf (getf *state* :next-prediction) (parse-integer id :start 1))
        (push (list :id id :project (getf project :id) :statement (getf prediction :statement)
                    :p (getf prediction :p) :resolve-by (getf prediction :resolve-by)
                    :how (getf prediction :how) :origin origin :outcome nil)
              (getf *state* :predictions))
        (push id ids)))
    (nreverse ids)))

(defun verdict-tool (arguments)
  (answering
    (let ((action (string-downcase (or (get-text arguments "action") "show"))))
      (cond
        ((string= action "base_rates")
         (format nil "Outside views to name before the premortem:~%~{~a~^
~}~%~%Or a checked claim C# for a class that fits better: the source must carry a quote this cell read."
                 (mapcar (lambda (row) (format nil "- ~a: ~a (~a)" (first row) (second row) (third row)))
                         +reference-classes+)))
        ((string= action "commit")
         (let* ((project (current-project)))
           (unless (= (getf project :phase) 5)
             (fail "the verdict belongs to phase 5; the project is in ~a" (phase-name (getf project :phase))))
           (when (artifact project "verdict")
             (fail "a verdict is already committed: present it, and revise only with evidence recorded after it"))
           (let* ((data (json->plist arguments))
                  (clean (validate-verdict project data))
                  (ids (add-predictions project (getf clean :predictions) "verdict")))
             (setf (getf clean :version) 1 (getf clean :prediction-ids) ids)
             (put-artifact project "verdict" "blueprint_verdict" clean)
             (save-state)
             (format nil "Committed ~a at P(success) ~,2f. Stored before it was shown: present it as it is.~%If the user pushes back, call action=pushback; revise only with a C# claim recorded after this commit.~%Predictions: ~{~a~^, ~}."
                     (getf clean :verdict) (getf clean :p-success) ids))))
        ((string= action "show")
         (let ((verdict (and (current-project) (artifact (current-project) "verdict"))))
           (unless verdict (fail "no verdict committed yet"))
           (let ((payload (getf verdict :payload)))
             (format nil "~a, P(success) ~,2f.~%Thesis: ~a~%Outside view: ~a (~a)~%Tripwires: ~{~a~^; ~}~%Predictions:~%~{~a~^
~}"
                     (getf payload :verdict) (getf payload :p-success) (getf payload :thesis)
                     (getf (getf payload :reference-class) :name)
                     (or (getf payload :source-detail) "") (getf payload :tripwires)
                     (mapcar #'prediction-line (project-predictions (current-project)))))))
        ((string= action "pushback")
         (let* ((project (current-project)) (verdict (and project (artifact project "verdict"))))
           (unless verdict (fail "no verdict committed yet"))
           (put-artifact project "pushback" "blueprint_verdict"
                         (list :argument (get-text arguments "argument") :at (now)))
           (save-state)
           (format nil "The stored verdict stands: ~a, P(success) ~,2f, because ~a.~%Restate it once with its strongest reason. An argument alone changes nothing: models flip under pushback about half the time, often to a wrong answer. To change it, fetch the user's source and record it with blueprint_research(action=claim), then revise citing the C#."
                   (getf (getf verdict :payload) :verdict) (getf (getf verdict :payload) :p-success)
                   (or (getf (getf verdict :payload) :thesis) "-"))))
        ((string= action "revise")
         (let* ((project (current-project))
                (verdict (and project (artifact project "verdict"))))
           (unless verdict (fail "no verdict committed yet"))
           (let ((refs (get-strings arguments "new_evidence"))
                 (reason (get-text arguments "reason"))
                 (changes (get-object arguments "changes")))
             (unless (and refs reason changes)
               (fail "revise needs new_evidence (C# claims recorded after the verdict), a reason and changes"))
             (let ((used '()))
               (dolist (ref refs)
                 (let ((claim (find-claim project ref)))
                   (unless claim (fail "~a is no claim id in this project" ref))
                   (unless (string> (getf claim :at) (getf verdict :at))
                     (fail "~a existed before the verdict: it is not new evidence" ref))
                   (unless (member (claim-label claim) '("verified" "partial") :test #'equal)
                     (fail "~a has no checked quote yet" ref))
                   (push (format nil "~a: ~a" (getf claim :id) (getf claim :text)) used)))
               (let* ((merged (copy-list (getf verdict :payload)))
                      (corrected (json->plist changes)))
                 (loop for (key value) on corrected by #'cddr
                       do (setf (getf merged key) value))
                 (setf (getf merged :version) (1+ (or (getf merged :version) 1))
                       (getf merged :changed-because) (list :evidence (nreverse used) :reason reason))
                 (let* ((clean (validate-verdict project merged))
                        (ids (add-predictions project
                                               (getf clean :predictions)
                                               (format nil "verdict v~d" (getf clean :version)))))
                   (setf (getf clean :prediction-ids) (append (getf clean :prediction-ids) ids))
                   (put-artifact project "verdict" "blueprint_verdict" clean)
                   (save-state)
                   (format nil "Revised to version ~d on ~{~a~^; ~}: ~a, P(success) ~,2f."
                           (getf clean :version) (nreverse used) (getf clean :verdict)
                           (getf clean :p-success))))))))
        (t (fail "action is one of base_rates, commit, show, pushback, revise"))))))

(defparameter +plan-schema+
  (list :description
        (concatenate 'string
                     "Blueprint plan (phase 6): work units with P10/P50/P90, checked in code against "
                     "rules S1-S7, with effort and calendar computed here. Actions: rules (the rules "
                     "and the unit classes); submit (plan: {shape, units [{id, title, class A|H|W, "
                     "ref_class (A), p10, p50, p90, depends_on, start_now, acceptance, "
                     "multipliers [{name, reason}], novel, note}], gates [{after, check, kill_if}], "
                     "effort_driver, scope_reasons}); show.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties"
         (nlk:make-json-object
          "action" (nlk:make-json-object "type" "string"
                                          "enum" (vector "rules" "submit" "show"))
          "plan" (nlk:make-json-object "type" "object" "description" "submit: the plan"))
         "required" (vector "action"))))

(defun plan-tool (arguments)
  (answering
    (let ((action (string-downcase (or (get-text arguments "action") "show"))))
      (cond
        ((string= action "rules")
         (format nil "~a~%~%Shapes: ~{~a~^; ~}.~%~%Unit classes (agent-bound active hours):~%~{~a~^
~}~%~%Multipliers: ~{~a~^; ~}.~%Known waits: ~{~a~^; ~}.~%~%Profile in play: ~a"
                 +plan-rules+
                 (mapcar (lambda (pair) (format nil "~a = ~a" (car pair) (cdr pair))) +shapes+)
                 (mapcar (lambda (row) (format nil "- ~a: ~a (~,1f / ~,1f / ~,1f h)"
                                               (first row) (second row) (third row) (fourth row) (fifth row)))
                         (class-rates))
                 (mapcar (lambda (row) (format nil "~a x~,1f" (first row) (second row))) +multipliers+)
                 (mapcar #'car +waits+)
                 (profile-line (effective-profile (current-project)))))
        ((string= action "submit")
         (let* ((project (current-project))
                (plan (as-object (nlk:json-value arguments :object "plan"))))
            (unless plan (fail "submit needs plan: {shape, units, gates, effort_driver, scope_reasons}"))
           (unless (= (getf project :phase) 6)
             (fail "the plan belongs to phase 6; the project is in ~a" (phase-name (getf project :phase))))
           (unless (artifact project "verdict")
             (fail "no critic verdict yet: phase 5 comes first"))
           (multiple-value-bind (clean computed) (validate-plan project plan)
             (when (stringp clean) (return-from plan-tool (refuse "~a" clean)))
             (let ((markdown (render-plan project clean computed)))
               (put-artifact project "plan" "blueprint_plan"
                             (list :plan clean :computed computed :markdown markdown))
               (save-state)
               (format nil "~a~%Recorded. blueprint_phase(action=advance) finishes the plan." markdown)))))
        ((string= action "show")
         (let ((plan (and (current-project) (artifact (current-project) "plan"))))
           (unless plan (fail "no plan stored yet"))
           (getf (getf plan :payload) :markdown)))
        (t (fail "action is one of rules, submit, show"))))))

(defparameter +recheck-schema+
  (list :description
        (concatenate 'string
                     "Blueprint re-check: what the plan predicted against what happened. Actions: "
                     "report (progress, the plan's numbers, predictions and their due dates, the "
                     "velocity ledger); score (prediction: P#, result: hit or miss, and optionally "
                     "actual_hours and ref_class for the ledger).")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties"
         (nlk:make-json-object
          "action" (nlk:make-json-object "type" "string" "enum" (vector "report" "score"))
          "prediction" (nlk:make-json-object "type" "string" "description" "score: P#")
          "result" (nlk:make-json-object "type" "string" "enum" (vector "hit" "miss"))
          "actual_hours" (nlk:make-json-object "type" "number"
                                                "description" "score: the active hours it really took")
          "ref_class" (nlk:make-json-object "type" "string"
                                             "description" "score: the unit class the hours belong to"))
         "required" (vector "action"))))

(defun recheck-tool (arguments)
  (answering
    (let ((action (string-downcase (or (get-text arguments "action") "report"))))
      (cond
        ((string= action "report")
         (let ((project (current-project)))
           (unless project (fail "no project yet"))
           (recheck-report project)))
        ((string= action "score")
         (let ((project (current-project)))
           (unless project (fail "no project yet"))
           (score-prediction arguments)))
        (t (fail "action is one of report, score"))))))

;;; --- the command ------------------------------------------------------------------

(defun subcommand (text word)
  "TEXT past its first WORD, trimmed."
  (nlk:trimmed (subseq text (min (length text) (length word)))))

(defun run-command (args session-id)
  "One /blueprint: start IDEA, status, phase N, list, resume ID, stop, profile, base-rates."
  (handler-case
      (let* ((text (nlk:trimmed (or args "")))
             (head (string-downcase (or (first (uiop:split-string text :separator '(#\Space))) "")))
             (rest (subcommand text head)))
        (cond
          ((zerop (length text)) (status-text))
          ((member head '("start" "new" "idea") :test #'equal)
           (let ((project (start-project rest)))
             (setf (getf project :session) session-id)
             (save-state)
             (status-text project)))
          ((member head '("status" "show") :test #'equal) (status-text))
          ((string= head "phase")
           (let* ((project (current-project))
                  (number (or (and (plusp (length rest))
                                    (ignore-errors (parse-integer rest :junk-allowed t)))
                              (getf project :phase))))
             (go-to project number rest)
             (save-state)
             (status-text project)))
          ((string= head "advance")
           (let ((project (current-project)))
             (advance project)
             (save-state)
             (status-text project)))
          ((string= head "list")
           (phase-tool (nlk:make-json-object "action" "list")))
          ((string= head "resume")
           (let* ((id (and (plusp (length rest)) (ignore-errors (parse-integer rest :junk-allowed t))))
                  (project (and id (find-project id))))
             (unless project (fail "resume needs a project id: /blueprint list shows them"))
             (setf (getf *state* :current) (getf project :id)
                   (getf project :status) "active"
                   (getf project :session) session-id)
             (save-state)
             (status-text project)))
          ((string= head "stop")
           (phase-tool (nlk:make-json-object "action" "stop")))
          ((string= head "profile")
           (profile-tool (nlk:make-json-object "action" "get")))
          ((string= head "recheck")
           (recheck-tool (nlk:make-json-object "action" "report")))
          (t (status-text))))
    (blueprint-error (condition) (format nil "ERROR: ~a" (blueprint-error-detail condition)))
    (error (condition) (format nil "blueprint: ~a" (nlk:one-line (princ-to-string condition))))))

;;; --- the cell ---------------------------------------------------------------------

(nle:define-cell blueprint
  (:section ("blueprint")
    (:guide "nothing is required: start a project from /blueprint start <your idea> or blueprint_phase action=start; tier sets how much research a briefing needs (quick 3, standard 6, deep 10 sourced claims); state_file moves the projects' one file")
    ("state_file" :path
     :doc "where the projects live; empty means blueprint/state.sexp under this home")
    ("tier" :choice :options '("quick" "standard" "deep") :default "standard"
     :doc "how much research a briefing needs: 3, 6 or 10 sourced claims")
    ("min_quote_chars" :integer :default 20 :min 10
     :doc "the shortest quote that can carry a check mark")
    ("max_fetch_chars" :integer :default 400000 :min 1000
     :doc "how much of one page the quote check reads")
    ("timeout_seconds" :integer :default 30 :min 5
     :doc "one page read's ceiling"))
  (:help :blueprint
         "blueprint: six gated phases from a raw idea to a plan you can build - (help :blueprint)"
         +primer+)
  (:command "blueprint" 'run-command
            :description "Blueprint: this idea's phase, its gate and what is missing"
            :argument-hint "start|status|phase|list|resume|stop"
            :session nil)
  (:tool "blueprint_phase" +phase-schema+ 'phase-tool)
  (:tool "blueprint_profile" +profile-schema+ 'profile-tool)
  (:tool "blueprint_research" +research-schema+ 'research-tool)
  (:tool "blueprint_verdict" +verdict-schema+ 'verdict-tool)
  (:tool "blueprint_plan" +plan-schema+ 'plan-tool)
  (:tool "blueprint_recheck" +recheck-schema+ 'recheck-tool)
  (:start (lambda ()
            (setf *state* (load-state))
            (nle:hook 'nle::read-harness-sections "nodecode-blueprint" #'standing-card)
            (nle:on-stop (lambda ()
                           (nle:unhook 'nle::read-harness-sections "nodecode-blueprint")
                           (setf *state* nil))))))
