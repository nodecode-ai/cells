;;;; cell.lisp --- the verbs, /curator, the manual, the section, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model reaches the curator through eval, as it reaches TypeSafe and
;;;; the web: (curator:inventory), (curator:candidates), (curator:judge A B),
;;;; (curator:plans) and (curator:apply-plan ID) in the curator: package the
;;;; cell exports; while the cell runs, (help :jev-curator) answers the
;;;; manual, every request's help section carries one line naming the verbs,
;;;; and /curator says where the key comes from and what the library holds.
;;;;
;;;; An apply never deletes: it moves each absorbed skill's folder into
;;;; <home>/jev-curator/archived/, after re-checking the digest the plan was
;;;; built from, and only while mode is apply.

(in-package #:nodecode-jev-curator)

(defparameter +manual+
  "Skill curation is available through the nodecode-jev-curator cell: typed judgments about how two
SKILL.md skills relate, and hash-bound plans for merging the ones that should be one, over the folders
the section's roots name. Every judgment is asked of TypeSafe's System One (POST /v1/systemone) and
nothing leaves the machine unless mode is not off and the section's egress is true.
  (curator:inventory)   every skill under the roots: name, digest, words, whether it is protected.
  (curator:candidates &key top-k max-pairs)
      the pairs worth judging, best first: the two names, the lexical score, the signals that made
      the pair (name-prefix, name-overlap, description-overlap, content-overlap) and the cheap
      baseline relation. Needs no key and sends nothing.
  (curator:judge A B)
      one judgment of two skills by their names in the inventory: the relation (one of duplicate,
      a_subset_of_b, b_subset_of_a, same_class, complementary, conflict, unrelated,
      insufficient_evidence), its confidence and every option's probability, plus coverage (the
      options did describe the pair), preservation a-in-b and b-in-a (each skill's rules survive in
      the other) and conflict. => a plist; read it with getf:
        (:relation \"duplicate\" :confidence 0.91 :coverage 0.88 :preservation-a-in-b 0.96 ...)
  (curator:plans &key max-plans)
      judges every candidate pair, folds them into the typed graph and answers this scan's merge
      plans, one per line: a content-hash id, keep the canonical skill, absorb the others, and the
      blockers that keep a plan from applying (protected, conflict, stale-hash, low-preservation).
      Absorption is star-shaped: every absorbed skill carries its own direct judgment against the
      canonical, so A~B plus B~C never merges C into A.
  (curator:apply-plan ID)   only while mode is apply: re-checks every digest the plan was built
      from and moves each absorbed skill's folder into <home>/jev-curator/archived/, never deleting
      it; answers what moved and where.
The evidence policy is versioned here, never settable: confidence 0.85, coverage 0.70, conflict at
most 0.15, preservation 0.90. A pair too large for one request is refused, not truncated."
  "What (help :jev-curator) answers while the cell runs.")

;;; --- the library ------------------------------------------------------------------------

(defun state-dir ()
  "The cell's own folder: plans' archive and the apply log."
  (ensure-directories-exist (nlk:home "jev-curator/")))

(defun archive-dir ()
  "Where an apply moves an absorbed skill's folder."
  (ensure-directories-exist (merge-pathnames "archived/" (state-dir))))

(defun skill-by-name (skills name)
  "The skill called NAME, or a refusal naming the library's own names."
  (or (find name skills :test #'string= :key #'skill-name)
      (fail "unknown skill: ~a~%~{the library holds: ~a~^, ~}" name
            (mapcar #'skill-name skills))))

(defun inventory ()
  "Every skill under the roots, one line each, and the roots they came from."
  (let* ((skills (collect-inventory))
         (protected (remove-if-not #'protected-p (mapcar #'skill-name skills)))
         (body (if skills
                   (format nil "~{~a~^~%~}"
                           (loop for skill in skills
                                 collect (format nil "~a  ~a  ~d words~a"
                                                 (skill-name skill)
                                                 (subseq (skill-digest skill) 7 19)
                                                 (length (tokenize (skill-text skill)))
                                                 (if (protected-p (skill-name skill)) "  (protected)" ""))))
                   "  (none)")))
    (format nil "~d skill~:p under ~{~a~^, ~}:~%~a~a"
            (length skills)
            (mapcar (lambda (root) (namestring root)) (skill-roots))
            body
            (if protected
                (format nil "~%~d protected, never proposed: ~{~a~^, ~}" (length protected) protected)
                ""))))

(defun candidates (&key (top-k (setting :top-k)) (max-pairs (setting :max-pairs)))
  "The pairs worth judging, best first, with their signals and baseline."
  (let* ((skills (collect-inventory))
         (pairs (candidate-pairs skills :top-k (or top-k 5) :max-pairs (or max-pairs 100)
                                         :protected (setting :protected))))
    (if (null pairs)
        (format nil "~d skill~:p, no pair worth judging: nothing shares a term or a name prefix"
                (length skills))
        (format nil "~d skill~:p, ~d candidate pair~:p (lexical, no judgment asked):~%~{~a~^~%~}~%~a"
                (length skills) (length pairs)
                (loop for pair in pairs
                      collect (format nil "~a ~a  ~,3f  ~a  baseline ~a"
                                      (candidate-a pair) (candidate-b pair)
                                      (candidate-similarity pair)
                                      (format nil "~{~a~^ ~}" (candidate-signals pair))
                                      (baseline-relation pair)))
                "Judge one with (curator:judge \"NAME-A\" \"NAME-B\"); plan with (curator:plans)."))))

;;; --- judgments --------------------------------------------------------------------------

(defun judgment-line (judgment)
  "One judged pair as the model reads it: the relation, its confidence, and
the three probabilities the evidence policy gates on."
  (format nil "~a ~a: ~a (confidence ~,3f, coverage ~,3f, conflict ~,3f, preservation ~,3f/~,3f)"
          (judgment-a judgment) (judgment-b judgment)
          (judgment-relation judgment) (judgment-confidence judgment)
          (judgment-coverage judgment) (judgment-conflict judgment)
          (judgment-preservation-a-in-b judgment) (judgment-preservation-b-in-a judgment)))

(defun judge (a b)
  "A and B judged; a plist, or a refusal when nothing may leave the machine."
  (let* ((skills (collect-inventory))
         (first (skill-by-name skills a))
         (second (skill-by-name skills b))
         (judgment (if (string< (skill-name first) (skill-name second))
                       (judge-pair first second)
                       (judge-pair second first))))
    (list :a (judgment-a judgment) :b (judgment-b judgment)
          :relation (judgment-relation judgment)
          :confidence (judgment-confidence judgment)
          :coverage (judgment-coverage judgment)
          :preservation-a-in-b (judgment-preservation-a-in-b judgment)
          :preservation-b-in-a (judgment-preservation-b-in-a judgment)
          :conflict (judgment-conflict judgment)
          :evidence (judgment-evidence judgment)
          :contract (copy-seq +contract-version+)
          :line (judgment-line judgment))))

(defun judge-each (skills pairs)
  "The judgments of PAIRS, and the errors each refusal named; the first
refusal of a kind stops the rest, so a scan without a key asks once."
  (let ((judgments '()) (errors '()) (stopped nil) (by-name (make-hash-table :test #'equal)))
    (dolist (skill skills) (setf (gethash (skill-name skill) by-name) skill))
    (dolist (pair pairs)
      (unless stopped
        (handler-case
            (push (judge-pair (gethash (candidate-a pair) by-name)
                               (gethash (candidate-b pair) by-name))
                  judgments)
          (jev-curator-error (condition)
            (push (one-line-of condition) errors)
            (setf stopped t)))))
    (values (nreverse judgments) (nreverse (remove-duplicates errors :test #'equal)))))

;;; --- plans ------------------------------------------------------------------------------

(defparameter *plans* (make-hash-table :test #'equal)
  "The plans the last scan built, by id: what an apply may name.")

(defun remember-plans (plans)
  "The plans this scan built, replacing the last scan's."
  (clrhash *plans*)
  (dolist (plan plans) (setf (gethash (merge-plan-id plan) *plans*) plan))
  plans)

(defun plan-line (plan)
  "PLAN as the model reads it: what merging would keep and absorb, or why it may not."
  (if (string= (merge-plan-status plan) "noop")
      (format nil "~a noop: nothing to merge~a"
              (merge-plan-id plan)
              (if (merge-plan-blockers plan)
                  (format nil " (blocked: ~{~a~^, ~})" (merge-plan-blockers plan))
                  ""))
      (format nil "~a ~a: keep ~s, absorb ~{~s~^, ~}~a"
              (merge-plan-id plan) (merge-plan-status plan)
              (merge-plan-canonical plan) (merge-plan-absorbed plan)
              (if (merge-plan-blockers plan)
                  (format nil "  blocked by ~{~a~^, ~}" (merge-plan-blockers plan))
                  ""))))

(defun plans (&key (max-plans +max-plans+))
  "This scan's merge plans, one per line: the candidates, the judgments and
what the evidence policy makes of them."
  (let* ((skills (collect-inventory))
         (pairs (candidate-pairs skills :top-k (or (setting :top-k) 5)
                                         :max-pairs (or (setting :max-pairs) 100))))
    (multiple-value-bind (judgments errors)
        (if (egress-enabled-p) (judge-each skills pairs) (values '() '()))
      (let* ((built (remember-plans (build-plans skills judgments :max-plans max-plans)))
             (validated (find "validated" built :test #'string= :key #'merge-plan-status)))
        (format nil "~d skill~:p, ~d candidate pair~:p, ~d judged~a~a~a~%~{~a~^~%~}~a~a"
                (length skills) (length pairs) (length judgments)
                (if (plusp (length judgments)) (format nil " -> ~a" (base-url)) "")
                (if errors (format nil "~%~{~a~^~%~}" errors) "")
                (if (egress-enabled-p)
                    ""
                    (format nil "~%egress is off: judging needs egress true and mode other than off"))
                (mapcar #'plan-line built)
                (if validated
                    (format nil "~%Apply one while mode is apply: (curator:apply-plan \"ID\"); it keeps the canonical and moves the others into the archive.")
                    "")
                (if validated
                    (format nil "~%the archive is ~a" (namestring (archive-dir)))
                    ""))))))

;;; --- apply -------------------------------------------------------------------------------

(defun current-digests (skills names)
  "A name -> digest alist for NAMES, or the refusal naming the one missing."
  (loop for name in names
        collect (cons name (skill-digest (skill-by-name skills name)))))

(defun digests-match-p (expected skills)
  "Whether every name in EXPECTED is still at the digest it was judged at."
  (every (lambda (pair)
           (let ((skill (find (car pair) skills :test #'string= :key #'skill-name)))
             (and skill (string= (skill-digest skill) (cdr pair)))))
         expected))

(defun move-to-archive (skill)
  "MOVE SKILL's folder into the archive, never deleting it; answers the new
pathname."
  (let* ((folder (uiop:pathname-directory-pathname (skill-path skill)))
         (target (merge-pathnames (format nil "~a-~d/" (skill-name skill) (get-universal-time))
                                  (archive-dir))))
    (handler-case (nlk:replace-file folder target)
      ;; A rename cannot cross a filesystem, and the archive often is one:
      ;; copy the tree, then take the original away, which is the same move.
      (error ()
        (nlk:copy-tree-preserving folder target)
        (nlk:remove-tree folder)))
    target))

(defun note-apply (text)
  "One line appended to the apply log, so an operator can see what moved."
  (ignore-errors
    (with-open-file (out (merge-pathnames "apply.log" (state-dir))
                          :direction :output :if-exists :append :if-does-not-exist :create
                          :external-format :utf-8)
      (format out "~d ~a~%" (get-universal-time) text))))

(defun apply-plan (id)
  "Apply the plan ID: verify, then move each absorbed skill's folder into the
archive. Only while mode is apply, and never for a blocked or stale plan."
  (unless (string= (mode) "apply")
    (fail "apply needs mode: apply (it is ~a); set it before an apply moves a folder" (mode)))
  (let* ((plan (gethash id *plans*))
         (skills (collect-inventory)))
    (unless plan
      (fail "unknown plan ~a: run (curator:plans) first; a plan is bound to the digests it was built from"
            id))
    (unless (string= (merge-plan-status plan) "validated")
      (fail "plan ~a is ~a~a" id (merge-plan-status plan)
            (if (merge-plan-blockers plan)
                (format nil ": ~{~a~^, ~}" (merge-plan-blockers plan))
                "")))
    (let ((expected (cons (cons (merge-plan-canonical plan) (merge-plan-canonical-digest plan))
                          (merge-plan-absorbed-digests plan))))
      (unless (digests-match-p expected skills)
        (fail "a skill changed after the plan was built: rescan with (curator:plans)"))
      (dolist (name (merge-plan-absorbed plan))
        (when (protected-p name) (fail "~a is protected, never moved: rescan" name))))
    (let ((moved '()))
      (handler-case
          (dolist (name (merge-plan-absorbed plan))
            (let* ((skill (skill-by-name skills name))
                   (target (move-to-archive skill)))
              (push (format nil "~a -> ~a" name (namestring target)) moved)))
        (error (condition)
          (note-apply (format nil "~a failed after ~{~a~^, ~}: ~a" id moved condition))
          (fail "plan ~a stopped after ~{~a~^, ~}: ~a; nothing else moved" id moved condition)))
      (let ((text (format nil "plan ~a applied: kept ~s, archived ~{~a~^; ~}; the folders sit under ~a"
                          id (merge-plan-canonical plan) (nreverse moved)
                          (namestring (archive-dir)))))
        (note-apply text)
        (clrhash *plans*)
        text))))

;;; --- /curator ----------------------------------------------------------------------------

(defun key-note ()
  "Where a judgment's key would come from, in a phrase; no network."
  (multiple-value-bind (key source) (api-key)
    (cond ((and (null key) (own-base-p)) "no key, none sent")
          ((null key) (format nil "no key (make one at ~a, then set ~a or save api_keys.typesafe in auth.json)"
                              +key-page+ +env+))
          ((eq source :auth) "key from auth.json")
          (t (format nil "key from ~a" +env+)))))

(defun status ()
  "Where the library, the mode and the key stand, in one line; no network."
  (let* ((skills (collect-inventory))
         (protected (remove-if-not #'protected-p (mapcar #'skill-name skills))))
    (format nil "curator: mode ~a, egress ~:[off~;on~], ~d skill~:p~a, ~:d plan~:p remembered; ~a; ~a"
            (mode) (egress-enabled-p) (length skills)
            (if protected (format nil " (~d protected)" (length protected)) "")
            (hash-table-count *plans*)
            (key-note) (base-url))))

(defun panel-run (label line detail)
  "A row of the panel whose Enter runs LINE."
  (list :label label :run (list :enter line) :detail detail))

(defun panel (summary)
  "The dialog bare /curator answers."
  (nle:list-dialog
   :title "Jev curator" :summary summary
   :rows (list (panel-run "Scan candidate pairs" "/curator scan"
                          "the pairs worth judging, with their signals")
               (panel-run "Build merge plans" "/curator plans"
                          "judge every candidate and fold the plan, hash-bound")
               (panel-run "What the library holds" "/curator inventory"
                          "every skill under the roots, its digest and size"))))

(defun run-slash (args session-id)
  "/curator typed alone: where the library stands, and its panel; a verb runs
one of its lines."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (verb (string-downcase text)))
    (cond ((equal verb "") (let ((line (status))) (values line (panel line))))
          ((equal verb "status") (status))
          ((equal verb "inventory") (inventory))
          ((equal verb "scan") (candidates))
          ((equal verb "plans") (plans))
          ((uiop:string-prefix-p "apply " verb)
           (apply-plan (nlk:trimmed (subseq text 6))))
          ((uiop:string-prefix-p "judge " verb)
           (let* ((words (remove "" (uiop:split-string (subseq text 6) :separator '(#\Space))
                                 :test #'equal))
                  (judgment (apply #'judge words)))
             (getf judgment :line)))
          (t (values (format nil "curator: ~a is not one of its choices: status, inventory, scan, plans, judge A B, apply ID"
                             text)
                     (panel (status)))))))

;;; --- the model's tool --------------------------------------------------------------------

(nlk:define-contract skill-relations
  (:doc "Judge how two skills in the configured SKILL.md library relate, or list the pairs worth judging and the merge plans the evidence supports. Use it before editing, merging or deleting skills: a judgment names duplicate, subset, same-class, complementary, conflict or unrelated, with the coverage, preservation and conflict probabilities behind it.")
  ("skill" :text :doc "one skill name, to narrow the scan to the pairs it is part of")
  ("a" :text :doc "the first skill of a pair to judge")
  ("b" :text :doc "the second skill of a pair to judge")
  ("use_jev" :boolean :default t
             :doc "ask the judgment endpoint; false lists the pairs the lexical signals alone propose"))

(defun skill-relations-tool (call)
  (handler-case
      (let ((a (skill-relations-a call))
            (b (skill-relations-b call)))
        (cond ((and a b) (getf (judge a b) :line))
              ((or a b) (candidates))
              ((skill-relations-use-jev call) (plans))
              (t (candidates))))
    (jev-curator-error (condition)
      (nle:failure (format nil "ERROR: JEV-CURATOR-ERROR: ~a" condition)))
    (error (condition)
      (nle:failure (format nil "ERROR: JEV-CURATOR-ERROR: ~a" condition)))))

;;; --- start -------------------------------------------------------------------------------

(defun start ()
  "A root must be named; the rest of the section comes from its defaults."
  (unless (setting :roots)
    (error 'nlk:config-refusal
           :detail "jev-curator: roots is required: name at least one folder that holds SKILL.md files"))
  (clrhash *plans*)
  (nle:on-stop (lambda () (clrhash *plans*))))

(nle:define-cell jev-curator
  (:section ("jev-curator")
    (:guide "roots lists the folders that hold SKILL.md files; egress says whether both skills' text may be sent to TypeSafe for a judgment; mode is off (no judgments), observe (judge and plan) or apply (an apply may also move a folder); protected names the skills an apply never touches; base_url and model are optional")
    ("roots" :list :required t
             :doc "folders scanned for SKILL.md files, recursively")
    ("mode" :choice :options '("off" "observe" "apply") :default "observe"
            :doc "off asks nothing; observe judges and plans; apply may also move an absorbed skill's folder")
    ("egress" :boolean :default nil
              :doc "consent to send both skills' text, redacted of this cell's key, to the judgment endpoint")
    ("protected" :list
                 :doc "skill names an apply never moves (skills a cron job or another cell relies on)")
    ("base_url" :string :default ""
                :doc "the judgment API root; empty is TYPESAFE_BASE_URL, else https://api.typesafe.ai")
    ("model" :string :default ""
             :doc "the judgment model; empty is TYPESAFE_DEFAULT_MODEL, else jev-latest")
    ("top_k" :integer :default 5 :min 1
             :doc "lexical neighbours kept per skill, so how many pairs one scan judges")
    ("max_pairs" :integer :default 100 :min 1
                 :doc "the most candidate pairs one scan considers"))
  (:tool "skill_relations" (nlk:contract 'skill-relations) 'skill-relations-tool)
  (:command "curator" 'run-slash
            :description "Skill relations: inventory, candidates, plans, apply"
            :argument-hint "[STATUS|INVENTORY|SCAN|PLANS|JUDGE A B|APPLY ID]")
  (:help :jev-curator
         "jev-curator: typed skill-relation judgments and hash-bound merge plans - (curator:plans)"
         +manual+)
  (:start #'start))
