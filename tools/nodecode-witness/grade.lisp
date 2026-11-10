;;;; grade.lisp --- the evidence gate: a kept skill graded against the session's own tool trail.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here asks a model anything. The trail is what the store already
;;;; journalled -- each tool result in this session, with the arguments of
;;;; the call that made it -- and the rules are the original plugin's, one for
;;;; one: an unresolved failure when the skill was written, the same error
;;;; twice, a verify script written and never run, a "verified" claim with no
;;;; passing command behind it, a high-impact action that drops one of the
;;;; operator's own recorded rules, placeholders, unsupported confidence.
;;;; A rule found is a finding with a weight; the weight is the grade.

(in-package #:nodecode-witness)

(defparameter +meta-tools+ '("witness" "help" "index")
  "Tools that read or judge rather than work: they are not evidence either way.")

(defparameter +weights+ '(("unresolved-failure" . 4)
                          ("omits-user-constraint" . 4)
                          ("verification-never-executed" . 3)
                          ("claims-verified-without-evidence" . 2)
                          ("repeated-error" . 2)
                          ("placeholders" . 2)
                          ("overconfident-language" . 1)))

(defparameter +verify-command+
  "(pytest|npm (run )?test|go test|cargo test|make test|unittest|verify|--check|lint|tsc\\b)"
  "A command whose passing is evidence: the same list the original read.")

(defparameter +error-text+
  "(Traceback \\(most recent call last\\)|command not found|No such file or directory|Permission denied|HTTP[ /]*[45]\\d\\d|ModuleNotFoundError|ECONNREFUSED|timed out|FAILED|AssertionError|fatal:|error:)"
  "Error text a result may carry when the store's failed bit is not enough.")

(defparameter +high-impact+
  '(("push" "\\bgit\\s+push\\b")
    ("merge" "\\bgit\\s+merge\\b|\\bgh\\s+pr\\s+merge\\b")
    ("deploy" "\\b(deploy|kubectl\\s+apply|terraform\\s+apply|helm\\s+upgrade)\\b")
    ("delete" "\\brm\\s+-rf?\\b|\\bDROP\\s+(TABLE|DATABASE)\\b|\\bkubectl\\s+delete\\b|--force\\b")
    ("publish" "\\b(npm|cargo|twine)\\s+publish\\b|\\bpip\\s+upload\\b"))
  "The actions whose side effect a user's own rule is most often about.")

(defparameter +constraint-word+ "\\b(only|never|always|must|do not|don't|except|unless|before)\\b")
(defparameter +constraint-key+ "\\b(main|master|develop|dev|staging|prod|production|release|hotfix|branch\\w*|review|approval|confirm\\w*|ask)\\b")
(defparameter +environments+ "main|master|develop|dev|staging|prod|production|release|hotfix")
(defparameter +only-rule+ (format nil "\\bonly\\b[^.\\n]{0,40}?\\b(~a)\\b" +environments+))
(defparameter +never-rule+ "\\b(never|do not|don't)\\b[^.\\n]{0,40}?\\b(main|master|production|prod|staging)\\b")
(defparameter +verify-claim+ "\\b(verified|validated|proven|tested|battle-tested|reliable|guaranteed)\\b")
(defparameter +overconfident+ "(always works|never fails|100%|guarantee[sd]?|maximi[sz]e success|bulletproof)")
(defparameter +placeholder+ "\\b(TODO|FIXME|XXX|REPLACE_ME|CHANGEME)\\b|<your[-_ ]|\\byour[-_]api[-_]key\\b")
(defparameter +script-ref+ "(?:scripts?/)?([\\w.-]+\\.(?:sh|py|js|ts|rb))")

(defvar *scanners* (make-hash-table :test #'equal)
  "pattern -> the same pattern with (?i) in front, kept once.")

(defun ci-pattern (pattern)
  "PATTERN with cl-ppcre's inline case-insensitive flag in front."
  (or (gethash pattern *scanners*)
      (setf (gethash pattern *scanners*) (concatenate 'string "(?i)" pattern))))

(defun ci-scan (pattern text)
  "Whether PATTERN matches TEXT, case-insensitively."
  (ppcre:scan (ci-pattern pattern) text))

;;; --- the trail ---------------------------------------------------------------------------

(defun meta-tool-p (tool)
  (member (string-downcase (or tool "")) +meta-tools+ :test #'string=))

(defun trail-for (session-id)
  "SESSION-ID's tool results, oldest first, each (:tool :args :result
:failed): every result with the arguments of the call that made it."
  (when (and session-id (nlk:store-open-p))
    (let ((calls (make-hash-table :test 'equal)))
      (loop for payload in (nlk:events :session-id session-id
                                       :kind nlk::+kind-tool-call-started+ :as :payloads)
            do (setf (gethash (or (nlk:json-value payload :string "call-id") "") calls) payload))
      (loop for payload in (nlk:events :session-id session-id
                                       :kind nlk::+kind-tool-result+ :as :payloads)
            for call-id = (or (nlk:json-value payload :string "call-id") "")
            for call = (gethash call-id calls)
            for result = (or (nlk:json-value payload :string "result") "")
            collect (list :tool (or (nlk:json-value payload :string "tool-name") "")
                          :args (or (and call (nlk:json-value call :string "arguments")) "")
                          :result result
                          :failed (or (and (nlk:json-value payload :boolean "failed") t)
                                      (and (ci-scan +error-text+ (subseq result 0 (min 6000 (length result))))
                                           t)))))))

(defun work-events (trail)
  (remove-if (lambda (event) (meta-tool-p (getf event :tool))) trail))

(defun error-signature (text)
  "TEXT with ids, numbers and paths folded to #, one line, clipped: the key
the same failure signs with however its details differ."
  (let ((clean (string-trim " " (ppcre:regex-replace-all
                                  "[0-9a-f]{6,}|[0-9]+|/[\\w./-]+"
                                  (string-downcase (or text "")) "#"))))
    (subseq clean 0 (min 80 (length clean)))))

(defun session-outcome (trail)
  "How a session ended, from its work events only: :failed, :succeeded, :neutral."
  (let ((work (work-events trail)))
    (cond ((null work) :neutral)
          ((getf (car (last work)) :failed) :failed)
          ((and (>= (length work) 4)
                (>= (* 2 (count-if (lambda (event) (getf event :failed)) work))
                    (length work)))
           :failed)
          (t :succeeded))))

(defun usage-stats (name)
  "The sessions that read NAME through help, and among the newest 20 how
many ended on a failed tool call. => (values USES OK FAIL)."
  (let* ((sessions (remove-duplicates
                     (loop for line in (nlk:knowledge-ledger-lines)
                           when (and (equal "view" (nlk:json-value line :string "kind"))
                                     (string-equal name (or (nlk:json-value line :string "name") "")))
                             collect (nlk:json-value line :string "session"))
                     :test #'equal))
         (recent (last sessions (min (length sessions) 20)))
         (outcomes (mapcar (lambda (session) (session-outcome (trail-for session))) recent))
         (fail (count :failed outcomes)))
    (values (length recent) (- (length recent) fail) fail)))

(defun degraded-p (record)
  (let ((uses (or (getf record :uses) 0)))
    (and (>= uses (setting :min-uses))
         (>= (* 100 (or (getf record :fail) 0)) (* (setting :degraded-percent) uses)))))

;;; --- the operator's own rules ---------------------------------------------------------

(defun entry-text (entry)
  "ENTRY's definition as it stands in its file, read by the span the index kept."
  (let* ((text (or (nlk:read-text (nlk:knowledge-entry-path entry)) ""))
         (start (min (nlk:knowledge-entry-start entry) (length text)))
         (end (min (nlk:knowledge-entry-end entry) (length text))))
    (subseq text start end)))

(defun constraints-for (&optional session-id)
  (declare (ignore session-id))
  "The operator's recorded rules: the lines of the memory definitions they
kept that state one. SESSION-ID is the reading session."
  (let ((lines '()))
    (dolist (entry (nlk:knowledge-entries))
      (when (member (nlk:knowledge-entry-operator entry) '("DEFINE-MEMORY" "DEFINE-SKILL")
                    :test #'string=)
        (let ((text (ignore-errors (entry-text entry))))
          (when text
            (loop for line in (ppcre:split "[\\n.;]" text)
                  for clean = (string-trim " -*\t" line)
                  when (and (<= 12 (length clean) 220)
                            (ci-scan +constraint-word+ clean)
                            (ci-scan +constraint-key+ clean))
                    do (push clean lines))))))
    (last (remove-duplicates (nreverse lines) :test #'string=) (min 40 (length lines)))))

;;; --- the rules ---------------------------------------------------------------------------

(defun last-work-finding (window)
  (let ((last (car (last window))))
    (when (and last (getf last :failed))
      (cons "unresolved-failure"
            (format nil "last tool call before saving failed: ~a -> ~a"
                    (getf last :tool)
                    (let ((sig (error-signature (getf last :result))))
                      (if (plusp (length sig)) sig "error")))))))

(defun repeated-error-finding (window)
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (event window)
      (when (getf event :failed)
        (let ((sig (error-signature (getf event :result))))
          (unless (zerop (length sig))
            (setf (gethash sig counts) (1+ (gethash sig counts 0)))))))
    (loop for sig being the hash-keys of counts using (hash-value n)
          when (>= n 2)
            return (cons "repeated-error" (format nil "same error ~ax: ~a" n sig)))))

(defun script-names (text)
  "Every script file TEXT names, lowercased."
  (loop for match in (ppcre:all-matches-as-strings (ci-pattern +script-ref+) text)
        for name = (string-downcase match)
        collect (if (uiop:string-prefix-p "scripts/" name) (subseq name 8)
                    (if (uiop:string-prefix-p "script/" name) (subseq name 7) name))))

(defun verification-findings (text window)
  "(values UNRUN-DETAIL POSITIVE): a verify script written and never run,
and whether a passing verify command stands in the window."
  (let* ((names (script-names text))
         (ran (format nil "~{~a~^ ~}" (loop for event in window
                                            when (not (getf event :failed))
                                              collect (string-downcase (or (getf event :args) "")))))
         (unrun (sort (remove-if-not (lambda (name)
                                       (and (or (search "verif" name) (search "test" name)
                                                (search "check" name))
                                            (not (search name ran))))
                                     (remove-duplicates names :test #'string=))
                      #'string<))
         (positive (loop for event in window
                         thereis (and (not (getf event :failed))
                                      (ci-scan +verify-command+ (or (getf event :args) ""))))))
    (values (and unrun
                 (cons "verification-never-executed"
                       (format nil "~{~a~^, ~} was written but never run successfully in this session" unrun)))
            positive)))

(defun constraint-finding (text constraints)
  "The first rule of the operator's that a high-impact action in TEXT drops."
  (let ((only-scanner (ci-pattern +only-rule+))
        (never-scanner (ci-pattern +never-rule+)))
   (dolist (impact +high-impact+)
    (when (ci-scan (second impact) text)
      (let* ((lines (loop for line in (ppcre:split "\\n" text)
                          when (ci-scan (second impact) line)
                            collect (string-downcase line)))
             (low (string-downcase text)))
        (dolist (rule constraints)
          (multiple-value-bind (match groups) (ppcre:scan-to-strings only-scanner rule)
            (declare (ignore match))
            (let ((only (and groups (aref groups 0))))
              (when (and only (not (search (string-downcase only) low)))
                (return-from constraint-finding
                  (cons "omits-user-constraint"
                        (format nil "skill performs '~a' but never mentions '~a' from the rule: ~s"
                                (first impact) (string-downcase only)
                                (subseq rule 0 (min 110 (length rule)))))))))
          (multiple-value-bind (match groups) (ppcre:scan-to-strings never-scanner rule)
            (declare (ignore match))
            (let ((forbidden (and groups (aref groups 1))))
              (when (and forbidden
                         (some (lambda (line) (search (string-downcase forbidden) line)) lines))
                (return-from constraint-finding
                  (cons "omits-user-constraint"
                        (format nil "skill performs '~a' on '~a', which the rule forbids: ~s"
                                (first impact) (string-downcase forbidden)
                                (subseq rule 0 (min 110 (length rule)))))))))))))
   nil))

(defun grade-skill (text trail constraints)
  "Grade TEXT, a skill's definition, against TRAIL and the operator's
CONSTRAINTS. => (values GRADE SCORE FINDINGS POSITIVE), FINDINGS a list of
(rule detail weight) and GRADE one of A, B, C, D."
  (let* ((work (work-events trail))
         (window (last work (min (length work) 12)))
         (findings '())
         (tally (lambda (pair weight)
                  (when pair (push (list (car pair) (cdr pair) weight) findings)))))
    (multiple-value-bind (unrun positive) (verification-findings text window)
      (funcall tally (last-work-finding window) 4)
      (funcall tally (repeated-error-finding window) 2)
      (funcall tally unrun 3)
      (when (and (ci-scan +verify-claim+ text)
                 (not positive) (null unrun))
        (funcall tally (cons "claims-verified-without-evidence"
                              "skill claims it is verified or reliable; no passing test or verify command in the trail")
                  2))
      (funcall tally (constraint-finding text constraints) 4)
      (when (ci-scan +placeholder+ text)
        (funcall tally (cons "placeholders" "contains TODO or placeholder text") 2))
      (when (and (not positive) (ci-scan +overconfident+ text))
        (funcall tally (cons "overconfident-language" "confident claims with no supporting evidence") 1))
      (let* ((score (reduce #'+ findings :key #'third :initial-value 0))
          (recovered (and window (some (lambda (event) (getf event :failed)) window)
                          (not (getf (car (last window)) :failed))))
          (score (max 0 (- score (cond (positive 2) (recovered 1) (t 0)))))
          (grade (cond ((>= score 4) "D")
                       ((>= score 2) "C")
                       (positive "A")
                       (t "B"))))
        (values grade score (nreverse findings) (and positive t))))))

;;; --- grading a kept skill -------------------------------------------------------------

(defun prose-skill-p (form)
  "True for (define-skill NAME \"one sentence\" ...): the prose shape, one
file in a knowledge cell, not the procedure the layer would run."
  (and (consp form) (symbolp (first form))
       (string= (symbol-name (first form)) "DEFINE-SKILL")
       (stringp (third form))))

(defun skill-name-of (form)
  (string-downcase (string (second form))))

(defun grade-named (name &key session-id (text nil))
  "Grade the kept skill NAME (or TEXT, when given) and keep the record.
=> the record, or NIL when no such skill is kept."
  (running-settings)
  (let* ((entry (and (null text) (nlk:knowledge-entry-named name session-id :data t)))
         (body (or text (and entry (entry-text entry))))
         (old (record-for name)))
    (unless body
      (fail "no skill named ~a is kept; (index \"\") lists what is" name))
    (when (and old (equal (getf old :status) "trusted"))
      (return-from grade-named old))
    (multiple-value-bind (grade score findings positive)
        (grade-skill body (trail-for session-id) (constraints-for session-id))
      (multiple-value-bind (uses ok fail) (usage-stats name)
        (let* ((record (list :grade grade :score score :findings findings :positive positive
                              :status "active" :rel (and entry (namestring (nlk:knowledge-entry-path entry)))
                              :qpath nil :uses uses :ok ok :fail fail
                              :created (or (getf old :created) (get-universal-time))))
               (record (if (equal grade "D") (gate name entry record) record)))
          (setf (record-for name) record)
          (ledger-append (list :type "grade" :ts (get-universal-time)
                               :session (or session-id "") :skill name :grade grade
                               :score score :status (getf record :status)))
          (when (member grade '("C" "D") :test #'string=)
            (notice-grade name record))
          record)))))

(defun gate (name entry record)
  "Mode gate: a grade-D skill's file leaves the index for the quarantine
folder beside its cell. Nothing moves when there is no file to move."
  (if (and (equal (setting :mode) "gate") entry)
      (handler-case
          (let* ((path (nlk:knowledge-entry-path entry))
                 (folder (uiop:ensure-directory-pathname (nlk:knowledge-entry-folder entry)))
                 (target (merge-pathnames (format nil "witness-quarantine/~a-~d.lisp"
                                                  name (get-universal-time))
                                           folder)))
            (ensure-directories-exist target)
            (rename-file path target)
            (setf (getf record :status) "quarantined"
                  (getf record :qpath) (namestring target))
            record)
        (error () record))
      record))

(defun notice-grade (name record)
  "Say in the model's prompt what the grade found, until the operator acts."
  (let ((why (or (second (first (getf record :findings)))
                 (format nil "~a of ~a uses ended badly" (getf record :fail) (getf record :uses)))))
    (nle:notice (format nil "witness: skill '~a' graded ~a and ~a: ~a. Do not claim it is verified; report this to the operator."
                        name (getf record :grade)
                        (if (equal (getf record :status) "quarantined")
                            "was QUARANTINED" "is flagged")
                        why)
                :level :warning :key "nodecode-witness")))

;;; --- the report ------------------------------------------------------------------------

(defun record-row (name record)
  (format nil "~a  ~a  ~a~@[ ~a~]~@[ DEGRADED~]"
          (getf record :grade) name
          (if (plusp (or (getf record :uses) 0))
              (format nil "~a/~a ok" (getf record :ok) (getf record :uses))
              "unused")
          (if (equal (getf record :status) "quarantined") "QUARANTINED"
              (if (equal (getf record :status) "trusted") "trusted" nil))
          (and (degraded-p record) t)))

(defun report-text ()
  "Every skill graded so far, worst first."
  (running-settings)
  (let ((sorted (sort (loop for name being the hash-keys of (records) using (hash-value record)
                            collect (cons name record))
                      #'string< :key #'car)))
    (setf sorted (stable-sort sorted (lambda (a b)
                                       (string< (or (getf (cdr a) :grade) "Z")
                                                (or (getf (cdr b) :grade) "Z")))))
    (if sorted
        (format nil "~{~a~^~%~}" (loop for (name . record) in sorted collect (record-row name record)))
        "No skill graded yet. One is graded when the agent keeps a define-skill, or with (witness:grade-named \"name\").")))

(defun show-text (name)
  "The evidence behind NAME's grade, every finding with its weight."
  (running-settings)
  (let ((record (record-for name)))
    (unless record (fail "no skill named ~a is graded; /witness report lists them" name))
    (format nil "~a: grade ~a (score ~a) - ~a - ~a uses~@[~%~{~a~^~%~}~]"
            name (getf record :grade) (getf record :score) (getf record :status)
            (getf record :uses)
            (loop for (rule detail weight) in (getf record :findings)
                  collect (format nil "  ! ~a (~a): ~a" rule weight detail)))))

(defun verify-text ()
  (running-settings)
  (multiple-value-bind (ok message) (ledger-verify)
    (format nil "~a ~a" (if ok "ledger intact:" "ledger TAMPERED:") message)))

;;; --- the operator's own verbs --------------------------------------------------------

(defun trust-named (name)
  "The operator vouches for NAME: the gate stops flagging it."
  (running-settings)
  (let ((record (record-for name)))
    (unless record (fail "no skill named ~a is graded" name))
    (setf (getf record :status) "trusted"
          (record-for name) record)
    (ledger-append (list :type "human" :ts (get-universal-time) :skill name :action "trusted"))
    (format nil "'~a' marked trusted; the gate stops flagging it." name)))

(defun release-named (name)
  "Restore a quarantined skill to where it was, and mark it trusted."
  (running-settings)
  (let ((record (record-for name)))
    (unless (and record (equal (getf record :status) "quarantined"))
      (fail "'~a' is not quarantined" name))
    (let ((from (probe-file (getf record :qpath)))
          (to (pathname (getf record :rel))))
      (unless from (fail "the quarantined file is gone: ~a" (getf record :qpath)))
      (when (probe-file to) (fail "~a already exists; resolve it by hand" (getf record :rel)))
      (ensure-directories-exist to)
      (rename-file from to)
      (setf (getf record :status) "trusted"
            (getf record :qpath) nil
            (record-for name) record)
      (ledger-append (list :type "human" :ts (get-universal-time) :skill name :action "released"))
      (format nil "Restored '~a' to ~a and marked trusted." name (getf record :rel)))))
