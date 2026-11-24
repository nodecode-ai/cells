;;;; cell.lisp --- a failure becomes a case, a case becomes a check that runs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The loop the plugin ran, kept in six verbs and one shelf of JSON files:
;;;; record a failure or a correction as a candidate, review it, draft the
;;;; command that would have caught it, approve that command by the digest it
;;;; has, run it and keep what it printed as evidence. Nothing leaves this
;;;; machine; the shelf is a folder of one JSON file per case.
;;;;
;;;; What the plugin got from Hermes and Nodecode has no seam for -- scanning
;;;; its state.db for corrections, the post_tool_call and on_session_end
;;;; capture hooks, the local web page, the uv-managed runtime and pytest
;;;; recipes -- is left out; see NOTICE.

(in-package #:nodecode-afterforge)

;;; --- the words ------------------------------------------------------------------

(defparameter *primer*
  "afterforge is this machine's regression shelf: a failure or a correction a
person cared about becomes a case, and a case becomes a command that proves it
stays fixed. The loop, in order:
  afterforge_case    record one failure or correction (title, kind, what happened, what should happen)
  afterforge_cases   list what is on the shelf, by status
  afterforge_review  approve or reject a candidate -- a person's decision, not the model's
  afterforge_recipe  for an approved case, draft the check: a shell command, the directory it runs in, the exit code it must give
  afterforge_verify  run that check with the recipe's digest; the run is kept as evidence
  afterforge_report  one case's evidence, or the whole shelf
A case is verified when its command last passed and regressed when it last
failed. Nothing is run until a check is drafted and its digest is passed back,
and every run happens in the project directory the `afterforge' section names.
(help :afterforge) is this text."
  "What (help :afterforge) answers while the cell runs.")

(defparameter *kinds* '("failure" "correction")
  "What a case is: something that went wrong, or a correction a person made.")

(defparameter *statuses* '("candidate" "reviewed" "recipe" "verified" "regressed" "rejected")
  "A case's place in the loop: recorded, approved, given a check, passing,
failing, or turned down.")

(defparameter *evidence-kept* 5
  "How many past runs of one check the shelf keeps: enough to see it flake,
few enough that a folder stays readable.")

(defparameter *unapproved*
  "not approved yet: call afterforge_review with decision \"approve\" first, then afterforge_recipe."
  "What drafting a check for an unreviewed case answers.")

;;; --- the shelf -------------------------------------------------------------------

(defun now-string ()
  "This moment as a sortable UTC-free stamp: the machine's own clock."
  (multiple-value-bind (second minute hour day month year) (get-decoded-time)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d"
            year month day hour minute second)))

(defun clip (text limit)
  "TEXT, at most LIMIT characters and ending in an ellipsis when it was cut."
  (if (and (integerp limit) (> (length text) (1+ limit)))
      (concatenate 'string (subseq text 0 (max 0 (- limit 1))) "…")
      text))

(defun last-n (n items)
  "The last N of ITEMS, or all of them."
  (let ((length (length items)))
    (if (> length n) (subseq items (- length n)) items)))

(defun store-dir ()
  "The folder the shelf lives in: this cell's `home' setting, else a folder
in the organism's own home."
  (uiop:ensure-directory-pathname
   (let ((home (nlk:trimmed (or (setting :home) ""))))
     (if (plusp (length home))
         (uiop:parse-native-namestring home)
         (nlk:home "afterforge/")))))

(defun case-id-p (text)
  "Whether TEXT can name a case: letters, digits and dashes, nothing that
could step out of the shelf folder."
  (and (stringp text) (plusp (length text)) (<= (length text) 64)
       (every (lambda (character)
                (or (alphanumericp character) (char= character #\-)))
              text)))

(defun case-file (id)
  "ID's file on the shelf."
  (merge-pathnames (format nil "~a.json" id) (store-dir)))

(defun case-ids ()
  "Every case on the shelf, oldest name first."
  (let ((dir (store-dir)))
    (if (uiop:directory-exists-p dir)
        (sort (loop for file in (uiop:directory-files dir "*.json")
                    for name = (pathname-name file)
                    when (case-id-p name) collect name)
              #'string<)
        '())))

(defun read-case (id)
  "The case ID names, or a refusal naming the shelf."
  (let ((id (nlk:trimmed (or id ""))))
    (unless (case-id-p id)
      (fail "~s is not a case id: they look like case-001; afterforge_cases lists them" id))
    (let ((file (case-file id)))
      (unless (probe-file file)
        (fail "no case ~a on the shelf (~a); afterforge_cases lists what is there" id (namestring (store-dir))))
      (let ((case (nlk:decode-json (uiop:read-file-string file))))
        (unless (hash-table-p case)
          (fail "~a is not a case file" (namestring file)))
        case))))

(defun write-case (case)
  "CASE written back to its own file, whole, and answered."
  (let ((file (case-file (or (nlk:json-value case :string "id") ""))))
    (ensure-directories-exist file)
    (with-open-file (out file :direction :output :if-exists :supersede
                               :if-does-not-exist :create
                               :external-format :utf-8)
      (write-string (nlk:encode-json-object case) out))
    case))

(defun next-id ()
  "The next free case name on the shelf."
  (let ((highest 0))
    (dolist (id (case-ids))
      (let ((number (ignore-errors (parse-integer id :start (min 5 (length id)) :junk-allowed t))))
        (when (and number (> number highest)) (setf highest number))))
    (format nil "case-~3,'0d" (1+ highest))))

(defun field (case key default)
  "CASE's KEY as a string, or DEFAULT when it is absent."
  (let ((value (ignore-errors (nlk:json-value case :string key))))
    (if (and (stringp value) (plusp (length value))) value default)))

(defun evidence (case)
  "CASE's past runs, oldest first."
  (let ((stored (gethash "evidence" case)))
    (cond ((vectorp stored) (coerce stored 'list))
          ((listp stored) stored)
          (t '()))))

;;; --- what a check is --------------------------------------------------------------

(defun project-dir ()
  "The folder the checks run in: the `project' setting."
  (uiop:ensure-directory-pathname (setting :project)))

(defun resolve-directory (text)
  "TEXT as a folder, read against the project when it is a relative one."
  (let* ((text (nlk:trimmed (or text "")))
         (path (uiop:parse-native-namestring (if (plusp (length text)) text "."))))
    (if (uiop:absolute-pathname-p path) path (merge-pathnames path (project-dir)))))

(defun digest-of (case-id command directory expect-exit)
  "What approving one check means: a short digest of the exact command, the
directory and the exit code it must give."
  (subseq (nlk:sha256-text (format nil "~a~%~a~%~a~%~a"
                                   case-id command directory expect-exit))
          7 23))

(defun run-shell (command directory timeout)
  "Run COMMAND through sh in DIRECTORY, at most TIMEOUT seconds. =>
(VALUES OUTPUT EXIT-CODE); a command that never finished is exit code -1."
  (let ((process nil))
    (unwind-protect
         (handler-case
             (sb-ext:with-timeout (max 1 timeout)
               (setf process (uiop:launch-program (list "sh" "-c" command)
                                                   :directory directory
                                                   :input nil
                                                   :output :stream
                                                   :error-output :output))
               (let ((output (with-output-to-string (out)
                               (loop for line = (read-line (uiop:process-info-output process)
                                                           nil nil)
                                     while line do (write-line line out))))
                     (code (uiop:wait-process process)))
                 (setf process nil)
                 (values output (or code -1))))
           (sb-ext:timeout ()
             (values (format nil "timed out after ~a seconds" timeout) -1))
           (error (condition)
             (values (format nil "could not run: ~a" condition) -1)))
      (when (and process (ignore-errors (uiop:process-alive-p process)))
        (ignore-errors (uiop:terminate-process process))))))

(defvar *run* #'run-shell
  "The one seam that runs a check: (COMMAND DIRECTORY TIMEOUT) => its output
and exit code. Tests replace it; nothing else does.")

;;; --- the verbs -------------------------------------------------------------------

(define-verb record (title &optional (kind "failure") (detail "") (expect ""))
  "Record one failure or correction as a candidate regression case. => the
case's id, and what to do next."
  (let* ((case (nlk:make-json-object
                 "id" (next-id)
                 "title" title
                 "kind" kind
                 "detail" detail
                 "expect" expect
                 "status" "candidate"
                 "created" (now-string)
                 "reviewed_at" ""
                 "reviewer_note" ""
                 "recipe" (nlk:make-json-object)
                 "evidence" (vector))))
    (write-case case)
    (format nil "~a recorded as a candidate: ~a~%next: afterforge_review with case_id \"~a\" and decision \"approve\" or \"reject\"."
            (field case "id" "?") title (field case "id" "?"))))

(define-verb cases (&optional status limit)
  "List the shelf: one line per case, its status, kind and title. => text."
  (let* ((wanted (nlk:trimmed (or status "")))
         (limit (or limit 20))
         (all (case-ids))
         (shown '()))
    (dolist (id all)
      (let ((case (ignore-errors (read-case id))))
        (when (and case
                   (or (zerop (length wanted))
                       (equal wanted (field case "status" "candidate"))))
          (push (format nil "~a  ~10a ~11a ~a" id (field case "status" "candidate")
                         (field case "kind" "failure") (field case "title" "?"))
                shown))))
    (setf shown (nreverse shown))
    (if (null shown)
        (if (zerop (length wanted))
            "afterforge: the shelf is empty. afterforge_case records the first failure or correction."
            (format nil "afterforge: no case is ~a. afterforge_cases lists them all." wanted))
        (format nil "~{~a~%~}~@[~d more; raise limit~]~%on the shelf: ~d case~:p in ~a"
                (last-n limit shown)
                (max 0 (- (length shown) limit))
                (length all)
                (namestring (store-dir))))))

(define-verb review (case-id decision &optional note)
  "Approve or reject a candidate case. A person's decision: only an approved
case gets a check. => the new status."
  (let* ((case (read-case case-id))
         (decision (nlk:trimmed (or decision "approve")))
         (note (nlk:trimmed (or note ""))))
    (unless (member decision '("approve" "reject") :test #'equal)
      (fail "decision is ~s: it is \"approve\" or \"reject\"" decision))
    (when (member (field case "status" "") '("verified" "regressed") :test #'equal)
      (fail "~a already has a check that ran (~a); afterforge_report ~a shows it"
            case-id (field case "status" "") case-id))
    (setf (gethash "status" case) (if (equal decision "approve") "reviewed" "rejected"))
    (setf (gethash "reviewed_at" case) (now-string))
    (setf (gethash "reviewer_note" case) note)
    (write-case case)
    (format nil "~a is ~a~@[ -- ~a~]. ~a" case-id (field case "status" "reviewed")
            (if (plusp (length note)) note nil)
            (if (equal decision "approve")
                (format nil "next: afterforge_recipe with case_id \"~a\" and the command that would have caught it." case-id)
                "the check is not drafted; a later approve reopens it."))))

(define-verb recipe (case-id command &optional directory expect-exit)
  "Draft the check for an approved case: a shell command, the folder it runs
in, the exit code it must give. => the digest to approve the run with."
  (let* ((case (read-case case-id))
         (status (field case "status" "candidate"))
         (command (nlk:trimmed (or command "")))
         (directory (nlk:trimmed (or directory "")))
         (directory (if (plusp (length directory)) directory "."))
         (expect-exit (or expect-exit 0)))
    (cond ((equal status "candidate") (fail "~a: ~a" case-id *unapproved*))
          ((equal status "rejected") (fail "~a was rejected; afterforge_review it with decision \"approve\" first" case-id))
          ((zerop (length command)) (fail "a check is a command: pass the shell command that must pass")))
    (let* ((digest (digest-of case-id command directory expect-exit))
           (drafted (nlk:make-json-object "command" command
                                           "directory" directory
                                           "expect_exit" expect-exit
                                           "digest" digest
                                           "at" (now-string))))
      (setf (gethash "recipe" case) drafted)
      (setf (gethash "status" case) "recipe")
      (write-case case)
      (format nil "~a has a check: ~a in ~a, exit ~a~%digest ~a~%afterforge_verify with case_id \"~a\" and approve_digest \"~a\" runs it."
              case-id command (namestring (resolve-directory directory)) expect-exit digest case-id digest))))

(define-verb verify (case-id approve-digest)
  "Run an approved case's check and keep what it printed as evidence. Needs
the digest afterforge_recipe named. => whether it passed, and the output."
  (let* ((case (read-case case-id))
         (drafted (gethash "recipe" case))
         (status (field case "status" "candidate")))
    (unless (and (hash-table-p drafted) (plusp (hash-table-count drafted)))
      (fail "~a has no check yet: afterforge_recipe drafts one" case-id))
    (when (equal status "rejected")
      (fail "~a was rejected; approve it before running its check" case-id))
    (let* ((digest (nlk:json-value drafted :string "digest"))
           (approved (nlk:trimmed (or approve-digest ""))))
      (unless (and digest (equal digest approved))
        (fail "~a's check was approved as ~s, not ~s: pass the digest afterforge_recipe named, or draft the check again"
              case-id digest approved))
      (let* ((command (nlk:json-value drafted :string "command"))
             (directory (resolve-directory (nlk:json-value drafted :string "directory")))
             (expected (or (ignore-errors (nlk:json-value drafted :integer "expect_exit")) 0)))
        (unless (uiop:directory-exists-p directory)
          (fail "the check runs in ~a, which is not a folder" (namestring directory)))
        (multiple-value-bind (output code)
            (funcall *run* command directory (setting :timeout-seconds))
          (let* ((passed (eql code expected))
                 (run (nlk:make-json-object "at" (now-string)
                                            "digest" digest
                                            "command" command
                                            "directory" (namestring directory)
                                            "exit" code
                                            "expected" expected
                                            "passed" (if passed t nil)
                                            "output" (clip output (setting :output-chars)))))
            (setf (gethash "evidence" case) (coerce (last-n *evidence-kept* (append (evidence case) (list run)))
                                                     'vector))
            (setf (gethash "status" case) (if passed "verified" "regressed"))
            (write-case case)
            (format nil "~a ~a: exit ~a, expected ~a~%~a"
                    (if passed "verified" "regressed") case-id code expected
                    (if (plusp (length output)) (clip output (setting :output-chars))
                        "(the check printed nothing)"))))))))

(define-verb report (&optional case-id)
  "The evidence for one case -- its recipe and the runs it had -- or the whole
shelf in a line each."
  (let ((id (nlk:trimmed (or case-id ""))))
    (if (plusp (length id))
        (let* ((case (read-case id))
               (drafted (gethash "recipe" case))
               (runs (reverse (evidence case))))
          (with-output-to-string (out)
            (format out "~a  ~a  ~a~%  ~a~@[~%  what happened: ~a~]~@[~%  should have happened: ~a~]~@[~%  note: ~a~]~%"
                    id (field case "status" "candidate") (field case "kind" "failure")
                    (field case "title" "?")
                    (let ((detail (field case "detail" ""))) (and (plusp (length detail)) detail))
                    (let ((expect (field case "expect" ""))) (and (plusp (length expect)) expect))
                    (let ((note (field case "reviewer_note" ""))) (and (plusp (length note)) note)))
            (when (and (hash-table-p drafted) (plusp (hash-table-count drafted)))
              (format out "  check: ~a in ~a (exit ~a), digest ~a~%"
                      (nlk:json-value drafted :string "command")
                      (nlk:json-value drafted :string "directory")
                      (nlk:json-value drafted :integer "expect_exit")
                      (nlk:json-value drafted :string "digest")))
            (if (null runs)
                (format out "  no run yet~%")
                (dolist (run runs)
                  (format out "  ~a ~a exit ~a~@[~%    ~a~]~%"
                          (field run "at" "?")
                          (if (eq t (gethash "passed" run)) "pass" "fail")
                          (gethash "exit" run)
                          (let ((output (field run "output" "")))
                            (and (plusp (length output)) output)))))))
        (let ((all (case-ids)))
          (with-output-to-string (out)
            (format out "afterforge: ~d case~:p on the shelf in ~a~%" (length all) (namestring (store-dir)))
            (dolist (id all)
              (let ((case (ignore-errors (read-case id))))
                (when case
                  (format out "  ~a  ~10a ~11a ~a~%" id (field case "status" "candidate")
                          (field case "kind" "failure") (field case "title" "?")))))
            (format out "~%afterforge_case records a failure or correction; afterforge_cases filters by status."))))))

;;; --- the boundaries ----------------------------------------------------------------

(nlk:define-contract afterforge-case
  (:doc "Record one failure or correction as a candidate regression case: a tool call that misbehaved, a test that broke, a check that lied, or a correction a person made to your work. Record it while it is fresh, with what happened and what should have happened instead. A candidate is not a check yet: a person approves it with afterforge_review.")
  ("title" :text :required t
           :doc "one line naming the failure, at most 120 characters")
  ("kind" :string :options '("failure" "correction") :default "failure"
         :doc "a failure you hit, or a correction a person made")
  ("detail" :string :default ""
            :doc "what happened, as concretely as you saw it: the command, the error, the wrong answer")
  ("expect" :string :default ""
            :doc "what should have happened, in the words of something that can be checked"))

(nlk:define-contract afterforge-cases
  (:doc "List this machine's regression shelf: every case with its status, kind and title. Filter by status to see what waits for review, which checks pass, and what regressed.")
  ("status" :string :default ""
            :doc "candidate, reviewed, recipe, verified, regressed or rejected; empty for all")
  ("limit" :integer :default 20
           :doc "how many cases to list, newest names last"))

(nlk:define-contract afterforge-review
  (:doc "Approve or reject a candidate case. Approval is a person's decision: ask before approving work you did yourself, and put their words in the note.")
  ("case_id" :text :required t :doc "the case to decide on, as afterforge_cases names it")
  ("decision" :string :options '("approve" "reject") :required t
              :doc "approve to let a check be drafted, reject to retire it")
  ("note" :string :default "" :doc "one sentence from the person deciding"))

(nlk:define-contract afterforge-recipe
  (:doc "Draft the check for an approved case: the shell command that would have caught the failure, the folder it runs in, and the exit code that means fixed. Running it is a separate, approved step.")
  ("case_id" :text :required t :doc "the approved case")
  ("command" :text :required t
           :doc "the command, run through sh: a test you can name, a grep of the source, a script the project already has")
  ("directory" :string :default "." :slot check-folder
               :doc "where it runs: a folder relative to the project, or an absolute one")
  ("expect_exit" :integer :default 0
                 :doc "the exit code that means the check passed"))

(nlk:define-contract afterforge-verify
  (:doc "Run an approved case's check now and keep what it printed as evidence. Refused until the digest afterforge_recipe named is passed back, so the command that runs is the command that was reviewed.")
  ("case_id" :text :required t :doc "the case whose check runs")
  ("approve_digest" :text :required t
                    :doc "the digest afterforge_recipe answered for exactly this command"))

(nlk:define-contract afterforge-report
  (:doc "One case's evidence -- its check and the runs it had, oldest first -- or the whole shelf in a line each when no case is named.")
  ("case_id" :string :default "" :doc "the case to report on; empty for the whole shelf"))

(defun record-tool (call)
  "The afterforge_case tool: CALL recorded as a candidate."
  (record (afterforge-case-title call) (afterforge-case-kind call)
          (afterforge-case-detail call) (afterforge-case-expect call)))

(defun cases-tool (call)
  "The afterforge_cases tool: the shelf, filtered."
  (cases (afterforge-cases-status call) (afterforge-cases-limit call)))

(defun review-tool (call)
  "The afterforge_review tool: CALL's case decided."
  (review (afterforge-review-case-id call) (afterforge-review-decision call)
          (afterforge-review-note call)))

(defun recipe-tool (call)
  "The afterforge_recipe tool: the check CALL drafts."
  (recipe (afterforge-recipe-case-id call) (afterforge-recipe-command call)
          (afterforge-recipe-check-folder call) (afterforge-recipe-expect-exit call)))

(defun verify-tool (call)
  "The afterforge_verify tool: CALL's check run, and its evidence kept."
  (verify (afterforge-verify-case-id call) (afterforge-verify-approve-digest call)))

(defun report-tool (call)
  "The afterforge_report tool: one case, or the shelf."
  (report (afterforge-report-case-id call)))

;;; --- /afterforge ------------------------------------------------------------------

(defun run-slash (args session-id)
  "/afterforge status | cases [STATUS] | report CASE-ID: the operator's own
view of the shelf."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (words (if (plusp (length text)) (uiop:split-string text :separator '(#\Space)) '()))
         (verb (string-downcase (or (first words) "status"))))
    (cond ((member verb '("status" "help") :test #'equal) (report))
          ((equal verb "cases") (cases (second words) nil))
          ((equal verb "report") (report (or (second words) "")))
          (t (format nil "afterforge: ~a is not a subcommand. Try /afterforge status, cases [STATUS] or report CASE-ID." verb)))))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell afterforge
  (:section ("afterforge")
    (:guide "project is the folder the checks run in; nothing else is required -- cases live in this home's afterforge folder unless home names another")
    ("project" :path :required t
               :doc "the folder the checks run in, usually the repository they cover")
    ("home" :path
            :doc "where cases and their evidence are kept; this home's afterforge folder when empty")
    ("timeout_seconds" :integer :default 120 :min 5
                       :doc "one check's ceiling, in seconds")
    ("output_chars" :integer :default 1200 :min 100
                    :doc "how much of a check's output the model is handed"))
  (:settings (lambda (values table)
               (declare (ignore table))
               (let ((project (getf values :project)))
                 (unless (and (stringp project) (plusp (length (nlk:trimmed project))))
                   (nlk:config-error "afterforge.project names the folder the checks run in; set it to the repository they cover"))
                 (unless (uiop:directory-exists-p (uiop:ensure-directory-pathname project))
                   (nlk:config-error "afterforge.project is ~s: no such folder" project))
                 values)))
  (:tool "afterforge_case" (nlk:contract 'afterforge-case) 'record-tool)
  (:tool "afterforge_cases" (nlk:contract 'afterforge-cases) 'cases-tool)
  (:tool "afterforge_review" (nlk:contract 'afterforge-review) 'review-tool)
  (:tool "afterforge_recipe" (nlk:contract 'afterforge-recipe) 'recipe-tool)
  (:tool "afterforge_verify" (nlk:contract 'afterforge-verify) 'verify-tool)
  (:tool "afterforge_report" (nlk:contract 'afterforge-report) 'report-tool)
  (:command "afterforge" 'run-slash
            :catches 'afterforge-error
            :description "Regression cases and the checks that prove them"
            :argument-hint "status | cases [STATUS] | report CASE-ID")
  (:help :afterforge "afterforge: reviewed regression checks - (afterforge:report \"case-001\")" *primer*))
