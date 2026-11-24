;;;; cell.lisp --- the stage gates: a structural check the model must call, and its log.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of SeaOf0/dsh-redteam-model's dsh-stage-gate plugin. A
;;;; red-team playbook closes each stage on a gate, and the gate is a text
;;;; contract the model could quietly grade itself on. Here it is a tool call
;;;; instead: stage_gate reads the stage's artifacts in a workspace and answers
;;;; what is structurally there and what is not, so a stage cannot be declared
;;;; done on a file that was never written. What the files mean - whether a
;;;; comparison really flips, whether two chains agree - stays with a reviewer,
;;;; and every gate carries the reminders for exactly that.
;;;;
;;;; A verdict appends one row to <workspace>/gate-log.md, the trail a report
;;;; chapter reads. Nothing here dials, spawns or reads a credential: the
;;;; artifacts and the log are the only things it touches, both under the
;;;; workspace the section names.

(in-package #:nodecode-redteam-gates)

(nlk:define-peripheral redteam-gates :not-running t)

;;; --- the modes and their gates ----------------------------------------------------

(defun find-mode (name)
  "The mode NAME names, whatever its case, or NIL."
  (and (stringp name)
       (find (string-downcase (nlk:trimmed name)) +gate-modes+
             :key (lambda (mode) (string-downcase (getf mode :id))) :test #'string=)))

(defun find-gate (mode stage)
  "The gate STAGE of MODE, whatever its case, or NIL."
  (and (stringp stage)
       (find (string-downcase (nlk:trimmed stage)) (getf mode :gates)
             :key (lambda (gate) (string-downcase (getf gate :stage))) :test #'string=)))

(defun mode-ids ()
  "Every mode id, in order."
  (mapcar (lambda (mode) (getf mode :id)) +gate-modes+))

(defun gate-ids (mode)
  "Every gate id of MODE, in order."
  (mapcar (lambda (gate) (getf gate :stage)) (getf mode :gates)))

;;; --- reading the artifacts --------------------------------------------------------

(defun artifact-path (workspace name)
  "The path NAME names, resolved under WORKSPACE when it is relative."
  (if (and (plusp (length name)) (char= #\/ (char name 0)))
      name
      (namestring (merge-pathnames name (uiop:ensure-directory-pathname workspace)))))

(defun artifact-text (path)
  "The text at PATH, or NIL when it is absent or unreadable."
  (and path (nlk:read-text path)))

(defun split-bars (line)
  "LINE's table cells, between its bars."
  (let ((body (if (and (plusp (length line)) (char= #\| (char line 0))) (subseq line 1) line))
        (cells '()) (start 0))
    (loop for index from 0 below (length body)
          when (char= #\| (char body index))
            do (push (subseq body start index) cells)
               (setf start (1+ index)))
    (push (subseq body start) cells)
    (let ((cells (nreverse cells)))
      (if (and cells (string= "" (first (last cells))) (plusp (length line))
               (char= #\| (char line (1- (length line)))))
          (butlast cells)
          cells))))

(defun dashes-only-p (cell)
  "Whether CELL is a table's separator cell, like --- or :--:."
  (let* ((text (if (and (plusp (length cell)) (char= #\: (char cell 0))) (subseq cell 1) cell))
         (text (if (and (plusp (length text)) (char= #\: (char text (1- (length text)))))
                   (subseq text 0 (1- (length text)))
                   text)))
    (and (>= (length text) 2) (every (lambda (character) (char= character #\-)) text))))

(defun markdown-rows (text)
  "One (LINE . NON-EMPTY-CELLS) per markdown table row of TEXT; separator
rows are not rows."
  (let ((rows '()) (number 0))
    (dolist (line (nlk:lines text))
      (incf number)
      (let ((row (nlk:trimmed line)))
        (when (and (plusp (length row)) (char= #\| (char row 0)))
          (let* ((cells (mapcar #'nlk:trimmed (split-bars row)))
                 (filled (remove "" cells :test #'string=)))
            (unless (and filled (every #'dashes-only-p filled))
              (push (cons number (length filled)) rows))))))
    (nreverse rows)))

(defparameter +hex-digits+ "0123456789abcdefABCDEF"
  "The digits a hash is written with; the original took either case too.")

(defun hex-present-p (text)
  "Whether TEXT holds a run of 64 hexadecimal digits, a sha256 written out."
  (let ((longest 0) (run 0))
    (loop for character across (or text "")
          do (setf run (if (find character +hex-digits+) (1+ run) 0))
             (setf longest (max longest run)))
    (>= longest 64)))

;;; --- one check, one gate ----------------------------------------------------------

(defun check-about (check)
  "What CHECK is about, as the answer names it: a file, or $file's stand-in."
  (let ((name (or (getf check :file) (getf check :dir) "")))
    (if (string= name "$file") "the file given" name)))

(defun run-check (check workspace file)
  "CHECK's verdict over WORKSPACE's artifacts: a plist (:ID :OK :DETAIL)."
  (let* ((kind (getf check :kind))
         (name (or (getf check :file) (getf check :dir) ""))
         (about (check-about check))
         (path (cond ((string= name "$file") (and file (artifact-path workspace file)))
                     ((string= "" name) nil)
                     (t (artifact-path workspace name))))
         (text (artifact-text path))
         (id (format nil "~(~a~) ~a" kind about)))
    (flet ((verdict (ok detail) (list :id id :ok (and ok t) :detail detail))
           (rows-of () (markdown-rows (or text ""))))
      (ecase kind
        (:file
         (verdict (and text (plusp (length (nlk:trimmed text))))
                  (if (and text (plusp (length (nlk:trimmed text))))
                      (format nil "~a is there" about)
                      (format nil "~a is missing or empty" about))))
        (:markers
         (if (null text)
             (verdict nil (format nil "~a is missing" about))
             (let ((absent (remove-if (lambda (marker) (search marker text)) (getf check :markers))))
               (verdict (null absent)
                        (if absent
                            (format nil "~a lacks ~{~a~^, ~}" about absent)
                            (format nil "~a carries every marker" about))))))
        (:hex-hash
         (verdict (and text (hex-present-p text))
                  (if (and text (hex-present-p text))
                      (format nil "~a carries a 64-digit hash" about)
                      (format nil "~a carries no 64-digit hash" about))))
        (:table
         (if (null text)
             (verdict nil (format nil "~a is missing" about))
             (let* ((rows (rows-of))
                    (minimum (getf check :min-cells 1))
                    (wanted (getf check :min-rows 1))
                    (thin (remove-if (lambda (row) (>= (cdr row) minimum)) rows)))
               (verdict (and (>= (length rows) wanted) (null thin))
                        (format nil "~a: ~d table row~:p, at least ~d needed~@[, rows short of ~d cells on line ~{~d~^, ~}~]"
                                about (length rows) wanted (and thin minimum) (mapcar #'car thin))))))
        (:provenance
         (let* ((directory (artifact-path workspace (getf check :dir "artifacts")))
                (found (loop for sub in (ignore-errors (uiop:subdirectories directory))
                             when (hex-present-p (artifact-text (merge-pathnames "provenance.md" sub)))
                               return sub)))
           (verdict found
                    (if found
                        (format nil "~a holds a hashed registration" (namestring found))
                        (format nil "no ~a/*/provenance.md carries a 64-digit hash" about)))))))))

(defun run-gate (mode stage &key file workspace)
  "MODE's gate STAGE checked over WORKSPACE's artifacts: a verdict plist
(:MODE :STAGE :TITLE :PASS :CHECKS :MANUAL :MISSING)."
  (let* ((gate (find-gate mode stage))
         (workspace (or workspace (setting :workspace)))
         (checks (mapcar (lambda (check) (run-check check workspace file)) (getf gate :checks))))
    (list :mode (getf mode :id)
          :stage (getf gate :stage)
          :title (getf gate :title)
          :pass (every (lambda (result) (getf result :ok)) checks)
          :checks checks
          :manual (getf gate :manual)
          :missing (remove-if (lambda (result) (getf result :ok)) checks))))

;;; --- the log ------------------------------------------------------------------------

(defparameter +gate-log-head+
  (format nil "# gate-log - one row a verdict, the trail a report chapter reads.~%~%| time | gate | result | missing |~%|---|---|---|---|~%")
  "What gate-log.md starts as under a workspace that has none.")

(defun gate-log-path (workspace)
  "The log's path, under WORKSPACE."
  (artifact-path workspace "gate-log.md"))

(defun append-gate-log (workspace verdict)
  "VERDICT's one row appended to WORKSPACE's gate-log.md; answers the path,
or NIL when the write failed - a failed trail never flips a verdict."
  (let* ((path (gate-log-path workspace))
         (head (or (artifact-text path) +gate-log-head+))
         (open (mapcar (lambda (result) (format nil "~a (~a)" (getf result :id) (getf result :detail)))
                       (getf verdict :missing)))
         (row (format nil "| ~a | ~a/~a | ~:[fail~;pass~] | ~a |~%"
                      (nlk:iso-time (nlk:unix-now) :millis nil)
                      (getf verdict :mode) (getf verdict :stage) (getf verdict :pass)
                      (if open (substitute #\/ #\| (format nil "~{~a~^; ~}" open)) "-"))))
    (handler-case (progn (nlk:write-file-atomically path (concatenate 'string head row)) path)
      (error () nil))))

(defun log-rows (path limit)
  "The verdict rows of the log at PATH, newest LIMIT of them, oldest first."
  (let ((rows (remove-if-not (lambda (line)
                                (let ((row (nlk:trimmed line)))
                                  (and (plusp (length row)) (char= #\| (char row 0))
                                       (not (search "| time |" row))
                                       (not (every (lambda (character)
                                                     (find character "-| "))
                                                   row)))))
                              (nlk:lines (or (artifact-text path) "")))))
    (last rows (min (max 1 limit) (length rows)))))

;;; --- the words ----------------------------------------------------------------------

(defun modes-text ()
  "Every mode with its focus and its gate ids: what the model reads first."
  (with-output-to-string (out)
    (format out "redteam-gates: ~d work modes, each closing its stages on gates a tool checks.~%"
            (length +gate-modes+))
    (dolist (mode +gate-modes+)
      (format out "~%  ~a - ~a~%    gates: ~{~a~^, ~}"
              (getf mode :id) (getf mode :focus) (gate-ids mode)))
    (format out "~%Call gates_list, with a mode, for what each gate wants in its files.~%")))

(defun gate-line (gate &key detail)
  "GATE as one line of gates_list: its id, its title, the files it reads."
  (let* ((named (remove-if (lambda (name) (string= name "$file"))
                            (remove nil (mapcar (lambda (check) (getf check :file)) (getf gate :checks)))))
         (directory (getf (find-if (lambda (check) (getf check :dir)) (getf gate :checks)) :dir)))
    (format nil "    ~a ~a~@[ (files: ~{~a~^, ~})~]~@[ (directory: ~a)~]~@[ (file argument: ~a)~]~@[ - manual: ~{~a~^; ~}~]"
            (getf gate :stage) (getf gate :title)
            (and named (remove-duplicates named :test #'string= :from-end t))
            directory (and (getf gate :requires-file) (getf gate :file-hint))
            (and detail (getf gate :manual)))))

(defun gates-list-text (&optional mode)
  "The schema: every mode and gate, or MODE's gates in full."
  (if (null mode)
      (with-output-to-string (out)
        (format out "redteam-gates: the gates, mode by mode.~%")
        (dolist (each +gate-modes+)
          (format out "~%  ~a - ~a~%" (getf each :id) (getf each :focus))
          (dolist (gate (getf each :gates))
            (format out "~a~%" (gate-line gate)))))
      (with-output-to-string (out)
        (format out "redteam-gates: ~a - ~a~%" (getf mode :id) (getf mode :focus))
        (dolist (gate (getf mode :gates))
          (format out "~a~%" (gate-line gate :detail t)))
        (format out "~%A gate passes on structure alone; every line marked manual is a reviewer's call.~%"))))

(defun unknown-mode-text (typed)
  "The line an unknown mode answers, naming the ones there are."
  (format nil "redteam-gates: no mode called ~a; the modes are ~{~a~^, ~}."
          typed (mode-ids)))

(defun unknown-gate-text (mode typed)
  "The line an unknown stage answers, naming MODE's gates."
  (format nil "redteam-gates: ~a has no stage called ~a; its gates are ~{~a~^, ~}."
          (getf mode :id) typed (gate-ids mode)))

(defun verdict-text (verdict &key log rows)
  "VERDICT as the model reads it: what passed, what is missing, whose call the
rest is, and where its row went."
  (with-output-to-string (out)
    (format out "stage gate ~a/~a: ~a~%" (getf verdict :mode) (getf verdict :stage) (getf verdict :title))
    (format out "result: ~:[FAIL~;pass~] of ~d structural check~:p~%"
            (getf verdict :pass) (length (getf verdict :checks)))
    (loop for result in (getf verdict :checks)
          for number from 1
          do (format out "  ~d. ~a - ~:[FAIL~;ok~]: ~a~%"
                     number (getf result :id) (getf result :ok) (getf result :detail)))
    (if (getf verdict :missing)
        (format out "a structural pass is not a full pass: ~d check~:p open, and the semantic ones below were never structural.~%"
                (length (getf verdict :missing)))
        (format out "a structural pass is not a full pass.~%"))
    (when (getf verdict :manual)
      (format out "the reviewer's call:~%")
      (dolist (line (getf verdict :manual))
        (format out "  - ~a~%" line)))
    (when log
      (format out "log: ~a~@[ (newest ~d row~:p)~]~%" log (and rows (length rows)))
      (dolist (row rows) (format out "  ~a~%" row)))))

(defparameter +manual+
  "redteam-gates: the ten red-team work modes' stage gates, as a check the model must call
rather than grade itself. A gate names the artifacts a stage ends with - a file that
must exist and not be empty, the markers its playbook requires, the table rows it must
fill, a 64-digit hash on a sample's registration - and the answer says which of them
are there. What the files mean stays with a reviewer, and every gate lists those
judgements as manual reminders, so a structural pass is never mistaken for a full one.

Modes (each a playbook with a focus of its own):
  pentest, code-audit, binary-analysis, attack-defense, av-evasion,
  incident-response, cloud-security, ctf-solver.

The tools:
  stage_gate - (mode, stage, file?, workspace?) checks one gate over the workspace's
      artifacts and appends its row to <workspace>/gate-log.md. A gate that needs a
      file argument names what it wants; without it the gate answers the hint instead
      of a verdict.
  gates_list - (mode?) the schema: every mode's gates, the files each reads, whether
      it takes a file argument, and the manual lines it carries.

The command /redteam-gates does the same for the operator: bare, it lists the modes;
with a mode, that mode's gates; with a mode and a stage, it runs the gate and shows
the verdict.

The section redteam-gates names the workspace (the directory the artifacts and
gate-log.md live in, `.' when unset) and log_rows (how many of the log's newest rows
a verdict shows back). Nothing here dials, spawns, installs or reads a credential: the
artifacts and the log are the whole of what it touches."
  "What (help :redteam-gates) answers while the cell runs.")

;;; --- the tools ------------------------------------------------------------------------

(nlk:define-contract stage-gate-arguments
  (:doc "Check one red-team stage gate against the artifacts in a workspace: which
files exist and are not empty, which required markers, table rows and hashes they
carry, and which judgements stay with a reviewer. Call it before you say a stage is
done, and never in place of writing the artifacts it names. The verdict is appended
to <workspace>/gate-log.md. Call gates_list first when you do not know a gate's id.")
  ("mode" :text :required t
          :doc "the work mode: pentest, code-audit, binary-analysis, attack-defense, av-evasion, incident-response, cloud-security or ctf-solver")
  ("stage" :text :required t
          :doc "the gate within that mode, as gates_list names it (P1, A2, B0, recon, V3, I5, C7, flag)")
  ("file" :string :default ""
          :doc "for a gate that reads one artifact of its own: its path, relative to the workspace unless absolute")
  ("workspace" :string :default ""
              :doc "the directory the artifacts and gate-log.md live in; the section's workspace when empty"))

(nlk:define-contract gates-list-arguments
  (:doc "The stage gate schema of the red-team work modes: every mode's gates, the
artifact files each one reads, whether it takes a file argument, and the manual
judgements a reviewer keeps. Call it before stage_gate when you do not know a gate id.")
  ("mode" :string :default ""
          :doc "one mode to lay out in full; every mode, briefly, when empty"))

(defun typed-name (text)
  "TEXT trimmed, or NIL when it is blank."
  (let ((trimmed (nlk:trimmed (or text ""))))
    (and (plusp (length trimmed)) trimmed)))

(defun run-stage-gate (arguments)
  "The stage_gate tool: one gate's structural verdict, and its row in the log."
  (let* ((typed-mode (stage-gate-arguments-mode arguments))
         (mode (find-mode typed-mode)))
    (if (null mode)
        (unknown-mode-text typed-mode)
        (let* ((typed-stage (stage-gate-arguments-stage arguments))
               (gate (find-gate mode typed-stage)))
          (if (null gate)
              (unknown-gate-text mode typed-stage)
              (let ((file (typed-name (stage-gate-arguments-file arguments)))
                    (workspace (or (typed-name (stage-gate-arguments-workspace arguments))
                                   (setting :workspace))))
                (cond
                  ((and (getf gate :requires-file) (null file))
                   (format nil "stage gate ~a/~a needs the file argument: ~a."
                           (getf mode :id) (getf gate :stage) (getf gate :file-hint)))
                  (t
                   (let* ((verdict (run-gate mode (getf gate :stage) :file file :workspace workspace))
                          (log (append-gate-log workspace verdict)))
                     (verdict-text verdict
                                   :log (or log (format nil "~a (not written)" (gate-log-path workspace)))
                                   :rows (and log (log-rows log (setting :log-rows)))))))))))))

(defun run-gates-list (arguments)
  "The gates_list tool: the schema, all of it or one mode's."
  (let ((typed (typed-name (gates-list-arguments-mode arguments))))
    (if (null typed)
        (gates-list-text)
        (let ((mode (find-mode typed)))
          (if mode (gates-list-text mode) (unknown-mode-text typed))))))

;;; --- /redteam-gates ---------------------------------------------------------------------

(defun run-slash (args session-id)
  "The /redteam-gates command: the modes, one mode's gates, or a verdict."
  (declare (ignore session-id))
  (running-settings)
  (let* ((words (nlk:split-words (or args "")))
         (typed-mode (first words))
         (mode (and typed-mode (find-mode typed-mode)))
         (typed-stage (second words))
         (file (third words)))
    (cond
      ((null words) (modes-text))
      ((null mode) (unknown-mode-text typed-mode))
      ((null typed-stage) (gates-list-text mode))
      ((null (find-gate mode typed-stage)) (unknown-gate-text mode typed-stage))
      (t (let* ((workspace (setting :workspace))
                (verdict (run-gate mode typed-stage :file file :workspace workspace))
                (log (append-gate-log workspace verdict)))
           (verdict-text verdict :log (or log (gate-log-path workspace))))))))

;;; --- the cell --------------------------------------------------------------------------

(nle:define-cell redteam-gates
  (:section ("redteam-gates")
    (:guide "nothing here is required: a gate reads the artifacts in the workspace and appends its verdict to gate-log.md there; set workspace to point it at the engagement's own folder")
    ("workspace" :path :default "."
                 :doc "the directory the artifacts and gate-log.md live in")
    ("log_rows" :integer :default 20 :min 1
                :doc "how many of the log's newest rows a verdict shows back"))
  (:settings (lambda (values table)
               (declare (ignore table))
               values))
  (:tool "stage_gate" (nlk:contract 'stage-gate-arguments) 'run-stage-gate)
  (:tool "gates_list" (nlk:contract 'gates-list-arguments) 'run-gates-list)
  (:command "redteam-gates" 'run-slash
            :description "Check a red-team stage gate, or list the gates"
            :argument-hint "MODE [STAGE [FILE]]")
  (:help :redteam-gates "redteam-gates: the ten red-team modes' stage gates, checked by a tool - /redteam-gates"
         +manual+))
