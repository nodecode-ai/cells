;;;; cell.lisp --- a card a contract opens, a proof that decides it, a verdict kept.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A card is a piece of work with a contract: its goal, the artifact it
;;;; lands in, what holds when it is done, and the one shell command that
;;;; decides it -- the proof, done when that exits 0. Nothing a card says about
;;;; itself is a verdict: only a proof run is, and every run is a line in the
;;;; card's verdict log. A card whose contract is not complete is a draft and
;;;; cannot be proved; the intake completes it by asking only for the fields
;;;; that are missing.
;;;;
;;;; The board is one readable file under the home (crew/<board>.lisp): a
;;;; list of cards, each a plist, which the operator can open in any editor.
;;;; The proof runs through NLK:RUN-BOUNDED -- a real shell, bounded in the
;;;; image, in the session's folder -- and its output is kept clipped in the
;;;; log. A proof that names a hardline command (a root wipe, a mkfs, a fork
;;;; bomb) is refused when the card is opened, so the unattended run is never
;;;; the first place it is seen. This is a speed bump and a record, not a
;;;; sandbox: a proof is shell the operator's card asked for.

(in-package #:nodecode-crew)

;;; --- the manual ---------------------------------------------------------------
;;; (help :crew) answers it while the cell runs; a request carries the one line
;;; the :HELP clause below gives, never this text.

(defparameter +primer+
  "A card is a piece of work with a contract -- its goal, where the result
lands, what holds when it is done, and the shell command that decides it --
and it is done only when that proof exits 0. Through the nodecode-crew cell:
Lisp functions in the crew: package, called through eval. Each answers a
string; a refusal is ERROR: CREW-ERROR with the reason.
  (crew:plan :title \"...\" :goal \"...\" :artifact \"...\" :done-when \"...\"
             :proof \"make test\" &key budget)   open a card; every field is
      required and a missing one is refused, naming it.
  (crew:contract \"CARD\" &key title goal artifact done-when proof budget)
      fill the fields a card is missing, and answer the ones still missing.
  (crew:verdict \"CARD\")   run the card's proof command in this session's
      folder and record PASS or FAIL, with its exit status and output, in the
      card's log. A draft (a contract field still missing) is refused.
  (crew:progress \"CARD\" &key unit result evidence)   list the card's units
      of work, or mark one pass or fail; a mark carries evidence.
  (crew:diagnose &optional \"CARD\")   the board, or one card, with why each
      stands where it does.
  (crew:proof-check \"COMMAND\")   whether a command is hardline, without
      running it.
A card's proof runs with no person watching: it inherits this session's
folder and the cell's proof_seconds, and its output is kept clipped on the
card. The operator reads, proves and drops cards from /crew."
  "What (help :crew) answers while the cell runs.")

;;; --- what no unattended proof runs -------------------------------------------------
;;; Crew's floors are Hermes's own; here the list is short, textual and said to
;;; be a speed bump: each entry is a substring of the lowercased command and
;;; the words it is refused in.

(defparameter +hardline+
  '(("rm -rf /" . "wipes the root")
    ("rm -fr /" . "wipes the root")
    ("rm -rf /*" . "wipes the root")
    ("mkfs" . "formats a filesystem")
    ("dd of=/dev/" . "writes straight to a device")
    (":(){" . "a fork bomb")
    ("shutdown " . "powers the machine off")
    ("reboot" . "restarts the machine")
    ("chmod -r 000 /" . "makes the root unreadable"))
  "Commands no card's proof may be, with the words the refusal gives.")

(defun hardline (command)
  "(values REASON NIL) when COMMAND names a hardline command, the words it is
refused in; NIL when it does not."
  (let ((text (string-downcase (or command ""))))
    (loop for (needle . reason) in +hardline+
          when (search needle text) return (values reason needle))))

;;; --- the board, one file under the home ------------------------------------------

(defparameter *version* 1
  "The board file's shape; a card is a plist and this is what it is written as.")

(defun board-name ()
  "The board's name, trimmed and lowercased, or a refusal: the file it is
written to is named by it."
  (let ((name (string-downcase (nlk:trimmed (princ-to-string (setting :board))))))
    (unless (and (<= 1 (length name) 40)
                 (every (lambda (c)
                          (or (and (< (char-code c) 128) (alphanumericp c)) (find c "._-")))
                        name)
                 (alphanumericp (char name 0)))
      (fail "~s is no board name: lowercase letters, digits, . _ and -, at most 40, a letter or digit first"
            name))
    name))

(defun card-file ()
  "The one file this board is kept in, under the home."
  (nlk:home (format nil "crew/~a.lisp" (board-name))))

(defun empty-board ()
  "A board with no card on it."
  (list :version *version* :next 1 :cards '()))

(defun load-board ()
  "The board as it is on disk; an absent, empty or unreadable file is an empty
board -- a card is worth more than the file it was in."
  (let ((path (card-file)))
    (if (probe-file path)
        (handler-case
            (with-open-file (in path :direction :input)
              (let ((*read-eval* nil) (*package* (find-package '#:nodecode-crew)))
                (or (read in nil nil) (empty-board))))
          (error () (empty-board)))
        (empty-board))))

(defun save-board (board)
  "Write BOARD to its file in one go: the cards are always one whole file."
  (let* ((path (card-file))
         (temp (merge-pathnames "board.writing" path)))
    (ensure-directories-exist path)
    (with-open-file (out temp :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (let ((*print-readably* t) (*print-pretty* t) (*print-circle* nil)
            (*package* (find-package '#:nodecode-crew)))
        (prin1 board out)
        (terpri out)))
    (uiop:rename-file-overwriting-target temp path)
    board))

(defun next-id (board)
  "The id the next card on BOARD takes, and BOARD with its counter moved."
  (let ((n (getf board :next 1)))
    (setf (getf board :next) (1+ n))
    (format nil "crew-~d" n)))

(defun find-card (board id)
  "BOARD's card named ID, or a refusal naming the ids it does have."
  (let ((wanted (string-downcase (nlk:trimmed (princ-to-string (or id ""))))))
    (or (find wanted (getf board :cards) :key (lambda (c) (string-downcase (getf c :id)))
                                   :test #'equal)
        (fail "no card ~s on board ~a~@[; it holds ~{~a~^, ~}~]"
              wanted (board-name)
              (mapcar (lambda (c) (getf c :id)) (getf board :cards))))))

;;; --- a card and its contract -------------------------------------------------------

(defparameter +fields+
  '((:goal . "goal") (:artifact . "artifact") (:done-when . "done when") (:proof . "proof"))
  "The contract fields a card cannot be proved without, with their names.")

(defun stamp ()
  "Now, as a line a card keeps: YYYY-MM-DD HH:MM."
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (declare (ignore second))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" year month date hour minute)))

(defun filled (value)
  "VALUE as a trimmed string, or NIL when it is nothing."
  (let ((text (and value (nlk:trimmed (princ-to-string value)))))
    (and text (plusp (length text)) text)))

(defun missing-fields (card)
  "The contract fields CARD is still missing, in the words a refusal uses."
  (loop for (key . name) in +fields+
        unless (filled (getf card key)) collect name))

(defun make-card (board &key title goal artifact done-when proof budget)
  "A new card on BOARD, its state decided by how complete its contract is."
  (let ((card (list :id (next-id board)
                    :title (or (filled title) "(untitled)")
                    :goal (filled goal)
                    :artifact (filled artifact)
                    :done-when (filled done-when)
                    :proof (filled proof)
                    :budget (or budget 0)
                    :spent 0
                    :state "draft"
                    :created (stamp)
                    :runs '()
                    :units '())))
    (setf (getf card :state) (if (missing-fields card) "draft" "open"))
    card))

(defun open-state (card)
  "CARD's state from what its contract and its log say, never from what a
worker claimed: a draft has a gap, a card whose newest run exited 0 is
passed, and one with no passing run is open."
  (cond ((equal (getf card :state) "dropped") "dropped")
        ((missing-fields card) "draft")
        ((passed-card-p card) "passed")
        (t "open")))

(defun last-run (card)
  "CARD's newest run, or NIL."
  (car (last (getf card :runs))))

(defun passed-card-p (card)
  "Whether CARD's newest run exited 0: the only thing that passes a card."
  (let ((run (last-run card)))
    (and run (equal "PASS" (getf run :verdict)))))

(defun run-line (run)
  "One run as a line: its verdict, the exit status and what it said."
  (format nil "run ~a ~a (exit ~a)~@[: ~a~]"
          (getf run :n) (getf run :verdict) (getf run :status)
          (let ((out (getf run :output)))
            (and out (plusp (length out)) (nlk:clip (nlk:one-line out) 300)))))

(defun contract-line (card)
  "CARD's contract as one line."
  (format nil "goal ~a; lands in ~a~@[; done when ~a~]~@[; proof `~a`~]"
          (or (getf card :goal) "(none)")
          (or (getf card :artifact) "(none)")
          (filled (getf card :done-when))
          (filled (getf card :proof))))

(defun card-headline (card)
  "CARD as one line of the board: its id, its state and its title."
  (format nil "~a  ~7a  ~a" (getf card :id) (open-state card)
          (nlk:clip (or (getf card :title) "(untitled)") 100)))

(defun units-line (card)
  "How CARD's units stand, or NIL when it has none."
  (let* ((units (getf card :units))
         (passed (count "pass" units :key (lambda (u) (getf u :state)) :test #'equal))
         (failed (count "fail" units :key (lambda (u) (getf u :state)) :test #'equal)))
    (and units (format nil "~d of ~d units passed~@[, ~d failed~], ~d open"
                       passed (length units) (and (plusp failed) failed)
                       (- (length units) passed failed)))))

(defun why (card)
  "Why CARD stands where it does, in one line, and what moves it on."
  (let ((state (open-state card)) (run (last-run card)))
    (cond
      ((equal state "dropped") "dropped by the operator; the history is kept")
      ((equal state "draft")
       (format nil "a draft: its contract still needs ~{~a~^, ~}; (crew:contract \"~a\" ...) fills them"
               (missing-fields card) (getf card :id)))
      ((equal state "passed")
       (format nil "its proof `~a` exited 0 in ~a~@[ (~a)~]"
               (getf card :proof) (getf run :at) (format nil "run ~a" (getf run :n))))
      (t (format nil "its proof `~a` has not exited 0 yet~@[: ~a~]; run (crew:verdict \"~a\") when the work is in"
                 (getf card :proof)
                 (and run (format nil "last run ~a, exit ~a" (getf run :n) (getf run :status)))
                 (getf card :id))))))

(defun describe-card (card)
  "CARD as the lines /crew and (crew:diagnose) answer."
  (format nil "~a~%  contract: ~a~@[~%  units: ~a~]~@[~%  last: ~a~]~%  why: ~a"
          (card-headline card)
          (contract-line card)
          (units-line card)
          (let ((run (last-run card))) (and run (run-line run)))
          (why card)))

(defun board-text ()
  "The board as /crew and (crew:board) answer it."
  (let* ((board (load-board))
         (cards (reverse (getf board :cards)))
         (passed (count "passed" cards :key #'open-state :test #'equal)))
    (if (null cards)
        (format nil "crew board ~a: no card on it; (crew:plan ...) opens one, /crew opens one by asking"
                (board-name))
        (format nil "crew board ~a: ~d card~:p, ~d passed~%~{~a~^~%~}"
                (board-name) (length cards) passed
                (mapcar #'card-headline (remove "dropped" cards :key #'open-state :test #'equal))))))

;;; --- the verbs ---------------------------------------------------------------------

(defun spent-tokens ()
  "The billable tokens this turn has spent so far; 0 outside a turn."
  (let ((turn (ignore-errors (nle:turn))))
    (if turn
        (+ (or (getf turn :input-tokens) 0)
           (or (getf turn :output-tokens) 0)
           (or (getf turn :reasoning-tokens) 0))
        0)))

(defun note-spend (card)
  "Record this turn's spend on CARD: a budget is the card's, not the session's."
  (let ((tokens (spent-tokens)))
    (when (> tokens (getf card :spent 0))
      (setf (getf card :spent) tokens)))
  card)

(defun budget-line (card)
  "CARD's spend against its budget, or NIL when it has none."
  (let ((budget (getf card :budget 0)) (spent (getf card :spent 0)))
    (and (plusp budget) (format nil "~a of ~a tokens" spent budget))))

(defun budget-exhausted (card)
  "Whether CARD has spent its budget and has not passed: the hard stop."
  (let ((budget (getf card :budget 0)))
    (and (plusp budget)
         (>= (getf card :spent 0) budget)
         (not (equal (open-state card) "passed")))))

(defun proving-folder ()
  "The folder a proof runs in: this turn's session folder, else the home."
  (let* ((turn (ignore-errors (nle:turn)))
         (session (and turn (getf turn :session-id)))
         (cwd (and session (ignore-errors (nlk:find-session-cwd session)))))
    (or (and cwd (namestring cwd)) (namestring (nlk:home)))))

(define-verb board ()
  "The board: every card but a dropped one, its id, its state and its title."
  (board-text))

(define-verb plan (&key title goal artifact done-when proof budget)
  "Open a card. Every contract field is required: a missing one is refused,
by name, and no card is written. => the new card's id and contract."
  (with-crew-lock
    (let* ((board (load-board))
           (card (make-card board :title title :goal goal :artifact artifact
                                  :done-when done-when :proof proof
                                  :budget (or budget (setting :budget-tokens))))
           (missing (missing-fields card)))
      (multiple-value-bind (reason needle) (hardline (getf card :proof))
        (when reason
          (fail "the proof command is hardline (`~a`: it ~a), and a proof runs with nobody watching; name a narrower command"
                needle reason)))
      (when missing
        (fail "the contract is missing ~{~a~^, ~}: a card opens only when every field is answered"
              missing))
      (push card (getf board :cards))
      (save-board board)
      (format nil "opened ~a: ~a~%  contract: ~a~@[; budget ~a tokens~]"
              (getf card :id) (getf card :title) (contract-line card)
              (getf card :budget)))))

(define-verb contract (card-id &key title goal artifact done-when proof budget)
  "Fill the contract fields CARD-ID is missing, and answer the ones still
missing: the intake asks only for what a card does not have."
  (with-crew-lock
    (let* ((board (load-board))
           (card (find-card board card-id)))
      (when (equal (open-state card) "dropped")
        (fail "card ~a is dropped; it takes no more contract" (getf card :id)))
      (flet ((put (key value) (when (filled value) (setf (getf card key) (filled value)))))
        (put :title title) (put :goal goal) (put :artifact artifact)
        (put :done-when done-when)
        (when (filled proof)
          (multiple-value-bind (reason needle) (hardline proof)
            (when reason
              (fail "the proof command is hardline (`~a`: it ~a)" needle reason)))
          (put :proof proof)))
      (when (and budget (plusp budget)) (setf (getf card :budget) budget))
      (let ((missing (missing-fields card)))
        (setf (getf card :state) (if missing "draft"
                                      (if (passed-card-p card) "passed" "open")))
        (save-board board)
        (if missing
            (format nil "~a is still a draft: ~{~a~^, ~} missing" (getf card :id) missing)
            (format nil "~a is a complete card: ~a" (getf card :id) (contract-line card)))))))

(define-verb verdict (card-id)
  "Run CARD-ID's proof command in this session's folder and record PASS or
FAIL, with its exit status and output, in the card's log."
  (with-crew-lock
    (let* ((board (load-board))
           (card (find-card board card-id)))
      (note-spend card)
      (let ((missing (missing-fields card)))
        (when missing
          (fail "card ~a is a draft (~{~a~^, ~} missing): there is no proof to run"
                (getf card :id) missing)))
      (when (budget-exhausted card)
        (fail "card ~a has spent its budget (~a), so its proof does not run again: raise :budget or close the card"
              (getf card :id) (budget-line card)))
      (let* ((command (getf card :proof))
             (folder (proving-folder))
             (started (get-internal-real-time)))
        (multiple-value-bind (out err status)
            (nlk:run-bounded (list "sh" "-c" command)
                             :seconds (setting :proof-seconds) :directory folder)
          (let* ((pass (eql status 0))
                 (n (1+ (length (getf card :runs))))
                 (said (nlk:trimmed (format nil "~a~@[~%~a~]" (or out "")
                                            (and err (plusp (length err)) err)))))
            (setf (getf card :runs)
                  (append (getf card :runs)
                          (list (list :n n :verdict (if pass "PASS" "FAIL") :status status
                                      :at (stamp)
                                      :seconds (round (- (get-internal-real-time) started)
                                                      internal-time-units-per-second)
                                      :output (nlk:clip said 800))))
                  (getf card :state) (if pass "passed" "open"))
            (save-board board)
            (format nil "~a ~a: `~a` exited ~a in ~a~@[~%  ~a~]~@[; budget ~a~]"
                    (getf card :id) (if pass "PASS" "FAIL") command status folder
                    (and (plusp (length said)) (nlk:clip (nlk:one-line said) 300))
                    (budget-line card))))))))

(define-verb progress (card-id &key name result evidence)
  "List CARD-ID's units of work, or mark one pass or fail with evidence."
  (with-crew-lock
    (let* ((board (load-board))
           (card (find-card board card-id)))
      (note-spend card)
      (if (null (filled name))
          (let ((units (getf card :units)))
            (if (null units)
                (format nil "~a has no unit of work yet: (crew:progress \"~a\" :name \"...\" :result \"pass\" :evidence \"...\") records one"
                        (getf card :id) (getf card :id))
                (format nil "~a: ~a~%~{~a~^~%~}" (getf card :id) (units-line card)
                        (mapcar (lambda (u)
                                  (format nil "  ~7a ~a~@[: ~a~]" (getf u :state) (getf u :name)
                                          (getf u :evidence)))
                                units))))
          (let ((state (string-downcase (filled result))))
            (unless (member state '("pass" "fail") :test #'equal)
              (fail "a unit is marked :result \"pass\" or \"fail\", not ~s" result))
            (when (null (filled evidence))
              (fail "a unit is marked with evidence: what shows it passes or fails"))
            (let ((existing (find (filled name) (getf card :units)
                                  :key (lambda (u) (getf u :name)) :test #'equal)))
              (if existing
                  (setf (getf existing :state) state
                        (getf existing :evidence) (filled evidence))
                  (setf (getf card :units)
                        (append (getf card :units)
                                (list (list :name (filled name) :state state
                                            :evidence (filled evidence)))))))
            (save-board board)
            (format nil "~a: unit ~s ~a~@[; ~a~]~@[~%~a~]"
                    (getf card :id) (filled name)
                    (if (equal state "pass") "passed" "failed")
                    (filled evidence) (units-line card)))))))

(define-verb diagnose (&optional card-id)
  "The board with why each card stands where it does, or one card's report."
  (with-crew-lock
    (let* ((board (load-board))
           (cards (reverse (getf board :cards))))
      (cond ((filled card-id) (describe-card (find-card board card-id)))
            ((null cards) (format nil "crew board ~a: no card on it; (crew:plan ...) opens one"
                                   (board-name)))
            (t (format nil "crew board ~a: ~d card~:p~2%~{~a~^~2%~}"
                       (board-name) (length cards) (mapcar #'describe-card cards)))))))

(define-verb proof-check (command)
  "Whether COMMAND names a hardline command, without running anything."
  (let ((command (filled command)))
    (unless command (fail "a proof check needs a command to read"))
    (multiple-value-bind (reason needle) (hardline command)
      (if reason
          (format nil "hardline: `~a` carries `~a`, which ~a: a card refuses it as a proof"
                  command needle reason)
          (format nil "ok: `~a` names no hardline command, so a card may keep it as its proof (a text check, not a sandbox)"
                  command)))))

(defun drop-card (card-id)
  "Drop CARD-ID: it stays on the board's history with its runs and units."
  (with-crew-lock
    (let* ((board (load-board))
           (card (find-card board card-id)))
      (setf (getf card :state) "dropped")
      (save-board board)
      (format nil "dropped ~a; its runs and units are kept" (getf card :id)))))

(defun draft (title)
  "Open a card with a title alone; the contract is completed by (crew:contract ...)."
  (with-crew-lock
    (let* ((board (load-board))
           (card (make-card board :title title)))
      (push card (getf board :cards))
      (save-board board)
      (format nil "opened ~a as a draft: ~{~a~^, ~} missing; (crew:contract \"~a\" :goal \"...\" :artifact \"...\" :done-when \"...\" :proof \"...\") completes it"
              (getf card :id) (missing-fields card) (getf card :id)))))

;;; --- what the model calls ------------------------------------------------------------
;;; One declaration is both the schema the provider is shown and the value the
;;; body runs on.

(nlk:define-contract crew-plan
  (:doc "Open a card on the crew board: the work, where it lands, what holds when it is done, and the one shell command that decides it -- the proof, done when that exits 0. A proof runs with nobody watching, so name one narrow command (a test, a build, a check), never a whole session's work. Every field is required and a missing one is refused, naming it. Answer with the card's id: (crew:verdict \"<id>\") runs its proof.")
  ("title" :text :required t :doc "one line naming the card")
  ("goal" :text :required t :doc "what the card delivers, in one or two sentences")
  ("artifact" :text :required t :doc "where the result lands: a file, a page, a commit")
  ("done_when" :text :required t :doc "what holds when it is done, in words")
  ("proof" :text :required t :doc "the shell command that decides it: exits 0 = done, e.g. 'make test'")
  ("budget_tokens" :integer :default 0 :doc "tokens this card may spend before its proof is refused again; 0 for the cell's default"))

(nlk:define-contract crew-verdict
  (:doc "Run a card's proof command in this session's folder and record PASS or FAIL, with its exit status and its output, in the card's verdict log. The card is passed only by a run that exits 0; a draft with a contract field still missing is refused. Running it again is how a failed card is retried.")
  ("card" :text :required t :doc "the card's id, e.g. crew-1"))

(nlk:define-contract crew-progress
  (:doc "List a card's units of work, or mark one unit pass or fail with the evidence that shows it. A unit is a step of the card's work; a mark without evidence is refused. Marks are the worker's record, not the verdict: only the proof command passes the card.")
  ("card" :text :required t :doc "the card's id, e.g. crew-1")
  ("unit" :string :default "" :doc "the unit to mark; leave it out to list them")
  ("result" :string :default "" :doc "pass or fail, with a unit")
  ("evidence" :string :default "" :doc "what shows the unit passes or fails: a command's output, a path, a line"))

(nlk:define-contract crew-diagnose
  (:doc "Read why each card on the board stands where it does -- its contract, its runs, its units and what moves it on -- or one card's report. Read-only: it proves, retries and drops nothing.")
  ("card" :string :default "" :doc "one card's id; leave it out for the whole board"))

(nlk:define-contract crew-proof-check
  (:doc "Ask whether a command would be refused as a card's proof, without running it: hardline commands (a root wipe, a mkfs, a fork bomb) are. Use it before proposing a proof whose command is destructive.")
  ("command" :text :required t :doc "the shell command to read"))

(defun plan-tool (arguments)
  "crew_plan: open a card from the contract the model filled."
  (let ((budget (crew-plan-budget-tokens arguments)))
    (plan :title (crew-plan-title arguments)
          :goal (crew-plan-goal arguments)
          :artifact (crew-plan-artifact arguments)
          :done-when (crew-plan-done-when arguments)
          :proof (crew-plan-proof arguments)
          :budget (and (plusp budget) budget))))

(defun verdict-tool (arguments)
  "crew_verdict: run the card's proof."
  (verdict (crew-verdict-card arguments)))

(defun progress-tool (arguments)
  "crew_progress: list the card's units, or mark one."
  (progress (crew-progress-card arguments)
            :name (crew-progress-unit arguments)
            :result (crew-progress-result arguments)
            :evidence (crew-progress-evidence arguments)))

(defun diagnose-tool (arguments)
  "crew_diagnose: the board, or one card."
  (diagnose (crew-diagnose-card arguments)))

(defun proof-check-tool (arguments)
  "crew_proof_check: is this command hardline?"
  (proof-check (crew-proof-check-command arguments)))

;;; --- the operator's command ----------------------------------------------------------

(defun crew-dialog ()
  "The rows /crew offers typed alone."
  (nle:list-dialog
   :title "Crew" :summary "a card is done when its proof exits 0"
   :rows (list (nle:ask-row "Open a card" "What it delivers" "/crew open"
                             :placeholder "one line" :verb "open")
               (nle:ask-row "Prove a card" "Card id" "/crew prove"
                             :placeholder "crew-1" :verb "prove")
               (nle:ask-row "Read a card" "Card id, blank for the board" "/crew diagnose"
                             :placeholder "crew-1" :verb "read")
               (nle:ask-row "Drop a card" "Card id" "/crew drop"
                             :placeholder "crew-1" :verb "drop"))))

(defun run-slash (args session-id)
  "One /crew invocation: the board and its rows, or one option over the words after it."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (words (and (plusp (length text))
                     (remove "" (uiop:split-string text :separator '(#\Space #\Tab)) :test #'equal))))
    (cond
      ((null words) (values (board-text) (crew-dialog)))
      (t (let ((option (string-downcase (first words)))
               (rest-text (nlk:trimmed (subseq text (min (length text) (length (first words)))))))
           (cond
             ((equal option "open")
              (if (plusp (length rest-text))
                  (values (draft rest-text) nil)
                  (values "what does the card deliver? /crew open <what it delivers>" (crew-dialog))))
             ((equal option "prove")
              (values (verdict rest-text) nil))
             ((equal option "diagnose")
              (values (diagnose rest-text) nil))
             ((equal option "drop")
              (values (drop-card rest-text) nil))
             (t (values (format nil "no /crew option ~(~a~): open, prove, diagnose or drop~%~a"
                                option (board-text))
                        (crew-dialog)))))))))

;;; --- the declaration --------------------------------------------------------------

(nle:define-cell crew
  (:section ("crew")
    (:guide "a board is one file under the home; a proof is shell that runs with nobody watching, so keep its ceiling low")
    ("board" :string :default "board" :doc "which board the cards are kept on, one file under the home")
    ("proof_seconds" :integer :default 300 :min 1 :doc "how long one proof command may run")
    ("budget_tokens" :integer :default 0 :min 0 :doc "a card's token budget when its plan names none; 0 for none"))
  (:tool "crew_plan" (nlk:contract 'crew-plan) 'plan-tool)
  (:tool "crew_verdict" (nlk:contract 'crew-verdict) 'verdict-tool)
  (:tool "crew_progress" (nlk:contract 'crew-progress) 'progress-tool)
  (:tool "crew_diagnose" (nlk:contract 'crew-diagnose) 'diagnose-tool)
  (:tool "crew_proof_check" (nlk:contract 'crew-proof-check) 'proof-check-tool)
  (:command "crew" 'run-slash
            :catches 'crew-error
            :description "Cards that are done only when their proof exits 0"
            :argument-hint "[open|prove|diagnose|drop] ...")
  (:help :crew "crew: cards with a contract, done when their proof command exits 0 - (crew:verdict \"crew-1\")" +primer+))
