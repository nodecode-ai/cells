;;;; cell.lisp --- the declaration, the hook on SH, the command, the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Safe-Swap never blocks and never nags: a destructive shell command is
;;;; rewritten into its reversible twin BEFORE it runs -- rm -r becomes a move
;;;; into a quarantine directory, git push --force becomes a lease, git reset
;;;; --hard, checkout --, restore, clean and stash drop take a snapshot first --
;;;; and a receipt is kept that /safe-swap undo puts back. The model is told
;;;; what really ran, and told when a destructive command was NOT swapped.
;;;;
;;;; The seam is one hook on the SH function, around every call: the command
;;;; string is rebuilt and passed through, and the answer is annotated. A
;;;; hook that errors calls SH as written, so nothing is ever blocked.

(in-package #:nodecode-safe-swap)

(defparameter *inside* nil
  "T while this cell's own hook is running the command: no recursion.")

(defparameter +what+
  '(("rm" . "move-to-quarantine")
    ("push" . "git push --force-with-lease --force-if-includes (not --force)")
    ("reset" . "preceded by a stash snapshot")
    ("checkout" . "preceded by a stash snapshot")
    ("restore" . "preceded by a stash snapshot")
    ("snapshot" . "preceded by a stash snapshot")
    ("branch" . "preceded by a record of the branch tip")
    ("clean" . "preceded by a snapshot of the untracked files")
    ("stash" . "preceded by a record of the dropped stash entries"))
  "What each kind of swap did, in the words the note uses.")

(defparameter +manual+
  "Safe-Swap swaps a destructive shell command for its reversible twin before it runs, and keeps a receipt.
  (sswap:plan COMMAND)   what would run: the reversible twin, with no receipt and nothing executed
  (sswap:receipts)       the receipts this quarantine holds, newest first
  (sswap:undo ID)        put back everything receipt ID quarantined or snapshotted ('last' for the newest)
  (sswap:purge ID)       delete receipt ID's quarantine directory; nothing else is ever deleted
  (sswap:status)         the quarantine directory, the rules that are on, and how many receipts there are
What runs: rm -r and rm (a move into <quarantine>/<id>/<n>, with the file and byte count in the note);
git push --force/-f and a +refspec (--force-with-lease --force-if-includes); git reset --hard, git
checkout --, git restore, git clean -f and git stash drop/clear (a snapshot first: a private ref under
refs/safe-swap, the HEAD, the branch tips, or a tar of what clean may delete); git branch -D (the tip).
A command that runs through sudo, env, xargs, eval, sh -c or find, uses $( ), backticks, a subshell, an
unreadable option, a redirection, a variable or a glob in an option, or a protected target (/, ~, a
system directory, the quarantine itself) runs exactly as written, and the model is told which and why.
The one thing to remember: a swapped rm no longer looks like a recursive delete to a guard that reads
the command, and it is reversible from the quarantine instead."
  "What (help :safe-swap) answers while the cell runs.")

(defun parse-settings (values table)
  "VALUES with quarantine_dir checked: an absolute path or ~/path, never with .. in it."
  (declare (ignore table))
  (let ((directory (getf values :quarantine-dir)))
    (when (pathnamep directory) (setf directory (namestring directory) (getf values :quarantine-dir) directory))
    (unless (and (stringp directory)
                 (or (uiop:string-prefix-p "/" directory)
                     (uiop:string-prefix-p "~/" directory))
                 (not (search ".." directory)))
      (error 'nlk:config-refusal
             :detail (format nil "quarantine_dir is an absolute path or a path under the home directory, with no .. in it: ~s"
                             directory)))
    values))

(defun current-rules ()
  "The switches the hook passes to the rewriter, out of the section."
  (list :rm (setting :rule-rm)
        :single-rm (setting :swap-single-rm)
        :push (setting :rule-git-push)
        :reset (setting :rule-git-reset)
        :restore (setting :rule-git-restore)
        :branch (setting :rule-git-branch)
        :clean (setting :rule-git-clean)
        :stash (setting :rule-git-stash)
        :qdir (setting :quarantine-dir)
        :rid (new-id)
        :trusted (setting :trusted-force-branches)
        :note (setting :note-to-agent)
        :notify (setting :notify-unswapped)))

(defun clip (text limit)
  "TEXT cut to LIMIT characters, with an ellipsis when it was."
  (let ((plain (string-trim '(#\Space #\Tab #\Newline) (substitute #\Space #\Newline text))))
    (if (> (length plain) limit) (cat (subseq plain 0 (1- limit)) "...") plain)))

(defun not-swapped-note (reason command)
  "The line for a destructive-looking command that ran untouched."
  (format nil "~a~a): ~a" +not-swapped+ reason (clip command 80)))

(defun describe-swap (analysis)
  "The one-line, honest account of what really ran, with the blast radius for an rm swap."
  (let* ((id (analysis-rid analysis))
         (slots (slot-directories (quarantine-dir) id))
         (files 0) (bytes 0) (rmp nil))
    (dolist (slot slots)
      (multiple-value-bind (slot-files slot-bytes) (slot-words slot)
        (incf files slot-files)
        (incf bytes slot-bytes))
      (when (equal "rm" (slot-file slot "kind")) (setf rmp t)))
    (format nil "[safe-swap] `~a` was run as: ~a (~@[~a, ~]receipt ~a). ~a"
            (clip (analysis-command analysis) 100)
            (format nil "~{~a~^; ~}"
                    (mapcar (lambda (kind) (or (cdr (assoc kind +what+ :test #'string=)) kind))
                            (analysis-kinds analysis)))
            (and rmp (plusp files) (blast-text files bytes))
            id
            (if (member "push" (analysis-kinds analysis) :test #'string=)
                "Not undoable (the lease only refuses unsafe pushes)."
                (format nil "Undo: /safe-swap undo ~a" id)))))

(defun answer-text (result rules)
  "RESULT's answer with this cell's own NOT-swapped lines rewritten, and the notes appended."
  (let ((text (or (first result) ""))
        (notes '()))
    (when (search +not-swapped+ text)
      (let ((command (analysis-command (second result))))
        (loop for at = (search +not-swapped+ text)
              while at
              do (let* ((end (or (position #\Newline text :start at) (length text)))
                        (line (subseq text at end))
                        (open (position #\) line)))
                   (if open
                       (setf text (cat (subseq text 0 at)
                                       (not-swapped-note (subseq line (length +not-swapped+) open) command)
                                       (subseq text end)))
                       (return))))))
    (when (and (analysis-rewrite (second result)) (getf rules :note))
      (push (describe-swap (second result)) notes))
    (when (and (null (analysis-rewrite (second result)))
               (getf rules :notify)
               (analysis-skips (second result)))
      (push (not-swapped-note (first (analysis-skips (second result)))
                              (analysis-command (second result)))
            notes))
    (dolist (note (nreverse notes))
      (setf text (cat text
                      (if (or (zerop (length text)) (char= (char text (1- (length text))) #\Newline))
                          "" (string #\Newline))
                      note)))
    text))

(defun swap-hook (next &rest args)
  "The :TOOL-shaped hook on SH: rewrite the command, run it, annotate the answer."
  (let ((command (first args)))
    (if (or *inside* (not (stringp command)))
        (apply next args)
        (let* ((rules (current-rules))
               (result (analyse command rules)))
          (multiple-value-bind (output status)
              (let ((*inside* t))
                (if (analysis-rewrite result)
                    (apply next (cons (analysis-rewrite result) (rest args)))
                    (apply next args)))
            (values (answer-text (list output result) rules) status))))))

;;; --- the verbs the model calls through eval ----------------------------------

(defun plan (command &optional rules)
  "What (sh COMMAND) would run: the reversible twin, and why when it is left alone."
  (let* ((rules (or rules (current-rules)))
         (result (analyse command rules)))
    (cond ((analysis-rewrite result)
           (format nil "~a~%was run as: ~a~%receipt ~a"
                   command (analysis-rewrite result) (analysis-rid result)))
          ((analysis-skips result)
           (format nil "~a~%runs as written: ~a" command (first (analysis-skips result))))
          (t (format nil "~a~%runs as written: nothing to swap" command)))))

(defun receipts (&key (limit 20))
  "The receipts this quarantine holds, newest first."
  (receipt-table (quarantine-dir) :limit limit))

(defun undo (id &key force)
  "Put back everything receipt ID quarantined or snapshotted; 'last' is the newest."
  (let* ((base (quarantine-dir))
         (id (if (string= id "last") (or (first (receipt-ids base)) (fail "no receipts to undo")) id))
         (script (restore-script base (checked-id id) :force force)))
    (multiple-value-bind (text status) (run-script script)
      (format nil "Undo ~a~@[: exit ~a~].~%~a" id (and status (plusp status) status)
              (if (zerop (length text)) "nothing to restore" text)))))

(defun purge (&optional id)
  "Delete receipt ID's quarantine directory (every one when ID is NIL)."
  (let* ((base (quarantine-dir))
         (ids (if id (list (checked-id id)) (receipt-ids base))))
    (if (null ids)
        "No receipts to purge."
        (progn
          (dolist (one ids (format nil "Purged ~d receipt~:p." (length ids)))
            (run-script (purge-script base one)))))))

(defun status ()
  "The quarantine directory, the rules that are on, and how many receipts there are."
  (format nil "safe-swap: quarantine ~a, ~d receipt~:p. rules: rm ~a (single rm ~a), push ~a, reset ~a, ~
               restore ~a, clean ~a, stash ~a, branch ~a; notes ~a, not-swapped notices ~a.~@[ trusted force branches: ~{~a~^ ~}~]"
          (quarantine-dir) (length (receipt-ids (quarantine-dir)))
          (if (setting :rule-rm) "on" "off") (if (setting :swap-single-rm) "on" "off")
          (if (setting :rule-git-push) "on" "off") (if (setting :rule-git-reset) "on" "off")
          (if (setting :rule-git-restore) "on" "off") (if (setting :rule-git-clean) "on" "off")
          (if (setting :rule-git-stash) "on" "off") (if (setting :rule-git-branch) "on" "off")
          (if (setting :note-to-agent) "on" "off") (if (setting :notify-unswapped) "on" "off")
          (setting :trusted-force-branches)))

(defun run-slash (args session-id)
  "The /safe-swap command: list, undo ID, purge [ID], or status."
  (declare (ignore session-id))
  (let* ((words (uiop:split-string (nlk:trimmed (or args "")) :separator " "))
         (head (or (first words) "list")))
    (cond ((member head '("list" "ls") :test #'string=) (receipts))
          ((string= head "undo") (undo (or (second words) "last")))
          ((string= head "purge") (purge (second words)))
          ((string= head "status") (status))
          (t (format nil "~a~%/safe-swap list | /safe-swap undo <id|last> | /safe-swap purge [id] | /safe-swap status"
                     (receipts))))))

(defun sh-function ()
  "The SH an eval snippet runs a command through: hooked, so every command is seen."
  (or (find-symbol "SH" '#:nodecode.evolved) (fail "this image has no SH")))

(nle:define-cell safe-swap
  (:section ("safe-swap")
    (:guide "nothing here is required; quarantine_dir is where swapped deletions are kept; every rule can be switched off")
    ("quarantine_dir" :string :default "~/.safe-swap/trash"
     :doc "where a swapped rm moves its targets: an absolute path or ~/path")
    ("rule_rm" :boolean :default t :doc "swap rm for a move into the quarantine")
    ("swap_single_rm" :boolean :default t :doc "also swap a plain single-file rm; off, only recursive or glob rm is swapped")
    ("rule_git_push" :boolean :default t :doc "swap git push --force for --force-with-lease --force-if-includes")
    ("rule_git_reset" :boolean :default t :doc "snapshot before git reset --hard")
    ("rule_git_restore" :boolean :default t :doc "snapshot before git checkout -- or git restore")
    ("rule_git_branch" :boolean :default t :doc "record the tip before git branch -D")
    ("rule_git_clean" :boolean :default t :doc "snapshot what git clean may delete first")
    ("rule_git_stash" :boolean :default t :doc "record the entries before git stash drop or clear")
    ("trusted_force_branches" :list :doc "branches where a force push is left alone, as globs (wip/*, scratch)")
    ("note_to_agent" :boolean :default t :doc "append a one-line note with the receipt id to the command's answer")
    ("notify_unswapped" :boolean :default t :doc "tell the model when a destructive command was NOT swapped, and why"))
  (:settings #'parse-settings)
  (:hook (sh-function) #'swap-hook)
  (:help :safe-swap "destructive shell commands swapped for reversible twins, with receipts to undo" +manual+)
  (:command "safe-swap" 'run-slash
            :description "Safe-Swap receipts: list, undo one, purge"
            :argument-hint "list | undo <id|last> | purge [id] | status"))

