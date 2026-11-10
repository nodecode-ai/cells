;;;; cell.lisp --- pushgate: a push gate backed by verification receipts.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Keep verification attached to the content you ship. A receipt binds the
;;;; exact exit status of one command to the Git tree the working directory
;;;; would commit to; `git push' and `gh pr create' are refused until a receipt
;;;; for the content being published has passed. Edit a file and the earlier
;;;; result no longer qualifies for the changed content; commit exactly what
;;;; you checked and the receipt still applies, because the tree id is the
;;;; commit's own.
;;;;
;;;; Three contributions, one declaration:
;;;;   * the :TOOL hook reads the commands an eval call runs, and answers a
;;;;     failure instead of running a push that has no passing receipt;
;;;;   * the `pushgate' tool records one: it runs the configured check itself
;;;;     and takes the exit status from the process, never from the model's
;;;;     word;
;;;;   * /pushgate is the operator's: status, run, check <command>, log, clear.
;;;; The command stays the operator's -- the tool can run a check and read
;;;; the ledger, never change which command the check is.
;;;;
;;;; The ledger is one JSON file in this cell's own data folder under the
;;;; Nodecode home. Nothing here reaches the network: the only subprocesses are
;;;; the user's own git and the check command.

(defpackage #:nodecode-pushgate
  (:use #:cl))

(in-package #:nodecode-pushgate)

(nlk:define-peripheral pushgate :not-running t)

;;; --- git, through the user's own binary ------------------------------------

(defun default-git-runner (directory arguments &key input environment)
  "=> (values OUTPUT EXIT-CODE) of git's ARGUMENTS read in DIRECTORY."
  ;; A fixed binary and a directory, never a shell string: nothing of the
  ;; repository's is interpreted as code here.
  (multiple-value-bind (output errors code)
      (apply #'uiop:run-program
             (append (list "git" "-c" "core.fsmonitor=false" "-C" (namestring directory))
                     arguments)
             (append (list :output :string :error-output :string :ignore-error-status t)
                     (when input (list :input (list :string input)))
                     (when environment (list :environment environment))))
    (declare (ignore errors))
    (values output (or code 0))))

(defvar *git-runner* #'default-git-runner
  "The one seam every git read goes through; tests rebind it.")

(defun git (directory arguments &key input environment)
  (funcall *git-runner* directory arguments :input input :environment environment))

(defun git-line (directory arguments &key input environment)
  "Git's ARGUMENTS as one trimmed line of text, and the exit code."
  (multiple-value-bind (output code) (git directory arguments :input input :environment environment)
    (values (string-trim '(#\Space #\Tab #\Newline #\Return) (or output "")) code)))

(defun git-value (directory &rest arguments)
  "Git's ARGUMENTS trimmed, or NIL when it failed."
  (multiple-value-bind (text code) (git-line directory arguments)
    (and (zerop code) (plusp (length text)) text)))

(defun repo-root (directory)
  "The top of the repository DIRECTORY sits in, or NIL."
  (and directory (git-value directory "rev-parse" "--show-toplevel")))

(defun commit-tree (root revision)
  "=> (values COMMIT TREE) when REVISION names a commit in ROOT."
  (let ((commit (git-value root "rev-parse" "--verify" "-q" "--end-of-options"
                           (format nil "~a^{commit}" revision))))
    (when commit
      (values commit (git-value root "rev-parse" "--verify" "-q" "--end-of-options"
                                       (format nil "~a^{tree}" commit))))))

;;; --- the fingerprint -------------------------------------------------------

(defstruct (fingerprint (:constructor make-fingerprint (&key tree head head-tree (clean nil))))
  "What ROOT would commit to: TREE, the id `git write-tree' answers for the
working directory, and whether that is HEAD's own tree."
  (tree "" :type string)
  (head nil)
  (head-tree "" :type string)
  (clean nil))

(defun temp-index-file ()
  "A path for one fingerprint's throwaway index, in this cell's data folder."
  (let ((directory (ensure-directories-exist (nlk:home "pushgate/index/"))))
    (namestring (merge-pathnames (format nil "index-~d-~d" (get-universal-time) (random 1000000))
                                 directory))))

(defun git-environment (index)
  "The environment one staging call runs with: INDEX for its own index, and
none of the caller's own GIT_* overrides."
  (cons (format nil "GIT_INDEX_FILE=~a" index)
        (cons "GIT_TERMINAL_PROMPT=0"
              (cons "GIT_OPTIONAL_LOCKS=0"
                    (remove-if (lambda (var) (uiop:string-prefix-p "GIT_" var))
                               (sb-ext:posix-environ))))))

(defun fingerprint (root)
  "The tree id ROOT's working directory would commit to, without touching the
repository's own index, HEAD or refs; the tree is staged in an index of ours.
=> FINGERPRINT, or (values NIL REASON) when it cannot be computed."
  (let ((index (temp-index-file)))
    (unwind-protect
         (let ((environment (git-environment index)))
           (multiple-value-bind (staged code) (git-line root '("add" "-A") :environment environment)
             (declare (ignore staged))
             (if (/= code 0)
                 (values nil "git could not stage the working tree")
                 (multiple-value-bind (tree tree-code) (git-line root '("write-tree") :environment environment)
                   (if (or (/= tree-code 0) (zerop (length tree)))
                       (values nil "git could not write the tree")
                       (let ((head-tree (git-value root "rev-parse" "--verify" "-q" "HEAD^{tree}")))
                         (make-fingerprint
                          :tree tree
                          :head (git-value root "rev-parse" "--verify" "-q" "HEAD^{commit}")
                          :head-tree (or head-tree
                                         (git-value root "hash-object" "-t" "tree" "--stdin" :input ""))
                          :clean (and head-tree (string= tree head-tree)))))))))
      (ignore-errors (delete-file index)))))

;;; --- the check, and the ledger ---------------------------------------------

(defun default-check-runner (command directory)
  "=> (values OUTPUT EXIT-CODE) of COMMAND, a shell string, run in DIRECTORY."
  (multiple-value-bind (output errors code)
      (uiop:run-program command :directory (namestring directory)
                         :output :string :error-output :string :ignore-error-status t)
    (declare (ignore errors))
    (values output (or code 0))))

(defvar *check-runner* #'default-check-runner
  "The one seam every check run goes through; tests rebind it.")

(defvar *ledger* nil
  "The kept ledger while the cell runs: {checks: {ROOT: COMMAND}, receipts: {ROOT: [RECEIPT...]}}.")

(defvar *ledger-file* nil
  "Where the ledger is kept, fixed when the cell starts.")

(defun ledger-file ()
  (or *ledger-file* (nlk:home "pushgate/ledger.json")))

(defun empty-ledger ()
  (nlk:make-json-object "checks" (nlk:make-json-object) "receipts" (nlk:make-json-object)))

(defun ledger-table (name)
  (let ((table (and *ledger* (gethash name *ledger*))))
    (if (hash-table-p table)
        table
        (setf (gethash name *ledger*) (nlk:make-json-object)))))

(defun load-ledger ()
  "Read the kept ledger, or start an empty one."
  (setf *ledger*
        (or (ignore-errors
              (nlk:json-value (nlk:decode-json (nlk:read-text (ledger-file))) :object))
            (empty-ledger)))
  (ledger-table "checks")
  (ledger-table "receipts")
  *ledger*)

(defun save-ledger ()
  (when *ledger*
    (nlk:write-file-atomically (ledger-file) (nlk:encode-json-object *ledger*)
                               :mode #o600 :directory-mode #o700)))

(defun clean-command (text)
  (let ((trimmed (and (stringp text) (string-trim '(#\Space #\Tab) text))))
    (and trimmed (plusp (length trimmed)) trimmed)))

(defun check-for (root)
  "The check command ROOT's pushes are gated on: this repository's own, else
the section's, else NIL -- and then nothing in it is refused."
  (or (clean-command (gethash root (ledger-table "checks")))
      (clean-command (setting :check))))

(defun set-check (root command)
  (let ((table (ledger-table "checks")))
    (if command
        (setf (gethash root table) command)
        (remhash root table))
    (save-ledger)
    command))

(defun receipts-of (root)
  "ROOT's receipts, newest first."
  (let ((kept (gethash root (ledger-table "receipts"))))
    (if (vectorp kept) kept #())))

(defun receipt-field (receipt name)
  (and (hash-table-p receipt) (nlk:json-value receipt :string name)))

(defun receipt-matches-p (receipt tree command)
  (and (equal (receipt-field receipt "tree") tree)
       (equal (receipt-field receipt "command") command)))

(defun latest-receipt (root tree command)
  "The newest conclusive receipt for exactly TREE and COMMAND, or NIL."
  (find-if (lambda (receipt)
             (and (member (receipt-field receipt "status") '("passed" "failed") :test #'equal)
                  (receipt-matches-p receipt tree command)))
           (receipts-of root)))

(defun last-passing (root command)
  (find-if (lambda (receipt)
             (and (equal (receipt-field receipt "status") "passed")
                  (equal (receipt-field receipt "command") command)))
           (receipts-of root)))

(defun record-receipt (root command tree status exit-code)
  "Put one receipt at the front of ROOT's list, keeping the newest few."
  (let* ((kept (receipts-of root))
         (entry (nlk:make-json-object "at" (get-universal-time)
                                       "command" command
                                       "tree" tree
                                       "status" status
                                       "exit_code" (or exit-code 0)))
         (limit (max 1 (or (setting :max-receipts) 200)))
         (fresh (concatenate 'vector (vector entry) kept)))
    (setf (gethash root (ledger-table "receipts"))
          (if (> (length fresh) limit) (subseq fresh 0 limit) fresh))
    (save-ledger)
    entry))

(defun run-check (root command)
  "Run COMMAND in ROOT, fingerprinting before and after.
=> (values STATUS EXIT-CODE TREE OUTPUT); STATUS is passed, failed, changed
(the content moved during the run) or unknown."
  (multiple-value-bind (before reason) (fingerprint root)
    (if (null before)
        (values "unknown" nil nil (format nil "no fingerprint: ~a" reason))
        (multiple-value-bind (output code) (funcall *check-runner* command root)
          (multiple-value-bind (after after-reason) (fingerprint root)
            (declare (ignore after-reason))
            (let* ((tree (fingerprint-tree before))
                   (status (cond ((null after) "unknown")
                                 ((not (string= (fingerprint-tree after) tree)) "changed")
                                 ((zerop code) "passed")
                                 (t "failed"))))
              (record-receipt root command tree status code)
              (values status code tree output)))))))

;;; --- what a push would publish ---------------------------------------------

(defparameter +spawn-heads+ '("sh" "run-program" "launch-program" "run-shell-command")
  "The heads whose argument is a command the call runs.")

(defparameter +wrappers+ '("sudo" "doas" "env" "command" "nohup" "exec" "nice" "time")
  "Programs that step out of the way of the command they run.")

(defun name-of (x)
  "X's symbol name downcased, or NIL: a form read in any package is read."
  (and (symbolp x) (string-downcase (symbol-name x))))

(defun program-name (word)
  "WORD's file name, so /usr/bin/git is git."
  (let ((slash (position #\/ word :from-end t)))
    (if slash (subseq word (1+ slash)) word)))

(defun dollar-entry (x strings)
  "The text a ($ \"name\") form reads from STRINGS, or NIL."
  (let ((name (and (consp x) (equal "$" (name-of (car x))) (stringp (second x)) (second x))))
    (and name (hash-table-p strings) (gethash name strings))))

(defun literal-text (x strings)
  "The text X alone names as a command, or NIL when X hides it."
  (cond ((stringp x) x)
        ((dollar-entry x strings))
        ((and (consp x) (member (name-of (car x)) '("list" "quote") :test #'equal))
         (let ((items (if (equal "quote" (name-of (car x))) (second x) (rest x))))
           (when (and (listp items)
                      (every (lambda (item) (or (stringp item) (dollar-entry item strings))) items))
             (format nil "~{~a~^ ~}"
                     (mapcar (lambda (item) (or (dollar-entry item strings) item)) items)))))))

(defun form-commands (form-text strings)
  "The shell commands FORM-TEXT runs, as text; the whole form when one of them
is hidden behind a variable; NIL when it runs none."
  (let ((commands '()) (hidden nil) (ran nil))
    (labels ((walk (x)
               (cond ((consp x)
                      (when (member (name-of (car x)) +spawn-heads+ :test #'equal)
                        (setf ran t)
                        (let ((text (literal-text (second x) strings)))
                          (if text (push text commands) (setf hidden t))))
                      (walk (car x))
                      (walk (cdr x)))
                     ((and x (member (name-of x) +spawn-heads+ :test #'equal))
                      (setf ran t)))))
      (ignore-errors
        (with-input-from-string (in form-text)
          (let ((*read-eval* nil) (*package* (find-package :nodecode-pushgate)))
            (loop for form = (read in nil :eof)
                  until (eq form :eof)
                  do (walk form)))))
      (cond ((and ran (not hidden)) (nreverse commands))
            (ran (list form-text))))))

(defun shell-segments (text)
  "TEXT cut at its unquoted ; & | and newlines: the commands it runs."
  (let ((segments '()) (start 0) (i 0) (n (length text)) (quote nil))
    (loop while (< i n)
          do (let ((c (char text i)))
               (cond ((char= c #\\) (incf i 2))
                     (quote (when (char= c quote) (setf quote nil)) (incf i))
                     ((or (char= c #\") (char= c #\')) (setf quote c) (incf i))
                     ((or (member c '(#\; #\& #\|) :test #'char=) (char= c #\Newline))
                      (push (subseq text start i) segments)
                      (setf start (1+ i))
                      (incf i))
                     (t (incf i)))))
    (push (subseq text start) segments)
    (remove "" (mapcar (lambda (segment)
                         (string-trim '(#\Space #\Tab #\Return) segment))
                       (nreverse segments))
            :test #'equal)))

(defun shell-tokens (segment)
  "SEGMENT's words, with their quotes taken off."
  (let ((words '()) (current nil) (i 0) (n (length segment)) (quote nil))
    (flet ((finish ()
             (when current
               (push (format nil "~{~a~}" (nreverse current)) words)
               (setf current nil))))
      (loop while (< i n)
            do (let ((c (char segment i)))
                 (cond ((char= c #\\)
                        (push (char segment (min (1+ i) (1- n))) current)
                        (incf i 2))
                       (quote (if (char= c quote) (setf quote nil) (push c current)) (incf i))
                       ((or (char= c #\") (char= c #\')) (setf quote c) (incf i))
                       ((member c '(#\Space #\Tab)) (finish) (incf i))
                       (t (push c current) (incf i))))
            finally (finish))
      (nreverse words))))

(defun static-text-p (text)
  "TEXT names one path or one ref, with no shell expansion in it."
  (and (stringp text)
       (plusp (length text))
       (notany (lambda (c) (find c "$`*?{}~")) text)))

(defun resolve-here (path base)
  "PATH as a directory, relative to BASE unless it is absolute."
  (uiop:ensure-directory-pathname
   (merge-pathnames (uiop:parse-native-namestring path)
                    (uiop:ensure-directory-pathname base))))

(defun strip-prefix (words)
  "WORDS with leading assignments and wrappers taken off."
  (loop while (and words
                   (let ((word (first words)))
                     (or (and (find #\= word)
                              (not (and (plusp (length word)) (char= #\- (char word 0)))))
                         (member (program-name word) +wrappers+ :test #'equal))))
        do (pop words))
  words)

(defun push-refs (arguments)
  "=> (values REFS WHY SKIP) for `git push' ARGUMENTS: the revisions whose
trees must carry receipts, why they cannot be known, and whether to skip."
  (let ((positional '()) (why nil) (skip nil) (i 0) (n (length arguments)))
    (loop while (< i n)
          do (let ((argument (nth i arguments)))
               (incf i)
               (cond ((member argument '("--all" "--mirror" "--branches" "--tags" "--follow-tags")
                              :test #'equal)
                      (setf why (format nil "`git push ~a` publishes more than the named commits" argument)))
                     ((member argument '("-d" "--delete" "-n" "--dry-run") :test #'equal)
                      (setf skip t))
                     ((member argument '("-o" "--push-option") :test #'equal) (incf i))
                     ((and (plusp (length argument)) (char= #\- (char argument 0))) nil)
                     (t (push argument positional)))))
    (let* ((refspecs (rest (nreverse positional)))
           (refs (loop for spec in refspecs
                       for source = (first (uiop:split-string
                                            (if (uiop:string-prefix-p "+" spec) (subseq spec 1) spec)
                                            :separator ":"))
                       when (and source (static-text-p source))
                         collect source)))
      (cond (skip (values nil nil t))
            ((and refspecs (null refs))
             (values nil "a dynamic or wildcard refspec is not supported" nil))
            ((null refs) (values (list "HEAD") why nil))
            (t (values refs why nil))))))

(defun git-action (words directory)
  "The action a `git ...' segment runs, or NIL."
  (let ((rest (rest words)) (dir directory) (why nil))
    (loop while (and rest (plusp (length (first rest))) (char= #\- (char (first rest) 0)))
          do (let ((option (pop rest)))
               (cond ((equal option "-C") (setf dir (resolve-here (or (pop rest) ".") dir)))
                     ((equal option "-c")
                      (setf why "git configuration overrides are not supported")
                      (pop rest))
                     ((member option '("--git-dir" "--work-tree" "--namespace" "--exec-path"
                                       "--config-env")
                              :test #'equal)
                      (setf why "git options that move the repository are not supported")
                      (pop rest)))))
    (when (and rest (equal (first rest) "push"))
      (multiple-value-bind (refs more skip) (push-refs (rest rest))
        (unless skip
          (list :kind "push" :dir dir :refs refs :why (or why more)))))))

(defun gh-action (words directory)
  "The action a `gh ...' segment runs, or NIL."
  (when (and (>= (length words) 3) (equal (second words) "pr") (equal (third words) "create"))
    (let ((arguments (cdddr words)) (head "HEAD") (why nil) (i 0) (n (length (cdddr words))))
      (loop while (< i n)
            do (let ((argument (nth i arguments)))
                 (incf i)
                 (cond ((member argument '("-H" "--head") :test #'equal)
                        (setf head (or (nth i arguments) "HEAD"))
                        (incf i))
                       ((uiop:string-prefix-p "--head=" argument)
                        (setf head (subseq argument 7)))
                       ((or (member argument '("-R" "--repo") :test #'equal)
                            (uiop:string-prefix-p "--repo=" argument))
                        (setf why "cross-repository PR creation is not supported")))))
      (let ((source (car (last (uiop:split-string head :separator ":")))))
        (list :kind "pr-create" :dir directory
              :refs (if (and source (static-text-p source)) (list source) (list "HEAD"))
              :why why)))))

(defun commands-of (text directory &optional (depth 0))
  "The guarded actions TEXT would run from DIRECTORY."
  (let ((actions '()) (opaque nil))
    (dolist (segment (shell-segments text))
      (let* ((words (strip-prefix (shell-tokens segment)))
             (head (and words (program-name (first words)))))
        (cond ((null words) nil)
              ((equal head "cd")
               (let ((target (second words)))
                 (when (static-text-p target) (setf directory (resolve-here target directory)))))
              ((and (member head '("sh" "bash" "zsh" "dash") :test #'equal)
                    (< depth 2)
                    (member (second words) '("-c" "-lc" "-ec") :test #'equal))
               (setf actions (append actions (commands-of (or (third words) "") directory (1+ depth)))))
              ((equal head "git")
               (let ((action (git-action words directory)))
                 (if action
                     (push action actions)
                     (when (and (member "push" words :test #'equal) (not opaque))
                       (setf opaque t)))))
              ((equal head "gh")
               (let ((action (gh-action words directory)))
                 (if action
                     (push action actions)
                     (when (and (member "create" words :test #'equal) (not opaque))
                       (setf opaque t)))))
        )))
    (cond ((and opaque (null actions)) (list (list :kind "opaque" :dir directory)))
          (t (nreverse actions)))))

;;; --- the gate --------------------------------------------------------------

(defun session-directory ()
  (or (ignore-errors (nlk:find-session-cwd nlk:*scribe-session-id*))
      (uiop:getcwd)))

(defun short-id (id)
  (subseq id 0 (min 10 (length id))))

(defun changed-since (root base target)
  "The names that differ between BASE and TARGET, at most five and a count."
  (let ((names (and base target (git-value root "diff" "--name-only" "-z" "--no-renames" base target))))
    (when names
      (let ((paths (remove "" (uiop:split-string names :separator (string #\Null)) :test #'equal)))
        (when paths
          (format nil " Changed since the last passing run: ~{~a~^, ~}~@[ (+~d more)~]."
                  (subseq paths 0 (min 5 (length paths)))
                  (max 0 (- (length paths) 5))))))))

(defun blocked (check commit tree root)
  "The refusal a push of COMMIT earns, naming what would make it pass."
  (let* ((receipt (latest-receipt root tree check))
         (why (if receipt
                  (format nil "the last run of `~a` on exactly this content failed (exit ~a)"
                          check (or (nlk:json-value receipt :integer "exit_code") "?"))
                  (format nil "no passing run of `~a` exists for this content" check)))
         (hint (if receipt
                   ""
                   (let ((last (last-passing root check)))
                     (or (changed-since root (and last (receipt-field last "tree")) tree) "")))))
    (format nil "ERROR: refused by nodecode-pushgate: `git push` of commit ~a -- ~a.~a Run the check through the pushgate tool (action \"run\") in ~a, fix anything it reports, and push again"
            (short-id commit) why hint root)))

(defun action-refusal (action)
  "The refusal ACTION earns, or NIL."
  (let* ((directory (getf action :dir))
         (root (and directory (repo-root directory))))
    (when root
      (let ((check (check-for root)))
        (when check
          (cond ((getf action :why)
                 (format nil "ERROR: refused by nodecode-pushgate: ~a; run a plain `git push origin HEAD` so the pushed commit can be named"
                         (getf action :why)))
                ((equal (getf action :kind) "opaque")
                 (format nil "ERROR: refused by nodecode-pushgate: this command looks like it pushes from ~a, but the pushed commits cannot be read from it; run `git push` (or `gh pr create`) as a plain command"
                         root))
                (t (dolist (revision (getf action :refs))
                     (multiple-value-bind (commit tree) (commit-tree root revision)
                       (unless commit
                         (return-from action-refusal
                           (format nil "ERROR: refused by nodecode-pushgate: `~a` cannot be resolved to a commit in ~a"
                                   revision root)))
                       (let ((receipt (latest-receipt root tree check)))
                         (unless (and receipt (equal (receipt-field receipt "status") "passed"))
                           (return-from action-refusal (blocked check commit tree root)))))))))))))

(defun gate-refusal (form-text strings)
  "The refusal the commands FORM-TEXT runs earn, or NIL."
  (let ((directory (session-directory)))
    (dolist (command (form-commands form-text strings))
      (dolist (action (commands-of command directory))
        (let ((refusal (action-refusal action)))
          (when refusal (return-from gate-refusal refusal)))))))

(defun gate-tool (op next)
  "The :TOOL point: a push without a passing receipt for what it publishes
answers a failure, and NEXT never runs."
  (let* ((arguments (getf op :arguments))
         (form (and (hash-table-p arguments) (gethash "form" arguments)))
         (strings (and (hash-table-p arguments) (gethash "strings" arguments)))
         (text (cond ((stringp form) form)
                     ((and form (not (eq form :false))) (format nil "~s" form))))
         (refusal (and text (ignore-errors (gate-refusal text strings)))))
    (if refusal
        (nle:failure refusal)
        (funcall next op))))

;;; --- the verbs -------------------------------------------------------------

(defun status-text (root)
  "What a caller asks first: the check, the content, and its receipt."
  (let ((check (check-for root)))
    (multiple-value-bind (current reason) (fingerprint root)
      (if (null current)
          (format nil "pushgate -- ~a~%  check: ~a~%  current content: unknown (~a)"
                  root (or check "none") reason)
          (let ((receipt (and check (latest-receipt root (fingerprint-tree current) check))))
            (format nil "pushgate -- ~a~%  check: ~a~%  current content: tree ~a~a~%  receipt: ~a"
                    root (or check "none (set one with /pushgate check <command>)")
                    (short-id (fingerprint-tree current))
                    (if (fingerprint-clean current) "" " (uncommitted changes)")
                    (cond (receipt (string-upcase (receipt-field receipt "status")))
                          (check "NONE for this content")
                          (t "none, nothing is gated"))))))))

(defun tail-of (text limit)
  (let ((text (or text "")))
    (if (> (length text) limit) (subseq text (- (length text) limit)) text)))

(defun run-text (root command)
  "Run the check now and say what it did to the gate."
  (multiple-value-bind (status code tree output) (run-check root command)
    (let ((head (format nil "pushgate: `~a` ~a on tree ~a~@[ (exit ~a)~].~%"
                         command
                         (if (equal status "passed") "passed" status)
                         (and tree (short-id tree))
                         code)))
      (cond ((equal status "passed")
             (concatenate 'string head
                          "Committing exactly this content keeps the receipt; `git push` is allowed for that tree."))
            ((equal status "changed")
             (concatenate 'string head
                          "The content moved while the check ran, so it proves nothing; run it again on a settled tree."))
            ((equal status "unknown")
             (concatenate 'string head "The repository could not be fingerprinted: " (or output "")))
            (t (concatenate 'string head
                            "`git push` stays blocked for this content. Last lines:~%"
                            (tail-of output 600)))))))

(defun log-text (root)
  (let ((receipts (receipts-of root)))
    (if (zerop (length receipts))
        (format nil "pushgate: no receipts for ~a yet." root)
        (with-output-to-string (out)
          (loop for receipt across (subseq receipts 0 (min 10 (length receipts)))
                do (format out "~a  ~a  tree ~a  exit=~a  `~a`~%"
                           (or (nlk:json-value receipt :integer "at") "")
                           (string-upcase (or (receipt-field receipt "status") "?"))
                           (short-id (or (receipt-field receipt "tree") ""))
                           (or (nlk:json-value receipt :integer "exit_code") "")
                           (or (receipt-field receipt "command") "")))))))

(defun clear-text (root)
  (let* ((table (ledger-table "receipts"))
         (count (length (receipts-of root))))
    (remhash root table)
    (save-ledger)
    (format nil "pushgate: cleared ~d receipt~:p for ~a." count root)))

(defun repo-of-session (session-id)
  (let ((directory (or (and session-id (ignore-errors (nlk:find-session-cwd session-id)))
                       (uiop:getcwd))))
    (values (repo-root directory) directory)))

(defun pushgate-tool (request)
  "The pushgate tool: run the check, or read the ledger of this repository."
  (nlk:with-handlers ((error () "ERROR: pushgate could not answer: the repository could not be read"))
    (let* ((directory (session-directory))
           (root (and directory (repo-root directory))))
      (if (null root)
          (format nil "pushgate: ~a is not inside a git repository." directory)
          (let ((action (pushgate-request-action request))
                (check (check-for root)))
            (cond ((equal action "status") (status-text root))
                  ((equal action "log") (log-text root))
                  ((equal action "clear") (clear-text root))
                  ((null check)
                   (format nil "pushgate: no check is configured for ~a; the operator sets one with /pushgate check <command>." root))
                  (t (run-text root check))))))))

(defun read-to-end (text at)
  (string-trim '(#\Space #\Tab) (subseq text (min at (length text)))))

(defun run-slash (arguments session-id)
  "The /pushgate command: the operator's side of the ledger."
  (let* ((text (string-trim '(#\Space #\Tab) (or arguments "")))
         (space (position #\Space text))
         (word (if space (subseq text 0 space) text))
         (rest (if space (read-to-end text (1+ space)) "")))
    (multiple-value-bind (root directory) (repo-of-session session-id)
      (if (null root)
          (format nil "pushgate: ~a is not inside a git repository." directory)
          (cond ((equal word "check")
                 (if (plusp (length rest))
                     (if (equal rest "--off")
                         (progn (set-check root nil)
                                (format nil "pushgate: turned the gate off for ~a (the section's own check, if any, still applies)."
                                        root))
                         (progn (set-check root rest)
                                (format nil "pushgate: the check for ~a is now `~a`; `git push` and `gh pr create` need a passing run of it."
                                        root (clean-command rest))))
                     "usage: /pushgate check <command>   (or: /pushgate check --off)"))
                ((equal word "clear") (clear-text root))
                ((equal word "log") (log-text root))
                ((equal word "run")
                 (let ((check (check-for root)))
                   (if check
                       (run-text root check)
                       (format nil "pushgate: no check is configured for ~a; set one with /pushgate check <command>." root))))
                (t (status-text root)))))))

;;; --- the manual and the declaration ---------------------------------------

(defparameter +manual+
  "pushgate: a receipt is one run of the configured check, bound to the Git tree
your working directory would commit to. `git push` and `gh pr create` are
refused until a passing receipt exists for exactly the content being published.
Edit and commit, and the earlier receipt no longer qualifies; commit the exact
content you checked, and it still does.

  /pushgate                       the check, the current tree, its receipt
  /pushgate run                   run the check now and record the result
  /pushgate check <command>       set the check for this repository
  /pushgate check --off           turn this repository's own check off
  /pushgate log                   the newest receipts
  /pushgate clear                 drop this repository's receipts

The agent calls the `pushgate` tool with action run, status, log or clear. Only
the operator changes which command is the check. A repository with no check --
neither of its own nor in the section -- is never refused. A workflow gate, not
a security boundary: a push run by the operator's own shell, an alias, or a
publish script is outside it.")

(nlk:define-contract pushgate-request
  (:doc "Verification receipts for the repository this session runs in. action `run` runs the configured check in the repository root, fingerprints the content before and after, and records the result -- a push needs a passing receipt for the exact tree it publishes. `status` says what the check is and whether the current content has a receipt; `log` lists recent ones; `clear` drops them. The operator, not the agent, chooses the check command (/pushgate check <command>).")
  ("action" :string :required t :options '("run" "status" "log" "clear")
            :doc "what to do: run the check, read the status, read the log, or clear the receipts"))

(nle:define-cell pushgate
  (:section ("pushgate")
    (:guide "check is the command that must pass before a push; every repository uses it unless /pushgate check gives that one its own; max_receipts bounds the ledger")
    ("check" :string
     :doc "the command that must pass before git push or gh pr create, run in the repository root")
    ("max_receipts" :integer :default 200 :min 1
     :doc "how many receipts to keep per repository"))
  (:tool "pushgate" (nlk:contract 'pushgate-request) 'pushgate-tool)
  (:help :pushgate "pushgate: git push and gh pr create gated on a passing run of your check" +manual+)
  (:command "pushgate" 'run-slash
            :description "Verification receipts and the push gate"
            :argument-hint "[status|run|check <command>|log|clear]")
  (:hook :tool #'gate-tool)
  (:start (lambda ()
            (load-ledger)
            (nle:on-stop (lambda () (setf *ledger* nil))))))
