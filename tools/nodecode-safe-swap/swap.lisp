;;;; swap.lisp --- what gets swapped: the shell scanner, the rules, the POSIX blocks.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A command is cut into the simple commands it runs (an unquoted ; & | or
;;;; newline separates them); a simple command's words are read with their
;;;; offsets in the command, so a rule replaces exactly the span it understood
;;;; and every other byte is left where it was. A rule that does not understand
;;;; a command's every word answers a SKIP reason instead: the command then
;;;; runs exactly as the model wrote it, and the model is told so.
;;;;
;;;; Every block is ONE grouped command ({ ...; }), so && || | and ; around it
;;;; keep their meaning and its exit status is the original command's.

(in-package #:nodecode-safe-swap)

;;; --- small things ----------------------------------------------------------

(defparameter +walk-cap+ 50000
  "How many entries one rm target is walked for before the count is a lower bound.")

(defparameter +marker+ "SS_RCPT="
  "How a rewritten command names itself: a command that carries it is left alone.")

(defparameter +not-swapped+ "[safe-swap] NOT swapped ("
  "A block's own line when it could not protect the command; the hook rewrites it.")

(defparameter +lease+ "--force-with-lease --force-if-includes")

(define-condition bail (error)
  ((reason :initarg :reason :reader bail-reason))
  (:report (lambda (condition stream) (write-string (bail-reason condition) stream))))

(defun bail (control &rest args)
  "Stop reading the command: its syntax hides what it runs."
  (error 'bail :reason (apply #'format nil control args)))

(defun cat (&rest parts)
  "PARTS concatenated into one string."
  (apply #'concatenate 'string parts))

(defun blanks-p (text)
  "Whether TEXT is empty or white space."
  (every (lambda (character) (member character '(#\Space #\Tab #\Return #\Newline))) text))

(defun sh-quote (text)
  "TEXT as one POSIX shell word, single-quoted."
  (with-output-to-string (out)
    (write-char #\' out)
    (loop for character across text
          do (if (char= character #\') (write-string "'\\''" out) (write-char character out)))
    (write-char #\' out)))

(defun new-id ()
  "A fresh receipt id: six hex digits, as the original's ids were."
  (with-output-to-string (out)
    (dotimes (index 6)
      (declare (ignore index))
      (write-char (char "0123456789abcdef" (random 16)) out))))

;;; --- words -----------------------------------------------------------------

(defstruct (word (:constructor make-word (value raw start end expand glob)))
  "One word of a simple command: its unquoted VALUE, its RAW text and its offsets."
  value raw start end expand glob)

(defun word-option-p (word)
  "Whether WORD is spelled as an option (a leading -)."
  (let ((value (word-value word)))
    (and (plusp (length value)) (char= (char value 0) #\-))))

(defun command-name (word)
  "WORD's program name, or \"\" when it hides behind an expansion or a glob."
  (if (or (word-expand word) (word-glob word))
      ""
      (let* ((value (word-value word))
             (slash (position #\/ value :from-end t)))
        (if slash (subseq value (1+ slash)) value))))

(defun split-commands (command)
  "The (START . END) spans of the simple commands COMMAND runs, in order."
  (let ((length (length command)) (spans '()) (start 0) (index 0) (quote nil))
    (loop while (< index length)
          do (let ((character (char command index)))
               (cond
                 ((and (char= character #\') (null quote)) (setf quote character) (incf index))
                 ((and (char= character #\') (char= quote #\')) (setf quote nil) (incf index))
                 ((and (char= character #\") (null quote)) (setf quote character) (incf index))
                 ((and (char= character #\") (char= quote #\")) (setf quote nil) (incf index))
                 (quote (incf index))
                 ((char= character #\\) (incf index (min 2 (- length index))))
                 ((char= character #\`) (bail "command substitution"))
                 ((and (char= character #\$) (< (1+ index) length)
                       (char= (char command (1+ index)) #\())
                  (bail "command substitution"))
                 ((or (char= character #\() (char= character #\))) (bail "subshell"))
                 ((char= character #\Newline)
                  (push (cons start index) spans) (incf index) (setf start index))
                 ((member character '(#\; #\& #\|))
                  (push (cons start index) spans)
                  (incf index)
                  (when (and (< index length) (char= (char command index) character)
                             (not (char= character #\;)))
                    (incf index))
                  (setf start index))
                 ((char= character #\Return) (incf index))
                 (t (incf index)))))
    (push (cons start length) spans)
    (loop for (from . to) in (nreverse spans)
          for first = (position-if-not (lambda (character)
                                         (member character '(#\Space #\Tab #\Return)))
                                       command :start from :end to)
          when first
            collect (cons first (1+ (position-if-not (lambda (character)
                                                       (member character '(#\Space #\Tab #\Return)))
                                                     command :start first :end to :from-end t))))))

(defun read-words (command start end)
  "The words of COMMAND between START and END, in order, with their offsets."
  (let ((words '()) (index start) (from nil) (value nil) (quote nil) (expand nil) (glob nil))
    (flet ((flush (stop)
             (when from
               (push (make-word (get-output-stream-string value) (subseq command from stop)
                                from stop expand glob)
                     words)
               (setf from nil value nil expand nil glob nil))))
      (loop while (< index end)
            do (let ((character (char command index)))
                 (cond
                   ((and (null quote) (member character '(#\Space #\Tab #\Return)))
                    (flush index)
                    (incf index))
                   (t
                    (unless from (setf from index value (make-string-output-stream)))
                    (cond
                      ((and (char= character #\\) (not (eql quote #\')))
                       (if (< (1+ index) end)
                           (progn (write-char (char command (1+ index)) value) (incf index 2))
                           (progn (write-char character value) (incf index))))
                      ((and (char= character #\') (null quote)) (setf quote character) (incf index))
                      ((and (char= character #\') (char= quote #\')) (setf quote nil) (incf index))
                      ((and (char= character #\") (null quote)) (setf quote character) (incf index))
                      ((and (char= character #\") (char= quote #\")) (setf quote nil) (incf index))
                      (t
                       (unless (eql quote #\')
                         (when (member character '(#\$ #\`)) (setf expand t))
                         (when (and (null quote) (find character "*?[")) (setf glob t)))
                       (write-char character value)
                       (incf index)))))))
      (flush end))
    (nreverse words)))

(defparameter +system-directories+
  '("etc" "usr" "bin" "sbin" "lib" "lib32" "lib64" "boot" "dev" "proc" "sys"
    "var" "root" "home" "opt" "srv" "mnt" "media" "run"))

(defun top-level-p (text)
  "Whether TEXT names a directory directly under / (or its contents)."
  (and (plusp (length text))
       (char= (char text 0) #\/)
       (not (find #\/ text :start 1))))

(defun system-directory-p (text)
  "Whether TEXT is one of the system's own directories, or inside one of its first level."
  (and (plusp (length text))
       (char= (char text 0) #\/)
       (let* ((rest (subseq text 1))
              (slash (position #\/ rest))
              (first (if slash (subseq rest 0 slash) rest)))
         (and (member first +system-directories+ :test #'string=)
              (or (null slash) (not (find #\/ rest :start (1+ slash))))))))

(defun dangerous-target-p (text)
  "Whether removing TEXT would take more than the model can see: /, ~, a system directory, the quarantine."
  (let ((trimmed (string-right-trim "/" text))
        (bare (string-right-trim "/*" text)))
    (or (member trimmed '("" "." ".." "~" "$HOME" "${HOME}" "*") :test #'string=)
        (uiop:string-suffix-p "/." text)
        (uiop:string-suffix-p "/.." text)
        (and (search ".safe-swap" text) t)
        (top-level-p bare)
        (system-directory-p bare))))

(defun temp-target-p (word)
  "Whether WORD names a temporary path, which is never worth quarantining."
  (let ((value (word-value word)))
    (and (some (lambda (prefix) (uiop:string-prefix-p prefix value))
               '("/tmp/" "/var/tmp/" "$TMPDIR/" "${TMPDIR}/"))
         (not (search ".." value)))))

;;; --- halves of the POSIX they share -------------------------------------------

(defun quarantine-root (directory)
  "DIRECTORY as the text a block writes between double quotes: ~ is ${HOME}."
  (if (uiop:string-prefix-p "~" directory)
      (cat "${HOME:-/tmp}" (subseq directory 1))
      directory))

(defun slot-open (rules slot)
  "The two variables every block opens with: the receipt and its slot directory."
  (cat "SS_RCPT=" (getf rules :rid) "; SS_Q=\"" (quarantine-root (getf rules :qdir)) "/" slot "\""))

(defun slot-notes (rules slot kind original)
  "The two files each slot carries: which command it guards, and of what kind."
  (declare (ignore rules))
  (cat "printf '%s\\n' " (sh-quote original) " > \"$SS_Q/original\"; "
       "printf '%s\\n' " (sh-quote kind) " > \"$SS_Q/kind\"; "
       "printf '%s\\n' " (sh-quote slot) " > \"$SS_Q/slot\"; "))

(defun unset-vars (names)
  "The line that clears NAMES, a string of variable names."
  (cat "unset " names "; "))

(defun git-ident ()
  "The identity a snapshot commit is made under, so it works on a machine with no git identity."
  "GIT_AUTHOR_NAME=safe-swap GIT_AUTHOR_EMAIL=safe-swap GIT_COMMITTER_NAME=safe-swap GIT_COMMITTER_EMAIL=safe-swap ")

;;; --- the rm block ------------------------------------------------------------

(defun rm-block (rules slot words &key force recursive original)
  "Move each of WORDS into the slot directory, keeping a manifest, then answer rm's status."
  (let* ((words (reverse words))
         (text (format nil "~{~a~^ ~}" (mapcar #'word-raw words)))
         (missing (if force ":"
                      (cat "printf \"rm: cannot remove '%s': No such file or directory\\n\" \"$SS_P\" >&2; SS_RC=1; ")))
         (directory-guard
           (if recursive ""
               (cat "if [ -d \"$SS_P\" ] && [ ! -L \"$SS_P\" ]; then "
                    "printf \"rm: cannot remove '%s': Is a directory\\n\" \"$SS_P\" >&2; SS_RC=1; continue; fi; ")))
         (count (cat "SS_ST=$(find " text " ! -type d 2>/dev/null | head -n " (format nil "~d" +walk-cap+)
                     " | wc -l) || SS_ST=0; "
                     "SS_BY=$(find " text " ! -type d -exec ls -lnd {} + 2>/dev/null | awk '/^-/{s+=$5} END{print s+0}') || SS_BY=0; ")))
    (cat "{ " (slot-open rules slot) "; SS_N=0; SS_RC=0; SS_ST=0; SS_BY=0; "
         "if mkdir -p -- \"$SS_Q\"; then "
         count
         "for SS_P in " text "; do "
         "if [ -e \"$SS_P\" ] || [ -L \"$SS_P\" ]; then " directory-guard
         "SS_T=${SS_P%\"${SS_P##*[!/]}\"}; [ -n \"$SS_T\" ] || SS_T=$SS_P; "
         "SS_D=$(unset CDPATH; cd -- \"$(dirname -- \"$SS_T\")\" 2>/dev/null && pwd -P) || SS_D=; "
         "SS_N=$((SS_N+1)); "
         "if [ -n \"$SS_D\" ] && mv -- \"$SS_T\" \"$SS_Q/$SS_N\"; then "
         "printf '%s\\t%s/%s\\n' \"$SS_N\" \"${SS_D%/}\" \"$(basename -- \"$SS_T\")\" >> \"$SS_Q/manifest.tsv\"; "
         "else printf \"rm: cannot remove '%s'\\n\" \"$SS_P\" >&2; SS_RC=1; fi; "
         "else " missing " fi; done; "
         "printf '%s %s\\n' \"${SS_ST:-0}\" \"${SS_BY:-0}\" > \"$SS_Q/stats\"; "
         (slot-notes rules slot "rm" original)
         "else printf '" +not-swapped+ "quarantine creation failed)\\n' >&2; SS_RC=1; fi; "
         "SS_END=$SS_RC; " (unset-vars "SS_Q SS_N SS_RC SS_ST SS_BY SS_P SS_T SS_D SS_END") " "
         "[ \"$SS_END\" = 0 ]; }")))

;;; --- the git blocks -----------------------------------------------------------

(defun snapshot-block (rules slot original &key gopts)
  "`git stash create` into a private ref, then the command, unchanged."
  (let ((git (cat "git " (or gopts ""))))
    (cat "{ " (slot-open rules slot) "; "
         "mkdir -p -- \"$SS_Q\" 2>/dev/null && { " git "rev-parse --show-toplevel > \"$SS_Q/gitdir\" && "
         git "rev-parse HEAD > \"$SS_Q/head\"; } 2>/dev/null; "
         "SS_SHA=$(" (git-ident) git "stash create 2>/dev/null) || SS_SHA=; "
         "if [ -n \"$SS_SHA\" ] && " git "update-ref \"refs/safe-swap/" slot "\" \"$SS_SHA\" 2>/dev/null; then "
         "printf '%s\\n' \"$SS_SHA\" > \"$SS_Q/stash.sha\"; "
         "elif " git "rev-parse --verify -q HEAD >/dev/null 2>&1 && ! " git "diff --quiet HEAD -- 2>/dev/null; then "
         "printf '" +not-swapped+ "snapshot failed, ran unprotected)\\n' >&2; fi; "
         (slot-notes rules slot "snapshot" original)
         (unset-vars "SS_Q SS_SHA SS_RCPT") original "; }")))

(defun branch-block (rules slot original &key gopts names)
  "Record the tip of every branch about to be deleted, then the command."
  (let ((git (cat "git " (or gopts ""))))
    (cat "{ " (slot-open rules slot) "; "
         "if mkdir -p -- \"$SS_Q\" 2>/dev/null && " git "rev-parse --show-toplevel > \"$SS_Q/gitdir\" 2>/dev/null; then "
         "for SS_B in " (format nil "~{~a~^ ~}" names) "; do "
         "SS_S=$(" git "rev-parse --verify -q \"refs/heads/$SS_B\") && "
         "printf '%s\\t%s\\n' \"$SS_B\" \"$SS_S\" >> \"$SS_Q/branches.tsv\"; done; "
         (slot-notes rules slot "branch" original)
         "else printf '" +not-swapped+ "could not record the branch tip, ran unprotected)\\n' >&2; fi; "
         (unset-vars "SS_Q SS_B SS_S SS_RCPT") original "; }")))

(defun clean-block (rules slot original &key gopts ignored pathspecs)
  "Tar the files `git clean` may delete, then the command, unchanged."
  (let ((git (cat "git " (or gopts ""))))
    (cat "{ " (slot-open rules slot) "; "
         "SS_TL=$(" git "rev-parse --show-toplevel 2>/dev/null) || SS_TL=; "
         "if [ -n \"$SS_TL\" ] && mkdir -p -- \"$SS_Q\" 2>/dev/null; then "
         "printf '%s\\n' \"$SS_TL\" > \"$SS_Q/gitdir\"; "
         "SS_LS='-o --exclude-standard'; " (if ignored "SS_LS=\"$SS_LS -i\"; " "")
         git "ls-files -z $SS_LS " (or pathspecs "") " 2>/dev/null | tar -c --null -T - -f \"$SS_Q/clean.tar\" 2>/dev/null || "
         "printf '" +not-swapped+ "could not snapshot the untracked files, ran unprotected)\\n' >&2; "
         (slot-notes rules slot "clean" original)
         "else printf '" +not-swapped+ "could not create the snapshot directory, ran unprotected)\\n' >&2; fi; "
         (unset-vars "SS_Q SS_TL SS_LS SS_RCPT") original "; }")))

(defun stash-block (rules slot original &key gopts clear ref)
  "Record the SHA of every stash entry about to be dropped, then the command."
  (let ((git (cat "git " (or gopts ""))))
    (cat "{ " (slot-open rules slot) "; "
         "SS_TL=$(" git "rev-parse --show-toplevel 2>/dev/null) || SS_TL=; "
         "if [ -n \"$SS_TL\" ] && mkdir -p -- \"$SS_Q\" 2>/dev/null; then "
         "printf '%s\\n' \"$SS_TL\" > \"$SS_Q/gitdir\"; "
         (if clear
             (cat "SS_R=; for SS_S in $(" git "stash list --format=%H 2>/dev/null); do SS_R=\"$SS_S $SS_R\"; done; SS_L=$SS_R; ")
             (cat "SS_L=$(" git "rev-parse --verify -q " (or ref "refs/stash") " 2>/dev/null) || SS_L=; "))
         "if [ -n \"$SS_L\" ]; then for SS_S in $SS_L; do printf '%s\\n' \"$SS_S\" >> \"$SS_Q/stash.shas\"; done; fi; "
         (slot-notes rules slot "stash" original)
         "else printf '" +not-swapped+ "could not record the stash entries, ran unprotected)\\n' >&2; fi; "
         (unset-vars "SS_Q SS_TL SS_L SS_R SS_S SS_RCPT") original "; }")))

;;; --- rules -------------------------------------------------------------------

(defstruct (edit (:constructor make-edit (start end text kind)))
  start end text kind)

(defstruct (skip (:constructor make-skip (reason)))
  reason)

(defstruct (analysis (:constructor make-analysis))
  command rewrite kinds skips rid)

(defun rule-on-p (rules key)
  "RULES' switch for KEY: absent means on, as the original's defaults did."
  (let ((value (getf rules key :absent)))
    (if (eq value :absent) t value)))

(defun rm-rule (words rules command start end)
  "WORDS as a move into quarantine, or a skip, or NIL to leave them alone."
  (let ((force nil) (recursive nil) (targets '()) (done nil))
    (dolist (word (rest words))
      (let ((value (word-value word)))
        (cond
          ((or done (not (word-option-p word)) (string= value "-")) (push word targets))
          ((word-expand word) (return-from rm-rule (make-skip "rm option uses a variable")))
          ((word-glob word) (return-from rm-rule (make-skip "rm option uses a glob")))
          ((string= value "--") (setf done t))
          ((uiop:string-prefix-p "--" value)
           (cond ((string= value "--recursive") (setf recursive t))
                 ((string= value "--force") (setf force t))
                 ((member value '("--verbose" "--preserve-root") :test #'string=))
                 (t (return-from rm-rule (make-skip (format nil "unsupported rm option ~a" value))))))
          (t (loop for character across (subseq value 1)
                   do (case character
                        ((#\r #\R) (setf recursive t))
                        (#\f (setf force t))
                        (#\v)
                        (t (return-from rm-rule
                             (make-skip (format nil "unsupported rm option -~c" character))))))))))
    (when (null targets) (return-from rm-rule nil))
    (dolist (word targets)
      (when (dangerous-target-p (word-value word))
        (return-from rm-rule
          (make-skip "protected path: /, home, a system directory or the quarantine"))))
    (when (every #'temp-target-p targets)
      (return-from rm-rule (make-skip "temp path, never quarantined")))
    (unless (or recursive (some #'word-glob targets) (rule-on-p rules :single-rm))
      (return-from rm-rule nil))
    (make-edit start end
               (rm-block rules (getf rules :slot) targets :force force :recursive recursive
                         :original (subseq command start end))
               "rm")))

(defun git-parts (words)
  "(values GOPTS SUBCOMMAND REST-WORDS), or NIL when the command is not one the rules read."
  (let ((index 1) (gopts '()))
    (loop while (and (< index (length words)) (word-option-p (nth index words)))
          do (let ((value (word-value (nth index words))))
               (cond
                 ((member value '("-C" "-c") :test #'string=)
                  (when (< (1+ index) (length words))
                    (push (word-raw (nth index words)) gopts)
                    (push (word-raw (nth (1+ index) words)) gopts)
                    (incf index 2)))
                 ((or (member value '("--no-pager" "-P" "--no-optional-locks" "--no-replace-objects"
                                      "--literal-pathspecs") :test #'string=)
                      (uiop:string-prefix-p "--git-dir=" value)
                      (uiop:string-prefix-p "--work-tree=" value))
                  (push (word-raw (nth index words)) gopts)
                  (incf index))
                 (t (return-from git-parts nil)))))
    (when (>= index (length words)) (return-from git-parts nil))
    (values (format nil "~{~a~^ ~}" (nreverse gopts))
            (word-value (nth index words))
            (nthcdr (1+ index) words))))

(defun push-rule (words rules)
  "The edits that turn --force into a lease and drop a + from a refspec, or NIL."
  (let ((edits '()))
    (dolist (word (rest words))
      (let ((value (word-value word)))
        (cond
          ((member value '("--force" "-f") :test #'string=)
           (push (make-edit (word-start word) (word-end word) +lease+ "push") edits))
          ((and (plusp (length value)) (char= (char value 0) #\+) (not (word-expand word)))
           (push (make-edit (word-start word) (1+ (word-start word)) "" "push") edits)))))
    (when edits
      (dolist (word (rest words))
        (when (some (lambda (pattern) (wildcard-match pattern (word-value word)))
                    (getf rules :trusted))
          (return-from push-rule nil))))
    edits))

(defun wildcard-match (pattern text)
  "Whether TEXT matches PATTERN, where * matches any run of characters."
  (cond ((zerop (length pattern)) (zerop (length text)))
        ((char= (char pattern 0) #\*)
         (or (wildcard-match (subseq pattern 1) text)
             (and (plusp (length text)) (wildcard-match pattern (subseq text 1)))))
        ((zerop (length text)) nil)
        ((char= (char pattern 0) (char text 0))
         (wildcard-match (subseq pattern 1) (subseq text 1)))
        (t nil)))

(defun any-word-p (value words)
  "Whether some word of WORDS has VALUE."
  (some (lambda (word) (string= value (word-value word))) words))

(defun flag-letter-p (letter words)
  "Whether an option word of WORDS carries LETTER."
  (some (lambda (word)
          (and (word-option-p word)
               (not (uiop:string-prefix-p "--" (word-value word)))
               (find letter (word-value word) :test #'char=)))
        words))

(defun git-rule (words rules command start end)
  "The git halves of the original's rules, as one segment's edit or skip."
  (multiple-value-bind (gopts sub rest) (git-parts words)
    (unless sub (return-from git-rule nil))
    (let ((original (subseq command start end))
          (slot (getf rules :slot)))
      (cond
        ((and (string= sub "push") (rule-on-p rules :push))
         (push-rule words rules))
        ((and (string= sub "reset") (rule-on-p rules :reset) (any-word-p "--hard" rest))
         (make-edit start end (snapshot-block rules slot original :gopts gopts) "reset"))
        ((and (string= sub "checkout") (rule-on-p rules :restore)
              (or (any-word-p "--" rest) (equal (mapcar #'word-value rest) '("."))))
         (make-edit start end (snapshot-block rules slot original :gopts gopts) "checkout"))
        ((and (string= sub "restore") (rule-on-p rules :restore)
              (not (some (lambda (value) (member value '("--staged" "-S") :test #'string=))
                         (mapcar #'word-value rest))))
         (make-edit start end (snapshot-block rules slot original :gopts gopts) "restore"))
        ((and (string= sub "branch") (rule-on-p rules :branch) (flag-letter-p #\D rest))
         (let ((names (loop for word in rest
                            when (not (word-option-p word))
                              collect (word-raw word))))
           (if names
               (make-edit start end (branch-block rules slot original :gopts gopts :names names) "branch")
               (make-skip "git branch -D with no literal branch name"))))
        ((and (string= sub "clean") (rule-on-p rules :clean) (flag-letter-p #\f rest))
         (let ((ignored (flag-letter-p #\x rest))
               (pathspecs (loop for word in rest
                                when (not (word-option-p word))
                                  collect (word-raw word))))
           (make-edit start end
                      (clean-block rules slot original :gopts gopts :ignored ignored
                                   :pathspecs (and pathspecs (format nil "-- ~{~a~^ ~}" pathspecs)))
                      "clean")))
        ((and (string= sub "stash") (rule-on-p rules :stash) rest
              (member (word-value (first rest)) '("drop" "clear") :test #'string=))
         (make-edit start end
                    (stash-block rules slot original :gopts gopts
                                  :clear (string= (word-value (first rest)) "clear")
                                  :ref (and (rest rest) (word-raw (second rest))))
                    "stash"))
        (t nil)))))

(defparameter +wrappers+
  '("sudo" "env" "xargs" "eval" "sh" "bash" "zsh" "dash" "nohup" "time" "find" "parallel")
  "Programs whose own words decide what runs: a rule reads none of them.")

(defun segment-opaque (words)
  "The name of the wrapper that hides what this command runs, or NIL."
  (let ((name (and words (command-name (first words)))))
    (and name (member name +wrappers+ :test #'string=) name)))

(defun skip-for-segment (words)
  "A SKIP for a command that looks destructive but is not read, or NIL."
  (let ((wrapper (segment-opaque words)))
    (when (and wrapper
               (some (lambda (word) (member (word-value word) '("rm" "git") :test #'string=)) words))
      (make-skip (format nil "~a wrapper" wrapper)))))

(defun segment-rules (words rules command start end)
  "The edit or skip one simple command earns, or NIL."
  (let ((name (command-name (first words))))
    (cond
      ((string= name "rm") (if (rule-on-p rules :rm) (rm-rule words rules command start end) nil))
      ((string= name "git") (git-rule words rules command start end))
      (t (skip-for-segment words)))))

(defun default-rules (&key (qdir "~/.safe-swap/trash") rid (slot nil) (trusted nil))
  "The switches the hook passes, with the original's defaults."
  (list :rm t :single-rm t :push t :reset t :restore t :branch t :clean t :stash t
        :qdir qdir :rid (or rid (new-id)) :slot slot :trusted trusted))

(defun analyse (command &optional (rules nil))
  "What to do with COMMAND: its reversible twin, or why it was left as written."
  (let ((rules (or rules (default-rules)))
        (analysis (make-analysis :command command :rid (and rules (getf rules :rid)))))
    (unless (and (stringp command) (plusp (length command)))
      (return-from analyse analysis))
    (setf (analysis-rid analysis) (getf rules :rid))
    (when (or (search +marker+ command) (search "SS_B=" command))
      (return-from analyse analysis))
    (unless (or (search "rm" command) (search "git" command))
      (return-from analyse analysis))
    (let ((segments '()) (edits '()) (skips '()))
      (handler-case (setf segments (split-commands command))
        (bail (condition)
          (setf (analysis-skips analysis) (list (format nil "unsupported syntax: ~a" (bail-reason condition))))
          (return-from analyse analysis)))
      (loop for (start . end) in segments
            for words = (read-words command start end)
            for index from 0
            do (when words
                 (let* ((rules (let ((copy (copy-list rules)))
                                 (setf (getf copy :slot) (if (zerop index)
                                                             (getf rules :rid)
                                                             (format nil "~a.~d" (getf rules :rid) index)))
                                 copy))
                        (result (segment-rules words rules command start end)))
                   (cond ((typep result 'edit) (push result edits))
                         ((typep result 'skip) (push (skip-reason result) skips))
                         ((listp result) (dolist (one result) (push one edits)))
                         (t nil)))))
      (setf (analysis-skips analysis) (remove-duplicates (nreverse skips) :test #'string=))
      (when edits
        (let ((text command))
          (dolist (edit (sort edits #'> :key #'edit-start))
            (setf text (cat (subseq text 0 (edit-start edit))
                            (edit-text edit)
                            (subseq text (edit-end edit)))))
          (setf (analysis-rewrite analysis) text
                (analysis-kinds analysis) (remove-duplicates (mapcar #'edit-kind edits) :test #'string=))))
      analysis)))

(defun rewrite-command (command &optional (rules nil))
  "COMMAND's reversible twin, or NIL to run it as written."
  (analysis-rewrite (analyse command rules)))
