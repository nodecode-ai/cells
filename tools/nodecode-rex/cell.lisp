;;;; cell.lisp --- the rex CLI in a Nodecode session: panes, labels, capture.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of kshitijk4poor's hermes-rex-plugin (see NOTICE). Rex is a
;;;; terminal multiplexer with a `rex' CLI on the machine; this cell is that
;;;; CLI in Nodecode's hands:
;;;;
;;;;   rex_run      run a command in a new pane beside this one, never focused,
;;;;                so a dev server, a watcher or a log tail stays visible
;;;;   rex_capture  read a pane's newest output back
;;;;   rex_panes    where this session is: Rex's sessions, and the windows and
;;;;                panes of one, with its own marked
;;;;   rex_label    name this session's window after the task, with one status
;;;;                glyph: ● busy, ? waiting, ✓ done, ■ stopped, ✗ failed
;;;;   /rex         the operator's own way to all four, with a panel
;;;;
;;;; WHERE AM I. REX_SESSION and REX_BLOCK are read from the environment the
;;;; Nodecode process was started in -- run Nodecode inside a Rex pane and they
;;;; are there -- and rex.session / rex.block override them, for a serve or a
;;;; shell started somewhere else. Every call passes them to the CLI, so the
;;;; pane resolved is the one asked for, and --autostart=false, so a call never
;;;; starts a Rex server.
;;;;
;;;; FAIL OPEN, OR SAY WHY. A slow or missing rex is one refusal naming what
;;;; failed, never a hang and never an error out of the tool's shape; the only
;;;; thing that stops the cell at start is a binary the section names that is
;;;; not there.

(in-package #:nodecode-rex)

;;; --- the words ---------------------------------------------------------------------
;;; Data, so a layer can reword them.

(defparameter *marks*
  '(("busy" . "●") ("waiting" . "?") ("done" . "✓") ("stopped" . "■") ("failed" . "✗"))
  "The one glyph a window label may start with, by state: the plugin's own set.")

(defparameter *split-ratio* 35
  "How much of the window a new pane splits off, in percent.")

(defparameter +directions+ '("below" "right")
  "Where a new pane may go: the setting's choices and the tool's.")

(defparameter *primer*
  "Rex is the terminal multiplexer this session may be running inside, and the
nodecode-rex cell drives its CLI: Lisp functions in the rex: package called
through eval, or the four tools below. Each answers a string; a refusal is
ERROR: REX-ERROR with the reason.
  (rex:panes)                      Rex's sessions, and this session's windows
      and panes, with its own window and pane marked. Nothing is changed.
  (rex:run \"npm run dev\" :label \"dev-server\")   run a command in a new pane
      beside this one, split below or right, never focused; :keep-open t (the
      default) leaves it open when the command exits. PUT LONG-LIVED WORK HERE
      -- dev servers, watchers, log tails, long builds -- not in a hidden
      background process, and read it with (rex:capture ...) rather than
      polling.
  (rex:capture :lines 80)          the newest output of this session's pane,
      or :pane a pane id from rex:panes.
  (rex:label \"fix-login\" :mark \"done\")  name this session's window after
      the task in two to four lowercase words, optionally with one status
      glyph: busy, waiting, done, stopped, failed; :mark \"clear\" removes it.
      The label is what you or the operator set, plus the glyph; keep the
      glyph out of a label you write.
The operator types /rex for the same, with a panel. Without REX_SESSION (and
rex.session) there is no Rex pane: every verb refuses and says so."
  "What (help :rex) answers while the cell runs.")

;;; --- running the CLI ----------------------------------------------------------------

(defvar *serial* 0
  "A counter that keeps one call's temporary files apart from the next's.")

(defun temporary (kind)
  "A pathname in the system's temporary directory that nothing else is using."
  (merge-pathnames (format nil "nodecode-rex-~d-~d.~a"
                            (get-universal-time) (incf *serial*) kind)
                   (uiop:temporary-directory)))

(defun file-text (path)
  "PATH's contents as text, or \"\" when it holds none."
  (if (probe-file path)
      (handler-case (uiop:read-file-string path :external-format :utf-8)
        (error () ""))
      ""))

(defun child-environment (session block)
  "The environment a rex call gets: this process's own, with the session and
pane asked for in front, so a call acts on the pane it was asked about."
  (let ((base (coerce (sb-ext:posix-environ) 'list)))
    (if (and session block)
        (cons (format nil "REX_SESSION=~a" session)
              (cons (format nil "REX_BLOCK=~a" block)
                    (remove-if (lambda (entry)
                                 (or (eql 0 (search "REX_SESSION=" entry))
                                     (eql 0 (search "REX_BLOCK=" entry))))
                               base)))
        base)))

(defun program-run (program args timeout session block)
  "Run PROGRAM with ARGS, standard input at /dev/null, for at most TIMEOUT
seconds. => (values EXIT-CODE STDOUT STDERR TIMED-OUT-P). The streams are
read back whole from files, and a call past its deadline is killed, so a stuck
Rex server cannot hold a turn."
  (let ((out (temporary "out")) (err (temporary "err")))
    (unwind-protect
         (let* ((process (sb-ext:run-program program args
                                             :search nil
                                             :input nil
                                             :output out
                                             :error err
                                             :if-output-exists :supersede
                                             :environment (child-environment session block)
                                             :wait nil))
                (deadline (+ (get-internal-real-time)
                             (* timeout internal-time-units-per-second)))
                (timed-out nil))
           (loop
             (unless (sb-ext:process-alive-p process) (return))
             (when (>= (get-internal-real-time) deadline)
               (setf timed-out t)
               (ignore-errors (sb-ext:process-kill process 15))
               (sleep 0.1)
               (when (sb-ext:process-alive-p process)
                 (ignore-errors (sb-ext:process-kill process 9)))
               (return))
             (sleep 0.02))
           (sb-ext:process-wait process)
           (values (ignore-errors (sb-ext:process-exit-code process))
                   (file-text out) (file-text err) timed-out))
      (ignore-errors (delete-file out))
      (ignore-errors (delete-file err)))))

(defvar *run* #'program-run
  "The one seam that runs the CLI: (PROGRAM ARGS TIMEOUT SESSION BLOCK) =>
(values EXIT-CODE STDOUT STDERR TIMED-OUT-P). Tests replace it; nothing else
does.")

(defun program-file (program)
  "PROGRAM as a file that exists: itself when it names a path, else its first
match on PATH. NIL when neither holds it."
  (if (find #\/ program)
      (and (probe-file program) program)
      (loop for directory in (uiop:split-string (or (uiop:getenv "PATH") "") :separator ":")
            for candidate = (and (plusp (length directory))
                                 (merge-pathnames program
                                                  (uiop:ensure-directory-pathname directory)))
            when (and candidate (probe-file candidate)) return (namestring candidate))))

(defun rex-call (args)
  "Run `rex --autostart=false ARGS' for this session's pane.
=> (values EXIT-CODE STDOUT STDERR TIMED-OUT-P)."
  (funcall *run* (setting :binary) (cons "--autostart=false" args)
           (setting :timeout-seconds) (env-session) (env-block)))

(defun rex (args)
  "ARGS run through rex; its stdout, or a refusal that names what rex said."
  (multiple-value-bind (exit out err timed-out) (rex-call args)
    (cond (timed-out
           (fail "rex did not answer within ~ds: `rex ~{~a~^ ~}' -- is the Rex server up?"
                 (setting :timeout-seconds) args))
          ((not (eql 0 exit))
           (fail "rex ~{~a~^ ~} exited ~a~@[: ~a~]"
                 args exit (let ((said (nlk:one-line (nlk:trimmed (or err out "")))))
                             (and (plusp (length said)) (nlk:clip said 200)))))
          (t out))))

(defun rex-json (args)
  "ARGS run through rex, its stdout as a decoded JSON object; NIL when rex
failed or printed something else, so a reader of what may not be there need
not guard."
  (multiple-value-bind (exit out) (rex-call args)
    (when (and (eql 0 exit) (plusp (length (nlk:trimmed out))))
      (let ((data (ignore-errors (nlk:decode-json out))))
        (and (hash-table-p data) data)))))

;;; --- where this session is ------------------------------------------------------------

(defun text-or-nil (value)
  "VALUE as a non-empty trimmed string, or NIL."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

(defun env-session ()
  "The Rex session this process acts on, or NIL: the setting, else REX_SESSION."
  (or (text-or-nil (setting :session)) (text-or-nil (uiop:getenv "REX_SESSION"))))

(defun env-block ()
  "The Rex pane this process acts on, or NIL: the setting, else REX_BLOCK."
  (or (text-or-nil (setting :block)) (text-or-nil (uiop:getenv "REX_BLOCK"))))

(defun this-session ()
  "The Rex session, or a refusal that says how to name one."
  (or (env-session)
      (fail "no Rex session: REX_SESSION is unset and rex.session is empty -- run Nodecode inside a Rex pane, or name the session in /settings (~/.nodecode/config.jsonc, the rex section)")))

(defun this-pane ()
  "This session's Rex pane, or a refusal that says how to name one."
  (or (env-block)
      (fail "no Rex pane: REX_BLOCK is unset and rex.block is empty -- run Nodecode inside a Rex pane, or name the pane in /settings (~/.nodecode/config.jsonc, the rex section)")))

(defun window ()
  "The Rex window holding this session's pane, as rex describes it, or NIL."
  (rex-json (list "window" "inspect" "--json")))

(defun window-label ()
  "This session's window label as Rex has it now, \"\" when rex cannot say."
  (let ((win (window)))
    (or (and win (nlk:json-value win :string "label")) "")))

;;; --- labels and marks ----------------------------------------------------------------

(defun mark-char-p (char)
  "Whether CHAR is one of the five status glyphs."
  (and char (find char "●?✓■✗")))

(defun strip-mark (text)
  "TEXT without a leading status glyph and its space."
  (let ((text (or text "")))
    (cond ((and (>= (length text) 2) (mark-char-p (char text 0)) (char= (char text 1) #\Space))
           (subseq text 2))
          ((and (= (length text) 1) (mark-char-p (char text 0))) "")
          (t text))))

(defun mark-of (text)
  "The status glyph TEXT starts with, or NIL."
  (let ((text (or text "")))
    (cond ((and (>= (length text) 2) (mark-char-p (char text 0)) (char= (char text 1) #\Space))
           (string (char text 0)))
          ((and (= (length text) 1) (mark-char-p (char text 0))) text)
          (t nil))))

(defun glyph (state)
  "The glyph STATE names, or NIL for a state this cell does not know."
  (cdr (assoc state *marks* :test #'equal)))

(defun marked (base glyph)
  "BASE with GLYPH and a space in front, when there is a glyph."
  (cond ((null glyph) base)
        ((zerop (length base)) glyph)
        (t (format nil "~a ~a" glyph base))))

(defun words (text)
  "TEXT's words: its runs of ASCII letters and digits, lowercased."
  (let ((words '()) (current (make-string-output-stream)))
    (labels ((finish ()
               (let ((word (get-output-stream-string current)))
                 (when (plusp (length word)) (push word words)))))
      (loop for char across (string-downcase (or text ""))
            do (if (and (< (char-code char) 128) (alphanumericp char))
                   (write-char char current)
                   (finish)))
      (finish))
    (nreverse words)))

(defun slug (text &key (max-words nil))
  "TEXT as a label: its first MAX-WORDS words (rex.label_words by default),
lowercased and joined by hyphens, as the plugin named labels."
  (let* ((words (words text))
         (max (or max-words (setting :label-words))))
    (unless words
      (fail "~s has no word in it to name a pane with" text))
    (format nil "~{~a~^-~}" (if (> (length words) max) (subseq words 0 max) words))))

(defun rename-window (text)
  "Give this session's window the label TEXT, and answer it."
  (let ((session (this-session)))
    (let* ((win (window))
           (id (and win (nlk:json-value win :text "window_id"))))
      (unless id
        (fail "rex knows no window holding pane ~a of session ~a" (env-block) session))
      (rex (list "window" "rename" "-s" session id text))
      text)))

(defun set-base (text)
  "Replace the window's label with TEXT as a slug, keeping the status glyph."
  (rename-window (marked (slug text) (mark-of (window-label)))))

(defun set-status (state)
  "Give the window's label one status glyph, STATE, or none for clear/empty."
  (let* ((state (string-downcase (nlk:trimmed (or state ""))))
         (mark (cond ((member state '("" "clear" "none") :test #'equal) nil)
                     ((glyph state))
                     (t (fail "~s is no status mark: busy, waiting, done, stopped, failed, or clear"
                              state)))))
    (rename-window (marked (strip-mark (window-label)) mark))))

(defun direction (split)
  "SPLIT as a direction: empty uses rex.split, else one of below or right."
  (let ((split (string-downcase (nlk:trimmed (or split "")))))
    (cond ((zerop (length split)) (setting :split))
          ((member split +directions+ :test #'equal) split)
          (t (fail "~s is no split direction: ~{~a~^, ~}" split +directions+)))))

;;; --- the four verbs -----------------------------------------------------------------

(define-verb run (command &key label split (keep-open t))
  "Run COMMAND in a new Rex pane beside this session's; answer what happened."
  (let ((command (nlk:trimmed (or command ""))))
    (unless (plusp (length command))
      (fail "no command to run: give one, e.g. \"npm run dev\""))
    (multiple-value-bind (sess pane-id) (values (this-session) (this-pane))
      (let* ((label (slug (let ((given (text-or-nil label)))
                            (or given command))))
             (split (direction split))
             (args (append (list "run" "-s" sess "-b" pane-id
                                 (format nil "--split=~a" split)
                                 "--ratio" (princ-to-string *split-ratio*)
                                 "--focus=false"
                                 "--label" label
                                 "--json")
                           (when keep-open (list "--keep-open"))
                           (list "--" command)))
             (out (rex args))
             (data (ignore-errors (nlk:decode-json out)))
             (opened (and (hash-table-p data)
                          (or (nlk:json-value data :text "block_id")
                              (nlk:json-value data :text "id")))))
        (format nil "rex: ~s is running in a new pane ~a of session ~a, labeled ~s, not focused; read it with rex_capture~@[, on pane ~a~]"
                command split sess label opened)))))

(define-verb capture (&key pane (lines 80))
  "Answer the newest LINES lines of PANE, this session's own when NIL."
  (let* ((pane (or (text-or-nil pane) (this-pane)))
         (lines (if (and (integerp lines) (plusp lines)) lines 80))
         (text (nlk:trimmed (rex (list "capture" "-b" pane "--trim")))))
    (if (zerop (length text))
        (format nil "rex: pane ~a shows nothing yet" pane)
        (let* ((all (nlk:lines text))
               (kept (if (> (length all) lines)
                         (subseq all (- (length all) lines))
                         all)))
          (format nil "~{~a~%~}~@[rex: ~d earlier line~:p not shown; ask for more lines~]"
                  kept (let ((omitted (- (length all) (length kept))))
                         (and (plusp omitted) omitted)))))))

(define-verb panes (&key session)
  "Answer Rex's sessions, then the windows and panes of one of them."
  (let* ((here (env-session))
         (want (or (text-or-nil session) here (this-session)))
         (listing (rex-json (list "ls" "--json")))
         (windows (rex-json (list "window" "list" "-s" want "--json")))
         (active (nlk:json-value windows :text "active_window_id"))
         (mine (env-block))
         (rows (nlk:json-array windows "windows"))
         (lines (list (format nil "rex: session ~a" want))))
    (loop for row across (nlk:json-array listing "sessions")
          for id = (nlk:json-value row :text "session_id")
          for label = (nlk:json-value row :string "label")
          do (push (format nil "  ~a~@[ ~s~]~:[~;  <- this one~]" id label (equal id want))
                   lines))
    (cond ((zerop (length rows))
           (push (format nil "  no window of session ~a answers" want) lines))
          (t
           (push (format nil "windows of ~a:" want) lines)
           (loop for row across rows
                 for id = (nlk:json-value row :text "window_id")
                 for label = (nlk:json-value row :string "label")
                 for pane = (nlk:json-value row :text "focused_block_id")
                 do (push (format nil "  ~a~@[ ~s~]~:[~; (active)~]~@[ pane ~a~]~:[~;  <- this pane~]"
                                  id label (equal id active) pane
                                  (and mine pane (equal mine pane)))
                          lines))))
    (format nil "~{~a~%~}" (nreverse lines))))

(define-verb label (text &key mark)
  "Name this session's Rex window TEXT, keeping its mark, and set MARK if given."
  (target-p)
  (unless (plusp (length (nlk:trimmed (or text ""))))
    (fail "a window label needs a word or two, e.g. \"fix-login\""))
  (set-base text)
  (when (text-or-nil mark) (set-status mark))
  (format nil "rex: this window is labeled ~s" (window-label)))

(defun target-p ()
  "Both a session and a pane, or the refusals that say which is missing."
  (this-session)
  (this-pane)
  t)

;;; --- the tools ----------------------------------------------------------------------

(nlk:define-contract rex-run
  (:doc "Run a command in a new Rex pane beside this session's, so a dev
server, a watcher, a log tail or a long build is visible instead of hidden in
the background. The pane never takes focus; read it back with rex_capture.")
  ("command" :text :required t :doc "the command line to run in the pane")
  ("label" :string :default "" :doc "the pane's label, two to four lowercase words; empty names it from the command")
  ("split" :string :default "" :doc "where the pane goes: below or right; empty uses the cell's split setting")
  ("keep_open" :boolean :default t :doc "leave the pane open after the command exits"))

(nlk:define-contract rex-capture
  (:doc "Read the newest output of a Rex pane: what a command printed, a
server's log, a test run's tail. One pane per call; ask for more lines rather
than the whole scrollback.")
  ("pane" :string :default "" :doc "a pane (block) id from rex_panes; empty reads this session's own pane")
  ("lines" :integer :default 80 :doc "how many of the newest lines to answer, at least 1"))

(nlk:define-contract rex-panes
  (:doc "Where this session is in Rex: the sessions Rex has, and the windows
and panes of one of them, with the window holding this session's pane and the
pane itself marked. Changes nothing.")
  ("session" :string :default "" :doc "a Rex session id from the listing to open; empty lists this session"))

(nlk:define-contract rex-label
  (:doc "Name this session's Rex window after the task, in two to four
lowercase words, and optionally set its status mark: busy, waiting, done,
stopped, failed, or clear. The label is the task's; the mark is the cell's.")
  ("label" :text :required t :doc "the window's label, two to four lowercase words, with no status mark in it")
  ("mark" :string :default "" :doc "a status mark to set with the label: busy, waiting, done, stopped, failed, or clear; empty keeps the one there"))

(defun run-tool (call)
  "The rex_run tool: the pane, and what became of the command."
  (run (rex-run-command call)
       :label (rex-run-label call)
       :split (rex-run-split call)
       :keep-open (rex-run-keep-open call)))

(defun capture-tool (call)
  "The rex_capture tool: the pane's newest lines."
  (capture :pane (rex-capture-pane call) :lines (rex-capture-lines call)))

(defun panes-tool (call)
  "The rex_panes tool: the sessions, and one session's windows and panes."
  (panes :session (rex-panes-session call)))

(defun label-tool (call)
  "The rex_label tool: the window's new label, and its mark when one was named."
  (label (rex-label-label call) :mark (rex-label-mark call)))

;;; --- /rex ---------------------------------------------------------------------------

(defun status-text ()
  "Where this shell is in Rex, in one line, or why there is nowhere."
  (let ((session (env-session)) (pane (env-block)))
    (cond ((null session)
           "rex: not in a Rex pane (REX_SESSION is unset): run Nodecode inside Rex, or set rex.session and rex.block in /settings")
          (t (format nil "rex: session ~a~@[, pane ~a~]~@[, window ~s~]"
                     session pane
                     (let ((label (ignore-errors (window-label))))
                       (and label (plusp (length label)) label)))))))

(defun panel ()
  "The dialog bare /rex answers: where it is, and a row for each verb."
  (nle:list-dialog
   :title "Rex" :summary (status-text)
   :rows (list (nle:ask-row "Name this window" "Label" "/rex rename"
                             :placeholder "two to four lowercase words" :verb "rename"
                             :detail "the window holding this pane, after the task")
                (nle:ask-row "Run a command in a pane" "Command" "/rex run"
                             :placeholder "npm run dev" :verb "run"
                             :detail "it stays visible beside this session")
                (nle:run-row "Read this pane" "/rex capture" :detail "its newest output")
                (nle:run-row "List panes" "/rex panes" :detail "sessions, windows and panes")
                (nle:run-row "Mark done" "/rex mark done" :detail "the window label gets a check")
                (nle:run-row "Clear the mark" "/rex mark clear" :detail "the label alone"))))

(defun run-slash (args session-id)
  "/rex typed alone: where this shell is and its panel. With words: panes,
capture [LINES], mark STATE, run COMMAND, rename LABEL, or a label alone --
what /title does for the window. => the answer's text, and the panel."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (space (position-if (lambda (char) (member char '(#\Space #\Tab))) text))
         (head (string-downcase (subseq text 0 space)))
         (rest (if space (nlk:trimmed (subseq text space)) "")))
    (cond ((zerop (length text)) (values (status-text) (panel)))
          ((string= head "panes") (panes :session rest))
          ((string= head "capture") (capture :lines (or (ignore-errors (parse-integer rest)) 80)))
          ((string= head "mark") (set-status rest))
          ((string= head "run") (run rest))
          ((string= head "rename") (label rest))
          (t (label text)))))

;;; --- the declaration ----------------------------------------------------------------

(nle:define-cell rex
  (:section ("rex")
    (:guide "the rex CLI on PATH is all it needs; session and block default to REX_SESSION and REX_BLOCK, which are set when Nodecode runs inside a Rex pane; split says where a new pane goes")
    ("binary" :string :default "rex"
              :doc "the rex program: its name on PATH, or a path to it")
    ("session" :string :default ""
               :doc "the Rex session id to act on; empty reads REX_SESSION")
    ("block" :string :default ""
             :doc "the Rex pane (block) id to act on; empty reads REX_BLOCK")
    ("split" :choice :options '("below" "right") :default "below"
             :doc "where a new pane goes: below this one, or to its right")
    ("label_words" :integer :default 4 :min 1
                   :doc "how many words a generated window label keeps")
    ("timeout_seconds" :integer :default 3 :min 1
                       :doc "how long one rex call may take before it is given up"))
  (:settings (lambda (values table)
               (declare (ignore table))
               (let ((binary (getf values :binary)))
                 (unless (program-file binary)
                   (nlk:config-error
                    "rex.binary is ~s: no such program -- name the rex CLI on PATH, or give its path here"
                    binary))
                 values)))
  (:tool "rex_run" (nlk:contract 'rex-run) 'run-tool)
  (:tool "rex_capture" (nlk:contract 'rex-capture) 'capture-tool)
  (:tool "rex_panes" (nlk:contract 'rex-panes) 'panes-tool)
  (:tool "rex_label" (nlk:contract 'rex-label) 'label-tool)
  (:command "rex" 'run-slash
            :catches 'rex-error
            :description "Rex panes: run a command in one, read it, name this window"
            :argument-hint "[run COMMAND | capture [LINES] | panes [SESSION] | mark STATE | LABEL]")
  (:help :rex
         "rex: the Rex terminal multiplexer from a session - panes for long commands (rex:run, rex_capture), where you are (rex:panes), the window's label and mark (rex:label), and /rex"
         *primer*))
