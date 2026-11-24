;;;; cell.lisp --- NeuralMind's code memory: the project's index in every turn.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of dfrostar/neuralmind's Hermes plugin (neuralmind/hermes_plugin)
;;;; as a Nodecode cell. The plugin is a stdlib shim: it runs
;;;; `neuralmind _hook <action>` with the JSON payload Claude Code's hooks send,
;;;; and puts what comes back -- hookSpecificOutput.additionalContext -- into
;;;; the turn. This cell asks the same program the same way:
;;;;
;;;;   :ROUND  one model request about to be built. A session's first turn
;;;;           carries the previous session's recap (`session-start`), and
;;;;           every turn carries what its own prompt recalls
;;;;           (`prompt-submit`), recorded with NLK:RECORD-TURN-CONTEXT-PROMPT
;;;;           so the block rides that very request.
;;;;   :TOOL   after an `eval` that wrote files with (write-file ...) or
;;;;           (edit ...), each written file is recorded (`edit-activity`),
;;;;           off the turn's path, for the next session's recap and for the
;;;;           synapse layer.
;;;;
;;;; The program is the user's own: pip install neuralmind, and
;;;; `neuralmind build` in the project. Where there is no built project the
;;;; cell does nothing, exactly as the plugin did, and every call fails open:
;;;; a program that is missing, too slow or unhappy costs the turn its context,
;;;; never the turn.

(in-package #:nodecode-neuralmind)

;;; --- the words --------------------------------------------------------------------

(defparameter *primer*
  "NeuralMind keeps a per-project index of the codebase -- files, the symbols
in them, how they are called from one another, and the decisions recorded in
commit trailers -- and answers what a prompt is about from it. This cell adds
that answer to a turn on its own: a session's first turn starts with a recap
of the previous session in the project, and every turn carries the files and
symbols its own prompt matches. Nothing has to be called for it.

(neuralmind:recall \"where is the discount code applied\")
    the files, symbols and decisions the project's index matches, as
    NeuralMind frames them. (neuralmind:recap)   the previous session's recap.
/neuralmind prints the project, the program and the switches; /neuralmind
recall QUERY and /neuralmind recap ask by hand.

The program must be installed (pip install neuralmind) and the project built
once (`neuralmind build` in it); the `neuralmind' section names the
executable, and optionally pins one project. A refusal is
ERROR: NEURALMIND-ERROR and says which one."
  "What (help :neuralmind) answers while the cell runs.")

(defparameter *no-project*
  "neuralmind: no built project here -- run \"neuralmind build\" in this project (or set neuralmind.project), then ask again."
  "What a verb answers where nothing has been built, as the plugin did nothing.")

(defparameter *nothing*
  "neuralmind: nothing in this project's index matches that."
  "What a verb answers when NeuralMind matched nothing.")

(defparameter +built-marker+ ".neuralmind/build_status.json"
  "The file `neuralmind build` leaves in a project: the plugin's own test for
one, and this cell's.")

(defparameter +write-heads+ '("(write-file \"" "(write-file #p\"" "(write-file #P\"" "(edit \"")
  "The forms an eval writes a file with, in the order they are looked for: the
path is the string right after one of these.")

;;; --- the program --------------------------------------------------------------------

(defun program-path (name)
  "Where the program NAME is: as a path when it names one, else the first file
of that name on PATH; NIL when there is none."
  (when (and (stringp name) (plusp (length name)))
    (if (or (find #\/ name) (find #\\ name))
        (ignore-errors (probe-file name))
        (loop for directory in (uiop:split-string (or (uiop:getenv "PATH") "")
                                                   :separator '(#\: #\;))
              for found = (and (plusp (length directory))
                               (ignore-errors
                                 (probe-file (merge-pathnames
                                              name
                                              (uiop:ensure-directory-pathname directory)))))
              when found return found))))

(defun run-command (action input directory)
  "ACTION asked of the neuralmind program, INPUT on its stdin, in DIRECTORY
=> its stdout, or \"\". Bounded by the section's timeout, as the plugin
bounded its child at NEURALMIND_HERMES_TIMEOUT."
  (handler-case
      (multiple-value-bind (output status)
          (nlk:run-bounded (list (setting :command) "_hook" action)
                           :input input
                           :directory (uiop:ensure-directory-pathname directory)
                           :seconds (setting :timeout)
                           :output :string
                           :error-output :string)
        (if (eql status 0) (or output "") ""))
    (error () "")))

(defvar *run* #'run-command
  "The one seam that runs the program: (ACTION INPUT DIRECTORY) => its stdout,
or \"\". Tests replace it; nothing else does.")

(defun context-of (reply)
  "REPLY, the JSON one hook action printed, as the block it adds to the
request; \"\" when it adds none -- the plugin read the same key."
  (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return) (or reply ""))))
    (if (plusp (length text))
        (let* ((parsed (ignore-errors (nlk:decode-json text)))
               (output (and (hash-table-p parsed)
                            (nlk:json-value parsed :object "hookSpecificOutput")))
               (context (and (hash-table-p output)
                             (nlk:json-value output :string "additionalContext"))))
          (if (stringp context)
              (string-trim '(#\Space #\Tab #\Newline) context)
              ""))
        "")))

(defun ask (action payload directory)
  "One hook ACTION of the program in DIRECTORY => the block it adds, or \"\"."
  (context-of (funcall *run* action (nlk:encode-json-object payload) directory)))

(defun payload (project session &rest pairs)
  "The JSON object the plugin sent a hook action: the project as CWD, the
session, then PAIRS."
  (apply #'nlk:make-json-object
         (append (list "cwd" (uiop:native-namestring (uiop:ensure-directory-pathname project))
                       "session_id" (or session ""))
                 pairs)))

;;; --- which project ------------------------------------------------------------------

(defun session-cwd (session-id)
  "The folder SESSION-ID works in, or this process's."
  (or (ignore-errors (and (nlk:store-open-p) session-id
                           (nlk:durable-session-cwd (nlk:find-session session-id))))
      (ignore-errors (uiop:getcwd))
      (uiop:temporary-directory)))

(defun built-p (directory)
  "Whether DIRECTORY is a project `neuralmind build` has run in."
  (and directory
       (ignore-errors
         (probe-file (merge-pathnames +built-marker+
                                       (uiop:ensure-directory-pathname directory))))))

(defun project-for (session-id)
  "The project to serve for SESSION-ID: the pinned one when the section names
it, else the session's own folder -- the first one built, as the plugin
chose. NIL when none has been."
  (loop for candidate in (list (setting :project) (session-cwd session-id))
        for directory = (and candidate (ignore-errors (uiop:ensure-directory-pathname candidate)))
        when (built-p directory) return directory))

(defun folder-word (directory)
  "DIRECTORY's name, for a line that does not print a machine path."
  (or (ignore-errors (nlk:folder-name directory))
      (let ((parts (pathname-directory (uiop:ensure-directory-pathname directory))))
        (or (car (last parts)) (namestring directory)))))

;;; --- the turn ------------------------------------------------------------------------

(defvar *recapped* (make-hash-table :test #'equal)
  "The sessions this process has already given their recap: it is about where
the work stood, so it is given once a session, not once a turn.")

(defvar *injected* 0
  "How many turns this cell has added a block to since it started.")

(defvar *session* nil
  "The session the newest :ROUND was for. The :TOOL point carries no session
id of its own, and the round that opened the turn set this.")

(defun session-source (session-id)
  "What admitted SESSION-ID's turns: \"channel\", \"cron\", \"cli\", ..."
  (ignore-errors (and (nlk:store-open-p) session-id (nlk:session-source-kind session-id))))

(defun turn-input (session-id)
  "The words the newest turn of SESSION-ID was admitted with -- its prompt --
or NIL."
  (ignore-errors
    (nlk:json-value (nlk:events :session-id session-id :kind "turn.started"
                                :order :newest :limit 1 :as :payload)
                    :string "input")))

(defun recap-due-p (session)
  "Whether SESSION has not had its recap from this process; the ask marks it."
  (if (gethash session *recapped*)
      nil
      (progn
        (when (>= (hash-table-count *recapped*) 512) (clrhash *recapped*))
        (setf (gethash session *recapped*) t))))

(defun blocks-for (project session prompt)
  "The blocks one turn's first request carries: the session's recap when it is
due, then what PROMPT recalls. => a list of texts, possibly empty."
  (let ((blocks '()))
    (when (setting :recap)
      (when (recap-due-p session)
        (let ((recap (ask "session-start" (payload project session "source" "startup") project)))
          (when (plusp (length recap)) (push recap blocks)))))
    (when (and (setting :recall) (stringp prompt)
               (plusp (length (string-trim '(#\Space #\Tab #\Newline) prompt))))
      (let ((recall (ask "prompt-submit" (payload project session "prompt" prompt) project)))
        (when (plusp (length recall)) (push recall blocks))))
    (nreverse blocks)))

(defun recall-at-round (op)
  "The :ROUND point, one model request about to be built: NeuralMind's recap
at a session's first turn, and what this turn's own prompt recalls. A cron
run recalls nothing on its own, as the plugin decided for it."
  (let ((session (getf op :session-id))
        (turn (getf op :turn))
        (round (getf op :round)))
    (when (and session turn (eql 0 round) (nlk:store-open-p)
               (not (equal "cron" (session-source session))))
      (setf *session* session)
      (nlk:when-let (project (project-for session))
        (let ((blocks (blocks-for project session (turn-input session))))
          (when blocks
            (incf *injected*)
            (nlk:record-turn-context-prompt
             turn :text (format nil "~{~a~^~%~%~}" blocks))))))))

;;; --- what a write records ---------------------------------------------------------

(defun failed-p (answer)
  "Whether a tool's own answer reads as a failure -- what the plugin read off
the call's status and its no_change flag."
  (and (stringp answer)
       (uiop:string-prefix-p "ERROR" (string-left-trim '(#\Space #\Tab #\Newline) answer))))

(defun next-head (text from)
  "The position of the first +write-heads+ form at or past FROM, and the
position just past it => (values POSITION AFTER), or NIL."
  (let ((at nil) (after nil))
    (dolist (head +write-heads+)
      (let ((found (search head text :start2 from)))
        (when (and found (or (null at) (< found at)))
          (setf at found after (+ found (length head))))))
    (values at after)))

(defun written-paths (text)
  "The paths TEXT's (write-file ...) and (edit ...) forms name, in order."
  (if (not (stringp text))
      '()
      (let ((paths '()) (from 0))
        (loop
          (multiple-value-bind (at after) (next-head text from)
            (unless at (return (nreverse paths)))
            (let ((close (position #\" text :start after)))
              (unless close (return (nreverse paths)))
              (push (subseq text after close) paths)
              (setf from (1+ close))))))))

(defvar *background* (lambda (thunk) (nlk:spawn "neuralmind edit" thunk))
  "How one edit is recorded off the turn's path: THUNK run on a thread of its
own. Tests run it inline.")

(defun record-edit (project session path cwd)
  "Tell NeuralMind that PATH was written, as the plugin's post_tool_call did:
the same edit-activity action, the same tool_input.file_path."
  (let ((file (if (uiop:absolute-pathname-p path)
                  (pathname path)
                  (merge-pathnames path (uiop:ensure-directory-pathname cwd)))))
    (funcall *run*
             "edit-activity"
             (nlk:encode-json-object
              (nlk:make-json-object
               "cwd" (uiop:native-namestring (uiop:ensure-directory-pathname project))
               "session_id" (or session "")
               "tool_input" (nlk:make-json-object
                             "file_path" (uiop:native-namestring file)
                             "new_string" "")))
             project)))

(defun note-edits (session project arguments)
  "Record each file the eval call ARGUMENTS wrote, each on its own."
  (let ((cwd (session-cwd session)))
    (dolist (path (written-paths (nlk:json-value arguments :string "form")))
      (let ((path path))
        (funcall *background*
                 (lambda () (ignore-errors (record-edit project session path cwd))))))))

(defun editing-session ()
  "The session this tool call runs for: the running turn's, else the one the
newest :ROUND was for."
  (or (ignore-errors (getf (nle:turn) :session-id)) *session*))

(defun note-tool (op next)
  "The :TOOL point: after an eval that wrote files, each written file is
recorded; every other call passes through untouched."
  (let ((answer (funcall next op)))
    (handler-case
        (when (and (eq t (setting :record-edits))
                   (equal "eval" (getf op :name))
                   (hash-table-p (getf op :arguments))
                   (not (failed-p answer)))
          (let ((session (editing-session)))
            (nlk:when-let (project (and session (project-for session)))
              (note-edits session project (getf op :arguments)))))
      (error () nil))
    answer))

;;; --- the verbs ---------------------------------------------------------------------

(define-verb recall (query &key session)
  "The files, symbols and recorded decisions NeuralMind's index matches QUERY
with in the project, framed as NeuralMind frames them. It also records the
query in the project's session recap, as the plugin's prompt hook does."
  (let ((project (project-for session)))
    (if (null project)
        *no-project*
        (let ((text (ask "prompt-submit"
                         (payload project session "prompt" (nlk:trimmed (or query "")))
                         project)))
          (if (plusp (length text)) text *nothing*)))))

(define-verb recap (&key session)
  "The recap of the previous session in the project, as a session's first turn
gets it. It runs NeuralMind's whole session-start action, the plugin's too."
  (let ((project (project-for session)))
    (if (null project)
        *no-project*
        (let ((text (ask "session-start" (payload project session "source" "startup") project)))
          (if (plusp (length text)) text *nothing*)))))

(defun project-word (session)
  "The project this session is served from, in one clause."
  (let ((project (ignore-errors (project-for session))))
    (if project
        (format nil "project ~a" (folder-word project))
        (format nil "no built project here -- run \"neuralmind build\" in it, or set neuralmind.project"))))

(defun status-line (session)
  "One line: the project, the program, the switches and what was added."
  (format nil "neuralmind: ~a; command ~a, ~d second~:p a call; recall ~:[off~;on~], recap ~:[off~;on~], edits ~:[off~;on~]; ~d turn~:p given a block this session"
          (project-word session)
          (setting :command)
          (setting :timeout)
          (eq t (setting :recall))
          (eq t (setting :recap))
          (eq t (setting :record-edits))
          *injected*))

(defun split-verb (args)
  "ARGS as (values HEAD REST): the first word, and the trimmed remainder."
  (let* ((text (nlk:trimmed (or args "")))
         (space (position #\Space text)))
    (values (if space (subseq text 0 space) text)
            (if space (nlk:trimmed (subseq text (1+ space))) ""))))

(defun run-slash (args session-id)
  "One /neuralmind invocation => the answer's text. Bare words are a query;
status, recall QUERY and recap ask by name."
  (multiple-value-bind (head rest) (split-verb args)
    (cond ((zerop (length head)) (status-line session-id))
          ((string-equal head "status") (status-line session-id))
          ((string-equal head "recap") (recap :session session-id))
          ((string-equal head "recall")
           (if (zerop (length rest))
               "neuralmind: recall what? /neuralmind recall QUERY"
               (recall rest :session session-id)))
          (t (recall (nlk:trimmed args) :session session-id)))))

;;; --- the declaration ----------------------------------------------------------------

(nle:define-cell neuralmind
  (:section ("neuralmind")
    (:guide "install NeuralMind (pip install neuralmind) and run \"neuralmind build\" in your project; command names the neuralmind executable (usually just neuralmind); project pins one project, and left empty each session is served from its own folder; recall and recap turn the two automatic halves off one at a time")
    ("command" :string :required t
               :doc "the neuralmind executable: its name on PATH, or a path to it")
    ("project" :directory
               :doc "the project to serve, ahead of each session's own folder; it must have been built with \"neuralmind build\"")
    ("timeout" :integer :default 8 :min 1
               :doc "seconds one call to neuralmind may take before it is stopped and its block left out")
    ("recall" :boolean :default t
              :doc "add the files, symbols and decisions this turn's prompt matches to the request")
    ("recap" :boolean :default t
             :doc "start a session with the recap of the previous session in the project")
    ("record_edits" :boolean :default t
                    :doc "record the files a write in the turn changed, for the next session's recap"))
  (:settings (lambda (values table)
               (declare (ignore table))
               (let ((program (getf values :command)))
                 (unless (and program (plusp (length program)) (program-path program))
                   (nlk:config-error
                    "neuralmind.command is ~s: no such program -- install NeuralMind with \"pip install neuralmind\" (or pipx, or uv tool), run \"neuralmind build\" in your project, and name the executable here"
                    program)))
               values))
  (:command "neuralmind" 'run-slash
            :catches 'neuralmind-error
            :description "The project's code memory: what this turn's prompt matches"
            :argument-hint "[status|recall QUERY|recap]")
  (:help :neuralmind "neuralmind: the project's code memory, added to each turn - (neuralmind:recall \"where is the discount code applied\")" *primer*)
  (:hook :round (nle:observer #'recall-at-round "neuralmind"))
  (:hook :tool #'note-tool)
  (:start (lambda ()
            (setf *recapped* (make-hash-table :test #'equal)
                  *injected* 0
                  *session* nil)
            (nle:on-stop (lambda ()
                           (setf *recapped* (make-hash-table :test #'equal)
                                 *injected* 0
                                 *session* nil))))))
