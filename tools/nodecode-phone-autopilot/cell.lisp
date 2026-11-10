;;;; cell.lisp --- phone-autopilot as one Nodecode tool: routine.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of phone-autopilot's Hermes plugin (tools/routine.py,
;;;; engine.py, prelude.py, guard.py and commands.py). The original kept each
;;;; routine as <data>/routines/<name>/{routine.json,routine.py,versions/},
;;;; bound the operator's approval to the sha256 of the exact bytes, merged
;;;; the run's params over the saved ones, prepended an app guard and a
;;;; prelude to the script, piped it to `python -m phone_harness.run', read
;;;; the `@@AUTOPILOT' lines it printed as events, and wrote the run as
;;;; <data>/runs/<name>/<run-id>.json with the checkpoint that missed, the
;;;; screenshot it left and the tail of what it printed. All of that is here.
;;;;
;;;; The runner is the operator's own phone-harness (the phone-harness cell
;;;; beside this one runs the same one); the script is fed on stdin. The one
;;;; name a test stubs is *RUN*, which answers the same three values
;;;; NLK:RUN-BOUNDED does, and *ASK*, the operator's approval question.

(defpackage #:nodecode-phone-autopilot
  (:use #:cl))

(in-package #:nodecode-phone-autopilot)

(nlk:define-peripheral phone-autopilot :not-running t)

;;; --- what the original saved, kept and bounded -------------------------------

(defparameter +default-seconds+ 300
  "A run's bound when neither the call nor the section names one.")
(defparameter +max-seconds+ 900
  "The original's ceiling on one run (DEFAULT_TIMEOUT_S and MAX_TIMEOUT_S).")
(defparameter +min-seconds+ 1)
(defparameter +keep-runs+ 50
  "Run records kept per routine; the original's MAX_RUNS.")
(defparameter +tail-chars+ 4000
  "How much of the run's output and of its stderr the record keeps.")
(defparameter +event-prefix+ "@@AUTOPILOT "
  "What the script prints an event on, one JSON object a line.")
(defparameter +preview-lines+ 12
  "How much of the code the approval question shows.")
(defparameter +screenshot-types+ '("png" "jpg" "jpeg" "webp")
  "What a screenshot may be called.")
(defparameter +secret-suffixes+ '("_KEY" "_TOKEN" "_SECRET" "_PASSWORD")
  "How a variable's name says it is a credential, as the original read them.")
(defparameter +actions+ '("save" "run" "list" "show" "runs" "delete")
  "The routine tool's actions, as the original's _ACTION_NAMES.")
(defparameter +helper-names+
  '("open_app" "shell" "type_text" "tap_text" "tap_ui" "tap_image" "tap"
    "swipe" "press" "checkpoint" "screenshot" "screen" "find_text" "done" "step")
  "The phone helpers the approval summary counts in a routine's code.")

;;; --- the call's boundary ------------------------------------------------------

(nlk:define-contract routine-call
  (:doc "Save, replay and repair phone routines: scripts that drive the
operator's phone with no model in the loop. Workflow: 1) explore the task
with phone_exec first and keep the steps that worked; 2) write a clean
script: call checkpoint(name, text=...) after every screen change, read
per-run values from PARAMS (never hardcode them), and end with done(); make
it survive change: skip a phase whose goal already holds, and act through
the sturdiest handle (open_app, then tap_text by label, coordinates last);
3) action=save (the operator approves the code, and approval is bound to
these exact bytes); 4) action=run to verify it end to end. A run that fails
answers failed_checkpoint (the name, the text it expected, what was on
screen, the screenshot it left) -- look at the screenshot, fix the screen,
then save a new version. Save only scripts that worked on the phone.")
  ("action" :text :required t :options '("save" "run" "list" "show" "runs" "delete")
   :doc "What to do with the routine.")
  ("name" :text
   :doc "Routine name: lowercase letters, digits and hyphens, up to 48 characters.")
  ("code" :text
   :doc "save: the routine body, Python with the phone-harness helpers and the
routine prelude already in scope (checkpoint, done, step, PARAMS).")
  ("title" :text :doc "save: a human title, e.g. 'Claim the daily reward'.")
  ("app" :text :doc "save: the Android package the routine drives, e.g. com.example.app.")
  ("description" :text :doc "save: what the routine does, one sentence.")
  ("params" :object
   :doc "save: default values for PARAMS; run: values for this run only, merged over them.")
  ("timeout_s" :integer
   :doc "run: the run's bound in seconds, 1 to 900 (default 300).")
  ("limit" :integer :doc "runs: how many run records to answer, newest first (default 20)."))

;;; --- where a routine lives ----------------------------------------------------

(defun data-directory ()
  "The folder this cell keeps routines and run records in: the section's
`directory', under the Nodecode home unless it names somewhere else."
  (setting :directory))

(defun routine-root () (merge-pathnames "routines/" (data-directory)))

(defun runs-root () (merge-pathnames "runs/" (data-directory)))

(defun routine-folder (name)
  (merge-pathnames (format nil "~a/" name) (routine-root)))

(defun routine-meta-path (name) (merge-pathnames "routine.json" (routine-folder name)))

(defun routine-code-path (name) (merge-pathnames "routine.py" (routine-folder name)))

(defun run-folder (name) (merge-pathnames (format nil "~a/" name) (runs-root)))

;;; --- saying no ----------------------------------------------------------------

(define-condition routine-refusal (error)
  ((message :initarg :message :reader routine-refusal-message))
  (:report (lambda (condition stream)
             (write-string (routine-refusal-message condition) stream))))

(defun refuse (control &rest arguments)
  "The tool's no: the body answers it as JSON with success false."
  (error 'routine-refusal :message (apply #'format nil control arguments)))

(defun check-name (name)
  "NAME as a routine name: it becomes a folder name, so only the slug
alphabet, 1 to 48 characters, as the original's _NAME_RE."
  (unless (and (stringp name)
               (plusp (length name))
               (<= (length name) 48)
               (cl-ppcre:scan "^[a-z0-9][a-z0-9-]*\\z" name))
    (refuse "~s is not a routine name: use 1 to 48 lowercase letters, digits or hyphens" name))
  name)

(defun tag (name)
  "NAME with its first letter capitalised, for a sentence."
  (if (plusp (length name))
      (concatenate 'string (string (char-upcase (char name 0))) (subseq name 1))
      name))

;;; --- the small file helpers ---------------------------------------------------

(defun read-text-file (path)
  "PATH's bytes as text, or NIL when there is no such file."
  (when (probe-file path) (nlk:read-text path)))

(defun write-text-file (path text)
  "TEXT at PATH in one atomic replace, the folder made first: a crash never
leaves half a routine or half a record."
  (ensure-directories-exist path)
  (let ((tmp (merge-pathnames (format nil ".~a-~d.tmp" (pathname-name path) (random 1000000))
                               path)))
    (with-open-file (out tmp :direction :output :if-exists :supersede
                             :if-does-not-exist :create :external-format :utf-8)
      (write-string text out))
    (nlk:replace-file tmp path))
  path)

(defun read-json-file (path)
  "PATH decoded as one JSON object, or NIL: an absent, blank or torn file
reads as nothing."
  (let ((text (read-text-file path)))
    (when (and text (plusp (length (nlk:trimmed text))))
      (let ((object (ignore-errors (nlk:decode-json text))))
        (and (hash-table-p object) object)))))

(defun write-json-file (path object)
  (write-text-file path (format nil "~a~%" (nlk:encode-json-object object))))

(defun field (object key)
  "OBJECT's string KEY, or NIL."
  (and (hash-table-p object) (gethash key object)))

(defun copy-object (object)
  "A fresh EQUAL table with OBJECT's members."
  (let ((copy (make-hash-table :test #'equal)))
    (when (hash-table-p object)
      (maphash (lambda (key value) (setf (gethash key copy) value)) object))
    copy))

(defun non-empty (text)
  "TEXT when it is a non-empty string, else NIL."
  (and (stringp text) (plusp (length (nlk:trimmed text))) text))

;;; --- a routine's approval -----------------------------------------------------

(defun sha256-hex (text)
  "TEXT's sha256, lowercase hex: approval is bound to these exact bytes."
  (string-downcase
   (ironclad:byte-array-to-hex-string
    (ironclad:digest-sequence :sha256 (sb-ext:string-to-octets text :external-format :utf-8)))))

(defun approved-p (meta)
  "Whether META's bytes are the ones the operator approved."
  (let ((sha (field meta "sha256")) (approved (field meta "approved_sha256")))
    (and (stringp sha) (stringp approved) (equal sha approved))))

(defun digests-match-p (path sha)
  "Whether PATH's bytes still hash to SHA: a routine.py edited on disk is refused."
  (let ((text (read-text-file path)))
    (and text (stringp sha) (equal (sha256-hex text) sha))))

;;; --- the approval question ----------------------------------------------------

(defun count-call (code helper)
  "How many times CODE calls HELPER: its name followed by an open paren."
  (loop with needle = (concatenate 'string helper "(")
        with start = 0
        for at = (search needle code :start2 start)
        while at
        do (setf start (+ at (length needle)))
        count 1))

(defun describe-code (code)
  "A one-line reading of CODE for the approval question: how long it is and
which phone helpers it calls. The original read an AST; this counts the
helpers a routine can reach the phone by, which is what the question is
about."
  (let* ((lines (1+ (count #\Newline code)))
         (calls (loop for helper in +helper-names+
                      for times = (count-call code helper)
                      when (plusp times)
                        collect (if (> times 1) (format nil "~a x~d" helper times) helper))))
    (format nil "~d line~:p~@[; calls ~{~a~^, ~}~]"
            lines (and calls (subseq calls 0 (min 8 (length calls)))))))

(defun preview (code)
  "The head of CODE the question shows, and how much it leaves out."
  (let* ((lines (uiop:split-string code :separator '(#\Newline)))
         (rest (- (length lines) +preview-lines+)))
    (format nil "~{~a~^~%~}~@[~%~d more lines~]"
            (subseq lines 0 (min +preview-lines+ (length lines)))
            (and (plusp rest) rest))))

(defun ask-approval (name code &key (verb "save") (version 1) title)
  "Ask the operator to approve CODE, the whole of it: the question names
what it calls and shows its head. => T, or :denied, :nobody, :timeout. With
the section's `approve' off nothing is asked, as the original under /yolo."
  (if (not (setting :approve))
      t
      (multiple-value-bind (choice state)
          (nle:ask-operator
           (format nil "~@[~a: ~]routine ~s v~d -- ~a this code?"
                   (non-empty title) name version (if (equal verb "save") "save" "run"))
           :detail (format nil "~a~%~%~a" (describe-code code) (preview code))
           :choices '("Approve" "Deny")
           :seconds 120)
        (cond ((null choice) (if (eq state :timeout) :timeout :nobody))
              ((equal choice "Approve") t)
              (t :denied)))))

(defun approval-note (decision name verb)
  "What the tool says about code the operator has not approved."
  (ecase decision
    (:denied (format nil "The operator denied this ~a. The routine is kept but not approved: fix it and save again, or approve it with `/phone-autopilot approve ~a'." verb name))
    (:nobody (format nil "Nobody answered the approval question, so the routine is not approved yet: approve it from a chat that answers questions, with `/phone-autopilot approve ~a', or turn off `approve' in the phone-autopilot section." name))
    (:timeout (format nil "The approval question went unanswered, so the routine is not approved yet: answer it, or approve it with `/phone-autopilot approve ~a'." name))))

;;; --- routines on disk ----------------------------------------------------------

(defun routine-meta (name)
  "NAME's routine.json, or NIL when there is no such routine."
  (read-json-file (routine-meta-path name)))

(defun known-routine (name)
  (or (routine-meta name) (refuse "no routine named ~s" name)))

(defun public-meta (meta &optional code)
  "META plus `approved' and, when CODE is given, the bytes it runs: the
original's _public and get_routine."
  (let ((copy (copy-object meta)))
    (setf (gethash "approved" copy) (if (approved-p meta) t :false))
    (when code (setf (gethash "code" copy) code))
    copy))

(defun list-routines ()
  "Every routine in the folder, by name."
  (let ((root (routine-root)))
    (when (uiop:directory-exists-p root)
      (loop for path in (sort (uiop:subdirectories root) #'string< :key #'namestring)
            for name = (car (last (pathname-directory path)))
            for meta = (and name (routine-meta name))
            when meta collect (public-meta meta)))))

(defun list-runs (name &key (limit 20))
  "NAME's run records, newest first, at most LIMIT of them."
  (let ((folder (run-folder name)))
    (when (uiop:directory-exists-p folder)
      (let* ((paths (sort (uiop:directory-files folder) #'string< :key #'pathname-name))
             (kept (last paths (max 1 limit))))
        (loop for path in kept
              for object = (read-json-file path)
              when object collect object)))))

(defun prune-runs (name)
  "Keep the newest +KEEP-RUNS+ records."
  (let ((folder (run-folder name)))
    (when (uiop:directory-exists-p folder)
      (let* ((files (sort (uiop:directory-files folder) #'string< :key #'pathname-name))
             (drop (- (length files) (max 1 +keep-runs+))))
        (loop for path in (subseq files 0 (max 0 drop))
              do (ignore-errors (delete-file path)))))))

(defun remember-run (name record)
  "Write RECORD under the routine's runs, prune them, and mark the routine's last run."
  (write-json-file (merge-pathnames (format nil "~a.json" (field record "run_id"))
                                     (run-folder name))
                    record)
  (prune-runs name)
  (let ((meta (routine-meta name)))
    (when meta
      (setf (gethash "last_run" meta)
            (nlk:json-object "run_id" (field record "run_id")
                             "ok" (field record "ok")
                             "finished_at" (field record "finished_at")
                             "duration_s" (field record "duration_s")))
      (write-json-file (routine-meta-path name) meta)))
  record)

;;; --- the runner ----------------------------------------------------------------

(defun runner-argv ()
  "The argv that runs phone-harness here, from the section's command: one
word for a program on PATH, more for `uvx phone-harness'."
  (nlk:split-words (or (setting :command) "")))

(defun environment-entries ()
  (or (ignore-errors (sb-ext:posix-environ)) '()))

(defun secret-variables ()
  "Every variable whose name ends as a credential's does: a bare NAME in a
child environment removes it (NLK:ENVIRONMENT-WITH)."
  (loop for entry in (environment-entries)
        for end = (or (position #\= entry) (length entry))
        for name = (subseq entry 0 end)
        when (some (lambda (suffix) (uiop:string-suffix-p (string-upcase name) suffix))
                   +secret-suffixes+)
          collect name))

(defun child-environment ()
  "This process's environment with the original's two settings laid on and
every credential-suffixed variable removed."
  (append (list "PYTHONUNBUFFERED=1" "DO_NOT_TRACK=1") (secret-variables)))

(defun run-child (argv &key seconds directory environment input)
  (nlk:run-bounded argv :seconds seconds :directory directory :environment environment :input input))

(defparameter *run* #'run-child
  "How the cell starts the runner: (*RUN* ARGV :SECONDS N :DIRECTORY DIR
:ENVIRONMENT LIST :INPUT TEXT) => (VALUES STDOUT STDERR STATUS), STATUS an
exit code, 128 plus a signal's number, or :TIMEOUT. A test binds it.")

(defun attempt (argv seconds directory environment input)
  "The child's (VALUES OUTPUT ERROR-OUTPUT STATUS); one that could not be
started answers :UNLAUNCHED with the reason where the error output goes."
  (handler-case
      (funcall *run* argv :seconds seconds :directory directory :environment environment :input input)
    (error (condition) (values nil (format nil "~a" condition) :unlaunched))))

(defun bounded-seconds (asked)
  (min +max-seconds+ (max +min-seconds+
                           (or (and (integerp asked) asked)
                               (setting :timeout-seconds)
                               +default-seconds+))))

(defun tail (text limit)
  (let ((text (or text "")))
    (if (> (length text) limit) (subseq text (- (length text) limit)) text)))

(defun last-line (text)
  "The last non-blank line of TEXT."
  (let ((lines (remove-if (lambda (line) (zerop (length (nlk:trimmed line))))
                           (uiop:split-string (or text "") :separator '(#\Newline)))))
    (and lines (nlk:trimmed (car (last lines))))))

;;; --- the script a routine runs --------------------------------------------------

(defun python-string (text)
  "TEXT as a Python string literal: double-quoted, its quotes, backslashes
and newlines escaped."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for char across text
          do (case char
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (t (write-char char out))))
    (write-char #\" out)))

(defparameter +prelude+
  "import json as _json
import time as _time

PARAMS = {}
ROUTINE_DIR = None

def _emit(event, **fields):
    print('@@AUTOPILOT ' + _json.dumps(dict(event=event, **fields), default=str), flush=True)

def _texts():
    return [str((box or {}).get('text', '')) for box in (screen() or [])]

def _lower(values):
    return [v.strip().lower() for v in values]

def _current():
    try:
        return str(current_app() or '')
    except Exception:
        return ''

GUARD_ALLOWED = set([~a])
GUARD_ASK = ~a
GUARD_SYSTEM = ['com.android.systemui', 'android']
GUARD_DIALOGS = ['com.google.android.permissioncontroller', 'com.android.permissioncontroller']
_GUARD_STOPPING = [False]

def _guard():
    if _GUARD_STOPPING[0]:
        return
    app = _current()
    if not app or app in GUARD_SYSTEM or app in GUARD_ALLOWED:
        return
    if app in GUARD_DIALOGS:
        _emit('guard', app=app, reason='permission')
    elif GUARD_ASK:
        _emit('guard', app=app, reason='blocked')
    else:
        return
    _GUARD_STOPPING[0] = True
    try:
        home()
    except Exception:
        pass
    raise SystemExit(3)

def _guarded(name, helper):
    def guarded(*args, **kwargs):
        _guard()
        return helper(*args, **kwargs)
    guarded.__name__ = name
    return guarded

for _helper_name in [~a]:
    _helper = globals().get(_helper_name)
    if callable(_helper):
        globals()[_helper_name] = _guarded(_helper_name, _helper)

class CheckpointMiss(SystemExit):
    pass

def _evidence():
    shot = None
    try:
        shot = screenshot()
    except Exception:
        shot = None
    try:
        seen = _texts()[:40]
    except Exception as exc:
        seen = ['(screen text unavailable: %s)' % type(exc).__name__]
    return shot, seen

def _miss(name, expected):
    shot, seen = _evidence()
    _emit('checkpoint', name=name, status='failed', expected=expected, seen_text=seen, screenshot=shot)
    raise CheckpointMiss(name)

def checkpoint(name, text=None, any_of=None, app=None, timeout=10.0, exact=False):
    wanted = []
    if text is not None:
        wanted.append(str(text))
    if any_of:
        wanted.extend(str(one) for one in any_of)
    expected = ', '.join(wanted) if wanted else 'the screen to be ready'
    if app is not None:
        expected = expected + ' app ' + str(app)
    _emit('checkpoint', name=name, status='started')
    deadline = _time.monotonic() + float(timeout)
    while True:
        ok = True
        if app is not None and str(app) not in _current():
            ok = False
        if ok and wanted:
            try:
                seen = _lower(_texts())
            except Exception:
                seen = None
            if seen is None:
                ok = False
            else:
                asked = _lower(wanted)
                if exact:
                    ok = all(any(one == seen_one for seen_one in seen) for one in asked)
                else:
                    ok = all(any(one in seen_one for seen_one in seen) for one in asked)
        if ok:
            break
        if _time.monotonic() >= deadline:
            _miss(name, expected)
        _time.sleep(0.5)
    _emit('checkpoint', name=name, status='passed')

def step(name):
    _emit('step', name=name)

def done(**summary):
    _emit('done', **summary)

"
  "The prologue a routine runs with: the app guard, PARAMS, and the three
helpers the original's prelude.py taught the model to use -- checkpoint,
step and done. Its holes are the allowed apps, whether the operator asks
about apps, and the phone helpers to guard.")

(defun build-script (name code params)
  "The script the runner reads: guard and prelude, the run's params, the
routine's folder, then the routine's own code."
  (let ((header (format nil +prelude+
                         (format nil "~{~a~^, ~}"
                                 (loop for app in (setting :allowed-apps)
                                       collect (python-string app)))
                         (if (setting :ask-before-apps) "True" "False")
                         (format nil "~{~a~^, ~}"
                                 (loop for helper in +helper-names+
                                       collect (python-string helper))))))
    (concatenate 'string header
                 (format nil "PARAMS.update(_json.loads(~a))~%ROUTINE_DIR = ~a~%~%"
                         (python-string (nlk:encode-json-object (or params (nlk:json-object))))
                         (python-string (uiop:native-namestring (routine-folder name))))
                 code)))

;;; --- one run -------------------------------------------------------------------

(defun split-lines (text)
  (uiop:split-string (or text "") :separator '(#\Newline)))

(defun parse-output (output)
  "OUTPUT as (VALUES EVENTS OUTPUT-TAIL): every @@AUTOPILOT line decoded as
one event, the rest kept as what the script printed."
  (let ((events '()) (lines '()))
    (dolist (line (split-lines output))
      (if (uiop:string-prefix-p +event-prefix+ line)
          (let ((event (ignore-errors
                         (nlk:decode-json (subseq line (length +event-prefix+))))))
            (when (hash-table-p event) (push event events)))
          (push line lines)))
    (values (nreverse events) (format nil "~{~a~^~%~}" (nreverse lines)))))

(defun named-event (events name)
  (find-if (lambda (event) (equal (field event "event") name)) events))

(defun failed-checkpoint (events)
  "The checkpoint event that missed, or NIL."
  (find-if (lambda (event) (and (equal (field event "event") "checkpoint")
                                 (equal (field event "status") "failed")))
           events))

(defun guard-message (event)
  "What the operator and the model read when the guard stopped a run."
  (let ((app (field event "app")))
    (if (equal (field event "reason") "permission")
        (format nil "stopped: the phone is showing Android's permission dialog for ~a. The operator has to answer it on the phone; a script must never tap it." app)
        (format nil "stopped: the operator has not allowed ~a. Allow it with `/phone-autopilot allow ~a', or turn off ask_before_apps in the phone-autopilot section." app app))))

(defun new-run-id ()
  "YYYYMMDD-HHMMSS-xxxx, the original's run id: they sort by time."
  (multiple-value-bind (second minute hour day month year) (get-decoded-time)
    (format nil "~4,'0d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d-~4,'0x"
            year month day hour minute second (random 65536))))

(defun run-error (status bound stderr failed guard)
  "Why a run failed, in one line, or NIL when it passed."
  (cond
    ((eq status :unlaunched) (format nil "could not start ~{~a~^ ~}: ~a" (runner-argv) (last-line stderr)))
    ((eq status :timeout) (format nil "timed out after ~ds" bound))
    (failed (format nil "checkpoint ~s failed" (field failed "name")))
    (guard (guard-message guard))
    ((not (eql status 0)) (or (last-line stderr) (format nil "exited with code ~a" status)))
    (t nil)))

(defun run-record (name meta run-id started bound status out err events)
  "The record the original wrote: what ran, whether it passed, where it
stopped, and the tail of what it printed."
  (let* ((finished (get-universal-time))
         (failed (failed-checkpoint events))
         (guard (named-event events "guard"))
         (error-text (run-error status bound err failed guard)))
    (nlk:json-object
     "run_id" run-id
     "routine" name
     "version" (or (field meta "version") 0)
     "ok" (if (null error-text) t :false)
     "started_at" started
     "finished_at" finished
     "duration_s" (- finished started)
     :opt "exit_code" (and (integerp status) status)
     "events" (coerce events 'vector)
     :opt "failed_checkpoint" (and failed (copy-object failed))
     "output_tail" (tail out +tail-chars+)
     "stderr_tail" (tail err +tail-chars+)
     :opt "error" error-text)))

(defun run-once (name meta code params bound)
  "Replay CODE on the phone as one run, holding the one phone for its
duration, and write the record."
  (let ((host (data-directory)))
    (ensure-directories-exist (routine-folder name))
    (ensure-directories-exist host)
    (unless (nlk:lock-folder host :wait 0)
      (refuse "phone busy: another routine is still running"))
    (unwind-protect
         (let ((started (get-universal-time))
               (run-id (new-run-id))
               (script (build-script name code params)))
           (multiple-value-bind (out err status)
               (attempt (runner-argv) bound (routine-folder name) (child-environment) script)
             (multiple-value-bind (events printed) (parse-output out)
               (remember-run name (run-record name meta run-id started bound status
                                               printed err events)))))
      (ignore-errors (nlk:unlock-folder host)))))

;;; --- the six actions -------------------------------------------------------------

(defun answer (object) (nlk:encode-json-object object))

(defun answer-failure (control &rest arguments)
  (nle:failure (answer (nlk:json-object "success" :false
                                         "error" (apply #'format nil control arguments)))))

(defun record-answer (record)
  "The run record as a model reads it: the same members, with success."
  (let ((copy (copy-object record)))
    (setf (gethash "success" copy) (gethash "ok" copy))
    (let ((failed (field record "failed_checkpoint")))
      (when (equal (field record "ok") :false)
        (setf (gethash "next_steps" copy)
              (if failed
                  (format nil "Run failed at checkpoint ~s. Read seen_text, and (look ~a) shows the screenshot it left: bring the phone to the screen the checkpoint expects, then save the fixed script as a new version with action=save and run it once more."
                          (field failed "name") (or (field failed "screenshot") "(none saved)"))
                  "Run failed without a checkpoint miss: read error and stderr_tail, fix the script, save a new version with action=save, and run it again."))))
    (answer copy)))

(defun saved-params (call old)
  "The defaults a save leaves the routine with: the ones given, or else the
ones it keeps."
  (or (routine-call-params call) (field old "params") (nlk:json-object)))

(defun save-action (call)
  (let* ((name (check-name (routine-call-name call)))
         (code (or (routine-call-code call) ""))
         (old (routine-meta name))
         (version (1+ (or (and old (field old "version")) 0))))
    (when (zerop (length (nlk:trimmed code)))
      (refuse "code is required for save"))
    (let* ((title (or (non-empty (routine-call-title call)) (field old "title") name))
           (decision (ask-approval name code :verb "save" :version version :title title))
           (meta (nlk:json-object
                  "name" name
                  "version" version
                  "sha256" (sha256-hex code)
                  :when (eq decision t) "approved_sha256" (sha256-hex code)
                  "title" title
                  "app" (or (non-empty (routine-call-app call)) (field old "app") "")
                  "description" (or (non-empty (routine-call-description call)) (field old "description") "")
                  "params" (saved-params call old)
                  "created_at" (or (field old "created_at") (get-universal-time))
                  "updated_at" (get-universal-time))))
      (write-text-file (merge-pathnames (format nil "versions/~d.py" version) (routine-folder name)) code)
      (write-text-file (routine-code-path name) code)
      (write-json-file (routine-meta-path name) meta)
      (answer (nlk:json-object
               "success" t
               "routine" (public-meta meta)
               "next_steps" (if (eq decision t)
                                "Run it with action=run to verify it on the phone, then say what it did."
                                (approval-note decision name "save")))))))

(defun run-action (call)
  (let* ((name (check-name (routine-call-name call)))
         (meta (known-routine name))
         (path (routine-code-path name))
         (code (or (read-text-file path)
                   (refuse "routine ~s has no code on disk; save it again" name))))
    (unless (digests-match-p path (field meta "sha256"))
      (refuse "routine ~s was edited on disk after it was saved; save it again" name))
    (let ((decision (if (approved-p meta)
                        t
                        (ask-approval name code :verb "run"
                                      :version (field meta "version")
                                      :title (field meta "title")))))
      (unless (eq decision t)
        (refuse "routine ~s v~a is not approved. ~a"
                name (field meta "version") (approval-note decision name "run")))
      (unless (approved-p meta)
        (setf (gethash "approved_sha256" meta) (field meta "sha256"))
        (write-json-file (routine-meta-path name) meta))
      (record-answer
       (run-once name meta code
                 (merge-params (field meta "params") (routine-call-params call))
                 (bounded-seconds (routine-call-timeout-s call)))))))

(defun merge-params (saved given)
  "The saved defaults with GIVEN laid over them, as the original merged them."
  (let ((merged (copy-object saved)))
    (when (hash-table-p given)
      (maphash (lambda (key value) (setf (gethash key merged) value)) given))
    merged))

(defun list-action ()
  (answer (nlk:json-object "success" t "routines" (coerce (list-routines) 'vector))))

(defun show-action (call)
  (let* ((name (check-name (routine-call-name call)))
         (meta (known-routine name)))
    (answer (nlk:json-object
             "success" t
             "routine" (public-meta meta (or (read-text-file (routine-code-path name)) ""))
             "runs" (coerce (list-runs name :limit 5) 'vector)))))

(defun runs-action (call)
  (let* ((name (check-name (routine-call-name call)))
         (limit (or (routine-call-limit call) 20)))
    (known-routine name)
    (answer (nlk:json-object "success" t "routine" name
                              "runs" (coerce (list-runs name :limit limit) 'vector)))))

(defun delete-action (call)
  (let ((name (check-name (routine-call-name call))))
    (known-routine name)
    (ignore-errors (uiop:delete-directory-tree (routine-folder name) :validate t))
    (ignore-errors (uiop:delete-directory-tree (run-folder name) :validate t))
    (answer (nlk:json-object "success" t "deleted" name))))

(defun routine-tool (call)
  "The routine tool: CALL's action, answered as JSON."
  (handler-case
      (let ((action (routine-call-action call)))
        (cond ((equal action "save") (save-action call))
              ((equal action "run") (run-action call))
              ((equal action "list") (list-action))
              ((equal action "show") (show-action call))
              ((equal action "runs") (runs-action call))
              ((equal action "delete") (delete-action call))
              (t (refuse "unknown action ~s; expected one of ~{~a~^, ~}" action +actions+))))
    (routine-refusal (condition)
      (answer-failure "~a" (routine-refusal-message condition)))
    (error (condition)
      (answer-failure "~a: ~a" (type-of condition) condition))))

;;; --- the allowlist ---------------------------------------------------------------

(defvar *allowed-apps* nil
  "The apps routines may use in this image: the section's, moved by
`/phone-autopilot allow' and `revoke'.")

(defun allowed-apps ()
  (or *allowed-apps* (setting :allowed-apps)))

(defun check-package (app)
  "APP as an Android package name, as the original's _PACKAGE_RE."
  (let ((app (and (stringp app) (nlk:trimmed app))))
    (unless (and app (plusp (length app)) (<= (length app) 200)
                 (cl-ppcre:scan "^[A-Za-z][A-Za-z0-9_]*(\\.[A-Za-z][A-Za-z0-9_]*)+\\z" app))
      (refuse "~s is not an Android package name (like com.spotify.music)" app))
    app))

(defun save-allowed-apps (apps)
  "Keep the allowlist in the section, for the next boot as for this one."
  (setf *allowed-apps* apps)
  (ignore-errors (nle:config-set '("phone-autopilot" "allowed_apps") apps))
  apps)

;;; --- /phone-autopilot ----------------------------------------------------------------

(defun when-text (universal)
  "A universal time as YYYY-MM-DD HH:MM, in this machine's zone."
  (if (and (integerp universal) (plusp universal))
      (multiple-value-bind (second minute hour day month year) (decode-universal-time universal)
        (declare (ignore second))
        (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" year month day hour minute))
      "never"))

(defun mark (ok) (if (equal ok t) "yes" "no"))

(defun list-text ()
  (let ((routines (list-routines)))
    (if (null routines)
        "No routines yet. Explore a task on the phone, then save what worked with the routine tool (action=save)."
        (with-output-to-string (out)
          (format out "| Routine | Version | Approved | Last run |~%|---|---|---|---|~%")
          (dolist (meta routines)
            (let* ((last (field meta "last_run"))
                   (ran (and last (format nil "~a ~a" (mark (field last "ok"))
                                          (when-text (field last "finished_at"))))))
              (format out "| ~a | ~a | ~a | ~a |~%"
                      (field meta "name") (field meta "version")
                      (mark (field meta "approved")) (or ran "never"))))))))

(defun show-text (name)
  (let* ((meta (known-routine name))
         (runs (list-runs name :limit 5)))
    (with-output-to-string (out)
      (format out "~a (~a), version ~a, ~a~@[ - ~a~]~%~@[App: ~a~%~]~%"
              (field meta "title") name (field meta "version")
              (if (approved-p meta) "approved" "not approved")
              (non-empty (field meta "description"))
              (non-empty (field meta "app")))
      (format out "```python~%~a~%```~%" (or (read-text-file (routine-code-path name)) ""))
      (format out "Recent runs:~%")
      (if runs
          (dolist (run runs)
            (format out "- ~a ~a ~a~%" (when-text (field run "finished_at"))
                    (mark (field run "ok")) (field run "run_id")))
          (format out "- no runs yet~%")))))

(defun run-text (name)
  (record-answer (run-once name (known-routine name)
                            (or (read-text-file (routine-code-path name))
                                (refuse "routine ~s has no code on disk; save it again" name))
                            (field (known-routine name) "params")
                            (bounded-seconds nil))))

(defun apps-text ()
  (format nil "ask before apps: ~a~%allowed: ~{~a~^, ~}~%"
          (if (setting :ask-before-apps) "on" "off")
          (or (allowed-apps) (list "none"))))

(defun run-slash (args session-id)
  "`/phone-autopilot' answers the routines, or moves the allowlist: the
human's own shortcut, with no model in it."
  (declare (ignore session-id))
  (handler-case
      (let* ((words (nlk:split-words (or args "")))
             (verb (string-downcase (or (first words) "list")))
             (name (second words)))
        (cond
          ((member verb '("list" "") :test #'equal) (list-text))
          ((equal verb "show") (show-text (check-name name)))
          ((equal verb "run") (run-text (check-name name)))
          ((equal verb "delete")
           (check-name name)
           (known-routine name)
           (ignore-errors (uiop:delete-directory-tree (routine-folder name) :validate t))
           (ignore-errors (uiop:delete-directory-tree (run-folder name) :validate t))
           (format nil "deleted ~a~%" name))
          ((equal verb "approve")
           (let* ((meta (known-routine (check-name name)))
                  (sha (field meta "sha256")))
             (setf (gethash "approved_sha256" meta) sha)
             (write-json-file (routine-meta-path name) meta)
             (format nil "~a v~a approved: these exact bytes may now run without a question.~%"
                     name (field meta "version"))))
          ((equal verb "deny")
           (let ((meta (known-routine (check-name name))))
             (remhash "approved_sha256" meta)
             (write-json-file (routine-meta-path name) meta)
             (format nil "~a no longer approved: its next run asks again.~%" name)))
          ((equal verb "apps") (apps-text))
          ((equal verb "allow")
           (let ((app (check-package name)))
             (unless (member app (allowed-apps) :test #'equal)
               (save-allowed-apps (append (allowed-apps) (list app))))
             (format nil "~a allowed.~%" app)))
          ((equal verb "revoke")
           (let ((app (check-package name)))
             (save-allowed-apps (remove app (allowed-apps) :test #'equal))
             (format nil "~a revoked.~%" app)))
          (t (format nil "Usage: /phone-autopilot [list | show <name> | run <name> | delete <name> | approve <name> | deny <name> | apps | allow <app> | revoke <app>]~%"))))
    (routine-refusal (condition) (format nil "~a~%" (routine-refusal-message condition)))))

;;; --- the manual ------------------------------------------------------------------

(defparameter +manual+
  "phone-autopilot keeps a phone task as a routine: one Python script that
drives the operator's phone through phone-harness, saved once, approved
once, and replayed afterwards with no model in the loop. Work the task out
with phone_exec first; then save the steps that worked.

  routine action=save  name code [title app description params]
  routine action=run   name [params timeout_s]
  routine action=list | show | runs | delete   name [limit]

A routine is stored as <directory>/routines/<name>/routine.json and
routine.py, with every saved version under versions/, and its runs as
<directory>/runs/<name>/<run-id>.json. Saving a routine asks the operator
to approve the code -- the question names the phone helpers it calls and
shows its head -- and the approval is bound to the sha256 of those exact
bytes: a routine.py edited on disk is refused, and a new save asks again.
`approve' off in the section saves without a question.

The script runs in one interpreter, with the phone-harness helpers already
in scope and these on top of them:

  PARAMS       this run's values: the saved defaults with the run's laid over
  ROUTINE_DIR  the routine's own folder, for anything it writes
  checkpoint(name, text=..., any_of=..., app=..., timeout=10, exact=False)
               poll until the screen shows it; a miss saves a screenshot,
               reports what was on screen and stops the run
  step(name)   mark a phase
  done(**summary)  report the outcome; the last event of a good run

Every helper is guarded: a run stops, and says so, at Android's own
permission dialog, and -- with ask_before_apps on -- in an app the operator
has not allowed.

The result of a run is JSON: success, run_id, version, ok, exit_code,
duration_s, the events (checkpoint, step, guard, done), failed_checkpoint
with the name, the expected screen, seen_text and the screenshot path,
output_tail, stderr_tail, and error. A screenshot path is one (look PATH)
shows.

/phone-autopilot lists the routines; show, run, delete, approve and deny
name one; apps, allow and revoke move the allowlist; check runs the
runner's --version.

Settings, in the phone-autopilot section of the config:
  command           how phone-harness runs here: phone-harness, uvx phone-harness
  directory         where routines and runs live
  approve           ask the operator before code is saved or run; on
  ask_before_apps   with the allowlist, keep routines out of other apps; off
  allowed_apps      Android package names a routine may use
  timeout_seconds   the default bound for one run, in seconds

One phone, one run: a run holds the phone's lock, and a second one started
meanwhile says the phone is busy.") 

;;; --- the cell ----------------------------------------------------------------------

(nle:define-cell phone-autopilot
  (:section ("phone-autopilot")
    (:guide "set command to how phone-harness runs here; next step: /phone-autopilot check; then save a task the phone_exec tool already did once")
    ("command" :string :default "phone-harness"
     :doc "how phone-harness is run here: phone-harness, uvx phone-harness, or python3 -m phone_harness.run")
    ("directory" :directory :default "phone-autopilot"
     :doc "where the routines and their run records live")
    ("approve" :boolean :default t
     :doc "ask the operator to approve a routine's code before it is saved or run; off saves without a question")
    ("ask_before_apps" :boolean :default nil
     :doc "with allowed_apps, stop a run that reaches an app the operator has not allowed")
    ("allowed_apps" :list :default '()
     :doc "Android package names a routine may use when ask_before_apps is on")
    ("timeout_seconds" :integer :default 300 :min 1
     :doc "the default bound for one run, in seconds; a call may ask for another, up to 900"))
  (:tool "routine" (nlk:contract 'routine-call) 'routine-tool)
  (:help :phone-autopilot "phone-autopilot: save a phone task as an approved script, then replay it with no model in the loop" +manual+)
  (:command "phone-autopilot" 'run-slash
            :description "List, run, show, approve or delete phone routines; allow the apps they may use"
            :argument-hint "list | show <name> | run <name> | delete <name> | approve <name> | deny <name> | apps | allow <app> | revoke <app>")
  (:start (lambda ()
            (setf *allowed-apps* (setting :allowed-apps)))))
