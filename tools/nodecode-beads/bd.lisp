;;;; bd.lisp --- the fixed-argv boundary to the bd (Beads) CLI.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One spawn point, as the plugin had one: a fixed argv list, no shell, an
;;;; explicit working directory (the store), an explicit --actor on every
;;;; mutation, and bd's exit codes read honestly rather than folded into an
;;;; empty result:
;;;;
;;;;   0  ok                  1  general failure
;;;;  13  a guarded write lost its race -- nothing written, never retried
;;;;   2  --max-rows tripped  -- a breached bound, NOT "nothing ready"
;;;;
;;;; Before any spawn, ARGV-OK-P refuses a token bd could read as a flag of
;;;; its own (an id like "--db=/x", a label like "-C/etc"): the value slots
;;;; this cell fills are positional or follow a flag that takes one, and
;;;; nothing else may start with a dash. The program is the operator's
;;;; setting, never a tool argument, so a model cannot choose argv[0].

(in-package #:nodecode-beads)

;;; --- what bd may be asked ---------------------------------------------------------

(defparameter *value-flags*
  '("-n" "--max-rows" "--limit" "--label" "--status" "--parent" "--reason"
    "--if-assignee" "--if-status" "--priority" "--notes" "--append-notes"
    "--title" "--description" "--due" "--estimate" "--external-ref" "--defer"
    "--metadata-field" "--set-metadata" "--unset-metadata")
  "The flags this cell sends, each followed by its one value.")

(defparameter *boolean-flags*
  '("--json" "--all" "--claim" "--dry-run" "--exclude-type=epic")
  "The flags this cell sends that take no value.")

(defparameter *id-slot*
  '(("show" . 1) ("update" . 1) ("close" . 1) ("reopen" . 1) ("heartbeat" . 1)
    ("unclaim" . 1) ("history" . 1) ("query" . 1) ("children" . 1)
    ("comments" . 1) ("epic" . 1))
  "The verbs whose second token is an id: it must not start with a dash, or
`update --claim --json` could be made to mean something else.")

(defun dash-leading-p (token)
  "Whether TOKEN starts with a dash (a flag to a getopt-style parser)."
  (and (stringp token) (plusp (length token)) (char= #\- (char token 0))))

(defun gate-tokens (argv)
  "Whether ARGV's dash-leading tokens are only flags this cell sends, with
every value-taking flag's value bound to the slot right after it."
  (let ((expect-value nil) (positional nil) (ok t))
    (dolist (token argv)
      (cond ((not ok))
            ((not (stringp token)) (setf ok nil))
            ((find (code-char 0) token) (setf ok nil))
            ((or expect-value positional) (setf expect-value nil))
            ((string= token "--") (setf positional t))
            ((dash-leading-p token)
             (cond ((member token *value-flags* :test #'string=) (setf expect-value t))
                   ((member token *boolean-flags* :test #'string=))
                   (t (setf ok nil))))))
    (and ok (not expect-value))))

(defun argv-ok-p (argv)
  "Whether ARGV is a verb this cell sends, with no caller value that could be
read by bd as a flag of its own. Pure: it runs before any spawn."
  (when (and (listp argv) (plusp (length argv))
             (stringp (first argv)) (not (dash-leading-p (first argv))))
    (let* ((verb (first argv))
           (at (if (member verb '("comments" "epic") :test #'string=)
                   (if (member (second argv) '("add" "status") :test #'string=) 2 1)
                   (cdr (assoc verb *id-slot* :test #'string=)))))
      (and (or (null at)
               (and (> (length argv) at)
                    (stringp (nth at argv))
                    (not (dash-leading-p (nth at argv)))))
           (gate-tokens argv)))))

;;; --- the store the call names -----------------------------------------------------

(defun store-problems (workspace)
  "What is wrong with WORKSPACE as a bd store directory, or NIL when nothing is."
  (let ((dir (and (stringp workspace)
                  (plusp (length workspace))
                  (ignore-errors (uiop:ensure-directory-pathname workspace)))))
    (cond ((null dir) "it is not a path")
          ((not (uiop:absolute-pathname-p dir)) "it is not an absolute path")
          ((not (uiop:directory-exists-p dir)) "there is no such directory")
          ((not (uiop:directory-exists-p (merge-pathnames ".beads/" dir)))
           "there is no .beads/ store in it -- run `bd init' there first")
          (t nil))))

(defun check-store (workspace)
  "WORKSPACE as a directory pathname, or a refusal naming what is wrong."
  (let ((problem (store-problems workspace)))
    (when problem
      (fail "~s cannot be used as a bd store: ~a" workspace problem))
    (uiop:ensure-directory-pathname workspace)))

;;; --- the program -----------------------------------------------------------------

(defun program-path (name)
  "Where the program NAME is: as a path when it names one, else the first
file of that name on PATH; NIL when there is none."
  (when (and (stringp name) (plusp (length name)))
    (flet ((file (dir)
             (ignore-errors
               (probe-file (merge-pathnames name
                                             (uiop:ensure-directory-pathname
                                              (if (plusp (length dir)) dir ".")))))))
      (cond ((or (find #\/ name) (find #\\ name)) (ignore-errors (probe-file name)))
            (t (loop for dir in (uiop:split-string (or (uiop:getenv "PATH") "")
                                                   :separator '(#\: #\;))
                     for found = (file dir)
                     when found return found))))))

(defun bd-program ()
  "The bd program's path or name, as the operator configured it."
  (or (program-path (setting :bd)) (setting :bd)))

;;; --- one run ----------------------------------------------------------------------

(defun call-bd (argv directory seconds)
  "Run the whole ARGV in DIRECTORY, at most SECONDS => (values EXIT STDOUT STDERR)."
  (handler-case
      (multiple-value-bind (stdout stderr status)
          (nlk:run-bounded argv :directory directory :seconds seconds
                                :output :string :error-output :string)
        (values (cond ((eql :timeout status) :timeout)
                      ((integerp status) status)
                      (t 1))
                (or stdout "") (or stderr "")))
    (error (condition)
      (fail "cannot run ~s: ~a" (first argv) condition))))

(defvar *run* #'call-bd
  "The one seam that runs bd: (ARGV DIRECTORY SECONDS) => (values EXIT STDOUT
STDERR). Tests replace it; nothing else does.")

(defun clip (text limit)
  "TEXT at most LIMIT characters, so a loud stderr never floods a turn."
  (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return) (or text ""))))
    (if (> (length text) limit) (subseq text 0 limit) text)))

(defun bd-run (prefix &key directory actor readonly (expect-json t))
  "One bd invocation: PREFIX's tokens after the program and the globals this
cell binds (--readonly for a read, --actor for a mutation). => (values PARSED
EXIT STDOUT STDERR); PARSED is NIL when EXPECT-JSON is off or stdout blank."
  (unless (argv-ok-p prefix)
    (fail "~s was refused before running: a value in it could be read by bd as a flag" prefix))
  (let* ((dir (check-store directory))
         (argv (append (list (bd-program))
                       (when readonly (list "--readonly"))
                       (when actor (list "--actor" actor))
                       prefix)))
    (multiple-value-bind (exit stdout stderr)
        (funcall *run* argv dir (setting :timeout-seconds))
      (cond ((eql :timeout exit)
             (fail "bd did not answer within ~a seconds: ~{~a~^ ~}"
                   (setting :timeout-seconds) prefix))
            ((and (eql exit 0) expect-json (plusp (length (clip stdout 1))))
             (let ((parsed (handler-case (nlk:decode-json (string-trim '(#\Space #\Newline #\Tab) stdout))
                              (error () :unparsed))))
               (when (eq parsed :unparsed)
                 (fail "bd printed something that is not JSON on ~{~a~^ ~}: ~a"
                       prefix (clip stdout 300)))
               (values parsed exit stdout stderr)))
            (t (values nil exit stdout stderr))))))

(defun bd-ok (parsed exit prefix stdout stderr)
  "PARSED when bd exited 0; otherwise the named refusal for EXIT."
  (cond ((eql exit 0) parsed)
        ((eql exit 2)
         (fail "bd stopped at its --max-rows bound (exit 2) on ~{~a~^ ~}: the frontier is bigger than the bound, not empty" prefix))
        ((eql exit 13)
         (fail "bd exited 13 on ~{~a~^ ~}: a guard no longer held; nothing was written" prefix))
        (t (fail "bd exited ~a on ~{~a~^ ~}: ~a" exit prefix
                 (or (clip (concatenate 'string stderr " " stdout) 400) "(no output)")))))

;;; --- reading bd's answer -----------------------------------------------------------

(defun row-text-field (row key)
  "ROW's KEY when it is a string, else NIL."
  (nlk:json-value row :string key))

(defun rows-of (parsed what)
  "PARSED as a list of rows: bd answers a list of objects, or one object for
the single-id verbs; anything else is refused rather than read as empty."
  (cond ((null parsed) nil)
        ((vectorp parsed) (coerce parsed 'list))
        ((listp parsed) parsed)
        ((hash-table-p parsed) (list parsed))
        (t (fail "~a: bd's JSON is neither a row nor a list of rows" what))))
