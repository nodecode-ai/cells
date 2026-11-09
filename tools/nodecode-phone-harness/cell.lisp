;;;; cell.lisp --- phone-harness as one Nodecode tool: phone_exec.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of phone-harness's Hermes plugin (integrations/hermes). The
;;;; original piped a model-written Python script on stdin to the phone-harness
;;;; package Hermes had installed, with PH_AGENT_WORKSPACE and PH_CLIENT set,
;;;; and handed back what it printed: the exit code, the output, the tail of
;;;; stderr, and the screenshot path found in that output. This folder does the
;;;; same through the runner the operator already has -- `phone-harness' on
;;;; PATH, or whatever the section's `command' names (`uvx phone-harness',
;;;; `python3 -m phone_harness.run') -- with the same two output caps, the same
;;;; 5 to 1800 second bound, the same scrubbing of *_API_KEY / *_TOKEN /
;;;; *_SECRET out of the child's environment, and the same rule for which
;;;; image path in the output counts. What Hermes attached as a native image
;;;; Nodecode shows with its own (look PATH), so the result names the
;;;; screenshot and says how to see it.
;;;;
;;;; The one name a test stubs is *RUN*: it takes the argv, the seconds, the
;;;; directory, the environment and the script, and answers the same three
;;;; values NLK:RUN-BOUNDED does.

(defpackage #:nodecode-phone-harness
  (:use #:cl))

(in-package #:nodecode-phone-harness)

(nlk:define-peripheral phone-harness :not-running t)

;;; --- what the original capped, bounded and looked for --------------------------

(defparameter +output-cap+ 50000
  "Characters of stdout kept, as the original's OUTPUT_CAP.")
(defparameter +stderr-cap+ 4000
  "Characters of the end of stderr kept, as the original's STDERR_CAP.")
(defparameter +default-seconds+ 300
  "A script's bound when neither the call nor the section names one.")
(defparameter +min-seconds+ 5)
(defparameter +max-seconds+ 1800)
(defparameter +check-seconds+ 30
  "The bound on `/phone check', which runs the runner's --version.")
(defparameter +summary-chars+ 140
  "How much of a script the approval question shows.")

(defparameter +image-types+ '("png" "jpg" "jpeg" "webp")
  "What a screenshot may be called; the original's PNG, JPEG and WebP.")

(defparameter +secret-suffixes+ '("_API_KEY" "_TOKEN" "_SECRET")
  "How a variable's name says it is a credential, as the original read them.")

;;; --- the call's boundary ------------------------------------------------------

(nlk:define-contract phone-exec-script
  (:doc "Drive the user's real phone with phone-harness: their iPhone through
the Mac's iPhone Mirroring window, an Android over adb, or a Phone Harness
Cloud phone. `code` runs as Python with the phone helpers pre-imported;
print() whatever you need back. Each call is a fresh interpreter (variables do
not persist); the phone keeps its state. Start with ensure_device(); it covers
iPhone Mirroring, Android, and a cloud phone. Read what is on screen with
screen(). screenshot() prints the image path it saved, and that path is in
this result's screenshots -- (look PATH) shows the image to you. Ask the
operator before anything outward-facing or hard to undo (sending, posting,
buying, deleting, changing settings). Never type passwords, PINs or 2FA codes.
(help :phone) has the helpers.")
  ("code" :text :required t
   :doc "Python using the pre-imported phone-harness helpers. print() what you need back.")
  ("timeout_s" :integer
   :doc "Max seconds for the script, 5 to 1800. Absent: the cell's timeout_seconds (300)."))

;;; --- the one seam a test stubs ------------------------------------------------

(defun run-child (argv &key seconds directory environment input)
  "Run the phone-harness runner through the kernel's own bounded runner."
  (nlk:run-bounded argv :seconds seconds :directory directory :environment environment :input input))

(defparameter *run* #'run-child
  "How the cell starts the runner: (*RUN* ARGV :SECONDS N :DIRECTORY DIR
:ENVIRONMENT LIST :INPUT TEXT) => (VALUES STDOUT STDERR STATUS), STATUS an
exit code, 128 plus a signal's number, or :TIMEOUT. A test binds it.")

(defun attempt (argv seconds directory environment input)
  "The child's (VALUES OUTPUT ERROR-OUTPUT STATUS); one that could not be
started answers :UNLAUNCHED with the reason where the error output goes."
  (handler-case
      (funcall *run* argv :seconds seconds :directory directory
                      :environment environment :input input)
    (error (condition)
      (values nil (format nil "~a" condition) :unlaunched))))

;;; --- the runner, and the child's environment ----------------------------------

(defun runner-argv ()
  "The argv that runs phone-harness here, from the section's command: one
word for a program on PATH, more for `uvx phone-harness' or
`python3 -m phone_harness.run'."
  (nlk:split-words (or (setting :command) "")))

(defun environment-entries ()
  "This process's environment, as NAME=value strings; none when it cannot be
read, which costs the scrub and nothing else."
  (or (ignore-errors (sb-ext:posix-environ)) '()))

(defun secret-variables ()
  "Every variable this process holds whose name ends as a credential's does.
A bare NAME in a child environment removes it (NLK:ENVIRONMENT-WITH), which is
how the scrub is handed on: PATH, HOME and ANDROID_* stay, so adb, iPhone
Mirroring and a cloud phone still find what they need."
  (loop for entry in (environment-entries)
        for end = (or (position #\= entry) (length entry))
        for name = (subseq entry 0 end)
        when (some (lambda (suffix) (uiop:string-suffix-p (string-upcase name) suffix))
                   +secret-suffixes+)
          collect name))

(defun child-environment (workspace)
  "This process's environment with the phone-harness variables this cell sets
laid over it and every credential-suffixed variable removed."
  (append (list (format nil "PH_AGENT_WORKSPACE=~a" (uiop:native-namestring workspace))
                "PH_CLIENT=nodecode"
                (format nil "PHONE_HARNESS_TELEMETRY=~a" (if (setting :telemetry) "1" "0")))
          (unless (equal "auto" (setting :platform))
            (list (format nil "PHONE_HARNESS_PLATFORM=~a" (setting :platform))))
          (secret-variables)))

(defun bounded-seconds (asked)
  "ASKED seconds as a bound between +MIN-SECONDS+ and +MAX-SECONDS+."
  (let ((seconds (or (and asked (plusp asked) asked)
                     (setting :timeout-seconds)
                     +default-seconds+)))
    (max +min-seconds+ (min (truncate seconds) +max-seconds+))))

(defun tail (text limit)
  "The end of TEXT that fits in LIMIT characters."
  (let ((text (or text "")))
    (if (> (length text) limit) (subseq text (- (length text) limit)) text)))

;;; --- the screenshot the call saved --------------------------------------------

(defun absolute-path-p (path)
  "T for /x, \\\\x, C:/x and C:\\x: a path no working directory completes."
  (or (and (plusp (length path)) (member (char path 0) '(#\/ #\\) :test #'char=))
      (and (> (length path) 2)
           (alpha-char-p (char path 0))
           (char= (char path 1) #\:)
           (member (char path 2) '(#\/ #\\) :test #'char=))))

(defun image-type-p (path)
  "T when PATH's file name ends in one of +IMAGE-TYPES+."
  (let* ((slash (max (or (position #\/ path :from-end t) -1)
                      (or (position #\\ path :from-end t) -1)))
         (dot (position #\. path :from-end t)))
    (and dot (> dot slash) (> dot 0)
         (member (string-downcase (subseq path (1+ dot))) +image-types+ :test #'equal))))

(defun printed-images (output)
  "Every word of OUTPUT that reads as an absolute image path, trimmed of the
punctuation a sentence wraps a path in."
  (loop for word in (nlk:split-words output)
        for path = (string-trim '(#\Space #\Tab #\Newline #\" #\' #\( #\) #\[ #\]
                                  #\< #\> #\, #\; #\: #\.)
                                word)
        when (and (absolute-path-p path) (image-type-p path))
          collect path))

(defun screenshot (output since)
  "The last absolute image path OUTPUT printed that exists and was written
during this call -- SINCE, in universal time -- with a second of slack for a
file system that keeps whole seconds."
  (loop for path in (reverse (printed-images output))
        for file = (ignore-errors (probe-file path))
        for written = (and file (ignore-errors (file-write-date file)))
        when (and written (>= written (1- since)))
          return path))

;;; --- the result a model reads -------------------------------------------------

(defun payload (success &key exit-code output stderr screenshot workspace)
  "The result, in the original's shape: success, exit_code, output, stderr,
screenshots and workspace."
  (nlk:encode-json-object
   (nlk:json-object
    "success" (if success t :false)
    :opt "exit_code" exit-code
    :opt "output" output
    :opt "stderr" stderr
    :opt "screenshots" (and screenshot (vector screenshot))
    :opt "note" (and screenshot
                      (format nil "The screenshot saved during this call: (look ~a) shows it to you."
                              screenshot))
    :opt "workspace" workspace)))

(defun refusal (control &rest arguments)
  "A call that did not run, or did not finish, as the JSON a model reads,
recorded as the call's failure."
  (nle:failure
   (nlk:encode-json-object
    (nlk:json-object "success" :false
                     "error" (apply #'format nil control arguments)))))

;;; --- asking first -------------------------------------------------------------

(defvar *allowed-sessions* (make-hash-table :test #'equal)
  "Sessions whose operator answered `Allow for this session': the original's
one answer for a session, kept here rather than by the host.")

(defun session-key ()
  (or (and (nle:turn) (getf (nle:turn) :session-id)) "no-session"))

(defun one-line (text &optional (limit +summary-chars+))
  "TEXT with its blank runs collapsed to one space, cut to LIMIT characters."
  (let ((line (format nil "~{~a~^ ~}" (nlk:split-words text))))
    (cond ((zerop (length line)) "(empty)")
          ((> (length line) limit) (concatenate 'string (subseq line 0 (- limit 3)) "..."))
          (t line))))

(defun allowed-p (code)
  "Whether CODE may run now: (VALUES T), or (VALUES NIL WHY). Ask is off by
default; with it on, the question goes to whoever answers questions on a
surface attached to this session, and an answer of `Allow for this session'
covers the rest of that session."
  (if (or (not (setting :ask)) (gethash (session-key) *allowed-sessions*))
      (values t)
      (multiple-value-bind (choice state)
          (nle:ask-operator (format nil "phone_exec: ~a" (one-line code))
                            :detail "A model-written Python script, run against your phone with the phone-harness helpers."
                            :choices '("Allow once" "Allow for this session" "Deny")
                            :seconds 120)
        (cond ((null choice) (values nil (if (eq state :nobody) :nobody :timeout)))
              ((equal choice "Deny") (values nil :denied))
              (t (when (equal choice "Allow for this session")
                   (setf (gethash (session-key) *allowed-sessions*) t))
                 (values t))))))

(defun untried-reason (why)
  "What a call that was not approved says."
  (ecase why
    (:nobody "phone_exec asked before running the script, and nobody answered: no attached surface answers questions. Answer from the web page, or set the phone-harness section's \"ask\" to false to run scripts without a question.")
    (:timeout "phone_exec asked before running the script, and the question went unanswered.")
    (:denied "phone_exec did not run: the operator denied the script.")))

;;; --- the tool -----------------------------------------------------------------

(defun run-phone-exec (script)
  "phone_exec: SCRIPT's code, run on the phone through the runner this cell's
command names. => the JSON a model reads."
  (let ((code (phone-exec-script-code script)))
    (cond
      ((null (runner-argv))
       (refusal "phone_exec has no runner to start: set the phone-harness section's \"command\" to how phone-harness runs here, e.g. \"phone-harness\" or \"uvx phone-harness\"."))
      ((zerop (length (nlk:trimmed code)))
       (refusal "No code provided. Start with: ensure_device(); print(screen_info())"))
      (t
       (multiple-value-bind (allowed why) (allowed-p code)
         (if (not allowed)
             (refusal "~a" (untried-reason why))
             (let* ((argv (runner-argv))
                    (workspace (setting :workspace))
                    (bound (bounded-seconds (phone-exec-script-timeout-s script)))
                    (started (get-universal-time))
                    ;; A workspace phone-harness has not made yet is left
                    ;; unmade: it seeds its own helpers into one it creates,
                    ;; and never touches one that is already there.
                    (directory (and (uiop:directory-exists-p workspace) workspace)))
               (multiple-value-bind (out err status)
                   (attempt argv bound directory (child-environment workspace) code)
                 (cond
                   ((eq status :unlaunched)
                    (refusal "phone_exec could not start ~{~a~^ ~}: ~a. Install it (`uv tool install phone-harness`) or set the phone-harness section's command to how it runs here." argv err))
                   ((eq status :timeout)
                    (refusal "phone_exec timed out after ~ds. Split the work into smaller scripts, or raise timeout_s (max ~d)." bound +max-seconds+))
                   (t
                    (let* ((output (tail out +output-cap+))
                           (errors (nlk:trimmed (tail err +stderr-cap+)))
                           (shot (screenshot output started)))
                      (payload (eql status 0)
                               :exit-code status
                               :output output
                               :stderr errors
                               :screenshot shot
                               :workspace (uiop:native-namestring workspace)))))))))))))

;;; --- /phone -------------------------------------------------------------------

(defun run-slash (args session-id)
  "`/phone' answers what the tool runs on and where it keeps its notes;
`/phone check' asks the runner for its version."
  (declare (ignore session-id))
  (let ((argv (runner-argv))
        (workspace (setting :workspace))
        (check (equal "check" (string-downcase (nlk:trimmed (or args ""))))))
    (with-output-to-string (out)
      (format out "phone_exec~@[ - ~{~a~^ ~}~]~%" argv)
      (format out "platform ~a - ask ~a - telemetry ~a~%"
              (setting :platform) (if (setting :ask) "on" "off")
              (if (setting :telemetry) "on" "off"))
      (format out "workspace ~a~%" (uiop:native-namestring workspace))
      (cond
        ((null argv)
         (format out "no command set: put how phone-harness runs here in the section's command~%"))
        (check
         (multiple-value-bind (text errors status)
             (attempt (append argv (list "--version")) +check-seconds+ nil nil "")
           (if (eq status :unlaunched)
               (format out "~a did not start: ~a~%install it with `uv tool install phone-harness', or set command to `uvx phone-harness'~%"
                       (first argv) errors)
               (format out "~a --version => ~a~@[ ~a~]~%"
                       (first argv) status (nlk:trimmed (format nil "~a~@[ ~a~]" text errors))))))
        (t (format out "`/phone check` runs ~{~a~^ ~} --version~%" argv)))
      (format out "helpers: ensure_device(), screen(), tap_text(), type_text(), open_app(), screenshot() - `phone-harness skill' prints the whole list, (help :phone) the rest.~%"))))

;;; --- the manual ---------------------------------------------------------------

(defparameter +manual+
  "phone_exec runs one Python script against the operator's phone: an iPhone
through the Mac's iPhone Mirroring window, an Android over adb, or a Phone
Harness Cloud phone. The script goes on the runner's stdin with the
phone-harness helpers already imported.

  ensure_device()      screen bounds once the phone is usable, or raise
  screen()             visible text as {text, confidence, source, x, y, w, h}
  find_text(q)  tap_text(q)  tap(x, y)  long_press(x, y)
  type_text(t)  press(combo)  swipe(dir)  scroll(dir)  scroll_until(done)
  open_app(name)  open_url(url)  push_file(path)
  home()  back()  app_switcher()  wait_stable()  wait_for_text(q)
  ui()  tap_ui(q)  current_app()  list_apps()  screen_info()  shell(cmd)
  screenshot(path=None)   saves a PNG, prints its path, and returns it

Each call is a fresh interpreter, so variables do not carry over; the phone
keeps its state. Start with print(ensure_device()), or with
print(screen_info()) to see whether a phone is reachable at all.
`phone-harness skill' prints the packaged skill: which phone to drive, the
working method, and every helper with its arguments.

The result is JSON: success, exit_code, output, stderr (its end), the
screenshots found in the output, and the workspace. A screenshot saved during
the call is a path in that list; (look PATH) shows it to you, and the operator
sees it under the call. A script past its bound is killed with everything it
started.

Settings, in the phone-harness section of the config:
  command          how the runner is run here: phone-harness, uvx phone-harness,
                   python3 -m phone_harness.run
  platform         auto (phone-harness chooses), ios or android
  workspace        where the helpers and any notes live; PH_AGENT_WORKSPACE
  ask              ask before each script; off, and only a surface that
                   answers questions can answer it
  telemetry        let phone-harness send its own telemetry; off unless on
  timeout_seconds  the default bound for a script, 5 to 1800

iPhone control needs Accessibility and Screen Recording for the app that runs
this image; Android needs adb; a cloud phone needs `phone-harness cloud login'.
Never type a password, PIN or 2FA code into the phone: hand it back to the
operator for those. /phone answers the state, /phone check the runner's
version.")

;;; --- the cell -----------------------------------------------------------------

(nle:define-cell phone-harness
  (:section ("phone-harness")
    (:guide "install phone-harness, then set command to how it runs here; next step: /phone check; then ask for something on your phone")
    ("command" :string :default "phone-harness"
     :doc "how the runner is run here: phone-harness, uvx phone-harness, or python3 -m phone_harness.run")
    ("platform" :choice :options '("auto" "ios" "android") :default "auto"
     :doc "which phone to drive; auto lets phone-harness choose, ios and android override it")
    ("workspace" :directory :default "phone-harness/agent-workspace"
     :doc "where the helpers and any notes live; passed as PH_AGENT_WORKSPACE")
    ("ask" :boolean :default nil
     :doc "ask before each script runs; needs a surface that answers questions")
    ("telemetry" :boolean :default nil
     :doc "let phone-harness send its own telemetry (script text, output tail); off unless on")
    ("timeout_seconds" :integer :default 300 :min 5
     :doc "the default bound for one script, in seconds; a call may ask for less, up to 1800"))
  (:tool "phone_exec" (nlk:contract 'phone-exec-script) 'run-phone-exec)
  (:help :phone "phone: drive a real iPhone, Android or cloud phone - phone_exec runs one Python script of phone-harness helpers on it" +manual+)
  (:command "phone" 'run-slash
            :description "What the phone tool runs on, and /phone check"
            :argument-hint "check")
  (:start (lambda ()
            (clrhash *allowed-sessions*))))
