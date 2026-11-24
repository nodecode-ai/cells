;;;; cell.lisp --- the local app store: a catalogue, a folder of apps, processes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the verbs through the manual: while the cell runs,
;;;; (help :athena) answers the primer below, and every request's help
;;;; section carries one line naming it. An operator can call the same verbs
;;;; from eval in the ATHENA package.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "athena": {"apps_dir": "~/nodecode-apps",
;;;;              "catalog_url": "https://api.pinokio.co/apps"}
;;;; A vetoed section ("enabled": false) installs nothing; a section without
;;;; apps_dir refuses at start, because every verb needs the one folder.
;;;;
;;;; Ported from athenaforhermes's Hermes Agent plugin (see NOTICE): the
;;;; catalogue read, install by clone, the launch, stop and log loop, the
;;;; reading of a log that says why an app failed, and the shapes the tools
;;;; answer with. The Hermes plugin drove its own Node engine; here the one
;;;; seam is a program the person already has (git, sh, node), and the app's
;;;; own output is the log.

(in-package #:nodecode-athena)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Athena is the local app store, through the nodecode-athena cell: seven tools, which a
person can also call from eval in the athena package.
  athena_search  {query, limit}
      Search the whole store catalogue for installable AI apps (image upscalers, voice
      cloners, LLM front ends, agent projects). Answers each app's name, repo, version, a
      short description, its tags and an install_with handle to hand to athena_install.
  athena_apps  {}
      The apps installed on this machine: id, folder, whether it is running, its process id
      and the URL its web UI answers on. Read this before launching, reading a log of or
      stopping anything. An empty answer means nothing is installed yet.
  athena_install  {url}
      Clone a git repository into the apps folder: a full URL, an owner/repo shorthand, or
      a catalogue app name, which is resolved to its repository first. The app's own code
      then runs as you, with your files and your privileges; nothing here sandboxes it.
  athena_launch  {app, script}
      Run the app's own start script (start.sh, start.js, start.py or run.sh, whichever the
      folder has) in the app's folder, in the background, its output going to the app's
      log. A cold start often needs ten seconds or more to bind a port: a missing url means
      still booting, not failed. Pass script to run another command instead.
  athena_stop  {app}
      End the process this cell started for an app, freeing its port.
  athena_logs  {app, lines, why}
      The app's output, and a reading of it: the named cause (the GPU's memory, the
      machine's memory, a port already taken, a missing package, the install script never
      run, a credential the app did not get, the network) with the action that answers it.
      why=1 answers only the reading, the fastest way to answer \"why did this fail\".
  athena_status  {}
      The apps folder, the catalogue it reads and how many apps are installed and running.
Logs live beside the apps, at .athena/<app>.log, and are kept after a stop, so a failure
can be read after the fact. An install only clones: when the app has an install step of its
own (npm install, pip install -r requirements.txt), the reading says so if the start fails."
  "What (help :athena) answers while the cell runs.")

;;; --- answers, and the one condition ---------------------------------------------

(defun refuse (format-control &rest args)
  "End the call here: ATHENA-ERROR carries the words, and ANSWERING answers them
as the call's failure, so the result fact records it failed and every transcript
draws it so."
  (apply #'fail format-control args))

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (athena-error (condition)
       (nle:failure (format nil "ERROR: ~a" (athena-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: Athena: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun nonblank (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

;;; --- the seams: a program the machine has, stubbed in the tests ------------------


(defun %run (argv &key directory)
  "Run ARGV to completion => (values OUTPUT EXIT-CODE)."
  (let* ((process (uiop:launch-program argv
                                       :directory directory
                                       :output :stream
                                       :error-output :output
                                       :ignore-error-status t))
         (output (uiop:slurp-stream-string (uiop:process-info-output process)))
         (code (uiop:wait-process process)))
    (values (or output "") (or code -1))))

(defun %pid-alive-p (pid)
  "Whether PID names a live process. A machine with no kill(1) answers no."
  (and (integerp pid)
       (handler-case (zerop (nth-value 1 (funcall *run* (list "kill" "-0" (format nil "~d" pid)))))
         (error () nil))))

(defparameter +shell-metacharacters+ '(#\| #\; #\& #\> #\<)
  "A command carrying one of these is run by sh as it is; a simple one is
EXECed, so the pid this cell keeps is the app's own and a stop reaches it.")

(defun shell-quote (text)
  "TEXT as one shell word."
  (format nil "'~a'" (ppcre:regex-replace-all "'" text "'\\\\''")))

(defun %launch (command directory log)
  "Start COMMAND in DIRECTORY, its output appended to LOG => (values PROCESS PID)."
  (let* ((simple (notany (lambda (character) (find character command))
                         +shell-metacharacters+))
         (inner (if simple (format nil "exec ~a" command) command))
         (line (format nil "~a >> ~a 2>&1" inner (shell-quote (namestring log))))
         (process (uiop:launch-program (list "sh" "-c" line)
                                        :directory directory
                                        :output :stream
                                        :error-output :output
                                        :ignore-error-status t)))
    (values process (uiop:process-info-pid process))))

(defun %alive (process)
  (and process (uiop:process-alive-p process)))

(defun %terminate (process)
  "SIGTERM, a moment to go, then SIGKILL: whether the process is gone."
  (when process
    (ignore-errors (uiop:terminate-process process :urgent nil))
    (loop repeat 20
          do (unless (uiop:process-alive-p process) (return t))
             (sleep 0.1))
    (when (uiop:process-alive-p process)
      (ignore-errors (uiop:terminate-process process :urgent t))
      (loop repeat 10
            do (unless (uiop:process-alive-p process) (return t))
               (sleep 0.1))))
  (not (and process (uiop:process-alive-p process))))

;;; --- the seams, bound to what runs them --------------------------------------------

(defvar *run* #'%run
  "ARGV to completion => (values OUTPUT EXIT-CODE): the one way this cell runs a
program and waits for it (git clone, and the kill -0 that asks whether a pid
from an earlier process still lives).")

(defvar *launch* #'%launch
  "COMMAND DIRECTORY LOG => (values PROCESS PID): start COMMAND in DIRECTORY in
the background, its output appended to LOG.")

(defvar *alive* #'%alive
  "PROCESS => whether the process a launch answered still runs.")

(defvar *pid-alive* #'%pid-alive-p
  "PID => whether a number names a live process: an app started before this
process began is known by its pid file alone.")

(defvar *terminate* #'%terminate
  "PROCESS => whether the process is gone: SIGTERM, then SIGKILL if it lingers.")

(defvar *running* (make-hash-table :test #'equal)
  "App id -> (:process P :pid N :since TIME), the apps this cell started. A
pid file beside the log carries the same fact across a restart.")

;;; --- where apps live ------------------------------------------------------------

(defun apps-root ()
  "The apps folder, as an absolute directory pathname."
  (uiop:ensure-directory-pathname (nlk:expand-home (setting :apps-dir))))

(defun state-dir ()
  "Where this cell keeps an app's log and its pid: beside the apps, not in one."
  (merge-pathnames ".athena/" (apps-root)))

(defun log-path (id)
  (merge-pathnames (format nil "~a.log" id) (state-dir)))

(defun pid-path (id)
  (merge-pathnames (format nil "~a.pid" id) (state-dir)))

(defun read-settings (values table)
  "The declared VALUES, checked once: every verb needs the one folder."
  (declare (ignore table))
  (let ((apps (nonblank (getf values :apps-dir)))
        (url (nonblank (getf values :catalog-url)))
        (plist (copy-list values)))
    (unless apps
      (nlk:config-error "apps_dir is not set: name the folder apps are installed into and run from, for example nodecode-apps in your home folder"))
    (setf (getf plist :apps-dir)
          (namestring (uiop:ensure-directory-pathname (nlk:expand-home apps))))
    (unless (or (uiop:string-prefix-p "http://" url) (uiop:string-prefix-p "https://" url))
      (nlk:config-error "catalog_url ~s has no http:// or https:// scheme: set it to the store's JSON API, e.g. \"https://api.pinokio.co/apps\"" url))
    (setf (getf plist :catalog-url) url)
    plist))

;;; --- reading the catalogue ------------------------------------------------------

(defparameter +page-size+ 100
  "The catalogue API's own per-page ceiling.")

(defparameter +answer-cap+ 60
  "The most apps one search answers, however large the limit asked for.")

(defun url-encode (text)
  "TEXT as a query-string value: a space is %20, not a truncation."
  (with-output-to-string (out)
    (loop for character across text
          for code = (char-code character)
          do (cond ((or (alphanumericp character) (find character "-_.~"))
                    (write-char character out))
                   (t (format out "%~2,'0X" code))))))

(defun catalogue-url (query limit)
  (format nil "~a?page=1&perPage=~d~@[&q=~a~]&sort=trending"
          (setting :catalog-url)
          (min (max limit 1) +page-size+)
          (and query (plusp (length query)) (url-encode query))))

(defun catalogue-fetch (query limit)
  "One page of the catalogue, decoded, or a refusal carrying the store's own words."
  (let ((url (catalogue-url query limit)))
    (multiple-value-bind (body status)
        (handler-case
            (nlk:http :get url
                      :headers '(("accept" . "application/json")
                                 ("user-agent" . "nodecode-athena/0.1"))
                      :timeout (setting :timeout)
                      :connect-timeout (setting :timeout))
          (error (condition)
            (fail "the store at ~a did not answer: ~a" url
                  (nlk:one-line (princ-to-string condition)))))
      (let ((text (if (stringp body) body (nlk:body-text body))))
        (unless (and (integerp status) (< status 400))
          (fail "the store at ~a answered HTTP ~a~@[: ~a~]" url status
                (and (plusp (length (nlk:trimmed (or text ""))))
                     (nlk:clip (nlk:one-line text) 200))))
        (or (ignore-errors (nlk:decode-json text))
            (fail "the store at ~a answered something that is not JSON" url))))))

(defun catalogue-record (item)
  "One catalogue row as a table: the API sometimes wraps it as {\"app\": {...}}."
  (let ((record (if (and (hash-table-p item) (nlk:json-value item :object "app"))
                    (nlk:json-value item :object "app")
                    item)))
    (and (hash-table-p record) record)))

(defun catalogue-apps (payload limit)
  "The rows of PAYLOAD, de-duplicated by slug, at most LIMIT of them."
  (let ((rows (and (hash-table-p payload) (nlk:json-array payload "items")))
        (seen (make-hash-table :test #'equal))
        (apps '()))
    (when rows
      (loop for item across rows
            for record = (catalogue-record item)
            for slug = (and record (nlk:json-value record :string "slug"))
            while (< (length apps) limit)
            do (unless (and slug (gethash slug seen))
                 (when (and slug (plusp (length slug))) (setf (gethash slug seen) t))
                 (when record (push record apps)))))
    (nreverse apps)))

(defun tag-names (record)
  "A catalogue row's tags, as names: the API answers [{tag, count}, ...]."
  (let ((tags (nlk:json-array record "tags")))
    (if tags
        (loop for tag across tags
              for name = (if (hash-table-p tag)
                             (nlk:json-value tag :string "tag")
                             tag)
              when (and (stringp name) (plusp (length name)))
                collect name)
        '())))

(defun app-answer (record)
  "One catalogue row as the model reads it, which is athena_search's shape."
  (let* ((slug (nlk:json-value record :string "slug"))
         (repo (nlk:json-value record :string "repoUrl"))
         (title (or (nlk:json-value record :string "title") slug)))
    (nlk:json-object
     "name" (or title "")
     "slug" (or slug "")
     "repo" (or repo "")
     "version" (or (nlk:json-value record :string "version") "")
     "description" (nlk:clip (or (nlk:json-value record :string "description") "") 300)
     "tags" (coerce (tag-names record) 'vector)
     "install_with" (or repo slug ""))))

;;; --- the folder -----------------------------------------------------------------

(defun app-ids ()
  "Every app installed in the apps folder: its subdirectories, by name."
  (let ((root (apps-root)))
    (sort (when (uiop:directory-exists-p root)
            (loop for dir in (uiop:subdirectories root)
                  for name = (car (last (pathname-directory dir)))
                  unless (or (null name) (char= #\. (char name 0)))
                    collect name))
          #'string<)))

(defun app-folder (id)
  (merge-pathnames (format nil "~a/" id) (apps-root)))

(defun installed-p (id)
  (and (nonblank id) (uiop:directory-exists-p (app-folder id))))

(defun app-record (id)
  (gethash id *running*))

(defun app-pid (id)
  "The pid this cell started ID as, from this process or from its own pid file."
  (or (getf (app-record id) :pid)
      (let ((path (pid-path id)))
        (when (probe-file path)
          (let ((text (nlk:trimmed (or (ignore-errors (uiop:read-file-string path)) ""))))
            (and (plusp (length text)) (parse-integer text :junk-allowed t)))))))

(defun app-running-p (id)
  "Whether ID runs: the process this cell holds, or the pid its file names."
  (let ((process (getf (app-record id) :process)))
    (if process
        (and (funcall *alive* process) t)
        (let ((pid (app-pid id)))
          (and pid (funcall *pid-alive* pid) t)))))

(defun forget-app (id)
  (remhash id *running*)
  (ignore-errors (delete-file (pid-path id))))

(defun remember-app (id process pid)
  "Keep the handle this process holds, and the pid a later process can read."
  (setf (gethash id *running*) (list :process process :pid pid :since (get-universal-time)))
  (ensure-directories-exist (pid-path id))
  (with-open-file (out (pid-path id) :direction :output :if-exists :supersede
                                     :if-does-not-exist :create)
    (format out "~a~%" pid)))

(defun app-url (id)
  "The loopback URL the app's log last named: what its web UI answers on, or NIL."
  (let ((text (read-log-text id)))
    (when text
      (ppcre:scan-to-strings "http://(?:127\\.0\\.0\\.1|localhost|0\\.0\\.0\\.0):[0-9]{2,5}" text))))

(defun app-row (id)
  (let ((running (app-running-p id)))
    (nlk:json-object
     "app" id
     "folder" (namestring (app-folder id))
     "running" (and running t)
     "pid" (or (and running (app-pid id)) 0)
     "url" (or (and running (app-url id)) "")
     "log" (namestring (log-path id)))))

;;; --- the log, and what it says --------------------------------------------------

(defparameter +log-tail-bytes+ 262144
  "How much of the end of a log is read: an app can log for hours.")

(defun read-log-text (id)
  "The last stretch of ID's log, or NIL when it has none."
  (let* ((path (log-path id))
         (text (and (probe-file path)
                    (ignore-errors (uiop:read-file-string path)))))
    (when text
      (subseq text (max 0 (- (length text) +log-tail-bytes+))))))

(defun tail-lines (text lines)
  "The last LINES of TEXT, or NIL. A trailing newline is not an empty line."
  (when (and text (plusp (length text)))
    (let ((all (uiop:split-string text :separator '(#\Newline))))
      (loop while (and all (string= "" (car (last all))))
            do (setf all (butlast all)))
      (when all
        (format nil "~{~a~^~%~}" (last all (min lines (length all))))))))

(defparameter +readings+
  '(("CUDA out of memory|out of memory on the gpu|CUDA error: out of memory"
     "the GPU ran out of memory"
     "lower the resolution, the batch size or the steps, close another app using the card, or run it where there is more GPU memory")
    ("^Killed$|MemoryError|Cannot allocate memory|bad_alloc"
     "the machine ran out of memory"
     "close other apps, lower the batch size, or add RAM or swap")
    ("Address already in use|EADDRINUSE|port [0-9]+ is already"
     "the port the app wants is already taken"
     "stop the app already on that port with athena_stop, then launch again")
    ("ModuleNotFoundError|No module named"
     "a Python package the app needs is missing"
     "the app's own install step never ran: run it in the app's folder, then launch again")
    ("npm ERR!|Cannot find module|ERR_MODULE_NOT_FOUND"
     "the app's Node packages are missing or broken"
     "run the app's install step in its folder, then launch again")
    ("401 Unauthorized|403 Forbidden|gated|requires authentication|HF_TOKEN"
     "the app asked a service for something it was not allowed to have"
     "the app needs a credential: set it in the app's own settings, or in the environment this cell starts apps from")
    ("Connection refused|Temporary failure in name resolution|ETIMEDOUT|timed out|Could not resolve host"
     "the app could not reach the network"
     "check the connection, then launch again: a weights download resumes where it stopped")
    ("Could not find a version that satisfies|No matching distribution|CUDA driver version is insufficient"
     "a package or build does not match this machine"
     "install the build this machine's card and driver can run")
    ("No space left on device"
     "the disk is full"
     "free space in the apps folder, then launch again"))
  "A log line, the cause it names, and the action that answers it, tried in
order. Case-insensitive: the first match is the reading.")

(defun read-log (text)
  "The named cause of a failure in TEXT, as (cause . fix), or NIL."
  (when text
    (loop for (pattern cause fix) in +readings+
          when (ppcre:scan (ppcre:create-scanner pattern :case-insensitive-mode t
                                                    :multi-line-mode t)
                           text)
            do (return (cons cause fix)))))

;;; --- the verbs -------------------------------------------------------------------

(defun search-store (args)
  "athena_search: the catalogue, asked for QUERY."
  (answering
    (let* ((query (store-search-query args))
           (limit (min (or (store-search-limit args) 20) +answer-cap+))
           (payload (catalogue-fetch query limit))
           (apps (catalogue-apps payload limit)))
      (when (null apps)
        (refuse "the store has no app for ~s: try fewer, plainer words (\"image upscaler\", \"voice clone\")" query))
      (nlk:encode-json-object
       (nlk:json-object
        "query" query
        "count" (length apps)
        "apps" (coerce (mapcar #'app-answer apps) 'vector)
        "note" "pass an app's install_with to athena_install")))))

(defun list-apps (args)
  "athena_apps: the apps installed here, and whether each runs."
  (answering
    (let* ((only (nonblank (app-list-app args)))
           (all (app-ids))
           (ids (if only
                    (remove-if-not (lambda (id) (equal id only)) all)
                    all)))
      (nlk:encode-json-object
       (nlk:json-object
        "count" (length ids)
        "apps_dir" (namestring (apps-root))
        "apps" (coerce (mapcar #'app-row ids) 'vector)
        "note" (if ids
                   "athena_launch starts one; athena_logs reads what it says"
                   "nothing is installed yet: athena_search, then athena_install"))))))

(defun resolve-ref (ref)
  "REF as a git URL: a URL as it is, owner/repo as GitHub, else a catalogue name."
  (cond ((some (lambda (prefix) (uiop:string-prefix-p prefix ref))
               '("http://" "https://" "git@"))
         ref)
        ((ppcre:scan "^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$" ref)
         (format nil "https://github.com/~a" (string-right-trim "/" ref)))
        (t
         (let* ((payload (catalogue-fetch ref 5))
                (apps (catalogue-apps payload 5)))
           (or (loop for record in apps
                     for repo = (nlk:json-value record :string "repoUrl")
                     when repo do (return repo))
               (fail "~s is neither a git URL nor an app the store knows: call athena_search first" ref))))))

(defun app-id-of (url)
  "The folder an app cloned from URL takes: its last path segment."
  (let* ((trimmed (string-right-trim "/" url))
         (name (car (last (uiop:split-string trimmed :separator '(#\/ #\:)))))
         (name (if (and name (uiop:string-suffix-p name ".git"))
                   (subseq name 0 (- (length name) 4))
                   name))
         (clean (string-downcase (or name ""))))
    (if (plusp (length clean))
        (string-downcase (ppcre:regex-replace-all "[^A-Za-z0-9._-]" clean "-"))
        "app")))

(defun install-app (args)
  "athena_install: clone REF into the apps folder."
  (answering
    (let* ((ref (nlk:trimmed (app-install-url args)))
           (url (resolve-ref ref))
           (id (app-id-of url))
           (target (app-folder id)))
      (when (installed-p id)
        (refuse "~a is already installed as ~a: athena_launch it, or athena_logs to read what it says" url id))
      (ensure-directories-exist (log-path id))
      (multiple-value-bind (output status)
          (funcall *run* (list (setting :git) "clone" "--depth" "1" url (namestring target)))
        (unless (and (integerp status) (zerop status))
          (ignore-errors (uiop:delete-directory-tree target :validate t))
          (fail "git clone of ~a failed~@[: ~a~]" url
                (and (plusp (length (nlk:trimmed output)))
                     (nlk:clip (nlk:one-line output) 300)))))
      (let ((script (suggested-script id)))
        (nlk:encode-json-object
         (nlk:json-object
          "app" id
          "folder" (namestring target)
          "repo" url
          "script" (or script "")
          "note" (if script
                      "athena_launch runs it; the app's own code runs as you, with your files"
                      "no start script found: pass script to athena_launch, e.g. \"python3 app.py\"")))))))

(defparameter +start-scripts+
  '(("start.sh" . "sh start.sh")
    ("start.js" . "node start.js")
    ("start.py" . "python3 start.py")
    ("run.sh" . "sh run.sh")
    ("app.py" . "python3 app.py"))
  "The start command a folder suggests, in the order they are looked for.")

(defun suggested-script (id)
  "The start command the app's folder suggests, or NIL."
  (let ((folder (app-folder id)))
    (loop for (file . command) in +start-scripts+
          when (probe-file (merge-pathnames file folder))
            do (return command))))

(defun launch-app (args)
  "athena_launch: run the app's own start script in its folder, in the background."
  (answering
    (let* ((id (app-launch-app args))
           (chosen (nonblank (app-launch-script args)))
           (command (or chosen (suggested-script id))))
      (unless (installed-p id)
        (refuse "no app named ~a is installed: athena_apps lists what is, athena_install adds one" id))
      (unless command
        (refuse "~a has no start script this cell knows (start.sh, start.js, start.py, run.sh): pass script to name the command that runs it" id))
      (when (app-running-p id)
        (refuse "~a is already running~@[ on ~a~]: athena_stop it first" id (app-url id)))
      (ensure-directories-exist (log-path id))
      (with-open-file (out (log-path id) :direction :output :if-exists :append
                                        :if-does-not-exist :create)
        (format out "~%===== ~a starts ~a~%" id command))
      (multiple-value-bind (process pid)
          (funcall *launch* command (app-folder id) (log-path id))
        (unless (and process (integerp pid))
          (fail "~a did not start: the launcher answered nothing" id))
        (remember-app id process pid)
        (nlk:encode-json-object
         (nlk:json-object
          "app" id
          "pid" pid
          "command" command
          "url" (or (app-url id) "")
          "log" (namestring (log-path id))
          "note" "started in the background: a cold start often needs ten seconds or more to bind a port, so a missing url means still booting; athena_logs says what it is doing"))))))

(defun stop-app (args)
  "athena_stop: end the process this cell started for the app."
  (answering
    (let* ((id (app-stop-app args))
           (record (app-record id))
           (process (getf record :process))
           (pid (app-pid id)))
      (unless (installed-p id)
        (refuse "no app named ~a is installed: athena_apps lists what is" id))
      (unless (or process pid (app-running-p id))
        (refuse "~a is not running: nothing to stop" id))
      (let ((gone (if process
                      (funcall *terminate* process)
                      (progn (funcall *run* (list "kill" "-TERM" (format nil "~d" pid)))
                             (not (funcall *pid-alive* pid))))))
        (unless gone
          (ignore-errors (funcall *run* (list "kill" "-KILL" (format nil "~d" pid)))))
        (forget-app id)
        (nlk:encode-json-object
         (nlk:json-object
          "app" id
          "pid" (or pid 0)
          "stopped" (and gone t)
          "note" (if gone
                     "the process this cell started is gone"
                     "the process did not answer SIGTERM; it was sent SIGKILL")))))))

(defun read-logs (args)
  "athena_logs: the tail of the app's output, and what it says went wrong."
  (answering
    (let* ((id (app-logs-app args))
           (lines (max 1 (or (app-logs-lines args) 80)))
           (why (app-logs-why args)))
      (unless (installed-p id)
        (refuse "no app named ~a is installed: athena_apps lists what is" id))
      (let* ((text (read-log-text id))
             (reading (read-log text)))
        (unless text
          (refuse "~a has no log yet: athena_launch starts it, and its output lands here" id))
        (if why
            (nlk:encode-json-object
             (nlk:json-object
              "app" id
              "running" (and (app-running-p id) t)
              "cause" (or (car reading) "no failure this cell can name in the log")
              "fix" (or (cdr reading) "read the log lines to see what it says")))
            (nlk:encode-json-object
             (nlk:json-object
              "app" id
              "running" (and (app-running-p id) t)
              "log" (namestring (log-path id))
              "lines" (or (tail-lines text lines) "")
              "cause" (or (car reading) "")
              "fix" (or (cdr reading) "")
              "note" (or (cdr reading) "nothing in the log names a cause this cell knows"))))))))

(defun store-status (args)
  "athena_status: what this cell is pointed at, and what it holds."
  (answering
    (let* ((ids (app-ids))
           (running (count-if #'app-running-p ids)))
      (nlk:encode-json-object
       (nlk:json-object
        "apps_dir" (namestring (apps-root))
        "catalog_url" (setting :catalog-url)
        "git" (setting :git)
        "installed" (length ids)
        "running" running
        "apps" (if (store-status-verbose args)
                    (coerce (mapcar #'app-row ids) 'vector)
                    (coerce '() 'vector))
        "help" "athena_search, athena_install, athena_launch, athena_logs, athena_stop")))))

;;; --- the /athena command ---------------------------------------------------------

(defun run-athena (args session-id)
  "One /athena invocation: with no argument the installed apps, with one a
search of the store. Answers text, never a failure."
  (declare (ignore session-id))
  (handler-case
      (let ((query (nlk:trimmed (or args ""))))
        (if (plusp (length query))
            (search-text query)
            (apps-text)))
    (error (condition)
      (format nil "athena: ~a" (nlk:one-line (princ-to-string condition))))))

(defun search-text (query)
  "The store's answer to QUERY, as lines a person reads."
  (let* ((payload (catalogue-fetch query 10))
         (apps (catalogue-apps payload 10)))
    (if (null apps)
        (format nil "athena: the store has no app for ~s" query)
        (with-output-to-string (out)
          (format out "athena: ~d app~:p for ~s~%" (length apps) query)
          (loop for record in apps
                for answer = (app-answer record)
                do (format out "  ~a ~a~%     ~a~%     install with: ~a~%"
                           (nlk:json-value answer :string "name")
                           (nlk:json-value answer :string "version")
                           (nlk:clip (nlk:json-value answer :string "description") 120)
                           (nlk:json-value answer :string "install_with")))))))

(defun apps-text ()
  "The installed apps, as lines a person reads."
  (let ((ids (app-ids)))
    (if (null ids)
        (format nil "athena: nothing is installed yet in ~a. Ask for an app (an image upscaler, a voice clone) and it is searched and installed from the store."
                (namestring (apps-root)))
        (with-output-to-string (out)
          (format out "athena: ~d app~:p in ~a~%" (length ids) (namestring (apps-root)))
          (loop for id in ids
                for running = (app-running-p id)
                do (format out "  ~a  ~a~@[  ~a~]~%"
                           id
                           (if running "running" "stopped")
                           (and running (app-url id))))))))

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell athena
  (:section ("athena")
    (:guide "apps_dir is where apps are installed and run, the one setting every verb needs; catalog_url is the store's JSON API; git must be on PATH, or name it here; next step: ask for an image upscaler or a voice cloner")
    ("apps_dir" :path :required t
     :doc "the folder apps are installed into and run from, e.g. ~/nodecode-apps")
    ("catalog_url" :string :default "https://api.pinokio.co/apps"
     :doc "the JSON catalogue API athena_search reads")
    ("git" :string :default "git"
     :doc "the git executable athena_install clones with")
    ("timeout" :integer :default 30 :min 1
     :doc "how long one catalogue request may take, in seconds"))
  (:settings #'read-settings)
  (:help :athena
         "athena: search a catalogue of AI apps, install one, run it, read its log, stop it"
         *manual*)
  (:tool "athena_search" (nlk:contract 'store-search) #'search-store)
  (:tool "athena_apps" (nlk:contract 'app-list) #'list-apps)
  (:tool "athena_install" (nlk:contract 'app-install) #'install-app)
  (:tool "athena_launch" (nlk:contract 'app-launch) #'launch-app)
  (:tool "athena_stop" (nlk:contract 'app-stop) #'stop-app)
  (:tool "athena_logs" (nlk:contract 'app-logs) #'read-logs)
  (:tool "athena_status" (nlk:contract 'store-status) #'store-status)
  (:command "athena" 'run-athena
            :description "The local app store: what is installed, or a search of the catalogue"
            :argument-hint "QUERY")
  (:start (lambda ()
            (ignore-errors (ensure-directories-exist (state-dir)))
            (clrhash *running*)
            (let ((ids (app-ids)))
              (nle:notice (format nil "athena: ~d app~:p in ~a"
                                   (length ids) (namestring (apps-root)))
                          :key "nodecode-athena")
              (nle:on-stop (lambda ()
                             (clrhash *running*)
                             (nle:notice nil :key "nodecode-athena")))))))
