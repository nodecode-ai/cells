;;;; cell.lisp --- the manual, the profiles, the launch, the CDP verbs, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A profile is a name for a Chromium-family browser reached over CDP: a
;;;; dedicated user-data-dir and remote-debugging port, a binary of the
;;;; operator's choosing, or a host that is already listening. SWITCH makes one
;;;; active, launching it first when it is local and not running; the other
;;;; verbs then speak the DevTools HTTP endpoints of that browser.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "browser-profiles": {"launch_timeout": 20,
;;;;                        "profiles": {"work": {"port": 9250},
;;;;                                     "box": {"host": "10.0.0.5", "port": 9222}}}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-browser-profiles)

;;; --- the manual -------------------------------------------------------------

(defparameter +primer+
  "A browser profile is a named Chromium-family browser (Chrome, Chromium, Brave, Edge) on its own
user-data-dir and remote-debugging port, reached over CDP; one of them can be active. The
nodecode-browser-profiles cell carries them, driven from Lisp through eval. Each verb answers a
string; a refusal is ERROR: BROWSER-PROFILES-ERROR with the reason.
  (browser-profiles:available)            the configured profiles: their port, whether CDP
                                          answers, and which one is active.
  (browser-profiles:switch \"work\")        make 'work' active: launch it when it is local and
                                          not running, wait for its endpoint, and leave it open.
  (browser-profiles:disconnect)          forget the active profile; the browser stays open.
  (browser-profiles:cdp-url)              the active profile's endpoint, http://host:port.
  (browser-profiles:version)              the browser's name, version and WebSocket URL.
  (browser-profiles:tabs)                 the open tabs: id, title, url.
  (browser-profiles:open-tab \"https://…\")  open a tab in the active profile; answers its id.
  (browser-profiles:activate-tab \"ID\")  bring that tab forward; (browser-profiles:close-tab \"ID\")
                                          closes it.
A verb with no profile of its own works on the active one, so switch first. A remote profile (one
with host) is never launched: its browser must already be listening. These verbs open and close
tabs; driving a page (clicking, typing, a screenshot) belongs to the browser itself or to a
browser tool, and a browser this cell launched is one the operator can see and use: do not close
its tabs."
  "What (help :browser-profiles) answers while the cell runs.")

;;; --- the state, and the seams a test rebinds --------------------------------
;;; Bound here so every function below compiles against them; the defaults are
;;; set at the end of the file, after the functions they name.

(defvar *http* nil
  "The cell's one network seam: (METHOD URL &key content timeout connect-timeout)
=> (values BODY STATUS).")

(defvar *find-binary* nil
  "KEY => the browser's executable on this machine, or NIL.")

(defvar *launch* nil
  "ARGV LOG => the process of a launched browser, not waited for.")

(defvar *terminate* nil
  "PROCESS => a browser stopped.")

(defvar *alive-p* nil
  "PROCESS => whether it still runs.")

(defvar *sleep* #'cl:sleep
  "How the readiness poll waits between two looks.")

(defvar *launched* (make-hash-table :test #'equal)
  "Profile name -> the process this cell started for it.")

(defvar *active* nil
  "The profile name the verbs default to, or NIL.")

;;; --- the browsers this machine has -------------------------------------------

(defparameter +browsers+
  '(("chrome" "google-chrome" "google-chrome-stable"
     "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
     "C:/Program Files/Google/Chrome/Application/chrome.exe")
    ("chromium" "chromium" "chromium-browser"
     "/Applications/Chromium.app/Contents/MacOS/Chromium")
    ("brave" "brave-browser" "brave-browser-stable"
     "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser"
     "C:/Program Files/BraveSoftware/Brave-Browser/Application/brave.exe")
    ("brave-origin" "brave-origin"
     "/Applications/Brave Browser Nightly.app/Contents/MacOS/Brave Browser Nightly")
    ("edge" "microsoft-edge" "microsoft-edge-stable"
     "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge"
     "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"))
  "Browser key to the command names or absolute paths that are it.")

(defun path-directories ()
  "The shell's PATH as a list of directory namestrings."
  (let ((path (uiop:getenv "PATH")))
    (if (and path (plusp (length path)))
        (uiop:split-string path :separator '(#\: #\;))
        '())))

(defun find-binary (key)
  "The executable KEY names on this machine, or NIL: a command looked up on
PATH, or an absolute path that exists."
  (loop for candidate in (cdr (assoc key +browsers+ :test #'equal))
        for found = (if (or (find #\/ candidate) (uiop:string-suffix-p candidate ".exe"))
                        (and (uiop:file-exists-p candidate) candidate)
                        (loop for directory in (path-directories)
                              for path = (merge-pathnames candidate
                                                           (uiop:ensure-directory-pathname directory))
                              when (uiop:file-exists-p path)
                                return (uiop:native-namestring path)))
        when found return found))

(defun detect-default-browser ()
  "The key of the first browser this machine has, chrome first."
  (or (loop for key in '("chrome" "chromium" "brave" "edge")
            when (funcall *find-binary* key) return key)
      "chrome"))

(defun expand-dir (text)
  "TEXT with a leading ~/ read against the operator's home."
  (if (uiop:string-prefix-p "~/" text)
      (uiop:native-namestring (merge-pathnames (subseq text 2) (user-homedir-pathname)))
      text))

(defun default-data-dir (name)
  "This home's own user-data-dir for profile NAME."
  (uiop:native-namestring (nlk:home (format nil "browser-profiles/~a/" name))))

(defun headless-by-default-p ()
  "Whether a headed launch would die here: a display-less Unix."
  #+darwin nil
  #-darwin (and (not (uiop:getenv "DISPLAY")) (not (uiop:getenv "WAYLAND_DISPLAY"))))

;;; --- the settings the section gives ------------------------------------------

(defun profile-entries (object)
  "OBJECT's members as (NAME . VALUE), sorted by name."
  (when (hash-table-p object)
    (let ((pairs '()))
      (maphash (lambda (name value) (push (cons name value) pairs)) object)
      (sort pairs #'string< :key #'car))))

(defun normalize-profile (name value)
  "One configured profile as a plist of its own fields."
  (if (not (hash-table-p value))
      (list :name name :error (format nil "profile '~a': the entry is not an object" name))
      (multiple-value-bind (headless present) (nlk:json-value value :boolean "headless")
        (list :name name
              :port (nlk:json-value value :integer "port")
              :host (nlk:json-value value :string "host")
              :browser (nlk:json-value value :string "browser")
              :binary (nlk:json-value value :string "binary")
              :data-dir (nlk:json-value value :string "data_dir")
              :profile-directory (nlk:json-value value :string "profile_directory")
              :headless (if present headless :auto)))))

(defun normalize-profiles (object)
  "The profiles section as a list of entries, each with a name."
  (loop for (name . value) in (profile-entries object)
        collect (normalize-profile (string-downcase name) value)))

(defun derive-settings (values table)
  "The typed values plus the profile list: the map is nested, which a section
field cannot carry, so it is read off the section's own object."
  (setf (getf values :profiles)
        (normalize-profiles (and (hash-table-p table) (gethash "profiles" table))))
  values)

;;; --- one entry to a spec ----------------------------------------------------

(defun resolve-profile (entry)
  "ENTRY as a connect/launch spec: (values SPEC ERROR-TEXT)."
  (let ((name (getf entry :name))
        (port (getf entry :port))
        (host (getf entry :host)))
    (when (getf entry :error)
      (return-from resolve-profile (values nil (getf entry :error))))
    (cond
      ((not (and (integerp port) (<= 1 port 65535)))
       (values nil (format nil "profile '~a': 'port' must be an integer 1-65535 (got ~s)"
                           name port)))
      ((and host (plusp (length host)))
       (values (list :name name :remote t :host host :port port) nil))
      (t
       (let* ((explicit (getf entry :browser))
              (key (if (and explicit (plusp (length explicit)) (not (equal explicit "auto")))
                       (string-downcase explicit)
                       (detect-default-browser))))
         (cond
           ((not (member key (mapcar #'car +browsers+) :test #'equal))
            (values nil (format nil "profile '~a': unknown browser ~s (known: ~{~a~^, ~})"
                                name key (mapcar #'car +browsers+))))
           (t
            (let ((binary (or (getf entry :binary) (funcall *find-binary* key))))
              (unless binary
                (return-from resolve-profile
                  (values nil (format nil "profile '~a': no ~a executable found; install it, or set 'binary' to its path"
                                      name key))))
              (values (list :name name :remote nil :browser key
                            :binary binary :port port
                            :data-dir (if (getf entry :data-dir)
                                          (expand-dir (getf entry :data-dir))
                                          (default-data-dir name))
                            :profile-directory (or (getf entry :profile-directory) "")
                            :headless (if (eq (getf entry :headless) :auto)
                                          (headless-by-default-p)
                                          (getf entry :headless)))
                      nil)))))))))

;;; --- CDP over HTTP ----------------------------------------------------------

(defun cdp-call (url &key (method :get))
  "One DevTools HTTP exchange: (values BODY STATUS), an unreachable endpoint as 0."
  (handler-case
      (multiple-value-bind (body status)
          (funcall *http* method url :timeout 3 :connect-timeout 2)
        (values body (or status 0)))
    (error () (values nil 0))))

(defun cdp-json (url &key (method :get))
  "The decoded answer at URL, or NIL when it does not answer or is not JSON."
  (multiple-value-bind (body status) (cdp-call url :method method)
    (when (and (integerp status) (< 0 status 300) (stringp body) (plusp (length body)))
      (ignore-errors (shasht:read-json body)))))

(defun endpoint (spec)
  "The CDP HTTP endpoint of SPEC."
  (format nil "http://~a:~d" (or (getf spec :host) "127.0.0.1") (getf spec :port)))

(defun cdp-version (spec)
  "SPEC's browser answering /json/version, or NIL."
  (cdp-json (format nil "~a/json/version" (endpoint spec))))

(defun reachable-p (spec)
  "Whether SPEC's browser answers CDP now."
  (and (cdp-version spec) t))

;;; --- launching --------------------------------------------------------------

(defparameter +launch-flags+
  '("--no-first-run" "--no-default-browser-check" "--disable-background-networking"
    "--disable-component-update" "--disable-default-apps" "--disable-hang-monitor"
    "--disable-prompt-on-repost" "--disable-features=Translate")
  "Flags a launched profile browser takes, as the original plugin passed them.")

(defun launch-argv (spec)
  "The argv a launch of SPEC runs."
  (append (list (getf spec :binary)
                (format nil "--user-data-dir=~a" (getf spec :data-dir))
                (format nil "--remote-debugging-port=~d" (getf spec :port)))
          +launch-flags+
          (let ((directory (getf spec :profile-directory)))
            (when (and (stringp directory) (plusp (length directory)))
              (list (format nil "--profile-directory=~a" directory))))
          (when (getf spec :headless)
            (list "--headless=new"))))

(defun launch-browser (argv log)
  "Start ARGV with its stderr in LOG and answer its process, not waited for."
  (ensure-directories-exist log)
  (uiop:launch-program argv :output :ignore :error-output log))

(defun terminate-browser (process)
  "Terminate PROCESS, killing it when it will not go."
  (ignore-errors (uiop:terminate-process process :urgent nil))
  (ignore-errors (uiop:wait-process process)))

(defun alive-p (process)
  "Whether PROCESS still runs."
  (ignore-errors (uiop:process-alive-p process)))

(defun log-tail (path &optional (lines 3))
  "The last LINES of the launch log at PATH."
  (handler-case
      (let ((text (uiop:read-file-string path)))
        (or (format nil "~{~a~^ | ~}"
                    (last (uiop:split-string (string-trim '(#\Space #\Newline #\Return) text)
                                              :separator '(#\Newline))
                          lines))
            "(empty launch log)"))
    (error () "(no launch log)")))

(defun wait-for-cdp (spec process attempts)
  "SPEC's endpoint once it answers, or NIL after ATTEMPTS polls; stops early
when the browser PROCESS it launched is gone."
  (loop repeat attempts
        for version = (cdp-version spec)
        when version return (endpoint spec)
        when (and process (not (funcall *alive-p* process))) return nil
        do (funcall *sleep* 0.25)))

(defun launch-profile (spec)
  "Launch SPEC's browser and answer its endpoint, or refuse."
  (let* ((log (merge-pathnames "launch.log"
                                (uiop:ensure-directory-pathname (getf spec :data-dir))))
         (process (handler-case (funcall *launch* (launch-argv spec) log)
                    (error (e)
                      (fail "could not launch ~a for '~a': ~a" (getf spec :browser) (getf spec :name) e))))
         (seconds (setting :launch-timeout)))
    (setf (gethash (getf spec :name) *launched*) process)
    (or (wait-for-cdp spec process (max 1 (ceiling (/ seconds 0.25))))
        (fail "~a did not expose CDP on port ~d for '~a' within ~ds: ~a"
              (getf spec :browser) (getf spec :port) (getf spec :name) seconds (log-tail log)))))

(defun terminate-launched ()
  "Terminate every browser this cell launched, and forget them."
  (maphash (lambda (name process)
             (declare (ignore name))
             (when (funcall *alive-p* process)
               (funcall *terminate* process)))
           *launched*)
  (clrhash *launched*))

;;; --- the profiles as the model and the operator read them -------------------

(defun profile-names ()
  "Every configured profile's name."
  (mapcar (lambda (entry) (getf entry :name)) (setting :profiles)))

(defun find-profile (name)
  "The entry NAME names, or NIL."
  (find (string-downcase name) (setting :profiles) :key (lambda (entry) (getf entry :name))
        :test #'equal))

(defun profile-line (entry)
  "One line for ENTRY, in AVAILABLE's listing."
  (let ((name (getf entry :name)))
    (multiple-value-bind (spec error) (resolve-profile entry)
      (if error
          (format nil "  x ~a - ~a" name error)
          (format nil "  ~a ~a - ~a, ~a:~d~@[, active~]"
                  (cond ((equal name *active*) "*")
                        ((reachable-p spec) "+")
                        (t "-"))
                  name
                  (if (getf spec :remote) "remote" (format nil "local ~a" (getf spec :browser)))
                  (or (getf spec :host) "127.0.0.1")
                  (getf spec :port)
                  (equal name *active*))))))

(defun available ()
  "The configured profiles, one line each: port, whether CDP answers, which is active."
  (running-settings)
  (let ((entries (setting :profiles)))
    (if (null entries)
        (format nil "No browser profiles are configured. Add them under `browser-profiles.profiles` in ~~/.nodecode/config.jsonc, one member a profile: {\"work\": {\"port\": 9250}}, then (restart-cells).")
        (with-output-to-string (out)
          (format out "~d profile~:p~@[, active: ~a~]:~%"
                  (length entries)
                  (and *active* (format nil "~a" *active*)))
          (dolist (entry entries) (format out "~a~%" (profile-line entry)))))))

(defun switch (name)
  "Make NAME the active profile, launching its browser when it is local and not running."
  (running-settings)
  (with-browser-profiles-lock
    (let* ((entry (find-profile name))
           (spec (when entry (resolve-profile entry))))
      (unless entry
        (fail "unknown profile ~s~@[ (configured: ~{~a~^, ~})~]"
              name (profile-names)))
      (let ((error (nth-value 1 (resolve-profile entry))))
        (when error (fail "~a" error)))
      (let ((url (if (reachable-p spec)
                     (endpoint spec)
                     (if (getf spec :remote)
                         (fail "remote profile '~a' is not answering CDP at ~a:~d"
                               (getf spec :name) (getf spec :host) (getf spec :port))
                         (launch-profile spec)))))
        (setf *active* (getf spec :name))
        (format nil "Browser profile '~a' is active at ~a~@[ (launched; it is a browser the operator can see)~]."
                (getf spec :name) url (gethash (getf spec :name) *launched*))))))

(defun disconnect ()
  "Forget the active profile; the browser keeps running."
  (running-settings)
  (prog1 (if *active*
               (format nil "Profile '~a' is no longer active; its browser keeps running." *active*)
               "No profile was active.")
    (setf *active* nil)))

(defun active-spec ()
  "The active profile's spec, or a refusal naming what to do."
  (unless *active*
    (fail "no profile is active: call (browser-profiles:switch \"name\") first, or (browser-profiles:available) to list them"))
  (multiple-value-bind (spec error) (resolve-profile (find-profile *active*))
    (when error (fail "~a" error))
    spec))

(defun active-spec-or (profile)
  "The spec of PROFILE, or of the active profile."
  (if profile
      (let ((entry (find-profile profile)))
        (unless entry (fail "unknown profile ~s~@[ (configured: ~{~a~^, ~})~]" profile (profile-names)))
        (multiple-value-bind (spec error) (resolve-profile entry)
          (when error (fail "~a" error))
          spec))
      (active-spec)))

(defun cdp-url (&optional profile)
  "The endpoint of PROFILE, or of the active profile."
  (running-settings)
  (endpoint (active-spec-or profile)))

(defun version (&optional profile)
  "The browser's own name, version and WebSocket URL, from /json/version."
  (running-settings)
  (let* ((spec (active-spec-or profile))
         (answer (cdp-version spec)))
    (unless answer
      (fail "profile '~a' is not answering CDP at ~a" (getf spec :name) (endpoint spec)))
    (format nil "~a:~%~{~a: ~a~%~}"
            (getf spec :name)
            (loop for (key . member) in '(("browser" . "Browser") ("protocol" . "Protocol-Version")
                                          ("websocket" . "webSocketDebuggerUrl"))
                  for value = (nlk:json-value answer :string member)
                  when value append (list key value)))))

(defun tabs (&optional profile)
  "The open tabs of PROFILE, or of the active profile: a list of plists."
  (running-settings)
  (let* ((spec (active-spec-or profile))
         (answer (cdp-json (format nil "~a/json/list" (endpoint spec)))))
    (unless answer
      (fail "profile '~a' is not answering CDP at ~a" (getf spec :name) (endpoint spec)))
    (loop for tab across (if (vectorp answer) answer #())
          collect (list :id (nlk:json-value tab :string "id")
                        :title (nlk:json-value tab :string "title")
                        :url (nlk:json-value tab :string "url")))))

(defun url-encode (text)
  "TEXT with everything but a URL's own reserved characters percent-escaped."
  (with-output-to-string (out)
    (loop for character across text
          do (if (or (alphanumericp character)
                     (find character "-._~:/?&=" :test #'char=))
                 (write-char character out)
                 (format out "%~2,'0X" (char-code character))))))

(defun tab-id-of (answer what)
  "The tab id ANSWER carries, or a refusal naming WHAT."
  (or (nlk:json-value answer :string "id")
      (nlk:json-value answer :string "targetId")
      (fail "~a answered no tab id: ~s" what answer)))

(defun open-tab (url &optional profile)
  "Open URL in a new tab of PROFILE, or of the active profile; answers its id."
  (running-settings)
  (let* ((spec (active-spec-or profile))
         (answer (cdp-json (format nil "~a/json/new?~a" (endpoint spec) (url-encode url))
                            :method :put)))
    (unless answer
      (fail "profile '~a' would not open a tab at ~a (it must be running and answering CDP)"
            (getf spec :name) url))
    (format nil "Opened ~a in profile '~a': tab ~a"
            url (getf spec :name) (tab-id-of answer "/json/new"))))

(defun close-tab (id &optional profile)
  "Close the tab ID of PROFILE, or of the active profile."
  (running-settings)
  (let* ((spec (active-spec-or profile))
         (answer (cdp-call (format nil "~a/json/close/~a" (endpoint spec) (url-encode id)))))
    answer
    (format nil "Closed tab ~a in profile '~a'." id (getf spec :name))))

(defun activate-tab (id &optional profile)
  "Bring the tab ID of PROFILE, or of the active profile, to the front."
  (running-settings)
  (let* ((spec (active-spec-or profile))
         (answer (cdp-call (format nil "~a/json/activate/~a" (endpoint spec) (url-encode id)))))
    answer
    (format nil "Brought tab ~a forward in profile '~a'." id (getf spec :name))))

;;; --- the tool and the command ------------------------------------------------

(nlk:define-contract browser-profile-arguments
  (:doc "Switch the browser this cell drives to a named Chromium-family profile (Chrome, Chromium, Brave or Edge), so the tabs and the logins it holds are the ones worked with. A local profile is launched on demand; a remote one must already be listening. Call with no arguments to list the profiles and see which one is active; call with disconnect to let go of it.")
  ("name" :string :doc "the profile to switch to; omit it to list the profiles")
  ("disconnect" :boolean :doc "release the active profile instead of switching"))

(defun run-browser-profile (arguments)
  "The browser_profile tool: list, switch, or disconnect."
  (cond
    ((browser-profile-arguments-disconnect arguments) (disconnect))
    ((let ((name (browser-profile-arguments-name arguments)))
       (and name (plusp (length name))))
     (switch (browser-profile-arguments-name arguments)))
    (t (available))))

(defun run-command (arguments session-id)
  "The /browser-profile command: list, switch NAME, or off."
  (declare (ignore session-id))
  (let ((argument (string-downcase (or (nlk:trimmed (or arguments "")) ""))))
    (values (cond
              ((member argument '("off" "disconnect" "none") :test #'equal) (disconnect))
              ((plusp (length argument)) (switch argument))
              (t (available)))
            nil)))

;;; --- start and stop ----------------------------------------------------------

(defun start ()
  "Forget what the last start held, and take the launched browsers down on stop."
  (setf *active* nil)
  (clrhash *launched*)
  (nle:on-stop (lambda ()
                 (terminate-launched)
                 (setf *active* nil))))

;;; --- the seams' defaults, after the functions they name --------------------

(setf *http* #'nlk:http
      *find-binary* #'find-binary
      *launch* #'launch-browser
      *terminate* #'terminate-browser
      *alive-p* #'alive-p)

;;; --- the cell ----------------------------------------------------------------

(nle:define-cell browser-profiles
  (:section ("browser-profiles")
    (:guide "profiles is a map of name to {port, host?, browser?, binary?, data_dir?, profile_directory?, headless?}: a port is required, a host makes the profile remote and never launched, browser is chrome, chromium, brave, brave-origin or edge; leave the rest out and the system default browser, ~/.nodecode/browser-profiles/<name>/ and a headed launch are used; then (browser-profiles:switch \"name\") from eval, or the browser_profile tool")
    ("launch_timeout" :integer :default 20 :min 1
     :doc "seconds to wait for a launched browser to expose its CDP endpoint"))
  (:settings #'derive-settings)
  (:help :browser-profiles
         "browser-profiles: switch the browser tools to a named Chrome, Edge or Brave profile over CDP"
         +primer+)
  (:tool "browser_profile" (nlk:contract 'browser-profile-arguments) 'run-browser-profile)
  (:command "browser-profile" 'run-command
            :description "Drive a named Chrome, Edge or Brave profile over CDP"
            :argument-hint "[name|off]")
  (:start #'start))
