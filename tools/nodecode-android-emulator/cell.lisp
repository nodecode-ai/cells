;;;; cell.lisp --- the emulator's eighteen verbs, over the adb CLI.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin was a Hermes sidecar: a Python module registering
;;;; eighteen adb-backed tools, a dashboard API that also started and stopped
;;;; the emulator and managed AVDs, and a desktop sidebar. The tools are the
;;;; part a model uses, and every one of them was a thin wrapper over one adb
;;;; command line, so this cell runs adb itself: the CLI is the operator's,
;;;; like the language servers of nodecode-lsp.
;;;;
;;;; Two rules carry over from the original's hardening pass, because they are
;;;; what keeps a phone safe from a plugin that says "emulator":
;;;;   - every device call is pinned with `-s SERIAL', and a serial that is not
;;;;     an emulator's refuses at start unless allow_physical is set;
;;;;   - a file this cell writes lands under the home directory, the cwd, the
;;;;     temp dir or output_root, and never silently replaces a file.
;;;;
;;;; *RUN* is the one seam: a verb builds the argv and hands it to *RUN*, so a
;;;; test asserts the argv and answers the output with no adb, no emulator and
;;;; no device.

(in-package #:nodecode-android-emulator)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Android Emulator is available through the nodecode-android-emulator cell: eighteen tools,
which a person can also call from eval in the android-emulator package. They drive ONE
emulator, the one `serial' names (emulator-5554 by default), through the adb CLI, which
must be on PATH or named in the section's adb.
  emu_status              where the emulator is: connected or not, Android version, model,
                          screen size, boot state and free space.
  emu_screenshot          {output_path, overwrite} a PNG of the screen, saved on this machine.
  emu_tap                 {x, y} a tap; emu_swipe {x1, y1, x2, y2, duration_ms} a swipe.
  emu_type                {text} types into the focused field (spaces and metacharacters
                          are escaped; a literal '%s' reads as a space).
  emu_key                 {keycode} one key: HOME, BACK, ENTER, POWER, APP_SWITCH ...
  emu_launch              {package, activity, url} an app, one activity, or a URL.
  emu_deeplink            {url} a deep link, e.g. myapp://path.
  emu_install             {apk_path, replace, grant_permissions} an APK from this machine.
  emu_uninstall           {package} an app, by package name.
  emu_packages            {filter} what is installed.
  emu_push / emu_pull     {local_path, remote_path} a file in; {remote_path, local_path,
                          overwrite} a file out.
  emu_logcat              {filter, lines} recent logs, e.g. filter \"*:E\" for errors only.
  emu_gps                 {lat, lng, clear} a simulated location.
  emu_battery             {level, unplug, reset} a simulated battery.
  emu_network             {condition} offline, slow or fast.
  emu_shell               {command, timeout_seconds} any device shell command -- it runs on
                          Android, not on this machine, and is the one raw entry point.
Every tool answers with the adb command it ran, its exit code, and what adb wrote, so a
failure reads as adb reported it. A screenshot's own path is in the answer. Nothing here
acts on a physical phone unless the section's allow_physical is set."
  "What (help :android-emulator) answers while the cell runs.")

;;; --- the section, and the one refusal it makes at start ------------------------

(defun nonblank (text)
  (let ((text (and (stringp text) (string-trim '(#\Space #\Tab #\Newline #\Return) text))))
    (and text (plusp (length text)) text)))

(defun clamp (value lo hi)
  (max lo (min hi value)))

(defun expand-prefix (word)
  "WORD with a leading ~ read against the home directory, as the original did."
  (if (and (stringp word) (uiop:string-prefix-p "~" word))
      (uiop:native-namestring (nlk:expand-home word))
      word))

(defun read-settings (values table)
  "The declared VALUES, with adb's argv prefix expanded and the serial checked."
  (declare (ignore table))
  (let* ((adb (mapcar #'expand-prefix (coerce (or (getf values :adb) '()) 'list)))
         (serial (nonblank (getf values :serial)))
         (physical (getf values :allow-physical))
         (plist (copy-list values)))
    (when (or (null adb) (not (every #'stringp adb))
              (every (lambda (word) (zerop (length word))) adb))
      (nlk:config-error "adb names no program: set the android-emulator section's adb to [\"adb\"] for an install on PATH, or to the full path of the adb binary"))
    (unless serial
      (nlk:config-error "serial is empty: set it to the emulator's serial (an emulator's first one is \"emulator-5554\")"))
    (unless (or physical (uiop:string-prefix-p "emulator" serial))
      (nlk:config-error "serial ~s is not an emulator's, and every call here is pinned to one so a phone is never touched: set allow_physical to true to act on ~s on purpose"
                        serial serial))
    (setf (getf plist :adb) adb
          (getf plist :serial) serial)
    plist))

;;; --- running adb ---------------------------------------------------------------

(defparameter *run* 'run-adb
  "How a verb runs adb: (funcall *RUN* ARGV :TIMEOUT SECONDS) => (values
STDOUT STDERR STATUS), STATUS an exit code or :TIMEOUT.")

(defun run-adb (argv &key timeout)
  "Run ARGV to completion or TIMEOUT, => (values STDOUT STDERR STATUS)."
  (handler-case (nlk:run-bounded argv :seconds (or timeout (setting :timeout-seconds)))
    (error (condition)
      ;; adb missing is the ordinary first state of this cell, so it is words
      ;; the model can act on rather than a signalled error; 127 is what a
      ;; shell says for a command it cannot find.
      (values nil
              (format nil "~a could not be run: ~a. Put the Android SDK's platform-tools on PATH, or set the android-emulator section's adb to [\"$HOME/Android/Sdk/platform-tools/adb\"]."
                      (first argv) (nlk:one-line (princ-to-string condition)))
              127))))

(defun adb-argv (arguments)
  "ARGUMENTS behind adb and the pinned serial."
  (append (setting :adb) (list "-s" (setting :serial)) arguments))

(defvar *shot-counter* 0
  "Names one screenshot's file in the emulator apart from the next.")

;;; --- one answer ----------------------------------------------------------------

(defun clip (text limit)
  (cond ((not (stringp text)) "")
        ((<= (length text) limit) text)
        (t (format nil "~a~%... cut at ~d of ~d characters (the android-emulator section's max_output_chars)"
                   (subseq text 0 limit) limit (length text)))))

(defun status-line (status)
  (cond ((eq status :timeout)
         (format nil "exit: passed timeout_seconds (~d s) and its process tree was killed"
                 (setting :timeout-seconds)))
        ((integerp status) (format nil "exit ~d" status))
        (t (format nil "exit ~a" status))))

(defun answer (argv out err status &key note)
  "The one string a verb answers: the command, how it ended, what adb wrote."
  (let ((limit (setting :max-output-chars))
        (out (or out ""))
        (err (string-trim '(#\Space #\Tab #\Newline #\Return) (or err ""))))
    (with-output-to-string (stream)
      (when note (format stream "~a~%" note))
      (format stream "$ ~{~a~^ ~}~%" argv)
      (format stream "~a~%" (status-line status))
      (if (plusp (length out))
          (progn (write-string (clip out limit) stream) (terpri stream))
          (format stream "(no output)~%"))
      (when (plusp (length err))
        (format stream "stderr: ~a~%" (clip err limit))))))

(defun run-tool (argv &key timeout note (fail-on-error nil))
  "Run ARGV and answer it as the tool's string; a run that could not happen,
passed its bound, or (with FAIL-ON-ERROR) failed is the call's failure."
  (multiple-value-bind (out err status) (funcall *run* argv :timeout timeout)
    (let ((text (answer argv out err status :note note)))
      (if (or (eq status :timeout)
              (and (integerp status)
                   (or (= status 127) (and fail-on-error (plusp status)))))
          (nle:failure text)
          text))))

(defmacro answering (&body body)
  "BODY's text; a refusal is that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (android-emulator-error (condition)
       (nle:failure (format nil "ERROR: ~a" (android-emulator-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: android emulator: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

;;; --- names the device is given -------------------------------------------------

(defun alnum-p (character)
  (and (< (char-code character) 128) (alphanumericp character)))

(defun package-name-p (name)
  (and (plusp (length name))
       (every (lambda (c) (or (alnum-p c) (find c "._"))) name)))

(defun activity-name-p (name)
  (and (plusp (length name))
       (every (lambda (c) (or (alnum-p c) (find c "._$"))) name)))

(defun keycode-p (name)
  (and (<= 1 (length name) 48)
       (every (lambda (c) (or (alnum-p c) (char= c #\_))) name)))

(defun remote-path-p (path)
  (and (plusp (length path))
       (char= #\/ (char path 0))
       (every (lambda (c) (or (alnum-p c) (find c "._-/") (char= c #\Space))) path)))

(defun url-p (url)
  (let ((colon (search "://" url)))
    (and colon (> colon 0) (< (+ colon 3) (length url))
         (let ((scheme (subseq url 0 colon))
               (rest (subseq url (+ colon 3))))
           (and (alpha-char-p (char scheme 0))
                (every (lambda (c) (or (alnum-p c) (find c "+.-"))) scheme)
                (every (lambda (c) (not (find c " ;|&$`'\"()<>\\"))) rest))))))

(defun filter-word-p (word)
  (and (<= (length word) 64)
       (every (lambda (c) (or (alnum-p c) (find c "._-"))) word)))

(defun logspec-p (part)
  (let ((colon (position #\: part :from-end t)))
    (and colon (= colon (- (length part) 2))
         (let ((tag (subseq part 0 colon)))
           (and (plusp (length tag)) (<= (length tag) 64)
                (or (equal tag "*")
                    (every (lambda (c) (or (alnum-p c) (find c "._-"))) tag))))
         (find (char part (1+ colon)) "VDIWEFS"))))

(defun logcat-specs (filter)
  (let ((parts (remove "" (uiop:split-string filter :separator '(#\Space #\Tab)) :test #'equal)))
    (dolist (part parts)
      (unless (logspec-p part)
        (fail "~s is not a logcat filter spec: each is tag:priority, the priority one of V D I W E F S, e.g. \"*:E\" or \"ActivityManager:I *:S\"" part)))
    parts))

(defun device-text (text)
  "TEXT as `adb shell input text` takes it: spaces become %s, then the whole
is one shell word, so the device shell sees metacharacters literally."
  (let ((mapped (with-output-to-string (stream)
                  (loop for character across text
                        do (if (char= character #\Space)
                               (write-string "%s" stream)
                               (write-char character stream))))))
    (with-output-to-string (stream)
      (write-char #\' stream)
      (loop for character across mapped
            do (if (char= character #\')
                   (write-string "'\\''" stream)
                   (write-char character stream)))
      (write-char #\' stream))))

(defun shell-quote (text)
  "TEXT as one POSIX shell word for the device's sh -c."
  (with-output-to-string (stream)
    (write-char #\' stream)
    (loop for character across text
          do (if (char= character #\')
                 (write-string "'\\''" stream)
                 (write-char character stream)))
    (write-char #\' stream)))

;;; --- where a file may land -------------------------------------------------------

(defun dir-namestring (path)
  (uiop:native-namestring (uiop:ensure-directory-pathname path)))

(defun output-roots ()
  "The directories a written file may land under."
  (remove nil
          (list (and (nonblank (setting :output-root))
                     (dir-namestring (uiop:ensure-directory-pathname (setting :output-root))))
                (dir-namestring (user-homedir-pathname))
                (dir-namestring (uiop:getcwd))
                (dir-namestring (uiop:temporary-directory)))))

(defun under-root-p (path root)
  (and (>= (length path) (length root))
       (string= root path :end2 (length root))))

(defun output-path (raw &key default overwrite)
  "RAW (or DEFAULT) as an absolute namestring under the home, the cwd, the
temp dir or output_root; refuses a .., a path outside them, an existing file
without OVERWRITE, and a missing parent."
  (let ((given (or (nonblank raw) default)))
    (unless given
      (fail "no output path given"))
    (let* ((expanded (expand-prefix given))
           (absolute (if (and (plusp (length expanded)) (char= #\/ (char expanded 0)))
                         expanded
                         (uiop:native-namestring
                          (uiop:merge-pathnames* (uiop:parse-native-namestring expanded)
                                                  (uiop:getcwd)))))
           (segments (uiop:split-string absolute :separator '(#\/))))
      (when (member ".." segments :test #'equal)
        (fail "~s has a .. in it: a write lands under the home directory, the cwd, the temp dir or output_root, and a .. would leave them" given))
      (unless (some (lambda (root) (under-root-p absolute root)) (output-roots))
        (fail "output path must be under the home directory, the cwd, the temp dir or output_root: ~a" absolute))
      (when (and (probe-file absolute) (not overwrite))
        (fail "refusing to overwrite ~a: pass overwrite=true" absolute))
      (let ((parent (uiop:pathname-directory-pathname (uiop:parse-native-namestring absolute))))
        (unless (uiop:directory-exists-p parent)
          (fail "the parent directory ~a does not exist" (uiop:native-namestring parent))))
      absolute)))

(defun file-size (path)
  (let ((file (probe-file path)))
    (and file
         (with-open-file (in file :element-type '(unsigned-byte 8))
           (file-length in)))))

;;; --- the verbs -----------------------------------------------------------------

(defun handle-status (arguments)
  "emu_status: is the emulator there, and what is it."
  (declare (ignore arguments))
  (answering
    (let* ((argv (append (setting :adb) (list "devices" "-l")))
           (serial (setting :serial)))
      (multiple-value-bind (out err status) (funcall *run* argv)
        (if (and (integerp status) (plusp status))
            (nle:failure (answer argv out err status :note "adb devices failed"))
            (let* ((lines (and out (uiop:split-string out :separator '(#\Newline))))
                   (line (find-if (lambda (one)
                                    (and (plusp (length one))
                                         (uiop:string-prefix-p serial one)))
                                  lines)))
              (if (null line)
                  (format nil "~a is not connected. Start one (emulator -avd <name>), or set serial to the one it got. adb devices says:~%~a"
                          serial (or (nonblank out) "(nothing)"))
                  (with-output-to-string (stream)
                    (format stream "~a is connected~%" serial)
                    (format stream "device: ~a~%" (string-trim '(#\Space #\Tab #\Return) line))
                    (loop for (label . args) in
                          (list (cons "android_version" (list "shell" "getprop" "ro.build.version.release"))
                                (cons "sdk" (list "shell" "getprop" "ro.build.version.sdk"))
                                (cons "model" (list "shell" "getprop" "ro.product.model"))
                                (cons "screen" (list "shell" "wm" "size"))
                                (cons "boot_completed" (list "shell" "getprop" "sys.boot_completed"))
                                (cons "disk" (list "shell" "df" "-h" "/data")))
                          do (multiple-value-bind (o e c) (funcall *run* (adb-argv args))
                               (declare (ignore e c))
                               (format stream "~a: ~a~%" label (or (nonblank o) "?"))))))))))))

(defun handle-shell (call)
  "emu_shell: one command on the device."
  (answering
    (run-tool (adb-argv (list "shell" "sh" "-c" (shell-quote (shell-command-command call))))
              :timeout (clamp (shell-command-timeout-seconds call) 1 600))))

(defun handle-install (call)
  "emu_install: an APK from this machine."
  (answering
    (let ((apk (install-apk-apk-path call)))
      (unless (probe-file apk)
        (fail "no file at ~a" apk))
      (unless (uiop:string-suffix-p (string-downcase apk) ".apk")
        (fail "~a is not an .apk file" apk))
      (run-tool (adb-argv (append (list "install")
                                   (and (install-apk-replace-p call) (list "-r"))
                                   (and (install-apk-grant-permissions call) (list "-g"))
                                   (list apk)))
                :timeout 120 :fail-on-error t))))

(defun handle-uninstall (call)
  "emu_uninstall: an app by package name."
  (answering
    (let ((package (uninstall-package-app-package call)))
      (unless (package-name-p package)
        (fail "~s is not a package name: letters, digits, dot and underscore only" package))
      (run-tool (adb-argv (list "uninstall" package)) :fail-on-error t))))

(defun handle-screenshot (call)
  "emu_screenshot: capture in the emulator, pull it here, remove it there."
  (answering
    (let* ((path (output-path (screenshot-take-output-path call)
                               :default "emu_screenshot.png"
                               :overwrite (screenshot-take-overwrite call)))
           (remote (format nil "/sdcard/emu_screenshot_~d_~d.png"
                           (get-universal-time) (incf *shot-counter*)))
           (capture (adb-argv (list "shell" "screencap" "-p" remote))))
      (multiple-value-bind (out err status) (funcall *run* capture)
        (declare (ignore out))
        (cond
          ((or (eq status :timeout) (and (integerp status) (plusp status)))
           (nle:failure (answer capture "" err status :note "the screen was not captured")))
          (t
           (let ((pull (adb-argv (list "pull" remote path))))
             (multiple-value-bind (pout perr pstatus) (funcall *run* pull)
               (funcall *run* (adb-argv (list "shell" "rm" "-f" remote)))
               (if (and (integerp pstatus) (plusp pstatus))
                   (nle:failure (answer pull pout perr pstatus
                                        :note (format nil "the screen was captured to ~a in the emulator, but the pull failed" remote)))
                   (format nil "~a~@[ (~d bytes on this machine)~]"
                           (answer pull pout perr pstatus)
                           (file-size path)))))))))))

(defun handle-tap (call)
  "emu_tap: one touch."
  (answering
    (run-tool (adb-argv (list "shell" "input" "tap"
                               (princ-to-string (clamp (tap-at-x call) 0 100000))
                               (princ-to-string (clamp (tap-at-y call) 0 100000))))
              :note (format nil "tapped ~d,~d" (tap-at-x call) (tap-at-y call)))))

(defun handle-swipe (call)
  "emu_swipe: a gesture between two points."
  (answering
    (run-tool (adb-argv (list "shell" "input" "swipe"
                               (princ-to-string (clamp (swipe-from-x1 call) 0 100000))
                               (princ-to-string (clamp (swipe-from-y1 call) 0 100000))
                               (princ-to-string (clamp (swipe-from-x2 call) 0 100000))
                               (princ-to-string (clamp (swipe-from-y2 call) 0 100000))
                               (princ-to-string (clamp (swipe-from-duration-ms call) 1 10000))))
              :note (format nil "swiped ~d,~d to ~d,~d"
                            (swipe-from-x1 call) (swipe-from-y1 call)
                            (swipe-from-x2 call) (swipe-from-y2 call)))))

(defun handle-type (call)
  "emu_type: text into the focused field."
  (answering
    (let ((text (typed-text-text call)))
      (run-tool (adb-argv (list "shell" "input" "text" (device-text text)))
                :note (format nil "typed ~s" (if (> (length text) 60)
                                                 (concatenate 'string (subseq text 0 60) "...")
                                                 text))))))

(defun handle-key (call)
  "emu_key: one key event."
  (answering
    (let ((key (string-upcase (key-event-keycode call))))
      (unless (uiop:string-prefix-p "KEYCODE_" key)
        (setf key (concatenate 'string "KEYCODE_" key)))
      (unless (keycode-p key)
        (fail "~s is not a keycode: letters, digits and underscore, e.g. HOME or KEYCODE_BACK" (key-event-keycode call)))
      (run-tool (adb-argv (list "shell" "input" "keyevent" key))))))

(defun handle-launch (call)
  "emu_launch: an app, an activity, or a URL."
  (answering
    (let ((package (nonblank (launch-intent-app-package call)))
          (activity (nonblank (launch-intent-activity call)))
          (url (nonblank (launch-intent-url call))))
      (cond
        ((and url (null package))
         (unless (url-p url)
           (fail "~s is not a URL: it needs a scheme, e.g. https://example.com or myapp://path" url))
         (run-tool (adb-argv (list "shell" "am" "start" "-a" "android.intent.action.VIEW" "-d" url))))
        (package
         (unless (package-name-p package)
           (fail "~s is not a package name: letters, digits, dot and underscore only" package))
         (when (and activity (not (activity-name-p activity)))
           (fail "~s is not an activity name" activity))
         (run-tool (adb-argv (list "shell" "am" "start" "-n"
                                    (if activity
                                        (format nil "~a/~a" package activity)
                                        package)))))
        (t (fail "give a package to launch, or a url to open"))))))

(defun handle-packages (call)
  "emu_packages: what is installed, optionally filtered."
  (answering
    (let ((filter (nonblank (package-list-filter call))))
      (when (and filter (not (filter-word-p filter)))
        (fail "~s is not a package filter: letters, digits, dot, dash and underscore, at most 64" filter))
      (let ((argv (adb-argv (append (list "shell" "pm" "list" "packages") (and filter (list filter))))))
        (multiple-value-bind (out err status) (funcall *run* argv)
          (if (and (integerp status) (plusp status))
              (nle:failure (answer argv out err status))
              (let ((packages (loop for line in (uiop:split-string (or out "") :separator '(#\Newline))
                                    when (uiop:string-prefix-p "package:" line)
                                      collect (string-trim '(#\Space #\Tab #\Return)
                                                           (subseq line (length "package:"))))))
                (with-output-to-string (stream)
                  (when (plusp (length (or err "")))
                    (format stream "adb said: ~a~%" (string-trim '(#\Space #\Tab #\Newline) err)))
                  (format stream "~d package~:p~@[ matching ~s~]:~%" (length packages) filter)
                  (if packages
                      (dolist (package packages) (format stream "~a~%" package))
                      (format stream "(none)~%"))))))))))

(defun handle-push (call)
  "emu_push: a file from this machine into the emulator."
  (answering
    (let ((local (push-file-local-path call))
          (remote (push-file-remote-path call)))
      (unless (probe-file local)
        (fail "no file at ~a" local))
      (unless (remote-path-p remote)
        (fail "~s is not a path inside the emulator: it starts with / and holds letters, digits, dot, dash, underscore and slashes" remote))
      (run-tool (adb-argv (list "push" local remote)) :fail-on-error t))))

(defun handle-pull (call)
  "emu_pull: a file out of the emulator onto this machine."
  (answering
    (let ((remote (pull-file-remote-path call)))
      (unless (remote-path-p remote)
        (fail "~s is not a path inside the emulator: it starts with / and holds letters, digits, dot, dash, underscore and slashes" remote))
      (let ((local (output-path (pull-file-local-path call)
                                 :overwrite (pull-file-overwrite call))))
        (run-tool (adb-argv (list "pull" remote local))
                  :fail-on-error t
                  :note (format nil "into ~a" local))))))

(defun handle-logcat (call)
  "emu_logcat: recent logs, filtered."
  (answering
    (let* ((lines (clamp (logcat-read-lines call) 1 5000))
           (filter (nonblank (logcat-read-filter call)))
           (specs (and filter (logcat-specs filter))))
      (run-tool (adb-argv (append (list "shell" "logcat" "-d" "-t" (princ-to-string lines)) specs))
                :note (format nil "the last ~d line~:p~@[ matching ~s~]" lines filter)))))

(defun handle-gps (call)
  "emu_gps: a simulated location, or clearing one."
  (answering
    (if (gps-fix-clear call)
        (run-tool (adb-argv (list "emu" "geo" "nmea" "$GPGGA,,,,,,0,,,,,,,,*66"))
                  :note "the location override is cleared")
        (let ((lat (gps-fix-lat call))
              (lng (gps-fix-lng call)))
          (unless (and lat lng)
            (fail "give both lat and lng to set a location, or clear=true to remove one"))
          (unless (<= -90 lat 90)
            (fail "lat ~a is outside -90..90" lat))
          (unless (<= -180 lng 180)
            (fail "lng ~a is outside -180..180" lng))
          (run-tool (adb-argv (list "emu" "geo" "fix"
                                     (format nil "~,6f" (coerce lng 'double-float))
                                     (format nil "~,6f" (coerce lat 'double-float))))
                    :note (format nil "GPS set to ~,6f, ~,6f" lat lng))))))

(defun handle-battery (call)
  "emu_battery: a simulated level, unplugged, or the real values again."
  (answering
    (cond
      ((battery-state-reset call)
       (run-tool (adb-argv (list "shell" "dumpsys" "battery" "reset"))
                 :note "back to the emulator's real battery values"))
      ((battery-state-unplug call)
       (run-tool (adb-argv (list "shell" "dumpsys" "battery" "unplug"))
                 :note "reported unplugged and draining"))
      (t
       (let ((level (battery-state-level call)))
         (unless level
           (fail "give a level 0-100, unplug=true, or reset=true"))
         (setf level (clamp level 0 100))
         (run-tool (adb-argv (list "shell" "dumpsys" "battery" "set" "level" (princ-to-string level)))
                   :note (format nil "battery level ~d%" level)))))))

(defun handle-network (call)
  "emu_network: offline, slow or fast."
  (answering
    (let ((condition (network-state-network-condition call)))
      (with-output-to-string (stream)
        (flet ((run-step (arguments &optional note)
                 (write-string (run-tool (adb-argv arguments) :note note) stream)
                 (terpri stream)))
          (cond
            ((string= condition "offline")
             (run-step (list "shell" "svc" "wifi" "disable") "wifi off")
             (run-step (list "shell" "svc" "data" "disable") "mobile data off"))
            ((string= condition "slow")
             (run-step (list "shell" "svc" "wifi" "enable") "wifi on")
             (run-step (list "shell" "svc" "data" "enable") "mobile data on")
             (run-step (list "shell" "tc" "qdisc" "add" "dev" "wlan0" "root" "netem" "delay" "500ms" "loss" "10%")
                   "500 ms delay and 10% loss on wlan0"))
            (t
             (run-step (list "shell" "tc" "qdisc" "del" "dev" "wlan0" "root") "the delay and loss removed")
             (run-step (list "shell" "svc" "wifi" "enable") "wifi on")
             (run-step (list "shell" "svc" "data" "enable") "mobile data on"))))))))

(defun handle-deeplink (call)
  "emu_deeplink: a deep link or URL scheme."
  (answering
    (let ((url (deep-link-url call)))
      (unless (url-p url)
        (fail "~s is not a URL: it needs a scheme, e.g. myapp://path or https://example.com" url))
      (run-tool (adb-argv (list "shell" "am" "start" "-a" "android.intent.action.VIEW" "-d" url))))))

(defun no-arguments (description)
  "The schema a tool that takes no arguments registers."
  (list :description description
        :parameters (nlk:json-object "type" "object"
                                      "properties" (nlk:json-object))))

;;; --- the declaration ------------------------------------------------------------

(nle:define-cell android-emulator
  (:section ("android-emulator")
    (:guide "the Android SDK's adb on PATH, or its full path in adb; one booted emulator, whose serial the section pins; nothing is required beyond adb; next step: ask for a screenshot of the emulator")
    ("adb" :list :default '("adb")
     :doc "the argv prefix that runs adb: [\"adb\"] for platform-tools on PATH, or [\"~/Android/Sdk/platform-tools/adb\"] for an install in its usual place (a leading ~ is read against the home directory)")
    ("serial" :string :default "emulator-5554"
     :doc "the serial of the one emulator every call is pinned to (adb devices lists it); an emulator's serial starts with emulator-")
    ("allow_physical" :boolean :default nil
     :doc "act on a serial that is not an emulator's, i.e. a real phone, on purpose")
    ("output_root" :path
     :doc "one more directory screenshots and pulls may be written under, beside the home directory, the cwd and the temp dir")
    ("timeout_seconds" :integer :default 60 :min 1
     :doc "one adb run's ceiling in seconds; its whole process tree is killed past it")
    ("max_output_chars" :integer :default 20000 :min 200
     :doc "how much of what adb wrote one tool result carries"))
  (:settings #'read-settings)
  (:help :android-emulator
         "android-emulator: emu_status, emu_screenshot, emu_tap, emu_swipe, emu_type, emu_install and thirteen more tools drive a headless Android emulator through adb"
         *manual*)
  (:tool "emu_status" (no-arguments "Check the emulator: whether it is connected, its Android version, model, screen size, boot state and free space.")
         #'handle-status)
  (:tool "emu_shell" (nlk:contract 'shell-command) #'handle-shell)
  (:tool "emu_install" (nlk:contract 'install-apk) #'handle-install)
  (:tool "emu_uninstall" (nlk:contract 'uninstall-package) #'handle-uninstall)
  (:tool "emu_screenshot" (nlk:contract 'screenshot-take) #'handle-screenshot)
  (:tool "emu_tap" (nlk:contract 'tap-at) #'handle-tap)
  (:tool "emu_swipe" (nlk:contract 'swipe-from) #'handle-swipe)
  (:tool "emu_type" (nlk:contract 'typed-text) #'handle-type)
  (:tool "emu_key" (nlk:contract 'key-event) #'handle-key)
  (:tool "emu_launch" (nlk:contract 'launch-intent) #'handle-launch)
  (:tool "emu_packages" (nlk:contract 'package-list) #'handle-packages)
  (:tool "emu_push" (nlk:contract 'push-file) #'handle-push)
  (:tool "emu_pull" (nlk:contract 'pull-file) #'handle-pull)
  (:tool "emu_logcat" (nlk:contract 'logcat-read) #'handle-logcat)
  (:tool "emu_gps" (nlk:contract 'gps-fix) #'handle-gps)
  (:tool "emu_battery" (nlk:contract 'battery-state) #'handle-battery)
  (:tool "emu_network" (nlk:contract 'network-state) #'handle-network)
  (:tool "emu_deeplink" (nlk:contract 'deep-link) #'handle-deeplink))
