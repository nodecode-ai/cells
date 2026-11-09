;;;; cell.lisp --- the manual, the readers, the two tools, /startup, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The question is "what launches itself when this machine signs in, from
;;;; where, and did the person switch it off". Windows answers it from the six
;;;; Run/RunOnce keys and Explorer's StartupApproved blobs plus the two
;;;; Startup folders; Linux from the XDG autostart folders; macOS from the
;;;; LaunchAgents and LaunchDaemons folders. Every entry is read, resolved
;;;; and reported -- nothing is enabled, disabled or deleted, and no network
;;;; is touched: the only process this cell starts is `reg query', on Windows,
;;;; to read a key the image has no other reader for.
;;;;
;;;; A rewrite of Yicheng-Mark/hermes-windows-plugins' windows-startup plugin
;;;; (see NOTICE). Its two tool names lose the win_ prefix and its two
;;;; registry-only halves gain an XDG and a launchd reader, so the same
;;;; question is answered on the machine the cell runs on.

(in-package #:nodecode-startup)

;;; --- the manual -------------------------------------------------------------

(defparameter +primer+
  "Answer 'what launches itself at sign-in' on this machine, read-only: the
nodecode-startup cell lists every autostart entry, where each one comes from,
whether it is user- or machine-scoped, whether the person switched it off, and
whether the binary it names is still on disk. Driven from Lisp through eval;
each verb answers a string, and a refusal is ERROR: STARTUP-ERROR.
  (startup:startup-table)     every autostart entry, one line each, with the
                               sources it was read from and what was switched off.
  (startup:startup-detail \"NAME\")  one entry in full: the exact command, the
                               binary it resolves to (quoting, arguments and
                               environment variables folded in) and whether that
                               binary is still there.
Sources: on Windows the six Run/RunOnce keys (HKCU, HKLM and the 32-bit view),
Explorer's StartupApproved verdicts -- the enabled/disabled state and when it
changed -- and the two Startup folders; on Linux ~/.config/autostart and the
XDG system autostart folders; on macOS the user's LaunchAgents and the
system's LaunchAgents and LaunchDaemons. Desktop entries with Hidden=true or
X-GNOME-Autostart-enabled=false, and a launchd job with Disabled, count as
switched off; an entry with no record means it runs, which is the platform's
default. Nothing here is enabled, disabled or deleted: use the desktop's own
Startup settings, Task Manager, or the file, and ask again."
  "What (help :startup) answers while the cell runs.")

;;; --- the seams a test rebinds -------------------------------------------------

(defparameter *command-chars* 200
  "How much of an entry's command the table shows; the detail tool shows it whole.")

(defparameter *max-rows* 400
  "The hard ceiling on rows one table answer carries, whatever the call asks for.")

(defvar *platform* nil
  "The platform to read as: :windows, :darwin or :linux. NIL reads the one this
process runs on, which is the normal case; a test binds it to read another's
fixtures.")

(defvar *home* nil
  "The home to read user-scope sources under, or NIL for the running user's.
A test binds it to a fixture; the section's `home' does the same for a person
inspecting a mounted disk or another profile.")

(defvar *probe* nil
  "() => (values ENTRIES ORPHANS): every autostart entry this machine has, and
the enabled/disabled records left behind by entries that are gone. The one seam
the two tools read the machine through; PROBE-NOW answers it and a test rebinds
it to its own fixture.")

(defvar *file-exists-p* nil
  "PATH => true when a file is there. The detail tool's one question about the
disk, so a test can answer it without one.")

(defvar *reg-query* nil
  "KEY => the lines `reg query' printed for it, or NIL when the key is not
there. The registry half's one seam: a test answers it with a fixture instead
of a registry.")

;;; --- the platform, and where a source is read from -----------------------------

(defun platform ()
  "The platform this answer is read as, a keyword: :windows, :darwin or :linux."
  (or *platform*
      (cond ((uiop:os-windows-p) :windows)
            ((uiop:os-macosx-p) :darwin)
            (t :linux))))

(defun home-override ()
  "The home a section asked to read instead of this user's, or NIL."
  (or *home*
      (and *startup*
           (let ((asked (setting :home)))
             (and asked (plusp (length (princ-to-string asked))) (pathname asked))))))

(defun read-home ()
  "The home user-scope sources are read under."
  (or (home-override) (user-homedir-pathname)))

(defun listed-files (path pattern)
  "The files in PATH matching PATTERN, a folder that cannot be read answering none."
  (ignore-errors (uiop:directory-files path pattern)))

(defun folder-files (directory &optional pattern)
  "The files directly in DIRECTORY, sorted by name: every file when PATTERN is
NIL, the matching ones when it is given. A folder that is not there, or cannot
be read, has none."
  (let ((path (ignore-errors (uiop:ensure-directory-pathname (pathname directory)))))
    (when (and path (ignore-errors (probe-file path)))
      (sort (remove-duplicates
             (remove-if (lambda (file) (or (uiop:directory-pathname-p file)
                                           (null (pathname-name file))))
                        (if pattern
                            (listed-files path pattern)
                            (append (listed-files path "*.*") (listed-files path "*"))))
             :test #'equal)
            #'string< :key #'file-namestring))))

(defun iso-from-universal (universal)
  "UNIVERSAL as an ISO-8601 UTC stamp, or NIL."
  (and universal (nlk:iso-time (- universal 2208988800))))

(defun expand-vars (text)
  "TEXT with %NAME% and $NAME (or ${NAME}) replaced by this environment --
an unset one reads empty, as a launcher's would -- and a leading ~ by the home
the cell reads."
  (let ((out (make-string-output-stream))
        (n (length text)))
    (loop with index = 0
          while (< index n)
          for character = (char text index)
          do (cond
               ((char= character #\%)
                (let ((close (position #\% text :start (1+ index))))
                  (if close
                      (progn (princ (or (uiop:getenv (subseq text (1+ index) close)) "") out)
                             (setf index (1+ close)))
                      (progn (princ character out) (incf index)))))
               ((char= character #\$)
                (let ((after (1+ index)))
                  (if (and (< after n) (char= (char text after) #\{))
                      (let ((close (position #\} text :start after)))
                        (if close
                            (progn (princ (or (uiop:getenv (subseq text (1+ after) close)) "") out)
                                   (setf index (1+ close)))
                            (progn (princ character out) (incf index))))
                      (let ((end (or (position-if-not
                                      (lambda (c) (or (alphanumericp c) (char= c #\_)))
                                      text :start after)
                                     n)))
                        (if (> end after)
                            (progn (princ (or (uiop:getenv (subseq text after end)) "") out)
                                   (setf index end))
                            (progn (princ character out) (incf index)))))))
               ((and (char= character #\~) (zerop index))
                (princ (namestring (read-home)) out) (incf index))
               (t (princ character out) (incf index))))
    (get-output-stream-string out)))

(defun absolute-path-p (text)
  "TEXT as a path a loader would treat as absolute."
  (and (plusp (length text))
       (handler-case (uiop:absolute-pathname-p (uiop:parse-native-namestring text))
         (error () nil))))

;;; --- reg query's output ------------------------------------------------------

(defun blankp (character)
  "CHARACTER is a space or a tab."
  (member character '(#\Space #\Tab)))

(defun skip-blanks (line index)
  "INDEX moved past the blanks at it."
  (loop for at = index then (1+ at)
        while (and (< at (length line)) (blankp (char line at)))
        finally (return at)))

(defun blank-run-at (line index)
  "The index where two blanks begin at or after INDEX, or NIL."
  (loop for at from index below (1- (length line))
        when (and (blankp (char line at)) (blankp (char line (1+ at))))
          return at))

(defun parse-reg-line (line)
  "One line of `reg query' output as (:name NAME :type TYPE :data DATA), or NIL
for a key header or a line that is not a value. The three columns are the
name, the type and everything after them, so a command keeping its own blank
runs survives."
  (let ((start (skip-blanks line 0)))
    (when (and (plusp start) (< start (length line)))
      (let* ((name-end (or (blank-run-at line start) (length line)))
             (name (subseq line start name-end))
             (type-start (skip-blanks line name-end))
             (type-end (or (blank-run-at line type-start) (length line)))
             (type (subseq line type-start type-end))
             (data-start (min (skip-blanks line type-end) (length line))))
        (when (uiop:string-prefix-p "REG_" type)
          (list :name name :type type
                :data (string-right-trim '(#\Space #\Tab #\Return)
                                         (subseq line data-start))))))))

(defun hex-value (character)
  "CHARACTER's value as a hex digit, or NIL."
  (cond ((char<= #\0 character #\9) (- (char-code character) (char-code #\0)))
        ((char<= #\a character #\f) (+ 10 (- (char-code character) (char-code #\a))))
        ((char<= #\A character #\F) (+ 10 (- (char-code character) (char-code #\A))))
        (t nil)))

(defun hex-bytes (text)
  "TEXT's hex digits as a vector of bytes; blanks and commas between them are
separators, and an odd trailing digit is dropped."
  (let* ((digits (remove-if-not #'hex-value text))
         (count (floor (length digits) 2)))
    (coerce (loop for index from 0 below count
                  collect (+ (* 16 (hex-value (char digits (* 2 index))))
                             (hex-value (char digits (1+ (* 2 index))))))
            'vector)))

(defun filetime-universal (bytes offset)
  "The eight-byte little-endian FILETIME at OFFSET as universal time, or NIL
when it is zero."
  (let ((stamp (loop for index from offset below (+ offset 8)
                     for byte = (aref bytes index)
                     for scale = 1 then (* scale 256)
                     sum (* byte scale))))
    (when (plusp stamp)
      (+ (- (floor stamp 10000000) 11644473600) 2208988800))))

(defun decode-approval (data)
  "Explorer's StartupApproved blob as (:state STATE :changed STAMP). The low
byte is 2 or 3 for an entry left on, 6 or 7 for one switched off in Task
Manager, and anything else is reported unknown with the byte, never guessed;
bytes 4 to 12 are the FILETIME of that decision."
  (let* ((bytes (hex-bytes (princ-to-string data)))
         (count (length bytes)))
    (when (plusp count)
      (let ((first (aref bytes 0)))
        (list :state (cond ((member first '(2 3)) "enabled")
                           ((member first '(6 7)) "disabled")
                           (t (format nil "unknown (state byte ~d)" first)))
              :changed (and (>= count 12) (iso-from-universal (filetime-universal bytes 4))))))))

(defun reg-values (key)
  "KEY's values as (:name :type :data) lists, read through *REG-QUERY*; a key
that is not there, or cannot be read, has none."
  (loop for line in (funcall *reg-query* key)
        for value = (parse-reg-line line)
        when value collect value))

(defun run-reg-query (key)
  "The lines `reg query KEY' prints, or NIL. The one process this cell starts."
  (handler-case
      (let ((text (uiop:run-program (list "reg" "query" key)
                                     :output :string :ignore-error-status t)))
        (and text (plusp (length text))
             (uiop:split-string text :separator '(#\Newline))))
    (error () nil)))


;;; --- the Windows half: six Run keys, StartupApproved, two folders ------------

(defparameter +run-sources+
  '(("HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Run"
     "registry-hkcu-run" "user")
    ("HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\RunOnce"
     "registry-hkcu-runonce" "user")
    ("HKLM\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Run"
     "registry-hklm-run" "machine")
    ("HKLM\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\RunOnce"
     "registry-hklm-runonce" "machine")
    ("HKLM\\SOFTWARE\\WOW6432Node\\Microsoft\\Windows\\CurrentVersion\\Run"
     "registry-hklm-run-32" "machine")
    ("HKLM\\SOFTWARE\\WOW6432Node\\Microsoft\\Windows\\CurrentVersion\\RunOnce"
     "registry-hklm-runonce-32" "machine"))
  "The six keys Windows launches from, and the label each entry is reported
under. The WOW6432Node pair exists only on a 64-bit install: on a 32-bit one
it answers nothing, which is the honest answer.")

(defparameter +approval-sources+
  '(("HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\StartupApproved\\Run"
     "registry-hkcu-run")
    ("HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\StartupApproved\\RunOnce"
     "registry-hkcu-runonce")
    ("HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\StartupApproved\\StartupFolder"
     "folder-user")
    ("HKLM\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Explorer\\StartupApproved\\Run"
     "registry-hklm-run")
    ("HKLM\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Explorer\\StartupApproved\\RunOnce"
     "registry-hklm-runonce")
    ("HKLM\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Explorer\\StartupApproved\\StartupFolder"
     "folder-common"))
  "Where Explorer keeps the enabled/disabled verdict for each source label.")

(defparameter +shell-folders-key+
  "\\Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\User Shell Folders"
  "The key a redirected Startup folder is found under.")

(defun shell-folder (value default &optional (hive "HKCU"))
  "VALUE of Explorer's User Shell Folders, expanded, or DEFAULT. A Startup
folder OneDrive or a policy redirects is found where the person has it, not
where the default says."
  (let* ((rows (reg-values (concatenate 'string hive +shell-folders-key+)))
         (row (find value rows :test #'string-equal :key (lambda (r) (getf r :name)))))
    (expand-vars (or (and row (plusp (length (getf row :data))) (getf row :data)) default))))

(defun windows-folder-sources ()
  "The two Startup folders as (DIRECTORY SOURCE SCOPE), redirections honored."
  (let ((home (home-override)))
    (list (list (if home
                    (merge-pathnames "AppData/Roaming/Microsoft/Windows/Start Menu/Programs/Startup/" home)
                    (shell-folder "Startup" "%APPDATA%\\Microsoft\\Windows\\Start Menu\\Programs\\Startup"))
                "folder-user" "user")
          (list (if home
                    (merge-pathnames "ProgramData/Microsoft/Windows/Start Menu/Programs/Startup/" home)
                    (shell-folder "Common Startup"
                                  "%ProgramData%\\Microsoft\\Windows\\Start Menu\\Programs\\Startup"
                                  "HKLM"))
                "folder-common" "machine"))))

(defun file-facts (file)
  "FILE's path, size and last change, or NIL when it cannot be read."
  (handler-case
      (list :path (namestring file)
            :size (with-open-file (in file :element-type '(unsigned-byte 8))
                    (file-length in))
            :modified (iso-from-universal (file-write-date file)))
    (error () nil)))

(defun folder-entries (directory source scope)
  "Every file in DIRECTORY as an entry, desktop.ini excepted: a shell launches
anything in a Startup folder, so no extension is filtered."
  (loop for file in (folder-files directory)
        for name = (file-namestring file)
        unless (string-equal name "desktop.ini")
          collect (list :name name
                        :command (namestring file)
                        :source source
                        :scope scope
                        :file (file-facts file))))

(defun file-stem (name)
  "NAME without its extension, downcased."
  (let* ((name (string-downcase name))
         (dot (position #\. name :from-end t)))
    (if (and dot (plusp dot)) (subseq name 0 dot) name)))

(defun windows-approvals ()
  "Explorer's enabled/disabled verdicts as an EQUAL hash of (SOURCE . NAME
downcased) => (:state :changed)."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (row +approval-sources+ table)
      (destructuring-bind (key label) row
        (dolist (value (reg-values key))
          (let ((decoded (decode-approval (getf value :data))))
            (when decoded
              (setf (gethash (cons label (string-downcase (princ-to-string (getf value :name))))
                              table)
                    decoded))))))))

(defun attach-approvals (entries approvals)
  "ENTRIES with Explorer's verdict folded in as :approved, and the records left
behind by entries that are gone -- an uninstalled program keeps its record."
  (let ((known '()))
    (values
     (loop for entry in entries
           for source = (getf entry :source)
           for name = (string-downcase (princ-to-string (getf entry :name)))
           for record = (or (gethash (cons source name) approvals)
                            (and (uiop:string-prefix-p "folder-" source)
                                 (gethash (cons source (file-stem name)) approvals)))
           do (push (cons source name) known)
           collect (list* :approved record entry))
     (loop for (source . name) being the hash-keys of approvals using (hash-value record)
           unless (member (cons source name) known :test #'equal)
             collect (list :name name :source source
                           :state (getf record :state) :changed (getf record :changed))))))

(defun windows-probe ()
  "Every Windows autostart entry, and the approval records left behind."
  (multiple-value-bind (entries orphans)
      (attach-approvals
       (append (loop for (key label scope) in +run-sources+
                  nconc (loop for value in (reg-values key)
                              collect (list :name (getf value :name)
                                            :command (or (getf value :data) "")
                                            :source label
                                            :scope scope)))
               (loop for (directory label scope) in (windows-folder-sources)
                     nconc (folder-entries directory label scope)))
       (windows-approvals))
    (values entries orphans)))

;;; --- the Linux half: the XDG autostart folders ---------------------------------

(defparameter +desktop-fields+
  '(("Name" . :name) ("Exec" . :exec) ("TryExec" . :try-exec)
    ("Hidden" . :hidden) ("X-GNOME-Autostart-enabled" . :gnome-enabled))
  "The [Desktop Entry] keys this cell reads; every other line is not its
business.")

(defun desktop-facts (file)
  "FILE's [Desktop Entry] group as a plist: :name :exec :try-exec :hidden
:gnome-enabled. A file that cannot be read has none."
  (handler-case
      (with-open-file (in file :if-does-not-exist nil)
        (when in
          (let ((facts '())
                (inside nil))
            (loop for line = (read-line in nil nil)
                  while line
                  for text = (string-trim '(#\Space #\Tab #\Return) line)
                  do (cond ((and (plusp (length text)) (char= (char text 0) #\[))
                            (setf inside (string-equal text "[Desktop Entry]")))
                           ((or (not inside) (zerop (length text)) (char= (char text 0) #\#)))
                           (t (let ((equals (position #\= text)))
                                (when equals
                                  (let ((slot (cdr (assoc (subseq text 0 equals)
                                                          +desktop-fields+
                                                          :test #'string-equal))))
                                    (when slot
                                      (setf (getf facts slot) (subseq text (1+ equals))))))))))
            facts)))
    (error () nil)))

(defun strip-field-codes (command)
  "COMMAND without a desktop entry's %f %U %i field codes, trimmed."
  (let ((out (make-string-output-stream))
        (skip nil))
    (loop for character across command
          do (cond (skip (setf skip nil))
                   ((char= character #\%) (setf skip t))
                   (t (princ character out))))
    (string-trim '(#\Space #\Tab) (get-output-stream-string out))))

(defun xdg-autostart-directories ()
  "The XDG autostart folders this account reads, system ones last:
((DIRECTORY SOURCE SCOPE) ...)."
  (let* ((config (or (uiop:getenv "XDG_CONFIG_HOME")
                      (merge-pathnames ".config/" (uiop:ensure-directory-pathname (read-home)))))
         (system (or (uiop:getenv "XDG_CONFIG_DIRS") "/etc/xdg")))
    (append
     (list (list (merge-pathnames "autostart/"
                                   (uiop:ensure-directory-pathname (pathname config)))
                 "xdg-user" "user"))
     (loop for directory in (uiop:split-string system :separator '(#\:))
           when (plusp (length directory))
             collect (list (merge-pathnames "autostart/"
                                             (uiop:ensure-directory-pathname (pathname directory)))
                           "xdg-system" "machine")))))

(defun desktop-entry (file facts source scope file-name)
  "One XDG desktop file as an entry, switched off as its own keys say."
  (let* ((name (getf facts :name))
         (hidden (equal "true" (string-downcase (or (getf facts :hidden) ""))))
         (gnome (equal "false" (string-downcase (or (getf facts :gnome-enabled) ""))))
         (command (strip-field-codes (or (getf facts :exec) ""))))
    (list :name (if (and name (plusp (length name))) name (file-stem file-name))
          :command (if (plusp (length command)) command (namestring file))
          :source source
          :scope scope
          :approved (when (or hidden gnome) (list :state "disabled" :changed nil))
          :file (file-facts file))))

(defun linux-probe ()
  "Every XDG autostart entry; a user's file shadows a system one of the same
name, which is what the desktop itself does."
  (let ((seen '())
        (entries '()))
    (dolist (row (xdg-autostart-directories))
      (destructuring-bind (directory source scope) row
        (dolist (file (folder-files directory "*.desktop"))
          (let ((name (file-namestring file)))
            (unless (member name seen :test #'string=)
              (push name seen)
              (let ((facts (desktop-facts file)))
                (when facts
                  (push (desktop-entry file facts source scope name) entries))))))))
    (values (nreverse entries) '())))

;;; --- the macOS half: launchd's agents and daemons ------------------------------

(defparameter +launchd-sources+
  '(("Library/LaunchAgents" "launchagent-user" "user")
    ("/Library/LaunchAgents" "launchagent-common" "machine")
    ("/Library/LaunchDaemons" "launchdaemon-common" "machine"))
  "The launchd folders this account reads; a relative one is under the home.")

(defun launchd-directories ()
  "The launchd folders as (DIRECTORY SOURCE SCOPE)."
  (loop for (path source scope) in +launchd-sources+
        collect (list (if (uiop:absolute-pathname-p (pathname path))
                          (pathname path)
                          (merge-pathnames path
                                           (uiop:ensure-directory-pathname (read-home))))
                      source scope)))

(defun plist-after (text key)
  "TEXT from just past KEY's <key> tag, or NIL."
  (let ((at (search (format nil "<key>~a</key>" key) text)))
    (and at (subseq text (min (length text) (+ at (length key) 11))))))

(defun plist-string (text key)
  "The <string> right after KEY's tag in TEXT, or NIL."
  (let* ((after (plist-after text key))
         (open (and after (search "<string>" after)))
         (close (and open (search "</string>" after :start2 (+ open 8)))))
    (and close (string-trim '(#\Space #\Tab #\Return) (subseq after (+ open 8) close)))))

(defun plist-true-p (text key)
  "Whether TEXT's KEY is <true/> before its next tag."
  (let* ((after (plist-after text key))
         (yes (and after (search "<true/>" after)))
         (no (and after (search "<false/>" after))))
    (and yes (or (null no) (< yes no)))))

(defun plist-facts (file)
  "FILE's launchd job as a plist: :label :program :arguments :disabled, read
from the XML a .plist usually is; :binary T for a binary plist."
  (handler-case
      (with-open-file (in file :if-does-not-exist nil)
        (when in
          (let ((text (with-output-to-string (out)
                        (loop for line = (read-line in nil nil)
                              while line
                              do (write-line line out)))))
            (if (uiop:string-prefix-p "bplist" text)
                (list :binary t)
                (list :label (plist-string text "Label")
                      :program (plist-string text "Program")
                      :arguments (plist-string text "ProgramArguments")
                      :disabled (plist-true-p text "Disabled"))))))
    (error () nil)))

(defun quoted-if-needed (program)
  "PROGRAM, quoted when it has a blank in it."
  (if (and program (position #\Space program))
      (format nil "\"~a\"" program)
      program))

(defun launchd-entry (file facts source scope file-name)
  "One launchd job as an entry: its label names it, its program is the command."
  (let* ((program (or (getf facts :program) (getf facts :arguments)))
         (binary (and (getf facts :binary) (file-stem file-name))))
    (list :name (or binary (getf facts :label) (file-stem file-name))
          :command (if program (quoted-if-needed program) (namestring file))
          :source source
          :scope scope
          :note (and binary "a binary plist: the job is named by its file, and its program cannot be read here")
          :approved (when (getf facts :disabled) (list :state "disabled" :changed nil))
          :file (file-facts file))))

(defun macos-probe ()
  "Every launchd agent and daemon this account reads."
  (let ((entries '()))
    (dolist (row (launchd-directories))
      (destructuring-bind (directory source scope) row
        (dolist (file (folder-files directory "*.plist"))
          (let ((facts (plist-facts file)))
            (when facts
              (push (launchd-entry file facts source scope (file-namestring file)) entries))))))
    (values (nreverse entries) '())))

;;; --- the machine, read as the platform it is --------------------------------

(defun probe-now ()
  "Every autostart entry this machine has, and the records left behind by ones
that are gone: the platform's own reader, and the default of *PROBE*."
  (ecase (platform)
    (:windows (windows-probe))
    (:darwin (macos-probe))
    (:linux (linux-probe))))

;;; --- one entry, its verdict, and the binary it names ---------------------------

(defun enabled-state (entry)
  "ENTRY's enabled/disabled verdict. No record means it runs, which is every
one of these platforms' default."
  (or (getf (getf entry :approved) :state) "enabled"))

(defun clip-text (text chars)
  "TEXT as one line no longer than CHARS, an ellipsis when it was cut."
  (let ((text (string-trim '(#\Space #\Tab #\Return #\Linefeed)
                            (princ-to-string (or text "")))))
    (if (<= (length text) chars)
        text
        (concatenate 'string (subseq text 0 chars) "..."))))

(defun entry-row (entry)
  "ENTRY as the table's row."
  (list :name (getf entry :name)
        :command (clip-text (getf entry :command) *command-chars*)
        :source (getf entry :source)
        :scope (getf entry :scope)
        :enabled (enabled-state entry)
        :changed (getf (getf entry :approved) :changed)))

(defun command-candidates (text)
  "The paths COMMAND might name, longest first: the quoted path, or every
whitespace prefix of it, the way a loader tries an unquoted one."
  (if (char= (char text 0) #\")
      (let ((close (position #\" text :start 1)))
        (list (if close (subseq text 1 close) (string-trim "\"" text))))
      (let ((words (remove "" (uiop:split-string text :separator '(#\Space #\Tab))
                           :test #'string=)))
        (loop for count from (length words) downto 1
              collect (format nil "~{~a~^ ~}" (subseq words 0 count))))))

(defun binary-state (command)
  "The binary COMMAND launches, whether it is on disk, and a note when it is
not or when the path is relative and nobody can say without the loader."
  (let ((text (and command (string-trim '(#\Space #\Tab) (princ-to-string command)))))
    (when (and text (plusp (length text)))
      (let ((candidates (command-candidates text)))
        (loop for candidate in candidates
              for resolved = (expand-vars candidate)
              do (cond ((not (absolute-path-p resolved))
                        (return (list :path resolved :present nil
                                      :note "the path is not absolute; not checked")))
                       ((funcall *file-exists-p* resolved)
                        (return (list :path resolved :present t :note nil))))
              finally (return (list :path (expand-vars (car (last candidates)))
                                    :present nil
                                    :note "the launched binary is not on disk any more")))))))

;;; --- the table's answer -------------------------------------------------------

(defun row-json (row)
  "ROW as the JSON object a tool answer carries."
  (nlk:make-json-object "name" (getf row :name)
                         "command" (getf row :command)
                         "source" (getf row :source)
                         "scope" (getf row :scope)
                         "enabled" (getf row :enabled)
                         "changed" (getf row :changed)))

(defun row-matches-p (row query scope include-disabled)
  "Whether ROW survives the table's three filters."
  (and (or (zerop (length query))
           (search query (string-downcase (princ-to-string (getf row :name))))
           (search query (string-downcase (princ-to-string (getf row :command)))))
       (or (zerop (length scope)) (string-equal scope (or (getf row :scope) "")))
       (or include-disabled (not (equal "disabled" (getf row :enabled))))))

(defun table-notes (listed matching rows orphans limit scope)
  "The notes that read the table: how many were listed, what was left out, and
what to change to see it."
  (let ((notes '())
        (disabled (count "disabled" rows :key (lambda (row) (getf row :enabled))
                                    :test #'equal))
        (machine (count "machine" rows :key (lambda (row) (getf row :scope))
                                   :test #'equal)))
    (push (format nil "~d of ~d matching entr~:p listed, ~d autostart entr~:p in total, ~d machine-scoped"
                  (length listed) (length matching) (length rows) machine)
          notes)
    (when (< (length listed) (length matching))
      (push (format nil "stopped at limit=~d; raise it or narrow the query" limit) notes))
    (when (plusp disabled)
      (push (format nil "~d switched off; include_disabled=false hides them" disabled) notes))
    (when orphans
      (push (format nil "~d approval record~:p remain for entries that are gone (an uninstalled program leaves them behind)"
                    (length orphans))
            notes))
    (when (null listed)
      (push "nothing matched; drop the query or the filters" notes))
    (when (and (plusp (length scope))
               (not (member scope '("user" "machine") :test #'string-equal)))
      (push "scope is one of: machine, user" notes))
    (nreverse notes)))

(defun orphan-json (orphan)
  "ORPHAN as the JSON object the table reports it in."
  (nlk:make-json-object "name" (getf orphan :name)
                         "source" (getf orphan :source)
                         "state" (getf orphan :state)
                         "changed" (getf orphan :changed)))

(defun table-payload (&key (query "") (scope "") (include-disabled t) (limit 100))
  "The table's answer as a JSON object: the rows, and what a person needs to
read them. Read-only; nothing here changes a start-up entry."
  (let ((query (string-downcase (string-trim '(#\Space #\Tab) (or query ""))))
        (scope (string-downcase (string-trim '(#\Space #\Tab) (or scope ""))))
        (limit (max 1 (min (or limit 100) *max-rows*))))
    (handler-case
        (multiple-value-bind (entries orphans) (funcall *probe*)
          (let* ((rows (mapcar #'entry-row entries))
                 (matching (remove-if-not (lambda (row) (row-matches-p row query scope include-disabled))
                                           rows))
                 (listed (subseq matching 0 (min limit (length matching))))
                 (kept (subseq orphans 0 (min 20 (length orphans))))
                 (members (list "ok" t
                                "startup" (coerce (mapcar #'row-json listed) 'vector)
                                "notes" (coerce (table-notes listed matching rows orphans limit scope)
                                                'vector))))
            (when orphans
              (setf members (append members
                                     (list "leftover_approvals"
                                           (coerce (mapcar #'orphan-json kept) 'vector)))))
            (apply #'nlk:make-json-object members)))
      (error (cause)
        (nlk:make-json-object "ok" nil "code" "unreadable"
                               "error" (format nil "~a: ~a" (type-of cause)
                                               (nlk:one-line cause)))))))

;;; --- one entry in full ---------------------------------------------------------

(defun find-entry (entries wanted)
  "The entry of ENTRIES the person means by WANTED: its name exactly, else the
first whose name or command contains it."
  (or (find-if (lambda (entry) (string-equal wanted (princ-to-string (getf entry :name))))
                entries)
      (find-if (lambda (entry)
                 (or (search wanted (string-downcase (princ-to-string (getf entry :name))))
                     (search wanted (string-downcase (princ-to-string (getf entry :command))))))
               entries)))

(defun detail-notes (entry row binary)
  "What the detail adds to the entry's own fields, in words."
  (let ((notes '()))
    (when (and binary (null (getf binary :present)))
      (push (getf binary :note) notes))
    (when (equal "disabled" (getf row :enabled))
      (push "switched off: it does not run, but it is still registered" notes))
    (when (uiop:string-prefix-p "unknown" (or (getf row :enabled) ""))
      (push "the platform's verdict byte is not one of the documented states; reported as-is" notes))
    (when (getf entry :note)
      (push (getf entry :note) notes))
    (nreverse notes)))

(defun detail-members (entry)
  "ENTRY's answer as the member/value pairs a JSON object is built from."
  (let* ((row (entry-row entry))
         (binary (binary-state (getf entry :command)))
         (file (getf entry :file)))
    (list "ok" t
          "name" (getf row :name)
          "command" (getf entry :command)
          "source" (getf row :source)
          "scope" (getf row :scope)
          "enabled" (getf row :enabled)
          "changed" (getf row :changed)
          "binary" (and binary
                         (nlk:make-json-object "path" (getf binary :path)
                                                "present" (getf binary :present)
                                                "note" (getf binary :note)))
          "file" (and file
                       (nlk:make-json-object "path" (getf file :path)
                                              "size" (getf file :size)
                                              "modified" (getf file :modified)))
          "notes" (coerce (detail-notes entry row binary) 'vector))))

(defun detail-payload (name)
  "One entry in full as a JSON object: the command, the binary it resolves to,
its file when it lives in a folder, and the verdict on it."
  (let ((wanted (string-downcase (string-trim '(#\Space #\Tab)
                                               (princ-to-string (or name ""))))))
    (handler-case
        (let* ((entries (funcall *probe*))
               (entry (find-entry entries wanted)))
          (if entry
              (apply #'nlk:make-json-object (detail-members entry))
              (nlk:make-json-object
               "ok" nil "code" "not_registered"
               "error" (format nil "no autostart entry named ~a is registered" wanted)
               "hint" "startup_table lists every entry with its exact name")))
      (error (cause)
        (nlk:make-json-object "ok" nil "code" "unreadable"
                               "error" (format nil "~a: ~a" (type-of cause)
                                               (nlk:one-line cause)))))))

;;; --- the answers in words -----------------------------------------------------

(defun table-text (&key (query "") (scope "") (include-disabled t) limit)
  "The table in words: a line an entry, then the notes that read them."
  (let ((payload (table-payload :query query :scope scope
                                 :include-disabled include-disabled
                                 :limit (or limit (setting :max-rows)))))
    (if (null (nlk:json-value payload :boolean "ok"))
        (format nil "Startup could not read this machine: ~a"
                (nlk:json-value payload :string "error"))
        (with-output-to-string (out)
          (loop for row across (nlk:json-array payload "startup")
                do (format out "~9a  ~20a  ~a  ->  ~a~%"
                           (nlk:json-value row :string "enabled")
                           (nlk:json-value row :string "source")
                           (nlk:json-value row :string "name")
                           (nlk:json-value row :string "command")))
          (terpri out)
          (loop for note across (nlk:json-array payload "notes")
                do (format out "~a~%" note))))))

(defun detail-text (name)
  "One entry in full, in words."
  (let ((payload (detail-payload name)))
    (if (null (nlk:json-value payload :boolean "ok"))
        (format nil "Startup: ~a -- ~a"
                (nlk:json-value payload :string "error")
                (or (nlk:json-value payload :string "hint") ""))
        (with-output-to-string (out)
          (format out "~a  (~a, ~a)~%" (nlk:json-value payload :string "name")
                  (nlk:json-value payload :string "source")
                  (nlk:json-value payload :string "scope"))
          (format out "  enabled: ~a~@[ (changed ~a)~]~%"
                  (nlk:json-value payload :string "enabled")
                  (nlk:json-value payload :string "changed"))
          (format out "  command: ~a~%" (nlk:json-value payload :string "command"))
          (let ((binary (nlk:json-value payload :object "binary")))
            (when binary
              (format out "  binary:  ~a (~:[gone~;on disk~])~%"
                      (nlk:json-value binary :string "path")
                      (nlk:json-value binary :boolean "present"))))
          (let ((file (nlk:json-value payload :object "file")))
            (when file
              (format out "  file:    ~a, ~a bytes, changed ~a~%"
                      (nlk:json-value file :string "path")
                      (nlk:json-value file :integer "size")
                      (nlk:json-value file :string "modified"))))
          (loop for note across (nlk:json-array payload "notes")
                do (format out "  note: ~a~%" note))))))

;;; --- /startup ----------------------------------------------------------------

(defun run-slash (args session-id)
  "The /startup command: no argument answers the table, a name answers one
entry, and --json answers the whole payload."
  (declare (ignore session-id))
  (let* ((words (remove "" (uiop:split-string (or args "") :separator '(#\Space #\Tab))
                        :test #'string=))
         (json (member "--json" words :test #'string-equal))
         (rest (remove "--json" words :test #'string-equal))
         (name (first rest)))
    (handler-case
        (cond ((null name)
               (if json
                   (nlk:pretty-json (table-payload :limit (setting :max-rows)))
                   (table-text)))
              ((member name '("all" "--all") :test #'string-equal)
               (if json
                   (nlk:pretty-json (table-payload :include-disabled t :limit (setting :max-rows)))
                   (table-text :include-disabled t)))
              (t (if json
                     (nlk:pretty-json (detail-payload name))
                     (detail-text name))))
      (error (condition)
        (format nil "Startup could not finish: ~a" (nlk:one-line condition))))))

;;; --- what a model may ask ----------------------------------------------------

(defparameter +table-doc+
  "Answer 'what launches itself when this machine signs in': every autostart
entry, where each comes from (on Windows the six Run/RunOnce keys, Explorer's
StartupApproved verdicts and the two Startup folders; on Linux the XDG
autostart folders; on macOS the launchd agents and daemons), whether it is
user- or machine-scoped, whether the person switched it off, and when that
changed. Use it to slim a slow sign-in, to catch what added itself since last
month, and to find a launcher whose binary is gone. Read-only: nothing is
enabled, disabled or deleted."
  "What the model is told the table does.")

(defparameter +detail-doc+
  "One autostart entry in full: the exact command it launches, the binary that
command resolves to (quoting, arguments and environment variables folded in)
and whether that binary is still on disk, the file's size and last change when
it lives in a Startup folder, and the platform's enabled/disabled verdict with
when it last changed. Use it after startup_table names a suspect. Read-only."
  "What the model is told the detail does.")

(nlk:define-contract startup-table-call
  (:doc +table-doc+)
  ("query" :text :default ""
            :doc "case-insensitive substring of an entry's name or its command, e.g. 'onedrive' or 'cc switch'")
  ("scope" :text :default "" :options '("user" "machine")
           :doc "only 'user' or only 'machine' entries; empty for both")
  ("include_disabled" :boolean :default t
                      :doc "whether entries switched off in Task Manager or the desktop's settings are listed")
  ("limit" :integer
           :doc "rows to return from 1 to 400; the section's max_rows when absent"))

(nlk:define-contract startup-detail-call
  (:doc +detail-doc+)
  (:unknown :refuse)
  ("name" :text :required t
          :doc "the entry's name as startup_table showed it, case-insensitive, e.g. 'OneDrive' or 'SecurityHealth'"))

(defun table-tool (call)
  "The startup_table tool: every autostart entry, filtered as CALL asks."
  (nlk:pretty-json
   (table-payload :query (startup-table-call-query call)
                   :scope (startup-table-call-scope call)
                   :include-disabled (startup-table-call-include-disabled call)
                   :limit (or (startup-table-call-limit call) (setting :max-rows)))))

(defun detail-tool (call)
  "The startup_detail tool: one entry in full."
  (nlk:pretty-json (detail-payload (startup-detail-call-name call))))

;;; --- the verbs a person calls from eval ---------------------------------------

(define-verb startup-table ()
  "Every autostart entry on this machine, one line each: what it is, where it
comes from, and whether it was switched off."
  (table-text))

(define-verb startup-detail (name)
  "One autostart entry in full, by the name startup-table showed."
  (detail-text name))

(define-verb entries ()
  "Every autostart entry this machine has, as plists -- the reading the two
tools are built on."
  (funcall *probe*))

;;; --- the declaration --------------------------------------------------------

(setf *probe* #'probe-now
      *file-exists-p* (lambda (path) (and (probe-file path) t))
      *reg-query* #'run-reg-query)

(nle:define-cell startup
  (:section ("startup")
    (:guide "nothing here is required: with no section the cell reads this machine's own autostart entries; max_rows bounds the table; home points the user-scope sources at another home, a mounted disk or another profile (the Windows registry is always the live one)")
    ("max_rows" :integer :default 100 :min 1
                :doc "rows one table answer carries, at most 400")
    ("home" :path
            :doc "read this home's user-scope sources instead of the running user's"))
  (:tool "startup_table" (nlk:contract 'startup-table-call) 'table-tool)
  (:tool "startup_detail" (nlk:contract 'startup-detail-call) 'detail-tool)
  (:command "startup" 'run-slash
            :description "What launches itself at sign-in, and what was switched off"
            :argument-hint "NAME | --json")
  (:help :startup "startup: what launches itself at sign-in, and what was switched off"
         +primer+)
  (:start (lambda ()
            (setf *platform* (platform))
            (nle:on-stop (lambda () (setf *platform* nil))))))
