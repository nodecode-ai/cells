;;;; cell.lisp --- installed software, swept from the four Uninstall keys.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Why the registry and not WMI: Win32_Product sees only MSI packages,
;;;; reconfigures each one while enumerating (a documented, repair-triggering
;;;; side effect) and costs a PowerShell spawn per query. The Uninstall keys
;;;; are where Apps & Features itself reads from: four keys cover the machine's
;;;; 64-bit and 32-bit views and the user's, with no installer touched and no
;;;; elevation needed to read them.
;;;;
;;;; The original plugin read them through CPython's winreg. This cell reads
;;;; them through reg.exe, the registry tool Windows ships, which prints the
;;;; same values as `reg query KEY /s': one header line per subkey, then four
;;;; spaces, the value's name, its type and its data. Nothing here writes a
;;;; key, runs an uninstaller or reads a hive the four keys do not name; a
;;;; missing key is an empty view, which is the honest answer, never an error.
;;;;
;;;; *REG-RUNNER* is the one seam: a query builds the command line and hands
;;;; it over, so a test asserts the line and answers the output with no
;;;; registry, no Windows and no reg.exe.

(in-package #:nodecode-windows-apps)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Windows Apps answers 'what is installed on this machine' from the four
Uninstall registry keys Apps & Features reads: the machine's 64-bit and 32-bit
views and the user's. Two tools, also callable from eval in the windows-apps
package:
  win_app_table   {query, view, include_system_components, limit} every program
                  with its version, publisher, size, install date and view; the
                  same name in two views is one row marked as a dual install;
                  entries the publisher hid from Settings are filtered out
                  unless include_system_components is set. In eval:
                  (windows-apps:win-app-table :query \"python\").
  win_app_detail  {name} one program in full -- install location and whether
                  it is still on disk, help and about links, comments -- and a
                  reading of its uninstall command (an MSI product code, a
                  setup executable, an appx removal or a script). In eval:
                  (windows-apps:win-app-detail \"OBS Studio\").
Read-only: keys are enumerated and uninstall strings are quoted, never run.
Windows only: the cell runs reg.exe, which the section's `reg' names. A machine
without it answers with what reg said."
  "What (help :windows-apps) answers while the cell runs.")

;;; --- where installed software is registered ------------------------------------

(defparameter +sources+
  '(("HKLM" "Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall" "machine-64")
    ("HKLM" "Software\\WOW6432Node\\Microsoft\\Windows\\CurrentVersion\\Uninstall" "machine-32")
    ("HKCU" "Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall" "user-64")
    ("HKCU" "Software\\WOW6432Node\\Microsoft\\Windows\\CurrentVersion\\Uninstall" "user-32"))
  "The four Uninstall keys, in the order they are read and shown: machine
entries before the user's, 64-bit before 32-bit. A key that does not exist on
a given install comes back empty.")

(defparameter +views+ '("machine-64" "machine-32" "user-64" "user-32")
  "The view labels, which are also the values win_app_table's `view' takes.")

(defparameter *max-rows* 400
  "The most rows one table answers, as the original capped it.")
(defparameter *publisher-chars* 60)
(defparameter *command-chars* 300)

;;; --- the one seam -------------------------------------------------------------

(defparameter *reg-runner* 'run-reg
  "How a query runs the registry tool: (funcall *REG-RUNNER* ARGV) =>
(values STDOUT STDERR STATUS), STATUS an exit code or :TIMEOUT.")

(defun run-reg (argv)
  "Run ARGV to completion or the section's timeout, => (values STDOUT STDERR STATUS)."
  (handler-case (nlk:run-bounded argv :seconds (setting :timeout-seconds))
    (error (condition)
      ;; reg.exe absent is the ordinary first state of this cell off Windows,
      ;; so it is words the model can act on rather than a signalled error;
      ;; 127 is what a shell says for a program it cannot find.
      (values nil
              (format nil "~a could not be run: ~a. The installed-software registry is Windows only; set the windows-apps section's reg to the registry tool's argv, e.g. [\"reg\"]."
                      (first argv) (nlk:one-line (princ-to-string condition)))
              127))))

;;; --- small readers -------------------------------------------------------------

(defun nonblank (value)
  "VALUE as a trimmed string, or NIL when it is absent or only spaces."
  (when (stringp value)
    (let ((text (string-trim '(#\Space #\Tab #\Return #\Linefeed) value)))
      (and (plusp (length text)) text))))

(defun clamp (value low high)
  (max low (min high value)))

(defun number-value (text)
  "TEXT as an integer. A REG_DWORD prints as 0x…, so hex is read as hex;
a decimal string is read as decimal; anything else answers NIL."
  (when (stringp text)
    (let ((text (string-trim '(#\Space #\Tab) text)))
      (when (plusp (length text))
        (handler-case
            (if (and (> (length text) 2)
                     (char= (char text 0) #\0)
                     (find (char text 1) "xX"))
                (parse-integer text :start 2 :radix 16)
                (parse-integer text))
          (error () nil))))))

(defun dword-on-p (value)
  "A publisher's flag that means 'on' when written as 1, 0x1, true or yes."
  (let ((number (number-value value)))
    (if number
        (plusp number)
        (member (string-downcase (or (nonblank value) "")) '("true" "yes") :test #'string=))))

(defun size-mb (raw)
  "EstimatedSize is a DWORD in kilobytes; surfaced as megabytes."
  (let ((kb (number-value raw)))
    (when kb (/ (coerce kb 'double-float) 1024.0))))

(defun install-date (raw)
  "InstallDate is written as YYYYMMDD or a slashed MM/DD/YYYY; both read as
YYYY-MM-DD, and anything else is reported as the publisher wrote it."
  (let ((text (nonblank raw)))
    (when text
      (cond
        ((and (= 8 (length text)) (every #'digit-char-p text))
         (format nil "~a-~a-~a" (subseq text 0 4) (subseq text 4 6) (subseq text 6 8)))
        ((and (= 10 (length text)) (char= (char text 4) #\-)) text)
        ((= 3 (length (uiop:split-string text :separator "/")))
         (let ((parts (uiop:split-string text :separator "/")))
           (if (= 4 (length (third parts)))
               (or (ignore-errors
                     (format nil "~a-~2,'0d-~2,'0d" (third parts)
                             (parse-integer (first parts)) (parse-integer (second parts))))
                   text)
               text)))
        (t text)))))

(defun clip (text chars)
  (let ((text (and (stringp text) (nonblank text))))
    (cond ((null text) nil)
          ((<= (length text) chars) text)
          (t (concatenate 'string (subseq text 0 chars) "...")))))

;;; --- the answer's plumbing -----------------------------------------------------

(defmacro answering (&body body)
  "BODY's text; a refusal is that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (windows-apps-error (condition)
       (nle:failure (format nil "ERROR: ~a" (windows-apps-error-detail condition))))))

;;; --- reading what reg.exe printed ------------------------------------------------

(defstruct (app (:constructor make-app))
  "One program subkey of one Uninstall key: where it was found, and the
values its publisher wrote, by lowercase name."
  (subkey "" :read-only t)
  (view "" :read-only t)
  (values '())
  unreadable)

(defun key-line-p (line)
  "A header line: a registry path, which reg prints flush left."
  (and (plusp (length line))
       (let ((first (char line 0)))
         (and (not (or (char= first #\Space) (char= first #\Tab)))
              (uiop:string-prefix-p "HKEY_" line)))))

(defun reg-depth (line)
  "How many components a registry path has."
  (1+ (count #\\ line)))

(defun value-line-p (line)
  "A value line: reg indents it under its key and prints its type beside it."
  (and (plusp (length line))
       (let ((first (char line 0)))
         (and (or (char= first #\Space) (char= first #\Tab))
              (search "REG_" line)))))

(defun parse-value (line)
  "NAME and DATA of one value line; the type token between them is dropped."
  (let* ((text (string-left-trim '(#\Space #\Tab) line))
         (at (search "REG_" text)))
    (when at
      (let* ((name (string-trim '(#\Space #\Tab) (subseq text 0 at)))
             (rest (subseq text at))
             (gap (or (search "  " rest) (length rest)))
             (data (string-trim '(#\Space) (subseq rest (min (length rest) (+ gap 2))))))
        (values name data)))))

(defun parse-reg-output (text view root)
  "The program subkeys in one `reg query ... /s' output. The root key itself
carries no program; every subkey under it is one entry, its values held by
lowercase name."
  (let ((depth (reg-depth root))
        (entries '())
        (current nil))
    (dolist (line (uiop:split-string (or text "") :separator '(#\Newline)))
      (let ((line (string-right-trim '(#\Return) line)))
        (cond
          ((zerop (length line)) nil)
          ((key-line-p line)
           (setf current (when (> (reg-depth line) depth)
                           (let ((parts (uiop:split-string line :separator "\\")))
                             (make-app :subkey (car (last parts)) :view view))))
           (when current (push current entries)))
          ((and current (value-line-p line))
           (multiple-value-bind (name data) (parse-value line)
             (when name
               (push (cons (string-downcase name) data) (app-values current))))))))
    (nreverse entries)))

(defun missing-key-p (out err)
  "A key that is not on this install answers 'unable to find': an empty view,
which is the honest answer, never an error."
  (let ((text (string-downcase (format nil "~a ~a" (or out "") (or err "")))))
    (or (search "unable to find" text)
        (search "cannot find" text))))

(defun query-key (hive path view)
  "The entries one Uninstall key holds, and one line saying why it could not
be read when it could not. => (values ENTRIES FAILURE)."
  (let* ((key (format nil "~a\\~a" hive path))
         (argv (append (setting :reg) (list "query" key "/s"))))
    (multiple-value-bind (out err status) (funcall *reg-runner* argv)
      (cond
        ((and (integerp status) (zerop status))
         (values (parse-reg-output out view key) nil))
        ((missing-key-p out err) (values '() nil))
        (t
         (values '()
                 (format nil "~a: ~a" view
                         (nlk:one-line (or (nonblank err) (nonblank out)
                                           (format nil "reg ended with ~a" status))))))))))

(defun sweep ()
  "Every program subkey across the four Uninstall keys, in the order they are
read. => (values ENTRIES FAILURES): FAILURES, one line per view that could
not be read, is empty on a machine where all four answer."
  (let ((entries '()) (failures '()) (read-any nil))
    (dolist (source +sources+)
      (destructuring-bind (hive path view) source
        (multiple-value-bind (found failure) (query-key hive path view)
          (unless failure (setf read-any t))
          (setf entries (append entries found))
          (when failure (setf failures (append failures (list failure)))))))
    (unless read-any
      (fail "the installed-software registry could not be read: ~{~a~^; ~}. ~
It exists only on Windows: this cell asks the registry tool Windows ships, which ~
the windows-apps section's reg names."
            failures))
    (values entries failures)))

;;; --- one row per program --------------------------------------------------------

(defstruct (program (:constructor make-program))
  "One program, as Apps & Features shows it: the first registry entry that
named it, and every view it is registered in."
  name app (views '()))

(defun field (app name)
  "The value APP's publisher wrote under NAME (lowercase), or NIL."
  (cdr (assoc name (app-values app) :test #'string=)))

(defun name-of (app)
  (or (nonblank (field app "displayname")) ""))

(defun hidden-p (app)
  "SystemComponent = 1: the publisher asked Apps & Features not to show it."
  (dword-on-p (field app "systemcomponent")))

(defun collapse (entries)
  "One program per name, in the order the sweep read them; a second view joins
the first as one more view -- the 32/64-bit dual install, listed once."
  (let ((by-name (make-hash-table :test #'equal))
        (order '()))
    (dolist (entry entries)
      (let ((name (name-of entry)))
        (when (plusp (length name))
          (let* ((key (string-downcase name))
                 (known (gethash key by-name)))
            (cond
              (known
               (unless (member (app-view entry) (program-views known) :test #'string=)
                 (setf (program-views known)
                       (append (program-views known) (list (app-view entry)))))
               (when (and (null (field (program-app known) "uninstallstring"))
                          (field entry "uninstallstring"))
                 (push (cons "uninstallstring" (field entry "uninstallstring"))
                       (app-values (program-app known)))))
              (t
               (setf known (make-program :name name :app entry
                                         :views (list (app-view entry))))
               (setf (gethash key by-name) known)
               (push key order)))))))
    (loop for key in (nreverse order) collect (gethash key by-name))))

(defun sorted (programs)
  (sort (copy-list programs) (lambda (a b) (string< (string-downcase (program-name a))
                                                    (string-downcase (program-name b))))))

;;; --- win_app_table ---------------------------------------------------------------

(defun row-text (program)
  (let* ((app (program-app program))
         (size (size-mb (field app "estimatedsize"))))
    (format nil "~a | ~a | ~a | ~a | ~a | ~a~@[ | hidden from Apps & Features~]"
            (program-name program)
            (or (nonblank (field app "displayversion")) "-")
            (or (clip (field app "publisher") *publisher-chars*) "-")
            (if size (format nil "~,1f MB" size) "-")
            (or (install-date (field app "installdate")) "-")
            (format nil "~{~a~^, ~}" (program-views program))
            (and (hidden-p app) t))))

(defun matching (programs query view include-hidden)
  "The programs QUERY and VIEW let through, hidden ones only when asked."
  (remove-if-not
   (lambda (program)
     (let ((app (program-app program)))
       (and (or include-hidden (not (hidden-p app)))
            (or (zerop (length view))
                (not (member view +views+ :test #'string=))
                (member view (program-views program) :test #'string=))
            (or (zerop (length query))
                (search query (string-downcase (program-name program)))
                (search query (string-downcase (or (field app "publisher") "")))))))
   programs))

(defun table-notes (entries programs candidates listed view include-hidden failures)
  (let ((notes (list (format nil "~d of ~d matching programs; ~d named programs across ~d registry entries"
                             (length listed) (length candidates)
                             (length programs) (length entries))))
        (per-view '()))
    (dolist (view-name +views+)
      (let ((count (count-if (lambda (entry) (string= (app-view entry) view-name)) entries)))
        (when (plusp count)
          (push (format nil "~a: ~d entries" view-name count) per-view))))
    (when per-view (push (format nil "~{~a~^; ~}" (nreverse per-view)) notes))
    (let ((hidden (count-if (lambda (program) (hidden-p (program-app program))) programs)))
      (when (and (plusp hidden) (not include-hidden))
        (push (format nil "~d hidden from Apps & Features (SystemComponent=1): add include_system_components=true to see them"
                      hidden)
              notes)))
    (let ((dual (count-if (lambda (program) (> (length (program-views program)) 1)) programs)))
      (when (plusp dual)
        (push (format nil "~d program(s) registered in more than one view (the 32/64-bit dual install)" dual)
              notes)))
    (when (> (length candidates) (length listed))
      (push (format nil "stopped at limit=~d; raise it or narrow the query" (length listed)) notes))
    (when (and (plusp (length view)) (not (member view +views+ :test #'string=)))
      (push (format nil "~a is not one of the views (~{~a~^, ~}), so it filtered nothing" view
                    +views+)
            notes))
    (when (null listed) (push "nothing matched; drop the query or the filters" notes))
    (dolist (failure failures) (push (format nil "not read: ~a" failure) notes))
    (nreverse notes)))

(defun table-text (query view include-hidden limit)
  (let* ((query (string-downcase (or (nonblank query) "")))
         (view (string-downcase (or (nonblank view) "")))
         (limit (clamp (or limit 100) 1 *max-rows*)))
    (multiple-value-bind (entries failures) (sweep)
      (let* ((programs (collapse entries))
             (candidates (matching programs query view include-hidden))
             (listed (subseq (sorted candidates) 0 (min limit (length candidates)))))
        (with-output-to-string (out)
          (dolist (note (table-notes entries programs candidates listed view
                                      include-hidden failures))
            (format out "~a~%" note))
          (when listed (terpri out))
          (dolist (program listed) (format out "~a~%" (row-text program))))))))

;;; --- win_app_detail ----------------------------------------------------------------

(defun classify-uninstaller (command msi-managed)
  "What kind of remover the publisher registered -- described, never run.
=> (values KIND NOTE), or NIL when nothing is registered."
  (let ((text (nonblank command))
        (low (string-downcase (or (nonblank command) ""))))
    (cond
      ((null text)
       (when msi-managed
         (values "windows installer" "no explicit string; the MSI database owns removal")))
      ((search "msiexec" low)
       (values "msiexec" "an MSI product code; removal goes through Windows Installer"))
      ((or (search "removeappxpackage" low) (search "appx" low))
       (values "appx" "a Store-side package; Settings removes it, not this string"))
      (t
       ;; the remover is the first quoted segment (paths carry spaces) or the
       ;; first bare token; arguments never classify it
       (let ((first (if (char= (char text 0) #\")
                        (let ((end (position #\" text :start 1)))
                          (string-downcase (subseq text 1 (or end (length text)))))
                        (string-downcase (subseq text 0 (or (position #\Space text) (length text)))))))
         (cond
           ((uiop:string-suffix-p ".exe" first)
            (values "setup executable" "the publisher's own uninstaller program"))
           ((some (lambda (suffix) (uiop:string-suffix-p suffix first)) '(".cmd" ".bat" ".ps1"))
            (values "script" "a script the publisher left behind"))
           ((search ".exe" low)
            ;; an unquoted path with spaces in it, as publishers write them
            (values "setup executable" "the publisher's own uninstaller program"))
           (t (values "other" "an undocumented command; read it before trusting it"))))))))

(defun expand-vars (text)
  "TEXT with %NAME% replaced by the environment's value, as a shell would;
a name the environment does not carry is left as it stands."
  (with-output-to-string (out)
    (loop with i = 0
          while (< i (length text))
          do (let* ((close (and (char= (char text i) #\%)
                                (position #\% text :start (1+ i))))
                    (name (and close (> close (1+ i)) (subseq text (1+ i) close)))
                    (value (and name (uiop:getenv name))))
               (if value
                   (progn (write-string value out) (setf i (1+ close)))
                   (progn (write-char (char text i) out) (incf i)))))))

(defun windows-absolute-p (path)
  "T for a drive-letter or UNC path, as the registry records one."
  (and (stringp path)
       (plusp (length path))
       (let ((first (char path 0)))
         (cond
           ((and (>= (length path) 3)
                 (alpha-char-p first)
                 (char= (char path 1) #\:)
                 (find (char path 2) "\\/"))
            t)
           ((and (>= (length path) 2) (char= first #\\) (char= (char path 1) #\\)) t)
           (t nil)))))

(defun location-lines (location out)
  (let ((expanded (expand-vars location)))
    (format out "  install location: ~a" expanded)
    (if (windows-absolute-p expanded)
        (format out " (~:[not on disk~;on disk~])~%"
                (if (or (uiop:directory-exists-p expanded) (probe-file expanded)) t nil))
        (format out "~%"))))

(defun occurrence-lines (app out)
  (format out "~a / ~a~%" (app-view app) (or (nonblank (app-subkey app)) "-"))
  (let ((version (nonblank (field app "displayversion")))
        (size (size-mb (field app "estimatedsize"))))
    (when version (format out "  version: ~a~%" version))
    (when size (format out "  size: ~,1f MB~%" size))
    (let ((installed (install-date (field app "installdate"))))
      (when installed (format out "  installed: ~a~%" installed)))
    (let ((location (nonblank (field app "installlocation"))))
      (when location (location-lines location out)))
    (multiple-value-bind (kind note)
        (classify-uninstaller (field app "uninstallstring")
                              (dword-on-p (field app "windowsinstaller")))
      (when kind (format out "  uninstaller: ~a - ~a~%" kind note)))
    (let ((command (clip (field app "uninstallstring") *command-chars*)))
      (when command (format out "  uninstall string: ~a~%" command)))))

(defun detail-notes (occurrences)
  (let ((notes '()))
    (when (some (lambda (app)
                  (let* ((location (nonblank (field app "installlocation")))
                        (expanded (and location (expand-vars location))))
                    (and expanded (windows-absolute-p expanded)
                         (not (or (uiop:directory-exists-p expanded) (probe-file expanded))))))
                occurrences)
      (push "the recorded install folder is not on disk any more -- an uninstalled program's leftovers, or a moved install" notes))
    (when (every (lambda (app)
                   (null (nonblank (field app "uninstallstring"))))
                 occurrences)
      (push "no uninstall command is registered; Apps & Features offers no Remove for it" notes))
    (when (some (lambda (app)
                  (and (dword-on-p (field app "nomodify"))
                       (dword-on-p (field app "norepair"))))
                occurrences)
      (push "the publisher locked modification and repair" notes))
    (when (> (length occurrences) 1)
      (push (format nil "registered in ~d views (~{~a~^, ~}): the 32/64-bit dual-install shape"
                    (length occurrences) (mapcar #'app-view occurrences))
            notes))
    (nreverse notes)))

(defun detail-text (name)
  (let ((requested (or (nonblank name)
                        (fail "give the program's name, e.g. 'Python 3.13' or 'OBS Studio'"))))
    (multiple-value-bind (entries failures)
        (sweep)
      (declare (ignore failures))
      (let* ((programs (collapse entries))
             (low (string-downcase requested))
             (exact (find-if (lambda (program)
                               (string= low (string-downcase (program-name program))))
                             programs))
             (near (find-if (lambda (program)
                              (search low (string-downcase (program-name program))))
                            programs))
             (program (or exact near
                          (let ((stem (subseq low 0 (min 4 (length low)))))
                            (fail "no installed program named ~a is registered~@[; did you mean ~{~a~^, ~}?~] win_app_table lists every program with its exact name."
                                  requested
                                  (let ((close (remove-duplicates
                                                (loop for program in programs
                                                      when (search stem (string-downcase (program-name program)))
                                                        collect (program-name program))
                                                :test #'string=)))
                                    (and close (subseq close 0 (min 5 (length close)))))))))
             (app (program-app program))
             (occurrences (if exact (remove-if-not
                                     (lambda (entry) (string= low (string-downcase (name-of entry))))
                                     entries)
                              (list app))))
        (with-output-to-string (out)
          (format out "~a~%" (program-name program))
          (dolist (pair (list (cons "publisher" (field app "publisher"))
                              (cons "version" (field app "displayversion"))
                              (cons "installed" (install-date (field app "installdate")))))
            (when (nonblank (cdr pair)) (format out "~a: ~a~%" (car pair) (nonblank (cdr pair)))))
          (let ((size (size-mb (field app "estimatedsize"))))
            (when size (format out "size: ~,1f MB~%" size)))
          (format out "hidden from Apps & Features: ~:[no~;yes~]~%" (hidden-p app))
          (format out "msi managed: ~:[no~;yes~]~%" (dword-on-p (field app "windowsinstaller")))
          (dolist (pair (list (cons "icon" (field app "displayicon"))
                              (cons "help" (field app "helplink"))
                              (cons "about" (field app "urlinfoabout"))
                              (cons "comments" (field app "comments"))))
            (when (nonblank (cdr pair)) (format out "~a: ~a~%" (car pair) (nonblank (cdr pair)))))
          (dolist (occurrence occurrences)
            (occurrence-lines occurrence out))
          (let ((notes (detail-notes occurrences)))
            (when notes (format out "notes: ~{~a~^; ~}~%" notes))))))))

;;; --- what a person calls from eval, and what the model calls as a tool ------------

(defun win-app-table (&key query view include-system-components (limit 100))
  "The installed-software inventory as text. QUERY is a substring of a name
or publisher, VIEW one of machine-64, machine-32, user-64, user-32,
INCLUDE-SYSTEM-COMPONENTS shows what the publisher hid from Apps & Features,
and LIMIT caps the rows (1-400). Refuses -- WINDOWS-APPS-ERROR -- when the
cell is not running or nothing could be read."
  (table-text query view include-system-components limit))

(defun win-app-detail (name)
  "One program in full, as text, by the name win_app_table showed."
  (detail-text name))

(defun handle-table (call)
  "win_app_table: the software inventory behind Apps & Features."
  (answering
    (win-app-table :query (app-table-query call)
                    :view (app-table-view call)
                    :include-system-components (app-table-include-system-components call)
                    :limit (app-table-limit call))))

(defun handle-detail (call)
  "win_app_detail: one program, every field its publisher wrote, and the
reading of its uninstall command."
  (answering (win-app-detail (app-detail-name call))))

(defun run-slash (args session-id)
  "One /windows-apps invocation: the inventory, narrowed by the words typed
after the command."
  (declare (ignore session-id))
  (handler-case (win-app-table :query (or (nonblank args) "") :limit 40)
    (windows-apps-error (condition)
      (format nil "ERROR: ~a" (windows-apps-error-detail condition)))))

;;; --- the declaration --------------------------------------------------------------

(nle:define-cell windows-apps
  (:section ("windows-apps")
    (:guide "nothing is required: reg.exe ships with Windows and answers all four keys; timeout_seconds bounds one query; next step: ask what is installed, or which version of a program is here")
    ("reg" :list :default '("reg")
     :doc "the argv prefix that runs the registry tool: [\"reg\"] for the reg.exe on PATH, or its full path")
    ("timeout_seconds" :integer :default 30 :min 1
     :doc "one registry query's ceiling in seconds; its process tree is killed past it"))
  (:help :windows-apps
         "windows-apps: what is installed on this Windows machine, swept from the four Uninstall registry keys behind Apps & Features"
         *manual*)
  (:tool "win_app_table" (nlk:contract 'app-table) #'handle-table)
  (:tool "win_app_detail" (nlk:contract 'app-detail) #'handle-detail)
  (:command "windows-apps" 'run-slash
            :description "What is installed on this Windows machine"))
