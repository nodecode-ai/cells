;;;; cell.lisp --- OrcaSlicer's presets, its CLI, and a printer's door.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the verbs through the manual: while the cell runs,
;;;; (help :orcaslicer) answers the primer below, and every request's help
;;;; section carries one line naming it. Nothing hooks the provider path: the
;;;; slicer is a CLI this machine already has, and a printer speaks HTTP.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "orcaslicer": {"slicer": "/usr/bin/orca-slicer",
;;;;                  "printers": [{"name": "voron", "kind": "moonraker",
;;;;                                "url": "http://printer.lan:7125",
;;;;                                "api_key_env": "ORCA_VORON_KEY"}]}
;;;;
;;;; Rewritten from mazzanfar's Hermes Agent plugin of the same name (see
;;;; NOTICE): its stock-CLI contract (--slice, --outputdir, --export-3mf,
;;;; --datadir, --load-settings, --load-filaments), its preset catalog and
;;;; inheritance, its whitelist of changeable process settings, the G-code
;;;; comment report, and Moonraker's, OctoPrint's and PrusaLink's status and
;;;; upload shapes.

(in-package #:nodecode-orcaslicer)

(nlk:define-peripheral orcaslicer :not-running t)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "OrcaSlicer is available through the nodecode-orcaslicer cell: the stock OrcaSlicer
on this machine picks the presets and slices, and a configured printer can be read
and loaded. Only orca_printer_status and orca_upload ever contact a printer, and
orca_upload never starts a print. The same verbs are Lisp functions in the orca
package.
  orca_diagnose  {}
      The OrcaSlicer executable this cell found, whether its command line carries
      every flag slicing needs, the preset roots it reads, and the printers the
      section configures. Run it first: without a slicer nothing else works.
  orca_presets  {query, kind}
      Search the installed machine, process and filament presets, for ANY
      manufacturer. query is a case-insensitive piece of the preset's name; kind
      is machine, process or filament. It answers exact paths: pass those paths
      to orca_slice, never a name from memory.
  orca_slice  {source, machine, process, filaments, changes, plate}
      Slice one model or project into a job of its own. A raw model (.stl, .obj,
      .step, .stp) needs the exact machine, process and filament preset paths; an
      .3mf project keeps its own presets and needs none. changes is a short,
      bounded list of global process settings that ride a settings file over the
      preset. plate is one plate number, starting at 1: slice each plate as its
      own job. The answer is the job record: state, artifacts with sha256, the
      log, and what the G-code's own comments report. It is not a claim that the
      print will fit, adhere or hold.
  orca_job  {job_id}
      One job's record as it stands: prepared, slicing, sliced or failed, with
      its artifacts, log path and report.
  orca_export  {job_id, artifact, destination}
      Copy one artifact to a new absolute path, for an SD card or a stick. It
      never overwrites a file.
  orca_printer_status  {name}
      One read-only look at a configured printer: state, readiness, progress,
      temperatures, the file it holds. Nothing changes.
  orca_upload  {name, job_id}
      Upload the job's G-code to a configured, idle printer (Moonraker,
      OctoPrint or PrusaLink). It never starts printing: do that on the
      printer's own screen or page, after checking the plate and the material.
A job is a folder under the state directory: a copy of the input, the settings it
sliced with, slice.log, and the artifacts. Slicing success is not physical
readiness, and no tool here can verify a clean bed or loaded material."
  "What (help :orcaslicer) answers while the cell runs.")

;;; --- answers, and the one condition ---------------------------------------------

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (orcaslicer-error (condition)
       (nle:failure (format nil "ERROR: ~a" (orcaslicer-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: OrcaSlicer: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun no-arguments (description)
  "The schema of a tool whose call carries nothing."
  (list :description description
        :parameters (nlk:json-object "type" "object" "properties" (nlk:make-json-object))))

;;; --- the tools' arguments, one declaration each --------------------------------

(nlk:define-contract preset-search
  (:doc "Search the OrcaSlicer presets installed on this machine, for any manufacturer, and answer their exact paths. Use it to find the machine, process and filament presets orca_slice needs; never type a preset name from memory.")
  ("query" :string :default ""
   :doc "case-insensitive text the preset's name must contain, e.g. 'Voron', '0.4 nozzle', 'PLA'; empty lists the first presets of each kind")
  ("kind" :string :default ""
   :doc "machine, process or filament; empty searches all three. A machine preset is the printer and its nozzle, a process preset the print settings, a filament preset the material"))

(nlk:define-contract slice-job
  (:doc "Slice one model or project with the installed OrcaSlicer into a job of its own. A raw model (.stl, .obj, .step, .stp) needs the exact machine, process and filament preset paths -- find them with orca_presets. An .3mf project keeps its own presets and needs none. Slicing never contacts a printer, and success is not a claim that the print will fit or hold: read the artifact and the report, then check the plate and the material before printing.")
  ("source" :text :required t
   :doc "absolute path to the .3mf project or the raw model (.stl, .obj, .step, .stp)")
  ("machine" :string :default ""
   :doc "path to the machine preset JSON, from orca_presets (required for a raw model)")
  ("process" :string :default ""
   :doc "path to the process preset JSON, from orca_presets (required for a raw model)")
  ("filaments" :array :of :string
   :doc "the filament preset JSON paths, in tool order (required for a raw model)")
  ("changes" :object
   :doc "global process settings to change, as a JSON object; only layer_height, initial_layer_print_height, wall_loops, top_shell_layers, bottom_shell_layers, sparse_infill_density and enable_support are accepted, each within its bound. Object-level overrides are not touched")
  ("plate" :integer :default 1
   :doc "which plate of the project to slice, starting at 1; slice each plate as its own job"))

(nlk:define-contract job-read
  (:doc "One slicing job's record: its state, its artifacts with their sha256, its log, and what the G-code's own comments report. Ask it after orca_slice answered, or when the thread of a job has been lost.")
  ("job_id" :text :required t
   :doc "the job id orca_slice answered"))

(nlk:define-contract export-job
  (:doc "Copy one of a job's artifacts to a new absolute path, for an SD card, a USB stick or the printer's own file page. It never overwrites an existing file: choose a new name. Exporting does not upload or print anything.")
  ("job_id" :text :required t
   :doc "the job id orca_slice answered")
  ("artifact" :string :default ""
   :doc "the artifact's exact name as orca_job lists it; empty, the job's one G-code")
  ("destination" :text :required t
   :doc "the absolute path to write: a new file name, never an existing one"))

(nlk:define-contract printer-read
  (:doc "Read one configured printer's state, readiness, progress, temperatures and loaded file, without changing anything. The printer must be listed in the orcaslicer section; orca_diagnose lists them.")
  ("name" :text :required t
   :doc "the printer's name as the configuration lists it"))

(nlk:define-contract upload-job
  (:doc "Upload a sliced job's G-code to a configured, idle printer -- Moonraker, OctoPrint or PrusaLink. It does NOT start printing: start the print on the printer's own screen or page, after checking the plate and the material. The printer must be idle and the job must have finished slicing.")
  ("name" :text :required t
   :doc "the printer's name as the configuration lists it")
  ("job_id" :text :required t
   :doc "the job id orca_slice answered"))

(defparameter +diagnose-schema+
  (no-arguments
   "Find the OrcaSlicer installed on this machine and report what its command line supports, the preset roots it reads and the printers configured. Nothing is sliced and no printer is contacted. Run it first: without a slicer no other tool works.")
  "orca_diagnose takes nothing.")

;;; --- reading the section --------------------------------------------------------

(defun nonblank (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defparameter +credential-env-pattern+
  "\\A(ORCA|HERMES_ORCA)_[A-Z0-9_]+\\z"
  "A printer credential's variable name, bounded as the original plugin bounded
it: one of this cell's own variables, so a printer URL can never be handed
another application's token.")

(defparameter +printer-kinds+ '("moonraker" "octoprint" "prusalink")
  "The printer protocols this cell speaks: one read-only status read and one
upload, both over HTTP with the instance's own key.")

(defun read-printer (entry)
  "One printers[] ENTRY as a plist, or a config refusal naming what is wrong."
  (unless (hash-table-p entry)
    (nlk:config-error "every printers[] entry is an object with name, kind and url"))
  (let* ((name (nonblank (nlk:json-value entry :string "name")))
         (kind (nonblank (nlk:json-value entry :string "kind")))
         (url (string-right-trim "/" (or (nlk:json-value entry :string "url") "")))
         (env (nonblank (nlk:json-value entry :string "api_key_env"))))
    (unless name
      (nlk:config-error "every printers[] entry needs a name"))
    (unless (member kind +printer-kinds+ :test #'string=)
      (nlk:config-error "printer ~s: kind must be one of ~{~a~^, ~}" name +printer-kinds+))
    (unless (or (uiop:string-prefix-p "http://" url) (uiop:string-prefix-p "https://" url))
      (nlk:config-error "printer ~s: url ~s has no http:// or https:// scheme, e.g. a printer's own base URL" name url))
    (when (and env (not (ppcre:scan +credential-env-pattern+ env)))
      (nlk:config-error "printer ~s: api_key_env ~s is not an ORCA_ or HERMES_ORCA_ variable name, so no other application's token can be sent to a printer" name env))
    (list :name name :kind kind :url url :key-env env)))

(defun read-settings (values table)
  "The declared VALUES, plus the printers the section lists: read once, here."
  (let ((plist (copy-list values)))
    (setf (getf plist :state-dir)
          (or (nonblank (getf values :state-dir))
              (namestring (nlk:home "orcaslicer/")))
          (getf plist :printers)
          (map 'list #'read-printer (nlk:json-array table "printers")))
    plist))

(defun state-dir ()
  (uiop:ensure-directory-pathname
   (uiop:parse-native-namestring (setting :state-dir))))

(defun printer-config (name)
  "The configured printer called NAME, or a refusal naming what is configured."
  (let ((wanted (nonblank name)))
    (or (find wanted (setting :printers) :test #'string=
              :key (lambda (printer) (getf printer :name)))
        (fail "no printer named ~s is configured; the orcaslicer section lists ~{[~a ~a]~^, ~}"
              name (loop for printer in (setting :printers)
                         collect (getf printer :name) collect (getf printer :kind))))))

;;; --- files ---------------------------------------------------------------------

(defun read-json-file (path)
  "PATH decoded, or NIL when it is not JSON."
  (handler-case (nlk:decode-json (uiop:read-file-string path :external-format :utf-8))
    (error () nil)))

(defun write-text-file (path text)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :external-format :utf-8)
    (write-string text out))
  path)

(defun write-json-file (path object)
  (write-text-file path (nlk:encode-json-object object)))

(defun file-octets (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence octets in)
      octets)))

(defun file-sha256 (path)
  (nlk:sha256-text (file-octets path)))

(defun file-size (path)
  (with-open-file (in path :element-type '(unsigned-byte 8)) (file-length in)))

(defun identifier (value what)
  "VALUE as an id this cell may build a path from, or a refusal."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (unless (and text (<= 1 (length text) 64)
                 (every (lambda (character)
                          (or (alphanumericp character) (find character "-_.")))
                        text)
                 (alphanumericp (char text 0)))
      (fail "~a ~s is not an id: letters, digits, - _ and ., a letter or digit first" what value))
    text))

;;; --- finding and running the slicer ---------------------------------------------

(defparameter +executable-names+ '("orca-slicer" "OrcaSlicer" "orca-slicer.exe")
  "What the OrcaSlicer executable is called on PATH.")

(defparameter +known-executables+
  '("~/Applications/OrcaSlicer.app/Contents/MacOS/OrcaSlicer"
    "/Applications/OrcaSlicer.app/Contents/MacOS/OrcaSlicer"
    "/usr/share/OrcaSlicer/orca-slicer"
    "/usr/local/bin/orca-slicer")
  "Where a desktop install keeps the executable when PATH does not carry it.")

(defun which (name)
  "NAME's path on this account's PATH, or NIL."
  (loop for directory in (uiop:split-string (or (uiop:getenv "PATH") "")
                                            :separator (string (if (uiop:os-windows-p) #\; #\:)))
        for root = (ignore-errors
                     (uiop:ensure-directory-pathname (uiop:parse-native-namestring directory)))
        for candidate = (and root (ignore-errors (merge-pathnames name root)))
        when (and candidate (uiop:file-exists-p candidate))
          return (uiop:native-namestring candidate)))

(defun find-executable ()
  "The OrcaSlicer executable: the section's slicer when it names one, else a
known name on PATH or at a desktop install's own path; a refusal when no
install was found."
  (let ((configured (nonblank (setting :slicer))))
    (if configured
        (let ((path (uiop:parse-native-namestring configured)))
          (unless (uiop:file-exists-p path)
            (fail "slicer names ~s, which is not a file: set it to the OrcaSlicer executable itself, not the .app directory" configured))
          (uiop:native-namestring path))
        (or (loop for name in +executable-names+ for hit = (which name) when hit return hit)
            (loop for text in +known-executables+
                  for path = (ignore-errors (nlk:expand-home text))
                  when (and path (uiop:file-exists-p path)) return (uiop:native-namestring path))
            (fail "OrcaSlicer is not installed where this cell can find it: install it, or set slicer in the orcaslicer section to the executable's full path")))))

(defun run-slicer (argv &key timeout)
  "Run the OrcaSlicer executable with ARGV. => (values OUTPUT CODE), OUTPUT
stdout and stderr together, CODE its exit status (NIL when it did not run).
The one seam every test stubs."
  (handler-case
      (multiple-value-bind (output error-output code)
          (uiop:run-program argv
                            :output :string :error-output :string
                            :ignore-error-status t
                            :timeout timeout
                            :external-format :utf-8)
        (values (concatenate 'string (or output "") (or error-output "")) code))
    (error (condition)
      (values (format nil "~a could not run: ~a" (first argv)
                      (nlk:one-line (princ-to-string condition)))
              nil))))

(defparameter +required-flags+ '("--slice" "--outputdir" "--export-3mf" "--datadir")
  "The command-line flags slicing needs; diagnose names the ones an install lacks.")

(defun diagnose (call)
  "The orca_diagnose tool: the executable, its CLI, its preset roots, the printers."
  (declare (ignore call))
  (running-settings)
  (answering
    (let ((executable (find-executable)))
      (multiple-value-bind (output code)
          (run-slicer (list executable "--help") :timeout 20)
        (let ((missing (remove-if (lambda (flag) (search flag output)) +required-flags+))
              (version (or (find-if (lambda (line) (search "OrcaSlicer" line))
                                    (uiop:split-string output :separator (string #\Newline)))
                           "unknown")))
          (nlk:encode-json-object
           (nlk:json-object
            "executable" executable
            "version" (nlk:clip version 200)
            "cli_ready" (and (equal code 0) (null missing))
            "exit_status" (or code -1)
            "missing_flags" (coerce missing 'vector)
            "profile_roots" (coerce (mapcar #'uiop:native-namestring (profile-roots)) 'vector)
            "printers" (coerce (mapcar (lambda (printer)
                                          (nlk:json-object "name" (getf printer :name)
                                                           "kind" (getf printer :kind)))
                                        (setting :printers))
                                'vector)
            "state_dir" (uiop:native-namestring (state-dir))
            "note" "A missing flag means this build cannot slice from the command line; a printer is only contacted by orca_printer_status and orca_upload.")))))))

;;; --- the preset catalog ---------------------------------------------------------

(defparameter +preset-kinds+ '("machine" "process" "filament")
  "OrcaSlicer's three preset kinds, as its own files name them.")

(defun default-profile-roots ()
  "The preset roots a stock install keeps here, the ones that exist."
  (remove-if-not #'uiop:directory-exists-p
                 (loop for text in '("~/.config/OrcaSlicer/system"
                                     "~/.config/OrcaSlicer/user"
                                     "~/Library/Application Support/OrcaSlicer/system"
                                     "~/Library/Application Support/OrcaSlicer/user"
                                     "/usr/share/OrcaSlicer/resources/profiles"
                                     "/usr/local/share/OrcaSlicer/resources/profiles")
                       for path = (ignore-errors (nlk:expand-home text))
                       when path collect path)))

(defun profile-roots ()
  "Every preset root this cell reads: the section's, then the install's own."
  (remove-duplicates
   (append (loop for text in (setting :profiles)
                 for path = (ignore-errors (nlk:expand-home text))
                 when (and path (uiop:directory-exists-p path)) collect path)
           (default-profile-roots))
   :test #'equal))

(defun json-files-under (root)
  "Every .json under ROOT, sorted; a root that cannot be read contributes none."
  (sort (or (ignore-errors
              (directory (merge-pathnames "**/*.json"
                                          (uiop:ensure-directory-pathname root))))
            '())
        #'string< :key #'namestring))

(defun preset-kind (path data)
  "PATH's preset kind: the file's own type, else the folder under a root that
names it, else NIL."
  (let ((type (and (hash-table-p data) (nlk:json-value data :string "type"))))
    (cond ((member type +preset-kinds+ :test #'string=) type)
          (t (loop for part in (rest (pathname-directory path))
                   when (member part +preset-kinds+ :test #'string=) return part)))))

(defun catalog ()
  "Every preset under the roots: a list of (NAME KIND PATH DATA), a later root
winning a name of the same kind, as a user folder overrides the system one."
  (let ((items (make-hash-table :test #'equal)))
    (dolist (root (profile-roots))
      (dolist (path (json-files-under root))
        (let* ((data (read-json-file path))
               (kind (preset-kind path data))
               (name (and (hash-table-p data) (nlk:json-value data :string "name"))))
          (when (and kind (nonblank name))
            (setf (gethash (list kind name) items) (list name kind path data))))))
    (loop for value being the hash-values of items collect value)))

(defun search-presets (call)
  "The orca_presets tool: the installed presets whose name carries QUERY."
  (running-settings)
  (answering
    (let* ((query (string-downcase (or (preset-search-query call) "")))
           (kind (nonblank (preset-search-kind call)))
           (limit (setting :max-presets)))
      (when (and kind (not (member kind +preset-kinds+ :test #'string=)))
        (fail "kind must be one of ~{~a~^, ~}, not ~s" +preset-kinds+ kind))
      (let* ((matches (remove-if-not
                       (lambda (item)
                         (and (or (null kind) (string= kind (second item)))
                              (search query (string-downcase (first item)))))
                       (catalog)))
             (ordered (sort matches #'string< :key (lambda (item)
                                                     (format nil "~a~a" (second item) (first item))))))
        (nlk:encode-json-object
         (nlk:json-object
          "query" query
          "kind" (or kind "")
          "total" (length ordered)
          "truncated" (> (length ordered) limit)
          "matches"
          (coerce
           (loop for item in (subseq ordered 0 (min limit (length ordered)))
                 collect (nlk:json-object
                          "name" (first item) "kind" (second item)
                          "path" (uiop:native-namestring (third item))
                          "inherits" (or (nlk:json-value (fourth item) :string "inherits") "")))
           'vector)))))))

(defun find-preset (name kind)
  "The catalog's preset called NAME of KIND, or a refusal."
  (or (find (list kind name) (catalog)
            :test #'equal :key (lambda (item) (list (second item) (first item))))
      (fail "no ~a preset named ~s is installed: find it with orca_presets" kind name)))

(defun resolve-preset (path kind &optional (depth 0))
  "PATH's own settings with what it inherits merged under them, as OrcaSlicer
resolves a preset. => (values NAME TABLE)."
  (let ((data (read-json-file path)))
    (unless (hash-table-p data)
      (fail "~s is not a preset file this cell can read" (uiop:native-namestring path)))
    (let* ((name (or (nonblank (nlk:json-value data :string "name"))
                     (fail "~s carries no preset name" (uiop:native-namestring path))))
           (parent (nonblank (nlk:json-value data :string "inherits"))))
      (when parent
        (when (>= depth 8)
          (fail "preset ~s inherits more than 8 deep: a cycle in the installed presets" name))
        (multiple-value-bind (parent-name merged)
            (resolve-preset (third (find-preset parent kind)) kind (1+ depth))
          (declare (ignore parent-name))
          (loop for key being the hash-keys of merged using (hash-value value)
                do (setf (gethash key data) value))))
      (values name data))))

(defparameter +changes-bounds+
  '(("layer_height" 0.05 1.0)
    ("initial_layer_print_height" 0.05 1.0)
    ("wall_loops" 1 10)
    ("top_shell_layers" 0 20)
    ("bottom_shell_layers" 0 20)
    ("sparse_infill_density" 0 100))
  "The process settings a call may change, and their bounds. Anything else
comes from a preset: a stray key would be a setting nobody validated.")

(defun as-number (value)
  "VALUE as a real, or a refusal."
  (let ((number (cond ((realp value) value)
                      ((stringp value) (read-from-string
                                         (string-trim " %" (nlk:trimmed value)) nil nil))
                      (t nil))))
    (unless (and number (realp number))
      (fail "~s is not a number" value))
    (coerce number 'double-float)))

(defun number-text (number)
  "NUMBER as OrcaSlicer writes it: no trailing zeros, no trailing point."
  (let ((text (format nil "~f" number)))
    (if (find #\. text)
        (let ((trimmed (string-right-trim "0" text)))
          (if (char= (char trimmed (1- (length trimmed))) #\.)
              (subseq trimmed 0 (1- (length trimmed)))
              trimmed))
        text)))

(defun validate-changes (changes)
  "CHANGES as the string values OrcaSlicer reads, each checked against its
bound, or a refusal naming the key."
  (let ((object (if (hash-table-p changes) changes (nlk:make-json-object)))
        (result (make-hash-table :test #'equal)))
    (loop for key being the hash-keys of object using (hash-value value)
          do (cond
               ((string= key "enable_support")
                (unless (member value '(t nil "0" "1" 0 1) :test #'equal)
                  (fail "enable_support must be true or false"))
                (setf (gethash key result)
                      (if (member value '(t "1" 1) :test #'equal) "1" "0")))
               (t
                (let ((bound (assoc key +changes-bounds+ :test #'string=)))
                  (unless bound
                    (fail "~s is not a setting this cell changes: use ~{~a~^, ~}, or a process preset"
                          key (mapcar #'first +changes-bounds+)))
                  (let ((number (as-number value)))
                    (unless (<= (second bound) number (third bound))
                      (fail "~a must be between ~a and ~a" key (second bound) (third bound)))
                    (when (and (search "layers" key) (not (= number (fround number))))
                      (fail "~a must be a whole number" key))
                    (setf (gethash key result) (number-text number)))))))
    result))

;;; --- what the G-code says itself -------------------------------------------------

(defparameter +report-keys+
  '("nozzle_diameter" "printer_settings_id" "gcode_flavor" "curr_bed_type" "filament_type"
    "nozzle_temperature" "first_layer_bed_temperature" "total layer number")
  "The G-code comment keys reported as they are; every 'estimated' and
'filament used' comment rides too, whatever its wording.")

(defparameter +report-lines+ 4000000
  "How many lines of G-code one report reads before it stops.")

(defun analyze-gcode (path)
  "What PATH's own comments say: the slicer's estimates and its layer count,
never a claim about geometry or strength."
  (let ((found (make-hash-table :test #'equal))
        (layers 0)
        (lines 0))
    (handler-case
        (with-open-file (in path :external-format :utf-8)
          (loop for line = (read-line in nil nil)
                while (and line (< lines +report-lines+))
                do (incf lines)
                   (cond
                     ((uiop:string-prefix-p "; CHANGE_LAYER" line) (incf layers))
                     ((uiop:string-prefix-p ";LAYER_CHANGE" line) (incf layers))
                     ((uiop:string-prefix-p "; model printing time:" line)
                      (dolist (part (uiop:split-string line :separator (string #\;)))
                        (let ((colon (position #\: part)))
                          (when colon
                            (setf (gethash (string-trim " ;" (subseq part 0 colon)) found)
                                  (string-trim " " (subseq part (1+ colon))))))))
                     ((and (uiop:string-prefix-p ";" line)
                           (or (find #\= line) (find #\: line)))
                      (let* ((body (subseq line 1))
                             (equal (or (position #\= body) (position #\: body)))
                             (key (string-trim " " (subseq body 0 equal)))
                             (value (string-trim " " (subseq body (1+ equal)))))
                        (when (or (member key +report-keys+ :test #'string=)
                                  (search "estimated" key :test #'char-equal)
                                  (search "filament used" key :test #'char-equal))
                          (setf (gethash key found) (nlk:clip value 300))))))))
      (error () nil))
    (let ((report (nlk:json-object
                   "layer_markers" layers
                   "limitations"
                   "Read from the slicer's own comments; no claim that the toolpath is safe, fits or holds.")))
      (loop for key being the hash-keys of found using (hash-value value)
            do (setf (gethash key report) value))
      report)))

;;; --- a job, its folder and its artifacts -----------------------------------------

(defparameter +source-types+ '("3mf" "stl" "obj" "step" "stp")
  "What orca_slice takes as a source.")

(defun source-path (value)
  (let ((text (nonblank value)))
    (unless text
      (fail "source is required: the .3mf project or the raw model"))
    (let ((path (uiop:parse-native-namestring (nlk:trimmed text))))
      (unless (uiop:file-exists-p path)
        (fail "source ~s is not a file" text))
      (unless (member (string-downcase (or (pathname-type path) "")) +source-types+
                      :test #'string=)
        (fail "source ~s is not a .3mf, .stl, .obj, .step or .stp file" text))
      path)))

(defun job-directory (job-id)
  "The folder of JOB-ID, or a refusal when no job of that id exists."
  (let* ((id (identifier job-id "job id"))
         (directory (merge-pathnames (format nil "jobs/~a/" id) (state-dir))))
    (unless (uiop:file-exists-p (merge-pathnames "job.json" directory))
      (fail "no job ~s is in ~a" id (uiop:native-namestring (state-dir))))
    directory))

(defun preset-argument (value kind)
  "VALUE as a resolved preset => (values NAME TABLE), or (values NIL NIL)."
  (let ((text (nonblank value)))
    (when text
      (let ((path (uiop:parse-native-namestring text)))
        (unless (uiop:file-exists-p path)
          (fail "the ~a preset ~s is not a file: find it with orca_presets" kind text))
        (resolve-preset path kind)))))

(defun check-compatibility (machine-name process process-name)
  "Refuse a process preset that does not list MACHINE-NAME as compatible."
  (let ((compatible (and process (nlk:json-array process "compatible_printers"))))
    (when (and machine-name compatible (plusp (length compatible))
               (not (find machine-name compatible :test #'string=)))
      (fail "the process preset ~s does not list ~s among its compatible printers"
            process-name machine-name))))

(defun job-artifacts (output)
  "Every artifact the slicer left in OUTPUT, with its sha256."
  (loop for path in (sort (or (ignore-errors (uiop:directory-files output)) '())
                           #'string< :key #'namestring)
        for type = (string-downcase (or (pathname-type path) ""))
        when (member type '("gcode" "3mf" "png") :test #'string=)
          collect (nlk:json-object "name" (file-namestring path)
                                    "kind" type
                                    "path" (uiop:native-namestring path)
                                    "bytes" (file-size path)
                                    "sha256" (file-sha256 path))))

(defun gcode-warnings (output)
  "The slicer's own warnings and errors, the last of them."
  (let ((lines '()))
    (dolist (line (uiop:split-string (or output "") :separator (string #\Newline)))
      (when (or (search "warning" line :test #'char-equal)
                (search "error" line :test #'char-equal))
        (push (nlk:clip line 400) lines)))
    (coerce (subseq (nreverse lines) 0 (min 20 (length lines))) 'vector)))

(defun resolved-presets (call)
  "CALL's preset paths resolved => (values MACHINE-NAME MACHINE PROCESS-NAME
PROCESS FILAMENT-DATA), each NAME NIL when the call names no such preset."
  (multiple-value-bind (machine-name machine)
      (preset-argument (slice-job-machine call) "machine")
    (multiple-value-bind (process-name process)
        (preset-argument (slice-job-process call) "process")
      (values machine-name machine process-name process
              (loop for path in (coerce (or (slice-job-filaments call) #()) 'list)
                    collect (nth-value 1 (preset-argument path "filament")))))))

(defun write-job-settings (directory machine process changes filaments)
  "The preset files one job slicks with => (values SETTINGS FILES), each a
namestring. The changes ride a partial file over the presets, or over the
project's own settings, which are never rewritten."
  (let ((settings '())
        (files '()))
    (when machine
      (push (namestring (write-json-file (merge-pathnames "machine.json" directory) machine))
            settings))
    (when process
      (push (namestring (write-json-file (merge-pathnames "process.json" directory) process))
            settings))
    (when (plusp (hash-table-count changes))
      (push (namestring (write-json-file (merge-pathnames "changes.json" directory) changes))
            settings))
    (loop for data in filaments
          for index from 0
          do (push (namestring (write-json-file
                                (merge-pathnames (format nil "filament-~d.json" index) directory)
                                data))
                   files))
    (values (nreverse settings) (nreverse files))))

(defun slicer-argv (executable directory input output plate settings-files filament-files)
  "The command line a stock OrcaSlicer slices PLATE of INPUT with, the same
flags the original plugin used."
  (append (list executable
                "--datadir" (namestring (merge-pathnames "orca-data/" directory))
                "--slice" (princ-to-string plate)
                "--outputdir" (namestring output)
                "--export-3mf" "sliced.3mf")
          (when settings-files
            (list "--load-settings" (format nil "~{~a~^;~}" settings-files)))
          (when filament-files
            (list "--load-filaments" (format nil "~{~a~^;~}" filament-files)))
          (list (namestring input))))

(defun job-record (fields)
  "One job's record as its file holds it, from the FIELDS plist."
  (nlk:json-object "id" (getf fields :id)
                    "state" (getf fields :state)
                    "source" (getf fields :source)
                    "source_name" (getf fields :source-name)
                    "source_sha256" (getf fields :source-sha256)
                    "printer_profile" (or (getf fields :printer-profile) "")
                    "plate" (getf fields :plate)
                    "executable" (getf fields :executable)
                    "command" (coerce (getf fields :command) 'vector)
                    "log" (getf fields :log)
                    "changes" (getf fields :changes)
                    "artifacts" (coerce (getf fields :artifacts) 'vector)
                    "warnings" (coerce (getf fields :warnings) 'vector)
                    "report" (getf fields :report)
                    "error" (getf fields :error)
                    "note" (or (getf fields :note)
                               "Slicing only; no printer was contacted, and success is no claim about fit, adhesion or strength.")))

(defun slice (call)
  "The orca_slice tool: one job, one plate, one outcome."
  (running-settings)
  (answering
    (let* ((source (source-path (slice-job-source call)))
           (raw-p (not (string= "3mf" (string-downcase (or (pathname-type source) "")))))
           (plate (or (slice-job-plate call) 1))
           (changes (validate-changes (slice-job-changes call)))
           (executable (find-executable)))
      (unless (and (integerp plate) (>= plate 1))
        (fail "plate must be a whole number starting at 1: slice each plate as its own job"))
      (multiple-value-bind (machine-name machine process-name process filaments)
          (resolved-presets call)
        (when (and raw-p (not (and machine-name process-name filaments)))
          (fail "a raw model needs the exact machine, process and filament presets: search them with orca_presets and pass their paths"))
        (when (and machine-name process-name)
          (check-compatibility machine-name process process-name))
        (let* ((id (nlk:make-durable-id "orca-job"))
               (directory (merge-pathnames (format nil "jobs/~a/" id) (state-dir)))
               (input (merge-pathnames
                       (format nil "input.~a" (string-downcase (pathname-type source)))
                       directory))
               (output (merge-pathnames "output/" directory))
               (log-path (merge-pathnames "slice.log" directory))
               (record-path (merge-pathnames "job.json" directory)))
          (ensure-directories-exist input)
          (uiop:copy-file source input)
          (multiple-value-bind (settings-files filament-files)
              (write-job-settings directory machine process changes filaments)
            (ensure-directories-exist output)
            (let* ((argv (slicer-argv executable directory input output plate
                                       settings-files filament-files))
                   (common (list :id id :source (namestring source)
                                 :source-name (file-namestring source)
                                 :source-sha256 (file-sha256 input)
                                 :printer-profile machine-name
                                 :plate plate :executable executable :command argv
                                 :log (namestring log-path) :changes changes)))
              (write-json-file record-path (job-record (list* :state "slicing" common)))
              (multiple-value-bind (output-text code)
                  (run-slicer argv :timeout (setting :slice-timeout))
                (write-text-file log-path output-text)
                (let* ((artifacts (job-artifacts output))
                       (gcodes (remove-if-not
                                (lambda (item)
                                  (string= "gcode" (nlk:json-value item :string "kind")))
                                artifacts))
                       (state (if (and (equal code 0) gcodes) "sliced" "failed"))
                       (error-text
                        (cond ((not (equal code 0))
                               (format nil "OrcaSlicer exited with status ~a: ~a"
                                       code (nlk:clip (nlk:one-line output-text) 400)))
                              ((null gcodes)
                               "OrcaSlicer produced no G-code: this build may not slice from the command line")
                              (t "")))
                       (report (if gcodes
                                   (analyze-gcode
                                    (uiop:parse-native-namestring
                                     (nlk:json-value (first gcodes) :string "path")))
                                   (nlk:json-object))))
                  (write-json-file
                   record-path
                   (job-record (list* :state state :artifacts artifacts
                                      :warnings (gcode-warnings output-text)
                                      :report report :error error-text
                                      :note "Sliced, not printed; success is no claim about fit, adhesion or strength."
                                      common)))
                  (uiop:read-file-string record-path))))))))))

(defun job (call)
  "The orca_job tool: the job's record, as its own file holds it."
  (running-settings)
  (answering
    (uiop:read-file-string (merge-pathnames "job.json"
                                             (job-directory (job-read-job-id call))))))

(defun export-artifact (call)
  "The orca_export tool: one artifact to a new path, never over an old one."
  (running-settings)
  (answering
    (let* ((directory (job-directory (export-job-job-id call)))
           (record (read-json-file (merge-pathnames "job.json" directory)))
           (artifacts (coerce (nlk:json-array record "artifacts") 'list))
           (wanted (nonblank (export-job-artifact call)))
           (chosen (cond (wanted
                          (or (find wanted artifacts :test #'string=
                                    :key (lambda (item)
                                           (nlk:json-value item :string "name")))
                              (fail "job ~a has no artifact named ~s"
                                    (nlk:json-value record :string "id") wanted)))
                         ((= 1 (length artifacts)) (first artifacts))
                         (t (fail "name the artifact to export: this job made ~d"
                                  (length artifacts)))))
           (source (uiop:parse-native-namestring (nlk:json-value chosen :string "path")))
           (destination (nonblank (export-job-destination call))))
      (unless destination
        (fail "destination is required: the new absolute path to write"))
      (let ((target (nlk:expand-home destination)))
        (unless (uiop:absolute-pathname-p target)
          (fail "destination ~s is not an absolute path" destination))
        (when (uiop:file-exists-p target)
          (fail "~s already exists: this cell never overwrites a file, choose a new name" destination))
        (ensure-directories-exist target)
        (uiop:copy-file source target)
        (nlk:encode-json-object
         (nlk:json-object "exported" (uiop:native-namestring target)
                           "artifact" (nlk:json-value chosen :string "name")
                           "bytes" (file-size target)
                           "sha256" (file-sha256 target)
                           "note" "Copied from the job; no printer was contacted."))))))

;;; --- printers, over HTTP ----------------------------------------------------------

(defun api (printer method path &key payload content headers timeout)
  "One exchange with PRINTER => (values DECODED STATUS), the key read from the
environment variable the configuration names."
  (let* ((url (concatenate 'string (getf printer :url) path))
         (all-headers (append (list (cons "accept" "application/json"))
                              (when payload (list (cons "content-type" "application/json")))
                              headers))
         (key (getf printer :key-env))
         (limit (or timeout (setting :printer-timeout))))
    (when key
      (let ((secret (nonblank (nle::credential-env key))))
        (unless secret
          (fail "printer ~a names ~a, which is unset or empty" (getf printer :name) key))
        (setf all-headers (cons (cons "x-api-key" secret) all-headers))))
    (handler-case
        (multiple-value-bind (body status)
            (nlk:http method url
                      :headers all-headers
                      :content (or content (and payload (nlk:encode-json-object payload)))
                      :timeout limit :connect-timeout limit)
          (values (cond ((hash-table-p body) body)
                        ((stringp body) (or (ignore-errors (nlk:decode-json body)) body))
                        (t body))
                  status))
      (error (condition)
        (fail "~a ~a did not answer: ~a" (string-upcase (string method)) url
              (nlk:one-line (princ-to-string condition)))))))

(defun percent-of (value scale)
  (when (numberp value) (round (* value scale))))

(defun temperature-of (object)
  (nlk:json-object "actual" (or (nlk:json-value object :number "temperature") 0)
                    "target" (or (nlk:json-value object :number "target") 0)))

(defun moonraker-status (printer)
  (multiple-value-bind (body status)
      (api printer :get "/printer/objects/query?print_stats&webhooks&virtual_sdcard&extruder&heater_bed")
    (unless (and (integerp status) (< status 400))
      (fail "Moonraker answered HTTP ~a" status))
    (let* ((state (nlk:json-value body :object "result" "status"))
           (stats (nlk:json-value state :object "print_stats"))
           (phase (or (nlk:json-value stats :string "state") "unknown")))
      (nlk:json-object
       "state" phase
       "ready_to_start" (and (not (member phase '("printing" "paused") :test #'string=))
                             (equal "ready" (nlk:json-value state :string "webhooks" "state")))
       "filename" (or (nlk:json-value stats :string "filename") "")
       "progress_percent" (percent-of (nlk:json-value state :number "virtual_sdcard" "progress") 100)
       "temperatures" (nlk:json-object
                       "nozzle" (temperature-of (nlk:json-value state :object "extruder"))
                       "bed" (temperature-of (nlk:json-value state :object "heater_bed")))))))

(defun octoprint-status (printer)
  (multiple-value-bind (body status) (api printer :get "/api/printer")
    (unless (and (integerp status) (< status 400))
      (fail "OctoPrint answered HTTP ~a for /api/printer" status))
    (multiple-value-bind (job job-status) (api printer :get "/api/job")
      (unless (and (integerp job-status) (< job-status 400))
        (fail "OctoPrint answered HTTP ~a for /api/job" job-status))
      (let* ((flags (nlk:json-value body :object "state" "flags"))
             (busy (loop for flag in '("printing" "paused" "pausing" "cancelling" "error" "closedOrError")
                         thereis (eq t (nlk:json-value flags :boolean flag))))
             (temperatures (nlk:json-value body :object "temperature")))
        (nlk:json-object
         "state" (or (nlk:json-value job :string "state") "unknown")
         "ready_to_start" (and (eq t (nlk:json-value flags :boolean "operational")) (not busy))
         "filename" (or (nlk:json-value job :string "job" "file" "name") "")
         "progress_percent" (percent-of (nlk:json-value job :number "progress" "completion") 1)
         "remaining_seconds" (or (nlk:json-value job :number "progress" "printTimeLeft") 0)
         "temperatures" (nlk:json-object
                         "nozzle" (temperature-of (nlk:json-value temperatures :object "tool0"))
                         "bed" (temperature-of (nlk:json-value temperatures :object "bed"))))))))

(defun prusalink-status (printer)
  (multiple-value-bind (body status) (api printer :get "/api/v1/status")
    (unless (and (integerp status) (< status 400))
      (fail "PrusaLink answered HTTP ~a" status))
    (let* ((machine (nlk:json-value body :object "printer"))
           (job (nlk:json-value body :object "job"))
           (phase (or (nlk:json-value machine :string "state") "UNKNOWN")))
      (nlk:json-object
       "state" phase
       "ready_to_start" (and (member phase '("IDLE" "READY" "FINISHED" "STOPPED") :test #'string=) t)
       "progress_percent" (percent-of (nlk:json-value job :number "progress") 1)
       "remaining_seconds" (or (nlk:json-value job :number "time_remaining") 0)
       "temperatures" (nlk:json-object
                       "nozzle" (nlk:json-object
                                 "actual" (or (nlk:json-value machine :number "temp_nozzle") 0)
                                 "target" (or (nlk:json-value machine :number "target_nozzle") 0))
                       "bed" (nlk:json-object
                              "actual" (or (nlk:json-value machine :number "temp_bed") 0)
                              "target" (or (nlk:json-value machine :number "target_bed") 0)))))))

(defun printer-doors (printer)
  "PRINTER's status, read the way its protocol names it."
  (let ((kind (getf printer :kind)))
    (cond ((string= kind "moonraker") (moonraker-status printer))
          ((string= kind "octoprint") (octoprint-status printer))
          ((string= kind "prusalink") (prusalink-status printer))
          (t (fail "~a printers are not readable by this cell" kind)))))

(defun printer-status (call)
  "The orca_printer_status tool: one read-only look at a configured printer."
  (running-settings)
  (answering
    (let* ((printer (printer-config (printer-read-name call)))
           (status (printer-doors printer)))
      (setf (gethash "printer" status) (getf printer :name)
            (gethash "kind" status) (getf printer :kind)
            (gethash "printer_contacted" status) t
            (gethash "note" status)
            "One read-only observation; temperatures are the printer's last report.")
      (nlk:encode-json-object status))))

(defun upload (call)
  "The orca_upload tool: the job's G-code to an idle printer, never a print."
  (running-settings)
  (answering
    (let* ((printer (printer-config (upload-job-name call)))
           (directory (job-directory (upload-job-job-id call)))
           (record (read-json-file (merge-pathnames "job.json" directory)))
           (gcodes (remove-if-not (lambda (item)
                                    (string= "gcode" (nlk:json-value item :string "kind")))
                                  (coerce (nlk:json-array record "artifacts") 'list))))
      (unless (string= "sliced" (nlk:json-value record :string "state"))
        (fail "job ~a is ~s, not sliced: nothing safe to upload"
              (nlk:json-value record :string "id") (nlk:json-value record :string "state")))
      (unless (= 1 (length gcodes))
        (fail "job ~a made ~d G-code artifacts: export the one to print, or slice a single plate"
              (nlk:json-value record :string "id") (length gcodes)))
      (let* ((artifact (first gcodes))
             (path (uiop:parse-native-namestring (nlk:json-value artifact :string "path")))
             (remote (format nil "nodecode-~a.gcode" (nlk:json-value record :string "id")))
             (kind (getf printer :kind))
             (status (printer-doors printer)))
        (unless (eq t (nlk:json-value status :boolean "ready_to_start"))
          (fail "printer ~a is ~s, not idle: nothing is uploaded until it is ready"
                (getf printer :name) (nlk:json-value status :string "state")))
        (multiple-value-bind (body code)
            (cond
              ((string= kind "moonraker")
               (api printer :post "/server/files/upload"
                    :content (list (cons "root" "gcodes")
                                   (cons "file" (pathname path)))
                    :timeout 300))
              ((string= kind "octoprint")
               (api printer :post "/api/files/local"
                    :content (list (cons "select" "false")
                                   (cons "print" "false")
                                   (cons "file" (pathname path)))
                    :timeout 300))
              (t
               (api printer :put (format nil "/api/v1/files/usb/~a" remote)
                    :content (file-octets path)
                    :headers (list (cons "content-type" "application/octet-stream")
                                   (cons "print-after-upload" "?0")
                                   (cons "overwrite" "?0"))
                    :timeout 300)))
          (unless (and (integerp code) (< code 400))
            (fail "printer ~a answered HTTP ~a to the upload" (getf printer :name) code))
          (nlk:encode-json-object
           (nlk:json-object
            "printer" (getf printer :name)
            "kind" kind
            "uploaded" (or (nlk:json-value body :string "result" "item" "path") remote)
            "artifact" (nlk:json-value artifact :string "name")
            "sha256" (nlk:json-value artifact :string "sha256")
            "print_started" nil
            "next" "The file is on the printer; start it there after checking the plate and the material.")))))))

;;; --- /orca -------------------------------------------------------------------------

(defun run-slash (args session-id)
  "The /orca command: what this cell can see, without slicing or printing."
  (declare (ignore args session-id))
  (handler-case
      (progn
        (running-settings)
        (let* ((executable (find-executable))
               (jobs (length (or (ignore-errors
                                   (directory (merge-pathnames "jobs/*/job.json" (state-dir))))
                                 '())))
               (printers (setting :printers)))
          (format nil "/orca: ~a~%  jobs: ~d under ~a~%  printers: ~{~a~^, ~}~%  (help :orcaslicer) has the verbs"
                  (file-namestring executable) jobs
                  (uiop:native-namestring (state-dir))
                  (if printers
                      (mapcar (lambda (printer)
                                (format nil "~a (~a)" (getf printer :name) (getf printer :kind)))
                              printers)
                      (list "none configured")))))
    (orcaslicer-error (condition)
      (format nil "/orca: ~a" (orcaslicer-error-detail condition)))
    (error (condition)
      (format nil "/orca: ~a" (nlk:one-line (princ-to-string condition))))))

;;; --- the cell ----------------------------------------------------------------------

(nle:define-cell orcaslicer
  (:section ("orcaslicer")
    (:guide "point it at the OrcaSlicer executable when it is not on PATH, list the printers it may read and load, and ask it to slice a model; a job is a folder under state_dir; a slicer path and printers are optional, and nothing here prints by itself")
    ("slicer" :path
     :doc "the OrcaSlicer executable itself (not the .app directory); unset, orca-slicer on PATH and a desktop install's own path are tried")
    ("profiles" :list
     :doc "extra preset roots to search, each a folder holding machine, process and filament presets; the install's own roots are always searched")
    ("state_dir" :path
     :doc "where jobs and their artifacts live; unset, orcaslicer/ under the Nodecode home")
    ("slice_timeout" :integer :default 1800 :min 30
     :doc "how long one slice may take, in seconds")
    ("printer_timeout" :integer :default 20 :min 1
     :doc "how long one printer request may take, in seconds")
    ("max_presets" :integer :default 30 :min 1
     :doc "the most presets one orca_presets answer lists"))
  (:settings #'read-settings)
  (:help :orcaslicer
         "orcaslicer: slice a model with the installed OrcaSlicer, read the job's artifacts, and check or load a printer"
         *manual*)
  (:tool "orca_diagnose" +diagnose-schema+ #'diagnose)
  (:tool "orca_presets" (nlk:contract 'preset-search) #'search-presets)
  (:tool "orca_slice" (nlk:contract 'slice-job) #'slice)
  (:tool "orca_job" (nlk:contract 'job-read) #'job)
  (:tool "orca_export" (nlk:contract 'export-job) #'export-artifact)
  (:tool "orca_printer_status" (nlk:contract 'printer-read) #'printer-status)
  (:tool "orca_upload" (nlk:contract 'upload-job) #'upload)
  (:command "orca" 'run-slash
            :description "OrcaSlicer: the slicer, the jobs and the printers this cell can see"))
