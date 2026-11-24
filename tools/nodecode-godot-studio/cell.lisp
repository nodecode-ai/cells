;;;; cell.lisp --- Godot 4 headless checks, exports, and build state.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Rewritten from Foolafroos's Hermes Agent plugin godot-studio (see
;;;; NOTICE). Its four tools drive a Godot 4 executable the machine already
;;;; has, without opening the editor: a headless check that surfaces script
;;;; and resource errors, an export from a named preset, the preset and
;;;; artifact state a build gallery needs, and an install report. The same
;;;; verbs are Lisp functions here -- CHECK, EXPORT-PROJECT, BUILDS, DIAGNOSE
;;;; -- so eval reaches them too, and /godot-studio says what the cell can see.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "godot-studio": {"binary": "/usr/local/bin/godot",
;;;;             "projects": ["~/games/hello"],
;;;;             "check_timeout": 300, "export_timeout": 900,
;;;;             "max_output": 20000}
;;;;
;;;; Nothing here contacts a network, and nothing here reads outside the
;;;; project a call names. A check and an export run that project's own
;;;; GDScript (autoloads, a main scene, export plugins), as the original
;;;; says: point them only at projects you trust.

(defpackage #:nodecode-godot-studio
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:setting #:godot-studio-error
           #:check #:export-project #:builds #:diagnose))

(in-package #:nodecode-godot-studio)

(nlk:define-peripheral godot-studio :not-running t)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Godot 4 headless workflows are available through the nodecode-godot-studio cell: the
Godot executable on this machine verifies and exports a project, and never opens
the editor. The same verbs are Lisp functions in the nodecode-godot-studio package.
  godot_headless_check  {project_path, quit_after_frames}
      Run the project headless and quit, surfacing script and resource errors.
      quit_after_frames 0 is the default and uses --quit: it validates what the
      project loads and parses. A count above 0 uses --quit-after N frames,
      which reaches runtime (a broken autoload or main scene shows up there and
      not with --quit). It runs the project's own GDScript with no approval
      prompt: point it only at projects you trust. Run it after every batch of
      edits, before claiming progress.
  godot_export  {project_path, preset, debug, output_path}
      Export a build from a named export preset (--export-release, or
      --export-debug). The preset must exist in the project's
      export_presets.cfg; find its name with godot_builds. output_path is
      optional: empty uses the preset's own export_path. It must resolve inside
      the project folder, and an existing file is only overwritten when it is a
      known export artifact of this project. The output's parent directory is
      created for you. It runs the project's own code and export plugins.
  godot_builds  {project_path}
      Every export preset in the project and the state of the artifact each
      would write: exists, size in bytes, modification time. Ask it before
      re-exporting, and after an export to confirm the artifact is there.
  godot_setup  {}
      The Godot executable this cell found, its version, and the projects the
      godot-studio section lists. Nothing is installed and nothing runs beyond
      --version.
The loop: edit, then godot_headless_check, then fix what it reports, then
godot_export, then play. An export fails with 'export template not found'
until Godot's export templates are installed for that version (editor, Project
-> Manage Export Templates); the answer says so when it sees it. A check or an
export that needs a window or a display is not what this is for."
  "What (help :godot-studio) answers while the cell runs.")

(defparameter +next-step+
  (format nil "An export needs Godot's export templates for the engine version ~
in use; without them it fails with 'export template not found'. Install them ~
from the editor (Project -> Manage Export Templates) or with ~
godot --headless --install-export-templates.")
  "What the install report says about the one thing an export needs.")

;;; --- answers, and the one condition ---------------------------------------------

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (godot-studio-error (condition)
       (nle:failure (format nil "ERROR: ~a" (godot-studio-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: Godot: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun no-arguments (description)
  "The schema of a tool whose call carries nothing."
  (list :description description
        :parameters (nlk:json-object "type" "object" "properties" (nlk:make-json-object))))

;;; --- the tools' arguments, one declaration each --------------------------------

(nlk:define-contract project-check
  (:doc "Run a Godot project headless and quit, surfacing script and resource errors without opening the editor. quit_after_frames 0 uses --quit, which validates what the project loads and parses; a count above 0 uses --quit-after N frames, which reaches runtime. It runs the project's own GDScript with no approval prompt: point it only at projects you trust. Run it after every batch of edits, before claiming progress.")
  ("project_path" :text :required t
   :doc "the project folder, the one that holds project.godot")
  ("quit_after_frames" :integer :default 0
   :doc "quit after this many frames instead of immediately; 0 validates parsing only, above 0 reaches runtime"))

(nlk:define-contract project-export
  (:doc "Export a Godot build headless from a named export preset. The preset must exist in the project's export_presets.cfg; godot_builds lists them. output_path, when empty, is the preset's own export_path; it must resolve inside the project folder, and an existing file is only overwritten when it is a known export artifact of this project. The output's parent directory is created for you. It runs the project's own GDScript and export plugins with no approval prompt: point it only at projects you trust.")
  ("project_path" :text :required t
   :doc "the project folder, the one that holds project.godot")
  ("preset" :text :required t
   :doc "the export preset's name exactly as export_presets.cfg lists it, e.g. Web or macOS")
  ("debug" :boolean :slot debug-export
   :doc "use --export-debug instead of --export-release; absent is a release export")
  ("output_path" :string :default ""
   :doc "the file to write, inside the project folder; empty uses the preset's export_path from export_presets.cfg"))

(nlk:define-contract project-read
  (:doc "List a Godot project's export presets and the current state of the artifact each would write: whether it exists, its size in bytes and when it was modified. Ask it before re-exporting, and after an export to confirm the artifact is there. It reads the project and the artifact metadata only; nothing runs.")
  ("project_path" :text :required t
   :doc "the project folder, the one that holds project.godot"))

(defparameter +setup-schema+
  (no-arguments
   "Report the Godot executable this cell found, its version, and the projects the godot-studio section lists, with whether each is a Godot project and how many presets it has. Nothing is installed and nothing runs beyond --version. Run it when a check or an export says it found no engine.")
  "godot_setup takes nothing.")

;;; --- finding and running the engine ---------------------------------------------

(defparameter +executable-names+ '("godot" "godot4" "Godot")
  "What the Godot 4 executable is called on PATH.")

(defparameter +known-executables+
  '("/Applications/Godot.app/Contents/MacOS/Godot"
    "~/Applications/Godot.app/Contents/MacOS/Godot"
    "/usr/local/bin/godot"
    "/usr/bin/godot")
  "Where a desktop install keeps the executable when PATH does not carry it.")

(defun nonblank (text)
  "TEXT as trimmed text when it is not blank; a :path setting arrives as a
pathname, so that is read as its namestring."
  (let ((text (typecase text
                (pathname (uiop:native-namestring text))
                (string text)
                (t nil))))
    (setf text (and text (nlk:trimmed text)))
    (and text (plusp (length text)) text)))

(defun expand-name (text)
  "TEXT with ~ expanded, as a native namestring."
  (let ((value (nlk:expand-home text)))
    (if (pathnamep value) (uiop:native-namestring value) value)))

(defun which (name)
  "NAME's path on this account's PATH, or NIL."
  (loop for directory in (uiop:split-string (or (uiop:getenv "PATH") "")
                                            :separator (string (if (uiop:os-windows-p) #\; #\:)))
        for root = (ignore-errors
                     (uiop:ensure-directory-pathname (uiop:parse-native-namestring directory)))
        for candidate = (and root (ignore-errors (merge-pathnames name root)))
        when (and candidate (uiop:file-exists-p candidate))
          return (uiop:native-namestring candidate)))

(defun find-binary ()
  "The Godot 4 executable: the section's binary when it names one, else a known
name on PATH or a desktop install's own path; a refusal when none was found."
  (let ((configured (nonblank (setting :binary))))
    (if configured
        (let ((path (uiop:parse-native-namestring (expand-name configured))))
          (unless (uiop:file-exists-p path)
            (fail "binary names ~s, which is not a file: set it to the Godot 4 executable itself"
                  configured))
          (uiop:native-namestring path))
        (or (loop for name in +executable-names+ for hit = (which name) when hit return hit)
            (loop for text in +known-executables+
                  for path = (ignore-errors (nlk:expand-home text))
                  when (and path (uiop:file-exists-p path)) return (uiop:native-namestring path))
            (fail "no Godot 4 executable was found: put godot on PATH, or set binary in the godot-studio section to its full path")))))

(defun run-godot (argv &key timeout)
  "Run the Godot executable with ARGV. => (values STDOUT STDERR CODE), CODE its
exit status (NIL when it did not run). The one seam every test stubs."
  (handler-case
      (multiple-value-bind (output error-output code)
          (uiop:run-program argv
                            :output :string :error-output :string
                            :ignore-error-status t
                            :timeout timeout
                            :external-format :utf-8)
        (values (or output "") (or error-output "") code))
    (error (condition)
      (values "" (format nil "~a did not finish: ~a"
                          (first argv) (nlk:one-line (princ-to-string condition)))
              nil))))

(defun tail (text)
  "The last (setting :max-output) characters of TEXT, the way the original
kept the end of a run's output."
  (let ((limit (setting :max-output))
        (text (or text "")))
    (if (> (length text) limit) (subseq text (- (length text) limit)) text)))

;;; --- the project, and its presets ------------------------------------------------

(defun project-root (path)
  "PATH as a directory pathname holding project.godot; a refusal naming what is
missing."
  (let ((given (nonblank path)))
    (unless given
      (fail "project_path is required: the folder that holds project.godot"))
    (let* ((namestring (expand-name given))
           (root (uiop:ensure-directory-pathname (uiop:parse-native-namestring (expand-name namestring)))))
      (unless (uiop:directory-exists-p root)
        (fail "~s is not a directory: project_path is the folder that holds project.godot"
              given))
      (unless (uiop:file-exists-p (merge-pathnames "project.godot" root))
        (fail "no project.godot in ~a: point project_path at a Godot project's own folder"
              (uiop:native-namestring root)))
      root)))

(defun preset-lines (path)
  "PATH's lines, or NONE when it cannot be read."
  (handler-case
      (with-open-file (in path :external-format :utf-8)
        (loop for line = (read-line in nil nil) while line collect line))
    (file-error () nil)))

(defun section-heading (line)
  "LINE's section name when it is a [heading], else NIL."
  (when (and (plusp (length line))
             (char= #\[ (char line 0))
             (char= #\] (char line (1- (length line)))))
    (subseq line 1 (1- (length line)))))

(defun preset-heading-p (section)
  "Is SECTION a preset's own section -- not its .options, not the top level?"
  (and section
       (uiop:string-prefix-p "preset." section)
       (not (search ".options" section))))

(defun strip-quotes (value)
  "VALUE without the double quotes export_presets.cfg writes around a string."
  (if (and (> (length value) 1)
           (char= #\" (char value 0))
           (char= #\" (char value (1- (length value)))))
      (subseq value 1 (1- (length value)))
      value))

(defun line-setting (line)
  "LINE's (KEY . VALUE) when it sets one, KEY downcased; else NIL."
  (let ((at (and (search "=" line) (position #\= line))))
    (when at
      (cons (string-downcase (string-trim '(#\Space #\Tab) (subseq line 0 at)))
            (strip-quotes (string-trim '(#\Space #\Tab) (subseq line (1+ at))))))))

(defun parse-presets (root)
  "ROOT's export_presets.cfg as a list of plists, one per preset: :name,
:export-path and :artifact-path (the file it would write, absolute). A project
without the file has no presets."
  (let ((config (merge-pathnames "export_presets.cfg" root))
        (presets '()) (current '()) (inside nil))
    (dolist (line (preset-lines config))
      (let* ((line (string-trim '(#\Space #\Tab #\Return) line))
             (heading (section-heading line)))
        (cond
          ((null heading)
           (when inside
             (let ((setting (line-setting line)))
               (when setting
                 (cond ((string= "name" (car setting))
                        (setf (getf current :name) (cdr setting)))
                       ((string= "export_path" (car setting))
                        (setf (getf current :export-path) (cdr setting))))))))
          ((preset-heading-p heading)
           (when (getf current :name) (push current presets))
           (setf current '() inside t))
          (t (setf inside nil)))))
    (when (getf current :name) (push current presets))
    (loop for preset in (nreverse presets)
          for export-path = (nonblank (getf preset :export-path))
          collect (append preset
                          (list :artifact-path
                                (and export-path
                                     (namestring (merge-pathnames export-path root))))))))

(defun artifact-path (preset)
  (getf preset :artifact-path))

(defun artifact-state (path)
  "PATH's state. => (values EXISTS SIZE MODIFIED), MODIFIED in epoch seconds and
0 when the artifact is not there."
  (if (and path (uiop:file-exists-p path))
      (let ((exists t)
            (size (handler-case
                      (with-open-file (in path :element-type '(unsigned-byte 8))
                        (file-length in))
                    (error () 0)))
            (modified (or (ignore-errors
                            (- (file-write-date path) (encode-universal-time 0 0 0 1 1 1970 0)))
                          0)))
        (values exists size modified))
      (values nil 0 0)))

;;; --- paths, confined the way the original confined them --------------------------

(defun canonical (namestring)
  "NAMESTRING with . and .. folded and every existing component's symlinks
followed; the file itself need not exist."
  (let* ((namestring (if (pathnamep namestring)
                          (uiop:native-namestring namestring)
                          namestring))
         (directory (uiop:pathname-directory-pathname namestring))
         (name (file-namestring namestring))
         (resolved (or (ignore-errors (truename directory))
                       (uiop:ensure-directory-pathname directory))))
    (uiop:native-namestring (if (and name (plusp (length name)))
                                 (merge-pathnames name resolved)
                                 resolved))))

(defun inside-p (root path)
  "Is PATH inside ROOT, both canonical namestrings, ROOT keeping its trailing
separator?"
  (uiop:string-prefix-p root path))

(defun absolute-name (root path)
  "PATH as the namestring an export writes: absolute stays absolute (~
expanded); a relative one is read from ROOT, so the operator's own directory
never decides where a build lands."
  (let* ((expanded (expand-name path))
         (namestring (if (pathnamep expanded)
                          (uiop:native-namestring expanded)
                          expanded)))
    (if (uiop:absolute-pathname-p (uiop:parse-native-namestring namestring))
        namestring
        (namestring (merge-pathnames namestring root)))))

(defun known-artifacts (presets)
  "Every preset's own artifact, canonical: the files an export may replace."
  (loop for preset in presets
        for path = (artifact-path preset)
        when path collect (canonical path)))

;;; --- godot_headless_check -------------------------------------------------------

(defun check (call)
  "The godot_headless_check tool: run the project headless and quit."
  (running-settings)
  (answering
    (let* ((binary (find-binary))
           (root (project-root (project-check-project-path call)))
           (frames (max 0 (or (project-check-quit-after-frames call) 0)))
           (argv (append (list binary "--headless" "--path" (uiop:native-namestring root))
                         (if (plusp frames)
                             (list "--quit-after" (princ-to-string frames))
                             (list "--quit")))))
      (multiple-value-bind (stdout stderr code)
          (run-godot argv :timeout (setting :check-timeout))
        (nlk:encode-json-object
         (nlk:json-object "project" (uiop:native-namestring root)
                          "command" (coerce argv 'vector)
                          "quit_after_frames" frames
                          "ok" (eql code 0)
                          "returncode" (or code -1)
                          "stdout" (tail stdout)
                          "stderr" (tail stderr)))))))

;;; --- godot_export ---------------------------------------------------------------

(defparameter +template-hint+
  (format nil "export templates for this platform are not installed. Install ~
them from the Godot editor (Project -> Manage Export Templates), or with ~
godot --headless --install-export-templates.")
  "What an export that lacked its templates says, instead of raw engine output.")

(defun export-project (call)
  "The godot_export tool: one headless export from a named preset, its output
confined to the project."
  (running-settings)
  (answering
    (let* ((binary (find-binary))
           (root (project-root (project-export-project-path call)))
           (root-name (namestring (truename root)))
           (preset (nonblank (project-export-preset call)))
           (debug (project-export-debug-export call))
           (presets (parse-presets root))
           (known (known-artifacts presets)))
      (unless preset
        (fail "preset is required: the name export_presets.cfg lists, which godot_builds answers"))
      (let ((chosen (nonblank (project-export-output-path call))))
        (unless chosen
          (let ((match (find preset presets :test #'string=
                                    :key (lambda (preset) (getf preset :name)))))
            (unless (and match (artifact-path match))
              (fail "preset ~s has no export_path in export_presets.cfg -- pass an explicit output_path inside the project"
                    preset))
            (setf chosen (artifact-path match))))
        (let* ((out (canonical (absolute-name root chosen)))
               (out-path (uiop:parse-native-namestring out)))
          (when (uiop:directory-exists-p out-path)
            (fail "output_path must be a file path, not a directory: ~a" out))
          (unless (inside-p root-name out)
            (fail "output_path must be inside the project directory (~a)" root-name))
          (when (and (uiop:file-exists-p out-path)
                     (not (find out known :test #'equal)))
            (fail "refusing to overwrite ~a: not a known export artifact of this project" out))
          (ensure-directories-exist out-path)
          (let ((argv (list binary "--headless" "--path" (uiop:native-namestring root)
                            (if debug "--export-debug" "--export-release")
                            preset out)))
            (multiple-value-bind (stdout stderr code)
                (run-godot argv :timeout (setting :export-timeout))
              (multiple-value-bind (exists size modified) (artifact-state out)
                (let ((answer (nlk:json-object "project" (uiop:native-namestring root)
                                               "preset" preset
                                               "mode" (if debug "debug" "release")
                                               "output_path" out
                                               "command" (coerce argv 'vector)
                                               "ok" (eql code 0)
                                               "returncode" (or code -1)
                                               "stdout" (tail stdout)
                                               "stderr" (tail stderr)
                                               "artifact" (nlk:json-object "exists" exists
                                                                           "size_bytes" size
                                                                           "modified" modified))))
                  (when (and (not (eql code 0))
                             (search "export_templates"
                                     (concatenate 'string (or stdout "") (or stderr ""))))
                    (setf (gethash "hint" answer) +template-hint+))
                  (nlk:encode-json-object answer))))))))))

;;; --- godot_builds ---------------------------------------------------------------

(defun build-entry (preset)
  "One preset's line: its name, the file it would write, and that file's state."
  (let ((path (artifact-path preset)))
    (multiple-value-bind (exists size modified) (artifact-state path)
      (nlk:json-object "preset" (or (getf preset :name) "")
                       "export_path" (or (getf preset :export-path) "")
                       "artifact_path" (or path "")
                       "exists" exists
                       "size_bytes" size
                       "modified" modified))))

(defun builds (call)
  "The godot_builds tool: every preset and its artifact's state."
  (running-settings)
  (answering
    (let* ((root (project-root (project-read-project-path call)))
           (presets (parse-presets root)))
      (nlk:encode-json-object
       (nlk:json-object "project" (uiop:native-namestring root)
                        "presets_file" (and (uiop:file-exists-p
                                              (merge-pathnames "export_presets.cfg" root))
                                             t)
                        "builds" (coerce (loop for preset in presets
                                                collect (build-entry preset))
                                         'vector))))))

;;; --- godot_setup ----------------------------------------------------------------

(defun describe-project (path)
  "One configured project as an answer: whether it is one, and its presets."
  (handler-case
      (let* ((root (project-root path))
             (presets (parse-presets root)))
        (nlk:json-object "path" (uiop:native-namestring root)
                          "exists" t
                          "presets" (length presets)))
    (godot-studio-error (condition)
      (nlk:json-object "path" (or path "")
                        "exists" nil
                        "presets" 0
                        "why" (godot-studio-error-detail condition)))))

(defun diagnose (call)
  "The godot_setup tool: the engine this cell found, its version, the projects."
  (declare (ignore call))
  (running-settings)
  (answering
    (let ((binary (find-binary)))
      (multiple-value-bind (stdout stderr code)
          (run-godot (list binary "--version") :timeout 60)
        (nlk:encode-json-object
         (nlk:json-object "binary" binary
                          "version" (or (nonblank stdout) (nonblank stderr) "")
                          "ok" (eql code 0)
                          "projects" (coerce (loop for project in (setting :projects)
                                                    collect (describe-project project))
                                             'vector)
                          "next_step" +next-step+))))))

;;; --- /godot-studio ---------------------------------------------------------------------

(defun project-line (path)
  "One project as the operator reads it in /godot-studio: its presets and built artifacts."
  (handler-case
      (let* ((root (project-root path))
             (presets (parse-presets root)))
        (format nil "~a: ~d preset~:p, ~d built"
                (uiop:native-namestring root)
                (length presets)
                (count-if (lambda (preset)
                            (multiple-value-bind (exists) (artifact-state (artifact-path preset))
                              exists))
                          presets)))
    (godot-studio-error (condition)
      (format nil "~a -- ~a" (or path "") (godot-studio-error-detail condition)))))

(defun run-slash (args session-id)
  "/godot-studio [PROJECT]: the engine this cell found and the builds of its projects."
  (declare (ignore session-id))
  (handler-case
      (let* ((binary (find-binary))
             (version (multiple-value-bind (stdout stderr)
                          (run-godot (list binary "--version") :timeout 60)
                        (or (nonblank stdout) (nonblank stderr) "version unknown")))
             (named (nonblank args))
             (projects (cond (named (list named))
                             ((setting :projects) (setting :projects))
                             (t '()))))
        (with-output-to-string (out)
          (format out "godot-studio: ~a (~a)~%" binary version)
          (if (null projects)
              (format out "no projects configured: name one, /godot-studio ~a~%"
                      "PATH/TO/PROJECT")
              (dolist (project projects)
                (format out "~a~%" (project-line project))))))
    (godot-studio-error (condition)
      (format nil "godot-studio: ~a" (godot-studio-error-detail condition)))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell godot-studio
  (:section ("godot-studio")
    (:guide "put godot on PATH or name binary; every tool call names its own project; projects is only what /godot-studio reports on; nothing here opens the editor")
    ("binary" :path
     :doc "the Godot 4 executable itself, not the .app directory; unset, godot and godot4 on PATH and a desktop install's own path are tried")
    ("projects" :list
     :doc "project folders /godot-studio reports on, each the folder that holds project.godot; a tool call always names its own project_path")
    ("check_timeout" :integer :default 300 :min 10
     :doc "how long one headless check may take, in seconds")
    ("export_timeout" :integer :default 900 :min 30
     :doc "how long one export may take, in seconds")
    ("max_output" :integer :default 20000 :min 1000
     :doc "how many characters of the engine's own output one answer keeps, from the end"))
  (:help :godot-studio
         "godot-studio: headless Godot 4 checks, exports and build state - (help :godot-studio) for the loop and its pitfalls"
         *manual*)
  (:tool "godot_headless_check" (nlk:contract 'project-check) #'check)
  (:tool "godot_export" (nlk:contract 'project-export) #'export-project)
  (:tool "godot_builds" (nlk:contract 'project-read) #'builds)
  (:tool "godot_setup" +setup-schema+ #'diagnose)
  (:command "godot-studio" 'run-slash
            :description "Godot: the engine this cell found and its projects' builds"))
