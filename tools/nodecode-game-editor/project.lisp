;;;; project.lisp --- the project folder, its settings file, and res:// paths.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One folder is the subject: game-editor.project names it, project.godot
;;;; is its settings file, and every path this cell answers with is res:// --
;;;; the name the editor shows -- never a path of the machine it runs on. A
;;;; path that resolves outside that folder, or through a hidden one, is
;;;; refused before it is read, the rule the original plugin applied to its
;;;; res:// and uid:// arguments.

(in-package #:nodecode-game-editor)

;;; --- the folder ---------------------------------------------------------------

(defun project-root ()
  "The configured Godot project folder, or a refusal naming the setting."
  (or (setting :project)
      (fail "game-editor.project is not set: name the Godot project folder, the one that holds project.godot, in the game-editor section of the config, then (restart-cells \"nodecode-game-editor\")")))

(defun project-ini ()
  "project.godot as INI sections, or a refusal when the folder holds none."
  (let* ((root (project-root))
         (text (nlk:read-text (merge-pathnames "project.godot" root))))
    (or (and text (parse-ini text))
        (fail "no project.godot in ~a: game-editor.project must name a Godot 4 project folder"
              (namestring root)))))

(defun project-setting (key)
  "project.godot's KEY, written section/name, as it is stored: a string
without its quotes, anything else as written."
  (let ((slash (position #\/ key)))
    (unless slash
      (fail "~s is no project setting: write section/name, such as application/run/main_scene"
            key))
    (let ((value (ini-get (project-ini)
                          (subseq key 0 slash) (subseq key (1+ slash)))))
      (unless value (fail "project.godot has no setting ~a" key))
      (plain-value value))))

(defun project-name ()
  "The project's name, or a refusal when project.godot names none."
  (project-setting "application/config/name"))

(defun main-scene ()
  "The project's main scene, res:// as project.godot stores it, or a refusal."
  (project-setting "application/run/main_scene"))

;;; --- res:// paths ---------------------------------------------------------------

(defun res-relative (path)
  "PATH as a path relative to the project folder: res:// and a plain relative
path are both taken from it; user:// and an absolute path are refused."
  (cond ((uiop:string-prefix-p "res://" path) (subseq path 6))
        ((uiop:string-prefix-p "user://" path)
         (fail "~s is in user://, the game's player-data folder, not the project" path))
        ((uiop:absolute-pathname-p (pathname path))
         (fail "~s is an absolute path: name a res:// path or one inside the project" path))
        (t path)))

(defun resolved (absolute)
  "ABSOLUTE with its folder resolved through any symbolic link; a file that
does not exist keeps its own last component."
  (or (ignore-errors (truename absolute))
      (let ((directory (uiop:pathname-directory-pathname absolute)))
        (merge-pathnames (file-namestring absolute)
                         (or (ignore-errors (truename directory)) directory)))))

(defun project-file (path &key (what "file"))
  "PATH as a file inside the project folder, or a refusal: a path that
resolves outside the folder, or one through a hidden file or folder, is refused
before it is read."
  (let* ((root (truename (project-root)))
         (relative (res-relative path))
         (file (resolved (merge-pathnames (pathname relative) root))))
    (when (find-if (lambda (component)
                     (and (plusp (length component))
                          (char= #\. (char component 0))
                          (not (member component '("." "..") :test #'string=))))
                   (uiop:split-string relative :separator "/"))
      (fail "~a ~s is in a hidden file or folder" what path))
    (unless (uiop:subpathp file root)
      (fail "~a ~s is outside the project folder" what path))
    file))

(defun res-name (file root)
  "FILE's res:// name, the one the editor shows: the project folder taken off
its namestring."
  (let ((namestring (namestring file))
        (root-name (namestring root)))
    (if (uiop:string-prefix-p root-name namestring)
        (concatenate 'string "res://" (subseq namestring (length root-name)))
        namestring)))

(defun project-unreadable (path)
  "A refusal that names PATH as unreadable text."
  (fail "~a could not be read as UTF-8 text, or is empty" path))

;;; --- walking it ----------------------------------------------------------------

(defun hidden-directory-p (directory)
  "Whether DIRECTORY's own name starts with a dot: .godot, .git, .import."
  (let ((name (car (last (pathname-directory directory)))))
    (and (stringp name) (plusp (length name)) (char= #\. (char name 0)))))

(defun project-files (&key extensions name (limit 5000))
  "Every file under the project folder, hidden folders skipped, in path order;
EXTENSIONS keeps those whose type is named (no dot), NAME those whose file
name holds it. At most LIMIT files, so one read stays cheap."
  (let ((files '()) (stack (list (truename (project-root)))) (seen 0))
    (loop while (and stack (< seen limit))
          do (let ((directory (pop stack)))
               (dolist (file (uiop:directory-files directory))
                 (push file files) (incf seen))
               (dolist (sub (uiop:subdirectories directory))
                 (unless (hidden-directory-p sub) (push sub stack)))))
    (let ((kept (remove-if-not
                 (lambda (file)
                   (and (or (null extensions)
                            (member (string-downcase (or (pathname-type file) ""))
                                    extensions :test #'string=))
                        (or (null name) (string= name "")
                            (search (string-downcase name)
                                    (string-downcase (file-namestring file))))))
                 files)))
      (sort kept #'string< :key #'namestring))))
