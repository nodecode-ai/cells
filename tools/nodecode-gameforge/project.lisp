;;;; project.lisp --- the game folder on disk: where an asset goes, the
;;;; ledger that remembers the recipe that made it, and what is installed.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original keeps gameforge.json at the project root and appends one
;;;; entry per generated asset -- name, kind, files, recipe, licence -- so any
;;;; asset can be regenerated, varied or credited later. This is that file,
;;;; with the same name and the same fields, so a project started by the
;;;; plugin reads here and one started here reads there.

(in-package #:nodecode-gameforge)

(defparameter +ledger-file+ "gameforge.json"
  "The ledger's name at a project's root, the plugin's.")

(defparameter +asset-directories+
  '(("sfx" . "assets/sfx") ("music" . "assets/music") ("sprite" . "assets/sprites")
    ("tileset" . "assets/tiles") ("engine" . ".") ("export" . "dist"))
  "Where each kind of asset lives inside a project.")

(defparameter +generated-licence+ "CC0-1.0 (procedurally generated)"
  "What the ledger records for an asset this cell synthesized: numbers and
shapes this rewrite made, with nothing sampled from anyone.")

(defun safe-name (text fallback)
  "TEXT as a file stem: letters, digits, dash and underscore, at most 48,
or FALLBACK when nothing is left."
  (let* ((trimmed (nlk:trimmed (or text "")))
         (clean (string-trim "_"
                             (with-output-to-string (out)
                               (loop for char across trimmed
                                     do (write-char (if (or (alphanumericp char) (char= char #\-)
                                                             (char= char #\_))
                                                        char #\_)
                                                    out))))))
    (if (plusp (length clean))
        (subseq clean 0 (min 48 (length clean)))
        fallback)))

(defun absolute-path-p (text)
  (or (uiop:absolute-pathname-p (uiop:parse-native-namestring text))
      (uiop:string-prefix-p "~/" text)))

(defun project-root (requested)
  "The project folder a call writes to: REQUESTED (absolute, or relative to
the cell's directory), else the section's default project. Created if new."
  (let* ((raw (if (plusp (length (nlk:trimmed (or requested ""))))
                  (nlk:trimmed requested)
                  (let ((default (nlk:trimmed (or (setting :default-project) ""))))
                    (when (zerop (length default))
                      (fail "no project was named and gameforge.default_project is empty: name a ~
project folder, or set default_project in the gameforge section so a call may omit it"))
                    default)))
         (expanded (if (uiop:string-prefix-p "~/" raw)
                       (nlk:home (subseq raw 2))
                       (uiop:parse-native-namestring raw)))
         (root (uiop:ensure-directory-pathname
                (if (uiop:absolute-pathname-p expanded)
                    expanded
                    (merge-pathnames expanded (setting :directory))))))
    (when (search ".." (namestring root))
      (fail "~a walks out of its folder: name a project without .. in its path" raw))
    (ensure-directories-exist root)
    root))

(defun asset-directory (root kind)
  (let ((relative (or (cdr (assoc kind +asset-directories+ :test #'string=)) ".")))
    (ensure-directories-exist (merge-pathnames (format nil "~a/" relative) root))))

(defun ledger-path (root)
  (merge-pathnames +ledger-file+ root))

(defun empty-ledger (root)
  (let ((ledger (make-hash-table :test #'equal)))
    (setf (gethash "project" ledger) (car (last (pathname-directory root)))
          (gethash "created" ledger) (nlk:iso-time (nlk:unix-now))
          (gethash "assets" ledger) (vector))
    ledger))

(defun read-ledger (root)
  "The ledger at ROOT, or a new one; never a refusal, since an unreadable
ledger is a ledger this cell can still append to."
  (let ((path (ledger-path root)))
    (if (probe-file path)
        (handler-case (let ((decoded (nlk:decode-json (uiop:read-file-string path))))
                        (if (hash-table-p decoded) decoded (empty-ledger root)))
          (error () (empty-ledger root)))
        (empty-ledger root))))

(defun ledger-assets (ledger)
  "The ledger's entries as a list, newest last."
  (let ((assets (gethash "assets" ledger)))
    (cond ((vectorp assets) (coerce assets 'list))
          ((listp assets) assets)
          (t '()))))

(defun asset-relative (root path)
  "PATH relative to ROOT, as the ledger stores it: a project's own spelling,
so a project can be moved and still regenerate."
  (enough-namestring path root))

(defun record-asset (root kind name files recipe)
  "Append (or replace, by name and kind) one entry, and answer it."
  (let* ((ledger (read-ledger root))
         (entry (make-hash-table :test #'equal))
         (older (remove-if (lambda (asset)
                             (and (equal name (gethash "name" asset))
                                  (equal kind (gethash "kind" asset))))
                           (ledger-assets ledger))))
    (setf (gethash "name" entry) name
          (gethash "kind" entry) kind
          (gethash "files" entry) (map 'vector (lambda (path) (asset-relative root path)) files)
          (gethash "recipe" entry) recipe
          (gethash "created" entry) (nlk:iso-time (nlk:unix-now))
          (gethash "license" entry) +generated-licence+)
    (setf (gethash "assets" ledger) (map 'vector #'identity (append older (list entry))))
    (with-open-file (out (ledger-path root) :direction :output :if-exists :supersede
                                            :if-does-not-exist :create)
      (write-string (nlk:encode-json-object ledger) out))
    entry))

(defun project-summary (root)
  "(values TOTAL KIND->COUNT) for the ledger at ROOT."
  (let ((counts (make-hash-table :test #'equal)) (assets (ledger-assets (read-ledger root))))
    (dolist (asset assets)
      (let ((kind (gethash "kind" asset)))
        (setf (gethash kind counts) (1+ (gethash kind counts 0)))))
    (values (length assets) counts)))

;;; --- what a user already has installed ----------------------------------------

(defparameter +engine-programs+
  '(;; (report-name . program names)
    ("godot" "godot" "godot4" "godot3")
    ("blender" "blender")
    ("unity" "unity" "unityhub")
    ("unreal" "UnrealEditor-Cmd" "UnrealEditor")
    ("chrome" "google-chrome" "chromium" "chromium-browser" "chrome")
    ("ffmpeg" "ffmpeg"))
  "The programs an engine scaffold or export would need, and every name each
answers to: the same list the original probes for.")

(defun path-directories ()
  (let ((path (uiop:getenv "PATH")))
    (if (null path) '() (uiop:split-string path :separator '(#\:)))))

(defun executable-at (path)
  (and (probe-file path) t))

(defun find-program (names)
  "The first of NAMES found on PATH, as a namestring, or NIL."
  (loop for directory in (path-directories)
        thereis (loop for name in names
                      for candidate = (merge-pathnames name (merge-pathnames "" directory))
                      when (executable-at candidate) return (namestring candidate))))

(defvar *program-finder* #'find-program
  "How a program is looked for: the tests bind it, so `status' and `run' can
be answered on a machine with nothing installed.")

(defvar *launcher* #'uiop:launch-program
  "How a program is started: the tests bind it, so a run is recorded and not
performed.")

(defun program-names-for (engine)
  "The program names ENGINE answers to on this machine's PATH."
  (cdr (assoc engine +engine-programs+ :test #'string=)))

(defun engine-program (engine)
  "The program ENGINE would run here, or NIL."
  (let ((names (program-names-for engine)))
    (and names (funcall *program-finder* names))))

(defun engine-status ()
  "An alist of report-name to what was found, or NIL."
  (loop for (name . programs) in +engine-programs+
        collect (cons name (funcall *program-finder* programs))))
