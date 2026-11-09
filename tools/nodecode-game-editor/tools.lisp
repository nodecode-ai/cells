;;;; tools.lisp --- the four reads the model calls, and their one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin published eight tools; four of them read the .tscn,
;;;; .gd and project.godot files on disk and never asked the editor, and those
;;;; four are here, with the same names and the same ops. The other four
;;;; (editor_look, asset_look, plan_change, apply_plan) need a Godot editor
;;;; running the Godot AI add-on; a cell on a project folder does not.
;;;;
;;;; Every member of every contract is refused before the body runs if it is
;;;; the wrong shape, and the body reads the materialized record, so a tool
;;;; cannot advertise one op and dispatch another.

(in-package #:nodecode-game-editor)

;;; --- the shape every answer takes ------------------------------------------------

(defun answer (text)
  "TEXT as one answer, cut to the section's character cap with the cut counted."
  (nlk:clip text (setting :read-chars) :disclose t))

(defun offset-or (offset)
  "OFFSET as a row to start at."
  (if (and (integerp offset) (plusp offset)) offset 0))

(defun limit-or (limit)
  "LIMIT when it is a row count, else the section's read_lines."
  (if (and (integerp limit) (plusp limit)) limit (setting :read-lines)))

(defun extension-list (type)
  "TYPE as the file extensions it names, one per comma."
  (when (and type (plusp (length (nlk:trimmed type))))
    (loop for part in (uiop:split-string type :separator ",")
          for name = (string-downcase (string-left-trim "." (nlk:trimmed part)))
          when (plusp (length name)) collect name)))

(defun resource-head-type (file)
  "The type FILE declares in its first line -- a resource's type, a scene's
scene -- or its extension when it declares none."
  (let ((line (find-if (lambda (line) (header-line-p (string-left-trim " \t" line)))
                        (nlk:lines (or (nlk:read-text file 4096) "")))))
    (if line
        (multiple-value-bind (kind attrs) (parse-header (string-left-trim " \t" line))
          (cond ((string= kind "gd_scene") "scene")
                (t (or (cdr (assoc "type" attrs :test #'string=)) kind))))
        (or (pathname-type file) "?"))))

;;; --- scene_look ----------------------------------------------------------------

(nlk:define-contract scene-look-request
  (:doc "Read a Godot 4 scene file (.tscn) from disk: the node tree, a search by name, type or group, a node's children, the values a node stores, its groups and its signal connections. It reads the file only -- the editor is not asked, so no project code runs, and unsaved editor edits are not in it. An inherited scene's root comes from another file and says so.")
  ("op" :text :default "scene_tree" :options '("scene_tree" "find_nodes" "children" "properties" "groups" "signals")
        :doc "what to read: the tree, a search, a node's children, a node's stored values, groups, or connections")
  ("scene" :string :doc "a res:// .tscn path; left out, the project's main scene")
  ("path" :string :doc "a node path from the scene root, such as /Main/Player; empty is the root")
  ("name" :string :doc "part of a node name, for find_nodes")
  ("type" :string :slot node-type :doc "a node type, such as Sprite2D, for find_nodes")
  ("group" :string :doc "a group name, for find_nodes")
  ("fields" :array :of :string :doc "only these stored values, for properties")
  ("offset" :integer :default 0 :doc "skip this many rows")
  ("limit" :integer :default 0 :doc "at most this many rows; 0 is the configured read_lines"))

(define-verb scene-look (request)
  "The scene_look tool: one of six views of a .tscn file, or a refusal."
  (let* ((op (scene-look-request-op request))
         (offset (offset-or (scene-look-request-offset request)))
         (limit (limit-or (scene-look-request-limit request)))
         (scene (load-scene (scene-look-request-scene request))))
    (answer
     (cond ((string= op "find_nodes")
            (scene-find-text scene (scene-look-request-name request)
                              (scene-look-request-node-type request)
                              (scene-look-request-group request) offset limit))
           ((string= op "children")
            (scene-children-text scene (scene-look-request-path request) offset limit))
           ((string= op "properties")
            (scene-properties-text scene (scene-look-request-path request)
                                   (scene-look-request-fields request)))
           ((string= op "groups")
            (scene-groups-text scene (scene-look-request-path request)))
           ((string= op "signals")
            (scene-signals-text scene (scene-look-request-path request) offset limit))
           (t (scene-tree-text scene offset limit))))))

;;; --- script_look ---------------------------------------------------------------

(nlk:define-contract script-look-request
  (:doc "Read a GDScript (.gd) or C# (.cs) file from disk: its source, numbered and in slices, or an outline of every class, signal, exported value, constant and function it declares. It reads the file only, so no project code runs.")
  ("op" :text :default "source" :options '("source" "outline")
        :doc "the source itself, or the declarations in it")
  ("path" :text :required t :doc "a res:// .gd or .cs path")
  ("offset" :integer :default 0 :doc "skip this many lines, for source")
  ("limit" :integer :default 0 :doc "at most this many lines, for source; 0 is the configured read_lines"))

(define-verb script-look (request)
  "The script_look tool: a script's source or its outline, or a refusal."
  (let* ((path (script-look-request-path request))
         (file (project-file path :what "script")))
    (unless (script-file-p file)
      (fail "~s is not a GDScript (.gd) or C# (.cs) script" path))
    (answer (if (string= (script-look-request-op request) "outline")
                (script-outline-text file)
                (script-source-text file (offset-or (script-look-request-offset request))
                                    (limit-or (script-look-request-limit request)))))))

;;; --- file_look -----------------------------------------------------------------

(nlk:define-contract file-look-request
  (:doc "Read or search the files of a Godot project on disk: a res:// text file with its lines numbered, the files in a folder filtered by name and type, or the resources by their declared type. It reads the project folder only, so no project code runs.")
  ("op" :text :default "text" :options '("text" "files" "resources")
        :doc "one file's text, the files there are, or the resources by type")
  ("path" :string :doc "a res:// file for text, or a res:// folder to list under")
  ("name" :string :doc "part of a file name, for files")
  ("type" :string :slot file-type :doc "file extensions, comma separated, for files; a resource type, for resources")
  ("offset" :integer :default 0 :doc "skip this many rows")
  ("limit" :integer :default 0 :doc "at most this many rows; 0 is the configured read_lines"))

(defun files-text (request root)
  "The project's files, filtered by the request's name, type and folder."
  (let* ((folder (file-look-request-path request))
         (base (if (and folder (plusp (length (nlk:trimmed folder))))
                   (res-name (project-file folder :what "folder") root)
                   "res://"))
         (files (project-files :extensions (extension-list (file-look-request-file-type request))
                               :name (file-look-request-name request)))
         (rows (loop for file in files
                      for name = (res-name file root)
                      when (uiop:string-prefix-p base name) collect name)))
    (format nil "~d file~:p under ~a~@[, name holding ~s~]~@[, type ~a~]:~%~a"
            (length rows) base
            (file-look-request-name request) (file-look-request-file-type request)
            (rows-text rows (offset-or (file-look-request-offset request))
                       (limit-or (file-look-request-limit request))))))

(defun resources-text (request root)
  "The project's resources of the type the request names, or of every type."
  (let* ((wanted (nlk:trimmed (or (file-look-request-file-type request) "")))
         (files (project-files :extensions '("tres" "res" "tscn")))
         (rows (loop for file in files
                      for type = (resource-head-type file)
                      when (or (zerop (length wanted)) (string-equal wanted type))
                        collect (format nil "~a  (~a)" (res-name file root) type))))
    (format nil "~d resource~:p~@[ of type ~a~] in the project:~%~a"
            (length rows) wanted
            (rows-text rows (offset-or (file-look-request-offset request))
                       (limit-or (file-look-request-limit request))))))

(defun file-text (request)
  "One res:// text file, its lines numbered."
  (let ((file (project-file (or (file-look-request-path request)
                               (fail "file_look op text needs a res:// path"))
                            :what "file")))
    (let ((lines (nlk:lines (or (nlk:read-text file) (project-unreadable (file-namestring file))))))
      (format nil "~a, ~d line~:p~%~a" (file-namestring file) (length lines)
              (numbered-lines lines (offset-or (file-look-request-offset request))
                              (limit-or (file-look-request-limit request)))))))

(define-verb file-look (request)
  "The file_look tool: a file's text, the files there are, or the resources, or a refusal."
  (let ((op (file-look-request-op request))
        (root (truename (project-root))))
    (answer (cond ((string= op "files") (files-text request root))
                  ((string= op "resources") (resources-text request root))
                  (t (file-text request))))))

;;; --- project_look --------------------------------------------------------------

(nlk:define-contract project-look-request
  (:doc "Read project.godot from disk: one setting written section/name, the autoloads, the input map with the keys each action is bound to, or a summary of the project. It reads the settings file only, so no project code runs.")
  ("op" :text :default "get" :options '("get" "autoloads" "input_map" "summary")
        :doc "one setting, the autoloads, the input map, or the project in a few lines")
  ("key" :string :doc "a project setting, such as application/run/main_scene, for get"))

(define-verb project-look (request)
  "The project_look tool: one setting, the autoloads, the input map or a summary."
  (let ((op (project-look-request-op request))
        (key (nlk:trimmed (or (project-look-request-key request) ""))))
    (answer
     (cond ((string= op "autoloads") (autoloads-text))
           ((string= op "input_map") (input-map-text))
           ((and (string= op "get") (plusp (length key)))
            (format nil "~a = ~a" key (project-setting key)))
           (t (project-summary-text))))))
