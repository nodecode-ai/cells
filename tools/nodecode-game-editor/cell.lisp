;;;; cell.lisp --- the manual, /game-editor, and the one declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Four reads of a Godot 4 project on disk, plus a command that answers
;;;; the first question a person asks about a project they have just opened:
;;;; what is in it. Nothing here starts Godot, opens a socket or writes a
;;;; file; every answer comes from project.godot, a .tscn, a .gd or a .cs.

(in-package #:nodecode-game-editor)

;;; --- the manual ---------------------------------------------------------------
;;; (help :game-editor) answers it while the cell runs; a request carries the one
;;; line the :HELP clause below gives, never this text.

(defparameter +primer+
  "The game-editor cell reads a Godot 4 project from the files in its folder,
through four tools and through eval. The editor is not asked, nothing runs, and
unsaved edits in an open editor are not in these answers. Set game-editor.project
to the project folder (the one holding project.godot).
  scene_look     a .tscn file: op scene_tree (the node tree), find_nodes (name,
      type or group), children (a node's), properties (the values it stores in
      the file), groups, signals (the connections). path is a node path from the
      root, /Main/Player; scene is a res:// path, the main scene when left out.
  script_look    a .gd or .cs file: op source (numbered lines, offset and limit)
      or outline (classes, signals, exports, constants, functions, with lines).
  file_look      op text (a res:// file, numbered), files (what is there, by name
      and type), resources (by their declared type).
  project_look   op get (one setting, section/name), autoloads, input_map (each
      action and its keys), summary.
The same reads answer through eval, each a string: (game-editor:scene-look ...)
takes the request object the tool takes. Every refusal is ERROR:
GAME-EDITOR-ERROR with the reason. /game-editor answers the project at a glance
and its panel; /game-editor scenes, scripts, settings, or a name to find."
  "What (help :game-editor) answers while the cell runs.")

;;; --- /game-editor ---------------------------------------------------------------

(defun res-list-text (heading extensions)
  "HEADING, then every file of EXTENSIONS in the project, by its res:// name."
  (let* ((root (truename (project-root)))
         (rows (mapcar (lambda (file) (res-name file root))
                       (project-files :extensions extensions))))
    (if (null rows)
        (format nil "~a: none" heading)
        (format nil "~a: ~d~%~{~a~^~%~}" heading (length rows) rows))))

(defun settings-text ()
  "project.godot's sections and how many settings each holds."
  (let ((ini (project-ini)))
    (format nil "project.godot, ~d section~:p:~%~{~a~^~%~}"
            (length ini)
            (loop for (name . pairs) in ini
                  collect (format nil "  [~a] ~d setting~:p" name (length pairs))))))

(defun find-files-text (needle)
  "Every project file whose res:// name holds NEEDLE."
  (let* ((root (truename (project-root)))
         (needle (string-downcase needle))
         (rows (loop for file in (project-files)
                      for name = (res-name file root)
                      when (search needle (string-downcase name)) collect name)))
    (if (null rows)
        (format nil "no file in ~a holds ~s in its name" (project-name) needle)
        (format nil "~d file~:p of ~a hold ~s:~%~{~a~^~%~}"
                (length rows) (project-name) needle (subseq rows 0 (min 60 (length rows)))))) )

(defun panel ()
  "Bare /game-editor's dialog: the three lists and a search, each row running
its line."
  (nle:list-dialog
   :title "Game editor"
   :summary (format nil "~a, read from its files" (project-name))
   :rows (list (nle:run-row "Scenes" "/game-editor scenes"
                             :detail "every .tscn in the project")
               (nle:run-row "Scripts" "/game-editor scripts"
                             :detail "every .gd and .cs")
               (nle:run-row "Project settings" "/game-editor settings"
                             :detail "project.godot's sections")
               (nle:ask-row "Find a file" "Name" "/game-editor"
                            :placeholder "part of a file name" :verb "find"
                            :detail "res:// names holding it"))))

(defun run-slash (args session-id)
  "/game-editor typed alone: the project at a glance and its panel. NAME is a
file search; scenes, scripts and settings list those."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (cond ((string= text "") (values (project-summary-text) (panel)))
          ((string-equal text "scenes") (res-list-text "scenes" '("tscn")))
          ((string-equal text "scripts") (res-list-text "scripts" '("gd" "cs")))
          ((string-equal text "settings") (settings-text))
          (t (find-files-text text)))))

;;; --- the declaration -------------------------------------------------------------

(define-verb game-editor-overview ()
  "The project at a glance, the text /game-editor answers with."
  (project-summary-text))

(nle:define-cell game-editor
  (:section ("game-editor")
    (:guide "project is the Godot project folder, the one holding project.godot; read_lines and read_chars bound one answer, and nothing is written")
    ("project" :directory :required t
               :doc "the Godot project folder: the one that holds project.godot")
    ("read_lines" :integer :default 200 :min 1
                  :doc "how many lines or rows one read answers with, unless the call asks for fewer")
    ("read_chars" :integer :default 6000 :min 200
                  :doc "how many characters one answer carries; a longer one says how much it left"))
  (:tool "scene_look" (nlk:contract 'scene-look-request) 'scene-look)
  (:tool "script_look" (nlk:contract 'script-look-request) 'script-look)
  (:tool "file_look" (nlk:contract 'file-look-request) 'file-look)
  (:tool "project_look" (nlk:contract 'project-look-request) 'project-look)
  (:help :game-editor "game-editor: a Godot 4 project read from its files - scenes, scripts, settings; /game-editor" +primer+)
  (:command "game-editor" 'run-slash
            :catches 'game-editor-error
            :description "The Godot project on disk: its scenes, scripts and settings"
            :argument-hint "scenes | scripts | settings | NAME"))
