;;;; package.lisp --- the GAME-EDITOR package: four reads of a Godot project on disk.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed GAME-EDITOR so an eval form reads
;;;; (game-editor:scene-look ...). Every exported verb answers a STRING --
;;;; the text the model reads -- and every refusal is GAME-EDITOR-ERROR,
;;;; which a tool call renders as a failed call and the eval snippet as
;;;; "ERROR: GAME-EDITOR-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `game-editor'
;;;; section declaration in cell.lisp (NLE:DEFINE-CELL), read with
;;;; (SETTING :project): a directory, a line cap and a character cap.

(defpackage #:nodecode-game-editor
  (:use #:cl)
  (:nicknames #:game-editor)
  (:export
   ;; the four model-facing reads, each a string or a refusal
   #:scene-look #:script-look #:file-look #:project-look
   ;; the one condition every refusal is
   #:game-editor-error))

(in-package #:nodecode-game-editor)

(nlk:define-peripheral game-editor :not-running t)
