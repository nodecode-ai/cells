;;;; script.lisp --- a script read as source, and as an outline of it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A GDScript file is classes, signals, exported values and functions at
;;;; column zero, with the body of each indented under it; a C# file is the
;;;; same shape with other heads. The outline reads that shape so a reader can
;;;; see what a script offers without reading every line of it, and the source
;;;; op answers the lines themselves, numbered, in slices.

(in-package #:nodecode-game-editor)

(defparameter *gdscript-heads*
  '("extends " "class_name " "signal " "@export" "@onready" "@tool" "@icon"
    "@static_unload" "const " "static var " "var " "static func " "func ")
  "The GDScript lines a reader looks for at column zero, in the order a line
is asked which it is (static func before func, static var before var).")

(defparameter *csharp-heads*
  '("namespace " "public class " "internal class " "private class " "class "
    "public struct " "struct " "public interface " "interface " "enum "
    "public enum ")
  "The C# lines a reader looks for at column zero.")

(defun top-level-p (line)
  "Whether LINE starts at column zero, which in GDScript and C# is where a
class member is declared."
  (and (plusp (length line))
       (not (member (char line 0) '(#\Space #\Tab) :test #'char=))))

(defun outline-head (line heads)
  "The head LINE opens with, or NIL."
  (find-if (lambda (head) (uiop:string-prefix-p head line)) heads))

(defun numbered-lines (lines offset limit)
  "LINES from OFFSET, at most LIMIT of them, each behind its number, and a
last line saying how to read on."
  (let* ((total (length lines))
         (start (min (max 0 (or offset 0)) total))
         (end (min total (+ start (max 1 (or limit 1)))))
         (width (length (princ-to-string total))))
    (format nil "~{~a~^~%~}~@[~%... ~d more line~:p; pass offset ~d~]"
            (loop for index from start below end
                  for line in (subseq lines start (min total end))
                  collect (format nil "~vd  ~a" width (1+ index) line))
            (- total end) (and (> total end) end))))

(defun script-lines (file)
  "FILE's lines, or a refusal."
  (nlk:lines (or (nlk:read-text file) (project-unreadable (file-namestring file)))))

(defun script-source-text (file offset limit)
  "FILE's source, numbered, from OFFSET, at most LIMIT lines."
  (let ((lines (script-lines file)))
    (format nil "~a, ~d line~:p~%~a" (file-namestring file) (length lines)
            (numbered-lines lines offset limit))))

(defun script-outline-text (file)
  "FILE's outline: every class, signal, exported value, constant and
function declared at column zero, with its line number."
  (let* ((lines (script-lines file))
         (heads (if (string-equal "cs" (or (pathname-type file) ""))
                    *csharp-heads* *gdscript-heads*))
         (rows '()))
    (loop for line in lines
          for number from 1
          when (and (top-level-p line) (outline-head line heads))
            do (push (format nil "~5d  ~a" number line) rows))
    (format nil "~a, ~d line~:p, ~d declaration~:p:~%~{~a~^~%~}"
            (file-namestring file) (length lines) (length rows)
            (if rows (reverse rows) (list "  (none at column zero)")))))

(defun script-file-p (file)
  "Whether FILE is a script this cell outlines: GDScript or C#."
  (member (string-downcase (or (pathname-type file) ""))
          '("gd" "cs" "gdshader") :test #'string=))
