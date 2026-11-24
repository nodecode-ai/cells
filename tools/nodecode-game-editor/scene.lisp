;;;; scene.lisp --- a .tscn file read as the tree it describes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Godot 4 scene file is the node tree in text: one [node ...] section a
;;;; node, in depth-first order, its stored values under it, its script an
;;;; ExtResource, and one [connection ...] section a signal wire. This file
;;;; reads that text and answers the six views the original plugin's
;;;; scene_look took from the same file: the tree, a search, a node's
;;;; children, a node's stored values, its groups, and the connections.
;;;; Nothing here opens the editor, so nothing here runs project code.

(in-package #:nodecode-game-editor)

;;; --- what a scene file holds ---------------------------------------------------

(defstruct (scene-node (:constructor make-scene-node))
  "One [node ...] section: the header Godot wrote and the values under it."
  (name "?")
  type
  parent
  path
  index
  instance
  (groups '())
  (props '())
  line)

(defstruct (godot-scene (:constructor make-godot-scene))
  "One .tscn file: its header, its nodes in file order, its connections, and
its external resources by id."
  (path "")
  format
  uid
  (nodes '())
  (connections '())
  (external '()))

(defun parse-group-list (value)
  "VALUE as a node header's groups=[...] => the group names."
  (when value
    (let ((text (string-trim "[] " value)) (out '()) (start 0) (in-string nil))
      (loop for index from 0 below (length text)
            for char = (char text index)
            do (cond ((char= char #\") (setf in-string (not in-string)))
                     ((and (char= char #\,) (not in-string))
                      (push (string-trim " \"" (subseq text start index)) out)
                      (setf start (1+ index)))))
      (push (string-trim " \"" (subseq text start)) out)
      (remove "" (nreverse out) :test #'string=))))

(defun ext-id (value)
  "VALUE's ExtResource id, from ExtResource(\"1_ab\"), or NIL."
  (when (and value (uiop:string-prefix-p "ExtResource(\"" value))
    (let ((close (position #\" value :start 13)))
      (and close (subseq value 13 close)))))

(defun ext-target (value external)
  "VALUE with an ExtResource id resolved to the resource's path, or VALUE as
it was written."
  (let ((id (ext-id value)))
    (or (and id (cdr (assoc "path" (cdr (assoc id external :test #'string=))
                            :test #'string=)))
        value)))

(defun parse-scene (text)
  "TEXT as a .tscn file => the scene it describes, nodes in file order."
  (let ((scene (make-godot-scene)) (current nil) (last nil) (number 0))
    (dolist (line (nlk:lines text))
      (incf number)
      (let ((line (string-right-trim " \t" line)))
        (cond ((comment-line-p line))
              ((header-line-p (string-left-trim " \t" line))
               (multiple-value-bind (kind attrs) (parse-header line)
                 (setf last nil)
                 (cond ((string= kind "gd_scene")
                        (setf (godot-scene-format scene) (cdr (assoc "format" attrs :test #'string=))
                              (godot-scene-uid scene) (cdr (assoc "uid" attrs :test #'string=))
                              current nil))
                       ((string= kind "ext_resource")
                        (push (cons (cdr (assoc "id" attrs :test #'string=)) attrs) (godot-scene-external scene))
                        (setf current nil))
                       ((string= kind "connection")
                        (push attrs (godot-scene-connections scene))
                        (setf current nil))
                       ((string= kind "node")
                        (setf current (make-scene-node))
                        (setf (scene-node-name current) (or (cdr (assoc "name" attrs :test #'string=)) "?")
                              (scene-node-type current) (cdr (assoc "type" attrs :test #'string=))
                              (scene-node-parent current) (cdr (assoc "parent" attrs :test #'string=))
                              (scene-node-instance current) (cdr (assoc "instance" attrs :test #'string=))
                              (scene-node-index current) (cdr (assoc "index" attrs :test #'string=))
                              (scene-node-groups current) (parse-group-list (cdr (assoc "groups" attrs :test #'string=)))
                              (scene-node-line current) number)
                        (push current (godot-scene-nodes scene)))
                       (t (setf current nil)))))
              ((and current (split-assignment line))
               (multiple-value-bind (key value) (split-assignment line)
                 (setf last (cons key value))
                 (push last (scene-node-props current))))
              (last
               (setf (cdr last) (concatenate 'string (cdr last) " " (string-trim " \t" line)))))))
    (setf (godot-scene-nodes scene) (nreverse (godot-scene-nodes scene))
          (godot-scene-connections scene) (nreverse (godot-scene-connections scene))
          (godot-scene-external scene) (nreverse (godot-scene-external scene)))
    (index-scene scene)
    scene))

(defun index-scene (scene)
  "Give every node its path, the root's name first as the editor writes a node
path, and resolve the scene it instances from the file's external resources.
File order is parent before child, so one pass is enough: a child's parent
attribute is a path from the root, and . is the root itself."
  (let ((root-name (scene-node-name (scene-root-node scene))))
    (dolist (node (godot-scene-nodes scene))
      (let* ((parent (scene-node-parent node))
             (rest (if (or (null parent) (string= parent "."))
                       (scene-node-name node)
                       (concatenate 'string parent "/" (scene-node-name node)))))
        (setf (scene-node-path node)
              (if (eq node (scene-root-node scene))
                  (concatenate 'string "/" rest)
                  (concatenate 'string "/" root-name "/" rest)))
        (setf (scene-node-instance node)
              (ext-target (scene-node-instance node) (godot-scene-external scene))))))
  scene)

;;; --- finding one node ----------------------------------------------------------

(defun scene-root-node (scene)
  "The scene's root: Godot writes it first."
  (first (godot-scene-nodes scene)))

(defun normalize-node-path (path)
  "PATH as this cell reads it: no leading or trailing slash, and . or the
empty path both name the scene root."
  (string-trim " /" (or path "")))

(defun node-at (scene path)
  "The node PATH names, or NIL."
  (let ((wanted (normalize-node-path path)))
    (if (zerop (length wanted))
        (scene-root-node scene)
        (find-if (lambda (node)
                   (string= wanted (string-trim "/" (scene-node-path node))))
                 (godot-scene-nodes scene)))))

(defun require-node (scene path)
  "NODE-AT, or a refusal naming the nodes the scene does have."
  (or (node-at scene path)
      (fail "the scene has no node at /~a; it has ~{~a~^, ~}"
            (normalize-node-path path)
            (loop for node in (godot-scene-nodes scene) collect (scene-node-path node)))))

(defun node-script (scene node)
  "The script NODE stores, res:// as the scene writes it, or NIL."
  (let ((value (cdr (assoc "script" (scene-node-props node) :test #'string=))))
    (and value (ext-target value (godot-scene-external scene)))))

(defun node-depth (node)
  "How deep NODE sits: the root is 1."
  (1- (count #\/ (scene-node-path node))))

(defun node-line (scene node)
  "One line for NODE: its path, type, groups, the scene it instances and its
script -- where it sits and what it is, which is what a reader needs."
  (format nil "~a~@[ (~a)~]~@[ groups: ~{~a~^, ~}~]~@[ instance of ~a~]~@[ script ~a~]"
          (scene-node-path node)
          (scene-node-type node)
          (scene-node-groups node)
          (and (scene-node-instance node) (plusp (length (scene-node-instance node)))
               (scene-node-instance node))
          (node-script scene node)))

(defun tree-row (scene node)
  "NODE's line, indented under its parent."
  (concatenate 'string (make-string (* 2 (max 0 (node-depth node))) :initial-element #\Space)
               (node-line scene node)))

;;; --- the six views -------------------------------------------------------------

(defun scene-tree-text (scene offset limit)
  "The scene as a tree, one node a line, in file order."
  (format nil "scene ~a: ~d node~:p, ~d connection~:p, format ~a~@[ uid ~a~]~%~a"
          (godot-scene-path scene)
          (length (godot-scene-nodes scene))
          (length (godot-scene-connections scene))
          (or (godot-scene-format scene) "?")
          (godot-scene-uid scene)
          (rows-text (mapcar (lambda (node) (tree-row scene node)) (godot-scene-nodes scene))
                     offset limit)))

(defun node-matches-p (node name type group)
  "Whether NODE answers all of NAME (part of it), TYPE and GROUP."
  (and (or (null name) (string= name "")
           (search (string-downcase name) (string-downcase (scene-node-name node))))
       (or (null type) (string= type "")
           (and (scene-node-type node) (string-equal type (scene-node-type node))))
       (or (null group) (string= group "")
           (member (string-downcase group)
                   (mapcar #'string-downcase (scene-node-groups node)) :test #'string=))))

(defun scene-find-text (scene name type group offset limit)
  "The nodes answering NAME, TYPE and GROUP, each with where it is written."
  (let ((found (remove-if-not (lambda (node) (node-matches-p node name type group))
                               (godot-scene-nodes scene))))
    (if (null found)
        (format nil "no node of ~a matches name ~s, type ~s, group ~s"
                (godot-scene-path scene) (or name "") (or type "") (or group ""))
        (format nil "~d node~:p of ~a match:~%~a"
                (length found) (godot-scene-path scene)
                (rows-text (mapcar (lambda (node) (format nil "~a  line ~d"
                                                          (node-line scene node) (scene-node-line node)))
                                    found)
                           offset limit)))))

(defun scene-children-text (scene path offset limit)
  "The children of PATH -- the root's when PATH is left out -- each with its
type and how far under it they sit."
  (let* ((node (if (zerop (length (normalize-node-path path)))
                    (scene-root-node scene)
                    (require-node scene path)))
         (relative (string-trim "/" (subseq (scene-node-path node)
                                             (1+ (length (scene-node-name (scene-root-node scene)))))))
         (children (if (eq node (scene-root-node scene))
                       (remove-if-not (lambda (child)
                                        (and (scene-node-parent child)
                                             (string= "." (scene-node-parent child))))
                                      (rest (godot-scene-nodes scene)))
                       (remove-if-not (lambda (child)
                                        (and (scene-node-parent child)
                                             (string= relative (scene-node-parent child))))
                                      (godot-scene-nodes scene)))))
    (if (null children)
        (format nil "~a has no children" (scene-node-path node))
        (format nil "~a, ~d child~:p:~%~a" (scene-node-path node) (length children)
                (rows-text (mapcar (lambda (child) (node-line scene child)) children)
                           offset limit)))))

(defun scene-properties-text (scene path fields)
  "The values PATH stores in the scene file, narrowed to FIELDS when given."
  (let* ((node (require-node scene path))
         (props (scene-node-props node))
         (kept (if fields
                   (remove-if-not (lambda (pair)
                                    (some (lambda (field)
                                            (search (string-downcase field)
                                                    (string-downcase (car pair))))
                                          fields))
                                  props)
                   props)))
    (format nil "~a~@[ (~a)~] stores ~d value~:p in the file, on line ~d:~%~{~a~^~%~}"
            (scene-node-path node)
            (scene-node-type node)
            (length kept)
            (scene-node-line node)
            (if kept
                (mapcar (lambda (pair)
                          (format nil "  ~a = ~a" (car pair) (nlk:clip (cdr pair) 240)))
                        kept)
                (list "  none")))))

(defun scene-groups-text (scene path)
  "PATH's groups, or every group in the scene and the nodes in it."
  (if (plusp (length (normalize-node-path path)))
      (let ((node (require-node scene path)))
        (format nil "~a groups: ~{~a~^, ~}" (scene-node-path node)
                (or (scene-node-groups node) (list "none"))))
      (let ((groups (make-hash-table :test #'equal))
            (names '()))
        (dolist (node (godot-scene-nodes scene))
          (dolist (group (scene-node-groups node))
            (unless (gethash group groups) (push group names))
            (push (scene-node-path node) (gethash group groups))))
        (if (null names)
            (format nil "no node of ~a is in a group" (godot-scene-path scene))
            (format nil "~d group~:p in ~a:~%~{~a~^~%~}" (length names) (godot-scene-path scene)
                    (mapcar (lambda (group)
                              (format nil "  ~a: ~{~a~^, ~}" group (reverse (gethash group groups))))
                            (sort names #'string<)))))))

(defun connection-row (scene connection)
  "One connection as FROM -- SIGNAL --> TO.METHOD."
  (let* ((from (cdr (assoc "from" connection :test #'string=)))
         (to (cdr (assoc "to" connection :test #'string=)))
         (root (scene-node-path (scene-root-node scene)))
         (flags (cdr (assoc "flags" connection :test #'string=))))
    (flet ((absolute (relative)
             (cond ((null relative) "?")
                   ((string= relative ".") root)
                   (t (concatenate 'string root "/" relative)))))
      (format nil "  ~a --~a--> ~a.~a~@[ (flags ~a)~]"
              (absolute from)
              (or (cdr (assoc "signal" connection :test #'string=)) "?")
              (absolute to)
              (or (cdr (assoc "method" connection :test #'string=)) "?")
              flags))))

(defun scene-signals-text (scene path offset limit)
  "Every connection in the scene, or the ones touching PATH."
  (let* ((wanted (normalize-node-path path))
         (root-name (scene-node-name (scene-root-node scene)))
         (absolute (lambda (relative)
                     (cond ((null relative) "")
                           ((string= relative ".") root-name)
                           (t (concatenate 'string root-name "/" relative)))))
         (connections (if (zerop (length wanted))
                          (godot-scene-connections scene)
                          (remove-if-not
                           (lambda (connection)
                             (member wanted
                                     (list (normalize-node-path (funcall absolute (cdr (assoc "from" connection :test #'string=))))
                                           (normalize-node-path (funcall absolute (cdr (assoc "to" connection :test #'string=)))))
                                     :test #'string=))
                           (godot-scene-connections scene)))))
    (if (null connections)
        (format nil "no connection in ~a~@[ touches /~a~]" (godot-scene-path scene)
                (and (plusp (length wanted)) wanted))
        (format nil "~d connection~:p in ~a:~%~a"
                (length connections) (godot-scene-path scene)
                (rows-text (mapcar (lambda (connection) (connection-row scene connection))
                                    connections)
                           offset limit)))))

(defun load-scene (path)
  "The scene PATH names -- the project's main scene when PATH is NIL -- parsed
from its .tscn file, or a refusal."
  (let* ((root (truename (project-root)))
         (named (and path (plusp (length (nlk:trimmed path))) (nlk:trimmed path)))
         (file (project-file (or named (main-scene)) :what "scene")))
    (unless (string= "tscn" (string-downcase (or (pathname-type file) "")))
      (fail "~a is not a .tscn scene file" (res-name file root)))
    (let ((scene (parse-scene (or (nlk:read-text file)
                                  (project-unreadable (res-name file root))))))
      (setf (godot-scene-path scene) (res-name file root))
      scene)))
