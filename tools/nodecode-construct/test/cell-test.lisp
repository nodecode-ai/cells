;;;; cell-test.lisp --- the starters, the checks, the edits, the three
;;;; exports, the page and the command.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Everything here is offline: every scene is written by the test into a
;;;; scratch projects folder, and every proof is read back off the file the
;;;; cell left on disk -- the geometry's counts, the edited scene, the glTF's
;;;; own declared byteLength against its .bin, the STL's header against its
;;;; length, and the page's text. No network, no key, no program run.

(in-package #:nodecode.test)

;;; --- the scratch projects folder ------------------------------------------------------

(defun construct-projects-dir ()
  "A projects folder of this run's own, emptied."
  (let ((dir (merge-pathnames "construct-projects/" (uiop:temporary-directory))))
    (when (probe-file dir)
      (ignore-errors (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))
    (ensure-directories-exist dir)
    (uiop:native-namestring dir)))

(define-test-slice "construct" "CONSTRUCT-CELL-" :start nodecode-construct:start-cell
  :defaults (("projects_folder" (construct-projects-dir))))

;;; --- calling a tool as the model does -----------------------------------------------

(defun construct-call (name &rest pairs)
  "NAME's registered tool, called with a payload of PAIRS through its own
contract: what the model's call goes through."
  (let* ((entry (nle::find-tool name))
         (schema (second entry))
         (call (nlk:contract-decode schema (apply #'nlk:make-json-object pairs))))
    (funcall (third entry) call)))

(defun construct-condition (form)
  "What FORM signals, as text, or NIL when it answers."
  (handler-case (progn (funcall form) nil)
    (error (condition) (princ-to-string condition))))

(defmacro construct-carrying (form &body checks)
  "IS-CARRYING over FORM's answer, which the checker binds as ANSWER."
  `(is-carrying (answer (princ-to-string ,form)) ,@checks))

(defun construct-part-object (id shape size position &key (certainty "inferred") (role "structure"))
  (nlk:json-object "id" id "shape" shape "size" (coerce size 'vector)
                    "position" (coerce position 'vector)
                    "certainty" certainty "role" role))

(defun construct-scene-object (parts &key name facts sources)
  (nlk:json-object "format" "construct-scene" "version" 2
                    "name" (or name "test scene") "category" "object"
                    "parts" (coerce parts 'vector)
                    "facts" (coerce (or facts '()) 'vector)
                    "sources" (coerce (or sources '()) 'vector)))

(defun construct-write-scene (slug scene)
  "SCENE as SLUG's scene.json, written by the test."
  (let ((dir (nodecode-construct:project-dir slug :create t)))
    (nodecode-construct:save-scene dir scene)
    dir))

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "construct"
  (:help :construct)
  (:command "construct")
  (:running (is (= 24 (nodecode-construct::setting :segments)) "a curved shape gets 24 sides")
            (is (search "construct_edit" (cddr (assoc :construct nle::*help-topics*))) "the manual says how to work"))
  (:refused ("segments" 4) ("projects_folder" "relative/dir"))
  (:idle nodecode-construct:construct-error (nodecode-construct:project-dir "nothing")))

;;; --- a project from a starter ----------------------------------------------------------

(deftest construct-cell-a-starter-checks-and-says-what-it-compiled-to ()
  (with-cell-stop ((construct-start))
    (construct-carrying (construct-call "construct_start" "slug" "cube-a" "starter" "cube")
      ("started cube-a") ("1 parts, 12 triangles, 12 lines"))
    (is (probe-file (nodecode-construct:scene-file (nodecode-construct:project-dir "cube-a"))) "the scene is on disk")
    (construct-carrying (construct-call "construct_check" "slug" "cube-a")
      ("ok:") ("1 parts, 12 triangles"))
    (construct-carrying (construct-condition (lambda () (construct-call "construct_start" "slug" "cube-a")))
      ("already has a scene" "pass overwrite"))
    (let ((answer (construct-call "construct_start" "slug" "cube-a" "overwrite" t)))
      (is (search "started cube-a" answer) answer))))

(deftest construct-cell-every-shape-in-the-vocabulary-compiles-closed-geometry ()
  (with-cell-stop ((construct-start))
    (dolist (shape (nodecode-construct:shape-names))
      (let* ((scene (construct-scene-object
                      (list (construct-part-object (format nil "one-~a" shape) shape
                                                   (list 1d0 1d0 1d0) (list 0d0 0d0 0d0)))
                      :name (format nil "a ~a" shape)))
             (slug (format nil "shape-~a" shape)))
        (construct-write-scene slug scene)
        (let ((check (construct-call "construct_check" "slug" slug)))
          (is-carrying check ("ok:") (:absent "parts["))
          (multiple-value-bind (triangles lines)
              (nodecode-construct::geometry-counts (nodecode-construct:compile-scene scene))
            (is (plusp triangles) (format nil "~a is a solid" shape))
            (is (plusp lines) (format nil "~a is drawn in lines" shape))
            (is (search (format nil "~d triangles, ~d lines" triangles lines) check)
                (format nil "the check counts what was compiled for ~a" shape))))))
    (construct-carrying (construct-call "construct_check" "slug" "shape-box") ("12 triangles, 12 lines"))
    (construct-carrying (construct-call "construct_check" "slug" "shape-cylinder") ("96 triangles"))
    (construct-carrying (construct-call "construct_check" "slug" "shape-cone") ("48 triangles"))))

;;; --- the edits ---------------------------------------------------------------------------

(deftest construct-cell-a-change-is-checked-before-it-is-written ()
  (with-cell-stop ((construct-start))
    (construct-call "construct_start" "slug" "moved")
    (construct-carrying (construct-call "construct_edit" "slug" "moved" "part" "cube"
                                 "move" (vector 1d0 2d0 0.5d0))
      ("cube: position [0.000, 0.000, 0.500] -> [1.000, 2.000, 0.500]"))
    (let* ((dir (nodecode-construct:project-dir "moved"))
           (part (first (nodecode-construct::parts-of (nodecode-construct:load-scene dir)))))
      (is (equal '(1.0d0 2.0d0 0.5d0) (coerce (nodecode-construct::part-position part) 'list)) "the scene on disk carries the move"))
    (construct-carrying (construct-condition
                  (lambda () (construct-call "construct_edit" "slug" "moved" "part" "cube"
                                             "size" (vector 0d0 0d0 0d0))))
      ("that change breaks the scene, so nothing was written") ("parts[0].size"))
    (let ((part (first (nodecode-construct::parts-of
                        (nodecode-construct:load-scene (nodecode-construct:project-dir "moved"))))))
      (is (equal '(1.0d0 1.0d0 1.0d0) (coerce (nodecode-construct::part-size part) 'list)) "and the size it had stands"))
    (let ((answer (construct-call "construct_edit" "slug" "moved" "part" "cube" "certainty" "verified")))
      (is (search "certainty inferred -> verified" answer) answer))
    (construct-carrying (construct-condition
                  (lambda () (construct-call "construct_edit" "slug" "moved" "part" "roof"
                                             "move" (vector 0d0 0d0 3d0))))
      ("has no part" "cube"))
    (construct-call "construct_start" "slug" "shed-b" "starter" "shed")
    (construct-carrying (construct-call "construct_edit" "slug" "shed-b" "part" "door" "remove" t)
      ("removed door from shed-b") ("6 parts"))))

;;; --- what the record says is known ---------------------------------------------------------

(deftest construct-cell-the-record-says-what-is-known ()
  (with-cell-stop ((construct-start))
    (construct-carrying (construct-call "construct_start" "slug" "shed-a" "starter" "shed")
      ("7 parts" "the shed is seven parts"))
    (construct-carrying (construct-call "construct_inspect" "slug" "shed-a")
      ("a shed (architecture)") ("inferred:") ("door  identity") ("facts:")
      ("none: every part rests on the request alone"))
    (construct-write-scene
     "known"
     (construct-scene-object
      (list (construct-part-object "cube" "box" (list 1d0 1d0 1d0) (list 0d0 0d0 0.5d0)
                                   :certainty "verified" :role "mass"))
      :name "a known cube"
      :sources (list (nlk:json-object "id" "listing" "kind" "document"
                                       "title" "the shop listing"))
      :facts (list (nlk:json-object "text" "it is a metre wide"
                                     "certainty" "verified" "source" "listing"))))
    (construct-carrying (construct-call "construct_inspect" "slug" "known")
      ("verified:") ("cube  mass") ("it is a metre wide (the shop listing)")
      ("listing  document  the shop listing"))
    (construct-carrying (construct-call "construct_check" "slug" "known") ("ok:") ("a known cube"))))

;;; --- the exports ----------------------------------------------------------------------------

(deftest construct-cell-every-export-is-read-back-and-counted ()
  (with-cell-stop ((construct-start))
    (construct-call "construct_start" "slug" "exported")
    (let* ((answer (construct-call "construct_export" "slug" "exported" "format" "all"))
           (out (merge-pathnames "export/" (nodecode-construct:project-dir "exported"))))
      (is-carrying answer ("1 parts, 12 triangles")
                   ("gltf: 1 nodes, 1 meshes, 1 accessors")
                   ("obj: 1 objects, 36 vertices, 12 faces")
                   ("stl: 12 triangles") ("millimetres"))
      (is (probe-file (merge-pathnames "construct.gltf" out)) "the glTF is there")
      (is (probe-file (merge-pathnames "construct.bin" out)) "and its geometry")
      (is (probe-file (merge-pathnames "construct.obj" out)) "the OBJ is there")
      (is (probe-file (merge-pathnames "construct.stl" out)) "and the STL")
      (is (= (* 12 50) (- (nodecode-construct::file-size (merge-pathnames "construct.stl" out)) 84)) "the STL is its header and twelve triangles")
      (construct-carrying (nlk:read-text (merge-pathnames "construct.gltf" out))
        ("\"POSITION\"") ("\"extras\"") ("\"certainty\""))
      (construct-carrying (construct-condition
                    (lambda () (construct-call "construct_export" "slug" "exported" "format" "box")))
        ("format must be one of" "a format outside the set refuses")))))

;;; --- the page ---------------------------------------------------------------------------------

(defun construct-compact (text) (remove #\Space text))

(deftest construct-cell-the-page-is-one-file-with-the-construct-in-it ()
  (with-cell-stop ((construct-start))
    (construct-call "construct_start" "slug" "paged")
    (let* ((answer (construct-call "construct_build" "slug" "paged"))
           (page (merge-pathnames "build/index.html" (nodecode-construct:project-dir "paged"))))
      (is-carrying answer ("page:") ("1 parts, 12 triangles") ("index.html"))
      (is (probe-file page) "the page is on disk")
      (construct-carrying (nlk:read-text page)
        ("<!doctype html>") ("requestAnimationFrame") ("\"triangles\":[[[")
        ("id=\"legend\"") (:absent "SCENE_JSON") (:absent "http"))
      (is (search "\"id\":\"cube\"" (construct-compact (nlk:read-text page))) "the part is named in the page"))))

;;; --- a broken scene, and the command ------------------------------------------------------------

(deftest construct-cell-a-broken-scene-names-its-path ()
  (with-cell-stop ((construct-start))
    (construct-write-scene
     "broken"
     (construct-scene-object
      (list (construct-part-object "wall" "box" (list 4d0 0.2d0 2.5d0) (list 0d0 0d0 1.25d0))
            (construct-part-object "wall" "wedge" (list 0d0 1d0 1d0) (list 0d0 0d0 0d0)))
      :name "broken"))
    (construct-carrying (construct-call "construct_check" "slug" "broken")
      ("parts[1].id: \"wall\" names two parts")
      ("parts[1].shape: shape \"wedge\" is not in the vocabulary")
      ("box, cylinder, cone, sphere"))))

(deftest construct-cell-the-command-lists-the-projects ()
  (with-cell-stop ((construct-start))
    (construct-call "construct_start" "slug" "one")
    (construct-call "construct_start" "slug" "two" "starter" "shed")
    (construct-carrying (nodecode-construct::run-slash "" "s1")
      ("2 projects") ("  one  a metre cube: 1 parts, 12 triangles") ("  two  a shed: 7 parts"))
    (construct-carrying (nodecode-construct::run-slash "two" "s1") ("a shed: 7 parts") ("scene:"))
    (construct-carrying (construct-condition
                  (lambda () (nodecode-construct::run-slash "nowhere" "s1")))
      ("no scene at"))))
