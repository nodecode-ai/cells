;;;; cell.lisp --- Construct: a described thing into a checked scene, a model,
;;;; a mesh, a print and a page.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The Hermes plugin rtmandreyev/construct researches a thing, writes one
;;;; exact description of it -- its named parts at their real sizes, how each
;;;; is known -- and builds from that description a hologram, a glTF, CAD and
;;;; a film. This folder keeps the part of it a Nodecode cell owns without
;;;; Python: the scene, its checks, its compilation, and what the geometry
;;;; becomes.
;;;;
;;;;   - a project folder, projects/<slug>/, holding scene.json;
;;;;   - construct_start, construct_edit: begin one, and change one part by
;;;;     its id -- checked before it is written, with the diff;
;;;;   - construct_check, construct_inspect: every problem with its path and
;;;;     its fix, and what the scene's own record says is known;
;;;;   - construct_export, construct_build: glTF, OBJ and STL, and the one
;;;;     self-contained page the hologram turns in.
;;;;
;;;; What is left out is in NOTICE: CAD, film, kits, NetLogo and the shipped
;;;; constructs.

(in-package #:nodecode-construct)

(defparameter *primer*
  "Construct turns one exact description of a thing into geometry you can
look at, print and hand on. A project is a folder with a scene.json in it:
named parts, each of a shape (box, cylinder, cone, sphere), a full size in
metres, a centre, a rotation, and how it is known (verified, user_supplied,
traced, inferred, decorative). Z is up.

  construct_start   slug, starter, scene -- begin a project: the starter
                    (cube, shed) or a whole scene you write as JSON.
  construct_edit    slug, part, move/size/rotate, certainty, role, color,
                    remove -- change one part by its id; nothing is written
                    when the change breaks the scene, and the diff says what
                    moved.
  construct_check   slug -- the scene's format, part ids, sizes and extents,
                    and how many triangles and lines it compiles to.
  construct_inspect slug -- the parts by certainty and size, the facts, and
                    where each fact came from.
  construct_export  slug, format -- glTF, OBJ and STL under export/, each
                    read back and counted.
  construct_build   slug -- build/index.html: the construct drawn in lines of
                    light, turning, with its parts named and coloured by
                    their own colour. Open it in a browser.

Work in this order: start, add the thing's parts with edit, check until no
line comes back, then build and export. A size is the part's full extent,
not half of it; a position is its centre, not its corner."
  "What (help :construct) answers while the cell runs.")

(defparameter *starter-help*
  "the starter a project begins from: cube (a metre cube on the ground) or shed (floor, four walls, roof, door)"
  "One line for the tool's own description.")

;;; --- the contracts --------------------------------------------------------------------

(defparameter +certainty-options+ +certainties+)

(nlk:define-contract construct-start
  (:doc "Begin a construct project: a folder with a scene.json in it, from the starter named or from a whole scene you write as JSON. Answers where it landed and what it checks to.")
  ("slug" :text :required t :doc "the project, 1-64 of a-z, 0-9, _ and -")
  ("starter" :text :default "cube" :options '("cube" "shed") :doc "the starter a project begins from: cube or shed")
  ("scene" :text :default "" :doc "a whole scene as JSON text; empty writes the starter")
  ("overwrite" :boolean :default nil :doc "replace the scene of a project that already has one"))

(nlk:define-contract construct-edit
  (:doc "Change one part of a scene by its id: move it, resize it, rotate it, restyle it, or remove it. The whole scene is checked first, so a change that breaks it is refused with every problem, and what did move comes back as the diff.")
  ("slug" :text :required t :doc "the project")
  ("part" :text :required t :doc "the id of the part to change")
  ("move" :array :of :number :doc "its new centre, [x, y, z] in metres")
  ("size" :array :of :number :doc "its new full extent, [x, y, z] in metres")
  ("rotate" :array :of :number :doc "its new rotation, [x, y, z] in degrees")
  ("certainty" :text :options +certainty-options+ :doc "how it is known: verified, user_supplied, traced, inferred, decorative")
  ("role" :text :options '("mass" "identity" "structure" "detail") :doc "what it does: the bulk, what makes the thing itself, what holds it up, what dresses it")
  ("color" :text :doc "its colour as #rrggbb")
  ("remove" :boolean :slot remove-part :default nil :doc "take the part out of the scene"))

(nlk:define-contract construct-check
  (:doc "Check a project's scene: its format and version, every part's id, shape, size, place and words, the facts and the sources they cite -- each problem with its path and the fix -- then what it compiles to.")
  ("slug" :text :required t :doc "the project"))

(nlk:define-contract construct-inspect
  (:doc "What a project's own record says is known: its parts by certainty, role and size, its overall size, and every fact with the source it came from.")
  ("slug" :text :required t :doc "the project"))

(nlk:define-contract construct-export
  (:doc "Write a checked scene's geometry as glTF (a node and a mesh per part), OBJ (a named object per part) and STL in millimetres, read each file back, and answer what it carries.")
  ("slug" :text :required t :doc "the project")
  ("format" :text :slot export-format :default "all" :options '("all" "gltf" "obj" "stl") :doc "which files to write, or all of them"))

(nlk:define-contract construct-build
  (:doc "Write a checked scene's page: one self-contained HTML file that draws the construct in lines of light, turning, with every part named. No network, nothing fetched: open it in a browser.")
  ("slug" :text :required t :doc "the project"))

;;; --- the verbs --------------------------------------------------------------------------

(defun project-slugs ()
  "Every project folder under the projects root, oldest name first."
  (let ((root (projects-root)))
    (when (uiop:directory-exists-p root)
      (sort (loop for dir in (uiop:subdirectories root)
                  for name = (string-right-trim "/" (car (last (pathname-directory dir))))
                  when (probe-file (scene-file dir)) collect name)
            #'string<))))

(defun start-tool (call)
  (let* ((slug (construct-start-slug call))
         (dir (project-dir slug))
         (path (scene-file dir))
         (text (string-trim '(#\Space #\Tab #\Newline) (construct-start-scene call))))
    (when (and (probe-file path) (not (construct-start-overwrite call)))
      (fail "~a already has a scene: pass overwrite, or edit its parts" (uiop:native-namestring path)))
    (let ((scene (if (plusp (length text))
                     (let ((decoded (ignore-errors (nlk:decode-json text))))
                       (unless (hash-table-p decoded)
                         (fail "scene is not a JSON object: ~a" (subseq text 0 (min 200 (length text)))))
                       decoded)
                     (starter-scene (construct-start-starter call)))))
      (let ((problems (check-scene scene)))
        (when problems
          (fail "~a does not check, so nothing was written:~%~a" slug (problems-text problems)))
        (save-scene dir scene)
        (format nil "started ~a~%~a~%in ~a"
                slug (scene-summary scene) (uiop:native-namestring path))))))

(defun part-number-text (numbers)
  (if numbers (format nil "[~{~,3f~^, ~}]" numbers) "absent"))

(defun part-field-text (part key)
  "KEY's value in PART as text a diff can show."
  (let ((value (members-of part key)))
    (cond ((null value) "absent")
          ((stringp value) value)
          ((vectorp value) (part-number-text (coerce value 'list)))
          (t (format nil "~a" value)))))

(defun part-at (parts id)
  "(VALUES PART INDEX) for the part named ID, or NIL NIL."
  (loop for part across parts
        for index from 0
        when (equal id (text-member part "id"))
          do (return (values part index))))

(defun replaced-part (part call)
  "PART with the fields CALL names set, and the list of what changed."
  (let ((new (nlk:copy-json-object part))
        (changes '()))
    (flet ((set-key (key value label)
             (let ((old (members-of part key)))
               (unless (and (vectorp old) (equal (coerce old 'list) (coerce value 'list)))
                 (setf (gethash key new) value)
                 (push (format nil "~a ~a -> ~a" label
                               (part-field-text part key) (part-number-text (coerce value 'list)))
                       changes)))))
      (let ((move (construct-edit-move call)))
        (when (and move (plusp (length move))) (set-key "position" move "position")))
      (let ((size (construct-edit-size call)))
        (when (and size (plusp (length size))) (set-key "size" size "size")))
      (let ((rotate (construct-edit-rotate call)))
        (when (and rotate (plusp (length rotate))) (set-key "rotation" rotate "rotation"))))
    (dolist (pair (list (cons "certainty" (construct-edit-certainty call))
                        (cons "role" (construct-edit-role call))
                        (cons "color" (construct-edit-color call))))
      (let ((value (cdr pair)))
        (when (and value (plusp (length value)) (not (equal value (text-member part (car pair)))))
          (setf (gethash (car pair) new) value)
          (push (format nil "~a ~a -> ~a" (car pair)
                        (part-field-text part (car pair)) value)
                changes))))
    (values new (nreverse changes))))

(defun remove-part-text (slug dir scene parts part id)
  "SCENE with PART taken out, written when it still checks."
  (let ((new-scene (nlk:copy-json-object scene)))
    (setf (gethash "parts" new-scene)
          (coerce (loop for p across parts unless (eql p part) collect p) 'vector))
    (let ((problems (check-scene new-scene)))
      (when problems (fail "removing ~a breaks the scene:~%~a" id (problems-text problems)))
      (save-scene dir new-scene)
      (format nil "removed ~a from ~a~%~a" id slug (scene-summary new-scene)))))

(defun change-part-text (dir scene parts part index call)
  "SCENE with PART's fields set from CALL, written when it still checks, with
what moved."
  (multiple-value-bind (new-part changes) (replaced-part part call)
    (when (null changes)
      (fail "nothing to change on ~a: pass move, size, rotate, certainty, role, color or remove"
            (construct-edit-part call)))
    (let ((copy (copy-seq parts))
          (new-scene (nlk:copy-json-object scene)))
      (setf (aref copy index) new-part
            (gethash "parts" new-scene) copy)
      (let ((problems (check-scene new-scene)))
        (when problems
          (fail "that change breaks the scene, so nothing was written:~%~a" (problems-text problems)))
        (save-scene dir new-scene)
        (format nil "~a: ~{~a~^; ~}~%~a" (construct-edit-part call) changes
                (scene-summary new-scene))))))

(defun edit-tool (call)
  (let* ((slug (construct-edit-slug call))
         (dir (project-dir slug))
         (scene (load-scene dir))
         (parts (coerce (parts-of scene) 'vector))
         (id (construct-edit-part call)))
    (multiple-value-bind (part index) (part-at parts id)
      (unless part (fail "~a has no part ~s: ~{~a~^, ~}" slug id (map 'list #'part-id parts)))
      (if (construct-edit-remove-part call)
          (remove-part-text slug dir scene parts part id)
          (change-part-text dir scene parts part index call)))))

(defun check-tool (call)
  (let* ((dir (project-dir (construct-check-slug call)))
         (scene (load-scene dir))
         (problems (check-scene scene)))
    (if problems
        (format nil "~a:~%~a~%" (uiop:native-namestring (scene-file dir)) (problems-text problems))
        (format nil "ok: ~a~%~a" (scene-summary scene) (uiop:native-namestring (scene-file dir))))))

(defun inspect-tool (call)
  (inspect-text (load-scene (project-dir (construct-inspect-slug call)))))

(defun export-tool (call)
  (export-scene (project-dir (construct-export-slug call)) (list (construct-export-export-format call))))

(defun build-tool (call)
  (build-page (project-dir (construct-build-slug call))))

;;; --- /construct ----------------------------------------------------------------------------

(defun run-slash (args session-id)
  "The operator's view: every project, or one project's summary and where its
files are."
  (declare (ignore session-id))
  (let* ((slug (string-trim '(#\Space #\Tab) (or args "")))
         (root (projects-root)))
    (if (plusp (length slug))
        (let* ((dir (project-dir slug))
               (scene (load-scene dir)))
          (format nil "~a~%scene: ~a~@[~%page: ~a~]"
                  (scene-summary scene) (uiop:native-namestring (scene-file dir))
                  (let ((page (merge-pathnames "build/index.html" dir)))
                    (and (probe-file page) (uiop:native-namestring page)))))
        (let ((slugs (project-slugs)))
          (if (null slugs)
              (format nil "no projects yet in ~a: construct_start begins one"
                      (uiop:native-namestring root))
              (format nil "~d project~:p in ~a~%~{~a~%~}"
                      (length slugs) (uiop:native-namestring root)
                      (loop for name in slugs
                            for dir = (project-dir name)
                            for scene = (ignore-errors (load-scene dir))
                            collect (if scene
                                        (format nil "  ~a  ~a" name (scene-summary scene))
                                        (format nil "  ~a  (unreadable scene.json)" name)))))))))

;;; --- the cell ---------------------------------------------------------------------------------

(nle:define-cell construct
  (:section ("construct")
    (:guide "nothing here is required; projects_folder is where projects live, empty keeps them under the home; segments is how many sides a curved shape gets")
    ("projects_folder" :string :default ""
                       :doc "where the projects live: a full path; empty is <home>/construct/")
    ("segments" :integer :default 24 :min 8
                :doc "sides on a curved shape (cylinder, cone, sphere)"))
  (:settings (lambda (values table)
               (declare (ignore table))
               (let ((folder (getf values :projects-folder)))
                 (when (and folder (plusp (length (string-trim '(#\Space #\Tab) folder)))
                            (not (uiop:absolute-pathname-p (uiop:parse-unix-namestring folder))))
                   (nlk:config-error
                    "construct.projects_folder is ~s: give a full path, or leave it empty to keep the projects under the home"
                    folder))
                 values)))
  (:tool "construct_start" (nlk:contract 'construct-start) 'start-tool)
  (:tool "construct_edit" (nlk:contract 'construct-edit) 'edit-tool)
  (:tool "construct_check" (nlk:contract 'construct-check) 'check-tool)
  (:tool "construct_inspect" (nlk:contract 'construct-inspect) 'inspect-tool)
  (:tool "construct_export" (nlk:contract 'construct-export) 'export-tool)
  (:tool "construct_build" (nlk:contract 'construct-build) 'build-tool)
  (:command "construct" 'run-slash
            :catches 'construct-error
            :description "Construct projects, and what each one checks to"
            :argument-hint "SLUG")
  (:help :construct
         "construct: a described thing into a checked scene, glTF, OBJ, STL and a turning page - (construct:scene-summary (construct:load-scene (construct:project-dir \"shed\")))"
         *primer*)
  (:start (lambda ()
            (setf *segments* (setting :segments))
            (ensure-directories-exist (projects-root))
            nil)))
