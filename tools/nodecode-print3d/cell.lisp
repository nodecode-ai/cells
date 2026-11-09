;;;; cell.lisp --- a print job: the model, the evidence behind its sizes, the
;;;; checks, and the slice.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The Hermes plugin mfranzon/print3d turns an idea, a photo or a shop
;;;; listing into a sliced Bambu Studio project for a Bambu Lab X2D. This
;;;; folder keeps the part of that a Nodecode cell can own without Python:
;;;;
;;;;   - a job folder, jobs/<slug>/, holding model.py (the bpy script the
;;;;     model writes), spec.sexp (every dimension and where it came from:
;;;;     the user said so, the listing said so, or it was assumed with a
;;;;     reason) and part.stl;
;;;;   - print3d_make: run the job's model.py through the user's own Blender
;;;;     in the background, then check the mesh it wrote, then slice it with
;;;;     the user's own Bambu Studio;
;;;;   - print3d_check: the plugin's cheap printability checks on an STL --
;;;;     size against the bed, volume, manifold, whether it sits on the bed,
;;;;     each note carrying the fix to apply;
;;;;   - print3d_spec: record and read back a job's dimensions and their
;;;;     evidence.
;;;;
;;;; What is left out is in NOTICE: the photo measuring, the likeness
;;;; overlay, the HTML viewer and the handoff sheet.

(defpackage #:nodecode-print3d
  (:use #:cl)
  (:export #:start-cell
           #:job-dir #:read-stl #:check-mesh #:check-mesh-text #:volume-mm3
           #:manifold-p #:record-dim #:spec-text #:load-spec #:save-spec #:make-job
           #:run-command #:print3d-error))

(in-package #:nodecode-print3d)

(nlk:define-peripheral print3d :not-running t)

;;; --- the words --------------------------------------------------------------------

(defparameter *primer*
  "print3d turns an idea into a checked, sliced Bambu Studio project for a Bambu
Lab X2D. Work in a job: record the sizes you have, model in Blender, then let
the cell slice it.

  print3d_spec   slug, name, mm, source, why -- record one size and where it
                 came from (user, listing, or assumed with a reason), or with
                 no name read the job's whole spec back.
  print3d_make   slug -- write jobs/<slug>/model.py, then run it: Blender
                 exports part.stl, the mesh is checked, and Bambu Studio
                 slices it. With no model.py yet the cell writes a starter and
                 names its path; edit that, call print3d_make again.
  print3d_check  mesh -- the same checks on an STL that already exists.

Every check names its fix, so iterate on model.py until no line starts with
ERROR, then hand the user the .3mf path. Nothing here talks to a printer."
  "What (help :print3d) answers while the cell runs.")

(defparameter *starter*
  "import bpy, sys
out = sys.argv[sys.argv.index(\"--\") + 1]
bpy.ops.wm.read_factory_settings(use_empty=True)
bpy.ops.mesh.primitive_cube_add(size=20, location=(0, 0, 10))
bpy.ops.wm.stl_export(filepath=out, export_selected_objects=True)
"
  "The model.py a job starts from: a 20 mm calibration cube, exported to the
path the cell passes after `--'. The model rewrites it.")

;;; --- the seam ---------------------------------------------------------------------

(defun program-path (name)
  "Where NAME is: as a path when it names one, else the first file of that
name on PATH; NIL when there is none."
  (when (and (stringp name) (plusp (length name)))
    (flet ((file (dir)
             (ignore-errors
               (probe-file (merge-pathnames name
                                             (uiop:ensure-directory-pathname
                                              (if (plusp (length dir)) dir ".")))))))
      (cond ((or (find #\/ name) (find #\\ name)) (ignore-errors (probe-file name)))
            (t (loop for dir in (uiop:split-string (or (uiop:getenv "PATH") "")
                                                   :separator '(#\: #\;))
                     for found = (file dir)
                     when found return found))))))

(defun run-command (args directory)
  "ARGS run in DIRECTORY => (VALUES OUTPUT EXIT-CODE). A program that is not
there answers 127 rather than signalling: its text is the advice."
  (handler-case
      (uiop:run-program args :output :string :error-output :output
                              :ignore-error-status t :directory directory)
    (error (condition) (values (format nil "~a" condition) 127))))

(defvar *run* #'run-command
  "The one seam that runs Blender and Bambu Studio: (ARGS DIRECTORY) =>
(VALUES OUTPUT EXIT-CODE). Tests replace it; nothing else does.")

(defun run (args directory)
  (funcall *run* args directory))

(defun tail-lines (text count)
  "The last COUNT non-empty lines of TEXT."
  (let ((lines (remove-if (lambda (line)
                            (zerop (length (string-trim '(#\Space #\Tab #\Return) line))))
                          (uiop:split-string (or text "") :separator '(#\Newline)))))
    (format nil "~{~a~^~%~}" (last lines (min count (length lines))))))

;;; --- the job folder ----------------------------------------------------------------

(defun safe-slug-p (slug)
  (and (stringp slug) (<= 1 (length slug) 64)
       (every (lambda (c) (find c "abcdefghijklmnopqrstuvwxyz0123456789_-")) slug)))

(defun jobs-root ()
  "Where the jobs live: the `jobs_dir' setting, else ~/print3d/jobs."
  (let ((dir (or (setting :jobs-dir) "")))
    (uiop:ensure-directory-pathname
     (if (plusp (length (string-trim '(#\Space #\Tab) dir)))
         dir
         (merge-pathnames "print3d/jobs/" (user-homedir-pathname))))))

(defun job-dir (slug &key (create t))
  "jobs/SLUG, refused unless SLUG is a plain slug, so it cannot leave the
jobs folder."
  (unless (safe-slug-p slug)
    (fail "slug ~s: use 1 to 64 of a-z, 0-9, _ and -" slug))
  (let ((dir (merge-pathnames (format nil "~a/" slug) (jobs-root))))
    (when create (ensure-directories-exist (merge-pathnames "spec.sexp" dir)))
    dir))

(defun spec-file (dir) (merge-pathnames "spec.sexp" dir))

(defun load-spec (dir)
  "The job's spec: a plist of :DIMS ((NAME :MM .. :SOURCE .. :WHY ..) ...),
:UNKNOWNS and :LISTING."
  (let ((file (spec-file dir)))
    (if (probe-file file)
        (handler-case
            (with-open-file (in file :direction :input)
              (let ((*read-eval* nil))
                (with-standard-io-syntax (read in))))
          (error () (list :dims '() :unknowns '() :listing '())))
        (list :dims '() :unknowns '() :listing '()))))

(defun save-spec (dir spec)
  (with-open-file (out (spec-file dir) :direction :output :if-exists :supersede
                                          :if-does-not-exist :create)
    (with-standard-io-syntax (prin1 spec out))
    (terpri out))
  (spec-file dir))

(defun record-dim (spec name mm source why)
  "Record one dimension and where it came from. => the entry."
  (unless (and (numberp mm) (plusp mm))
    (fail "~a: give its size in millimetres, a positive number" name))
  (unless (member source '("user" "assumed" "listing") :test #'equal)
    (fail "source ~s: say where the number came from -- user, listing, or assumed" source))
  (when (and (equal source "assumed")
             (or (null why) (zerop (length (string-trim '(#\Space #\Tab) why)))))
    (fail "~a: an assumed size needs a why -- the reason you chose it" name))
  (let ((entry (list :mm (coerce mm 'single-float) :source source
                     :why (and why (plusp (length why)) why))))
    (setf (getf spec :dims)
          (cons (cons name entry) (remove name (getf spec :dims) :key #'car :test #'equal)))
    entry))

(defun spec-text (spec)
  "The spec as the model reads it back: each size, its source and its evidence."
  (with-output-to-string (out)
    (let ((dims (getf spec :dims)))
      (if (null dims)
          (format out "no dimensions recorded yet~%")
          (progn
            (format out "~va  ~va  ~a~%" 14 "dimension" 10 "mm" "source")
            (dolist (entry (sort (copy-list dims) #'string< :key #'car))
              (format out "~va  ~va  ~a~@[ -- ~a~]~%"
                      14 (car entry)
                      10 (format nil "~,2f" (getf (cdr entry) :mm))
                      (getf (cdr entry) :source)
                      (getf (cdr entry) :why))))))
    (when (getf spec :unknowns)
      (format out "unknowns:~%~{  ~a~%~}" (getf spec :unknowns)))
    (when (getf spec :listing)
      (format out "listing: ~{~a~^, ~}~%" (getf spec :listing)))))

;;; --- the mesh ---------------------------------------------------------------------

(defun u32 (bytes i)
  (+ (aref bytes i) (ash (aref bytes (+ i 1)) 8)
     (ash (aref bytes (+ i 2)) 16) (ash (aref bytes (+ i 3)) 24)))

(defun single-float-bits (bits)
  "The single float a little-endian IEEE 754 BIT pattern names."
  (let* ((sign (if (logbitp 31 bits) -1.0 1.0))
         (exponent (ldb (byte 8 23) bits))
         (fraction (ldb (byte 23 0) bits))
         (value (cond ((zerop exponent) (* fraction (expt 2.0 -149)))
                      ((= exponent 255) 0.0)
                      (t (* (+ 1.0 (/ fraction (expt 2.0 23)))
                            (expt 2.0 (- exponent 127)))))))
    (coerce (* sign value) 'single-float)))

(defun read-file-bytes (path)
  "PATH as a simple (UNSIGNED-BYTE 8) vector."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence bytes in)
      bytes)))

(defun read-stl (path)

  "PATH as a list of triangles, each a list of three (X Y Z) vertices.
Binary and ASCII STL both, as the plugin's dfm.py reads them."
  (let* ((file (uiop:ensure-pathname path :want-existing t))
         (bytes (read-file-bytes file)))
    (when (< (length bytes) 15) (fail "~a: too small to be an STL" path))
    (let ((head (map 'string #'code-char (subseq bytes 0 (min 300 (length bytes))))))
      (if (and (>= (length head) 5) (string-equal "solid" head :end2 5)
               (search "facet" (string-downcase head)))
          (read-ascii-stl bytes)
          (read-binary-stl bytes path)))))

(defun read-ascii-stl (bytes)
  (let ((triangles '()) (verts '()))
    (with-input-from-string (in (map 'string #'code-char bytes))
      (loop for line = (read-line in nil nil) while line
            for trimmed = (string-trim '(#\Space #\Tab #\Return) line)
            when (>= (length trimmed) 6)
              do (let ((words (uiop:split-string trimmed :separator '(#\Space #\Tab))))
                   (when (and (string-equal (first words) "vertex") (>= (length words) 4))
                     (handler-case
                         (push (list (read-from-string (second words))
                                     (read-from-string (third words))
                                     (read-from-string (fourth words)))
                               verts)
                       (error () nil))
                     (when (= 3 (length verts))
                       (push (reverse verts) triangles)
                       (setf verts '()))))))
    (nreverse triangles)))

(defun read-binary-stl (bytes path)
  (let ((count (u32 bytes 80)))
    (unless (>= (length bytes) (+ 84 (* count 50)))
      (fail "~a: a binary STL of ~d triangles is truncated" path count))
    (loop for index below count
          for base = (+ 84 (* index 50) 12)
          collect (list (list (single-float-bits (u32 bytes base))
                              (single-float-bits (u32 bytes (+ base 4)))
                              (single-float-bits (u32 bytes (+ base 8))))
                        (list (single-float-bits (u32 bytes (+ base 12)))
                              (single-float-bits (u32 bytes (+ base 16)))
                              (single-float-bits (u32 bytes (+ base 20))))
                        (list (single-float-bits (u32 bytes (+ base 24)))
                              (single-float-bits (u32 bytes (+ base 28)))
                              (single-float-bits (u32 bytes (+ base 32))))))))

(defun volume-mm3 (triangles)
  (abs (/ (loop for ((ax ay az) (bx by bz) (cx cy cz)) in triangles
                sum (+ (* ax (- (* by cz) (* bz cy)))
                       (* ay (- (* bz cx) (* bx cz)))
                       (* az (- (* bx cy) (* by cx)))))
          6.0)))

(defun vertex< (a b)
  (loop for x in a for y in b
        when (< x y) return t
        when (> x y) return nil
        finally (return nil)))

(defun manifold-p (triangles)
  "Every edge shared by exactly two triangles, and there are edges."
  (let ((edges (make-hash-table :test 'equal)))
    (dolist (tri triangles)
      (destructuring-bind (a b c) tri
        (dolist (pair (list (list a b) (list b c) (list c a)))
          (incf (gethash (if (vertex< (first pair) (second pair))
                             pair (reverse pair))
                         edges 0)))))
    (and (plusp (hash-table-count edges))
         (loop for n being the hash-values of edges always (= n 2)))))

(defun bed-mm ()
  "The bed the checks measure against: the `bed' setting, \"X,Y,Z\" in mm."
  (loop for part in (uiop:split-string (or (setting :bed) "256,256,260") :separator '(#\,))
        for value = (ignore-errors
                      (multiple-value-bind (number end)
                          (read-from-string (string-trim '(#\Space) part) nil nil)
                        (and number (= end (length (string-trim '(#\Space) part)))
                             (numberp number) (coerce number 'single-float))))
        collect (or value 256.0)))

(defun check-mesh (path &optional (bed (bed-mm)))
  "The plugin's cheap printability checks: => a plist of :TRIANGLES :SIZE
:VOLUME :MANIFOLD :ON-BED :ERRORS :WARNINGS."
  (let* ((triangles (read-stl path))
         (vertices (apply #'append triangles)))
    (when (null triangles) (fail "~a: the mesh has no triangles" path))
    (let* ((xs (mapcar #'first vertices))
           (ys (mapcar #'second vertices))
           (zs (mapcar #'third vertices))
           (low (list (reduce #'min xs) (reduce #'min ys) (reduce #'min zs)))
           (high (list (reduce #'max xs) (reduce #'max ys) (reduce #'max zs)))
           (size (mapcar #'- high low))
           (volume (volume-mm3 triangles))
           (manifold (manifold-p triangles))
           (on-bed (<= (abs (third low)) 0.2))
           (errors '()) (warnings '()))
      (when (< volume 1.0)
        (push (format nil "volume ~,3f mm^3 is too small" volume) errors))
      (loop for index below 3
            when (> (nth index size) (+ (nth index bed) 0.01))
              do (push (format nil "size ~{~,2f~^ x ~} mm exceeds the ~{~d~^ x ~} mm bed"
                               size (mapcar (lambda (value) (round value)) bed))
                       errors))
      (when (> (length triangles) 2000000)
        (push (format nil "~d triangles exceeds 2000000" (length triangles)) errors))
      (unless manifold
        (push "the mesh is not manifold: an edge is not shared by exactly two triangles" errors))
      (unless on-bed
        (push (format nil "the lowest Z is ~,3f mm off the bed; Bambu Studio --ensure-on-bed lifts it"
                       (third low))
              warnings))
      (list :triangles (length triangles) :size size :volume volume
            :manifold manifold :on-bed on-bed
            :errors (remove-duplicates (nreverse errors) :test #'equal)
            :warnings (nreverse warnings)))))

(defun check-mesh-text (report)
  "REPORT as the model reads it: the numbers, then each note with its fix."
  (with-output-to-string (out)
    (format out "triangles: ~d~%" (getf report :triangles))
    (format out "size: ~{~,2f~^ x ~} mm~%" (getf report :size))
    (format out "volume: ~,2f mm^3~%" (getf report :volume))
    (format out "manifold: ~:[no~;yes~]~%" (getf report :manifold))
    (format out "on the bed: ~:[no~;yes~]~%" (getf report :on-bed))
    (dolist (note (getf report :errors)) (format out "ERROR ~a~%" note))
    (dolist (note (getf report :warnings)) (format out "WARN ~a~%" note))
    (format out "~:[not printable yet: fix the ERROR lines in model.py and run print3d_make again~;ok~]~%"
            (null (getf report :errors)))))

;;; --- the run ----------------------------------------------------------------------

(defun write-starter (dir)
  "Put the starter model.py in DIR unless one is there already. => its path."
  (let ((file (merge-pathnames "model.py" dir)))
    (unless (probe-file file)
      (with-open-file (out file :direction :output :if-exists :supersede
                                  :if-does-not-exist :create)
        (write-string *starter* out)))
    file))

(defun model-blender (dir script out)
  "Run SCRIPT through Blender, exporting OUT. => OUT's pathname."
  (let* ((blender (or (program-path (setting :blender)) (setting :blender)))
         (args (list (uiop:native-namestring blender)
                     "--background" "--factory-startup" "--python-exit-code" "5"
                     "--python" (uiop:native-namestring script)
                     "--" (uiop:native-namestring out))))
    (multiple-value-bind (output code) (run args dir)
      (unless (and (integerp code) (zerop code))
        (fail "Blender exited ~a on ~a:~%~a" code (file-namestring script)
              (tail-lines output 12)))
      (unless (probe-file out)
        (fail "Blender ran ~a but wrote no ~a: the script has to export the mesh to the path after --"
              (file-namestring script) (file-namestring out))))
    out))

(defun file-kilobytes (path)
  (or (ignore-errors
        (with-open-file (in path :element-type '(unsigned-byte 8))
          (/ (file-length in) 1024.0)))
      0.0))

(defun slice-mesh (dir mesh)
  "Slice MESH with the user's Bambu Studio. => the .3mf pathname and NIL, or
NIL and a line saying why not."
  (let ((studio (program-path (or (setting :studio) ""))))
    (unless studio
      (return-from slice-mesh
        (values nil (format nil "Bambu Studio is not on PATH (~a); set print3d.studio to it, or slice the STL yourself"
                            (setting :studio)))))
    (let* ((out (merge-pathnames "part.gcode.3mf" dir))
           (args (list (uiop:native-namestring studio)
                       "--slice" "0" "--ensure-on-bed"
                       "--outputdir" (uiop:native-namestring dir)
                       "--export-3mf" (file-namestring out)
                       (uiop:native-namestring mesh))))
      (multiple-value-bind (output code) (run args dir)
        (if (and (integerp code) (zerop code) (probe-file out))
            (values out nil)
            (values nil (format nil "Bambu Studio exited ~a:~%~a" code (tail-lines output 12))))))))

(defun make-job (slug script-name dry-run slice)
  "Model, check and slice SLUG. => the report text."
  (let* ((dir (job-dir slug))
         (script (if (and script-name (plusp (length script-name)))
                     (merge-pathnames script-name dir)
                     (write-starter dir)))
         (out (merge-pathnames "part.stl" dir))
         (started (get-universal-time))
         (report '()))
    (unless (probe-file script)
      (fail "~a: no such script in job ~a" script-name slug))
    (when dry-run
      (return-from make-job
        (format nil "job ~a~%script: ~a~%the script has not been run (dry run)"
                slug (uiop:native-namestring script))))
    (when (probe-file out) (ignore-errors (delete-file out)))
    (model-blender dir script out)
    (let ((checked (check-mesh out)))
      (push (format nil "job ~a~%script: ~a~%mesh: ~a~%~a"
                    slug (file-namestring script) (file-namestring out)
                    (check-mesh-text checked))
            report)
      (when (and slice (null (getf checked :errors)))
        (multiple-value-bind (project why-not) (slice-mesh dir out)
          (push (if project
                    (format nil "sliced: ~a (~,1f KB) in ~ds"
                            (uiop:native-namestring project) (file-kilobytes project)
                            (- (get-universal-time) started))
                    (format nil "not sliced: ~a" why-not))
                report))))
    (format nil "~{~a~^~%~}" (nreverse report))))

;;; --- the boundaries ----------------------------------------------------------------

(nlk:define-contract print3d-spec
  (:doc "Record one dimension of the part being modelled and where it came from, or read the whole job spec back when no name is given. The evidence is what makes a wrong number traceable: a size the user gave, one a shop listing states, or one assumed with the reason.")
  ("slug" :text :required t :doc "the job, 1-64 of a-z, 0-9, _ and -")
  ("name" :text :default "" :doc "the dimension, e.g. width; empty reads the spec back")
  ("mm" :number :doc "its size in millimetres")
  ("source" :text :default "user" :options '("user" "listing" "assumed")
            :doc "who says so: the user, the listing, or you")
  ("why" :text :default "" :doc "the listing's own words, or why an assumed size is what you chose"))

(nlk:define-contract print3d-check
  (:doc "Printability of an STL: triangle count, size against the bed, volume, whether it is manifold and whether it sits on the bed, each error with the fix to apply.")
  ("mesh" :text :required t :doc "path to an STL, absolute or relative to the session's folder"))

(nlk:define-contract print3d-make
  (:doc "Model, check and slice a print job: run its model.py through Blender, check the mesh it exported, then slice it with Bambu Studio. With no model.py yet it writes a starter and names the path.")
  ("slug" :text :required t :doc "the job, 1-64 of a-z, 0-9, _ and -")
  ("script" :text :default "" :doc "the bpy script in the job folder; empty uses model.py")
  ("dry_run" :boolean :default nil :doc "write nothing and run nothing, only name the script")
  ("slice" :boolean :default t :doc "slice the mesh once it passes the checks"))

(defun spec-tool (call)
  (let* ((slug (print3d-spec-slug call))
         (name (print3d-spec-name call))
         (dir (job-dir slug))
         (spec (load-spec dir)))
    (cond ((zerop (length name))
           (format nil "job ~a (~a)~%~a" slug (uiop:native-namestring (spec-file dir))
                   (spec-text spec)))
          (t (let ((mm (print3d-spec-mm call)))
               (unless (numberp mm)
                 (fail "~a: give its size in millimetres" name))
               (record-dim spec name mm (print3d-spec-source call) (print3d-spec-why call))
               (save-spec dir spec)
               (format nil "recorded ~a in job ~a~%~a" name slug (spec-text spec)))))))

(defun check-tool (call)
  (check-mesh-text (check-mesh (print3d-check-mesh call))))

(defun make-tool (call)
  (make-job (print3d-make-slug call) (print3d-make-script call)
            (print3d-make-dry-run call) (print3d-make-slice call)))

;;; --- /print3d ----------------------------------------------------------------------

(defun job-line (dir)
  (format nil "  ~a~:[  no mesh~;  mesh~]~:[  unsliced~;  sliced~]"
          (car (last (pathname-directory dir)))
          (probe-file (merge-pathnames "part.stl" dir))
          (probe-file (merge-pathnames "part.gcode.3mf" dir))))

(defun run-slash (args session-id)
  "/print3d [SLUG]: the jobs in the jobs folder and how far each got, or one
job's spec."
  (declare (ignore session-id))
  (let ((slug (nlk:trimmed (or args ""))))
    (if (plusp (length slug))
        (let ((dir (job-dir slug :create nil)))
          (if (probe-file (spec-file dir))
              (format nil "job ~a~%~a" slug (spec-text (load-spec dir)))
              (format nil "no job ~a yet: print3d_make starts one, starter model.py and all" slug)))
        (let* ((root (jobs-root))
               (dirs (and (probe-file root)
                          (remove-if-not #'uiop:directory-pathname-p
                                         (uiop:directory-files root)))))
          (if (null dirs)
              (format nil "no print jobs yet in ~a; print3d_make SLUG starts one"
                      (uiop:native-namestring root))
              (format nil "print jobs in ~a:~%~{~a~%~}" (uiop:native-namestring root)
                      (mapcar #'job-line (sort dirs #'string< :key #'namestring))))))))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell print3d
  (:section ("print3d")
    (:guide "Blender 4.2 or later is the one requirement, and the cell refuses to start without it; studio is Bambu Studio's command, used for the slice; jobs_dir is where the jobs live")
    ("blender" :string :default "blender"
               :doc "Blender 4.2 or later: its name on PATH, or a path to it")
    ("studio" :string :default "bambu-studio"
              :doc "Bambu Studio's CLI: its name on PATH, or a path to it")
    ("jobs_dir" :string :default ""
                :doc "where the jobs live; empty is ~/print3d/jobs")
    ("bed" :string :default "256,256,260"
           :doc "the bed the checks measure against, in mm as X,Y,Z"))
  (:settings (lambda (values table)
               (declare (ignore table))
               (let ((blender (getf values :blender)))
                 (unless (program-path blender)
                   (nlk:config-error
                    "print3d.blender is ~s: no such program on PATH -- install Blender 4.2 or later, or give its path here"
                    blender))
                 values)))
  (:tool "print3d_spec" (nlk:contract 'print3d-spec) 'spec-tool)
  (:tool "print3d_check" (nlk:contract 'print3d-check) 'check-tool)
  (:tool "print3d_make" (nlk:contract 'print3d-make) 'make-tool)
  (:command "print3d" 'run-slash
            :catches 'print3d-error
            :description "Print jobs, and how far each got"
            :argument-hint "SLUG")
  (:help :print3d "print3d: a Blender model, checked and sliced for a Bambu Lab X2D - (print3d:check-mesh \"part.stl\")" *primer*)
  (:start (lambda ()
            (ensure-directories-exist (jobs-root))
            nil)))
