;;;; cell-test.lisp --- the checks, the make loop, the spec, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Everything here is offline: an STL is written by the test, and the one
;;;; seam that would run Blender and Bambu Studio (PRINT3D::*RUN*) is replaced
;;;; by a Lisp function that writes the mesh the model script would have
;;;; written. So the mesh reader, the manifold and volume arithmetic, the bed
;;;; check, the make loop and the slice hand-off are all proved without
;;;; either program installed.

(in-package #:nodecode.test)

;;; --- the stubs ---------------------------------------------------------------------

(defvar *stub-blender* nil)
(defvar *stub-studio* nil)
(defparameter *stub-jobs-dir* nil)

(defun print3d-script (path &rest lines)
  "PATH as an executable script of LINES."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-line "#!/bin/sh" out)
    (dolist (line lines) (write-line line out)))
  (uiop:run-program (list "chmod" "755" (uiop:native-namestring path)) :output nil :error-output nil)
  (uiop:native-namestring path))

(defun print3d-stub-blender ()
  "A file the cell accepts as Blender: a start only asks that it is there."
  (or *stub-blender*
      (setf *stub-blender*
            (print3d-script (merge-pathnames "print3d-stub/blender" (uiop:temporary-directory))
                             "exit 0"))))

(defun print3d-stub-studio ()
  (or *stub-studio*
      (setf *stub-studio*
            (print3d-script (merge-pathnames "print3d-stub/bambu-studio" (uiop:temporary-directory))
                             "exit 0"))))

(defun print3d-jobs-dir ()
  (or *stub-jobs-dir*
      (setf *stub-jobs-dir*
            (uiop:native-namestring (merge-pathnames "print3d-jobs/" (uiop:temporary-directory))))))

(define-test-slice "print3d" "PRINT3D-CELL-" :start nodecode-print3d:start-cell
  :defaults (("blender" (print3d-stub-blender)) ("jobs_dir" (print3d-jobs-dir))))

;;; --- STL, written here ---------------------------------------------------------------

(defun print3d-bytes (path bytes)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create
                             :element-type '(unsigned-byte 8))
    (write-sequence (coerce bytes '(vector (unsigned-byte 8))) out))
  path)

(defun print3d-u32 (n) (loop for shift from 0 below 32 by 8 collect (ldb (byte 8 shift) n)))
(defun print3d-f32 (x) (print3d-u32 (ldb (byte 32 0) (sb-kernel:single-float-bits (coerce x 'single-float)))))

(defun print3d-cube-triangles (size &key (z 0.0))
  (let* ((h (/ size 2.0)) (x0 (- h)) (x1 h) (y0 (- h)) (y1 h)
         (z0 (coerce z 'single-float)) (z1 (coerce (+ z size) 'single-float)))
    (flet ((v (x y zz) (list (coerce x 'single-float) (coerce y 'single-float) zz)))
      (list (list (v x0 y0 z0) (v x1 y0 z0) (v x1 y1 z0))
            (list (v x0 y0 z0) (v x1 y1 z0) (v x0 y1 z0))
            (list (v x0 y0 z1) (v x1 y1 z1) (v x1 y0 z1))
            (list (v x0 y0 z1) (v x0 y1 z1) (v x1 y1 z1))
            (list (v x0 y0 z0) (v x0 y0 z1) (v x1 y0 z1))
            (list (v x0 y0 z0) (v x1 y0 z1) (v x1 y0 z0))
            (list (v x1 y0 z0) (v x1 y0 z1) (v x1 y1 z1))
            (list (v x1 y0 z0) (v x1 y1 z1) (v x1 y1 z0))
            (list (v x1 y1 z0) (v x1 y1 z1) (v x0 y1 z1))
            (list (v x1 y1 z0) (v x0 y1 z1) (v x0 y1 z0))
            (list (v x0 y1 z0) (v x0 y1 z1) (v x0 y0 z1))
            (list (v x0 y1 z0) (v x0 y0 z1) (v x0 y0 z0))))))

(defun print3d-write-cube (path size &key (z 0.0))
  "A closed binary STL cube of SIZE mm, its lowest face at Z."
  (let ((triangles (print3d-cube-triangles size :z z)))
    (print3d-bytes path
                   (append (make-list 80 :initial-element 0)
                           (print3d-u32 (length triangles))
                           (loop for (a b c) in triangles
                                 append (append (print3d-f32 0.0) (print3d-f32 0.0) (print3d-f32 0.0)
                                                (loop for vertex in (list a b c)
                                                      append (loop for value in vertex
                                                                   append (print3d-f32 value)))
                                                (list 0 0)))))))

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "print3d"
  (:help :print3d)
  (:command "print3d")
  (:running (is (equal "256,256,260" (nodecode-print3d::setting :bed)) "the X2D bed by default")
            (is (search "print3d_make" (cddr (assoc :print3d nle::*help-topics*))) "the manual says how to work"))
  (:refused ("blender" "definitely-no-such-program-xyz"))
  (:idle nodecode-print3d:print3d-error (nodecode-print3d:job-dir "cube")))

;;; --- the mesh ----------------------------------------------------------------------

(deftest print3d-cell-a-cube-checks-clean ()
  (with-cell-stop ((print3d-start))
    (let* ((stl (print3d-write-cube (merge-pathnames "cube-20.stl" (uiop:temporary-directory)) 20.0))
           (report (nodecode-print3d:check-mesh stl))
           (text (nodecode-print3d:check-mesh-text report)))
      (is (= 12 (getf report :triangles)) "twelve triangles")
      (is (getf report :manifold) "a cube is manifold")
      (is (getf report :on-bed) "its lowest face is on the bed")
      (is (< 7999.0 (getf report :volume) 8001.0) "20 mm cubed is 8000 mm^3")
      (is (null (getf report :errors)) "nothing to fix")
      (is (search "ok" text) "the report ends ok"))))

(deftest print3d-cell-a-part-bigger-than-the-bed-names-the-bed ()
  (with-cell-stop ((print3d-start))
    (let* ((stl (print3d-write-cube (merge-pathnames "cube-300.stl" (uiop:temporary-directory)) 300.0))
           (text (nodecode-print3d:check-mesh-text (nodecode-print3d:check-mesh stl))))
      (is (search "ERROR size 300.00 x 300.00 x 300.00 mm exceeds the 256 x 256 x 260 mm bed" text) "the error carries the bed it was measured against")
      (is (search "not printable yet" text) "and says it is not printable yet"))))

(deftest print3d-cell-a-part-that-floats-is-a-warning-not-an-error ()
  (with-cell-stop ((print3d-start))
    (let* ((stl (print3d-write-cube (merge-pathnames "cube-lifted.stl" (uiop:temporary-directory)) 20.0 :z 5.0))
           (report (nodecode-print3d:check-mesh stl)))
      (is (null (getf report :errors)) "a lifted part still slices")
      (is (search "ensure-on-bed" (first (getf report :warnings))) "the warning says what lifts it"))))

;;; --- the run -----------------------------------------------------------------------

(deftest print3d-cell-make-runs-the-model-then-slices ()
  (with-cell-stop ((print3d-start "studio" (print3d-stub-studio)))
    (let* ((dir (nodecode-print3d:job-dir "cube"))
           (stl (merge-pathnames "part.stl" dir))
           (project (merge-pathnames "part.gcode.3mf" dir))
           (nodecode-print3d::*run*
             (lambda (args directory)
               (cond ((search "blender" (file-namestring (first args)))
                      (print3d-write-cube (pathname (car (last args))) 20.0)
                      (values "" 0))
                     (t (print3d-bytes (merge-pathnames (second (member "--export-3mf" args :test #'equal)) directory)
                                       (make-list 64 :initial-element 0))
                        (values "" 0))))))
      (let ((text (nodecode-print3d:make-job "cube" nil nil t)))
        (is (probe-file (merge-pathnames "model.py" dir)) "a starter model.py is written")
        (is (search "volume: 8000.00" text) "the mesh is checked")
        (is (search "sliced:" text) "and sliced")
        (is (probe-file stl) "the STL is where the job keeps it")
        (is (probe-file project) "and so is the .3mf"))
      (is (search "sliced:" (nodecode-print3d:make-job "cube" nil nil t)) "a second run follows the same loop"))))

(deftest print3d-cell-a-failed-blender-says-what-it-printed ()
  (with-cell-stop ((print3d-start))
    (let ((nodecode-print3d::*run* (lambda (args directory)
                                     (declare (ignore args directory))
                                     (values "line one~%boom: no such bpy operator" 5))))
      (let ((condition (handler-case (progn (nodecode-print3d:make-job "flop" nil nil nil) nil)
                           (nodecode-print3d:print3d-error (condition) condition))))
        (is (typep condition 'nodecode-print3d:print3d-error) "a failed run signals the cell's condition")
        (is (search "boom" (princ-to-string condition)) "and carries Blender's own last lines")))))

(deftest print3d-cell-a-job-that-needs-no-slice-says-why ()
  (with-cell-stop ((print3d-start "studio" "definitely-no-such-studio-xyz"))
    (let ((nodecode-print3d::*run*
            (lambda (args directory)
              (declare (ignore directory))
              (print3d-write-cube (pathname (car (last args))) 20.0)
              (values "" 0))))
      (let ((text (nodecode-print3d:make-job "noslice" nil nil t)))
        (is (search "not sliced:" text) "an absent slicer is a line, not a crash")
        (is (search "print3d.studio" text) "and names the setting to fill")))))

;;; --- the spec ----------------------------------------------------------------------

(deftest print3d-cell-a-dimension-keeps-its-evidence ()
  (with-cell-stop ((print3d-start))
    (let* ((dir (nodecode-print3d:job-dir "evidence"))
           (spec (nodecode-print3d:load-spec dir)))
      (nodecode-print3d:record-dim spec "width" 24.5 "listing" "the listing says 24.5 mm wide")
      (nodecode-print3d:save-spec dir spec)
      (let* ((back (nodecode-print3d:load-spec dir))
             (entry (cdr (assoc "width" (getf back :dims) :test #'equal))))
        (is (= 24.5 (getf entry :mm)) "the size comes back")
        (is (equal "listing" (getf entry :source)) "with where it came from")
        (is (search "the listing says 24.5 mm wide" (nodecode-print3d:spec-text back)) "and the evidence itself"))
      (let ((condition (handler-case (progn (nodecode-print3d:record-dim spec "height" 10.0 "assumed" "") nil)
                           (nodecode-print3d:print3d-error (condition) condition))))
        (is (typep condition 'nodecode-print3d:print3d-error) "an assumed size without a reason refuses")))))

(deftest print3d-cell-each-tool-is-registered-and-answers-through-its-contract ()
  (with-cell-stop ((print3d-start))
    (dolist (name '("print3d_spec" "print3d_check" "print3d_make"))
      (is-present (entry (nle::find-tool name))
        (format nil "~a is registered" name)
        (is (hash-table-p (nlk:contract-schema (second entry))) "against a declared contract")))
    (let* ((schema (second (nle::find-tool "print3d_spec")))
           (call (nlk:contract-decode schema (nlk:make-json-object "slug" "viacontract"
                                                                   "name" "length"
                                                                   "mm" 40
                                                                   "source" "user")))
           (answer (funcall (third (nle::find-tool "print3d_spec")) call)))
      (is (search "recorded length in job viacontract" answer) "the tool records the size")
      (is (search "40.00" answer) "and answers the spec back"))))
