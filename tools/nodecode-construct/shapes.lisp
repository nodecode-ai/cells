;;;; shapes.lisp --- the closed vocabulary of shapes a part is built from, as
;;;; triangles and as the lines the hologram page draws.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The original compiles a part's shape from a closed vocabulary into packed
;;;; geometry: triangles for the solid and lines for the hologram. Four shapes
;;;; carry most of what a described thing is made of, so these four are here:
;;;; box, cylinder, cone, sphere. Every shape is built at the origin, sized by
;;;; the part's size, and the same triangle list feeds the glTF, the OBJ and
;;;; the STL, so no two outputs can disagree about the geometry.

(in-package #:nodecode-construct)

(defparameter *segments* 24
  "Sides on a curved shape. The `segments' setting sets it at start.")

(defun circle-segments ()
  "How many sides a curved shape gets: even, and at least eight."
  (let ((n (or *segments* 24)))
    (cond ((not (integerp n)) 24)
          ((< n 8) 8)
          ((oddp n) (1+ n))
          (t n))))

(defun shape-names () '("box" "cylinder" "cone" "sphere"))

(defun point (x y z)
  (vector (coerce x 'double-float) (coerce y 'double-float)
          (coerce z 'double-float)))

(defun px (a) (aref a 0))
(defun py (a) (aref a 1))
(defun pz (a) (aref a 2))

(defun sub (a b) (point (- (px a) (px b)) (- (py a) (py b)) (- (pz a) (pz b))))

(defun cross (a b)
  (point (- (* (py a) (pz b)) (* (pz a) (py b)))
         (- (* (pz a) (px b)) (* (px a) (pz b)))
         (- (* (px a) (py b)) (* (py a) (px b)))))

(defun triangle-area2 (a b c)
  "Twice the area of the triangle; zero for a degenerate one."
  (let ((n (cross (sub b a) (sub c a))))
    (+ (abs (px n)) (abs (py n)) (abs (pz n)))))

(defun live-triangle-p (a b c)
  (> (triangle-area2 a b c) 1d-12))

(defun clean-triangles (triangles)
  "TRIANGLES without the ones a pole of a sphere collapses."
  (remove-if-not (lambda (triangle)
                   (destructuring-bind (a b c) triangle (live-triangle-p a b c)))
                 triangles))

(defun make-mesh (triangles lines)
  (list :triangles (clean-triangles triangles) :lines lines))

(defun mesh-triangles (mesh) (getf mesh :triangles))
(defun mesh-lines (mesh) (getf mesh :lines))

(defun ring (rx ry z n &key (start 0d0))
  "N points around Z at height Z, the first at START radians."
  (loop for i below n
        for a = (+ start (* 2d0 pi i (/ (coerce n 'double-float))))
        collect (point (* rx (cos a)) (* ry (sin a)) z)))

(defun cap-triangles (center rim reversed)
  "The fan that closes a ring RIM with CENTER; REVERSED faces the other way."
  (loop for i below (length rim)
        for a = (nth i rim)
        for b = (nth (mod (1+ i) (length rim)) rim)
        collect (if reversed (list center b a) (list center a b))))

(defun side-triangles (low high)
  "The quads between two rings of the same length, as triangles."
  (loop for i below (length low)
        for j = (mod (1+ i) (length low))
        for a = (nth i low) for b = (nth j low)
        for c = (nth i high) for d = (nth j high)
        append (list (list a b d) (list a d c))))

(defun box-mesh (size)
  (let* ((hx (/ (aref size 0) 2d0)) (hy (/ (aref size 1) 2d0)) (hz (/ (aref size 2) 2d0))
         (x0 (- hx)) (x1 hx) (y0 (- hy)) (y1 hy) (z0 (- hz)) (z1 hz)
         (v000 (point x0 y0 z0)) (v100 (point x1 y0 z0)) (v110 (point x1 y1 z0)) (v010 (point x0 y1 z0))
         (v001 (point x0 y0 z1)) (v101 (point x1 y0 z1)) (v111 (point x1 y1 z1)) (v011 (point x0 y1 z1)))
    (make-mesh
     (list (list v000 v110 v100) (list v000 v010 v110)
           (list v001 v101 v111) (list v001 v111 v011)
           (list v000 v100 v101) (list v000 v101 v001)
           (list v010 v011 v111) (list v010 v111 v110)
           (list v000 v001 v011) (list v000 v011 v010)
           (list v100 v110 v111) (list v100 v111 v101))
     (list (list v000 v100) (list v100 v110) (list v110 v010) (list v010 v000)
           (list v001 v101) (list v101 v111) (list v111 v011) (list v011 v001)
           (list v000 v001) (list v100 v101) (list v110 v111) (list v010 v011)))))

(defun cylinder-mesh (size)
  (let* ((n (circle-segments))
         (rx (/ (aref size 0) 2d0)) (ry (/ (aref size 1) 2d0))
         (z0 (/ (aref size 2) -2d0)) (z1 (/ (aref size 2) 2d0))
         (low (ring rx ry z0 n)) (high (ring rx ry z1 n))
         (bottom (point 0d0 0d0 z0)) (top (point 0d0 0d0 z1)))
    (make-mesh (append (side-triangles low high)
                       (cap-triangles top high nil)
                       (cap-triangles bottom low t))
               (append (loop for i below n
                             for j = (mod (1+ i) n)
                             collect (list (nth i low) (nth j low)))
                       (loop for i below n
                             for j = (mod (1+ i) n)
                             collect (list (nth i high) (nth j high)))
                       (loop for i in (list 0 (floor n 4) (floor n 2) (floor (* 3 n) 4))
                             collect (list (nth i low) (nth i high)))))))

(defun cone-mesh (size)
  (let* ((n (circle-segments))
         (rx (/ (aref size 0) 2d0)) (ry (/ (aref size 1) 2d0))
         (z0 (/ (aref size 2) -2d0)) (z1 (/ (aref size 2) 2d0))
         (low (ring rx ry z0 n))
         (apex (point 0d0 0d0 z1))
         (bottom (point 0d0 0d0 z0)))
    (make-mesh (append (loop for i below n
                             for j = (mod (1+ i) n)
                             collect (list (nth i low) (nth j low) apex))
                       (cap-triangles bottom low t))
               (append (loop for i below n
                             for j = (mod (1+ i) n)
                             collect (list (nth i low) (nth j low)))
                       (loop for i in (list 0 (floor n 4) (floor n 2) (floor (* 3 n) 4))
                             collect (list (nth i low) apex))))))

(defun sphere-meridian (rx ry rz n plane)
  "N+1 points along a meridian in PLANE (:xz or :yz), pole to pole."
  (loop for i from 0 to n
        for phi = (* pi i (/ (coerce n 'double-float)))
        for s = (sin phi) for c = (cos phi)
        collect (if (eq plane :xz)
                    (point (* rx s) 0d0 (* rz c))
                    (point 0d0 (* ry s) (* rz c)))))

(defun polyline-lines (points &key (closed nil))
  "POINTS as the line segments between neighbours."
  (loop for (a b) on (if closed (append points (list (first points))) points)
        while b collect (list a b)))

(defun sphere-mesh (size)
  (let* ((n (circle-segments))
         (m (max 4 (round n 2)))
         (rx (/ (aref size 0) 2d0)) (ry (/ (aref size 1) 2d0)) (rz (/ (aref size 2) 2d0))
         (rings (loop for i from 0 to m
                      for phi = (* pi i (/ (coerce m 'double-float)))
                      collect (ring (* rx (sin phi)) (* ry (sin phi)) (* rz (cos phi)) n))))
    (make-mesh
     (loop for (low high) on rings while high
           append (side-triangles low high))
     (append (polyline-lines (ring rx ry 0d0 n) :closed t)
             (polyline-lines (sphere-meridian rx ry rz (* 2 m) :xz))
             (polyline-lines (sphere-meridian rx ry rz (* 2 m) :yz))))))

(defun shape-mesh (shape size)
  "SIZE (a vector of three) in SHAPE, at the origin."
  (cond ((string-equal shape "box") (box-mesh size))
        ((string-equal shape "cylinder") (cylinder-mesh size))
        ((string-equal shape "cone") (cone-mesh size))
        ((string-equal shape "sphere") (sphere-mesh size))
        (t (fail "shape ~s is not one of ~{~a~^, ~}" shape (shape-names)))))
