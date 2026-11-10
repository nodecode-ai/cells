;;;; export.lisp --- what a compiled scene becomes: a glTF, an OBJ, an STL and
;;;; the page the hologram plays in.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The original writes glTF (named parts, working joints), OBJ, STL and 3MF
;;;; from one compiled scene and reads every file back before it says it is
;;;; done. The same discipline is here with fewer formats: one triangle list
;;;; per part feeds all three, each file is read back and counted, and the
;;;; page is one self-contained HTML file with no network in it -- the lines
;;;; and triangles are inlined, and the viewer is a little canvas script.

(in-package #:nodecode-construct)

;;; --- bytes --------------------------------------------------------------------------

(defun f32-octets (value)
  "VALUE as four little-endian octets, single precision."
  (let ((bits (sb-kernel:single-float-bits (coerce value 'single-float))))
    (list (ldb (byte 8 0) bits) (ldb (byte 8 8) bits)
          (ldb (byte 8 16) bits) (ldb (byte 8 24) bits))))

(defun u32-octets (value)
  (list (ldb (byte 8 0) value) (ldb (byte 8 8) value)
        (ldb (byte 8 16) value) (ldb (byte 8 24) value)))

(defun octets (list) (coerce list '(vector (unsigned-byte 8))))

(defun write-octets (path list)
  (nlk:write-file-atomically path (octets list))
  path)

(defun file-octets-of (path)
  (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
    (let ((buffer (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence buffer in)
      buffer)))

(defun read-u32-le (octets start)
  "The little-endian 32-bit word at START, for reading a written file back."
  (+ (aref octets start) (ash (aref octets (+ start 1)) 8)
     (ash (aref octets (+ start 2)) 16) (ash (aref octets (+ start 3)) 24)))

(defun file-size (path)
  (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
    (file-length in)))

;;; --- colours -------------------------------------------------------------------------

(defun color-rgb (text)
  "TEXT (#rrggbb) as three numbers from 0 to 1."
  (let ((hex (if (color-p text) (subseq text 1) "808080")))
    (loop for i from 0 below 6 by 2
          collect (/ (parse-integer hex :start i :end (+ i 2) :radix 16) 255d0))))

(defun color-hex (rgb)
  (format nil "#~2,'0x~2,'0x~2,'0x"
          (max 0 (min 255 (round (* 255d0 (first rgb)))))
          (max 0 (min 255 (round (* 255d0 (second rgb)))))
          (max 0 (min 255 (round (* 255d0 (third rgb)))))))

;;; --- glTF ------------------------------------------------------------------------------

(defun part-vertices (part)
  "PART's triangles as one flat list of coordinates: three vertices each, in order."
  (loop for triangle in (getf part :triangles)
        append (loop for vertex in triangle append (coerce vertex 'list))))

(defun part-bounds (part)
  "(VALUES LOW HIGH), each a list of three, over the part's own vertices."
  (let ((low (list most-positive-double-float most-positive-double-float most-positive-double-float))
        (high (list most-negative-double-float most-negative-double-float most-negative-double-float)))
    (dolist (triangle (getf part :triangles))
      (dolist (vertex triangle)
        (loop for axis from 0 below 3
              do (setf (nth axis low) (min (nth axis low) (aref vertex axis))
                       (nth axis high) (max (nth axis high) (aref vertex axis))))))
    (values low high)))

(defun write-gltf (dir name geometry)
  "One construct.gltf and its construct.bin: a node and a mesh per part, its
triangles as POSITION, its own material and its certainty in the node's
extras. Answers the two paths."
  (let ((chunks '()) (views '()) (accessors '()) (meshes '()) (nodes '())
        (materials '()) (total 0) (index 0))
    (dolist (part geometry)
      (let* ((octets (loop for value in (part-vertices part) append (f32-octets value)))
             (count (/ (length octets) 12)))
        (multiple-value-bind (low high) (part-bounds part)
          (push (nlk:json-object "buffer" 0 "byteOffset" total "byteLength" (length octets)
                                  "target" 34962)
                views)
          (push (nlk:json-object "bufferView" index "componentType" 5126 "count" count
                                  "type" "VEC3" "min" (coerce low 'vector) "max" (coerce high 'vector))
                accessors)
          (push (nlk:json-object "name" (getf part :id)
                                  "primitives" (vector (nlk:json-object
                                                        "attributes" (nlk:json-object "POSITION" index)
                                                        "material" index "mode" 4)))
                meshes)
          (push (nlk:json-object "name" (getf part :id) "mesh" index
                                  "extras" (nlk:json-object "certainty" (getf part :certainty)
                                                             "role" (getf part :role)
                                                             "color" (getf part :color)
                                                             "size" (coerce (getf part :size) 'vector)))
                nodes)
          (push (nlk:json-object "name" (getf part :id) "doubleSided" t
                                  "pbrMetallicRoughness"
                                  (nlk:json-object
                                   "baseColorFactor"
                                   (coerce (append (color-rgb (getf part :color)) (list 1d0)) 'vector)
                                   "metallicFactor" 0d0 "roughnessFactor" 0.9d0))
                materials)
          (push octets chunks)
          (incf total (length octets))
          (incf index))))
    (let* ((json (nlk:json-object
                   "asset" (nlk:json-object "version" "2.0" "generator" "nodecode-construct")
                   "scene" 0
                   "scenes" (vector (nlk:json-object
                                      "name" name
                                      "nodes" (coerce (loop for i below (length geometry) collect i)
                                                      'vector)))
                   "nodes" (coerce (nreverse nodes) 'vector)
                   "meshes" (coerce (nreverse meshes) 'vector)
                   "accessors" (coerce (nreverse accessors) 'vector)
                   "bufferViews" (coerce (nreverse views) 'vector)
                   "buffers" (vector (nlk:json-object "byteLength" total "uri" "construct.bin"))
                   "materials" (coerce (nreverse materials) 'vector)))
           (gltf (merge-pathnames "construct.gltf" dir))
           (bin (merge-pathnames "construct.bin" dir)))
      (write-octets bin (apply #'append (nreverse chunks)))
      (nlk:write-file-atomically gltf (nlk:pretty-json json))
      (values gltf bin))))

(defun gltf-summary (gltf bin)
  "What the written glTF says, read back from disk: its parts, its accessors
and the geometry's bytes against the file's."
  (let* ((json (nlk:decode-json (nlk:read-text gltf)))
         (nodes (nlk:json-array json "nodes"))
         (meshes (nlk:json-array json "meshes"))
         (accessors (nlk:json-array json "accessors"))
         (buffer (first (coerce (nlk:json-array json "buffers") 'list)))
         (declared (nlk:json-value buffer :integer "byteLength"))
         (on-disk (file-size bin)))
    (unless (= declared on-disk)
      (fail "the glTF declares ~d bytes of geometry and ~a holds ~d"
            declared (file-namestring bin) on-disk))
    (format nil "gltf: ~d nodes, ~d meshes, ~d accessors, ~d bytes"
            (length nodes) (length meshes) (length accessors) on-disk)))

;;; --- OBJ ---------------------------------------------------------------------------------

(defun write-obj (dir name geometry)
  "One construct.obj: a named object per part, its vertices and faces in
metres."
  (let ((path (merge-pathnames "construct.obj" dir))
        (text (with-output-to-string (out)
                (let ((vertices 0))
                  (format out "# ~a~%# nodecode-construct, metres, Z up~%" name)
                  (dolist (part geometry)
                    (format out "o ~a~%" (getf part :id))
                    (dolist (triangle (getf part :triangles))
                      (let ((start (1+ vertices)))
                        (dolist (vertex triangle)
                          (format out "v ~,6f ~,6f ~,6f~%" (px vertex) (py vertex) (pz vertex))
                          (incf vertices))
                        (format out "f ~d ~d ~d~%" start (1+ start) (+ 2 start)))))))))
    (nlk:write-file-atomically path text)
    path))

(defun obj-summary (path)
  "The objects, vertices and faces the OBJ file actually carries."
  (let ((objects 0) (vertices 0) (faces 0))
    (dolist (line (uiop:split-string (or (nlk:read-text path) "") :separator '(#\Newline)))
      (when (> (length line) 2)
        (let ((head (subseq line 0 2)))
          (cond ((string= head "o ") (incf objects))
                ((string= head "v ") (incf vertices))
                ((string= head "f ") (incf faces))))))
    (format nil "obj: ~d objects, ~d vertices, ~d faces" objects vertices faces)))

;;; --- STL -----------------------------------------------------------------------------------

(defun stl-header (name)
  "80 octets naming the file, padded, as a binary STL's header is."
  (let ((text (format nil "nodecode-construct ~a" name)))
    (loop for i below 80 collect (if (< i (length text)) (char-code (char text i)) 0))))

(defun write-stl (dir name geometry &key (scale 1000d0))
  "A binary STL in millimetres, as a slicer reads it, whatever the scene's unit."
  (let ((triangles (loop for part in geometry append (getf part :triangles)))
        (path (merge-pathnames "construct.stl" dir))
        (bytes (stl-header name)))
    (flet ((add (octets) (setf bytes (nconc bytes octets))))
      (add (u32-octets (length triangles)))
      (dolist (triangle triangles)
        (add (append (u32-octets 0) (u32-octets 0) (u32-octets 0)))
        (dolist (vertex triangle)
          (add (append (f32-octets (* scale (px vertex)))
                       (f32-octets (* scale (py vertex)))
                       (f32-octets (* scale (pz vertex))))))
        (add (list 0 0))))
    (write-octets path bytes)))

(defun stl-summary (path)
  "The triangle count the STL's own header carries, against the bytes on disk."
  (let* ((octets (file-octets-of path))
         (count (read-u32-le octets 80))
         (expected (+ 84 (* count 50))))
    (unless (= expected (length octets))
      (fail "the STL says ~d triangles (~d bytes) and is ~d bytes"
            count expected (length octets)))
    (format nil "stl: ~d triangles, ~d bytes, millimetres" count (length octets))))

;;; --- export ---------------------------------------------------------------------------------

(defun export-scene (dir formats)
  "Write DIR/export/ in each of FORMATS (gltf, obj, stl, or all), read every
file back, and answer one line for each under the scene's summary."
  (let* ((scene (load-scene dir))
         (problems (check-scene scene)))
    (when problems
      (fail "~a does not check:~%~a" (scene-file dir) (problems-text problems)))
    (multiple-value-bind (geometry low high) (compile-scene scene)
      (declare (ignore low high))
      (let ((out (ensure-directories-exist (merge-pathnames "export/" dir)))
            (wanted (if (member "all" formats :test #'equal)
                        '("gltf" "obj" "stl")
                        formats))
            (lines '()))
        (when (member "gltf" wanted :test #'equal)
          (multiple-value-bind (gltf bin) (write-gltf out (scene-name scene) geometry)
            (push (gltf-summary gltf bin) lines)))
        (when (member "obj" wanted :test #'equal)
          (push (obj-summary (write-obj out (scene-name scene) geometry)) lines))
        (when (member "stl" wanted :test #'equal)
          (push (stl-summary (write-stl out (scene-name scene) geometry)) lines))
        (unless lines
          (fail "format ~{~a~^, ~}: one of gltf, obj, stl, all" formats))
        (format nil "~a~%~{~a~%~}in ~a"
                (scene-summary scene) (nreverse lines) (uiop:native-namestring out))))))

;;; --- the page --------------------------------------------------------------------------------

(defparameter *page-script*
  "const scene = SCENE_JSON;
const canvas = document.getElementById('view');
const ctx = canvas.getContext('2d');
let yaw = 0.7, pitch = 0.35, zoom = 1, spin = true, drag = null, last = 0;
function fit() {
  const r = window.devicePixelRatio || 1;
  canvas.width = canvas.clientWidth * r;
  canvas.height = canvas.clientHeight * r;
  ctx.setTransform(r, 0, 0, r, 0, 0);
}
function turn(p) {
  const x = p[0] - scene.center[0], y = p[1] - scene.center[1], z = p[2] - scene.center[2];
  const ca = Math.cos(yaw), sa = Math.sin(yaw), cb = Math.cos(pitch), sb = Math.sin(pitch);
  const X = x * ca - y * sa, Y = x * sa + y * ca;
  return [X, Y * sb + z * cb, Y * cb - z * sb];
}
function draw() {
  const w = canvas.clientWidth, h = canvas.clientHeight;
  const s = Math.min(w, h) * 0.42 * zoom / scene.radius;
  ctx.clearRect(0, 0, w, h);
  ctx.save();
  ctx.translate(w / 2, h / 2);
  const faces = [];
  for (const part of scene.parts) {
    for (const t of part.triangles) {
      const p = t.map(turn);
      faces.push({z: (p[0][1] + p[1][1] + p[2][1]) / 3, p: p, color: part.color});
    }
  }
  faces.sort((a, b) => b.z - a.z);
  ctx.globalAlpha = 0.16;
  for (const f of faces) {
    ctx.beginPath();
    ctx.moveTo(f.p[0][0] * s, -f.p[0][2] * s);
    ctx.lineTo(f.p[1][0] * s, -f.p[1][2] * s);
    ctx.lineTo(f.p[2][0] * s, -f.p[2][2] * s);
    ctx.closePath();
    ctx.fillStyle = f.color;
    ctx.fill();
  }
  ctx.globalAlpha = 1;
  ctx.lineWidth = 1;
  for (const part of scene.parts) {
    ctx.strokeStyle = part.color;
    ctx.beginPath();
    for (const line of part.lines) {
      const a = turn(line[0]), b = turn(line[1]);
      ctx.moveTo(a[0] * s, -a[2] * s);
      ctx.lineTo(b[0] * s, -b[2] * s);
    }
    ctx.stroke();
  }
  ctx.restore();
  const legend = document.getElementById('legend');
  if (!legend.dataset.filled) {
    legend.dataset.filled = '1';
    for (const part of scene.parts) {
      const row = document.createElement('div');
      const swatch = document.createElement('i');
      swatch.style.background = part.color;
      row.className = 'row';
      row.appendChild(swatch);
      const name = document.createElement('b');
      name.textContent = part.id;
      row.appendChild(name);
      const note = document.createElement('span');
      note.textContent = part.role + ' - ' + part.certainty;
      row.appendChild(note);
      legend.appendChild(row);
    }
  }
}
function frame(now) {
  if (spin && !drag) yaw += (now - last) / 4000;
  last = now;
  draw();
  requestAnimationFrame(frame);
}
canvas.addEventListener('pointerdown', e => { drag = [e.clientX, e.clientY]; spin = false; });
canvas.addEventListener('pointermove', e => {
  if (!drag) return;
  yaw += (e.clientX - drag[0]) / 200;
  pitch += (e.clientY - drag[1]) / 300;
  drag = [e.clientX, e.clientY];
});
window.addEventListener('pointerup', () => { drag = null; });
canvas.addEventListener('wheel', e => {
  zoom *= e.deltaY < 0 ? 1.1 : 0.9;
  e.preventDefault();
}, {passive: false});
window.addEventListener('resize', fit);
fit();
requestAnimationFrame(frame);
")

(defun replace-once (what with text)
  "TEXT with its first WHAT replaced by WITH."
  (let ((at (search what text)))
    (if at
        (concatenate 'string (subseq text 0 at) with (subseq text (+ at (length what))))
        text)))

(defun rounded (value) (/ (round (* value 10000d0)) 10000d0))

(defun page-data (name geometry low high)
  "The scene the page draws: each part's lines and triangles, rounded, and the
box it sits in so the viewer can frame it."
  (let ((center (if (and low high)
                    (loop for axis from 0 below 3
                          collect (/ (+ (aref low axis) (aref high axis)) 2d0))
                    '(0d0 0d0 0d0)))
        (radius (if (and low high)
                    (max 1d-3
                         (/ (sqrt (loop for axis from 0 below 3
                                        sum (expt (- (aref high axis) (aref low axis)) 2)))
                            2d0))
                    1d0)))
    (flet ((r3 (p) (coerce (loop for axis from 0 below 3 collect (rounded (aref p axis))) 'vector)))
      (nlk:json-object
       "name" name
       "center" (coerce (loop for value in center collect (rounded value)) 'vector)
       "radius" (rounded radius)
       "parts" (coerce
                (loop for part in geometry
                      collect (nlk:json-object
                               "id" (getf part :id)
                               "role" (getf part :role)
                               "certainty" (getf part :certainty)
                               "color" (color-hex (color-rgb (getf part :color)))
                               "lines" (coerce (loop for (a b) in (getf part :lines)
                                                     collect (vector (r3 a) (r3 b)))
                                               'vector)
                               "triangles" (coerce (loop for (a b c) in (getf part :triangles)
                                                         collect (vector (r3 a) (r3 b) (r3 c)))
                                                   'vector)))
                'vector)))))

(defparameter *page-style*
  "html,body{margin:0;height:100%;background:#0b1016;color:#d7e3ee;font:13px/1.4 system-ui,sans-serif}canvas{position:fixed;inset:0;width:100%;height:100%;touch-action:none}#legend{position:fixed;left:16px;top:16px;max-width:280px;pointer-events:none}h1{font-size:15px;margin:0 0 4px}p{margin:0 0 10px;color:#6f8496}.row{display:flex;gap:6px;align-items:baseline}.row i{width:9px;height:9px;border-radius:2px}.row b{font-weight:600}.row span{color:#6f8496}")

(defun page-html (data)
  (let ((name (nlk:json-value data :string "name"))
        (script (replace-once "SCENE_JSON" (nlk:encode-json-object data) *page-script*)))
    (format nil "<!doctype html>~%<html lang=\"en\">~%<head>~%<meta charset=\"utf-8\">~%<title>~a</title>~%<style>~a~%</style>~%</head>~%<body>~%<canvas id=\"view\"></canvas>~%<aside id=\"legend\"><h1>~a</h1><p>drag to turn, wheel to zoom</p></aside>~%<script>~a~%</script>~%</body>~%</html>~%"
            name *page-style* name script)))

(defun build-page (dir)
  "Write DIR/build/index.html: one self-contained page that draws the construct
in lines and pale faces, turning. Answers its path and what is in it."
  (let* ((scene (load-scene dir))
         (problems (check-scene scene)))
    (when problems
      (fail "~a does not check:~%~a" (scene-file dir) (problems-text problems)))
    (multiple-value-bind (geometry low high) (compile-scene scene)
      (let* ((out (ensure-directories-exist (merge-pathnames "build/" dir)))
             (path (merge-pathnames "index.html" out))
             (html (page-html (page-data (scene-name scene) geometry low high))))
        (nlk:write-file-atomically path html)
        (multiple-value-bind (triangles lines) (geometry-counts geometry)
          (format nil "~a~%page: ~a (~d parts, ~d triangles, ~d lines, ~d bytes)~%open it in a browser"
                  (scene-summary scene) (uiop:native-namestring path)
                  (length geometry) triangles lines (length html)))))))
