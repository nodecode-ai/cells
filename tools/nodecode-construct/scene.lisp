;;;; scene.lisp --- one scene: the file, what is wrong with it, and what it
;;;; compiles to.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A scene is declarative data: a named list of parts, each of a shape the
;;;; vocabulary carries at a size and a place, plus the facts the thing is
;;;; known by and where each came from. No code, no URLs, no file reads.
;;;;
;;;; Units are metres and Z is up, as in the original's format. A part's
;;;; size is its full extent in X, Y and Z; its rotation is XYZ degrees about
;;;; its own centre; its position is that centre. The compiler turns each
;;;; part into triangles (the solid every export is cut from) and lines (what
;;;; the hologram page draws), so a build, an export and an inspection of one
;;;; scene can never disagree with each other.

(in-package #:nodecode-construct)

(defparameter +format+ "construct-scene")
(defparameter +version+ 2)
(defparameter +max-parts+ 1024)
(defparameter +max-extent+ 500d0)
(defparameter +min-size+ 1d-4)
(defparameter +default-color+ "#9ad7ff")

(defparameter +certainties+ '("verified" "user_supplied" "traced" "inferred" "decorative")
  "How a part or a fact is known, strongest first.")

(defparameter +roles+ '("mass" "identity" "structure" "detail")
  "What a part does in the thing: its bulk, what makes it itself, what holds
it up, what dresses it.")

(defparameter +categories+
  '("architecture" "object" "machine" "vehicle" "organism" "fictional" "other"))

(defparameter +source-kinds+
  '("measurement" "document" "drawing" "reference_image" "observation" "dataset"
    "user_supplied" "generated"))

;;; --- where a project sits -------------------------------------------------------------

(defun safe-slug-p (slug)
  "SLUG names one folder and cannot leave the projects folder."
  (and (stringp slug) (<= 1 (length slug) 64)
       (every (lambda (c) (find c "abcdefghijklmnopqrstuvwxyz0123456789_-")) slug)))

(defun projects-root ()
  "The `projects_folder' setting, else <home>/construct/."
  (let ((folder (string-trim '(#\Space #\Tab) (or (setting :projects-folder) ""))))
    (uiop:ensure-directory-pathname
     (if (plusp (length folder)) folder (nlk:home "construct/")))))

(defun project-dir (slug &key (create nil))
  "projects/SLUG, refused unless SLUG is a plain slug."
  (unless (safe-slug-p slug)
    (fail "slug ~s: use 1 to 64 of a-z, 0-9, _ and -" slug))
  (let ((dir (merge-pathnames (format nil "~a/" slug) (projects-root))))
    (when create (ensure-directories-exist (merge-pathnames "scene.json" dir)))
    dir))

(defun scene-file (dir) (merge-pathnames "scene.json" dir))

(defun load-scene (dir)
  "DIR's scene.json as decoded JSON, refused when it is absent or is not a
JSON object."
  (let* ((path (scene-file dir))
         (text (nlk:read-text path)))
    (unless text (fail "no scene at ~a: construct_start writes one" (uiop:native-namestring path)))
    (let ((scene (ignore-errors (nlk:decode-json text))))
      (unless (hash-table-p scene)
        (fail "~a is not a JSON object" (uiop:native-namestring path)))
      scene)))

(defun save-scene (dir scene)
  (let ((path (scene-file dir)))
    (nlk:write-file-atomically path (nlk:pretty-json scene))
    path))

;;; --- reading a member without trusting it ---------------------------------------------

(defun members-of (object key)
  "(VALUES VALUE PRESENT) for KEY in OBJECT; NIL NIL when OBJECT is not an object."
  (if (hash-table-p object)
      (gethash key object)
      (values nil nil)))

(defun text-member (object key)
  (let ((value (members-of object key)))
    (and (stringp value) value)))

(defun number-list (object key)
  "KEY's member as a list of numbers, or NIL when it is not an array of numbers."
  (let ((value (members-of object key)))
    (when (and (vectorp value) (plusp (length value))
               (every #'numberp value))
      (map 'list (lambda (n) (coerce n 'double-float)) value))))

(defun part-size (part)
  (or (number-list part "size") (list 1d0 1d0 1d0)))

(defun part-position (part)
  (or (number-list part "position") (list 0d0 0d0 0d0)))

(defun part-rotation (part)
  (or (number-list part "rotation") (list 0d0 0d0 0d0)))

(defun part-id (part) (or (text-member part "id") ""))
(defun part-shape (part) (or (text-member part "shape") "box"))
(defun part-certainty (part) (or (text-member part "certainty") "inferred"))
(defun part-role (part) (or (text-member part "role") "structure"))
(defun part-color (part) (or (text-member part "color") +default-color+))

(defun parts-of (scene)
  (let ((parts (members-of scene "parts")))
    (if (vectorp parts) (coerce parts 'list) '())))

(defun facts-of (scene)
  (let ((facts (members-of scene "facts")))
    (if (vectorp facts) (coerce facts 'list) '())))

(defun sources-of (scene)
  (let ((sources (members-of scene "sources")))
    (if (vectorp sources) (coerce sources 'list) '())))

;;; --- what is wrong with a scene --------------------------------------------------------

(defun color-p (text)
  (and (stringp text) (= 7 (length text)) (char= #\# (char text 0))
       (every (lambda (c) (find c "0123456789abcdefABCDEF")) (subseq text 1))))

(defun triple-problem (path numbers)
  "Why NUMBERS is not three numbers, or NIL."
  (cond ((null numbers) (format nil "~a needs three numbers" path))
        ((/= 3 (length numbers)) (format nil "~a has ~d numbers, not three" path (length numbers)))
        (t nil)))

(defun check-parts (scene problems)
  (let* ((parts (members-of scene "parts"))
         (seen '()))
    (cond ((not (vectorp parts))
           (push (list :path "parts" :text "parts is missing or is not an array"
                       :hint "parts: [{\"id\": \"wall\", \"shape\": \"box\", \"size\": [4,0.2,2.5], \"position\": [0,0,1.25]}]")
                 problems))
          ((zerop (length parts))
           (push (list :path "parts" :text "parts is empty"
                       :hint "one part is enough to start; add the thing's own parts") problems))
          ((> (length parts) +max-parts+)
           (push (list :path "parts" :text (format nil "~d parts, more than the ~d a scene carries"
                                                    (length parts) +max-parts+)
                       :hint "join what is small into the part it belongs to") problems)))
    (loop for part across (if (vectorp parts) parts #())
          for index from 0
          for at = (format nil "parts[~d]" index)
          do (cond
               ((not (hash-table-p part))
                (push (list :path at :text "not an object" :hint "each part is {\"id\": .., \"shape\": ..}") problems))
               (t
                (let ((id (text-member part "id")))
                  (cond ((null id)
                         (push (list :path (format nil "~a.id" at) :text "no id"
                                     :hint "a short name, a-z 0-9 _ -") problems))
                        ((not (safe-slug-p id))
                         (push (list :path (format nil "~a.id" at)
                                     :text (format nil "~s is not a plain id" id)
                                     :hint "1 to 64 of a-z, 0-9, _ and -") problems))
                        ((member id seen :test #'equal)
                         (push (list :path (format nil "~a.id" at)
                                     :text (format nil "~s names two parts" id)
                                     :hint "an id names one part, so an edit can find it") problems))
                        (t (push id seen))))
                (let ((shape (text-member part "shape")))
                  (unless (member shape (shape-names) :test #'equal)
                    (push (list :path (format nil "~a.shape" at)
                                :text (format nil "shape ~s is not in the vocabulary" shape)
                                :hint (format nil "one of ~{~a~^, ~}" (shape-names))) problems)))
                (let ((size (number-list part "size"))
                      (why (triple-problem (format nil "~a.size" at) (number-list part "size"))))
                  (cond (why (push (list :path (format nil "~a.size" at) :text why
                                         :hint "the part's full extent in X, Y and Z, in metres") problems))
                        ((some (lambda (n) (or (< n +min-size+) (> n +max-extent+))) size)
                         (push (list :path (format nil "~a.size" at)
                                     :text (format nil "size ~{~,3f~^, ~} is outside ~,4f to ~,0f m"
                                                   size +min-size+ +max-extent+)
                                     :hint "metres; a part is at least 0.1 mm") problems))
                        (t
                         (let ((position (part-position part)))
                           (when (some (lambda (p s) (> (+ (abs p) (/ s 2d0)) +max-extent+)) position size)
                             (push (list :path (format nil "~a.position" at)
                                         :text (format nil "reaches past ~,0f m from the origin" +max-extent+)
                                         :hint "the extent is measured from the origin") problems))))))
                (let ((why (triple-problem (format nil "~a.position" at) (number-list part "position"))))
                  (when (and why (members-of part "position"))
                    (push (list :path (format nil "~a.position" at) :text why
                                :hint "the part's centre in metres") problems)))
                (let ((why (triple-problem (format nil "~a.rotation" at) (number-list part "rotation"))))
                  (when (and why (members-of part "rotation"))
                    (push (list :path (format nil "~a.rotation" at) :text why
                                :hint "XYZ degrees about the part's own centre") problems)))
                (let ((certainty (text-member part "certainty")))
                  (when (and certainty (not (member certainty +certainties+ :test #'equal)))
                    (push (list :path (format nil "~a.certainty" at)
                                :text (format nil "certainty ~s is not one of the words" certainty)
                                :hint (format nil "~{~a~^, ~}; a part without one is inferred" +certainties+))
                          problems)))
                (let ((role (text-member part "role")))
                  (when (and role (not (member role +roles+ :test #'equal)))
                    (push (list :path (format nil "~a.role" at)
                                :text (format nil "role ~s is not one of the words" role)
                                :hint (format nil "~{~a~^, ~}" +roles+)) problems)))
                (let ((color (text-member part "color")))
                  (when (and color (not (color-p color)))
                    (push (list :path (format nil "~a.color" at)
                                :text (format nil "color ~s is not #rrggbb" color)
                                :hint "six hex digits after #, e.g. #9ad7ff") problems)))))))
  problems)

(defun check-facts (scene problems)
  (let ((source-ids (loop for source in (sources-of scene)
                          for id = (text-member source "id")
                          when id collect id)))
    (loop for fact across (let ((facts (members-of scene "facts")))
                            (if (vectorp facts) facts #()))
          for index from 0
          for at = (format nil "facts[~d]" index)
          do (cond
               ((not (hash-table-p fact))
                (push (list :path at :text "not an object" :hint "each fact is {\"text\": .., \"certainty\": ..}") problems))
               (t
                (let ((text (text-member fact "text")))
                  (unless (and text (plusp (length (string-trim '(#\Space) text))))
                    (push (list :path (format nil "~a.text" at) :text "no text"
                                :hint "one sentence the thing is known by, in the user's words") problems)))
                (let ((certainty (text-member fact "certainty")))
                  (unless (member certainty +certainties+ :test #'equal)
                    (push (list :path (format nil "~a.certainty" at)
                                :text (format nil "certainty ~s is not one of the words" certainty)
                                :hint (format nil "~{~a~^, ~}" +certainties+)) problems)))
                (let ((source (text-member fact "source")))
                  (when (and source (not (member source source-ids :test #'equal)))
                    (push (list :path (format nil "~a.source" at)
                                :text (format nil "source ~s is not declared" source)
                                :hint "declare it under sources, or leave it out: a fact with no source rests on the request")
                          problems)))))))
  problems)

(defun check-sources (scene problems)
  (let ((seen '()))
    (loop for source across (let ((sources (members-of scene "sources")))
                              (if (vectorp sources) sources #()))
          for index from 0
          for at = (format nil "sources[~d]" index)
          do (cond
               ((not (hash-table-p source))
                (push (list :path at :text "not an object" :hint "each source is {\"id\": .., \"kind\": ..}") problems))
               (t
                (let ((id (text-member source "id")))
                  (cond ((null id)
                         (push (list :path (format nil "~a.id" at) :text "no id"
                                     :hint "a short name the facts cite") problems))
                        ((not (safe-slug-p id))
                         (push (list :path (format nil "~a.id" at) :text (format nil "~s is not a plain id" id)
                                     :hint "1 to 64 of a-z, 0-9, _ and -") problems))
                        ((member id seen :test #'equal)
                         (push (list :path (format nil "~a.id" at) :text (format nil "~s names two sources" id)
                                     :hint "an id names one source") problems))
                        (t (push id seen))))
                (let ((kind (text-member source "kind")))
                  (when (and kind (not (member kind +source-kinds+ :test #'equal)))
                    (push (list :path (format nil "~a.kind" at)
                                :text (format nil "kind ~s is not one of the words" kind)
                                :hint (format nil "~{~a~^, ~}" +source-kinds+)) problems)))))))
  problems)

(defun check-scene (scene)
  "Every problem with SCENE as (:path .. :text .. :hint ..), in reading order."
  (let ((problems '()))
    (cond
      ((not (hash-table-p scene))
       (list (list :path "" :text "not a JSON object" :hint "{\"format\": \"construct-scene\", \"version\": 2, \"parts\": [..]}")))
      (t
       (let ((format (text-member scene "format")))
         (unless (equal format +format+)
           (push (list :path "format" :text (format nil "format is ~s, not ~s" format +format+)
                       :hint "a scene says which format it is in") problems)))
       (let ((version (members-of scene "version")))
         (unless (eql version +version+)
           (push (list :path "version" :text (format nil "version is ~s, not ~d" version +version+)
                       :hint "the format version this cell reads") problems)))
       (let ((category (text-member scene "category")))
         (when (and category (not (member category +categories+ :test #'equal)))
           (push (list :path "category" :text (format nil "category ~s is not one of the words" category)
                       :hint (format nil "~{~a~^, ~}" +categories+)) problems)))
       (setf problems (check-sources scene problems))
       (setf problems (check-parts scene problems))
       (setf problems (check-facts scene problems))
       (nreverse problems)))))

(defun problems-text (problems &key (limit 12))
  "PROBLEMS one per line, each with the fix; the rest counted."
  (if (null problems)
      "no problems"
      (with-output-to-string (out)
        (loop for problem in problems
              for index from 1
              when (<= index limit)
                do (format out "~a: ~a -- ~a~%"
                           (getf problem :path) (getf problem :text) (getf problem :hint))
              finally (when (> (length problems) limit)
                        (format out "and ~d more~%" (- (length problems) limit)))))))

;;; --- the compiler ---------------------------------------------------------------------

(defun rotation-matrix (rotation)
  "ROTATION (X, Y, Z degrees) as the nine numbers of Rz*Ry*Rx, row by row."
  (destructuring-bind (x y z) rotation
    (let* ((a (* (/ pi 180d0) x)) (b (* (/ pi 180d0) y)) (c (* (/ pi 180d0) z))
           (ca (cos a)) (sa (sin a)) (cb (cos b)) (sb (sin b)) (cc (cos c)) (sc (sin c)))
      (list (* cc cb) (- (* cc sb sa) (* sc ca)) (+ (* cc sb ca) (* sc sa))
            (* sc cb) (+ (* sc sb sa) (* cc ca)) (- (* sc sb ca) (* cc sa))
            (- sb) (* cb sa) (* cb ca)))))

(defun transform-point (m position p)
  "P rotated by M and moved by POSITION."
  (destructuring-bind (m0 m1 m2 m3 m4 m5 m6 m7 m8) m
    (destructuring-bind (tx ty tz) position
      (point (+ (* m0 (px p)) (* m1 (py p)) (* m2 (pz p)) tx)
             (+ (* m3 (px p)) (* m4 (py p)) (* m5 (pz p)) ty)
             (+ (* m6 (px p)) (* m7 (py p)) (* m8 (pz p)) tz)))))

(defun part-geometry (part)
  "PART as (:id :color :certainty :role :size :triangles :lines), its
geometry in the scene's own coordinates."
  (let* ((mesh (shape-mesh (part-shape part) (coerce (part-size part) 'vector)))
         (m (rotation-matrix (part-rotation part)))
         (position (part-position part))
         (move (lambda (p) (transform-point m position p))))
    (list :id (part-id part) :color (part-color part) :certainty (part-certainty part)
          :role (part-role part) :size (part-size part)
          :triangles (loop for (a b c) in (mesh-triangles mesh)
                           collect (list (funcall move a) (funcall move b) (funcall move c)))
          :lines (loop for (a b) in (mesh-lines mesh)
                       collect (list (funcall move a) (funcall move b))))))

(defun compile-scene (scene)
  "SCENE's parts compiled, in order (VALUES GEOMETRY MIN MAX), MIN and MAX
the scene's own box, or NIL NIL when it has no triangle."
  (let ((geometry (mapcar #'part-geometry (parts-of scene)))
        (low nil) (high nil))
    (dolist (part geometry)
      (dolist (triangle (getf part :triangles))
        (dolist (vertex triangle)
          (setf low (if low (point (min (px low) (px vertex)) (min (py low) (py vertex)) (min (pz low) (pz vertex)))
                        vertex))
          (setf high (if high (point (max (px high) (px vertex)) (max (py high) (py vertex)) (max (pz high) (pz vertex)))
                         vertex)))))
    (values geometry low high)))

(defun geometry-counts (geometry)
  "(VALUES TRIANGLES LINES) of compiled GEOMETRY."
  (loop for part in geometry
        sum (length (getf part :triangles)) into triangles
        sum (length (getf part :lines)) into lines
        finally (return (values triangles lines))))

(defun size-text (low high)
  (if (and low high)
      (format nil "~,3f x ~,3f x ~,3f m" (- (px high) (px low)) (- (py high) (py low)) (- (pz high) (pz low)))
      "no size yet"))

;;; --- what a scene says about itself ----------------------------------------------------

(defun certainty-order (certainty)
  (or (position certainty +certainties+ :test #'equal) (length +certainties+)))

(defun scene-name (scene) (or (text-member scene "name") "untitled"))

(defun scene-summary (scene)
  "One line: the scene, its parts, its geometry and its size."
  (multiple-value-bind (geometry low high) (compile-scene scene)
    (multiple-value-bind (triangles lines) (geometry-counts geometry)
      (format nil "~a: ~d parts, ~d triangles, ~d lines, ~a"
              (scene-name scene) (length geometry) triangles lines (size-text low high)))))

(defun part-color-of (part) (or (getf part :color) +default-color+))


(defun inspect-text (scene)
  "What the scene's own record says is known: its parts by certainty and
size, and every fact with where it came from."
  (multiple-value-bind (geometry low high) (compile-scene scene)
    (let ((sources (loop for source in (sources-of scene)
                         for id = (text-member source "id")
                         when id collect (cons id (or (text-member source "title") "")))))
      (with-output-to-string (out)
        (format out "~a (~a)~%" (scene-name scene) (or (text-member scene "category") "other"))
        (format out "~d parts, ~a~%" (length geometry) (size-text low high))
        (dolist (certainty +certainties+)
          (let ((parts (remove-if-not (lambda (part) (equal certainty (getf part :certainty))) geometry)))
            (when parts
              (format out "~%~a:~%" certainty)
              (dolist (part parts)
                (format out "  ~a  ~a  ~{~,3f~^ x ~} m  ~a~%"
                        (getf part :id) (getf part :role) (getf part :size)
                        (part-color-of part))))))
        (let ((facts (facts-of scene)))
          (format out "~%facts:~%")
          (if (null facts)
              (format out "  none: every part rests on the request alone~%")
              (dolist (fact facts)
                (format out "  [~a] ~a~@[ (~a)~]~%"
                        (or (text-member fact "certainty") "inferred")
                        (or (text-member fact "text") "")
                        (let ((source (text-member fact "source")))
                          (and source (cdr (assoc source sources :test #'equal))))))))
        (let ((sources (sources-of scene)))
          (when sources
            (format out "~%sources:~%")
            (dolist (source sources)
              (format out "  ~a  ~a  ~a~%"
                      (text-member source "id") (or (text-member source "kind") "-")
                      (or (text-member source "title") "")))))))))


;;; --- the two starters -----------------------------------------------------------------

(defun origin-part (id shape size position certainty role color)
  (nlk:json-object "id" id "shape" shape "size" (coerce size 'vector)
                    "position" (coerce position 'vector) "certainty" certainty
                    "role" role "color" color))

(defun starter-cube ()
  (nlk:json-object
   "format" +format+ "version" +version+ "name" "a metre cube" "category" "object"
   "parts" (vector (origin-part "cube" "box" (list 1d0 1d0 1d0) (list 0d0 0d0 0.5d0)
                                 "inferred" "mass" +default-color+))))

(defun starter-shed ()
  (nlk:json-object
   "format" +format+ "version" +version+ "name" "a shed" "category" "architecture"
   "parts" (vector
            (origin-part "floor" "box" (list 3d0 2.4d0 0.1d0) (list 0d0 0d0 0.05d0)
                         "inferred" "mass" "#8a7f6a")
            (origin-part "back-wall" "box" (list 3d0 0.1d0 2.2d0) (list 0d0 1.15d0 1.15d0)
                         "inferred" "structure" "#c9bda6")
            (origin-part "front-wall" "box" (list 3d0 0.1d0 2.2d0) (list 0d0 -1.15d0 1.15d0)
                         "inferred" "structure" "#c9bda6")
            (origin-part "left-wall" "box" (list 0.1d0 2.4d0 2.2d0) (list -1.45d0 0d0 1.15d0)
                         "inferred" "structure" "#c9bda6")
            (origin-part "right-wall" "box" (list 0.1d0 2.4d0 2.2d0) (list 1.45d0 0d0 1.15d0)
                         "inferred" "structure" "#c9bda6")
            (origin-part "roof" "box" (list 3.3d0 2.6d0 0.08d0) (list 0d0 0d0 2.3d0)
                         "inferred" "mass" "#7d4f3a")
            (origin-part "door" "box" (list 0.9d0 0.06d0 1.9d0) (list 0d0 -1.21d0 0.95d0)
                         "inferred" "identity" "#5c6b73"))))

(defun starter-scene (kind)
  "The scene a project starts from: KIND names a starter that comes with the
cell, or is empty for the menu's default."
  (cond ((member kind '("shed" "garden-shed") :test #'equal) (starter-shed))
        ((member kind '("cube" "") :test #'equal) (starter-cube))
        (t (fail "starter ~s: one of cube, shed" kind))))
