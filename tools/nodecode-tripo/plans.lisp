;;;; plans.lisp --- tool arguments turned into Tripo v3 requests, and priced.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's plans.py, in Lisp. Everything here is pure: no network,
;;;; no filesystem. A local path a body would upload stays a marker
;;;; (UPLOAD-MARKER) until CLIENT:RESOLVE-UPLOADS turns it into a file_token,
;;;; so the credits an answer states are computed from exactly the request
;;;; that gets sent. Prices are Tripo's published list, as the original read
;;;; it: estimates, since Tripo bills what a task actually consumes.

(in-package #:nodecode-tripo)

;;; --- what the wire accepts --------------------------------------------------------

(defparameter +geometry-models+
  '("v3.1-20260211" "v3.0-20250812" "v2.5-20250123" "P1-20260311" "P2-20260801"))
(defparameter +default-geometry-model+ "v3.1-20260211")
(defparameter +fast-texture-model+ "v3.5-20260815")
(defparameter +texture-models+ (list +fast-texture-model+ "v3.0-20250812" "v2.5-20250123"))
(defparameter +texture-qualities+ '("fast" "standard" "detailed" "extreme"))
(defparameter +views+ '("front" "left" "back" "right"))
(defparameter +image-models+
  '("seedream_v4" "seedream_v5" "banana" "banana_pro" "banana2"
    "chat_image_2" "chat_image_2.5_flare" "chat_image_2.5_sunburst"))
(defparameter +image-templates+ '("asset_extraction" "character_completion" "t_pose" "variants" "figure"))
(defparameter +edit-models+
  '("seedream_v5" "banana" "banana_pro" "banana2"
    "chat_image_2" "chat_image_2.5_flare" "chat_image_2.5_sunburst"))
(defparameter +edit-templates+ '("t_pose" "character_completion" "3d_enhance" "variants" "figure"))
(defparameter +premium-edit-models+ '("chat_image_2" "chat_image_2.5_flare" "chat_image_2.5_sunburst"
                                      "banana_pro" "banana2"))
(defparameter +segment-models+ '("v1.0-20250506" "v2.0-20260430"))
(defparameter +completion-modes+ '("ai_completion" "quick_cap"))
(defparameter +rig-models+ '("v1.0-20240301" "v2.5-20260210" "v3.0-20260909"))
(defparameter +rig-types+ '("biped" "quadruped" "hexapod" "octopod" "avian" "serpentine" "aquatic" "others"))
(defparameter +rig-specs+ '("tripo" "mixamo" "actorcore" "unity" "unreal" "vrm"))
(defparameter +convert-formats+ '("GLTF" "FBX" "USDZ" "OBJ" "STL" "3MF"))
(defparameter +retopology-models+ '("v1.0" "v2.0" "v3.0"))
(defparameter +max-animations+ 5)

;;; --- a plan ---------------------------------------------------------------------

(defstruct (plan (:conc-name plan-))
  (endpoint "" :type string)
  (body nil)
  (credits 0 :type integer)
  (summary "" :type string)
  (notes '()))

(defun cost-text (credits)
  (if (zerop credits) "free" (format nil "about ~d credits" credits)))

;;; --- reading the arguments -------------------------------------------------------

(defun arg (args name)
  (gethash name args))

(defun arg-text (args name)
  "ARGS' NAME as a trimmed non-empty string, or NIL."
  (let ((value (gethash name args)))
    (and (stringp value)
         (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
           (and (plusp (length trimmed)) trimmed)))))

(defun arg-required-text (args name)
  (or (arg-text args name) (fail "~a is required" name)))

(defun arg-present-true (args name default)
  "NAME's boolean, DEFAULT when absent (JSON false is present and NIL)."
  (multiple-value-bind (value present) (gethash name args)
    (if present (and value t) default)))

(defun arg-choice (args name options &optional default)
  (let ((value (gethash name args)))
    (cond ((null value) default)
          ((member value options :test #'equal) value)
          (t (fail "~a must be one of: ~{~a~^, ~}" name options)))))

(defun arg-integer (args name low high)
  (let ((value (gethash name args)))
    (cond ((null value) nil)
          ((and (integerp value) (<= low value high)) value)
          (t (fail "~a must be a whole number from ~d to ~d" name low high)))))

(defun task-id-p (value)
  "Whether VALUE is a Tripo task id: a UUID, or the task_... form its docs use."
  (and (stringp value)
       (let ((text (string-downcase (string-trim '(#\Space) value))))
         (or (and (uiop:string-prefix-p "task_" text) (> (length text) 5)
                  (every (lambda (c) (or (alphanumericp c) (find c "_-"))) text))
             (and (= (length text) 36)
                  (loop for index below 36
                        always (if (member index '(8 13 18 23))
                                   (char= #\- (char text index))
                                   (digit-char-p (char text index) 16))))))))

(defun reference (value kind name)
  "VALUE as a task id, file_token or URL that passes through, else a local
file the handler uploads (plans.py:125)."
  (unless (and (stringp value) (plusp (length (string-trim '(#\Space) value))))
    (fail "~a must be a task id, file_token, URL or local file path" name))
  (let ((text (string-trim '(#\Space) value)))
    (if (or (task-id-p text)
            (uiop:string-prefix-p "task_" text)
            (uiop:string-prefix-p "file_" text)
            (uiop:string-prefix-p "http://" text)
            (uiop:string-prefix-p "https://" text))
        text
        (upload-marker text kind))))

(defun path (args name kind)
  (reference (gethash name args) kind name))

(defun body-new (&rest pairs)
  "A JSON object from alternating keys and values, NILs left out."
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr
          do (when value (setf (gethash key object) value)))
    object))

(defun body-put (body key value)
  (when value (setf (gethash key body) value))
  body)

(defun string-list (args name)
  "ARGS' NAME as a list of trimmed non-empty strings, or NIL."
  (let ((value (gethash name args)))
    (cond ((null value) nil)
          ((stringp value) (list (string-trim '(#\Space) value)))
          ((and (vectorp value) (plusp (length value))
                (every (lambda (item) (and (stringp item) (plusp (length (string-trim '(#\Space) item)))))
                       value))
           (map 'list (lambda (item) (string-trim '(#\Space) item)) value))
          (t (fail "~a must be a list of names" name)))))

;;; --- text, image or multiview to 3D ------------------------------------------------

(defparameter +base-prices+ '(("text" 10 20) ("image" 20 30) ("multiview" 20 30))
  "(MODE UNTEXTURED TEXTURED) for the standard geometry models (plans.py:63).")

(defparameter +p-series-prices+ '(("text" 30 40) ("image" 40 50) ("multiview" 40 50))
  "The P series' list, low-poly geometry being pricier.")

(defun price (table mode textured)
  (let ((row (assoc mode table :test #'equal)))
    (if textured (third row) (second row))))

(defun plan-generate-model (args)
  (let* ((prompt (arg-text args "prompt"))
         (image (gethash "image" args))
         (views (or (gethash "views" args) (gethash "multiview_task" args)))
         (model (arg-choice args "model" +geometry-models+ +default-geometry-model+))
         (p-series (uiop:string-prefix-p "P" model))
         (v25 (uiop:string-prefix-p "v2.5" model)))
    (unless (= 1 (count-if #'identity (list prompt image views)))
      (fail "pass exactly one of prompt (text to 3D), image (image to 3D) or views (multiview to 3D)"))
    (let ((body (body-new "model" model))
          (mode nil)
          (endpoint nil))
      (cond
        (prompt
         (when (> (length prompt) 1024) (fail "prompt is limited to 1024 characters"))
         (setf mode "text" endpoint "/generation/text-to-model")
         (body-put body "prompt" prompt))
        (image
         (setf mode "image" endpoint "/generation/image-to-model")
         (body-put body "input" (reference image "image" "image")))
        (t
         (setf mode "multiview" endpoint "/generation/multiview-to-model")
         (cond
           ((task-id-p views)
            ;; the four views a tripo_multiview task made
            (setf (gethash "inputs" body)
                  (vector (body-new "task_id" (string-trim '(#\Space) views)))))
           ((not (hash-table-p views))
            (fail "views must be a tripo_multiview task id or an object with keys from: ~{~a~^, ~}" +views+))
           (t
            (let ((present (remove-if-not (lambda (view) (gethash view views)) +views+)))
              (when (or (not (member "front" present :test #'equal)) (< (length present) 2))
                (fail "views needs the front view and at least one other view"))
              (setf (gethash "inputs" body)
                    (coerce (loop for view in present
                                   collect (body-new view (reference (gethash view views)
                                                                    "image"
                                                                    (format nil "views.~a" view))))
                            'vector)))))))
      (let* ((negative (arg-text args "negative_prompt"))
             (parts (arg-present-true args "generate_parts" nil))
             (texture (arg-present-true args "texture" t))
             (pbr (arg-present-true args "pbr" texture))
             (notes '()))
        (when negative
          (unless (equal mode "text") (fail "negative_prompt only applies to text to 3D"))
          (body-put body "negative_prompt" (subseq negative 0 (min 255 (length negative)))))
        (when parts
          (when (or p-series v25) (fail "generate_parts needs a v3.x model"))
          (when (or (eq (gethash "texture" args) t) (eq (gethash "pbr" args) t))
            (fail "generate_parts cannot be combined with texture or pbr"))
          (setf texture nil pbr nil)
          (push "generate_parts turns texture and PBR off; texture the parts afterwards if needed" notes))
        (let ((textured (or texture pbr))
              (quality (arg-choice args "texture_quality" +texture-qualities+)))
          (setf (gethash "texture" body) (and textured t)
                (gethash "pbr" body) (and pbr t))
          (when (and quality textured)
            (body-put body "texture_quality" quality)
            (when (equal quality "fast") (body-put body "texture_version" +fast-texture-model+)))
          (let ((geometry (arg-choice args "geometry_quality" '("standard" "detailed"))))
            (when (equal geometry "detailed")
              (when (or p-series v25) (fail "geometry_quality=detailed needs a v3.x model"))
              (body-put body "geometry_quality" geometry)))
          (let ((quad (arg-present-true args "quad" nil))
                (smart (arg-present-true args "smart_low_poly" nil)))
            (when (and smart parts) (fail "smart_low_poly and generate_parts cannot be combined"))
            (when (and v25 (or quad smart parts (gethash "auto_size" args)))
              (fail "quad, smart_low_poly, generate_parts and auto_size need a v3.x or P model"))
            (when quad
              (body-put body "quad" t)
              (push "quad output is always FBX" notes))
            (when smart
              (when p-series (fail "smart_low_poly is for v3.x models; P models are low-poly already"))
              (body-put body "smart_low_poly" t))
            (body-put body "generate_parts" parts)
            (body-put body "face_limit" (arg-integer args "face_limit" 48 2000000))
            (body-put body "auto_size" (gethash "auto_size" args))
            (body-put body "model_seed" (gethash "model_seed" args))
            (body-put body "texture_seed" (gethash "texture_seed" args))
            (let* ((base (price (if p-series +p-series-prices+ +base-prices+) mode textured))
                   (credits (+ base
                               (if (and textured quality) (texture-surcharge quality) 0)
                               (if (equal (gethash "geometry_quality" body) "detailed") 20 0)
                               (if quad 5 0)
                               (if smart 10 0)
                               (if parts 20 0)))
                   (label (cond ((equal mode "text") "text to 3D")
                                ((equal mode "image") "image to 3D")
                                (t "multiview to 3D")))
                   (details (append (list model (if textured "textured" "untextured"))
                                    (when (and textured quality (not (equal quality "standard")))
                                      (list (format nil "~a texture" quality)))
                                    (when (equal (gethash "geometry_quality" body) "detailed")
                                      (list "detailed geometry"))
                                    (when quad (list "quad"))
                                    (when smart (list "smart low-poly"))
                                    (when parts (list "parts")))))
              (make-plan :endpoint endpoint :body body :credits credits
                         :summary (format nil "~a (~{~a~^, ~})" label details)
                         :notes (reverse notes)))))))))

(defun texture-surcharge (quality)
  (cond ((equal quality "detailed") 20) ((equal quality "extreme") 30) (t 0)))

;;; --- 2D images ---------------------------------------------------------------------

(defun plan-generate-image (args)
  (let* ((prompt (arg-text args "prompt"))
         (image (gethash "image" args))
         (images (or (gethash "images" args) (and image (vector image))))
         (model (arg-choice args "model" (remove-duplicates (append +image-models+ +edit-models+)
                                                              :test #'equal))))
    (unless (or (null images) (vectorp images)) (fail "images must be a list"))
    (if (or (null images) (zerop (length images)))
        (progn
          (unless prompt (fail "prompt is required"))
          (let ((template (arg-choice args "template" +image-templates+))
                (body (body-new "prompt" prompt "model" model "template" template
                                "size" (gethash "size" args)
                                "aspect_ratio" (gethash "aspect_ratio" args))))
            (make-plan :endpoint "/generation/text-to-image" :body body :credits 5
                       :summary (format nil "text to image~@[ (~a)~]~@[ with ~a~]" template model)
                       :notes (when (and model (uiop:string-prefix-p "chat_image" model))
                                (list "GPT image models can cost more")))))
        (progn
          (when (> (length images) 4) (fail "at most 4 reference images"))
          (let ((template (arg-choice args "template" +edit-templates+))
                (model (arg-choice args "model" +edit-models+)))
            (unless (or prompt template) (fail "editing needs a prompt or a template"))
            (let ((refs (loop for item across images collect (reference item "image" "images")))
                  (body nil))
              (setf body (body-new "input" (first refs)
                                   "prompt" prompt "model" model "template" template
                                   "size" (gethash "size" args)
                                   "aspect_ratio" (gethash "aspect_ratio" args)))
              (when (> (length refs) 1) (body-put body "inputs" (coerce refs 'vector)))
              (make-plan :endpoint "/generation/image-to-image" :body body
                         :credits (if (and model (member model +premium-edit-models+ :test #'equal)) 10 5)
                         :summary (format nil "image edit~@[ (~a)~]~@[ with ~a~]" template model))))))))

;;; --- multiview -----------------------------------------------------------------------

(defun plan-multiview (args)
  (let ((edits (gethash "edits" args))
        (source (gethash "input" args)))
    (if edits
        (progn
          (unless (task-id-p source)
            (fail "edits need input: the task id of an earlier tripo_multiview task"))
          (unless (and (hash-table-p edits)
                       (every (lambda (view) (member view +views+ :test #'equal))
                              (loop for key being the hash-keys of edits collect key))
                       (every (lambda (text) (and (stringp text) (plusp (length (string-trim '(#\Space) text)))))
                              (loop for value being the hash-values of edits collect value)))
            (fail "edits must map views (~{~a~^, ~}) to an instruction" +views+))
          (let ((prompts (loop for view in +views+
                               when (gethash view edits)
                                 collect (body-new "view" view
                                                    "prompt" (string-trim '(#\Space) (gethash view edits))))))
            (make-plan :endpoint "/generation/edit-multiview"
                       :body (body-new "input" (string-trim '(#\Space) source)
                                       "prompts" (coerce prompts 'vector))
                       :credits (* 5 (length prompts))
                       :summary (format nil "edit ~d view~:p (~{~a~^, ~})" (length prompts)
                                         (loop for prompt in prompts collect (gethash "view" prompt))))))
        (let ((image (gethash "image" args)))
          (unless image (fail "pass image (to make four views) or input plus edits (to change them)"))
          (make-plan :endpoint "/generation/image-to-multiview"
                     :body (body-new "input" (reference image "image" "image"))
                     :credits 10 :summary "image to four views")))))

;;; --- retexture ---------------------------------------------------------------------------

(defun plan-texture (args)
  (let* ((quality (arg-choice args "texture_quality" +texture-qualities+ "standard"))
         (model (arg-choice args "model" +texture-models+))
         (prompt (arg-text args "prompt"))
         (image (gethash "image" args))
         (style (gethash "style_image" args)))
    (when (equal quality "fast")
      (when (and model (not (equal model +fast-texture-model+)))
        (fail "texture_quality=fast needs model ~a" +fast-texture-model+))
      (setf model +fast-texture-model+))
    (when (and prompt image) (fail "pass prompt or image as texture guidance, not both"))
    (when (and style (not prompt)) (fail "style_image goes with a text prompt"))
    (let* ((guidance (body-new "text" prompt
                               "image" (and image (reference image "image" "image"))
                               "style_image" (and style (reference style "image" "style_image"))))
           (body (body-new "input" (path args "input" "model")
                           "model" model
                           "texture_quality" quality
                           "texture_prompt" (and (plusp (hash-table-count guidance)) guidance)
                           "pbr" (gethash "pbr" args)
                           "texture_seed" (gethash "texture_seed" args)
                           "part_names" (part-names args))))
      (make-plan :endpoint "/models/texture" :body body
                 :credits (+ (cond ((equal quality "extreme") 30)
                                   ((equal quality "detailed") 20)
                                   (t 10))
                             (if style 5 0))
                 :summary (format nil "retexture (~a)" quality)))))

(defun part-names (args)
  (let ((names (string-list args "part_names")))
    (and names (coerce names 'vector))))

;;; --- parts, UV, rig, animation, retopology, conversion --------------------------------------

(defun plan-parts (args)
  (let ((action (arg-choice args "action" '("segment" "complete") "segment")))
    (if (equal action "segment")
        (let* ((model (arg-choice args "model" +segment-models+))
               (granularity (arg-choice args "granularity" '("simple" "balanced" "detailed"))))
          (when granularity
            (setf model (or model "v2.0-20260430"))
            (unless (equal model "v2.0-20260430")
              (fail "granularity needs segmentation model v2.0-20260430")))
          (make-plan :endpoint "/mesh/segment"
                     :body (body-new "input" (path args "input" "model")
                                     "model" model
                                     "segmentation_granularity" granularity)
                     :credits 40
                     :summary (format nil "split into parts~@[ (~a)~]" granularity)))
        (let ((source (gethash "input" args)))
          (unless (task-id-p source)
            (fail "complete needs input: the task id of a finished tripo_parts segment task"))
          (let ((mode (arg-choice args "completion_mode" +completion-modes+))
                (names (part-names args)))
            (make-plan :endpoint "/mesh/complete"
                       :body (body-new "input" (string-trim '(#\Space) source)
                                       "completion_mode" mode "part_names" names)
                       :credits 50
                       :summary (format nil "complete ~a~@[ (quick cap)~]"
                                        (if names
                                            (format nil "~d part~:p" (length names))
                                            "all parts")
                                        (equal mode "quick_cap"))))))))

(defun plan-uv (args)
  (let* ((bake (arg-present-true args "bake" t))
         (body (body-new "input" (path args "input" "model"))))
    (unless bake (setf (gethash "bake" body) nil))
    (make-plan :endpoint "/mesh/uv" :body body :credits 20
               :summary (if bake "smart UV unwrap" "smart UV unwrap (no texture bake)"))))

(defun plan-rig-check (args)
  (make-plan :endpoint "/animations/rig-check"
             :body (body-new "input" (path args "input" "model"))
             :credits 0 :summary "rig check"))

(defun plan-rig (args)
  (let* ((given-model (arg-choice args "model" +rig-models+))
         (rig-type (arg-choice args "rig_type" +rig-types+))
         (spec (arg-choice args "spec" +rig-specs+))
         (out-format (arg-choice args "out_format" '("glb" "fbx")))
         (model given-model))
    ;; v1.0, Tripo's default, rigs bipeds only (plans.py:447)
    (when (and rig-type (not (equal rig-type "biped"))
               (member model '(nil "v1.0-20240301") :test #'equal))
      (setf model (if (equal rig-type "others") "v3.0-20260909" "v2.5-20260210")))
    (when (and (equal rig-type "others") (not (equal model "v3.0-20260909")))
      (fail "rig_type=others needs rig model v3.0-20260909"))
    (when (and (member spec '("actorcore" "unity" "unreal" "vrm") :test #'equal)
               (member model '(nil "v1.0-20240301") :test #'equal))
      (setf model "v3.0-20260909"))
    (make-plan :endpoint "/animations/rig"
               :body (body-new "input" (path args "input" "model")
                               "model" model "rig_type" rig-type
                               "spec" spec "out_format" out-format)
               :credits 25
               :summary (format nil "auto-rig (~a, ~a)" (or rig-type "biped")
                                (or model "v1.0-20240301")))))

(defun plan-animate (args)
  (let ((source (gethash "input" args)))
    (unless (task-id-p source) (fail "input must be the task_id of a finished tripo_rig task"))
    (let* ((animations (string-list args "animations"))
           (out-format (arg-choice args "out_format" '("glb" "fbx"))))
      (unless animations (fail "animations must be a list of preset ids such as preset:walk"))
      (setf animations (loop for name in animations
                             collect (if (uiop:string-prefix-p "preset:" name)
                                         name
                                         (concatenate 'string "preset:" name))))
      (when (> (length animations) +max-animations+)
        (fail "at most ~d animations per task" +max-animations+))
      (let ((body (body-new "input" (string-trim '(#\Space) source)
                            "out_format" out-format
                            "animate_in_place" (gethash "animate_in_place" args)
                            "export_with_geometry" (gethash "export_with_geometry" args))))
        (if (= 1 (length animations))
            (setf (gethash "animation" body) (first animations))
            (setf (gethash "animations" body) (coerce animations 'vector)))
        (make-plan :endpoint "/animations/retarget" :body body
                   :credits (* 10 (length animations))
                   :summary (format nil "animate (~{~a~^, ~})" animations))))))

(defun plan-retopology (args)
  (let* ((model (arg-choice args "model" +retopology-models+ "v3.0"))
         (face-limit (arg-integer args "face_limit" 48 2000000))
         (quad (arg-present-true args "quad" nil))
         (smart-uv (arg-present-true args "smart_uv" nil)))
    (unless face-limit (fail "face_limit (the target polycount) is required"))
    (when (and smart-uv (not (equal model "v3.0"))) (fail "smart_uv needs model v3.0"))
    (make-plan :endpoint "/mesh/decimate"
               :body (body-new "input" (path args "input" "model")
                               "model" model "face_limit" face-limit
                               "quad" (and quad t) "smart_uv" (and smart-uv t))
               :credits (if (equal model "v3.0") (if quad 40 35) 30)
               :summary (format nil "retopology to ~:d faces (~a~@[, quad~]~@[, smart UV~])"
                                face-limit model quad smart-uv))))

(defparameter +convert-options+
  '("face_limit" "quad" "texture_size" "texture_format" "scale_factor"
    "pivot_to_center_bottom" "flatten_bottom" "export_orientation"
    "fbx_preset" "with_animation" "animate_in_place" "part_names"))

(defun plan-convert (args)
  (let* ((raw (gethash "format" args))
         (format (and (stringp raw) (string-upcase raw))))
    (unless (member format +convert-formats+ :test #'equal)
      (fail "format must be one of: ~{~a~^, ~}" +convert-formats+))
    (let ((body (body-new "input" (path args "input" "model") "format" format))
          (advanced nil))
      (arg-choice args "export_orientation" '("+x" "-x" "+y" "-y"))
      (arg-choice args "fbx_preset" '("blender" "3dsmax" "mixamo" "bake_scale"))
      (dolist (key +convert-options+)
        (multiple-value-bind (value present) (gethash key args)
          (when present
            (setf advanced t)
            (setf (gethash key body) (if (equal key "part_names") (part-names args) value)))))
      (make-plan :endpoint "/models/convert" :body body
                 :credits (if advanced 10 5)
                 :summary (format nil "convert to ~a~@[ with processing~]" format advanced)))))

;;; --- the table ------------------------------------------------------------------------------

(defparameter *builders*
  (list (cons "tripo_generate_model" #'plan-generate-model)
        (cons "tripo_generate_image" #'plan-generate-image)
        (cons "tripo_texture" #'plan-texture)
        (cons "tripo_rig_check" #'plan-rig-check)
        (cons "tripo_rig" #'plan-rig)
        (cons "tripo_animate" #'plan-animate)
        (cons "tripo_retopology" #'plan-retopology)
        (cons "tripo_convert" #'plan-convert)
        (cons "tripo_parts" #'plan-parts)
        (cons "tripo_uv" #'plan-uv)
        (cons "tripo_multiview" #'plan-multiview))
  "Every tool that creates a task, and how its arguments become one.")

(defparameter *paid-tools*
  (remove "tripo_rig_check" (mapcar #'car *builders*) :test #'equal)
  "The builders that spend credits: rig_check creates a task but is free.")
