;;;; cell.lisp --- the fourteen tripo_* tools, /tripo, and the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's tools.py and __init__.py: the tool schemas keep their
;;;; names, parameters and wording (minus what only Hermes had: skill_view, the
;;;; viewer directive, MEDIA: lines), and every answer is the text the model
;;;; repairs from or continues with. A create answers at once with its
;;;; estimated cost and the task id; tripo_task waits, tripo_download saves.

(in-package #:nodecode-tripo)

;;; --- the schema, once ---------------------------------------------------------------

(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES -- (NAME TYPE DOC REQUIRED-P [ENUM]) ... -- as a tool schema."
  (list :description description
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (apply #'nlk:make-json-object
                             (loop for property in properties
                                   append (destructuring-bind (name type &optional (doc "") required enum) property
                                            (declare (ignore required))
                                            (list name (apply #'nlk:make-json-object
                                                             (append (list "type" type "description" doc)
                                                                     (when enum
                                                                       (list "enum" (coerce enum 'vector)))))))))
         "required" (coerce (loop for property in properties
                                   when (fourth property) collect (first property))
                            'vector))))

(defparameter +input-doc+
  "A finished Tripo task id (a UUID), a file_token, a public https URL, or a local file path (uploaded to Tripo first; a relative path resolves against the working folder)."
  "What the original's _INPUT says an input may be.")

(defparameter *generate-model-schema*
  (tool-schema
   "Start a Tripo 3D generation task. SPENDS CREDITS (text 10-20, image 20-30, P series 30-50, plus surcharges); say what it will cost before you call it. Pass exactly one of prompt, image, views or multiview_task. Answers a task_id at once; follow with tripo_task to wait, then tripo_download."
   '(("prompt" "string" "Text to 3D: shape, material, style (max 1024 chars).")
     ("image" "string" "Image to 3D (PNG/JPEG/WebP). A tripo_generate_image task_id also works.")
     ("views" "object" "Multiview to 3D: front plus at least one of left/back/right, same object and lighting; each value is an input as described.")
     ("multiview_task" "string" "Multiview to 3D from the four views a tripo_multiview task made (its task id).")
     ("model" "string" "v3.1-20260211 (default, best detail); P1/P2 for clean low-poly game assets (48-20k faces, pricier)." nil
      ("v3.1-20260211" "v3.0-20250812" "v2.5-20250123" "P1-20260311" "P2-20260801"))
     ("texture" "boolean" "Default true. false = bare geometry, cheaper.")
     ("pbr" "boolean" "PBR maps; default true (forces texture on).")
     ("texture_quality" "string" "standard (default), fast, detailed (+20), extreme 8K (+30)." nil
      ("fast" "standard" "detailed" "extreme"))
     ("geometry_quality" "string" "detailed = Ultra geometry, +20, v3.x only." nil ("standard" "detailed"))
     ("face_limit" "integer" "Max faces. Games 10k-100k; omit for adaptive.")
     ("quad" "boolean" "Quad mesh, +5, output becomes FBX.")
     ("smart_low_poly" "boolean" "Hand-made-style low-poly topology, +10, v3.x.")
     ("generate_parts" "boolean" "Editable separate parts, +20, untextured.")
     ("auto_size" "boolean" "Scale to real-world meters (AR/VR, engines).")
     ("negative_prompt" "string" "Text mode only: what to avoid.")
     ("model_seed" "integer")
     ("texture_seed" "integer"))))

(defparameter *generate-image-schema*
  (tool-schema
   "Make a 2D image (5 credits, 10 for chat_image/banana_pro/banana2 edits; say so before you call it). Text only: a concept to agree on before paying for 3D. With image or images: edit them, transfer a style, or apply a template (t_pose, 3d_enhance, variants). Pass the result's task id as tripo_generate_model image."
   '(("prompt" "string" "What to draw or change, most important first.")
     ("image" "string" "One reference image to edit.")
     ("images" "array" "Up to 4 reference images to combine; refer to them as image[1], image[2].")
     ("template" "string" "t_pose for characters you will rig; figure for a full body; asset_extraction (text only) isolates a subject; 3d_enhance (edits) readies a photo for 3D; variants.")
     ("model" "string" "Default seedream_v4 for text, seedream_v5 for edits.")
     ("size" "string" "e.g. 2048x2048 or 2K (seedream / chat_image).")
     ("aspect_ratio" "string" "e.g. 1:1, 16:9 (banana models only)."))))

(defparameter *multiview-schema*
  (tool-schema
   "Make front, left, back and right views of one subject from a single image (10 credits), or edit an earlier set view by view (5 per edited view). Multiview to 3D gives better shapes than one image: pass the task id as tripo_generate_model views."
   '(("image" "string" "The subject, on a clean background.")
     ("input" "string" "For edits: the task id of an earlier tripo_multiview.")
     ("edits" "object" "For edits: view -> instruction, e.g. {\"back\": \"add a logo\"}."))))

(defparameter *texture-schema*
  (tool-schema
   "Re-texture an existing model (10/20/30 credits for standard/detailed/extreme, +5 with style_image). Guide it with a text prompt or a reference image."
   '(("input" "string" "The model to re-texture." t)
     ("prompt" "string" "Texture description, e.g. 'worn leather'.")
     ("image" "string" "Reference image instead of a prompt.")
     ("style_image" "string" "Style reference that goes with prompt.")
     ("texture_quality" "string" "Default standard." nil ("fast" "standard" "detailed" "extreme"))
     ("model" "string" "Texture model; v3.0 default for v3.x geometry, v2.5 for v2.5 geometry." nil
      ("v3.5-20260815" "v3.0-20250812" "v2.5-20250123"))
     ("pbr" "boolean")
     ("texture_seed" "integer")
     ("part_names" "array" "Texture only these parts (names from a tripo_parts segment task)."))))

(defparameter *parts-schema*
  (tool-schema
   "Split a model into named parts (action segment, 40 credits), then optionally rebuild the geometry hidden where parts met (action complete, 50). The part names come back in tripo_task's result; use them with tripo_texture part_names (repaint one part) or tripo_convert part_names (export some parts)."
   '(("action" "string" "Default segment." nil ("segment" "complete"))
     ("input" "string" "segment: the model, complete: the segment task id." t)
     ("model" "string" "Segment model: v1.0 geometric (default), v2.0 semantic, labelled parts." nil
      ("v1.0-20250506" "v2.0-20260430"))
     ("granularity" "string" "Segment v2.0 only: how finely to split." nil ("simple" "balanced" "detailed"))
     ("part_names" "array" "complete: only these parts. Default all.")
     ("completion_mode" "string" "complete: ai_completion (default, rebuilds shape) or quick_cap (seals holes)." nil
      ("ai_completion" "quick_cap")))))

(defparameter *uv-schema*
  (tool-schema
   "Unwrap a model's UVs again and re-bake its textures, rig and animation onto the new layout (20 credits). The result includes a UV layout image. The model must be under 80,000 triangles: run tripo_retopology first if it is heavier."
   '(("input" "string" "The model." t)
     ("bake" "boolean" "Default true. false returns geometry with UVs only."))))

(defparameter *rig-check-schema*
  (tool-schema
   "Free. Check whether a model can be rigged and which rig_type fits. Answers a task_id; tripo_task shows riggable and rig_type when it finishes."
   '(("input" "string" "The model." t))))

(defparameter *rig-schema*
  (tool-schema
   "Add a skeleton to a model (25 credits). Run tripo_rig_check first and pass its rig_type."
   '(("input" "string" "The model." t)
     ("rig_type" "string" "From tripo_rig_check. Default biped." nil
      ("biped" "quadruped" "hexapod" "octopod" "avian" "serpentine" "aquatic" "others"))
     ("model" "string" "Pick it from the animation you will apply: v1.0 for dances and other preset:biped:* clips; v3.0 for preset:walk/idle/run and engine specs (no dances); v2.5 for creatures. Chosen from rig_type/spec when omitted." nil
      ("v1.0-20240301" "v2.5-20260210" "v3.0-20260909"))
     ("spec" "string" "Bone naming for bipeds; unity/unreal/vrm/actorcore use v3.0." nil
      ("tripo" "mixamo" "actorcore" "unity" "unreal" "vrm"))
     ("out_format" "string" "Default glb." nil ("glb" "fbx")))))

(defparameter *animate-schema*
  (tool-schema
   "Apply preset animations to a rigged model (10 credits each, up to 5 per task). The rig model decides which presets exist: rig v1.0 bipeds use preset:biped:* (dances); rig v3.0 humanoids use preset:idle, walk, run, jump, climb, dive, slash, shoot, hurt, fall, turn; rig v2.5 creatures use preset:quadruped:walk, hexapod:walk, octopod:walk, serpentine:march, aquatic:march."
   '(("input" "string" "task_id of a finished tripo_rig task." t)
     ("animations" "array" "Preset ids, e.g. [\"preset:idle\", \"preset:walk\"]." t)
     ("out_format" "string" "Default glb." nil ("glb" "fbx"))
     ("animate_in_place" "boolean" "No root motion (good for engines).")
     ("export_with_geometry" "boolean" "Default true."))))

(defparameter *retopology-schema*
  (tool-schema
   "Rebuild a model at a lower polycount with clean topology (v3.0: 35 credits, 40 with quad; v1.0/v2.0: 30)."
   '(("input" "string" "The model." t)
     ("face_limit" "integer" "Target face count." t)
     ("quad" "boolean")
     ("smart_uv" "boolean" "Also unwrap UVs (v3.0 only, same price).")
     ("model" "string" "Default v3.0 (newest)." nil ("v1.0" "v2.0" "v3.0")))))

(defparameter *convert-schema*
  (tool-schema
   "Convert a model's format (5 credits; 10 when any processing option is set). FBX for Unity/Unreal, USDZ for Apple AR, STL/3MF/OBJ for printing, GLTF for web."
   '(("input" "string" "The model." t)
     ("format" "string" "Target format." t ("GLTF" "FBX" "USDZ" "OBJ" "STL" "3MF"))
     ("face_limit" "integer")
     ("quad" "boolean")
     ("texture_size" "integer" "Texture pixels, default 4096.")
     ("texture_format" "string" "JPEG (default), PNG, WEBP, ...")
     ("scale_factor" "number")
     ("pivot_to_center_bottom" "boolean")
     ("flatten_bottom" "boolean" "Flat base, useful for printing.")
     ("export_orientation" "string" "Forward axis." nil ("+x" "-x" "+y" "-y"))
     ("fbx_preset" "string" "FBX target." nil ("blender" "3dsmax" "mixamo" "bake_scale"))
     ("with_animation" "boolean")
     ("animate_in_place" "boolean")
     ("part_names" "array" "Export only these parts (names from a tripo_parts segment task)."))))

(defparameter *task-schema*
  (tool-schema
   "Free. Get a Tripo task's status, progress, credits consumed and outputs. With wait=true (default) it polls until the task finishes or wait_seconds pass; a timeout leaves the task running, so call again with the same task_id rather than creating a new task."
   '(("task_id" "string" t)
     ("wait" "boolean" "Default true.")
     ("wait_seconds" "integer" "Default 120, max 300."))))

(defparameter *download-schema*
  (tool-schema
   "Free. Save a finished task's outputs (model file, preview render, and any other files) to disk and answer their paths. Output URLs expire, so download soon after a task succeeds."
   '(("task_id" "string" t)
     ("save_dir" "string" "Directory to save into, normally in the user's project. Default: the home's tripo/<task_id>/.")
     ("name" "string" "File name stem, e.g. 'treasure_chest'. Default: the task_id.")
     ("model_only" "boolean" "Skip previews and other extras."))))

(defparameter *balance-schema*
  (tool-schema
   "Free. Tripo credit balance (available and frozen in running tasks), optionally with recent per-task usage."
   '(("usage" "boolean" "Also list recent tasks and their cost."))))

;;; --- answers ---------------------------------------------------------------------

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the ERROR: line the model repairs from."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: tripo: ~a" condition)))))

(defun argument-text (args name)
  (arg-required-text args name))

(defun output-files (output)
  "OUTPUT's members that are signed URLs, as (KEY . URL) pairs, the model first."
  (when (hash-table-p output)
    (sort (loop for key being the hash-keys of output using (hash-value value)
                when (and (stringp value)
                          (or (uiop:string-prefix-p "https://" value)
                              (uiop:string-prefix-p "http://" value)))
                  collect (cons key value))
          (lambda (left right) (declare (ignore right)) (equal (car left) "model_url")))))

(defun describe-task (data)
  "One task's record as the lines the model reads."
  (let* ((id (nlk:json-value data :string "task_id"))
         (status (nlk:json-value data :string "status"))
         (progress (nlk:json-value data :integer "progress"))
         (credits (nlk:json-value data :integer "credits_consumed"))
         (output (nlk:json-value data :any "output"))
         (files (output-files output))
         (preview (nlk:json-value output :string "rendered_image_url"))
         (error (nlk:json-value data :string "error_message")))
    (with-output-to-string (out)
      (format out "~a: ~a~@[ (~d%)~]~@[, ~d credits consumed~]" id status progress credits)
      (when error (format out "~%error: ~a" error))
      (when files (format out "~%files: ~{~a~^, ~}" (mapcar #'cdr files)))
      (when preview (format out "~%preview: ~a" preview))
      (format out "~%~a"
              (cond ((equal status "success")
                     "next: tripo_download to save the files; reuse this task_id for texture, rig or convert.")
                    ((not (member status +terminal+ :test #'equal))
                     "still running: call tripo_task again with this task_id; do not create a new task.")
                    (t "the task did not succeed; Tripo refunds the credits it froze for a failed task."))))))

;;; --- the eleven that create a task ---------------------------------------------------

(defun create (endpoint body)
  "POST BODY, replacing its upload markers first; answer the task id it made (client.py:187)."
  (let* ((data (call :post endpoint :body (resolve-uploads body)))
         (task-id (nlk:json-value data :string "task_id")))
    (or task-id
        (fail "Tripo accepted the request but answered no task_id"))))

(defun create-tool (tool)
  "A handler for one of the creating tools: its plan, the request, the answer."
  (lambda (args)
    (answering
      (let* ((build (or (cdr (assoc tool *builders* :test #'equal))
                        (fail "no builder for ~a" tool)))
             (plan (funcall build (or args (make-hash-table :test #'equal))))
             (task-id (create (plan-endpoint plan) (plan-body plan))))
        (format nil "~a: ~a~%~@[~{~a~%~}~]Next: tripo_task with this task_id to wait for it, then tripo_download to save the files."
                task-id
                (format nil "~a, ~a" (plan-summary plan) (cost-text (plan-credits plan)))
                (plan-notes plan))))))

;;; --- the three that read ----------------------------------------------------------------

(defun tool-task (args)
  (answering
    (let* ((id (argument-text args "task_id"))
           (wait (arg-present-true args "wait" t))
           (seconds (min (or (arg-integer args "wait_seconds" 0 300) 120) 300))
           (data (if wait (wait-for-task id seconds) (get-task id))))
      (unless data (fail "Tripo answered nothing for task ~a" id))
      (describe-task data))))

(defun safe-name (text)
  (string-downcase
   (with-output-to-string (out)
     (loop for character across text
           do (write-char (if (or (alphanumericp character) (find character "-._"))
                              character
                              #\_)
                          out)))))

(defun url-extension (url fallback)
  (let* ((clean (subseq url 0 (or (position #\? url) (length url))))
         (dot (position #\. clean :from-end t))
         (slash (position #\/ clean :from-end t)))
    (if (and dot (> dot (or slash -1)) (<= (- (length clean) dot) 6))
        (subseq clean dot)
        fallback)))

(defun free-stem (directory stem tails)
  "STEM, or STEM (2), ... so no <stem><tail> exists yet (client.py:74)."
  (loop for candidate = stem then (format nil "~a (~d)" stem count)
        for count from 2
        unless (some (lambda (tail) (probe-file (merge-pathnames (concatenate 'string candidate tail)
                                                                 directory)))
                     tails)
          return candidate))

(defun save-directory (args task-id)
  (let ((given (arg-text args "save_dir"))
        (configured (setting :download-folder)))
    (cond
      (given
       (let ((path (if (uiop:string-prefix-p "~/" given)
                       (merge-pathnames (subseq given 2) (user-homedir-pathname))
                       (uiop:parse-unix-namestring given))))
         (uiop:ensure-directory-pathname
          (if (uiop:absolute-pathname-p path) path (merge-pathnames path (uiop:getcwd))))))
      ((and configured (plusp (length configured)))
       (uiop:ensure-directory-pathname configured))
      (t (uiop:ensure-directory-pathname (nlk:home (format nil "tripo/~a" (safe-name task-id))))))))

(defun tool-download (args)
  (answering
    (let* ((id (argument-text args "task_id"))
           (data (get-task id))
           (status (nlk:json-value data :string "status")))
      (unless (equal status "success")
        (fail "task ~a is ~a, not success; nothing to download yet" id status))
      (let* ((output (nlk:json-value data :any "output"))
             (urls (output-files output))
             (directory (save-directory args id))
             (stem (safe-name (or (arg-text args "name") id))))
        (when (arg-present-true args "model_only" nil)
          (setf urls (remove-if-not (lambda (pair) (equal (car pair) "model_url")) urls)))
        (unless urls (fail "the task has no downloadable files"))
        (let* ((tails (loop for (key . url) in urls
                            collect (if (equal key "model_url")
                                        (url-extension url ".glb")
                                        (format nil "_~a~a" key (url-extension url ".bin")))))
               (stem (free-stem directory stem tails))
               (saved '()))
          (loop for (key . url) in urls
                for tail in tails
                do (let ((target (merge-pathnames (concatenate 'string stem tail) directory)))
                     (push (format nil "~a (~:d bytes)" (namestring target) (fetch url target)) saved)))
          (format nil "saved ~d file~:p for ~a:~%~{~a~^~%~}"
                  (length saved) id (reverse saved)))))))

(defun tool-balance (args)
  (answering
    (let ((data (balance)))
      (with-output-to-string (out)
        (format out "tripo credits: ~a available~@[, ~a frozen in running tasks~]"
                (or (nlk:json-value data :number "balance") "?")
                (nlk:json-value data :number "frozen"))
        (when (arg-present-true args "usage" nil)
          (let ((rows (usage)))
            (format out "~%recent tasks:~:[ none~;~{~%  ~a~}~]"
                    (plusp (length rows))
                    (loop for row across (subseq rows 0 (min 20 (length rows)))
                          collect (format nil "~a ~a ~@[~a credits~]"
                                          (or (nlk:json-value row :string "task_id") "?")
                                          (or (nlk:json-value row :string "type") "task")
                                          (nlk:json-value row :number "credits_consumed"))))))))))

;;; --- the manual and /tripo --------------------------------------------------------------

(defparameter +primer+
  "Tripo turns a description, a photo or a few views into a 3D model, then textures, rigs,
animates, retopologises and converts it. (help :tripo) is this page; the tools are the
tripo_* set.
  tripo_generate_model {prompt | image | views, ...}   10-50 credits: text, image or multiview to 3D
  tripo_generate_image {prompt, image?, ...}            5-10: a concept image, or an edit of one
  tripo_multiview {image} / {input, edits}             10, or 5 a view: four views, or changes to them
  tripo_texture {input, prompt?|image?, ...}            10-30: re-texture a model
  tripo_rig_check {input}                              free: can this model be rigged, and how
  tripo_rig {input, rig_type?}                         25: add a skeleton
  tripo_animate {input, animations}                     10 each: apply preset clips to a rig
  tripo_retopology {input, face_limit}                 30-40: a lower polycount with clean topology
  tripo_parts {action, input}                          40 split into parts, 50 complete them
  tripo_uv {input}                                      20: unwrap UVs and re-bake
  tripo_convert {input, format}                         5, or 10 with processing: GLTF, FBX, USDZ, OBJ, STL, 3MF
  tripo_task {task_id, wait?}                           free: status, progress, credits, outputs
  tripo_download {task_id, save_dir?, name?}            free: save the finished files; never overwrites
  tripo_balance {usage?}                                free: credits available and frozen
Every creating call answers a task_id at once and states its estimated cost from Tripo's
published list; say what it will cost before you call it, since the operator pays. Then
call tripo_task with that id -- a timeout leaves the task running, so call again rather
than making a second one -- and tripo_download once it succeeds, because output URLs
expire. An input is a task id, a file_token, a public https URL, or a local file path,
which is uploaded to Tripo (at most 50 MB). The tripo section of config.jsonc names the
key (api_key, api_key_env or TRIPO_API_KEY), the API root, and where downloads go."
  "What (help :tripo) answers while the cell runs.")

(defun run-command (args session-id)
  "One /tripo invocation: the balance, and with 'usage' the recent tasks too."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (if (plusp (length text))
        (tool-balance (let ((args (make-hash-table :test #'equal)))
                        (setf (gethash "usage" args) t)
                        args))
        (tool-balance (make-hash-table :test #'equal)))))

(defun check-config ()
  "Refuse a section that could reach no account, naming the member."
  (unless (api-key)
    (error 'nlk:config-refusal
           :detail "tripo: no key: set TRIPO_API_KEY, or \"api_key\" (or \"api_key_env\") in the tripo section; keys come from platform.tripo3d.ai"))
  (root))

;;; --- the cell ------------------------------------------------------------------------------

(nle:define-cell tripo
  (:section ("tripo")
    (:guide "set api_key (or api_key_env, or TRIPO_API_KEY in the environment) from platform.tripo3d.ai; base_url is Tripo's v3 API unless a local server answers; next step: /tripo shows the balance")
    (:one-of "api_key" "api_key_env")
    ("api_key" :secret
     :doc "the Tripo key, from platform.tripo3d.ai (it starts with tsk_)")
    ("api_key_env" :env
     :doc "the environment variable holding the Tripo key, when it is not written here (TRIPO_API_KEY is read either way)")
    ("base_url" :string :default +root+
     :doc "the v3 API root: https://openapi.tripo3d.ai/v3 unless another answers")
    ("download_folder" :path
     :doc "where tripo_download saves when a call names no save_dir; empty means the home's tripo folder")
    ("timeout_seconds" :integer :default 60 :min 1
     :doc "one API request's ceiling, in seconds"))
  (:help :tripo "tripo: text, image or multiview to 3D, then texture, rig, animate, retopologize, convert; tripo_task, tripo_download, tripo_balance" +primer+)
  (:command "tripo" 'run-command
            :description "Show the Tripo balance, or recent usage"
            :argument-hint "usage")
  (:tool "tripo_generate_model" *generate-model-schema* (create-tool "tripo_generate_model"))
  (:tool "tripo_generate_image" *generate-image-schema* (create-tool "tripo_generate_image"))
  (:tool "tripo_texture" *texture-schema* (create-tool "tripo_texture"))
  (:tool "tripo_rig_check" *rig-check-schema* (create-tool "tripo_rig_check"))
  (:tool "tripo_rig" *rig-schema* (create-tool "tripo_rig"))
  (:tool "tripo_animate" *animate-schema* (create-tool "tripo_animate"))
  (:tool "tripo_retopology" *retopology-schema* (create-tool "tripo_retopology"))
  (:tool "tripo_convert" *convert-schema* (create-tool "tripo_convert"))
  (:tool "tripo_parts" *parts-schema* (create-tool "tripo_parts"))
  (:tool "tripo_uv" *uv-schema* (create-tool "tripo_uv"))
  (:tool "tripo_multiview" *multiview-schema* (create-tool "tripo_multiview"))
  (:tool "tripo_task" *task-schema* #'tool-task)
  (:tool "tripo_download" *download-schema* #'tool-download)
  (:tool "tripo_balance" *balance-schema* #'tool-balance)
  (:start (lambda () (check-config))))
