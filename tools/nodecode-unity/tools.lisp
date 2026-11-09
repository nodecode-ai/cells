;;;; tools.lisp --- the four tools, the plan store, and the path check.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin's nineteen unity_* tools were one door to MCP for Unity's own
;;;; tools, plus three that made a change reviewable: plan_batch, apply_plan
;;;; and confirm_risky_plan. This cell keeps the four things a model needs --
;;;; the tool list, one call, a plan, and its apply -- because every MCP for
;;;; Unity operation is reachable through unity_call and the plan is what
;;;; spares the operator one approval per call.

(in-package #:nodecode-unity)

;;; --- what each tool takes -------------------------------------------------------

(nlk:define-contract tools-call
  (:doc "The editor's own tool list, so the model can pick a tool by name.")
  ("query" :text :doc "only tools whose name or description holds this word; all of them when empty"))

(nlk:define-contract editor-call
  (:doc "One tool of the open Unity editor, called with its own arguments.")
  ("tool" :text :required t
   :doc "the editor tool's name, as unity_tools lists it: manage_scene, manage_gameobject, manage_script, ...")
  ("arguments" :object
   :doc "the tool's arguments as one JSON object, e.g. {\"path\": \"Assets/Scenes/Main.unity\"}; leave it out when the tool takes none"))

(nlk:define-contract plan-step
  (:doc "One step of a change: an editor tool and its arguments.")
  ("tool" :text :required t :doc "the editor tool to run")
  ("arguments" :object :doc "its arguments as one JSON object; leave it out when the tool takes none"))

(nlk:define-contract plan-call
  (:doc "A whole change, as the steps that make it, shown before any of them runs.")
  ("steps" :array :of plan-step :required t :doc "the steps, in the order they run")
  ("note" :text :doc "one line saying what the batch does, shown at the top of the plan"))

(nlk:define-contract apply-call
  (:doc "Run the plan unity_plan answered, once, in the process that made it.")
  ("plan_id" :text :required t :doc "the plan_id unity_plan answered with"))

;;; --- what a step may name ---------------------------------------------------------

(defparameter +path-keys+
  '("path" "prefab_path" "uri" "destination" "clip_path" "controller_path" "material_path"
    "folder_path" "prefab_folder" "profile_path" "source_asset" "image_path" "scene_path"
    "output_folder" "log_file" "snapshot_path" "search_path" "object_path" "output_path"
    "output_dir" "source_path" "snapshot_a" "snapshot_b")
  "The argument names MCP for Unity hands to a file system call, read out of
its own source by the original plugin, so a path this cell checks is a path
something will use.")

(defparameter +secret-names+
  '("credentials.json" "serviceaccount.json" "id_rsa" "id_dsa" "id_ecdsa" "id_ed25519"
    ".netrc" ".npmrc" ".pypirc" ".git-credentials" "known_hosts" "authorized_keys")
  "File names that hold a key or a credential, which a step may not write.")

(defparameter +secret-suffixes+
  '(".pem" ".key" ".keystore" ".jks" ".p12" ".pfx" ".p8" ".kdbx" ".tfvars" ".tfstate"
    ".mobileprovision")
  "Suffixes of the same, refused the way the original refused them.")

(defparameter +protected-folders+ '("Assets" "Packages" "ProjectSettings")
  "Whole project folders a step may not delete in one move.")

(defun path-parts (value)
  (remove "" (uiop:split-string (substitute #\/ #\\ value) :separator "/") :test #'string=))

(defun refusal (tool key value why)
  (error 'unity-error
         :detail (format nil "~a: refused ~a ~s: ~a. Nothing was sent." tool key value why)))

(defun check-path (tool key value)
  "Refuse a VALUE the editor would take outside the project, out of sight, or
onto a key; the original refused the same four, before the plan was made."
  (let* ((text (substitute #\/ #\\ value))
         (parts (path-parts text))
         (base (string-downcase (or (car (last parts)) ""))))
    (when (member ".." parts :test #'string=)
      (refusal tool key value "it climbs out of the project with .."))
    (when (find-if (lambda (part) (and (plusp (length part)) (char= (char part 0) #\.)))
                   parts)
      (refusal tool key value "a hidden file or folder"))
    (when (or (member base +secret-names+ :test #'string=)
              (uiop:string-prefix-p ".env" base)
              (some (lambda (suffix) (uiop:string-suffix-p base suffix)) +secret-suffixes+))
      (refusal tool key value "a key or credential file"))
    (when (or (uiop:string-prefix-p "/" text) (uiop:string-prefix-p "~" text)
              (and (>= (length text) 2) (alpha-char-p (char text 0)) (char= (char text 1) #\:)))
      (refusal tool key value "an absolute path; name it inside Assets/ or Packages/"))))

(defun check-menu (tool arguments)
  "Refuse the editor's own quit from a menu: the plan cannot say what the
editor drops on its way out."
  (let ((menu (nlk:json-value arguments :string "menu_path")))
    (when (and menu (member (string-downcase menu)
                            '("file/quit" "file/exit" "unity/quit") :test #'string=))
      (error 'unity-error
             :detail (format nil "~a: refused menu_path ~s: closing the editor drops unsaved work. Nothing was sent."
                             tool menu)))))

(defun check-delete (tool arguments)
  "Refuse deleting a whole project folder: every file in it would go unnamed."
  (let ((path (nlk:json-value arguments :string "path"))
        (action (nlk:json-value arguments :string "action")))
    (when (and path (stringp action) (string-equal action "delete")
               (member (string-right-trim "/" path) +protected-folders+ :test #'string-equal))
      (error 'unity-error
             :detail (format nil "~a: refused deleting ~a, a whole project folder: name the files or folders inside it. Nothing was sent."
                             tool path)))))

(defun checked-arguments (step)
  "STEP's arguments as a decoded object, refused when a path in them is one
this cell will not send."
  (let ((tool (plan-step-tool step))
        (arguments (or (plan-step-arguments step) (nlk:json-object))))
    (unless (hash-table-p arguments)
      (error 'unity-error
             :detail (format nil "~a: arguments is not one JSON object" tool)))
    (dolist (key +path-keys+)
      (let ((value (nlk:json-value arguments :string key)))
        (when value (check-path tool key value))))
    (check-menu tool arguments)
    (check-delete tool arguments)
    arguments))

;;; --- the plan store --------------------------------------------------------------

(defparameter +plan-steps+ 24 "How many steps one plan may carry.")
(defparameter +step-chars+ 1200 "How much of one step's arguments the plan shows.")

(defvar *plans* (make-hash-table :test #'equal)
  "This process's plans, by id: a plist (:STEPS :CREATED). A plan runs once.")
(defvar *newest-plan* nil
  "The id of the newest plan: only it can run, as the original held it.")


(defun new-plan-id ()
  "An id nothing here has used: eight characters of a fresh digest."
  (loop for id = (subseq (nlk:sha256-text (format nil "~a-~a" (get-universal-time) (random 1000000000)))
                          7 15)
        unless (gethash id *plans*) return id))

(defun plan-ttl ()
  (or (setting :plan-ttl-minutes) 10))

(defun plan-line (index step)
  (format nil "~d. ~a ~a" index (plan-step-tool step)
          (clip (nlk:encode-json-object (or (plan-step-arguments step) (nlk:json-object)))
                +step-chars+)))

(defun format-plan (id note steps)
  "The plan in words: every step, and the one call that runs it."
  (with-output-to-string (out)
    (format out "plan ~a: ~d step~:p. Nothing has run yet.~%" id (length steps))
    (when (nonempty note) (format out "note: ~a~%" (nlk:trimmed note)))
    (loop for step in steps for index from 1 do (format out "~a~%" (plan-line index step)))
    (format out "Every path stays inside the project: no .., no hidden file, no key file, no absolute path.~%")
    (format out "Only the newest plan runs, once, within ~d minutes, in this process.~%" (plan-ttl))
    (format out "Show this plan, then call unity_apply with {\"plan_id\": \"~a\"}.~%" id)))

(defun check-plan (steps)
  (unless (plusp (length steps))
    (error 'unity-error :detail "a plan needs at least one step"))
  (when (> (length steps) +plan-steps+)
    (error 'unity-error :detail (format nil "~d steps is over this cell's ~d: split the work into two plans"
                                        (length steps) +plan-steps+))))

(defun tool-plan (call)
  "unity_plan: check the steps, keep them, answer the plan in words."
  (let ((steps (coerce (plan-call-steps call) 'list)))
    (check-plan steps)
    (mapcar #'checked-arguments steps)
    (let ((id (new-plan-id)))
      (setf (gethash id *plans*) (list :steps steps :created (get-universal-time))
            *newest-plan* id)
      (format-plan id (plan-call-note call) steps))))

(defun plan-expired-p (plan)
  (> (- (get-universal-time) (getf plan :created)) (* 60 (plan-ttl))))

(defun take-plan (id)
  "The plan ID, removed so it runs once, or a refusal naming why not."
  (let ((id (nlk:trimmed (or id ""))))
    (when (zerop (length id))
      (error 'unity-error :detail "unity_apply needs the plan_id unity_plan answered with"))
    (let ((plan (gethash id *plans*)))
      (unless plan
        (error 'unity-error
               :detail (format nil "plan ~a is unknown or already used: a plan runs once, in the process that made it. Call unity_plan again." id)))
      (unless (and *newest-plan* (string= id *newest-plan*))
        (error 'unity-error
               :detail (format nil "plan ~a was replaced by a newer plan: only the newest plan can run. Show the newest one, or plan again." id)))
      (when (plan-expired-p plan)
        (remhash id *plans*)
        (setf *newest-plan* nil)
        (error 'unity-error
               :detail (format nil "plan ~a is older than ~d minutes: plan again and show the new one." id (plan-ttl))))
      (remhash id *plans*)
      (setf *newest-plan* nil)
      plan)))

(defun run-plan (plan max-chars)
  "Run PLAN's steps in order, one at a time, and answer what happened. A step
that fails stops the plan; the later ones are not sent."
  (let* ((steps (getf plan :steps))
         (each (max 200 (floor max-chars (max 1 (length steps)))))
         (done '())
         (failure nil)
         (unrun 0))
    (loop for step in steps for index from 1
          do (handler-case
                 (push (format nil "~d. ~a: ~a" index (plan-step-tool step)
                               (clip (first-line (call-editor (plan-step-tool step)
                                                              (checked-arguments step))
                                                 400)
                                     each))
                       done)
               (unity-error (condition)
                 (setf failure (format nil "FAILED at step ~d of ~d: ~a"
                                       index (length steps) (unity-error-detail condition))
                       unrun (- (length steps) index))
                 (return))))
    (if failure
        (format nil "~a~%~{~a~%~}~d step~:p did not run."
                failure (reverse done) unrun)
        (format nil "done, ~d step~:p~%~{~a~%~}" (length done) (reverse done)))))

;;; --- the three doors ---------------------------------------------------------------

(defun max-chars ()
  "How much of one answer the model reads."
  (or (setting :max-chars) 6000))

(defun tool-matches-p (tool query)
  "Whether TOOL's name or description holds QUERY, case-insensitively."
  (or (zerop (length query))
      (search query (string-downcase (tool-name tool)))
      (search query (string-downcase (tool-description tool)))))

(defun tool-tools (call)
  "unity_tools: the tools the open editor advertises, and one line each."
  (let* ((query (string-downcase (nlk:trimmed (or (tools-call-query call) ""))))
         (tools (remove-if-not (lambda (tool) (tool-matches-p tool query)) (editor-tools))))
    (with-output-to-string (out)
      (format out "~a answers ~d tool~:p~@[, matching ~s~].~%"
              (endpoint) (length tools) (and (plusp (length query)) query))
      (loop for tool in tools
            do (format out "  ~a - ~a~%" (tool-name tool)
                       (first-line (tool-description tool) 200)))
      (format out "Call one with unity_call {\"tool\": \"<name>\", \"arguments\": {...}}.~%"))))

(defun arguments-object (tool arguments)
  "ARGUMENTS -- what the contract decoded for TOOL -- as a JSON object."
  (cond ((null arguments) (nlk:json-object))
        ((hash-table-p arguments) arguments)
        (t (error 'unity-error
                  :detail (format nil "~a: arguments is not one JSON object" tool)))))

(defun check-call (tool arguments)
  "Every path ARGUMENTS names, checked the way a plan's step is."
  (dolist (key +path-keys+)
    (let ((value (nlk:json-value arguments :string key)))
      (when value (check-path tool key value))))
  (check-menu tool arguments)
  (check-delete tool arguments)
  arguments)

(defun tool-call (call)
  "unity_call: one editor tool, its own arguments, its answer in its own words."
  (let ((tool (nlk:trimmed (or (editor-call-tool call) ""))))
    (when (zerop (length tool))
      (error 'unity-error :detail "unity_call needs a tool name: unity_tools lists them"))
    (let ((arguments (check-call tool (arguments-object tool (editor-call-arguments call)))))
      (clip (call-editor tool arguments) (max-chars)))))

(defun tool-apply (call)
  "unity_apply: the newest plan, once, step by step, and what each step answered."
  (run-plan (take-plan (apply-call-plan-id call)) (max-chars)))
