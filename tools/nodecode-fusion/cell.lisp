;;;; cell.lisp --- fusion: what this machine can call, and how to split the work.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of hermes-fusion; see NOTICE. The original was a Hermes
;;;; plugin: a `hermes fusion ...' CLI over four detection modules, a plan
;;;; packet validator, a model index, and an executor that spawned the Hermes
;;;; binary once per team member. This cell keeps the half a cell answers on its
;;;; own, offline: it reads the same harness configs, tiers what it finds with
;;;; the same name heuristics, builds the same Frontier -> Middle Manager ->
;;;; Frontier rosters, and validates the same v3/v4 plan packets. No network, no
;;;; subprocess, no model call of its own.

(defpackage #:nodecode-fusion
  (:use #:cl)
  (:export #:fusion-teams #:fusion-validate #:run-fusion))

(in-package #:nodecode-fusion)

(nlk:define-peripheral fusion :not-running t)

;;; --- the original's tiering heuristics -------------------------------------
;;; Carried over as the original wrote them: a name that says frontier wins, a
;;; name that says cheap loses, and a relay provider's default is cheap.

(defparameter +frontier-hints+
  '("opus" "claude-4" "sonnet-4" "gpt-5.6" "gpt-5.5" "gpt-5.2" "grok-4.5"
    "grok-4.6" "gemini-2.5-pro" "gemini-3" "o3" "o4" "thinking" "ultra"
    "max" "sol" "luna" "terra")
  "Substrings of a provider-and-model name that read frontier.")

(defparameter +cheap-hints+
  '("flash" "haiku" "mini" "lite" "nano" "fast" "deepseek-chat"
    "deepseek-reasoner" "deepseek-v4-flash" "glm-4-flash" "glm-5-flash"
    "qwen-turbo" "grok-4.1" "grok-4-fast" "gpt-5-mini" "gpt-5-flash")
  "Substrings that read cheap, when no frontier hint matched first.")

(defparameter +cheap-providers+ '("deepseek" "inferx" "venice" "zai" "glm" "groq")
  "Providers whose ambiguous model names still read cheap.")

(defun frontier-p (provider model)
  "Whether PROVIDER's MODEL is a frontier tier one, by the name alone."
  (let ((name (string-downcase (format nil "~a ~a" (or provider "") (or model "")))))
    (cond ((some (lambda (hint) (search hint name)) +frontier-hints+) t)
          ((some (lambda (hint) (search hint name)) +cheap-hints+) nil)
          (t (not (member (string-downcase (or provider "")) +cheap-providers+
                          :test #'string=))))))

(defun entry (harness provider model)
  "One detected (HARNESS, PROVIDER, MODEL), tiered."
  (list :harness harness
        :provider (or provider "")
        :model (or model "")
        :frontier (frontier-p provider model)))

(defparameter +harnesses+ '("hermes" "codex" "claude" "grok")
  "The harnesses the original read, and this cell reads.")

;;; --- reading this machine's configs ----------------------------------------

(defparameter *getenv* #'uiop:getenv
  "How a provider key in the environment is read; a test rebinds it.")

(defun key-present-p (name)
  "Whether this process carries a non-empty NAME in its environment."
  (let ((value (funcall *getenv* name)))
    (and (stringp value) (plusp (length (nlk:trimmed value))))))

(defun setting-text (key)
  "KEY's section value as a trimmed string, or NIL when it is unset."
  (let* ((value (ignore-errors (setting key)))
         (text (and value (string-trim " " (princ-to-string value)))))
    (and text (plusp (length text)) text)))

(defun folder (value fallback)
  "VALUE (or FALLBACK when unset) as a directory pathname, ~/ expanded."
  (let* ((text (or (and value (string-trim " " (princ-to-string value)))
                   (and (plusp (length fallback)) fallback)))
         (text (if (and text (plusp (length text))) text fallback)))
    (uiop:ensure-directory-pathname
     (if (and (> (length text) 1) (char= #\~ (char text 0)) (char= #\/ (char text 1)))
         (merge-pathnames (subseq text 2) (user-homedir-pathname))
         text))))

(defun slurp (path)
  "PATH's whole text, or NIL when there is no such file or it cannot be read."
  (when (and path (handler-case (probe-file path) (error () nil)))
    (handler-case (uiop:read-file-string path) (error () nil))))

(defun text-lines (text)
  (with-input-from-string (in text)
    (loop for line = (read-line in nil) while line collect line)))

(defun strip-comment (line marker)
  "LINE without a comment that starts at MARKER outside a string."
  (let ((in-string nil) (escaped nil))
    (dotimes (i (length line) line)
      (let ((ch (char line i)))
        (cond (escaped (setf escaped nil))
              ((char= ch #\\) (when in-string (setf escaped t)))
              ((char= ch #\") (setf in-string (not in-string)))
              ((and (not in-string) (char= ch marker)) (return (subseq line 0 i))))))))

(defun scalar (text)
  "TEXT as a config scalar: trimmed, one layer of quotes off, empty as NIL."
  (let ((text (string-trim " " (or text ""))))
    (when (plusp (length text))
      (if (and (>= (length text) 2)
               (char= #\" (char text 0))
               (char= #\" (char text (1- (length text)))))
          (subseq text 1 (1- (length text)))
          text))))

(defun yaml-pair (line)
  "LINE as (values KEY VALUE) for `key: value', a leading list dash and the
indentation ignored; NIL for a line that is not a pair."
  (let* ((body (string-trim " " (strip-comment line #\#)))
         (body (if (and (>= (length body) 2) (string= "- " (subseq body 0 2)))
                   (subseq body 2)
                   body))
         (colon (position #\: body)))
    (when (and colon (plusp colon))
      (let ((key (string-downcase (string-trim " " (subseq body 0 colon)))))
        (when (and (plusp (length key))
                   (every (lambda (ch) (or (alphanumericp ch) (find ch "-_"))) key))
          (values key (scalar (subseq body (1+ colon)))))))))

(defun toml-pair (line)
  "LINE as (values KEY VALUE) for `key = value'; NIL for anything else."
  (let* ((body (string-trim " " (strip-comment line #\#)))
         (equals (position #\= body)))
    (when (and equals (plusp equals))
      (values (string-downcase (string-trim " " (subseq body 0 equals)))
              (scalar (subseq body (1+ equals)))))))

(defun json-field (object key)
  "KEY's member of OBJECT, a JSON object; NIL when it has none."
  (and (hash-table-p object) (gethash key object)))

(defun json-list (value)
  "VALUE as a list: a JSON array either way it arrives; NIL for a scalar."
  (cond ((and (vectorp value) (not (stringp value))) (coerce value 'list))
        ((consp value) value)
        (t nil)))

(defun detect-codex ()
  "What this machine's Codex config names: its model, its subagent model, and
one entry per [model_providers.NAME] block."
  (let* ((home (folder (setting-text :codex-home) "~/.codex"))
         (text (slurp (merge-pathnames "config.toml" home)))
         (section nil) (top '()) (providers '()) (entries '()))
    (when text
      (dolist (line (text-lines text))
        (let ((line (string-trim " " (strip-comment line #\#))))
          (cond
            ((and (plusp (length line)) (char= #\[ (char line 0)))
             (setf section (string-downcase
                            (string-trim " []" (strip-comment line #\#)))))
            (t
             (multiple-value-bind (key value) (toml-pair line)
               (when key
                 (cond
                   (section
                    (when (and (uiop:string-prefix-p "model_providers." section)
                               (member key '("model" "default_model") :test #'string=)
                               value)
                      (push (cons (subseq section (length "model_providers.")) value)
                            providers)))
                   ((member key '("model" "model_provider" "default_subagent_model")
                            :test #'string=)
                    (when value (push (cons key value) top)))))))))))
    (let* ((provider (or (cdr (assoc "model_provider" top :test #'string=)) "openai"))
           (model (cdr (assoc "model" top :test #'string=)))
           (subagent (cdr (assoc "default_subagent_model" top :test #'string=))))
      (when model (push (entry "codex" provider model) entries))
      (when subagent (push (entry "codex" provider subagent) entries))
      (dolist (block (reverse providers))
        (push (entry "codex" (car block) (cdr block)) entries))
      (nreverse entries))))

(defun detect-hermes-yaml (text)
  "TEXT, a Hermes config.yaml, as entries: model.default with model.provider,
one per provider block under model:, and its fallback_providers pairs. The
original read this file with a YAML library; this reads the shapes it used."
  (let ((in-model nil) (base 0) (child-indent nil) (child nil)
        (default-model nil) (default-provider nil) (list-provider nil)
        (entries '()))
    (dolist (line (text-lines text))
      (let ((indent (- (length line) (length (string-left-trim " " line)))))
        (multiple-value-bind (key value) (yaml-pair line)
          (when key
            (progn
              (cond
                ((not in-model)
                 (when (and (string= key "model") (null value))
                   (setf in-model t base indent child nil child-indent nil)))
                ((<= indent base)
                 (setf in-model nil child nil child-indent nil)
                 (when (and (string= key "model") (null value))
                   (setf in-model t base indent)))
                (t
                 (when (or (null child-indent) (< indent child-indent))
                   (setf child-indent indent))
                 (cond
                   ((eq child :fallbacks)
                    (when (and (string= key "provider") value)
                      (setf list-provider value))
                    (when (and (string= key "model") value list-provider)
                      (push (entry "hermes" list-provider value) entries)))
                   ((= indent child-indent)
                    (cond ((member key '("default" "default_model") :test #'string=)
                           (when value (setf default-model value)))
                          ((member key '("provider" "default_provider") :test #'string=)
                           (when value (setf default-provider value)))
                          ((string= key "fallback_providers")
                           (setf child :fallbacks list-provider nil))
                          ((null value) (setf child key))))
                   ((stringp child)
                    (when (and (member key '("model" "default_model") :test #'string=)
                               value)
                      (push (entry "hermes" child value) entries)))))))))))
    (when (and default-model default-provider)
      (push (entry "hermes" default-provider default-model) entries))
    (nreverse entries)))

(defun detect-hermes ()
  "What this machine's Hermes home names under model:."
  (let* ((home (folder (setting-text :hermes-home) "~/.hermes"))
         (text (slurp (merge-pathnames "config.yaml" home))))
    (if text (detect-hermes-yaml text) '())))

(defun detect-claude ()
  "The model Claude's settings name; failing one, the standard pairing the
original assumed when an Anthropic key is set in this process."
  (let* ((home (folder (setting-text :claude-home) "~/.claude"))
         (text (slurp (merge-pathnames "settings.json" home)))
         (settings (and text (handler-case (shasht:read-json text) (error () nil))))
         (model (json-field settings "model")))
    (cond ((and (stringp model) (plusp (length (nlk:trimmed model))))
           (list (entry "claude" "anthropic" (nlk:trimmed model))))
          ((and (setting :count-env-keys) (key-present-p "ANTHROPIC_API_KEY"))
           (list (entry "claude" "anthropic" "claude-opus-4-8")
                 (entry "claude" "anthropic" "claude-haiku-4-5")))
          (t '()))))

(defun detect-grok ()
  "The grok pairing when Hermes' auth.json carries the xai sign-in, and the
frontier model when XAI_API_KEY is set in this process."
  (let* ((home (folder (setting-text :hermes-home) "~/.hermes"))
         (text (slurp (merge-pathnames "auth.json" home)))
         (auth (and text (handler-case (shasht:read-json text) (error () nil))))
         (entries '()))
    (when (json-field (json-field auth "providers") "xai-oauth")
      (push (entry "grok" "xai" "grok-4.6") entries)
      (push (entry "grok" "xai" "grok-4.1-fast") entries))
    (when (and (setting :count-env-keys) (key-present-p "XAI_API_KEY"))
      (push (entry "grok" "xai" "grok-4.6") entries))
    (nreverse entries)))

(defun wanted-harnesses (asked)
  "The harnesses to read: the one ASKED names, else the section's list, else
all four. A name outside the four is dropped, as the original's filter was."
  (let* ((asked (and asked (string-downcase (nlk:trimmed asked))))
         (names (if (and asked (plusp (length asked)))
                    (list asked)
                    (mapcar (lambda (name) (string-downcase (princ-to-string name)))
                            (or (setting :harnesses) '()))))
         (names (remove-duplicates (remove-if-not (lambda (name)
                                                    (member name +harnesses+ :test #'string=))
                                                  names)
                                   :test #'string=)))
    (or names +harnesses+)))

(defun detected-pool (&optional harness)
  "Every (harness, provider, model) the chosen harnesses name, deduped,
tiered, frontier first as the original sorted it."
  (let* ((wanted (wanted-harnesses harness))
         (all (append (when (member "hermes" wanted :test #'string=) (detect-hermes))
                      (when (member "codex" wanted :test #'string=) (detect-codex))
                      (when (member "claude" wanted :test #'string=) (detect-claude))
                      (when (member "grok" wanted :test #'string=) (detect-grok))))
         (seen '()) (pool '()))
    (dolist (one all)
      (let ((key (list (getf one :harness) (getf one :provider) (getf one :model))))
        (unless (member key seen :test #'equal)
          (push key seen)
          (push one pool))))
    (sort pool (lambda (a b)
                 (let ((fa (getf a :frontier)) (fb (getf b :frontier)))
                   (cond ((and fa (not fb)) t)
                         ((and fb (not fa)) nil)
                         (t (string< (getf a :harness) (getf b :harness))))))))

)

;;; --- the rosters the original built from whatever it found -------------------

(defun member-json (role model provider delegation thread-policy &optional trigger)
  (apply #'nlk:make-json-object
         (append (list "role" role "model" model "provider" provider
                       "delegation" delegation)
                 (when thread-policy (list "thread_policy" thread-policy))
                 (when trigger (list "trigger" trigger)))))

(defun roster-json (task-type middle-manager members consults)
  (nlk:make-json-object "task_type" task-type
                         "middle_manager" middle-manager
                         "members" (coerce members 'vector)
                         "consults" (coerce consults 'vector)))

(defun hash-keys (table)
  (loop for key being the hash-keys of table collect key))

(defun templates-path ()
  (setting-text :templates-file))

(defun checked-templates (path)
  "PATH's team_templates object, or a CONFIG-REFUSAL naming what is wrong."
  (unless (handler-case (probe-file path) (error () nil))
    (nlk:config-error "fusion.templates_file is ~a and no such file exists; name a JSON file holding {\"team_templates\": {name: roster}}, or leave it empty" path))
  (let ((object (handler-case (shasht:read-json (uiop:read-file-string path))
                  (error () nil))))
    (unless (hash-table-p object)
      (nlk:config-error "fusion.templates_file ~a is not a JSON object; it holds {\"team_templates\": {name: roster}}" path))
    (let ((templates (gethash "team_templates" object)))
      (unless (hash-table-p templates)
        (nlk:config-error "fusion.templates_file ~a has no team_templates object; it holds {\"team_templates\": {name: roster}}" path))
      templates)))

(defun build-rosters (pool)
  "POOL tiered into the original's five rosters, then whatever templates_file adds."
  (let* ((frontier (remove-if-not (lambda (one) (getf one :frontier)) pool))
         (cheap (remove-if-not (lambda (one) (not (getf one :frontier))) pool))
         (every (append frontier cheap))
         (rosters (make-hash-table :test #'equal)))
    (labels ((middle-manager ()
               (let ((pick (or (first cheap) (car (last every)))))
                 (if pick
                     (nlk:make-json-object "model" (getf pick :model)
                                            "provider" (getf pick :provider))
                     (nlk:make-json-object "model" "" "provider" ""
                                            "note" "no models detected"))))
             (workers (n)
               (let ((picked '()) (seen '()))
                 (dolist (one frontier (coerce (nreverse picked) 'vector))
                   (let ((key (list (getf one :provider) (getf one :model))))
                     (when (and (< (length picked) n)
                                (not (member key seen :test #'equal)))
                       (push key seen)
                       (push (member-json (format nil "worker-~d" (1+ (length picked)))
                                          (getf one :model) (getf one :provider)
                                          "delegate_task" "continuous")
                             picked))))))
             (consult ()
               (let ((one (first frontier)))
                 (if one
                     (vector (member-json "consult" (getf one :model) (getf one :provider)
                                           "clean_session" nil
                                           "final synthesis / decision"))
                     #()))))
      (setf (gethash "default" rosters) (roster-json "general" (middle-manager) (workers 2) (consult)))
      (setf (gethash "technical" rosters) (roster-json "technical" (middle-manager) (workers 2) (consult)))
      (setf (gethash "strategy" rosters) (roster-json "strategy" (middle-manager) (workers 3) #()))
      (setf (gethash "operations" rosters) (roster-json "operations" (middle-manager) (workers 1) #()))
      (setf (gethash "single" rosters) (roster-json "general" (middle-manager) #() #())))
    (let ((path (templates-path)))
      (when path
        (let ((templates (checked-templates path)))
          (dolist (name (hash-keys templates))
            (setf (gethash name rosters) (gethash name templates))))))
    rosters))

;;; --- what the model and the operator read ----------------------------------

(defun shown (value)
  "VALUE as text for a listing, or ? when it names nothing."
  (let ((text (and value (string-trim " " (princ-to-string value)))))
    (if (and text (plusp (length text))) text "?")))

(defun roster-member-text (members)
  (let ((rows (mapcar (lambda (member)
                        (format nil "~a=~a@~a"
                                (shown (json-field member "role"))
                                (shown (json-field member "model"))
                                (shown (json-field member "provider"))))
                      (json-list members))))
    (if rows (format nil "~{~a~^, ~}" rows) "(none)")))

(defun roster-lines (name roster)
  (let* ((mm (json-field roster "middle_manager"))
         (roles (mapcar (lambda (one) (shown (json-field one "role")))
                        (json-list (json-field roster "consults")))))
    (list (format nil "  ~a  [~a]" name (shown (json-field roster "task_type")))
          (format nil "    MM:       ~a via ~a"
                  (shown (json-field mm "model"))
                  (shown (json-field mm "provider")))
          (format nil "    members:  ~a" (roster-member-text (json-field roster "members")))
          (format nil "    consults: ~a" (if roles (format nil "~{~a~^, ~}" roles) "-")))))

(defun fleet-lines (pool asked)
  (let ((filter (let ((name (and asked (nlk:trimmed asked))))
                  (if (and name (plusp (length name))) name "all"))))
    (append
     (list (format nil "Detected model fleet (harness filter: ~a):" filter))
     (if pool
         (mapcar (lambda (one)
                   (format nil "  [~a] ~a:~a~:[ (cheap)~; (frontier)~]"
                           (getf one :harness) (getf one :provider) (getf one :model)
                           (getf one :frontier)))
                 pool)
         (list "  none: no config under the folders this cell reads names a model; name one in the fusion section, or a templates_file with rosters of your own")))))

(defun teams-text (harness template)
  "The fleet, the rosters, or one roster's JSON."
  (let* ((pool (detected-pool harness))
         (rosters (build-rosters pool))
         (name (and template (string-downcase (nlk:trimmed template)))))
    (cond
      ((and name (plusp (length name)))
       (let ((roster (gethash name rosters)))
         (if roster
             (format nil "roster ~a: ~a" name (shasht:write-json roster nil))
             (format nil "unknown template ~a; this machine has: ~{~a~^, ~}"
                     template (sort (hash-keys rosters) #'string<)))))
      (t
       (format nil "~{~a~%~}"
               (append (fleet-lines pool harness)
                       (list "" "Fusion rosters (Frontier -> Middle Manager -> Frontier):" "")
                       (mapcan (lambda (key) (roster-lines key (gethash key rosters)))
                               (sort (hash-keys rosters) #'string<))
                       (list "")
                       (list (if (templates-path)
                                 "Rosters your templates_file adds are listed with these."
                                 "Override or add a roster in fusion.templates_file ({\"team_templates\": {name: roster}})."))))))))

;;; --- the plan packet validator ---------------------------------------------
;;; The original's validate_fusion_packet.py, rule for rule.

(defparameter +required-fields+
  '(("fusion_version" :string) ("task_type" :string) ("lead_model" :string)
    ("lead_provider" :string) ("judgment_to_retain" :array)
    ("model_selection_rationale" :string) ("phases" :array) ("inputs" :array)
    ("deliverables" :array) ("scope_in" :array) ("scope_out" :array)
    ("constraints" :array) ("evidence_requirements" :array)
    ("verification" :array) ("stop_conditions" :array) ("max_retries" :integer))
  "Every packet's required fields, and the type each must arrive as.")

(defparameter +task-types+
  '("strategy" "personal" "explicit" "research" "document" "operations"
    "monitoring" "technical" "direct"))

(defparameter +delegations+ '("delegate_task" "kanban_handoff" "clean_session"))
(defparameter +thread-policies+ '("continuous" "fresh_per_item"))
(defparameter +blend-types+ '("layer" "parallel" "amplify" "audit" "debate"))

(defun json-kind (value)
  (cond ((stringp value) :string)
        ((eq value t) :boolean)
        ((null value) :null)
        ((hash-table-p value) :object)
        ((vectorp value) :array)
        ((integerp value) :integer)
        ((realp value) :number)
        ((consp value) :array)
        (t :other)))

(defun kind-is-p (value want)
  (eq (json-kind value)
      (case want (:integer :integer) (:string :string) (t want))))

(defun parse-json-object (text)
  (let ((object (handler-case (shasht:read-json text) (error () nil))))
    (and (hash-table-p object) object)))

(defun balanced-object (text start)
  "TEXT's JSON object beginning at START, as a substring, or NIL."
  (let ((depth 0) (in-string nil) (escaped nil))
    (loop for index from start below (length text)
          for ch = (char text index)
          do (cond (escaped (setf escaped nil))
                   ((char= ch #\\) (when in-string (setf escaped t)))
                   ((char= ch #\") (setf in-string (not in-string)))
                   (in-string nil)
                   ((char= ch #\{) (incf depth))
                   ((char= ch #\})
                    (decf depth)
                    (when (zerop depth) (return (subseq text start (1+ index))))))
          finally (return nil))))

(defun extract-packet (text)
  "The first JSON object in TEXT: a ```json fence first, then balanced braces
outside strings, the way the original's extractor found one."
  (block found
    (let ((fence (search "```json" text)))
      (when fence
        (let ((end (search "```" text :start2 (+ fence 7))))
          (when end
            (let ((object (parse-json-object (subseq text (+ fence 7) end))))
              (when object (return-from found object)))))))
    (dotimes (index (length text))
      (when (char= #\{ (char text index))
        (let ((candidate (balanced-object text index)))
          (when candidate
            (let ((object (parse-json-object candidate)))
              (when object (return-from found object)))))))
    nil))

(defun validate-packet (packet)
  "PACKET's errors as the original found them; an empty list is a valid plan."
  (let ((errors '()))
    (flet ((complain (control &rest args) (push (apply #'format nil control args) errors)))
      (dolist (spec +required-fields+)
        (destructuring-bind (field want) spec
          (multiple-value-bind (value present) (gethash field packet)
            (cond ((not present) (complain "missing field: ~a" field))
                  ((not (kind-is-p value want))
                   (complain "~a: expected ~(~a~), got ~(~a~)"
                             field want (json-kind value)))))))
      (unless errors
        (let ((version (gethash "fusion_version" packet)))
          (unless (member version '("3.0" "4.0") :test #'string=)
            (complain "fusion_version must be '3.0' or '4.0'"))
          (when (string= version "4.0")
            (check-team packet #'complain))
          (let ((task-type (gethash "task_type" packet)))
            (unless (member task-type +task-types+ :test #'string=)
              (complain "task_type must be one of: ~{~a~^, ~}"
                        (sort (copy-list +task-types+) #'string<))))
          (check-phases packet #'complain)
          (let ((moa (gethash "moa_blend" packet)))
            (when (hash-table-p moa) (check-blend moa #'complain)))
          (dolist (field '("judgment_to_retain" "deliverables" "scope_in" "constraints"
                            "evidence_requirements" "verification" "stop_conditions"))
            (when (null (json-list (gethash field packet)))
              (complain "~a must not be empty" field)))
          (let ((retries (gethash "max_retries" packet)))
            (unless (and (integerp retries) (<= 1 retries 4))
              (complain "max_retries must be between 1 and 4")))))
      (nreverse errors))))

(defun check-team (packet complain)
  "A v4 packet's team: roster-level model pinning is the gate."
  (let ((team (gethash "team" packet)))
    (cond
      ((not (hash-table-p team)) (funcall complain "v4 packet requires a team object"))
      (t
       (dolist (field '("template" "middle_manager" "members"))
         (unless (nth-value 1 (gethash field team))
           (funcall complain "team missing field: ~a" field)))
       (let ((mm (gethash "middle_manager" team)))
         (if (hash-table-p mm)
             (dolist (field '("model" "provider"))
               (unless (json-field mm field)
                 (funcall complain "team.middle_manager missing field: ~a" field)))
             (funcall complain "team.middle_manager must be an object with model+provider")))
       (let ((members (json-list (gethash "members" team))))
         (cond
           ((null members) (funcall complain "team.members must be a non-empty list"))
           ((> (length members) 4)
            (funcall complain "team.members has ~d workers (max 4 - keep teams small)"
                     (length members)))
           (t
            (loop for member in members
                  for index from 0
                  do (cond
                       ((not (hash-table-p member))
                        (funcall complain "team.members[~d] must be an object" index))
                       (t
                        (dolist (field '("role" "model" "provider" "delegation"))
                          (unless (json-field member field)
                            (funcall complain "team.members[~d] missing field: ~a" index field)))
                        (unless (member (json-field member "delegation") +delegations+
                                        :test #'string=)
                          (funcall complain
                                   "team.members[~d].delegation must be delegate_task|kanban_handoff|clean_session"
                                   index))
                        (let ((policy (json-field member "thread_policy")))
                          (when (and policy (not (member policy +thread-policies+
                                                        :test #'string=)))
                            (funcall complain
                                     "team.members[~d].thread_policy must be 'continuous' or 'fresh_per_item'"
                                     index)))))))))
       (let ((contract (gethash "escalation_contract" team)))
         (when (and contract (not (hash-table-p contract)))
           (funcall complain "team.escalation_contract must be an object")))))))

(defun check-phases (packet complain)
  (let ((phases (json-list (gethash "phases" packet))))
    (if (null phases)
        (funcall complain "phases must contain at least one phase")
        (loop for phase in phases
              for index from 0
              do (cond
                   ((not (hash-table-p phase))
                    (funcall complain "phases[~d] must be an object" index))
                   (t
                    (dolist (field '("phase" "model" "provider" "model_reason"
                                     "max_turns" "output"))
                      (unless (nth-value 1 (gethash field phase))
                        (funcall complain "phases[~d] missing field: ~a" index field)))
                    (multiple-value-bind (tools present) (gethash "tools" phase)
                      (when (and present tools (null (json-list tools)))
                        (funcall complain "phases[~d].tools must be a list" index)))))))))

(defun check-blend (blend complain)
  (dolist (field '("blend_type" "aggregator_model" "aggregator_provider" "layers"))
    (unless (nth-value 1 (gethash field blend))
      (funcall complain "moa_blend missing field: ~a" field)))
  (unless (member (gethash "blend_type" blend) +blend-types+ :test #'string=)
    (funcall complain "moa_blend.blend_type must be one of: ~{~a~^, ~}" +blend-types+))
  (let ((layers (json-list (gethash "layers" blend))))
    (if (null layers)
        (funcall complain "moa_blend.layers must be a non-empty list")
        (loop for layer in layers
              for index from 0
              do (cond
                   ((not (hash-table-p layer))
                    (funcall complain "moa_blend.layers[~d] must be an object" index))
                   (t (dolist (field '("layer" "purpose" "model_1" "provider_1" "output"))
                        (unless (nth-value 1 (gethash field layer))
                          (funcall complain "moa_blend.layers[~d] missing field: ~a"
                                   index field))))))))
  (let ((synthesis (gethash "synthesis" blend)))
    (cond ((not (nth-value 1 (gethash "synthesis" blend)))
           (funcall complain "moa_blend missing synthesis block"))
          ((not (hash-table-p synthesis))
           (funcall complain "moa_blend.synthesis must be an object"))
          (t (dolist (field '("model" "provider" "instruction"))
               (unless (nth-value 1 (gethash field synthesis))
                 (funcall complain "moa_blend.synthesis missing field: ~a" field)))))))

(defun valid-text (packet)
  (let* ((phases (json-list (gethash "phases" packet)))
         (team (gethash "team" packet))
         (mm (json-field team "middle_manager"))
         (workers (length (json-list (json-field team "members")))))
    (format nil "VALID plan: version ~a, task_type ~a, ~d phase~:p~@[, middle manager ~a via ~a, ~d worker~:p~]."
            (gethash "fusion_version" packet)
            (gethash "task_type" packet)
            (length phases)
            (json-field mm "model")
            (json-field mm "provider")
            (if (plusp workers) workers nil))))

(defun validate-text (text)
  "TEXT, a packet or output holding one, answered as VALID or INVALID and why."
  (let ((packet (or (parse-json-object (or text "")) (extract-packet (or text "")))))
    (cond
      ((null packet)
       "INVALID: no JSON object found in the text; pass the packet itself, or model output with one JSON object in it")
      (t (let ((errors (validate-packet packet)))
           (if errors
               (format nil "INVALID~%~{- ~a~%~}" errors)
               (valid-text packet)))))))

;;; --- the two boundaries ----------------------------------------------------

(nlk:define-contract teams-request
  (:doc "Show the models the agent harnesses on this machine have installed,
tiered, and the Fusion rosters built from them.")
  ("harness" :string :default ""
   :doc "read one harness only: hermes, codex, claude or grok; empty reads every one present")
  ("template" :string :default ""
   :doc "one roster's JSON: default, technical, strategy, operations, single, or one a templates_file adds; empty lists them all"))

(nlk:define-contract validate-request
  (:doc "Validate a Fusion plan packet: pass the packet's JSON, or raw planner
output with the packet somewhere inside it.")
  ("packet" :text :required t
   :doc "the packet JSON, or model output containing one JSON object"))

(defun fusion-teams (request)
  (teams-text (teams-request-harness request) (teams-request-template request)))

(defun fusion-validate (request)
  (validate-text (validate-request-packet request)))

(defun run-fusion (args session-id)
  "One /fusion invocation: no argument lists the fleet and the rosters, a name
shows that one roster's JSON."
  (declare (ignore session-id))
  (teams-text "" (or args "")))

;;; --- the manual ------------------------------------------------------------

(defparameter +manual+
  "What the agent harnesses on this machine can call, as the fusion plugin read
them: Hermes' config.yaml and auth.json, Codex' config.toml, Claude's
settings.json. Each (provider, model) is tiered frontier or cheap by the
original's name heuristics, and composed into Frontier -> Middle Manager ->
Frontier rosters: a cheap middle manager plans and delegates, frontier workers
implement, a frontier consult owns the final decision.

Read it three ways:
  the fusion_teams tool       the fleet, then every roster's lines; give it
      harness=codex to read one harness, or template=technical for one
      roster's JSON.
  the fusion_validate tool    a plan packet's JSON (or output with one in it):
      VALID, or INVALID and every rule it broke.
  /fusion [TEMPLATE]          the same listing, from the operator's side.

Nothing is required: with no settings the cell reads ~/.hermes, ~/.codex and
~/.claude. fusion.harnesses narrows which are read, the three _home members
point at a config kept elsewhere, count_env_keys counts a provider whose key is
set in the environment, and fusion.templates_file is a JSON file holding
{\"team_templates\": {name: roster}} merged over the detected rosters.
Nothing leaves the machine: no network, no subprocess, no model call of its own."
  "What (help :fusion) answers while the cell runs.")

(defun checked-settings (values table)
  "The section's values with templates_file read once: a file it cannot use
refuses at start, never mid-turn."
  (declare (ignore table))
  (let ((path (getf values :templates-file)))
    (when (and path (plusp (length (string-trim " " (princ-to-string path)))))
      (checked-templates path)))
  values)

(nle:define-cell fusion
  (:section ("fusion")                                ; the config section, once, as data
    (:guide "nothing here is required: with no settings the cell reads the harness configs it finds under your home; name a folder to read one kept elsewhere; templates_file adds a roster of your own")
    ("harnesses" :list
     :doc "which configs to read: hermes, codex, claude, grok; empty reads every one present")
    ("hermes_home" :directory :default "~/.hermes"
     :doc "the folder holding Hermes' config.yaml and auth.json")
    ("codex_home" :directory :default "~/.codex"
     :doc "the folder holding Codex' config.toml")
    ("claude_home" :directory :default "~/.claude"
     :doc "the folder holding Claude's settings.json")
    ("count_env_keys" :boolean :default t
     :doc "count a provider whose API key is set in this process's environment, even when no config names a model")
    ("templates_file" :path
     :doc "a JSON file of {\"team_templates\": {name: roster}} merged over the detected rosters"))
  (:settings #'checked-settings)
  (:tool "fusion_teams" (nlk:contract 'teams-request) 'fusion-teams)
  (:tool "fusion_validate" (nlk:contract 'validate-request) 'fusion-validate)
  (:command "fusion" 'run-fusion
            :description "The models your harnesses have, tiered into Fusion rosters"
            :argument-hint "[TEMPLATE]")
  (:help :fusion
         "fusion: what the agent harnesses on this machine can call, tiered into Frontier / Middle Manager / Frontier rosters"
         +manual+))
