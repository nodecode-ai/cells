;;;; cell.lisp --- named presets for a session's model and its reasoning effort.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin sat on the harness gateway and rewrote the line a person
;;;; typed: `/model fast` became `/model <model> --provider <provider>
;;;; --reasoning <rung>`, from a table of aliases in its own settings. Nodecode's
;;;; pick is a /models line, and the gateway already owns what such a pick means
;;;; (SELECT-MODEL and SELECT-EFFORT): resolve the lane, check the rung against
;;;; that model's own ladder, pin the session, make it the default new sessions
;;;; start on, tell every attached shell. So the table is all this cell keeps,
;;;; and /preset -- or one tool call -- lands it.
;;;;
;;;; It fails the way the original did: a name the table does not carry, a
;;;; preset of the wrong shape, or a rung the model refuses is said out loud and
;;;; moves nothing else.

(defpackage #:nodecode-model-presets
  (:use #:cl)
  (:export #:start-cell #:reads-a-preset #:presets-tool #:model-presets-error))

(in-package #:nodecode-model-presets)

(nlk:define-peripheral model-presets :not-running t)

;;; --- what it says it does ------------------------------------------------------

(defparameter +primer+
  "Model presets are named picks: a preset is a model, the provider that serves it and, when
it matters, a reasoning effort, under one short name the operator writes in the
`model-presets' section of the shared config.
  /preset                the presets, as a picker; a row switches this session
  /preset list           their names, models, providers and efforts, one a line
  /preset status         what this session is pinned to now
  /preset fast           switch this session to the preset `fast'
  /preset fast --effort high --provider openrouter
                         the preset, with this one pick's overrides
A switch pins the session, makes the pair the default a session opened later starts on, and
applies from the next turn; a session already begun keeps its own model and effort. The
config's shape, one object a preset:
  \"model-presets\": {\"presets\": {\"fast\": {\"model\": \"claude-haiku-4.5\", \"provider\":
  \"anthropic\", \"effort\": \"low\"}}}
`aliases' is read instead of `presets' when the section carries the original's word, and
`reasoning_effort' is read instead of `effort'. The model tool `model_presets' does the
same from a turn: action list, or action switch with preset (and optional effort, provider).
ERROR: model-presets-error names the problem: no preset by that name, a preset of the wrong
shape, or the gateway's own refusal of a model or a rung."
  "What (help :model-presets) answers while the cell runs.")

(defparameter +effort-rungs+
  '("off" "minimal" "low" "medium" "high" "xhigh" "max")
  "Every reasoning rung a preset may name, weakest first. A model whose own
ladder is narrower refuses the pick, and says which rungs it takes.")

;;; --- the table the section carries ----------------------------------------------

(defun section-presets-object (table)
  "The presets object SECTION carries, as (values OBJECT SOURCE), SOURCE the
member it came from: `presets', else `aliases', the original's word for it."
  (let ((presets (and (hash-table-p table) (gethash "presets" table))))
    (if presets
        (values presets "presets")
        (values (and (hash-table-p table) (gethash "aliases" table)) "aliases"))))

(defun preset-member (object name)
  "OBJECT's NAME as a trimmed string, or NIL when it is absent or not one."
  (let ((value (and (hash-table-p object) (gethash name object))))
    (and (stringp value)
         (let ((trimmed (nlk:trimmed value)))
           (and (plusp (length trimmed)) trimmed)))))

(defun reads-a-preset (name object origin)
  "The preset NAME, an entry of ORIGIN, as the plist (:name :model :provider
:effort); refuses a shape no pick could use."
  (unless (hash-table-p object)
    (nlk:config-error "~a.~a is ~s: one object a preset, as {\"model\": \"claude-haiku-4.5\", \"provider\": \"anthropic\", \"effort\": \"low\"}"
                      origin name object))
  (let ((model (preset-member object "model"))
        (provider (preset-member object "provider"))
        (effort (or (preset-member object "effort")
                    (preset-member object "reasoning_effort"))))
    (unless model
      (nlk:config-error "~a.~a.model is missing or not a string: name the model id a /models pick would take" origin name))
    (unless provider
      (nlk:config-error "~a.~a.provider is missing or not a string: name the provider that serves that model" origin name))
    (when (and effort (not (member (string-downcase effort) +effort-rungs+ :test #'equal)))
      (nlk:config-error "~a.~a.effort is ~s: one of ~{~a~^, ~}" origin name effort +effort-rungs+))
    (list :name name :model model :provider provider
          :effort (and effort (string-downcase effort)))))

(defun read-presets (table)
  "The presets TABLE declares, in declaration order; refuses one of the wrong
shape instead of dropping it."
  (multiple-value-bind (object origin) (section-presets-object table)
    (cond ((null object) '())
          ((not (hash-table-p object))
           (nlk:config-error "~a is ~s: an object of named presets, as {\"fast\": {\"model\": \"...\", \"provider\": \"...\"}}"
                             origin object))
          (t (let ((presets '()))
               (maphash (lambda (name value) (push (reads-a-preset name value origin) presets))
                        object)
               (nreverse presets))))))

(defun read-settings (values table)
  "The section's own reader: the derived VALUES, and the table parsed into
:PRESETS."
  (list* :presets (read-presets table) values))

;;; --- the pick itself ------------------------------------------------------------

;; Three seams, so a test never needs a store, a provider or a key.
(defvar *select-model* #'nle:select-model
  "What lands a model pick: the gateway's own SELECT-MODEL, by default.")

(defvar *select-effort* #'nle:select-effort
  "What lands a reasoning-effort pick: the gateway's own SELECT-EFFORT.")

(defun session-selection (session-id)
  "(values PROVIDER MODEL EFFORT) SESSION-ID is pinned to, NILs when it is
pinned to nothing; nothing here moves."
  (when (and (stringp session-id) (plusp (length session-id)))
    (multiple-value-bind (provider model) (nlk:session-model-selection session-id)
      (values provider model (nlk:session-model-effort session-id)))))

(defvar *selection* #'session-selection
  "What a status line asks about a session, so a test answers without a store.")

(defun presets ()
  "The presets this cell runs on, in the section's order."
  (setting :presets))

(defun find-preset (name)
  "The preset called NAME, or NIL."
  (find name (presets) :key (lambda (preset) (getf preset :name)) :test #'equal))

(defun names-line ()
  "The preset names, as one comma-separated line."
  (let ((names (mapcar (lambda (preset) (getf preset :name)) (presets))))
    (if names
        (format nil "the presets are ~{~a~^, ~}" names)
        "no presets are named in the model-presets section yet")))

(defun preset-line (preset)
  "PRESET as one line: its name, its model on its provider, its rung."
  (format nil "~a: ~a on ~a~@[ at ~a~]"
          (getf preset :name) (getf preset :model) (getf preset :provider) (getf preset :effort)))

(defun presets-text ()
  "Every preset, one a line, or what to do about there being none."
  (let ((presets (presets)))
    (if presets
        (format nil "~{~a~^~%~}" (mapcar #'preset-line presets))
        "no presets yet: name them under \"presets\" in the model-presets section of ~/.nodecode/config.jsonc, one {\"model\": \"...\", \"provider\": \"...\", \"effort\": \"low\"} each, then (restart-cells \"nodecode-model-presets\")")))

(defun switch (name &key effort provider (session-id nlk:*scribe-session-id*))
  "Land preset NAME the way a /models pick lands: pin SESSION-ID to its model
on its provider, then its rung (or EFFORT's, or PROVIDER's, when the pick
says so). => what landed, as lines."
  (let ((preset (find-preset name)))
    (unless preset
      (fail "no preset named ~a; ~a" name (names-line)))
    (unless (and (stringp session-id) (plusp (length session-id)))
      (fail "preset ~a needs a session to switch: run /preset in one, or pass :session-id" name))
    (let ((said (funcall *select-model* (getf preset :model)
                          :provider (or provider (getf preset :provider))
                          :session-id session-id))
          (rung (or effort (getf preset :effort))))
      (if (null rung)
          said
          (handler-case
              (format nil "~a~%~a" said (funcall *select-effort* rung :session-id session-id))
            ;; The model moved before the rung was checked; say so rather than
            ;; leave the pick looking half-done.
            (error (condition)
              (format nil "~a~%the model moved, but the effort rung ~a was refused: ~a"
                      said rung condition)))))))

(defun status-text (session-id)
  "What SESSION-ID is pinned to now, and the presets that would move it."
  (multiple-value-bind (provider model effort)
      (handler-case (funcall *selection* session-id) (error () nil))
    (format nil "~a~%~a"
            (if provider
                (format nil "this session is pinned to ~a/~a~@[ at ~a~]" provider model effort)
                "this session follows the default model; no preset has moved it")
            (let ((presets (presets)))
              (if presets
                  (format nil "~d preset~:p: ~{~a~^, ~}"
                          (length presets) (mapcar (lambda (preset) (getf preset :name)) presets))
                  "no presets named yet")))))

;;; --- /preset ---------------------------------------------------------------------

(defparameter +flags-alone+ '("--global" "--session")
  "The original's scope flags: a Nodecode pick both pins the session and moves
the default, so these are taken and change nothing.")

(defun read-arguments (words)
  "=> (values NAME EFFORT PROVIDER) from the words after /preset; refuses a
flag the command does not take, or one left without its value."
  (let ((name (first words)) (effort nil) (provider nil) (rest (rest words)))
    (loop while rest
          for word = (pop rest)
          do (cond
               ((member word +flags-alone+ :test #'string-equal))
               ((member word '("--effort" "--reasoning") :test #'string-equal)
                (setf effort (pop rest))
                (unless effort
                  (fail "~a needs a rung after it: ~{~a~^, ~}" word +effort-rungs+)))
               ((string-equal word "--provider")
                (setf provider (pop rest))
                (unless provider
                  (fail "--provider needs a provider id after it")))
               (t (fail "~a is not a flag of /preset: name a preset, then --effort RUNG or --provider ID"
                        word))))
    (values name effort provider)))

(defun run-slash (args session-id)
  "One /preset: alone, the picker; list, the table; status, this session's
pin; a name, the switch."
  (let ((words (remove "" (uiop:split-string (nlk:trimmed (or args ""))
                                             :separator '(#\Space #\Tab #\Newline))
                       :test #'equal)))
    (cond
      ((null words)
       (let ((text (presets-text)))
         (values text
                 (nle:list-dialog
                  :title "Model presets" :summary text
                  :empty-label "no presets yet"
                  :rows (loop for preset in (presets)
                              collect (nle:run-row
                                       (getf preset :name)
                                       (format nil "/preset ~a" (getf preset :name))
                                       :detail (format nil "~a on ~a~@[ at ~a~]"
                                                       (getf preset :model) (getf preset :provider)
                                                       (getf preset :effort))
                                       :verb "switch"))))))
      ((string-equal (first words) "list") (presets-text))
      ((string-equal (first words) "status") (status-text session-id))
      (t (multiple-value-bind (name effort provider) (read-arguments words)
           (switch name :effort effort :provider provider :session-id session-id))))))

;;; --- the model's own tool --------------------------------------------------------

(nlk:define-contract preset-request
  (:doc "The operator's named model presets. `list' answers them; `switch' lands one on this session: its model on its provider, then its reasoning effort, the same pick /preset makes. Use it when the operator says what a preset is for (\"switch to my fast preset\", \"use the deep one here\") rather than naming a model and a provider themselves.")
  ("action" :text :required t :options '("list" "switch")
   :doc "list the presets, or switch this session to one")
  ("preset" :text :doc "the preset to switch to, exactly as list names it")
  ("effort" :text :doc "this one pick's reasoning rung, instead of the preset's: off, minimal, low, medium, high, xhigh, max")
  ("provider" :text :doc "this one pick's provider, instead of the preset's"))

(defun presets-tool (request)
  "MODEL_PRESETS' body: the table, or the pick."
  (let ((action (string-downcase (preset-request-action request))))
    (cond
      ((equal action "list") (presets-text))
      ((equal action "switch")
       (let ((name (preset-request-preset request)))
         (unless (and (stringp name) (plusp (length (nlk:trimmed name))))
           (fail "switch needs a preset: ~a" (names-line)))
         (switch (nlk:trimmed name)
                 :effort (preset-request-effort request)
                 :provider (preset-request-provider request))))
      (t (fail "action is ~s: list or switch" action)))))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell model-presets
  (:section ("model-presets")
    (:guide "nothing is required: presets is an object of named picks, one {\"model\": \"...\", \"provider\": \"...\", \"effort\": \"low\"} each; the model tool and /preset switch a session to one"))
  (:settings #'read-settings)
  (:tool "model_presets" (nlk:contract 'preset-request) #'presets-tool)
  (:help :model-presets
         "model-presets: named model, provider and effort picks, landed by /preset or the model_presets tool"
         +primer+)
  (:command "preset" #'run-slash
            :catches 'model-presets-error
            :description "Switch this session to a named model preset"
            :argument-hint "NAME | LIST | STATUS [--effort RUNG] [--provider ID]"))
