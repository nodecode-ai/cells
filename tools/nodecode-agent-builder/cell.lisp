;;;; cell.lisp --- the verbs, the tool, /agent-builder, and the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original was a desktop section over thirteen builder actions
;;;; (builder.py:190 Builder.dispatch): list, get, interview, build, refine,
;;;; edit, trial, compare, duplicate, prepare_companion, publish_spec,
;;;; created, published, pin_capabilities, export, delete. The shape worth
;;;; keeping is the loop a person actually walks -- a brief becomes a
;;;; blueprint, the blueprint is refined and snapshotted, one task is run
;;;; against two versions, and the best one is kept -- so that is what is
;;;; here, as Lisp verbs the model and the operator both call:
;;;;
;;;;   (agent-builder:build "a brief")        a new draft
;;;;   (agent-builder:refine ID "make it...") version N+1, the old one kept
;;;;   (agent-builder:trial ID "a task")      the blueprint answering it
;;;;   (agent-builder:compare ID "a task")    the last two versions, one task
;;;;   (agent-builder:drafts) (show ID) (export-blueprint ID) (drop ID)
;;;;
;;;; What the desktop gave and a file store does not need is left out: the
;;;; interview step, profile creation and its capability pinning, companion
;;;; installs, the duplicate button and the browser panel (see NOTICE).

(in-package #:nodecode-agent-builder)

;;; --- what (help :agent-builder) answers ---------------------------------------

(defparameter +primer+
  "An agent blueprint is what a specialist agent is before it exists: a name, a
slug, a description, its standing instructions, three starters, three success
checks and the toolsets and skills it needs. The nodecode-agent-builder cell
drafts one from a plain-language brief, keeps it as a draft under
~/.nodecode/agent-builder/, and can run a task against it.
The model's tool is agent_builder: brief (what the agent is for) and draft_id
(an existing draft, to refine it). The operator's command is /agent-builder:
  /agent-builder                 the drafts in this home
  /agent-builder BRIEF           a new draft from BRIEF
  /agent-builder refine ID TEXT  the draft ID, refined by TEXT; the old version is kept
  /agent-builder show ID         one draft whole
  /agent-builder try ID TASK     the draft answering TASK, as the agent would
  /agent-builder compare ID TASK the last two versions answering TASK
  /agent-builder drop ID         the draft's file, gone
Through eval the same is (agent-builder:build \"...\"), (agent-builder:refine ID TEXT),
(agent-builder:trial ID TASK), (agent-builder:compare ID TASK),
(agent-builder:drafts), (agent-builder:show ID), (agent-builder:export-blueprint ID),
(agent-builder:drop ID). A draft is not a running agent: nothing here creates a
profile, changes the config or starts anything. A refusal is AGENT-BUILDER-ERROR
with the reason; a trial runs on the auxiliary model, so the operator's own
selection and key answer it."
  "What (help :agent-builder) answers while the cell runs.")

;;; --- what a brief and its references may be ------------------------------------

(defun require-brief (value &key (label "Brief"))
  "VALUE as a brief, trimmed and bounded by the section's max_brief, or a refusal."
  (let ((text (nlk:trimmed (or (and (stringp value) value) ""))))
    (when (zerop (length text))
      (fail "Add ~(~a~) first: one or two sentences saying what the agent is for." label))
    (let ((max (setting :max-brief)))
      (if (> (length text) max)
          (fail "~(~a~) is too long: ~d characters of a maximum ~d." label (length text) max)
          text))))

(defun require-references (references)
  "REFERENCES -- a list of (NAME . CONTENT) -- as the list this cell stores, or
a refusal. Names are labels only; nothing here is used as a path."
  (let ((references (or references '())))
    (unless (listp references)
      (fail "References are a list of (\"name\" . \"text\") pairs."))
    (when (> (length references) 6)
      (fail "Attach up to 6 text reference files, not ~d." (length references)))
    (loop for entry in references
          for name = (nlk:trimmed (if (consp entry) (princ-to-string (car entry)) ""))
          for content = (nlk:trimmed (if (consp entry) (princ-to-string (cdr entry)) ""))
          when (zerop (length name)) do (fail "A reference file needs a name.")
          when (zerop (length content)) do (fail "Reference file ~a is empty." name)
          collect (cons name content))))

(defun allowed-toolsets () (or (setting :toolsets) '()))
(defun allowed-skills () (or (setting :skills) '()))

;;; --- the side call ------------------------------------------------------------

(defun ask-model (system user &key (max-tokens 3000) (what "blueprint"))
  "USER answered by the model under SYSTEM as one side call; => its text.
A provider that refused signals as AGENT-BUILDER-ERROR naming WHAT."
  (let ((answer (handler-case (nle:complete system user :max-tokens max-tokens)
                  (error (condition)
                    (fail "No model answered this ~a: ~a. Check the provider and the model ~
in /settings, then try again." what condition)))))
    (or (and answer (plusp (length (nlk:trimmed answer))) answer)
        (fail "The model answered nothing for this ~a. Try again." what))))

(defun capabilities-object ()
  "What the model is told it may pick from: the section's two lists."
  (nlk:make-json-object "toolsets" (coerce (allowed-toolsets) 'vector)
                         "skills" (coerce (allowed-skills) 'vector)))

(defun generate-plan (brief references previous)
  "BRIEF (with REFERENCES framed as data and PREVIOUS, a plan, as the version
to refine) as a validated plan."
  (let* ((input (nlk:encode-json-object
                 (apply #'nlk:make-json-object
                        (append (list "user_brief" brief
                                      "references" (references-json references)
                                      "available_capabilities" (capabilities-object))
                                (when previous (list "previous_blueprint" previous))))))
         (object (json-in-text (ask-model +blueprint-instructions+ input))))
    (unless object
      (fail "The model did not answer with an agent blueprint. Try again."))
    (validate-plan object (allowed-toolsets) (allowed-skills))))

;;; --- drafts, read and written ---------------------------------------------------

(defun make-draft (brief references plan)
  "A fresh draft of PLAN: version one, nothing tried or compared yet."
  (nlk:make-json-object "id" (new-id)
                         "brief" brief
                         "references" (references-json references)
                         "plan" plan
                         "version" 1
                         "revision" 0
                         "history" (vector brief)
                         "versions" (vector)
                         "trials" (vector)
                         "comparisons" (vector)
                         "updated" (get-universal-time)))

(defun snapshot (draft)
  "DRAFT's current plan kept as the version a comparison can reach, at most
the ten newest."
  (let* ((versions (nlk:json-value draft :array "versions"))
         (snapshot (nlk:make-json-object "version" (draft-version draft)
                                          "plan" (draft-plan draft)
                                          "references" (nlk:json-value draft :array "references")))
         (kept (coerce (loop for item across (or versions (vector)) collect item) 'vector)))
    (when (> (length kept) 10) (setf kept (subseq kept (- (length kept) 10))))
    (setf (gethash "versions" draft)
          (concatenate 'vector kept (vector snapshot)))))

(defun version-snapshots (draft)
  "DRAFT's kept versions, oldest first."
  (let ((versions (nlk:json-value draft :array "versions")))
    (and versions (loop for item across versions collect item))))

(defun draft-line (draft)
  "One line naming DRAFT: its id, version and name."
  (format nil "~a  v~d  ~a~%" (draft-id draft) (draft-version draft)
          (or (nlk:json-value (draft-plan draft) :string "name") "unnamed")))

(defun draft-text (draft &key (note nil))
  "DRAFT as the block a person reads: the one line, the note, the plan whole."
  (concatenate 'string (draft-line draft)
               (or note "")
               (plan-text (draft-plan draft))))

;;; --- the verbs ----------------------------------------------------------------

(define-verb drafts ()
  "Every draft this home holds, newest first: its id, its version and its name."
  (let ((all (stored-drafts))
        (keep (setting :keep)))
    (if (null all)
        "No drafts yet: /agent-builder BRIEF, or the agent_builder tool with a brief."
        (with-output-to-string (out)
          (loop for draft in all
                for index from 0
                when (< index keep) do (write-string (draft-line draft) out))
          (when (> (length all) keep)
            (format out "[~d more: raise the section's keep to list them]~%" (- (length all) keep)))))))

(define-verb show (id)
  "Draft ID whole: the plan, the instructions, and what has been tried on it."
  (let* ((draft (load-draft id))
         (trials (nlk:json-value draft :array "trials"))
         (count (if trials (length trials) 0)))
    (with-output-to-string (out)
      (write-string (draft-text draft) out)
      (format out "~%last revision ~d, ~d trial~:p kept, ~d version~:p before this one~%"
              (draft-revision draft) count (length (version-snapshots draft)))
      (format out "the brief:~%~a~%" (draft-brief draft)))))

(define-verb build (brief &key references)
  "A new draft from BRIEF (with REFERENCES, a list of (NAME . CONTENT)), its
plan written by the model. => the draft, one line and its instructions."
  (let* ((brief (require-brief brief))
         (references (require-references references))
         (plan (generate-plan brief references nil))
         (draft (save-draft (make-draft brief references plan))))
    (draft-text draft
                :note (format nil "draft created; /agent-builder try ~a TASK runs it, ~
/agent-builder refine ~a TEXT changes it~%" (draft-id draft) (draft-id draft)))))

(define-verb refine (id brief &key references)
  "Draft ID refined by BRIEF: version N+1, the plan it replaced kept for a
comparison. => the new version."
  (let* ((draft (load-draft id))
         (refinement (require-brief brief :label "Refinement"))
         (references (if references (require-references references) (references-of draft)))
         (plan (generate-plan refinement references (draft-plan draft)))
         (version (1+ (draft-version draft))))
    (snapshot draft)
    (setf (gethash "plan" draft) plan
          (gethash "references" draft) (references-json references)
          (gethash "version" draft) version
          (gethash "brief" draft) (format nil "~a~%~%refined: ~a" (draft-brief draft) refinement)
          (gethash "history" draft)
          (concatenate 'vector (nlk:json-value draft :array "history") (vector refinement)))
    (draft-text (save-draft draft)
                :note (format nil "version ~d saved; ~d earlier version~:p kept~%"
                              version (length (version-snapshots draft))))))

(define-verb trial (id message)
  "Draft ID answering MESSAGE as the agent it describes would, on the model's
auxiliary target. The earlier trials at this version are the conversation. => the reply."
  (let* ((draft (load-draft id))
         (message (require-brief message :label "Trial prompt"))
         (soul (soul-text (draft-plan draft) (references-of draft)))
         (trials (nlk:json-value draft :array "trials"))
         (earlier (when trials
                    (loop for item across trials
                          when (= (or (nlk:json-value item :integer "version") 0)
                                   (draft-version draft))
                            collect item)))
         (earlier (if (> (length earlier) 4) (subseq earlier (- (length earlier) 4)) earlier))
         (prompt (with-output-to-string (out)
                   (dolist (item earlier)
                     (format out "Earlier in this conversation:~%user: ~a~%agent: ~a~%~%"
                             (or (nlk:json-value item :string "prompt") "")
                             (or (nlk:json-value item :string "reply") "")))
                   (write-string message out)))
         (reply (nlk:trimmed (ask-model soul prompt :max-tokens 2500 :what "trial")))
         (record (nlk:make-json-object "prompt" message "reply" reply
                                        "version" (draft-version draft)
                                        "at" (get-universal-time))))
    (setf (gethash "trials" draft)
          (concatenate 'vector (or trials (vector)) (vector record)))
    (save-draft draft)
    (format nil "~a v~d~%~%~a" (or (nlk:json-value (draft-plan draft) :string "name") "agent")
            (draft-version draft) reply)))

(define-verb compare (id message)
  "The two newest versions of draft ID answering MESSAGE on one task, side by
side: what changed, and whether it changed anything. => both replies."
  (let* ((draft (load-draft id))
         (message (require-brief message :label "Comparison task"))
         (snapshots (version-snapshots draft)))
    (unless snapshots
      (fail "Refine this draft once before comparing: a comparison is a version and the one ~
it replaced."))
    (let* ((previous (car (last snapshots)))
           (old-plan (nlk:json-value previous :object "plan"))
           (old-version (or (nlk:json-value previous :integer "version") 0))
           (old-references (let ((items (nlk:json-value previous :array "references")))
                             (and items
                                  (loop for item across items
                                        for name = (nlk:json-value item :string "name")
                                        for content = (nlk:json-value item :string "content")
                                        when (and name content) collect (cons name content)))))
           (old-reply (nlk:trimmed (ask-model (soul-text old-plan old-references) message
                                              :max-tokens 2500 :what "comparison")))
           (new-reply (nlk:trimmed (ask-model (soul-text (draft-plan draft) (references-of draft))
                                              message :max-tokens 2500 :what "comparison")))
           (again (nlk:make-json-object "prompt" message "at" (get-universal-time)
                                         "outputs"
                                         (vector (nlk:make-json-object "version" old-version
                                                                        "reply" old-reply)
                                                 (nlk:make-json-object "version" (draft-version draft)
                                                                        "reply" new-reply)))))
      (setf (gethash "comparisons" draft)
            (concatenate 'vector (or (nlk:json-value draft :array "comparisons") (vector))
                         (vector again)))
      (save-draft draft)
      (format nil "~a, one task, two versions~%~%the task: ~a~%~%v~d:~%~a~%~%v~d:~%~a"
              (or (nlk:json-value (draft-plan draft) :string "name") "agent")
              message old-version old-reply (draft-version draft) new-reply))))

(define-verb export-blueprint (id)
  "Draft ID as the portable blueprint: the plan and its references, one JSON object."
  (let ((draft (load-draft id)))
    (nlk:encode-json-object
     (nlk:make-json-object "format" "nodecode-agent-builder" "format_version" 1
                            "plan" (draft-plan draft)
                            "references" (nlk:json-value draft :array "references")))))

(define-verb drop (id)
  "Draft ID's file, gone from this home. => what was removed."
  (let ((draft (load-draft id)))
    (forget-draft id)
    (format nil "Dropped ~a (~a)." (draft-id draft)
            (or (nlk:json-value (draft-plan draft) :string "name") "unnamed"))))

;;; --- the tool the model calls --------------------------------------------------

(nlk:define-contract agent-builder-call
  (:doc "Create or refine a reusable specialist agent draft: a name, a description, standing instructions, starters, success checks and the toolsets and skills it needs. A draft is not a running agent; nothing is created until a person saves it.")
  ("brief" :text :required t
   :doc "the job the specialist should perform, or for a draft_id, what to change about it")
  ("draft_id" :string
   :doc "an existing draft id, to refine that draft instead of starting a new one; /agent-builder list names them"))

(defun agent-builder-tool (call)
  "agent_builder: a new draft from a brief, or an existing one refined."
  (let ((id (agent-builder-call-draft-id call)))
    (if id
        (refine id (agent-builder-call-brief call))
        (build (agent-builder-call-brief call)))))

;;; --- /agent-builder -------------------------------------------------------------

(defun split-first (text)
  "TEXT as two values: its first word, and the rest with the space between trimmed."
  (let* ((text (nlk:trimmed (or text "")))
         (space (position #\Space text)))
    (if space
        (values (subseq text 0 space) (nlk:trimmed (subseq text (1+ space))))
        (values text ""))))

(defun run-command (args session-id)
  "/agent-builder: the drafts, one draft, or a draft being made, refined, tried,
compared or dropped. => the answer's text."
  (declare (ignore session-id))
  (multiple-value-bind (word rest) (split-first args)
    (cond
      ((zerop (length word)) (drafts))
      ((member word '("list" "ls") :test #'string-equal) (drafts))
      ((string-equal word "show") (show (first (multiple-value-list (split-first rest)))))
      ((string-equal word "build") (build rest))
      ((string-equal word "refine")
       (multiple-value-bind (id text) (split-first rest) (refine id text)))
      ((string-equal word "try")
       (multiple-value-bind (id text) (split-first rest) (trial id text)))
      ((string-equal word "compare")
       (multiple-value-bind (id text) (split-first rest) (compare id text)))
      ((string-equal word "drop") (drop (first (multiple-value-list (split-first rest)))))
      ((draft-id-p word) (show word))
      (t (build (nlk:trimmed (or args "")))))))

;;; --- the one declaration ---------------------------------------------------------

(nle:define-cell agent-builder
  (:section ("agent-builder")
    (:guide "nothing here is required; a brief is bounded by max_brief, and toolsets and skills list what a blueprint may pick from, empty meaning no check")
    ("max_brief" :integer :default 12000 :min 100
     :doc "the longest brief, refinement or trial prompt this cell accepts")
    ("toolsets" :list
     :doc "the toolsets a blueprint may select; empty means the model's choice is taken as it comes")
    ("skills" :list
     :doc "the skills a blueprint may select; empty means the model's choice is taken as it comes")
    ("keep" :integer :default 50 :min 1
     :doc "how many drafts /agent-builder lists"))
  (:help :agent-builder
         "agent-builder: draft, refine, try and keep specialist agent blueprints"
         +primer+)
  (:tool "agent_builder" (nlk:contract 'agent-builder-call) #'agent-builder-tool)
  (:command "agent-builder" 'run-command
            :description "Draft, refine, try or list specialist agent blueprints"))
