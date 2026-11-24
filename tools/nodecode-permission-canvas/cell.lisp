;;;; cell.lisp --- the three tools, /permission-canvas, the manual, and the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of TANGBUDU/permission-canvas, a Hermes Agent
;;;; computer-use backend. The original wrapped a desktop driver and enforced
;;;; element-level permissions inside it; here the same policy engine is the
;;;; cell: the model asks before it acts (permission_canvas_check) and asks
;;;; what it may see (permission_canvas_observe), and a person - never the
;;;; model - writes the rules and grants one exact operation
;;;; (/permission-canvas). The folder holds policy.json, approvals.json and
;;;; audit.jsonl, and nothing leaves the machine.

(in-package #:nodecode-permission-canvas)

;;; --- the shapes at the boundary ------------------------------------------------

(nlk:define-contract screen-control
  (:doc "One control of a window, as an accessibility tree describes it: its index, its role and label, the process and window it belongs to, and where it sits on screen.")
  ("index" :integer :required t :doc "the control's position in the observation the agent is acting on")
  ("app" :string :default "" :doc "the application the window belongs to, when known")
  ("role" :string :default "" :doc "the accessibility role: Button, Edit, CheckBox, MenuItem, ...")
  ("label" :string :default "" :doc "the control's own text, as the tree reports it")
  ("pid" :integer :doc "the process id of the window, when known")
  ("window_id" :integer :doc "the window's id or handle, when known")
  ("bounds" :array :of :integer :doc "left, top, width and height in screen pixels")
  ("parent" :integer :doc "the index of the nearest listed ancestor, when the tree reports one"))

(nlk:define-contract gate-arguments
  (:doc "Ask whether one proposed desktop action may be performed on one control. Answers allow, refuse, or one exact approval the person must grant.")
  ("action" :text :required t :doc "what is proposed: click, double_click, type, key, set_value, drag or observe")
  ("element" :object :of screen-control :required t :doc "the control the action targets")
  ("text" :string :default "" :doc "the text to type, for type")
  ("keys" :string :default "" :doc "the key or combination, for key")
  ("window_title" :string :default "" :doc "the window's title, when known: title rules and the privacy pattern read it"))

(nlk:define-contract observation-arguments
  (:doc "Hand over one observation of a window - its controls as the accessibility tree reports them - and be told which of them the person's rules withhold and which rectangles to mask.")
  ("elements" :array :of screen-control :required t :doc "the controls of one window")
  ("window_title" :string :default "" :doc "the window's title, when known"))

(nlk:define-contract policy-arguments
  (:doc "Read the policy in force: the default level, the rules, the selected window, the pause, and what waits for a person's approval.")
  ("verbose" :boolean :default nil :doc "name every rule's scope, not only its id and level"))

;;; --- reading a record back -----------------------------------------------------

(defun control-of (record)
  "RECORD as the plist POLICY.LISP decisions read."
  (control (screen-control-index record)
           :app (screen-control-app record)
           :role (screen-control-role record)
           :label (screen-control-label record)
           :pid (screen-control-pid record)
           :window-id (screen-control-window-id record)
           :bounds (screen-control-bounds record)
           :parent (screen-control-parent record)))

;;; --- the decision -------------------------------------------------------------

(defparameter +outside-selected-window+
  "permission-canvas: capture withheld: outside_selected_window. Only the window the person selected may be observed or operated; select one with /permission-canvas target PID WINDOW_ID APP TITLE. No screenshot was returned and no input was performed."
  "Fixed text: no title, app or label of another window crosses into it.")

(defparameter +paused+
  "permission-canvas: desktop access is paused. Ask the person to continue with /permission-canvas resume, then retry."
  "Fixed text for the paused state.")

(defvar *approval-seq* 0
  "How many approvals this image has asked for: two asks for one signature in one
second still get two ids.")

(defun new-approval-id (ledger signature)
  "A short id no pending approval already holds."
  (loop with base = (subseq (nlk:sha256-text (format nil "~a~c~a~c~d"
                                                     signature #\Nul (now) #\Nul (incf *approval-seq*)))
                            0 8)
        for attempt from 0
        for candidate = (if (zerop attempt) base (format nil "~a~d" base attempt))
        unless (gethash candidate (ledger-pending ledger))
          return candidate))

(defun target-mismatch-p (policy element)
  "Is ELEMENT outside the window the person selected? A control that reports no
process or window cannot be shown to be inside it, so it is outside."
  (when (policy-require-target-p policy)
    (let ((target (policy-target policy)))
      (or (null target)
          (not (and (equal (getf target :pid) (getf element :pid))
                    (equal (getf target :window-id) (getf element :window-id))))))))

(defun decide-action (policy ledger action element arguments window-title)
  "The answer to one proposed action, as a plist: :decision, :code, :text, :level
and, for an approval, :id. Every uncertain case is a refusal."
  (cond
    ((policy-paused-p policy)
     (audit! (nlk:json-object "decision" "deny" "code" "paused" "action" action
                               "element" (getf element :index)))
     (list :decision "deny" :code "paused" :text +paused+))
    ((target-mismatch-p policy element)
     (audit! (nlk:json-object "decision" "deny" "code" "outside_selected_window" "action" action
                               "element" (getf element :index)))
     (list :decision "deny" :code "outside_selected_window" :text +outside-selected-window+))
    (t
     (multiple-value-bind (level rule) (effective-level policy element window-title)
       (cond
         ((string= level "green")
          (audit! (nlk:json-object "decision" "allow" "action" action "level" level
                                    "element" (getf element :index)
                                    "rule" (getf rule :id)))
          (list :decision "allow" :level level
                :text (format nil "permission-canvas: ~a on element ~d allowed (green)."
                              action (getf element :index))))
         ((member level '("blue" "black") :test #'string=)
          (audit! (nlk:json-object "decision" "deny" "action" action "level" level
                                    "element" (getf element :index)))
          (list :decision "deny" :level level :code (format nil "~a_zone" level)
                :text (format nil "permission-canvas: ~a refused: element ~d is ~a (~a)."
                              action (getf element :index) level
                              (if (string= level "blue") "read only" "hidden"))))
         (t
          (let ((signature (approval-signature action element arguments policy)))
            (cond
              ((gethash signature (ledger-denied ledger))
               (audit! (nlk:json-object "decision" "human_denied" "action" action
                                         "element" (getf element :index)))
               (list :decision "deny" :level level :code "approval_denied"
                     :text (format nil "permission-canvas: the person denied this exact operation on element ~d; do not retry it."
                                   (getf element :index))))
              ((gethash signature (ledger-approved ledger))
               (remhash signature (ledger-approved ledger))
               (ledger-save ledger)
               (audit! (nlk:json-object "decision" "allow_approved" "action" action
                                         "element" (getf element :index)))
               (list :decision "allow" :level level :approved t
                     :text (format nil "permission-canvas: ~a on element ~d allowed once, as approved; the approval is now used."
                                   action (getf element :index))))
              (t
               (let* ((id (or (loop for key being the hash-keys of (ledger-pending ledger)
                                     using (hash-value value)
                                     when (equal signature (nlk:json-value value :string "signature"))
                                       return key)
                              (new-approval-id ledger signature)))
                      (expires (+ (now) (setting :approval-ttl-seconds))))
                 (setf (gethash id (ledger-pending ledger))
                       (nlk:json-object "signature" signature
                                        "description" (format nil "~a on ~a" action (control-text element))
                                        "expires" expires))
                 (ledger-save ledger)
                 (audit! (nlk:json-object "decision" "approval_required" "action" action
                                           "element" (getf element :index) "action_id" id))
                 (list :decision "approval_required" :level level :code "approval_required" :id id
                       :text (format nil "permission-canvas: ~a on element ~d needs the person's approval (id ~a); ask them to run /permission-canvas approve ~a, then retry the same action."
                                     action (getf element :index) id id))))))))))))

;;; --- the tools -----------------------------------------------------------------

(defun run-check (arguments)
  "The gate: one proposed action, one control."
  (let* ((element (control-of (gate-arguments-element arguments)))
         (action (gate-arguments-action arguments))
         (result (decide-action (policy-load) (ledger-load) action element
                                 (list :text (gate-arguments-text arguments)
                                       :keys (gate-arguments-keys arguments))
                                 (gate-arguments-window-title arguments))))
    (getf result :text)))

(defun run-observe (arguments)
  "The redaction half: what of one observation may reach the model at all."
  (let* ((policy (policy-load))
         (records (coerce (observation-arguments-elements arguments) 'list))
         (title (observation-arguments-window-title arguments)))
    (when (> (length records) (setting :max-controls))
      (refuse "permission-canvas: ~d controls is more than one observation reads (max_controls is ~d)"
              (length records) (setting :max-controls)))
    (if (policy-paused-p policy)
        +paused+
        (let ((elements (mapcar #'control-of records)))
          (multiple-value-bind (visible withheld rectangles)
              (observation policy elements title)
            (audit! (nlk:json-object "decision" "observe" "elements" (length elements)
                                      "withheld" (length withheld)))
            (with-output-to-string (out)
              (cond ((null withheld)
                     (format out "permission-canvas: all ~d controls are visible; nothing to mask."
                             (length elements)))
                    (t
                     (format out "permission-canvas: ~d of ~d controls withheld; ~d rectangle~:p to mask: ~{~a~^, ~}.~%"
                             (length withheld) (length elements) (length rectangles)
                             (loop for rectangle in rectangles
                                   collect (format nil "(~{~d~^,~})" rectangle)))
                     (format out "withheld: ~{~d~^, ~} (their roles and labels are not named).~%"
                             (sort (mapcar (lambda (element) (getf element :index)) withheld) #'<))
                     (format out "visible: ~{~a~^; ~}~%"
                             (loop for element in visible
                                   collect (format nil "~d ~a" (getf element :index)
                                                   (control-text element))))
                     (format out "Mask the rectangles before the picture reaches the model, and act only on the visible ones.")))))))))

(defun run-policy (arguments)
  "The policy in force, and what waits for a person."
  (declare (ignore arguments))
  (let* ((policy (policy-load))
         (ledger (ledger-prune (ledger-load)))
         (rules (policy-rules policy))
         (target (policy-target policy))
         (pending (pending-ids ledger)))
    (with-output-to-string (out)
      (format out "permission-canvas: ~:[running~;paused~]; ~d rule~:p; default ~a; an unnamed control gets ~a; directory ~a.~%"
              (policy-paused-p policy) (length rules)
              (policy-level policy "default_level" "yellow")
              (policy-level policy "unidentified_level" "yellow")
              (namestring (data-directory)))
      (when (policy-require-target-p policy)
        (format out "selected window: ~a~@[ pid ~d~]~@[ window ~d~]~%"
                (or (getf target :app) "none") (getf target :pid) (getf target :window-id)))
      (loop for rule in rules
            do (format out "  ~a ~a~@[ app=~a~]~@[ role=~a~]~@[ label=~s~]~@[ label_regex=~s~]~@[ title_regex=~s~]~@[ pid=~d~]~@[ window=~d~]~%"
                        (getf rule :id) (getf rule :level)
                        (getf rule :app) (getf rule :role) (getf rule :label)
                        (getf rule :label-regex) (getf rule :window-title-regex)
                        (getf rule :pid) (getf rule :window-id)))
      (format out "~[no approval is pending~:;~d approval~:p pending: ~@{~a~^, ~}~]~%"
              (length pending) pending))))

;;; --- the command: what a person does -------------------------------------------

(defun words-of (text)
  "TEXT split on spaces, with a double-quoted run kept whole."
  (let ((words '()) (current (make-string-output-stream)) (quoted nil) (started nil))
    (flet ((settle ()
             (when started (push (get-output-stream-string current) words) (setf started nil))))
      (loop for character across (or text "")
            do (case character
                 (#\" (setf quoted (not quoted) started t))
                 ((#\Space #\Tab)
                  (if quoted
                      (write-char character current)
                      (progn (settle) (setf started nil))))
                 (t (write-char character current) (setf started t)))
            finally (settle)))
    (nreverse words)))

(defun policy-save-rules (policy rules)
  (setf (gethash "rules" policy)
        (coerce (loop for rule in rules
                      collect (apply #'nlk:make-json-object
                                     (loop for (key value) on rule by #'cddr
                                           when value
                                             append (list (substitute #\_ #\- (string-downcase (string key)))
                                                          value))))
                'vector))
  (policy-save policy))

(defun rule-id-for (policy)
  (loop with taken = (rule-ids (policy-rules policy))
        for index from 1
        for id = (format nil "r~d" index)
        unless (member id taken :test #'string=) return id))

(defun member-value (key text)
  "The value a rule member takes: numbers for pid and window_id, TEXT otherwise."
  (cond ((member key '("pid" "window_id") :test #'string=)
         (handler-case (parse-integer text) (error () (refuse "permission-canvas: ~a must be a number, not ~s" key text))))
        (t text)))

(defun level-word (text)
  "TEXT as a level: one of the four names, or the word a person uses for it."
  (let ((word (string-downcase (or text ""))))
    (or (cdr (assoc word '(("allow" . "green") ("green" . "green")
                           ("readonly" . "blue") ("read-only" . "blue") ("read" . "blue")
                           ("blue" . "blue")
                           ("ask" . "yellow") ("ask-first" . "yellow") ("yellow" . "yellow")
                           ("hide" . "black") ("hidden" . "black") ("black" . "black"))
                         :test #'string=))
        word)))

(defun add-rule (policy level words)
  "RULE LEVEL [key=value ...], for the keys a rule narrows on."
  (let ((members '()))
    (dolist (word words)
      (let ((equals (position #\= word)))
        (unless equals
          (refuse "permission-canvas: ~s is not key=value; keys are app, role, label, label_regex, window_title_regex, pid, window_id" word))
        (let* ((key (subseq word 0 equals))
               (value (subseq word (1+ equals))))
          (unless (member key '("app" "role" "label" "label_regex" "window_title_regex" "pid" "window_id")
                          :test #'string=)
            (refuse "permission-canvas: ~a is not a rule key; keys are app, role, label, label_regex, window_title_regex, pid, window_id" key))
          (when (zerop (length value))
            (refuse "permission-canvas: ~a= needs a value" key))
          (setf members (append members (list (intern (string-upcase key) :keyword)
                                              (member-value key value)))))))
    (when (null members)
      (refuse "permission-canvas: a rule needs a scope; name at least one of app, role, label, label_regex, window_title_regex, pid, window_id"))
    (let* ((rules (policy-rules policy))
           (id (rule-id-for policy))
           (rule (list* :id id :level level members)))
      (policy-save-rules policy (append rules (list rule)))
      (format nil "permission-canvas: rule ~a is ~a on ~{~a~}; every match of it now carries that level.~%"
              id level (loop for (key value) on members by #'cddr
                             collect (format nil "~a=~s" (string-downcase (symbol-name key)) value))))))

(defun status-text (policy)
  (with-output-to-string (out)
    (format out "permission-canvas: ~:[running~;paused~]. A control with no rule ~:[needs the person's approval first~;is allowed~].~%"
            (policy-paused-p policy)
            (string= (policy-level policy "default_level" "yellow") "green"))
    (format out "  directory: ~a~%  rules: ~d  approval: ~a, identifies each control by its app, role, label, process and window - never by the observation's index.~%"
            (namestring (data-directory))
            (length (policy-rules policy))
            (if (policy-paused-p policy) "refused while paused" "consumed once per exact action"))
    (when (policy-require-target-p policy)
      (format out "  selected window: ~s~%" (policy-target policy)))
    (format out "Use /permission-canvas rules, pending, approve ID, deny ID, rule LEVEL key=value..., remove ID, default LEVEL, pause, resume, target PID WINDOW_ID APP TITLE, target none, block APP, unblock APP, hide-title REGEX.~%")))

(defun run-slash (arguments session-id)
  "The person's side: rules, approvals, the pause, and the selected window."
  (declare (ignore session-id))
  (handler-case
      (let* ((words (words-of arguments))
             (command (string-downcase (or (first words) "status")))
             (rest (rest words)))
        (values (dispatch-command command rest) nil))
    (permission-canvas-error (condition)
      (values (princ-to-string condition) nil))))

(defun dispatch-command (command rest)
  (let ((policy (policy-load))
        (ledger (ledger-prune (ledger-load))))
    (cond
      ((member command '("status" "") :test #'string=) (status-text policy))
      ((string= command "rules")
       (if (null (policy-rules policy))
           "permission-canvas: no rule yet; everything follows the default level. /permission-canvas rule LEVEL role=Button adds one."
           (with-output-to-string (out)
             (loop for rule in (policy-rules policy)
                   do (format out "~a ~a ~{~a~^ ~}~%"
                              (getf rule :id) (getf rule :level)
                              (loop for (key value) on rule by #'cddr
                                    unless (member key '(:id :level))
                                      collect (format nil "~a=~s" (string-downcase (symbol-name key)) value)))))))
      ((string= command "pending")
       (if (null (pending-ids ledger))
           "permission-canvas: no approval is pending."
           (with-output-to-string (out)
             (loop for id in (pending-ids ledger)
                   for item = (gethash id (ledger-pending ledger))
                   do (format out "~a  ~a  (expires ~ait)~%" id
                              (nlk:json-value item :string "description")
                              (nlk:json-value item :integer "expires"))))))
      ((string= command "approve")
       (let* ((id (or (first rest) (refuse "permission-canvas: approve needs the id /permission-canvas pending printed")))
              (item (or (gethash id (ledger-pending ledger))
                        (refuse "permission-canvas: no approval is pending under ~a" id)))
              (signature (nlk:json-value item :string "signature")))
         (remhash id (ledger-pending ledger))
         (setf (gethash signature (ledger-approved ledger)) (nlk:json-value item :integer "expires"))
         (ledger-save ledger)
         (format nil "permission-canvas: approved ~a once: ~a. It is used by the next identical action and by nothing else.~%"
                 id (nlk:json-value item :string "description"))))
      ((string= command "deny")
       (let* ((id (or (first rest) (refuse "permission-canvas: deny needs the id /permission-canvas pending printed")))
              (item (or (gethash id (ledger-pending ledger))
                        (refuse "permission-canvas: no approval is pending under ~a" id))))
         (remhash id (ledger-pending ledger))
         (setf (gethash (nlk:json-value item :string "signature") (ledger-denied ledger)) t)
         (ledger-save ledger)
         (format nil "permission-canvas: denied ~a. The agent is told the operation is refused and not to retry it.~%" id)))
      ((string= command "pause")
       (setf (gethash "paused" policy) t)
       (policy-save policy)
       "permission-canvas: paused. Every check and observation is refused until /permission-canvas resume.")
      ((string= command "resume")
       (setf (gethash "paused" policy) nil)
       (policy-save policy)
       "permission-canvas: running. The agent may check actions against the policy again.")
      ((string= command "default")
       (let ((level (coerce-level (level-word (first rest)) "the default level")))
         (setf (gethash "default_level" policy) level)
         (policy-save policy)
         (format nil "permission-canvas: a control no rule matches is ~a now.~%" level)))
      ((string= command "unidentified")
       (let ((level (coerce-level (level-word (first rest)) "the unidentified level")))
         (setf (gethash "unidentified_level" policy) level)
         (policy-save policy)
         (format nil "permission-canvas: a control with no label, in a window that has a restriction, is ~a now.~%" level)))
      ((string= command "block")
       (let ((app (or (first rest) (refuse "permission-canvas: block needs an app name"))))
         (setf (gethash "blocked_apps" policy)
               (coerce (adjoin app (policy-blocked-apps policy) :test #'string=) 'vector))
         (policy-save policy)
         (format nil "permission-canvas: ~a is hidden now: its controls are masked and refused.~%" app)))
      ((string= command "unblock")
       (let ((app (or (first rest) (refuse "permission-canvas: unblock needs an app name"))))
         (setf (gethash "blocked_apps" policy)
               (coerce (remove app (policy-blocked-apps policy) :test #'string=) 'vector))
         (policy-save policy)
         (format nil "permission-canvas: ~a is no longer blocked.~%" app)))
      ((string= command "hide-title")
       (let ((regex (or (first rest) (refuse "permission-canvas: hide-title needs a regular expression"))))
         (let ((problem (regex-problem regex "the title pattern")))
           (when problem (refuse "permission-canvas: ~a" problem)))
         (setf (gethash "hide_window_title_regex" policy) regex)
         (policy-save policy)
         (format nil "permission-canvas: anything whose title or label matches ~s is hidden now.~%" regex)))
      ((string= command "no-hide-title")
       (remhash "hide_window_title_regex" policy)
       (policy-save policy)
       "permission-canvas: the title pattern is off; titles are read as they are.")
      ((string= command "rule")
       (let ((level (coerce-level (level-word (first rest)) "the rule's level")))
         (add-rule policy level (rest rest))))
      ((string= command "remove")
       (let* ((id (or (first rest) (refuse "permission-canvas: remove needs a rule id")))
              (rules (policy-rules policy)))
         (unless (member id (rule-ids rules) :test #'string=)
           (refuse "permission-canvas: no rule ~a; /permission-canvas rules lists them" id))
         (policy-save-rules policy (remove id rules :key (lambda (rule) (getf rule :id)) :test #'string=))
         (format nil "permission-canvas: rule ~a is gone.~%" id)))
      ((string= command "target")
       (cond
         ((null rest)
          (format nil "permission-canvas: selected window ~s~%" (policy-target policy)))
         ((and (= 1 (length rest)) (string= (first rest) "none"))
          (remhash "target_window" policy)
          (setf (gethash "require_target" policy) nil)
          (policy-save policy)
          "permission-canvas: no window is selected; any window may be observed.")
         (t
          (destructuring-bind (pid window-id app &rest title) rest
            (let ((pid (handler-case (parse-integer pid) (error () (refuse "permission-canvas: target needs PID WINDOW_ID APP TITLE"))))
                  (window-id (handler-case (parse-integer window-id) (error () (refuse "permission-canvas: target needs PID WINDOW_ID APP TITLE")))))
              (when (or (null app) (null title))
                (refuse "permission-canvas: target needs PID WINDOW_ID APP TITLE; the app and the title are part of what the person selected"))
              (setf (gethash "target_window" policy)
                    (nlk:json-object "pid" pid "window_id" window-id "app" app
                                      "title" (format nil "~{~a~^ ~}" title))
                    (gethash "require_target" policy) t)
              (policy-save policy)
              (format nil "permission-canvas: only pid ~d, window ~d (~a) may be observed or operated now.~%" pid window-id app))))))
      (t (refuse "permission-canvas: ~a is no subcommand; try status, rules, pending, approve ID, deny ID, rule LEVEL key=value..., remove ID, default LEVEL, pause, resume, target PID WINDOW_ID APP TITLE, block APP, hide-title REGEX" command)))))

;;; --- the manual ----------------------------------------------------------------

(defparameter +manual+
  "permission-canvas: element-level permissions for desktop work, kept in a folder on this machine so the model asks before it acts and sees only what a person let it see. It enforces one thing: a proposed action is checked against the person's rules, and an observation is stripped of what they hid - it is not an operating-system sandbox, and it does not drive the desktop itself. Four levels, least to most restrictive: green (allow), blue (read only: observe, no click, drag, typing or value), yellow (one exact action needs the person's approval), black (invisible: removed from the observation and its pixels masked, and no operation). A control is matched by scope - app, role, label, label_regex, window_title_regex, pid, window_id - and the most restrictive matching rule wins; a scope field the tree did not report never rules a rule out, except for a green rule, which needs the evidence, so a missing field cannot widen what the model may operate. A control with no label, in a window that carries any restriction, gets the unidentified level rather than the default one. The tools: permission_canvas_check before any action, with the action and the control; permission_canvas_observe with one window's controls, which answers which are visible, which are withheld and which rectangles to mask; permission_canvas_policy to read the policy in force. A check answers allow, a refusal, or one approval id - the model may not approve itself. The person does that: /permission-canvas approve ID or deny ID, and writes the rules with /permission-canvas rule LEVEL key=value..., default LEVEL, unidentified LEVEL, block APP, hide-title REGEX, remove ID, pause, resume, target PID WINDOW_ID APP TITLE. An approval is one operation: it names the action, the control, its arguments and the policy it was given under, it is used once, and editing the policy makes every earlier approval stale. Every decision is one line in audit.jsonl. The folder holds policy.json, approvals.json and audit.jsonl; the section permission-canvas holds dir (that folder, required), default_level, unidentified_level, approval_ttl_seconds and max_controls. When the person selected a window, only that process and window may be observed or operated; anything else is withheld with the fixed outside_selected_window text. Hidden names never leak: a refused check names the element's index, and a withheld control is reported by index only."
  "What (help :permission-canvas) answers while the cell runs.")

;;; --- the cell ------------------------------------------------------------------

(nle:define-cell permission-canvas
  (:section ("permission-canvas")
    (:guide "dir is the one required setting: the folder that holds policy.json, approvals.json and audit.jsonl; the rest have defaults; /permission-canvas status reads the policy back")
    ("dir" :path :required t
           :doc "folder for policy.json, approvals.json and audit.jsonl")
    ("default_level" :choice :options '("green" "blue" "yellow" "black") :default "yellow"
                     :doc "what a control no rule matches gets; a new policy starts with ask-first")
    ("unidentified_level" :choice :options '("green" "blue" "yellow" "black") :default "yellow"
                          :doc "what a control with no label gets in a window that has any restriction")
    ("approval_ttl_seconds" :integer :default 120 :min 1
                            :doc "how long one approval waits for the person before it expires")
    ("max_controls" :integer :default 2000 :min 1
                    :doc "how many controls one observation may carry"))
  (:tool "permission_canvas_check" (nlk:contract 'gate-arguments) 'run-check)
  (:tool "permission_canvas_observe" (nlk:contract 'observation-arguments) 'run-observe)
  (:tool "permission_canvas_policy" (nlk:contract 'policy-arguments) 'run-policy)
  (:command "permission-canvas" 'run-slash
            :description "Element-level desktop permissions: rules, approvals, the pause"
            :argument-hint "status | pending | approve ID | rule LEVEL key=value ...")
  (:help :permission-canvas
         "permission-canvas: element-level permissions for desktop work - allow, read only, ask first, hide"
         +manual+))
