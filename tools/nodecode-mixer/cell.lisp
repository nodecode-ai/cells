;;;; cell.lisp --- the mixer: one gate on the EVAL call, and the /mixer desk.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Four dials the operator sets in the `mixer' section or turns with
;;;; /mixer: EDITS (auto, ask, plan), SCOPE (a glob allow-list of writable
;;;; paths), LINES (a per-turn budget of added+removed lines, shared by
;;;; everything one turn does) and DEPS (allow, ask, block for a new
;;;; dependency). One :TOOL hook reads the call before it runs; the stages
;;;; are ordered plan, scope, deps, lines, ask, and the first denial wins.
;;;;
;;;; A speed bump, not a sandbox: the gate reads the writes and the commands
;;;; the form shows. A command built out of a variable, a script it runs, or a
;;;; Lisp call nobody listed can still write. What it shows, it refuses.

(in-package #:nodecode-mixer)

(nlk:define-peripheral mixer :not-running t)

(defvar *session* nil
  "The session the running call belongs to: the turn's, or the one a
caller named. The dials are read for it, and its overrides are its own.")

;;; --- the operator's answer -------------------------------------------------------

(defparameter +allow-once+ "Allow once"
  "Answer that runs this one call.")
(defparameter +always-here+ "Allow this file"
  "Answer that runs this call and every later one on the same target.")
(defparameter +deny+ "Deny"
  "Answer that refuses the call.")

(defun ask-operator-function ()
  "The core's ASK-OPERATOR, or NIL in a core that has none."
  (let ((symbol (find-symbol "ASK-OPERATOR" '#:nodecode.evolved)))
    (and symbol (fboundp symbol) (symbol-function symbol))))

(defun call-shown (text)
  "What the question shows beneath it: the call, cut to a screenful."
  (if (> (length text) 1500) (concatenate 'string (subseq text 0 1500) " ...") text))

(defun refusal-text (decision answer why)
  "The model's words for a call the operator would not let run."
  (format nil "ERROR: ~a~@[ The operator chose ~a.~]~@[ Nobody was there to answer, so it was refused.~]~@[ No answer came in time, so it was refused.~] The call did not run; do not retry it or work around it - ask the operator what they want instead."
          (getf decision :text)
          (and (equal answer +deny+) +deny+)
          (eq why :nobody)
          (eq why :timeout)))

(defun ask-then-run (decision op next)
  "Ask the operator about DECISION; run the call on an allow, refuse it on
anything else."
  (let ((ask (ask-operator-function)))
    (if (null ask)
        (nle:failure (refusal-text decision nil :nobody))
        (multiple-value-bind (answer why)
            (funcall ask (getf decision :text)
                     :detail (call-shown (or (getf op :form-text) ""))
                     :choices (list +allow-once+ +always-here+ +deny+)
                     :seconds (max 5 (or (setting :ask-seconds) 120)))
          (cond
            ((equal answer +allow-once+) (funcall next op))
            ((equal answer +always-here+)
             (approve *session* (getf decision :key))
             (funcall next op))
            (t (nle:failure (refusal-text decision answer why))))))))

;;; --- the gate --------------------------------------------------------------------

(defun gate (op next)
  "The :TOOL point: the dials read the call before it runs, and answer the
refusal when they stop it. Nothing is judged when the call shows no write and
runs no command."
  (let* ((arguments (getf op :arguments))
         (form (and (hash-table-p arguments) (gethash "form" arguments)))
         (strings (and (hash-table-p arguments) (gethash "strings" arguments)))
         (text (if (stringp form) form (format nil "~s" form)))
         (turn (nle:turn))
         (*session* (or (getf turn :session-id) *session*))
         (request (read-request text strings)))
    (if (and (null (request-writes request))
             (null (request-commands request))
             (not (request-patch-p request)))
        (funcall next op)
        (let ((decision (decide request *session* (session-root *session*)
                                 (dials-for *session*) (getf turn :turn-id))))
          (cond ((null decision) (funcall next op))
                ((eq (getf decision :action) :deny) (nle:failure (getf decision :text)))
                (t (ask-then-run (list* :form-text text decision) op next)))))))

;;; --- presets: the same four changes, bundled --------------------------------------

(defparameter +presets+
  '(("builder" :edits "auto" :scope nil :lines 0 :deps "allow")
    ("auditor" :edits "plan" :scope nil :lines 0 :deps "ask")
    ("explorer" :edits "plan" :scope nil :lines 0 :deps "allow")
    ("surgical" :edits "auto" :scope ("src/**" "tests/**") :lines 30 :deps "ask"))
  "Named bundles of all four dials. A preset pins the bundle; it does not
nudge what differs.")

(defun preset-names ()
  (mapcar #'first +presets+))

(defun preset-list ()
  (with-output-to-string (out)
    (format out "Presets (apply with /mixer preset <name>):~%")
    (dolist (preset +presets+)
      (format out "  ~a~12t edits=~a, scope=~a, lines=~a, deps=~a~%"
              (first preset)
              (getf (rest preset) :edits)
              (let ((scope (getf (rest preset) :scope)))
                (if scope (format nil "~{~a~^, ~}" scope) "(no limit)"))
              (let ((lines (getf (rest preset) :lines)))
                (if (plusp lines) lines "off"))
              (getf (rest preset) :deps)))))

(defun apply-preset (name session-id)
  "Apply one preset to SESSION-ID's dials, all four of them."
  (let ((preset (assoc (string-downcase name) +presets+ :test #'equal)))
    (if (null preset)
        (format nil "mixer: unknown preset '~a'. Presets: ~{~a~^, ~}. See one with /mixer preset show."
                name (preset-names))
        (progn
          (loop for (key value) on (rest preset) by #'cddr
                do (set-override session-id key (if (listp value) (copy-list value) value)))
          (format nil "mixer: preset ~a applied.~%~a" (first preset) (board session-id))))))

;;; --- the board, and the grammar ---------------------------------------------------

(defun session-name (session-id)
  (or session-id "(no session)"))

(defun dial-mark (session-id key)
  "Where the value came from: the session, the saved defaults."
  (if (overridden-p session-id key) "  (session)" "  (saved)"))

(defun board (session-id)
  "The desk as it stands: every dial, where its value comes from, and how
much of the line budget this turn has left."
  (let* ((dials (dials-for session-id))
         (lines (getf dials :lines))
         (scope (getf dials :scope)))
    (with-output-to-string (out)
      (format out "mixer ~a~%" (session-name session-id))
      (format out "  edits  ~a~a~%" (getf dials :edits) (dial-mark session-id :edits))
      (format out "  scope  ~a~a~%"
              (if scope (format nil "~{~a~^, ~}" scope) "(no limit)")
              (dial-mark session-id :scope))
      (format out "  lines  ~a~a~%"
              (if (plusp lines)
                  (format nil "~a per turn, ~a used this turn, ~a left"
                          lines (lines-used session-id) (max (- lines (lines-used session-id)) 0))
                  "off")
              (dial-mark session-id :lines))
      (format out "  deps   ~a~a~%" (getf dials :deps) (dial-mark session-id :deps))
      (format out "/mixer edits|scope|lines|deps|preset|reset|save - (help :mixer)~%"))))

(defparameter +grammar+
  "The grammar: /mixer, /mixer edits ask|auto|plan, /mixer scope add <glob>|remove <glob>|clear|show, /mixer lines <N>|off, /mixer deps allow|ask|block, /mixer preset <name>|show, /mixer reset, /mixer save [<dial> <value>].")

(defun needs-session (session-id)
  "The refusal a dial change answers when no session is bound, or NIL."
  (and (null session-id)
       "mixer: cannot tell which session this is, so nothing changed; /mixer save <dial> <value> sets the saved default instead."))

(defun mixer-edits (words session-id)
  (or (needs-session session-id)
      (let ((value (string-downcase (or (first words) ""))))
        (if (member value '("ask" "auto" "plan") :test #'equal)
            (progn
              (set-override session-id :edits value)
              (format nil "mixer: edits ~a for this session.~%~a" value (board session-id)))
            (format nil "mixer: edits takes ask, auto or plan.~%~a" (board session-id))))))

(defun mixer-scope (words session-id)
  (or (needs-session session-id)
      (let* ((action (string-downcase (or (first words) "show")))
             (glob (second words))
             (current (copy-list (or (getf (dials-for session-id) :scope) '()))))
        (cond
          ((equal action "add")
           (cond ((null glob) (format nil "mixer: scope add takes a glob.~%~a" (board session-id)))
                 (t (pushnew glob current :test #'equal)
                    (set-override session-id :scope current)
                    (format nil "mixer: scope ~{~a~^, ~}.~%~a" current (board session-id)))))
          ((equal action "remove")
           (set-override session-id :scope (remove glob current :test #'equal))
           (format nil "mixer: scope ~{~a~^, ~}.~%~a"
                   (or (override session-id :scope) '("(no limit)")) (board session-id)))
          ((equal action "clear")
           (set-override session-id :scope nil)
           (format nil "mixer: scope cleared; no limit.~%~a" (board session-id)))
          (t (format nil "mixer: scope ~{~a~^, ~}.~%~a"
                     (or current '("(no limit)")) (board session-id)))))))

(defun mixer-lines (words session-id)
  (or (needs-session session-id)
      (let ((value (string-downcase (or (first words) ""))))
        (cond
          ((or (equal value "off") (equal value "0"))
           (set-override session-id :lines 0)
           (format nil "mixer: lines off for this session.~%~a" (board session-id)))
          ((every #'digit-char-p value)
           (set-override session-id :lines (parse-integer value))
           (format nil "mixer: lines ~a per turn for this session.~%~a" (parse-integer value) (board session-id)))
          (t (format nil "mixer: lines takes a number of lines, or off.~%~a" (board session-id)))))))

(defun mixer-deps (words session-id)
  (or (needs-session session-id)
      (let ((value (string-downcase (or (first words) ""))))
        (if (member value '("allow" "ask" "block") :test #'equal)
            (progn
              (set-override session-id :deps value)
              (format nil "mixer: deps ~a for this session.~%~a" value (board session-id)))
            (format nil "mixer: deps takes allow, ask or block.~%~a" (board session-id))))))

(defun mixer-reset (session-id)
  (or (needs-session session-id)
      (progn
        (remhash session-id *overrides*)
        (format nil "mixer: this session's overrides are gone; the saved defaults stand.~%~a"
                (board session-id)))))

;;; --- save: the section, written through the core --------------------------------

(defun config-writer ()
  "The core's CONFIG-SET, or NIL in a core that has none."
  (let ((symbol (find-symbol "CONFIG-SET" '#:nodecode.evolved)))
    (and symbol (fboundp symbol) (symbol-function symbol))))

(defun write-saved (key value)
  "Write one saved default to the `mixer' section, and answer what to say
about it. The section is the config's own, so a restart reads it back."
  (let ((writer (config-writer)))
    (cond ((null writer) (format nil "mixer: ~a saved for this run (this core cannot write the config)." key))
          (t (handler-case
                 (progn (funcall writer (list "mixer" (string-downcase (string key)))
                                 (case key
                                   (:lines (or value 0))
                                   (:scope (coerce (or value '()) 'vector))
                                   (t (or value ""))))
                        (format nil "mixer: ~a saved in the mixer section." key))
               (error (condition)
                 (format nil "mixer: ~a saved for this run; the config write failed: ~a" key condition)))))))

(defun save-dial (key value)
  "Set one saved default, in memory and in the config."
  (let ((plist *saved*))
    (setf (getf plist key) value)
    (setf *saved* plist))
  (write-saved key value))

(defun mixer-save (words session-id)
  (let ((dial (and words (intern (string-upcase (string-downcase (first words))) :keyword))))
    (cond
      ((null words)
       (let ((dials (dials-for session-id)))
         (format nil "~a~%~a~%~a~%~a"
                 (save-dial :edits (getf dials :edits))
                 (save-dial :scope (getf dials :scope))
                 (save-dial :lines (getf dials :lines))
                 (save-dial :deps (getf dials :deps)))))
      ((not (member dial '(:edits :scope :lines :deps)))
       (format nil "mixer: save takes edits, scope, lines or deps.~%~a" (board session-id)))
      (t
       (let ((value (second words)))
         (case dial
           (:edits
            (if (member (string-downcase (or value "")) '("ask" "auto" "plan") :test #'equal)
                (save-dial :edits (string-downcase value))
                "mixer: edits takes ask, auto or plan."))
           (:deps
            (if (member (string-downcase (or value "")) '("allow" "ask" "block") :test #'equal)
                (save-dial :deps (string-downcase value))
                "mixer: deps takes allow, ask or block."))
           (:lines
            (cond ((null value) "mixer: lines takes a number, or off.")
                  ((equal (string-downcase value) "off") (save-dial :lines 0))
                  ((every #'digit-char-p value) (save-dial :lines (parse-integer value)))
                  (t "mixer: lines takes a number, or off.")))
           (:scope
            (if (member (string-downcase (or value "")) '("add" "remove" "clear") :test #'equal)
                (let* ((action (string-downcase value))
                       (target (third words))
                       (current (copy-list (or (getf *saved* :scope) '()))))
                  (case (intern (string-upcase action) :keyword)
                    (:add (pushnew target current :test #'equal)
                          (save-dial :scope current))
                    (:remove (save-dial :scope (remove target current :test #'equal)))
                    (:clear (save-dial :scope nil))))
                (save-dial :scope (remove "" (rest words) :test #'equal))))))))))

;;; --- the command -----------------------------------------------------------------

(defun mixer-command (args session-id)
  "The /mixer desk: the board, one dial at a time, a preset, reset or save.
Typed alone it draws the board."
  (let* ((text (string-trim '(#\Space #\Tab) (or args "")))
         (words (split-words text)))
    (flet ((answer (text) (values text nil)))
      (cond
        ((null words) (answer (board session-id)))
        ((member (string-downcase (first words)) '("board" "show") :test #'equal)
         (answer (board session-id)))
        ((string-equal (first words) "edits") (answer (mixer-edits (rest words) session-id)))
        ((string-equal (first words) "scope") (answer (mixer-scope (rest words) session-id)))
        ((string-equal (first words) "lines") (answer (mixer-lines (rest words) session-id)))
        ((string-equal (first words) "deps") (answer (mixer-deps (rest words) session-id)))
        ((string-equal (first words) "reset") (answer (mixer-reset session-id)))
        ((string-equal (first words) "save") (answer (mixer-save (rest words) session-id)))
        ((string-equal (first words) "preset")
         (let ((name (string-downcase (or (second words) ""))))
           (cond ((or (null (second words)) (equal name "show")) (answer (preset-list)))
                 ((needs-session session-id) (answer (needs-session session-id)))
                 (t (answer (apply-preset name session-id))))))
        (t (answer (format nil "mixer: unknown '~a'. ~a" (first words) +grammar+)))))))

(defun mixer-complete (text session-id)
  "What the menu offers while /mixer is typed: the dials, then their values."
  (declare (ignore session-id))
  (let* ((words (split-words (or text "")))
         (first-word (string-downcase (or (first words) ""))))
    (cond
      ((or (null words) (and (null (rest words)) (not (find #\Space (or text "")))))
       (mapcar (lambda (word) (list :name word :value word))
               '("edits" "scope" "lines" "deps" "preset" "reset" "save")))
      ((string-equal first-word "edits")
       (mapcar (lambda (word) (list :name word :value (format nil "edits ~a" word)))
               '("ask" "auto" "plan")))
      ((string-equal first-word "deps")
       (mapcar (lambda (word) (list :name word :value (format nil "deps ~a" word)))
               '("allow" "ask" "block")))
      ((string-equal first-word "scope")
       (mapcar (lambda (word) (list :name word :value (format nil "scope ~a" word)))
               '("add" "remove" "clear" "show")))
      ((string-equal first-word "lines")
       (mapcar (lambda (word) (list :name word :value (format nil "lines ~a" word)))
               '("30" "100" "off")))
      ((string-equal first-word "preset")
       (mapcar (lambda (word) (list :name word :value (format nil "preset ~a" word)))
               (append (preset-names) '("show"))))
      (t nil))))

;;; --- the manual -------------------------------------------------------------------

(defparameter +manual+
  (format nil "mixer: four dials on how much a call may change, read before it runs. The board is /mixer; each value says session or saved.~%~%~
  edits  auto|ask|plan   auto lets a write go; ask puts a card up (Allow once, Allow this file, Deny) before it; plan is read-only, and the shell is limited to commands that write nothing (ls, cat, grep, rg, find without -delete/-exec, git status/log/diff/show, and so on).~%~
  scope  globs           an allow-list of the paths that may be written. Relative globs anchor at the session's folder (/mixer scope add src/**); a glob starting with / matches anywhere. Empty means no limit. A shell command's redirects, tee, sed -i, cp/mv/ln destinations, touch, rm, mkdir and dd of= are read too, best effort.~%~
  lines  N|off           added plus removed lines one turn may change, counted from the file as it is now. A call that would go over is refused, naming what is left; an approval cannot override it. The budget resets at the turn boundary.~%~
  deps   allow|ask|block new dependencies: a name a manifest edit adds (package.json, pyproject.toml, requirements*.txt, Cargo.toml, go.mod), or a package an install command names (npm/pnpm/yarn/bun add, pip install, uv add, poetry add, cargo add, go get). Bumps, removals, reorders and restores pass.~%~%~
  /mixer edits ask|auto|plan, /mixer scope add <glob>|remove <glob>|clear|show, /mixer lines <N>|off, /mixer deps allow|ask|block, /mixer preset builder|auditor|explorer|surgical|show, /mixer reset, /mixer save [<dial> <value>].~%~%~
  It is a speed bump, not a sandbox: the gate reads the writes and commands a call's form shows, so a command built from a variable, a script a command runs, or Lisp that writes by hand can still write. Every refusal names the dial and the fix.")
  "What (help :mixer) answers.")

(nle:define-cell mixer
  (:section ("mixer")
    (:guide "edits is auto, ask or plan; scope is a glob allow-list of writable paths (empty means no limit); lines is a per-turn budget of added and removed lines, 0 turning it off; deps is allow, ask or block for a new dependency")
    ("edits" :choice :options '("ask" "auto" "plan") :default "auto"
     :doc "ask waits for approval before a write; auto lets it go; plan is read-only")
    ("scope" :list
     :doc "glob allow-list for writes: relative globs anchor at the session's folder, /... globs match anywhere; empty means no limit")
    ("lines" :integer :default 0 :min 0
     :doc "lines (added plus removed) one turn may change; 0 turns the budget off")
    ("deps" :choice :options '("allow" "ask" "block") :default "ask"
     :doc "what happens when a change adds a new package")
    ("ask_seconds" :integer :default 120 :min 5
     :doc "how long an approval question waits for an answer"))
  (:hook :tool #'gate)
  (:help :mixer "mixer: four dials on how much a call may change - edit permission, write scope, a per-turn line budget and new dependencies; /mixer draws the desk" +manual+)
  (:command "mixer" 'mixer-command
            :description "The four dials: edits, scope, lines, deps"
            :argument-hint "[edits|scope|lines|deps|preset|reset|save] ..."
            :complete #'mixer-complete)
  (:start (lambda ()
            ;; The saved defaults are the section as it read at start; /mixer
            ;; save moves them, and every session without an override follows.
            (setf *saved* (list :edits (setting :edits)
                                 :scope (or (setting :scope) '())
                                 :lines (or (setting :lines) 0)
                                 :deps (setting :deps)))
            (clrhash *overrides*)
            (clrhash *approvals*)
            (clrhash *lines-used*)
            (clrhash *turn-of-session*)
            (nle:on-stop (lambda ()
                           (setf *saved* nil)
                           (clrhash *overrides*)
                           (clrhash *approvals*)
                           (clrhash *lines-used*)
                           (clrhash *turn-of-session*))))))
