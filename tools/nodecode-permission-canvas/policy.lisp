;;;; policy.lisp --- rules, levels, decisions, and what the model may see.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The pure half of the rewrite, and the one place a question about a
;;;; control is answered: which level a control carries, what an observation
;;;; withholds, what a proposed action is worth (allow, read only, one
;;;; approval, hidden), and how much of a window a person has ruled out.
;;;; Nothing here reads a file or knows a tool; FILE-STORE below adds the
;;;; folder, and cell.lisp the tools, the command and the manual.
;;;;
;;;; A control is a plist: :index, :app, :role, :label, :pid, :window-id,
;;;; :bounds (left top width height), :parent (the index of the nearest
;;;; listed ancestor, when the tree reports one). A rule is a plist of the
;;;; same names plus :id, :level, :label-regex and :window-title-regex.
;;;; Every uncertain case answers the more restrictive level, never the
;;;; friendlier one.

(in-package #:nodecode-permission-canvas)

;;; --- the one condition ---------------------------------------------------------

(define-condition permission-canvas-error (error)
  ((text :initarg :text :reader error-text))
  (:report (lambda (condition stream) (write-string (error-text condition) stream)))
  (:documentation "A refusal this cell states in one line: what is missing or unreadable."))

(defun refuse (control-string &rest arguments)
  "Signal PERMISSION-CANVAS-ERROR with ARGUMENTS formatted into CONTROL-STRING."
  (error 'permission-canvas-error :text (apply #'format nil control-string arguments)))

;;; --- levels --------------------------------------------------------------------

(defparameter +levels+ '("green" "blue" "yellow" "black")
  "Operate, observe only, ask once, invisible - least to most restrictive.")

(defun level-p (level)
  "Is LEVEL one of the four?"
  (and (stringp level) (member level +levels+ :test #'string=) t))

(defun coerce-level (level what)
  "LEVEL, or a refusal naming WHAT: a typo must never read as the default level."
  (unless (level-p level)
    (refuse "~a must be green, blue, yellow or black - not ~s" what level))
  level)

(defun level-rank (level)
  "LEVEL's place in +LEVELS+: the higher, the more restrictive."
  (or (position level +levels+ :test #'string=) 0))

(defparameter +non-actionable-roles+
  '("TitleBar" "AXWindow" "Window" "Pane" "AXGroup" "Group" "StaticText"
    "AXStaticText" "Text" "Image" "AXImage" "Separator")
  "Roles that carry no operation, so an unnamed one is not a control to ask about.")

(defun control (index &key app role label pid window-id bounds parent)
  "One control, as the rules and the decisions read it."
  (list :index index :app (or app "") :role (or role "") :label (or label "")
        :pid pid :window-id window-id
        :bounds (and bounds (coerce bounds 'list)) :parent parent))

(defun control-text (control)
  "A control named for a person: its role and its label, never a hidden one's."
  (format nil "~a ~s" (let ((role (or (getf control :role) "")))
                         (if (plusp (length role)) role "control"))
          (getf control :label)))

;;; --- rules ---------------------------------------------------------------------

(defun rule-ids (rules)
  (loop for rule in rules collect (getf rule :id)))

(defun rule-matches-p (rule control window-title)
  "Does RULE narrow to CONTROL? A scope field the driver did not report cannot
rule a rule out, so it applies - except for a green rule, which needs the
evidence: a missing field never widens what an agent may operate."
  (let ((level (getf rule :level))
        (app (or (getf control :app) "")))
    (and (let ((wanted (getf rule :app)))
           (or (null wanted)
               (if (plusp (length app))
                   (string= wanted app)
                   (not (string= level "green")))))
         (let ((wanted (getf rule :role)))
           (or (null wanted) (string= wanted (or (getf control :role) ""))))
         (let ((wanted (getf rule :label)))
           (or (null wanted) (string= wanted (or (getf control :label) ""))))
         (let ((regex (getf rule :label-regex)))
           (or (null regex) (and (ppcre:scan regex (or (getf control :label) "")) t)))
         (let ((regex (getf rule :window-title-regex)))
           (or (null regex)
               (if (and window-title (plusp (length window-title)))
                   (and (ppcre:scan regex window-title) t)
                   (not (string= level "green")))))
         (loop for key in '(:pid :window-id)
               always (let ((wanted (getf rule key))
                            (actual (getf control key)))
                        (or (null wanted)
                            (if actual
                                (equal wanted actual)
                                (not (string= level "green")))))))))

(defun policy-rules (policy)
  "POLICY's rules, decoded into plists."
  (loop for object across (nlk:json-array policy "rules")
        collect (list :id (nlk:json-value object :string "id")
                      :level (nlk:json-value object :string "level")
                      :app (nlk:json-value object :string "app")
                      :role (nlk:json-value object :string "role")
                      :label (nlk:json-value object :string "label")
                      :label-regex (nlk:json-value object :string "label_regex")
                      :window-title-regex (nlk:json-value object :string "window_title_regex")
                      :pid (nlk:json-value object :integer "pid")
                      :window-id (nlk:json-value object :integer "window_id"))))

(defun policy-level (policy key fallback)
  "POLICY's own KEY, or FALLBACK when it does not carry one."
  (or (nlk:json-value policy :string key) fallback))

(defun policy-blocked-apps (policy)
  (coerce (nlk:json-array policy "blocked_apps") 'list))

(defun policy-window-title-regex (policy)
  (nlk:json-value policy :string "hide_window_title_regex"))

(defun policy-require-target-p (policy)
  (nlk:json-value policy :boolean "require_target"))

(defun policy-target (policy)
  "The person's selected window, or NIL."
  (let ((object (nlk:json-value policy :object "target_window")))
    (and object
         (list :pid (nlk:json-value object :integer "pid")
               :window-id (nlk:json-value object :integer "window_id")
               :app (nlk:json-value object :string "app")
               :title (nlk:json-value object :string "title")))))

(defun policy-paused-p (policy)
  (nlk:json-value policy :boolean "paused"))

(defun hides-text-p (policy text)
  "Does the person's title pattern withhold TEXT?"
  (let ((regex (policy-window-title-regex policy)))
    (and regex (stringp text) (plusp (length text))
         (ignore-errors (ppcre:scan regex text)) t)))

;;; --- one control's level -------------------------------------------------------

(defun app-restricted-p (policy app)
  "Does the app carry any restriction at all? An unnamed control in such a window
is not assumed harmless."
  (or (member (or app "") (policy-blocked-apps policy) :test #'string=)
      (not (string= (policy-level policy "default_level" "yellow") "green"))
      (loop for rule in (policy-rules policy)
            thereis (and (not (string= (getf rule :level) "green"))
                         (let ((wanted (getf rule :app)))
                           (or (null wanted) (zerop (length (or app "")))
                               (string= wanted app)))))))

(defun effective-level (policy control window-title)
  "CONTROL's level and the rule that decided it. The most restrictive match
wins; a match with no scope is not a wildcard for a green rule."
  (let ((app (or (getf control :app) "")))
    (if (member app (policy-blocked-apps policy) :test #'string=)
        (values "black" nil)
        (let ((best nil) (best-rule nil))
          (dolist (rule (policy-rules policy))
            (when (and (rule-matches-p rule control window-title)
                       (or (null best) (> (level-rank (getf rule :level)) (level-rank best))))
              (setf best (getf rule :level)
                    best-rule rule)))
          (cond (best (values best best-rule))
                ((and (zerop (length (string-trim " " (or (getf control :label) ""))))
                      (not (member (or (getf control :role) "") +non-actionable-roles+
                                   :test #'string=))
                      (app-restricted-p policy app))
                 (values (policy-level policy "unidentified_level" "yellow") nil))
                (t (values (policy-level policy "default_level" "yellow") nil)))))))

;;; --- what an observation withholds ---------------------------------------------

(defun placed-p (control)
  "Does CONTROL carry bounds an image mask could use?"
  (let ((bounds (getf control :bounds)))
    (and bounds
         (= 4 (length bounds))
         (every #'integerp bounds)
         (plusp (third bounds))
         (plusp (fourth bounds)))))

(defun overlaps-p (a b)
  (and a b
       (plusp (min (third a) (third b) (fourth a) (fourth b)))
       (< (first a) (+ (first b) (third b)))
       (> (+ (first a) (third a)) (first b))
       (< (second a) (+ (second b) (fourth b)))
       (> (+ (second a) (fourth a)) (second b))))

(defun ancestors (control by-index)
  "CONTROL's indices up to the root, nearest first; a cycle stops it."
  (loop with index = (getf control :parent)
        with seen = (list (getf control :index))
        with chain = '()
        repeat 64
        while (and (integerp index) (not (member index seen)))
        do (push index chain)
           (push index seen)
           (setf index (getf (gethash index by-index) :parent))
        finally (return chain)))

(defun observation (policy controls window-title)
  "What a person's rules withhold from one observation of a window.
Answers three values: the visible controls, the withheld ones, and the
rectangles to mask. A control is withheld when it is hidden itself, when it
descends from something hidden, when it overlaps a hidden rectangle, or when
it has no geometry to prove it lies clear of one."
  (let* ((by-index (make-hash-table :test #'eql))
         (seeds '()))
    (dolist (control controls)
      (setf (gethash (getf control :index) by-index) control)
      (when (or (string= "black" (effective-level policy control window-title))
                (hides-text-p policy (getf control :label)))
        (push control seeds)))
    (let* ((seed-indices (mapcar (lambda (control) (getf control :index)) seeds))
           (protected (append seeds
                              (remove-if-not (lambda (control)
                                               (intersection (ancestors control by-index) seed-indices))
                                             controls)))
           (rectangles (remove-duplicates
                        (loop for control in protected when (placed-p control) collect (getf control :bounds))
                        :test #'equal))
           (withheld (make-hash-table :test #'eql)))
      (dolist (control protected)
        (setf (gethash (getf control :index) withheld) t))
      (dolist (control controls)
        (let ((index (getf control :index)))
          (cond ((placed-p control)
                 (when (some (lambda (rectangle) (overlaps-p (getf control :bounds) rectangle))
                             rectangles)
                   (setf (gethash index withheld) t)))
                ((or (intersection (ancestors control by-index)
                                   (loop for key being the hash-keys of withheld collect key))
                     (and seeds (null (getf control :parent))))
                 (setf (gethash index withheld) t)))))
      (values (remove-if (lambda (control) (gethash (getf control :index) withheld)) controls)
              (remove-if-not (lambda (control) (gethash (getf control :index) withheld)) controls)
              rectangles))))

;;; --- a policy file's own shape -------------------------------------------------

(defun regex-problem (regex what)
  "NIL when REGEX compiles, else the one line to refuse with."
  (and regex
       (handler-case (progn (ppcre:create-scanner regex) nil)
         (error (condition) (format nil "~a is not a regular expression: ~a" what condition)))))

(defun validate-policy (policy)
  "POLICY as decoded JSON, checked member by member: the file a person edits is
refused by name rather than read into a decision nobody expected."
  (unless (hash-table-p policy)
    (refuse "policy.json must be a JSON object"))
  (let ((default (policy-level policy "default_level" "yellow"))
        (unidentified (policy-level policy "unidentified_level" "yellow")))
    (coerce-level default "default_level")
    (coerce-level unidentified "unidentified_level")
    (let ((problem (regex-problem (policy-window-title-regex policy) "hide_window_title_regex")))
      (when problem (refuse "policy.json: ~a" problem)))
    (let ((rules (policy-rules policy))
          (seen '()))
      (dolist (rule rules)
        (let ((id (getf rule :id)))
          (unless (and id (plusp (length id)))
            (refuse "policy.json: a rule has no id"))
          (when (member id seen :test #'string=)
            (refuse "policy.json: two rules share the id ~a" id))
          (push id seen)
          (coerce-level (getf rule :level) (format nil "rule ~a's level" id))
          (dolist (key '(:label-regex :window-title-regex))
            (let ((problem (regex-problem (getf rule key) (format nil "rule ~a's ~(~a~)" id key))))
              (when problem (refuse "policy.json: ~a" problem))))
          (dolist (key '(:pid :window-id))
            (let ((value (getf rule key)))
              (when (and value (or (not (integerp value)) (not (plusp value))))
                (refuse "policy.json: rule ~a's ~(~a~) must be a positive integer" id key))))))
      (when (and (policy-require-target-p policy) (null (policy-target policy)))
        (refuse "policy.json: require_target is on and no target_window is selected - /permission-canvas target PID WINDOW_ID APP TITLE sets one")))
    policy))

(defun canonical-json (value)
  "VALUE written with object members sorted, so a digest does not move with a
hash table's order."
  (cond ((hash-table-p value)
         (format nil "{~{~a~^,~}}"
                 (loop for key in (sort (loop for key being the hash-keys of value collect key) #'string<)
                       append (list (json-quote key) (canonical-json (gethash key value))))))
        ((and (vectorp value) (not (stringp value)))
         (format nil "[~{~a~^,~}]" (loop for item across value collect (canonical-json item))))
        ((eq value :null) "null")
        ((stringp value) (json-quote value))
        ((eq value t) "true")
        ((null value) "false")
        ((integerp value) (format nil "~d" value))
        ((realp value) (format nil "~f" value))
        (t (json-quote (princ-to-string value)))))

(defun json-quote (text)
  "TEXT as a JSON string literal."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for character across text
          do (case character
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (#\Return (write-string "\\r" out))
               (#\Tab (write-string "\\t" out))
               (t (write-char character out))))
    (write-char #\" out)))

(defun policy-digest (policy)
  "Short, stable digest of the whole policy: an approval given for one policy is
not an approval for the next edit of it."
  (subseq (nlk:sha256-text (canonical-json policy)) 0 16))

(defun control-fingerprint (control)
  "What a person approved, independent of an observation's element index and of
where it sat on the screen."
  (subseq (nlk:sha256-text
           (format nil "~a~c~a~c~s~c~s~c~s"
                   (or (getf control :app) "") #\Nul
                   (or (getf control :role) "") #\Nul
                   (or (getf control :label) "") #\Nul
                   (getf control :pid) #\Nul
                   (getf control :window-id)))
          0 16))

(defun approval-signature (action control arguments policy)
  "The exact operation an approval is for: one action, one control, one argument
set, one policy."
  (nlk:sha256-text (format nil "~a~c~a~c~s~c~a"
                            action #\Nul (control-fingerprint control) #\Nul
                            arguments #\Nul (policy-digest policy))))

;;; --- the folder ---------------------------------------------------------------

(defun expand-path (text)
  "TEXT as a pathname: ~/ against the home folder, a relative one against the
process's own directory."
  (let ((text (string-trim " " (or text ""))))
    (cond ((zerop (length text)) nil)
          ((and (> (length text) 1) (char= #\/ (char text 0)) (char= #\~ (char text 1)))
           (merge-pathnames (subseq text 2) (user-homedir-pathname)))
          ((char= #\/ (char text 0)) (pathname text))
          (t (merge-pathnames text (uiop:getcwd))))))

(defun data-directory ()
  "The folder the section names, created if it is not there yet."
  (let ((dir (expand-path (setting :dir))))
    (unless dir
      (refuse "permission-canvas: set dir first - the folder that holds policy.json, approvals.json and audit.jsonl"))
    (ensure-directories-exist (uiop:ensure-directory-pathname dir))))

(defun data-file (name)
  (merge-pathnames name (data-directory)))

(defun write-file-atomic (path text)
  "TEXT into PATH through a sibling temporary file, so a reader sees the old
file or the new one and never half of either."
  (let* ((directory (uiop:pathname-directory-pathname path))
         (temporary (merge-pathnames (format nil ".~a.tmp" (file-namestring path)) directory)))
    (with-open-file (out temporary :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string text out))
    (uiop:rename-file-overwriting-target temporary path)
    path))

(defun read-json-file (path what)
  "PATH decoded, or a refusal naming WHAT when it is not JSON."
  (handler-case (nlk:decode-json (uiop:read-file-string path))
    (error (condition) (refuse "permission-canvas: ~a is not JSON: ~a" what condition))))

(defun policy-defaults ()
  "The policy a folder with no policy.json starts from: the section's own
default level, nothing selected, nothing hidden, not paused."
  (nlk:json-object
   "default_level" (setting :default-level)
   "unidentified_level" (setting :unidentified-level)
   "blocked_apps" #()
   "require_target" nil
   "paused" nil
   "rules" #()))

(defun policy-load ()
  "The policy in force. A missing file is the defaults; an unreadable one
refuses, because a decision made from half a policy is not a decision."
  (let ((path (data-file "policy.json")))
    (if (probe-file path)
        (validate-policy (read-json-file path "policy.json"))
        (policy-defaults))))

(defun policy-save (policy)
  (validate-policy policy)
  (write-file-atomic (data-file "policy.json") (nlk:encode-json-object policy))
  policy)

;;; --- approvals -----------------------------------------------------------------

(defstruct (ledger (:constructor make-ledger))
  "One approval store: what is waiting for a person, what a person allowed
once, and what a person refused outright."
  (pending (make-hash-table :test #'equal))
  (approved (make-hash-table :test #'equal))
  (denied (make-hash-table :test #'equal)))

(defun ledger-object (table)
  "TABLE itself, which the encoder writes as one JSON object; empty means {}."
  table)

(defun ledger-load ()
  (let ((path (data-file "approvals.json"))
        (ledger (make-ledger)))
    (when (probe-file path)
      (let ((object (read-json-file path "approvals.json")))
        (when (hash-table-p object)
          (dolist (pair '(("pending" . pending) ("approved" . approved) ("denied" . denied)))
            (let ((source (nlk:json-value object :object (car pair))))
              (when source
                (maphash (lambda (key value)
                           (setf (gethash key (slot-value ledger (cdr pair))) value))
                         source)))))))
    ledger))

(defun ledger-save (ledger)
  (write-file-atomic (data-file "approvals.json")
                     (nlk:encode-json-object
                      (nlk:json-object "pending" (ledger-object (ledger-pending ledger))
                                       "approved" (ledger-object (ledger-approved ledger))
                                       "denied" (ledger-object (ledger-denied ledger)))))
  ledger)

(defun now ()
  (get-universal-time))

(defun ledger-prune (ledger)
  "Drop what has expired: an approval is for a moment, not for a session."
  (flet ((prune (table)
           (let ((moment (now)))
             (maphash (lambda (key value)
                        (let ((expires (cond ((hash-table-p value)
                                              (nlk:json-value value :integer "expires"))
                                             ((integerp value) value))))
                          (when (and expires (< expires moment))
                            (remhash key table))))
                      table))))
    (prune (ledger-pending ledger))
    (prune (ledger-approved ledger))
    (prune (ledger-denied ledger)))
  ledger)

(defun pending-ids (ledger)
  (sort (loop for key being the hash-keys of (ledger-pending ledger) collect key) #'string<))

(defun audit! (event)
  "One line in audit.jsonl: every decision this cell made, in order, with no
label of anything hidden."
  (ignore-errors
    (with-open-file (out (data-file "audit.jsonl") :direction :output
                                                :if-exists :append
                                                :if-does-not-exist :create)
      (format out "~a~%" (nlk:encode-json-object event))))
  event)
