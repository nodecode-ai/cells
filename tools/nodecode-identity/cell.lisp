;;;; cell.lisp --- the identity files, the profile roster, and a line's last use.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of hermes-identity-plugin: it views and edits the operator's
;;;; identity files, switches and lists profiles, and stamps each line of
;;;; those files with the last time a turn's answer or a tool call repeated
;;;; it, so a line nothing leans on shows up as stale.
;;;;
;;;; The files ride every turn's standing context, as they did under the
;;;; runtime this came from: a stamp is a heuristic read of the model's own
;;;; words, never a count of prompt bytes.

(defpackage #:nodecode-identity
  (:use #:cl)
  ;; IDENTITY is a COMMON-LISP symbol: shadowed so the cell's own name, and
  ;; the condition, the settings variable and the verbs derived from it, are
  ;; this package's.
  (:nicknames #:identity)
  (:shadow #:identity)
  (:export #:start-cell #:stop-cell
           #:view #:edit #:dashboard #:profiles #:switch-profile #:reset-usage))

(in-package #:nodecode-identity)

(nlk:define-peripheral identity :not-running t)

;;; --- the words -----------------------------------------------------------------------------------

(defparameter +files+ '("SOUL.md" "MEMORY.md" "USER.md" "AGENT.md")
  "The four identity files, in the order every list shows them.")

(defparameter +memories-files+ '("MEMORY.md" "USER.md")
  "The two the original kept under a memories/ folder as often as at the root.")

(defparameter +agent-fallback+ "HERMES.md"
  "What an absent AGENT.md is answered by, as the original did.")

(defparameter +edit-modes+ '("overwrite" "replace_line" "replace_section" "append")
  "Every way identity_edit writes: the original's whole enum.")

(defparameter +usage-file+ ".identity-usage.json"
  "Where a line's last use is kept, inside the identity folder itself.")

(defparameter +stop-words+
  '("they" "them" "their" "he" "him" "his" "she" "her" "hers" "it" "its" "we" "us" "our" "you"
    "your" "me" "my" "i" "the" "and" "for" "are" "but" "not" "all" "can" "had" "was" "one" "out"
    "has" "have" "been" "some" "same" "also" "very" "just" "than" "then" "that" "this" "with"
    "will" "what" "when" "where" "which" "who" "how" "why")
  "Values too common to count as the model leaning on a fact line.")

(defparameter +min-value-chars+ 12
  "The shortest value, after a line's colon, that may mark that line used.")

(defparameter +primer+
  "The identity cell keeps four markdown files in `dir` -- SOUL.md (who the
agent is), MEMORY.md (what it keeps), USER.md (who the operator is) and
AGENT.md (how it works; an absent AGENT.md is answered by HERMES.md) -- and
stamps each line with the last time a turn's answer or a tool call repeated
it. The files ride every turn's standing context, so the stamps say which
lines the model actually leans on, and the rest show up as stale.
  (identity:view \"SOUL.md\")   one file, a line a row, each with when it was
      last used. (identity:view \"SOUL.md\" :used-only t) drops the lines
      nothing has used.
  (identity:edit \"SOUL.md\" :mode \"append\" :content \"Name: ...\")   write it:
      mode is overwrite, replace_line (:line 3), replace_section (:section
      \"Facts\") or append; :content is the new text.
  (identity:dashboard)   every file's used and stale count, and with T the
      lines themselves.
  (identity:profiles)   the profiles this machine has, and which have
      identity files. (identity:switch-profile \"work\") makes it the one a
      launch that names none boots into; this process keeps its own.
  (identity:reset-usage)   clear every stamp.
/identity, /identity SOUL.md, /identity all, /identity profiles,
/identity switch NAME and /identity reset answer the same from a shell.
A stamp is a heuristic: a line counts as used when its first 60 characters,
or the value after its colon, appears in what the model said. It sees the
model's words only, never the operator's."
  "What (help :identity) answers while the cell runs.")

;;; --- where a line's last use is kept --------------------------------------------------------------

(defvar *usage* (make-hash-table :test #'equal)
  "One entry per identity-file line: \"SOUL.md:3\" -> the time the model last
repeated it. Emptied on stop, reloaded from the folder on start.")

(defun usage-path ()
  "The usage file, inside the identity folder so one folder is the whole cell's state."
  (merge-pathnames +usage-file+ (setting :dir)))

(defun load-usage ()
  "Every stamp the usage file holds, as a fresh table; an absent or torn one is none."
  (let ((table (make-hash-table :test #'equal))
        (data (ignore-errors (nlk:decode-json (nlk:read-text (usage-path))))))
    (when (hash-table-p data)
      (maphash (lambda (key object)
                 (declare (ignore object))
                 (let ((stamp (nlk:json-value data :integer key)))
                   (when stamp (setf (gethash key table) stamp))))
               data))
    table))

(defun save-usage ()
  "Write every stamp down, making the folder when it is not there."
  (let ((path (usage-path)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                               :if-does-not-exist :create :external-format :utf-8)
      (write-string (nlk:encode-json-object *usage*) out)
      (terpri out))
    path))

(defun line-key (filename index)
  "The usage file's key for line INDEX of FILENAME, as the original wrote it."
  (format nil "~a:~d" filename index))

(defun line-stamp (filename index)
  "When line INDEX of FILENAME was last used, or NIL for never."
  (gethash (line-key filename index) *usage*))

;;; --- a time in words ------------------------------------------------------------------------------

(defun ago (stamp)
  "STAMP as a rough age -- 3d ago -- or never."
  (if (null stamp)
      "never"
      (let ((seconds (max 0 (- (nlk:unix-now) stamp))))
        (cond ((< seconds 60) (format nil "~ds ago" seconds))
              ((< seconds 3600) (format nil "~dm ago" (floor seconds 60)))
              ((< seconds 86400) (format nil "~dh ago" (floor seconds 3600)))
              ((< seconds 604800) (format nil "~dd ago" (floor seconds 86400)))
              ((< seconds 2592000) (format nil "~dw ago" (floor seconds 604800)))
              (t (format nil "~dmo ago" (floor seconds 2592000)))))))

(defun age-word (stamp)
  "STAMP as the original's colour word: green under six hours, yellow under a
day, orange under a week, red beyond it or never."
  (if (null stamp)
      "red"
      (let ((hours (/ (max 0 (- (nlk:unix-now) stamp)) 3600)))
        (cond ((< hours 6) "green")
              ((< hours 24) "yellow")
              ((< hours 168) "orange")
              (t "red")))))

(defun stale-p (stamp)
  "Whether STAMP is older than the section's stale_days, or never."
  (or (null stamp)
      (> (- (nlk:unix-now) stamp) (* 86400 (setting :stale-days)))))

;;; --- the files -----------------------------------------------------------------------------------

(defun file-path (filename)
  "Where FILENAME is: the folder, its memories/ for the two that live there in
some layouts, HERMES.md for an absent AGENT.md, and the folder's own name
when nothing is there yet."
  (let ((dir (setting :dir)))
    (or (probe-file (merge-pathnames filename dir))
        (and (member filename +memories-files+ :test #'equal)
             (probe-file (merge-pathnames (format nil "memories/~a" filename) dir)))
        (and (equal filename "AGENT.md")
             (probe-file (merge-pathnames +agent-fallback+ dir)))
        (merge-pathnames filename dir))))

(defun file-lines (filename)
  "FILENAME's lines, or NIL when the file is not there."
  (let ((path (file-path filename)))
    (when (probe-file path) (nlk:lines (nlk:read-text path)))))

(defun write-text (path text)
  "TEXT into PATH whole, the folder made when it is not there."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                             :if-does-not-exist :create :external-format :utf-8)
    (write-string text out))
  path)

(defun joined-lines (lines)
  "LINES as the file holds them: one newline after each, empty for none."
  (if lines (format nil "~{~a~%~}" lines) ""))

(defun heading-name (line)
  "The words after LINE's one to six leading #s and a space, or NIL."
  (let ((start (position-if-not (lambda (character) (char= character #\#)) line)))
    (when (and start (<= 1 start 6) (< start (length line)) (char= (char line start) #\space))
      (string-trim '(#\Space #\Tab) (subseq line (1+ start))))))

(defun heading-p (line)
  "Whether LINE opens a markdown section."
  (not (null (heading-name line))))

;;; --- the matching --------------------------------------------------------------------------------

(defun painted-value (line)
  "LINE's value: what follows its first colon, its * and white space trimmed."
  (let ((colon (position #\: line)))
    (when colon
      (string-trim '(#\Space #\Tab #\*)
                   (subseq line (1+ colon))))))

(defun line-used-p (line text)
  "Whether TEXT, lowercased, repeats LINE: its first sixty characters, or the
value after its colon when that value is long enough to mean something."
  (let ((snippet (string-downcase (subseq line 0 (min 60 (length line))))))
    (or (and (plusp (length snippet)) (not (null (search snippet text))))
        (let ((value (painted-value line)))
          (and value
               (>= (length value) +min-value-chars+)
               (not (member (string-downcase value) +stop-words+ :test #'equal))
               (not (null (search (string-downcase value) text))))))))

(defun stamp-text (text)
  "Stamp every identity-file line TEXT repeats. The model's words only: the
files ride the prompt every turn, so only what it says or does is a signal."
  (when (and (stringp text) (plusp (length text)) *identity* (setting :track))
    (let ((lower (string-downcase text))
          (changed nil))
      (dolist (filename +files+)
        (loop for line in (file-lines filename)
              for index from 0
              for stripped = (string-trim '(#\Space #\Tab #\Return) line)
              when (and (>= (length stripped) 10)
                        (not (char= (char stripped 0) #\#))
                        (line-used-p stripped lower))
                do (setf (gethash (line-key filename index) *usage*) (nlk:unix-now)
                         changed t)))
      (when changed (save-usage)))))

;;; --- the verbs -----------------------------------------------------------------------------------

(defun refuse (format-control &rest arguments)
  "A tool's refusal: the text, and the call recorded as failed."
  (nle:failure (format nil "ERROR: ~a" (apply #'format nil format-control arguments))))

(define-verb view (filename &key used-only)
  "FILENAME's every line, a stamp each: when the model last repeated it."
  (unless (member filename +files+ :test #'equal)
    (return-from view (refuse "~s is no identity file; the four are ~{~a~^, ~}."
                              filename +files+)))
  (let ((path (file-path filename)))
    (unless (probe-file path)
      (return-from view
        (format nil "~a is not there yet: ~a. identity_edit with mode append writes it."
                filename (namestring path))))
    (let ((lines (file-lines filename)))
      (with-output-to-string (out)
        (format out "=== ~a (~d lines, ~a) ===" filename (length lines) (namestring path))
        (loop for line in lines
              for index from 0
              for stamp = (line-stamp filename index)
              unless (and used-only (null stamp))
                do (format out "~%[~a~a] ~a" (ago stamp)
                           (if (stale-p stamp) " stale" "")
                           line))))))

(define-verb edit (filename &key (mode "replace_line") (content "") line section)
  "Write FILENAME as MODE says: overwrite, replace_line, replace_section or append."
  (unless (member filename +files+ :test #'equal)
    (return-from edit (refuse "~s is no identity file; the four are ~{~a~^, ~}."
                              filename +files+)))
  (unless (member mode +edit-modes+ :test #'equal)
    (return-from edit (refuse "~s is no mode; the four are ~{~a~^, ~}." mode +edit-modes+)))
  (let* ((path (file-path filename))
         (lines (file-lines filename)))
    (macrolet ((wrote (text)
                 `(progn (write-text path ,text)
                         (format nil "~a written (~a): ~d line~:p now."
                                 filename mode (length (file-lines filename))))))
      (cond
        ((equal mode "overwrite") (wrote content))
        ((equal mode "append")
         (ensure-directories-exist path)
         (with-open-file (out path :direction :output :if-exists :append
                                    :if-does-not-exist :create :external-format :utf-8)
           (write-string content out))
         (format nil "~a appended to: ~d line~:p now."
                 filename (length (file-lines filename))))
        ((equal mode "replace_line")
         (unless line (return-from edit (refuse "replace_line needs line: which line to replace")))
         (let ((index (1- line)))
           (unless (<= 0 index (1- (length lines)))
             (return-from edit (refuse "line ~d is out of range: ~a has ~d line~:p."
                                       line filename (length lines))))
           (setf (nth index lines) content)
           (wrote (joined-lines lines))))
        (t
         (let ((heading (string-trim '(#\Space #\Tab) (or section ""))))
           (when (zerop (length heading))
             (return-from edit (refuse "replace_section needs section: which heading to replace")))
           (let ((kept '()) (inside nil) (found nil))
             (dolist (text lines)
               (cond ((and (heading-p text) (string-equal (heading-name text) heading))
                      (push text kept)
                      (push content kept)
                      (setf inside t found t))
                     ((and inside (heading-p text))
                      (setf inside nil)
                      (push text kept))
                     ((not inside) (push text kept))))
             (unless found
               (push (format nil "# ~a" heading) kept)
               (push content kept))
             (wrote (joined-lines (nreverse kept))))))))))

(define-verb dashboard (&optional show-all)
  "Every file's used and stale count, and with SHOW-ALL the lines themselves."
  (let ((total 0) (used 0) (stale 0))
    (with-output-to-string (out)
      (format out "IDENTITY DASHBOARD  profile: ~a  folder: ~a~%~
                   green = under 6h  yellow = under a day  orange = under a week  red = older or never"
              (nlk:profile-label) (namestring (setting :dir)))
      (dolist (filename +files+)
        (let ((lines (file-lines filename)))
          (if (null lines)
              (format out "~2%-- ~a -- missing --" filename)
              (let ((file-used 0))
                (loop for line in lines
                      for index from 0
                      for stamp = (line-stamp filename index)
                      do (incf total)
                         (when stamp (incf used) (incf file-used))
                         (when (stale-p stamp) (incf stale))
                         (when (or show-all (and stamp (not (stale-p stamp))))
                           (format out "~%~8a | ~a"
                                   (if stamp (format nil "~a ~a" (age-word stamp) (ago stamp)) "never")
                                   (let* ((trimmed (string-trim '(#\Space #\Tab) line))
                                          (end (min 80 (length trimmed))))
                                     (if (plusp (length trimmed)) (subseq trimmed 0 end) "(blank)")))))
                (format out "~2%-- ~a (~d of ~d line~:p used) --"
                        filename file-used (length lines))))))
      (format out "~2%Total: ~d of ~d line~:p used; ~d not used in ~d day~:p."
              used total stale (setting :stale-days)))))

(define-verb profile-file-names (home)
  "The identity files PROFILE HOME holds, the four in their declared order."
  (let ((dir (merge-pathnames "identity/" home)))
    (remove nil
            (mapcar (lambda (filename)
                      (and (or (probe-file (merge-pathnames filename dir))
                               (probe-file (merge-pathnames filename home)))
                           filename))
                    +files+))))

(define-verb profiles (&optional only)
  "Every profile this machine has, which is this process's, which a launch
boots into, and which identity files each holds."
  (with-output-to-string (out)
    (format out "current: ~a" (nlk:profile-label))
    (dolist (record (nlk:profiles))
      (let ((name (nlk:profile-record-name record)))
        (when (or (null only) (string-equal (nlk:trimmed only) name))
          (format out "~%~a~@[ (this one)~]~@[ (sticky)~]: ~a" name
                  (nlk:profile-record-current-p record)
                  (nlk:profile-record-sticky-p record)
                  (let ((files (profile-file-names (nlk:profile-record-home record))))
                    (if files (format nil "~{~a~^, ~}" files) "no identity files"))))))))

(define-verb switch-profile (profile)
  "Make PROFILE the one a launch that names none boots into; this process
keeps its own home."
  (let ((name (string-downcase (nlk:trimmed profile))))
    (when (zerop (length name))
      (return-from switch-profile
        (refuse "which profile: this one is ~a, see identity:profiles" (nlk:profile-label))))
    (handler-case
        (progn (nlk:profile-use name)
               (format nil "~a is the profile a new launch boots into; this one keeps ~a."
                       name (nlk:profile-label)))
      (error (condition) (refuse "~a" condition)))))

(define-verb reset-usage ()
  "Clear every stamp this folder holds."
  (let ((count (hash-table-count *usage*)))
    (clrhash *usage*)
    (save-usage)
    (format nil "cleared ~d line stamp~:p." count)))

;;; --- the command ---------------------------------------------------------------------------------

(defun run-slash (args session-id)
  "The answer to one /identity: no word is the dashboard, a file name that
file, and switch, profiles, reset and all are what they say."
  (declare (ignore session-id))
  (let ((words (nlk:split-words args)))
    (nlk:dispatch (string-downcase (or (first words) "")) string=
      ("" (dashboard))
      ("all" (dashboard t))
      ("profiles" (profiles))
      ("reset" (reset-usage))
      ("switch" (switch-profile (or (second words) "")))
      (t (if (member (first words) +files+ :test #'string-equal)
             (view (first words))
             (format nil "~a~2%usage: /identity | /identity SOUL.md | /identity all | /identity profiles | /identity switch NAME | /identity reset"
                     (dashboard)))))))

;;; --- what rides every turn ------------------------------------------------------------------------

(defun prompt-section ()
  "The identity files as one standing section, or NIL when the folder holds
none: what the stamps have to mean."
  (when (and *identity* (setting :prompt))
    (let ((blocks '()))
      (dolist (filename +files+)
        (let ((lines (file-lines filename)))
          (when lines
            (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return)
                                      (format nil "~{~a~%~}" lines))))
              (when (plusp (length text))
                (push (format nil "~a:~2%~a" filename text) blocks))))))
      (when blocks
        (format nil "The operator keeps these identity files: the persona and facts to work from.~2%~{~a~^~2%~}"
                (nreverse blocks))))))

(defun supply-section (next session-id)
  "The four files as a standing <harness> section, replacing one of the same name."
  (let ((sections (funcall next session-id))
        (body (prompt-section)))
    (if body
        (acons "identity" body (remove "identity" sections :key #'car :test #'equal))
        sections)))

;;; --- the hooks -----------------------------------------------------------------------------------

(defun observe-frame (op)
  "A :FRAME observer: one assistant message ends, and what it repeated is stamped."
  (multiple-value-bind (kind payload) (nle::frame-fact op)
    (when (equal kind "turn.assistant_message_completed")
      (let ((text (nle::fact-message-content payload)))
        (when text (stamp-text text))))))

(defun arguments-text (arguments)
  "ARGUMENTS -- a decoded object or a string -- as the one text the model meant."
  (cond ((stringp arguments) arguments)
        ((hash-table-p arguments) (ignore-errors (nlk:encode-json-object arguments)))
        (t (princ-to-string arguments))))

(defun names-a-file-p (text)
  "Whether TEXT names one of the four, which is when a tool call is worth reading."
  (and text (some (lambda (filename) (search filename text)) +files+)))

(defun observe-tool (op next)
  "A :TOOL around: the call lands first, then what its arguments repeated is
stamped -- the original's second signal, for a model that acts rather than says."
  (prog1 (funcall next op)
    (handler-case
        (let ((text (arguments-text (getf op :arguments))))
          (when (names-a-file-p text) (stamp-text text)))
      (error (condition) (warn "identity: ~a" condition)))))

;;; --- the boundaries -------------------------------------------------------------------------------

(nlk:define-contract identity-view
  (:doc "View one identity file with each line's last use: when the model last repeated it.")
  ("filename" :text :required t :options +files+
              :doc "which of the four identity files to read"))

(nlk:define-contract identity-edit
  (:doc "Edit one identity file: overwrite it, replace one line, replace one markdown section, or append.")
  ("filename" :text :required t :options +files+
              :doc "which of the four identity files to write")
  ("mode" :text :default "replace_line" :options +edit-modes+
          :doc "overwrite the file, replace one line, replace a section's body, or append")
  ("content" :string :default ""
             :doc "the new text: what replaces, or what is appended")
  ("line_num" :integer
              :doc "the 1-based line replace_line replaces")
  ("section" :string :default ""
             :doc "the heading whose body replace_section replaces"))

(nlk:define-contract identity-dashboard
  (:doc "How much of the identity files the model uses, a line a row with its last use and stale mark.")
  ("show_all" :boolean
              :doc "every line, not only the ones already used"))

(nlk:define-contract identity-profiles
  (:doc "The profiles this machine has, which is this one and which a launch boots into, each with its identity files.")
  ("profile" :string
             :doc "one profile only, by name; every profile when empty"))

(nlk:define-contract identity-switch
  (:doc "Make a profile the one a launch that names none boots into. This process keeps its own home.")
  ("profile" :text :required t
             :doc "the profile's name, or default"))

(defparameter +reset-schema+
  (list :description "Clear every line stamp the identity folder holds."
        :parameters (nlk:json-object "type" "object" "properties" (nlk:json-object)))
  "identity_reset_usage takes no member but a contract must carry one: the
plist schema is the escape hatch, and the body ignores its arguments.")

;;; --- the tools -----------------------------------------------------------------------------------

(defun view-tool (request)
  "identity_view: the file REQUEST names."
  (view (identity-view-filename request)))

(defun edit-tool (request)
  "identity_edit: the write REQUEST describes."
  (edit (identity-edit-filename request)
        :mode (identity-edit-mode request)
        :content (identity-edit-content request)
        :line (identity-edit-line-num request)
        :section (identity-edit-section request)))

(defun dashboard-tool (request)
  "identity_dashboard: what the model has leaned on, and what has gone stale."
  (dashboard (identity-dashboard-show-all request)))

(defun profiles-tool (request)
  "identity_list_profiles: the roster, or one profile of it."
  (profiles (identity-profiles-profile request)))

(defun switch-tool (request)
  "identity_switch_profile: the sticky profile, which the next launch reads."
  (switch-profile (identity-switch-profile request)))

(defun reset-tool (arguments)
  "identity_reset_usage: every stamp cleared."
  (declare (ignore arguments))
  (reset-usage))

;;; --- the entry -----------------------------------------------------------------------------------

(defun install ()
  "The stamps loaded, and emptied again on stop: what the clauses do not carry."
  (setf *usage* (load-usage))
  (nle:on-stop (lambda () (setf *usage* (make-hash-table :test #'equal)))))

(nle:define-cell identity
  (:section ("identity")
    (:guide "dir is the folder the four identity files live in, the home's identity/ folder by default; a line nothing repeats for stale_days days is listed stale; prompt off keeps the files out of every turn")
    ("dir" :directory :default "identity"
           :doc "the folder holding SOUL.md, MEMORY.md, USER.md and AGENT.md")
    ("stale_days" :integer :default 21 :min 1
                  :doc "a line not repeated for this many days is listed as stale")
    ("track" :boolean :default t
             :doc "stamp a line each time the model's words or a tool call repeat it")
    ("prompt" :boolean :default t
              :doc "the files ride every turn's standing context"))
  (:tool "identity_view" (nlk:contract 'identity-view) 'view-tool)
  (:tool "identity_edit" (nlk:contract 'identity-edit) 'edit-tool)
  (:tool "identity_dashboard" (nlk:contract 'identity-dashboard) 'dashboard-tool)
  (:tool "identity_list_profiles" (nlk:contract 'identity-profiles) 'profiles-tool)
  (:tool "identity_switch_profile" (nlk:contract 'identity-switch) 'switch-tool)
  (:tool "identity_reset_usage" +reset-schema+ 'reset-tool)
  (:help :identity "the four identity files, when each line was last used, and the profile roster" +primer+)
  (:hook :frame (nlk:observer #'observe-frame "identity"))
  (:hook :tool #'observe-tool)
  (:hook 'nle::read-harness-sections #'supply-section)
  (:command "identity" #'run-slash
            :description "The identity files, when each line was last used, and the profiles"
            :argument-hint "SOUL.md | all | profiles | switch NAME | reset")
  (:start #'install))
