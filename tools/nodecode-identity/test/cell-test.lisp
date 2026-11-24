;;;; cell-test.lisp --- the six tools, the command, the stamps, the standing section.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: that the cell starts and stops and leaves nothing
;;;; behind; that each tool writes and reads the folder it is given; that a
;;;; line the model repeats is stamped, a tool call that names a file is read,
;;;; and a stamp outlives a restart; that the files ride the standing
;;;; context while `prompt' says so; that the command answers every word; and
;;;; that a bad threshold refuses at start. Offline, every file a fixture.

(in-package #:nodecode.test)

(define-test-slice "identity" "IDENTITY-CELL-" :start nodecode-identity:start-cell)

;;; --- lifecycle ------------------------------------------------------------------------------------

(define-cell-lifecycle-tests "identity"
  (:fixture with-temp-trees ((dir "identity-cell")))
  (:config "dir" (namestring dir))
  (:hooks :frame :tool 'nle::read-harness-sections)
  (:help :identity)
  (:command "identity")
  (:running (is (search "IDENTITY DASHBOARD" (nodecode-identity::run-slash "" nil)) "the command answers the dashboard")
            (is (hash-table-p nodecode-identity::*usage*) "and the stamps are a table while it runs"))
  (:stopped (is (zerop (hash-table-count nodecode-identity::*usage*)) "a stop empties the stamps"))
  (:refused ("stale_days" 0) ("stale_days" "fortnight"))
  (:idle nodecode-identity::identity-error (nodecode-identity::run-slash "" nil)))

;;; --- a folder of one's own, and the tools over it --------------------------------------------------

(defmacro with-identity ((dir &rest config) &body body)
  "BODY over a fresh identity folder DIR, the cell started on it and stopped on unwind."
  `(with-temp-trees ((,dir "identity-cell"))
     (with-cell-stop ((identity-start "dir" (namestring ,dir) ,@config))
       ,@body)))

(defun identity-text (&rest lines)
  "LINES as a file holds them: one newline after each -- CL's string reader
has no backslash-n escape, so a fixture says it this way."
  (format nil "~{~a~%~}" lines))

(defparameter +tools+
  ;; The tool's wire name, the body that answers it, and the contract its
  ;; arguments cross: the table a call is made through.
  '(("identity-view" "VIEW-TOOL" "identity-view")
    ("identity-edit" "EDIT-TOOL" "identity-edit")
    ("identity-dashboard" "DASHBOARD-TOOL" "identity-dashboard")
    ("identity-list-profiles" "PROFILES-TOOL" "identity-profiles")
    ("identity-switch" "SWITCH-TOOL" "identity-switch")
    ("identity-reset-usage" "RESET-TOOL" nil))
  "Every tool the cell registers, by wire name.")

(defun identity-record (name &rest pairs)
  "A tool's boundary value: the contract NAME materialized from PAIRS."
  (apply (intern (format nil "MAKE-~:@(~a~)" name) (find-package "NODECODE-IDENTITY")) pairs))

(defun identity-stamp-p (filename index)
  "When line INDEX of FILENAME was last used, as the cell keeps it."
  (nodecode-identity::line-stamp filename index))

(defun identity-call (name &rest pairs)
  "The tool called NAME on PAIRS, which is the whole of a model's call to it."
  (let* ((row (or (assoc name +tools+ :test #'string=)
                   (error "~s is no tool of the cell: ~{~a~^, ~}" name (mapcar #'first +tools+))))
         (fn (intern (second row) (find-package "NODECODE-IDENTITY"))))
    (if (third row)
        (funcall fn (apply #'identity-record (third row) pairs))
        (funcall fn (nlk:make-json-object pairs)))))

(deftest identity-cell-every-tool-answers-over-a-real-folder ()
  (with-identity (dir)
    (is (search "appended" (identity-call "identity-edit" :filename "SOUL.md" :mode "append"
                                          :content (identity-text "Name: Indigo Karasu" "Role: keeper of the house"))) "identity_edit appends to a file that was not there")
    (is (search "Indigo Karasu"
                (identity-call "identity-view" :filename "SOUL.md")) "identity_view reads it back")
    (is (search "written (replace_line)"
                (identity-call "identity-edit" :filename "SOUL.md" :mode "replace_line"
                               :line-num 1 :content "Name: Indigo Karasu the keeper")) "replace_line names the mode it wrote in")
    (is (search "Indigo Karasu the keeper"
                (uiop:read-file-string (merge-pathnames "SOUL.md" dir))) "and the file holds the new line")
    (is (search "written"
                (identity-call "identity-edit" :filename "SOUL.md" :mode "replace_section"
                               :section "Facts" :content (identity-text "A fact worth keeping" "Another one"))) "replace_section writes a heading that was not there")
    (is (search "# Facts"
                (uiop:read-file-string (merge-pathnames "SOUL.md" dir))) "the heading is in the file")
    (is (search "written (overwrite)"
                (identity-call "identity-edit" :filename "SOUL.md" :mode "overwrite"
                               :content (identity-text "# Facts" "One fact only"))) "and overwrite replaces the whole file")
    (is (equal (identity-text "# Facts" "One fact only")
               (uiop:read-file-string (merge-pathnames "SOUL.md" dir))) "which the file now holds whole")
    (is (search "IDENTITY DASHBOARD" (identity-call "identity-dashboard")) "identity_dashboard opens with its legend and profile")
    (is (search "current:" (identity-call "identity-list-profiles")) "identity_list_profiles names this process's profile")
    (is (search "profile" (identity-call "identity-switch" :profile "default")) "identity_switch_profile answers about a profile")
    (is (search "cleared" (identity-call "identity-reset-usage")) "identity_reset_usage says what it cleared")
    (is (search "no identity file"
                (identity-call "identity-view" :filename "notes.txt")) "a file outside the four is refused")))

(deftest identity-cell-a-missing-file-says-how-to-make-it ()
  (with-identity (dir)
    (is (search "not there yet" (identity-call "identity-view" :filename "USER.md")) "a file nothing wrote reads as absent, not as an error")))

;;; --- the stamps -----------------------------------------------------------------------------------

(deftest identity-cell-a-line-the-model-repeats-is-stamped ()
  (with-identity (dir)
    (identity-call "identity-edit" :filename "SOUL.md" :mode "append"
                   :content (identity-text "Name: Indigo Karasu" "Role: keeper of a household ledger"))
    (cell-publish "nodecode-identity"
                  (cell-frame-op "turn.assistant_message_completed"
                                 :payload (nlk:json-object "message"
                                                            "I am Indigo Karasu, and I keep the ledger.")))
    (is (identity-stamp-p "SOUL.md" 0) "the value line the answer repeats is stamped")
    (is (not (identity-stamp-p "SOUL.md" 1)) "a line it did not repeat is not")
    (is (search "green" (identity-call "identity-dashboard")) "the dashboard shows it as used just now")
    (is (search "never" (identity-call "identity-dashboard" :show-all t)) "and the unrepeated line as never")))

(deftest identity-cell-a-tool-call-that-names-a-file-is-read ()
  (with-identity (dir)
    (identity-call "identity-edit" :filename "SOUL.md" :mode "append"
                   :content (identity-text "Name: Indigo Karasu" "Role: keeper of a household ledger"))
    (is (equal "ok"
               (funcall (cell-hook "nodecode-identity" :tool)
                        (list :name "read_file" :call-id "c1"
                              :arguments (nlk:json-object "path" "SOUL.md"
                                                           "text" "ask Indigo Karasu about it"))
                        (lambda (op) (declare (ignore op)) "ok"))) "the call lands untouched")
    (is (identity-stamp-p "SOUL.md" 0) "and the line its arguments repeat is stamped")
    (is (equal "ok" (funcall (cell-hook "nodecode-identity" :tool)
                             (list :name "run_shell" :call-id "c2"
                                   :arguments (nlk:json-object "command" "ls"))
                             (lambda (op) (declare (ignore op)) "ok"))) "a call that names no identity file stamps nothing")
    (is (equal 1 (hash-table-count nodecode-identity::*usage*)) "and adds no stamp")))

(deftest identity-cell-a-stamp-outlives-a-restart ()
  (with-temp-trees ((dir "identity-cell"))
    (let ((pairs (list "dir" (namestring dir))))
      (with-cell-stop ((apply #'identity-start pairs))
        (identity-call "identity-edit" :filename "SOUL.md" :mode "append"
                       :content (identity-text "Name: Indigo Karasu"))
        (cell-publish "nodecode-identity"
                      (cell-frame-op "turn.assistant_message_completed"
                                     :payload (nlk:json-object "message" "Indigo Karasu speaking"))))
      (with-cell-stop ((apply #'identity-start pairs))
        (is (identity-stamp-p "SOUL.md" 0) "the stamp the last run kept is read back")
        (is (probe-file (merge-pathnames ".identity-usage.json" dir)) "from the folder's own usage file")))))

(deftest identity-cell-an-old-stamp-is-marked-stale ()
  (with-identity (dir "stale_days" 1)
    (identity-call "identity-edit" :filename "SOUL.md" :mode "append"
                   :content (identity-text "Name: Indigo Karasu"))
    (setf (gethash (nodecode-identity::line-key "SOUL.md" 0) nodecode-identity::*usage*)
          (- (nlk:unix-now) (* 3 86400)))
    (is (search "stale" (identity-call "identity-view" :filename "SOUL.md")) "three days old is stale under a one-day threshold")
    (is (search "not used in 1 day" (identity-call "identity-dashboard")) "and the dashboard counts it")))

;;; --- the standing section and the command ---------------------------------------------------------

(deftest identity-cell-the-files-ride-the-standing-context ()
  (with-identity (dir)
    (let ((empty (lambda (session-id) (declare (ignore session-id)) '())))
      (is (null (funcall (cell-hook "nodecode-identity" 'nle::read-harness-sections) empty "s1")) "an empty folder contributes no section")
      (identity-call "identity-edit" :filename "SOUL.md" :mode "append"
                     :content (identity-text "Name: Indigo Karasu"))
      (let ((sections (funcall (cell-hook "nodecode-identity" 'nle::read-harness-sections)
                                empty "s1")))
        (is (equal "identity" (car (assoc "identity" sections :test #'equal))) "one identity section")
        (is (search "Indigo Karasu" (cdr (assoc "identity" sections :test #'equal))) "carrying the file whole")
        (is (= 1 (count "identity" (funcall (cell-hook "nodecode-identity" 'nle::read-harness-sections)
                                             (lambda (session-id)
                                               (declare (ignore session-id))
                                               (list (cons "identity" "old")))
                                             "s1")
                        :key #'car :test #'equal)) "replacing one of its own name")))))

(deftest identity-cell-the-command-answers-each-word ()
  (with-identity (dir)
    (identity-call "identity-edit" :filename "USER.md" :mode "append"
                   :content (identity-text "Callsign: the night operator"))
    (is (search "IDENTITY DASHBOARD" (nodecode-identity::run-slash "" nil)) "no word: the dashboard")
    (is (search "Callsign" (nodecode-identity::run-slash "USER.md" nil)) "a file name: that file")
    (is (search "never" (nodecode-identity::run-slash "all" nil)) "all: every line")
    (is (search "current:" (nodecode-identity::run-slash "profiles" nil)) "profiles: the roster")
    (is (search "usage:" (nodecode-identity::run-slash "selenium" nil)) "anything else: the usage line")
    (is (search "cleared" (nodecode-identity::run-slash "reset" nil)) "reset: the stamps")))
