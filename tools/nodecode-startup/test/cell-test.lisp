;;;; cell-test.lisp --- the lifecycle, the table, the filters, one entry in
;;;; full, the two parsers, and the command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring over a fixture machine: that
;;;; a start installs the two tools, the command and the manual and a stop
;;;; takes them back; that the table lists every entry of every source with
;;;; its scope and its enabled/disabled verdict; that each filter narrows it
;;;; and says so; that approval records left behind by entries that are gone
;;;; are reported, never guessed into a verdict; that the detail resolves an
;;;; unquoted command with a blank in its path the way a loader does, and says
;;;; when the binary is gone; that a registry query's three columns and a
;;;; StartupApproved blob are read correctly; that an XDG desktop file is an
;;;; entry, its field codes dropped and its own keys obeyed; that a call
;;;; without a name is refused before the body runs; and that /startup answers
;;;; the table, one entry, and the payload.

(in-package #:nodecode.test)

;;; --- lifecycle ------------------------------------------------------------------

(define-cell-lifecycle-tests "startup"
  (:command "startup")
  (:help :startup)
  (:running (is (nle::find-tool "startup_table") "the table tool is registered")
            (is (nle::find-tool "startup_detail") "the detail tool is registered")
            (is (= 100 (nodecode-startup::setting :max-rows)) "max_rows defaults to 100")
            (is (member (nodecode-startup:platform) '(:windows :darwin :linux)) "the platform is one of the three the cell reads"))
  (:stopped (is (null (nle::find-tool "startup_table")) "stop unregisters the tools"))
  (:refused ("max_rows" 0))
  (:idle nodecode-startup:startup-error (nodecode-startup:startup-table)))

;;; --- the table -------------------------------------------------------------------

(deftest startup-cell-the-table-lists-every-entry-and-what-was-off ()
  (with-cell-stop ((startup-start))
    (with-probe ((startup-entries))
      (let ((payload (startup-answer "startup_table")))
        (is (nlk:json-value payload :boolean "ok") "the tool answers ok")
        (is (equal '("OneDrive" "SecurityHealth" "CC Switch" "spare.lnk")
                   (startup-names payload)) "every entry from every source, in the order read")
        (is (equal "enabled" (nlk:json-value (startup-row payload "OneDrive") :string "enabled")) "an entry with no record runs, which is the platform's default")
        (is (equal "disabled" (nlk:json-value (startup-row payload "CC Switch") :string "enabled")) "the switched-off one is reported switched off")
        (is (equal "2026-01-02T03:04:05Z"
                   (nlk:json-value (startup-row payload "SecurityHealth") :string "changed")) "with when that decision was made")
        (is (equal "machine" (nlk:json-value (startup-row payload "SecurityHealth") :string "scope")) "and whether it is machine- or user-scoped")
        (is (search "1 switched off" (or (startup-note payload "switched off") "")) "a note counts what was switched off")))))

(deftest startup-cell-the-filters-narrow-the-table-and-say-so ()
  (with-cell-stop ((startup-start))
    (with-probe ((startup-entries))
      (is (equal '("CC Switch") (startup-names (nodecode-startup::table-payload :query "cc"))) "a query matches a name")
      (is (equal '("OneDrive") (startup-names (nodecode-startup::table-payload :query "onedrive"))) "case-insensitively")
      (is (equal '("SecurityHealth")
                 (startup-names (nodecode-startup::table-payload :scope "machine"))) "scope picks one side")
      (is (= 3 (length (startup-names (nodecode-startup::table-payload :include-disabled nil)))) "include_disabled=false hides what was switched off")
      (let ((payload (nodecode-startup::table-payload :limit 2)))
        (is (= 2 (length (startup-names payload))) "limit bounds the rows")
        (is (startup-note payload "limit=2") "and the answer says where it stopped"))
      (is (startup-note (nodecode-startup::table-payload :query "nowhere")
                        "nothing matched") "an empty answer says how to widen it"))))

(deftest startup-cell-leftover-approvals-are-reported-not-guessed ()
  (with-cell-stop ((startup-start))
    (with-probe ((startup-entries)
                 :orphans (list (list :name "gone.exe" :source "folder-user"
                                      :state "unknown (state byte 1)" :changed nil)))
      (let* ((payload (nodecode-startup::table-payload))
             (left (nlk:json-array payload "leftover_approvals")))
        (is (= 1 (length (coerce left 'list))) "the record nobody owns is listed")
        (is (equal "unknown (state byte 1)" (nlk:json-value (aref left 0) :string "state")) "its state byte is reported raw, never guessed into a verdict")
        (is (startup-note payload "gone") "and a note says what those records are")))))

;;; --- one entry in full ------------------------------------------------------------

(deftest startup-cell-the-detail-resolves-the-binary-and-says-when-it-is-gone ()
  (with-cell-stop ((startup-start))
    (let ((nodecode-startup::*file-exists-p* (startup-on-disk '("/opt/CC Switch/cc-switch"))))
      (with-probe ((list (startup-entry "cc-switch" "/opt/CC Switch/cc-switch --minimized"
                                         "xdg-user" "user")
                         (startup-entry "ghost" "/opt/ghost/ghost --quiet"
                                        "xdg-user" "user")))
        (let* ((payload (nodecode-startup::detail-payload "cc-switch"))
               (binary (nlk:json-value payload :object "binary")))
          (is (equal "/opt/CC Switch/cc-switch" (nlk:json-value binary :string "path")) "an unquoted path with a blank resolves to the longest prefix that is a file")
          (is (nlk:json-value binary :boolean "present") "and it is on disk"))
        (let* ((payload (nodecode-startup::detail-payload "ghost"))
               (binary (nlk:json-value payload :object "binary")))
          (is (null (nlk:json-value binary :boolean "present")) "a binary that is gone is reported as gone")
          (is (find "not on disk" (coerce (nlk:json-array payload "notes") 'list)
                    :test #'search) "and the answer says so in words")))))
  (with-cell-stop ((startup-start))
    (with-probe ((startup-entries))
      (let ((payload (nodecode-startup::detail-payload "cc switch")))
        (is (equal "disabled" (nlk:json-value payload :string "enabled")) "the detail keeps the switched-off verdict")
        (is (find "switched off" (coerce (nlk:json-array payload "notes") 'list)
                  :test #'search) "and says it does not run, though it is still registered")))))

(deftest startup-cell-a-name-that-is-not-registered-answers-the-hint ()
  (with-cell-stop ((startup-start))
    (with-probe ((startup-entries))
      (let ((payload (startup-answer "startup_detail" "name" "nowhere")))
        (is (null (nlk:json-value payload :boolean "ok")))
        (is (equal "not_registered" (nlk:json-value payload :string "code")))
        (is (search "startup_table lists" (nlk:json-value payload :string "hint")) "the answer names the tool that lists the real names")))))

(deftest startup-cell-a-call-without-a-name-is-refused ()
  (is (signals-error nlk:contract-refusal
        (nlk:contract-decode (nlk:contract 'nodecode-startup::startup-detail-call)
                              (nlk:make-json-object))) "the contract refuses a payload with no name")
  (with-cell-stop ((startup-start))
    (with-probe ((startup-entries))
      (let ((text (string-downcase (startup-raw "startup_detail"))))
        (is (search "name" text) "and a tool call is answered with the refusal, naming the member")))))

;;; --- the two parsers -----------------------------------------------------------------

(deftest startup-cell-reg-query-and-approval-bytes-are-read ()
  (let ((value (nodecode-startup::parse-reg-line
                (format nil "    CC Switch    REG_SZ    C:/Program Files/CC Switch/cc-switch.exe --minimized"))))
    (is (equal "CC Switch" (getf value :name)) "a name with a blank in it is one column")
    (is (equal "REG_SZ" (getf value :type)) "the type is the second")
    (is (search "--minimized" (getf value :data)) "the command keeps its own blanks")
    (is (null (nodecode-startup::parse-reg-line "HKEY_CURRENT_USER\\Software\\Run")) "a key header is not a value"))
  (is (equal "enabled" (getf (nodecode-startup::decode-approval "020000000000000000000000") :state)) "state byte 2 is an entry left on")
  (is (equal "disabled" (getf (nodecode-startup::decode-approval "060000000000000000000000") :state)) "state byte 6 is one switched off in Task Manager")
  (is (equal "unknown (state byte 1)"
             (getf (nodecode-startup::decode-approval "010000000000000000000000") :state)) "any other byte is reported, not guessed")
  (is (null (getf (nodecode-startup::decode-approval "020000000000000000000000") :changed)) "a zero FILETIME is no date at all")
  (let ((stamp 3900000000))
    (is (equal (nlk:iso-time (- stamp 2208988800))
               (getf (nodecode-startup::decode-approval (startup-approval-blob 2 stamp))
                     :changed)) "a real FILETIME reads as when the person made the decision")))

(deftest startup-cell-the-registry-half-reads-run-values-and-folder-files ()
  (let* ((home (startup-fixture-directory "home"))
         (folder (startup-windows-folder home)))
    (startup-plant folder "spare.lnk" "not really a shortcut")
    (let ((nodecode-startup::*platform* :windows)
          (nodecode-startup::*home* (uiop:ensure-directory-pathname home))
          (nodecode-startup::*reg-query* (startup-windows-reg-query)))
      (multiple-value-bind (entries orphans) (nodecode-startup::windows-probe)
        (let ((registry (remove-if-not (lambda (entry)
                                         (uiop:string-prefix-p "registry-" (getf entry :source)))
                                       entries)))
          (is (equal '("OneDrive" "CC Switch")
                     (mapcar (lambda (entry) (getf entry :name)) registry)) "every Run value of every key is one entry")
          (is (equal "enabled" (nodecode-startup::enabled-state (first registry))) "the entry with no record runs")
          (is (equal "disabled" (nodecode-startup::enabled-state (second registry))) "the entry with a switched-off record does not"))
        (is (= 2 (length (nodecode-startup::windows-folder-sources))) "the two Startup folders are the sources this half reads")
        (is (= 1 (length orphans)) "the record whose entry is gone is the orphan")))))

(deftest startup-cell-a-startup-folder-file-is-an-entry ()
  (let* ((directory (startup-fixture-directory "folder"))
         (folder (merge-pathnames "Microsoft/Windows/Start Menu/Programs/Startup/"
                                   (uiop:ensure-directory-pathname directory))))
    (startup-plant (namestring folder) "spare.lnk" "a shortcut, here")
    (startup-plant (namestring folder) "desktop.ini" "folder customization, not a launch")
    (let ((entries (nodecode-startup::folder-entries (namestring folder) "folder-user" "user")))
      (is (= 1 (length entries)) "desktop.ini is skipped, the shortcut is an entry")
      (is (equal "spare.lnk" (getf (first entries) :name)) "named by its file")
      (is (equal "folder-user" (getf (first entries) :source)) "and says where it came from")
      (is (equal "user" (getf (first entries) :scope)))
      (is (getf (getf (first entries) :file) :size) "with its size on disk"))))

(deftest startup-cell-an-xdg-desktop-file-is-an-entry ()
  (let* ((directory (startup-fixture-directory "xdg"))
         (path (startup-plant directory "cc-switch.desktop"
                               (format nil "[Desktop Entry]~%Type=Application~%Name=CC Switch~%~
Exec=/opt/CC Switch/cc-switch --minimized %U~%X-GNOME-Autostart-enabled=false~%"))))
    (let* ((facts (nodecode-startup::desktop-facts path))
           (entry (nodecode-startup::desktop-entry (pathname path) facts "xdg-user" "user"
                                                    "cc-switch.desktop")))
      (is (equal "CC Switch" (getf entry :name)) "the Name key names it")
      (is (equal "disabled" (nodecode-startup::enabled-state entry)) "X-GNOME-Autostart-enabled=false is switched off")
      (is (search "/opt/CC Switch/cc-switch --minimized" (getf entry :command)) "the Exec line is the command")
      (is (null (search "%U" (getf entry :command))) "and its field codes are dropped")
      (is (getf entry :file) "a file entry carries its own size and change")))
  (let* ((path (startup-plant (startup-fixture-directory "xdg-hidden") "hidden.desktop"
                               (format nil "[Desktop Entry]~%Name=Hidden One~%Exec=/usr/bin/hidden~%~
Hidden=true~%")))
         (entry (nodecode-startup::desktop-entry (pathname path)
                                                 (nodecode-startup::desktop-facts path)
                                                 "xdg-system" "machine" "hidden.desktop")))
    (is (equal "disabled" (nodecode-startup::enabled-state entry)) "Hidden=true is switched off too")))

;;; --- the command ----------------------------------------------------------------------

(deftest startup-cell-the-command-answers-the-table-one-entry-and-the-payload ()
  (with-cell-stop ((startup-start))
    (with-probe ((startup-entries))
      (let ((text (nodecode-startup::run-slash "" nil)))
        (is (search "OneDrive" text) "the table names the entries")
        (is (search "registry-hkcu-run" text) "and where each came from"))
      (is (search "CC Switch" (nodecode-startup::run-slash "cc switch" nil)) "a name answers that entry in full")
      (let ((payload (nlk:decode-json (nodecode-startup::run-slash "--json" nil))))
        (is (nlk:json-value payload :boolean "ok") "--json answers the whole table payload"))
      (let ((payload (nlk:decode-json (nodecode-startup::run-slash "nowhere --json" nil))))
        (is (equal "not_registered" (nlk:json-value payload :string "code")) "and an unknown name answers the same not-found shape")))))
