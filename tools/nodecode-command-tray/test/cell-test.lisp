;;;; cell-test.lisp --- lifecycle, what a run leaves in the tray, /commands.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, not a shell: the config
;;;; surface (a row count of 0 refuses the start), the two tools through the
;;;; real dispatch seam against a stubbed shell, the same command updating its
;;;; row instead of adding one, the list bounded by max_items, a token-shaped
;;;; string redacted, the file surviving a restart, /commands, and a verb
;;;; refusing while the cell is not running.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "command-tray"
  (:hooks 'nodecode.evolved:sh)
  (:help :command-tray)
  (:command "commands")
  (:running
   (dolist (name '("command_tray_list" "command_tray_clear"))
     (is (nle::find-tool name) (format nil "the ~a tool is registered" name))))
  (:stopped
   (dolist (name '("command_tray_list" "command_tray_clear"))
     (is (null (nle::find-tool name)) (format nil "stop unregisters ~a" name))))
  (:refused
   ("max_items" 0)
   ("preview_chars" -1))
  (:idle nodecode-command-tray:command-tray-error (command-tray:runs)))


;;; --- what a run leaves behind ----------------------------------------------------

(deftest command-tray-cell-a-run-lands-with-its-verdict ()
  (with-cell-stop ((command-tray-start))
    (command-tray:clear)
    (let ((answer (tray-shell "pytest -q" "collected 3 items
3 passed" 0 :directory "/work/project")))
      (is (equal "collected 3 items
3 passed" answer) "the command's own answer is untouched"))
    (tray-shell "make deploy" "nothing to be done
ERROR: no such target" 2)
    (let ((text (tray-call-tool "command_tray_list" "{}")))
      (is (search "pytest -q" text) "the command is listed")
      (is (search "ok (0)" text) "with its verdict and exit code")
      (is (search "fail (2)" text) "and a failure is listed too")
      (is (search "3 passed" text) "the last non-empty output line is kept")
      (is (not (search "collected 3 items" text)) "not the whole output")
      (is (search "/work/project" text) "the directory the call named"))
    (is (= 2 (length (tray-rows))) "two rows, in the order they ran")
    (is (equal "pytest -q" (nlk:json-value (first (tray-rows)) :string "command")))))

(deftest command-tray-cell-secrets-are-redacted-and-a-preview-is-bounded ()
  (with-cell-stop ((command-tray-start "preview_chars" 24))
    (command-tray:clear)
    (tray-shell "curl -H 'Authorization: Bearer abc123' https://example.test"
                "done token=ghp_abcdefghijklmnopqrstuv" 0)
    (let* ((row (first (tray-rows)))
           (command (nlk:json-value row :string "command"))
           (preview (nlk:json-value row :string "preview")))
      (is (search "[redacted]" command) "a bearer header is masked in the command")
      (is (not (search "abc123" command)))
      (is (search "[redacted]" preview) "and in the kept output line")
      (is (not (search "ghp_" preview)))
      (is (<= (length preview) 24) "the preview is cut to preview_chars")
      (is (search "https://example.test" command) "the rest of the command is kept"))))

(deftest command-tray-cell-the-same-command-updates-and-the-list-is-bounded ()
  (with-cell-stop ((command-tray-start "max_items" 2))
    (command-tray:clear)
    (tray-shell "one" "1" 0)
    (tray-shell "two" "2" 0)
    (tray-shell "one" "again" 1)
    (let ((rows (tray-rows)))
      (is (= 2 (length rows)) "a command run twice is one row")
      (is (equal "two" (nlk:json-value (first rows) :string "command")) "the older row stays first")
      (is (equal "one" (nlk:json-value (second rows) :string "command")) "the rerun moves to the end")
      (is (= 2 (nlk:json-value (second rows) :integer "runs")) "and counts its runs")
      (is (equal "fail" (nlk:json-value (second rows) :string "status")) "the newest verdict stands"))
    (tray-shell "three" "3" 0)
    (let ((rows (tray-rows)))
      (is (= 2 (length rows)) "max_items bounds the list")
      (is (equal "three" (nlk:json-value (second rows) :string "command")))
      (let ((json (tray-call-tool "command_tray_list" "{\"markdown\":false}")))
        (is (not (search "two" json)) "the row that fell off is gone from the JSON answer too")))))

(deftest command-tray-cell-the-tray-outlives-a-restart-and-clear-empties-it ()
  (with-cell-stop ((command-tray-start))
    (command-tray:clear)
    (tray-shell "ls -la" "total 0" 0))
  (with-cell-stop ((command-tray-start))
    (is (= 1 (length (tray-rows))) "the file is the tray that comes back at the next start")
    (is (equal "ls -la" (nlk:json-value (first (tray-rows)) :string "command")))
    (is (search "cleared" (command-tray:clear)) "clear says what it did")
    (is (null (tray-rows)) "and the tray is empty")
    (is (equal nodecode-command-tray:*empty* (tray-call-tool "command_tray_list" "{}")) "the empty tray answers its own words")))

;;; --- the slash command ------------------------------------------------------------

(deftest command-tray-cell-the-slash-command-lists-clears-and-explains ()
  (with-cell-stop ((command-tray-start))
    (command-tray:clear)
    (tray-shell "npm test" "ok" 0)
    (is (search "npm test" (cell-entry "nodecode-command-tray" "commands" "")) "/commands lists the tray")
    (is (search "npm test" (cell-entry "nodecode-command-tray" "commands" "list")))
    (let ((usage (cell-entry "nodecode-command-tray" "commands" "frobnicate")))
      (is (search "Usage" usage) "a word it does not know is told what /commands takes"))
    (is (search "cleared" (cell-entry "nodecode-command-tray" "commands" "clear")))
    (is (null (tray-rows)) "and the tray is empty after it")))
