;;;; cell-test.lisp --- the two tools, the command, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring and reading, not Windows: the
;;;; section, the two tools through the real dispatch seam against canned
;;;; PowerShell output, the filters and the limits, and the answer a query that
;;;; failed gives. Nothing runs a command and nothing reads this machine.

(in-package #:nodecode.test)

;;; --- lifecycle -------------------------------------------------------------------

(define-cell-lifecycle-tests "windows-updates"
  (:config "powershell" "powershell.exe")
  (:help :windows-updates)
  (:command "windows-updates")
  (:running
   (is (nle::find-tool "win_update_history") "win_update_history is registered")
   (is (nle::find-tool "win_hotfix_table") "win_hotfix_table is registered")
   (is (equal "powershell.exe" (nodecode-windows-updates::setting :powershell)) "the executable the section names")
   (is (= 90 (nodecode-windows-updates::setting :timeout-seconds)) "and the bound a query gets"))
  (:stopped
   (is (null (nle::find-tool "win_update_history")) "stop unregisters the tools")
   (is (null (nle::find-tool "win_hotfix_table")) "both of them"))
  (:refused
   ("timeout_seconds" 0)
   ("powershell" "")))

(deftest windows-updates-cell-a-blank-executable-says-what-to-set ()
  ;; The one setting a query cannot work without: a section naming no
  ;; executable refuses at start, in words that name the key.
  (let ((condition (signals-error nlk:config-refusal (windows-updates-start "powershell" ""))))
    (is (search "no PowerShell executable" (princ-to-string condition)) "the refusal says what is missing")
    (is (search "powershell.exe" (princ-to-string condition)) "and what a good one is"))
  (is (null (nle::find-tool "win_update_history")) "and nothing was installed"))

;;; --- the history tool --------------------------------------------------------------

(defun wu-meaning (row)
  "ROW's outcome in words."
  (nlk:json-value (and (hash-table-p row) (gethash "result" row)) :string "meaning"))

(defun wu-hex (row)
  "ROW's failure code in 0x form."
  (nlk:json-value (and (hash-table-p row) (gethash "hresult" row)) :string "hex"))

(defun wu-noted (text phrase)
  "PHRASE as one of the notes the tool answer TEXT carries, or NIL."
  (let ((notes (value-of text "notes")))
    (and notes (find phrase (coerce notes 'list) :test #'search))))

(deftest windows-updates-cell-the-history-answers-in-words ()
  (with-updates ()
    (with-powershell (:history +history+)
      (let* ((text (wu-call-tool "win_update_history" "{}"))
             (rows (rows-of-answer text "updates")))
        (is (eq t (value-of text "ok")) "the call answers ok")
        (is (= 3 (length rows)) "every entry the script wrote is a row")
        (is (equal "2026-09-09T10:11:00Z" (text-of (first rows) "date")) "newest first, with its date")
        (is (equal "it succeeded" (wu-meaning (first rows))) "the outcome is in words")
        (is (equal "it failed" (wu-meaning (second rows))) "a failed update says so")
        (is (equal "0x80240034" (wu-hex (second rows))) "the signed HRESULT in the unsigned 0x form")
        (is (equal "0x00000000" (wu-hex (first rows))) "a clean update keeps a zero code")
        (is (equal "it was aborted" (wu-meaning (third rows))) "an aborted entry says aborted")
        (is (null (text-of (third rows) "date")) "an entry with no date carries none")
        (is (= 2 (length (values (nlk:json-value (second rows) :array "categories")))) "categories travel as a list")
        (is (wu-noted text "3 of 3 matching update(s) listed, 723 in the history") "one note counts the rows and the history")
        (is (wu-noted text "2 of the newest 3 did not end clean") "and names the ones that did not end clean")
        (is (search "NODECODE_WU_TAKE=50" (princ-to-string (getf (stub-call) :environment))) "the row count reaches the script through the environment")))))

(deftest windows-updates-cell-only-failures-filters-the-window ()
  (with-updates ()
    (with-powershell (:history +history+)
      (let* ((text (wu-call-tool "win_update_history" "{\"only_failures\":true}"))
             (rows (rows-of-answer text "updates")))
        (is (= 2 (length rows)) "the clean update is left out")
        (is (equal "it failed" (wu-meaning (first rows))) "the failed one is here")
        (is (equal "it was aborted" (wu-meaning (second rows))) "and the aborted one")
        (is (null (wu-noted text "did not end clean")) "and the note about them would be noise")))))

(deftest windows-updates-cell-a-query-and-a-count-narrow-it ()
  (with-updates ()
    (with-powershell (:history +history+)
      (let* ((text (wu-call-tool "win_update_history" "{\"query\":\"defender\"}"))
             (rows (rows-of-answer text "updates")))
        (is (= 1 (length rows)) "the title substring picks one entry")
        (is (search "Defender" (text-of (first rows) "title")) "and it is the Defender one"))
      (let* ((text (wu-call-tool "win_update_history" "{\"count\":1}"))
             (rows (rows-of-answer text "updates")))
        (is (= 1 (length rows)) "count bounds the rows")
        (is (wu-noted text "stopped at count=1") "and says it stopped")
        (is (search "NODECODE_WU_TAKE=1" (princ-to-string (getf (stub-call) :environment))) "the script was asked for one row"))
      (let ((text (wu-call-tool "win_update_history" "{\"query\":\"nothing here\"}")))
        (is (zerop (length (rows-of-answer text "updates"))) "a query that matches nothing answers no rows")
        (is (wu-noted text "nothing matched") "and says so")))))

;;; --- the hotfix tool ---------------------------------------------------------------

(deftest windows-updates-cell-the-quick-fix-list-is-newest-first ()
  (with-updates ()
    (with-powershell (:hotfix +hotfixes+)
      (let* ((text (wu-call-tool "win_hotfix_table" "{}"))
             (rows (rows-of-answer text "hotfixes")))
        (is (eq t (value-of text "ok")) "the call answers ok")
        (is (= 3 (length rows)) "every row the script wrote is here")
        (is (equal "KB5041585" (text-of (first rows) "id")) "the newest install date comes first")
        (is (equal "KB5039212" (text-of (second rows) "id")) "then the older dated one")
        (is (equal "KB2267602" (text-of (third rows) "id")) "and an undated row goes last")
        (is (equal "Security Update" (text-of (first rows) "description")) "what it is travels too")
        (is (wu-noted text "3 of 3 hotfix(es) listed, 3 known") "one note counts the rows")
        (is (wu-noted text "carries OS-level updates only") "and one says what the list leaves out"))
      (let* ((text (wu-call-tool "win_hotfix_table" "{\"limit\":2}"))
             (rows (rows-of-answer text "hotfixes")))
        (is (= 2 (length rows)) "limit bounds the rows")
        (is (wu-noted text "stopped at limit=2") "and says it stopped"))
      (let* ((text (wu-call-tool "win_hotfix_table" "{\"query\":\"5041\"}"))
             (rows (rows-of-answer text "hotfixes")))
        (is (= 1 (length rows)) "the KB substring picks one row")
        (is (equal "KB5041585" (text-of (first rows) "id")) "and it is the KB that matched")))))

;;; --- a query that could not answer --------------------------------------------------

(deftest windows-updates-cell-a-failing-powershell-answers-what-to-fix ()
  (with-updates ()
    (with-powershell (:history +history+ :status 1
                      :stderr "The term 'Get-CimInstance' is not recognized.\n")
      (let* ((text (wu-call-tool "win_hotfix_table" "{}"))
             (error (value-of text "error")))
        (is (null (value-of text "ok")) "a failed query answers ok false")
        (is (search "Get-CimInstance" (princ-to-string error)) "with what PowerShell said")
        (is (null (search (string #\Newline) (princ-to-string error))) "on one line")))
    (with-powershell (:history +history+ :status :timeout)
      (let ((error (value-of (wu-call-tool "win_update_history" "{}") "error")))
        (is (search "timeout_seconds" (princ-to-string error)) "a timeout names the setting that bounds it")))
    (with-powershell (:history "not a history")
      (let ((error (value-of (wu-call-tool "win_update_history" "{}") "error")))
        (is (search "was not JSON" (princ-to-string error)) "an answer that is not JSON is refused")))))

;;; --- /windows-updates ---------------------------------------------------------------

(deftest windows-updates-cell-the-command-shows-what-is-there ()
  (with-updates ()
    (with-powershell (:history +history+ :hotfix +hotfixes+)
      (let ((status (nodecode-windows-updates::run-slash "" "windows-updates-cell")))
        (is (search "723 update(s) in the history" status) "the status counts the history")
        (is (search "2 of the newest 3 did not end clean" status) "and the ones that failed")
        (is (search "0x80240034" status) "with the newest failure's code")
        (is (search "last patched: 2026-09-09 KB5041585" status) "and when the OS was last patched"))
      (let ((history (nodecode-windows-updates::run-slash "history 2" "windows-updates-cell")))
        (is (search "of 723, newest first" history) "history COUNT reads the history")
        (is (search "KB5039212" history) "and names the newest entries"))
      (let ((hot (nodecode-windows-updates::run-slash "hotfixes 5041" "windows-updates-cell")))
        (is (search "1 quick-fix row(s)" hot) "hotfixes QUERY filters the quick-fix list"))
      (is (search "win_update_history" (nodecode-windows-updates::run-slash "help" "windows-updates-cell")) "help is the usage")
      (is (search "no \"sometimes\"" (nodecode-windows-updates::run-slash "sometimes" "windows-updates-cell")) "an unknown word is usage"))))
