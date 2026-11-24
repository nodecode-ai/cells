;;;; cell.lisp --- the Windows update history and the hotfix list, read-only.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of Yicheng-Mark's hermes-windows-updates plugin. The original
;;;; ran two fixed PowerShell scripts through subprocess and answered JSON;
;;;; this runs the same two scripts through Nodecode's own bounded runner and
;;;; answers the same JSON: the same result-code table, the same title,
;;;; support-link and category fields, and the same unsigned 0x spelling of an
;;;; HRESULT the Agent hands over signed.
;;;;
;;;; READ-ONLY BY CONSTRUCTION. QueryHistory reads what was already installed
;;;; or attempted -- Searcher.Search() is the call that reaches the network,
;;;; and no script here calls it -- and Win32_QuickFixEngineering is a CIM
;;;; read. Nothing is downloaded, installed or uninstalled, and no file is
;;;; written. Windows only: elsewhere the first query says what failed.
;;;;
;;;; The row count travels to the script in the environment, never on the
;;;; command line, so no call has a quoting or injection surface.

(defpackage #:nodecode-windows-updates (:use #:cl))

(in-package #:nodecode-windows-updates)

(nlk:define-peripheral windows-updates :not-running t)

;;; --- the words -------------------------------------------------------------
;;; Data, so a layer can reword them.

(defparameter *meanings*
  '((1 . "it is still in progress")
    (2 . "it succeeded")
    (3 . "it succeeded with errors")
    (4 . "it failed")
    (5 . "it was aborted"))
  "IUpdateHistoryEntry.ResultCode in words: the numbers are fixed by the
Windows Update Agent's API and read the same in every display language.")

(defparameter +not-clean+ '(3 4 5)
  "The result codes of an update that did not end clean.")

(defparameter +title-chars+ 200
  "How much of an update's title one row carries.")

(defparameter +max-count+ 500
  "The most history entries one call reads; the API takes a range, and this is
its useful ceiling.")

(defparameter +max-rows+ 400
  "The most hotfix rows one call answers.")

(defparameter *timeout* 90
  "Seconds one PowerShell query may take, the section's default.")

(defparameter *status-count* 50
  "How many of the newest history entries /windows-updates reads.")

(defparameter +usage+
  "windows-updates: status (bare) | history [COUNT] | hotfixes [QUERY]
  status           what the newest updates say, and when the OS was last patched
  history [N]      the newest N entries with each outcome in words (default 50)
  hotfixes [QUERY] the OS-level quick-fix list, newest first, filtered by KB id
The same rows are the win_update_history and win_hotfix_table tools."
  "What /windows-updates answers with no word it knows.")

(defparameter *primer*
  "Windows Update is readable through the nodecode-windows-updates cell, which is
read-only on the operator's own machine -- Windows only. Two tools:
  win_update_history  the update history the Settings app shows, newest first:
                      Windows, Defender and driver updates, each with its outcome in
                      words (2 succeeded, 3 succeeded with errors, 4 failed, 5
                      aborted) and, when it did not end clean, the failure's code in
                      the 0x form a person can paste into a search box. Filter it
                      with only_failures, or by a title substring such as Defender or
                      a KB number, and raise count when the window is too small.
  win_hotfix_table    the OS-level quick-fix list: KB ids, install dates and
                      installers, newest first -- 'when was this machine last
                      patched' at a glance. It carries OS-level updates only; the two
                      tools together are the whole picture.
Neither tool installs, downloads or searches for an update: they only read what is
there. /windows-updates status is the same read in a few lines. A tool that cannot
run PowerShell answers an error member saying what to fix: the section's `powershell`
names the executable (powershell.exe on a stock Windows, pwsh on PowerShell 7)."
  "What (help :windows-updates) answers while the cell runs.")

;;; --- the two scripts --------------------------------------------------------

(defparameter +history-script+
  "$ErrorActionPreference = 'Stop'
[Console]::OutputEncoding = [Text.Encoding]::UTF8
$session = New-Object -ComObject Microsoft.Update.Session
$searcher = $session.CreateUpdateSearcher()
$total = $searcher.GetTotalHistoryCount()
if ($total -lt 1) { '{\"total\": 0, \"entries\": []}'; exit 0 }
$take = [int]$env:NODECODE_WU_TAKE
if ($take -lt 1) { $take = 1 }
if ($take -gt 500) { $take = 500 }
if ($take -gt $total) { $take = $total }
$entries = $searcher.QueryHistory(0, $take)
$out = foreach ($e in $entries) {
  [pscustomobject]@{
    date = if ($e.Date -and $e.Date.Year -gt 1) { $e.Date.ToString('o') } else { $null }
    title = $e.Title
    result_code = [int]$e.ResultCode
    hresult = [int]$e.HResult
    support_url = $e.SupportUrl
    categories = @($e.Categories | ForEach-Object { $_.Name })
  }
}
@{ total = [int]$total; entries = @($out) } | ConvertTo-Json -Depth 4 -Compress"
  "The update history, newest first, from the Windows Update Agent's own
session interface. Dates are formatted inside PowerShell ('o'): ConvertTo-Json
turns a DateTime into an epoch wrapper that does not survive transport.")

(defparameter +hotfix-script+
  "$ErrorActionPreference = 'Stop'
[Console]::OutputEncoding = [Text.Encoding]::UTF8
$qfe = Get-CimInstance Win32_QuickFixEngineering
$out = foreach ($k in $qfe) {
  [pscustomobject]@{
    id = $k.HotFixID
    description = $k.Description
    installed_on = if ($k.InstalledOn -and $k.InstalledOn.Year -gt 1) { $k.InstalledOn.ToString('yyyy-MM-dd') } else { $null }
    installed_by = $k.InstalledBy
  }
}
if ($out) { $out | ConvertTo-Json -Depth 4 -Compress } else { '[]' }"
  "The OS-level quick-fix list, one row an installed update.")

;;; --- running PowerShell -----------------------------------------------------

(defun query (script &key environment)
  "SCRIPT through the PowerShell executable the section names: => (values
STDOUT STDERR STATUS), STATUS :TIMEOUT when the bound fell first. The one place
this cell starts a process, so a test replaces it with a canned answer."
  (nlk:run-bounded (list (setting :powershell) "-NoProfile" "-NonInteractive"
                          "-Command" script)
                    :seconds (setting :timeout-seconds)
                    :environment environment))

(defun strip-bom (text)
  "TEXT without the byte-order mark a pipe written by PowerShell may still start with."
  (if (and (plusp (length text)) (char= (char text 0) (code-char #xFEFF)))
      (subseq text 1)
      text))

(defun checked (script environment what)
  "WHAT's stdout, or a refusal saying why: a timeout, an exit status, or an
executable that could not be started at all."
  (multiple-value-bind (out err status)
      (handler-case (query script :environment environment)
        (windows-updates-error (condition) (error condition))
        (error (condition)
          (fail "~a could not be run: ~a" (setting :powershell)
                (nlk:one-line (princ-to-string condition) :cap 200))))
    (cond
      ((eq status :timeout)
       (fail "the ~a query did not finish in ~d s; raise timeout_seconds in the windows-updates section"
             what (setting :timeout-seconds)))
      ((and (integerp status) (not (zerop status)))
       (let ((detail (strip-bom (or err ""))))
         (fail "the ~a query failed: ~a" what
               (if (plusp (length detail))
                   (nlk:one-line detail :cap 400)
                   (format nil "exit ~d" status)))))
      (t (strip-bom (or out ""))))))

(defun decoded (text what)
  "TEXT as JSON, or a refusal naming WHAT when PowerShell wrote something else."
  (or (ignore-errors (nlk:decode-json text))
      (fail "the ~a answer was not JSON: ~a" what
            (nlk:one-line text :cap 200))))

;;; --- reading an answer ------------------------------------------------------

(defun field (object key)
  "KEY's value in the JSON OBJECT, or NIL."
  (and (hash-table-p object) (gethash key object)))

(defun text-at (object key)
  "KEY's value in OBJECT as a non-empty string, or NIL; JSON null reads as absent."
  (let ((value (field object key)))
    (typecase value
      (string (if (plusp (length value)) value nil))
      (null nil)
      (keyword nil)
      (t (princ-to-string value)))))

(defun integer-at (object key)
  "KEY's value in OBJECT as an integer, or NIL when it carries none."
  (let ((value (field object key)))
    (cond ((integerp value) value)
          ((realp value) (round value))
          ((stringp value) (ignore-errors (parse-integer (nlk:trimmed value))))
          (t nil))))

(defun rows-of (value)
  "VALUE as a list of JSON objects: a listing, or the one object a single row is."
  (let ((list (cond ((vectorp value) (coerce value 'list))
                     ((hash-table-p value) (list value))
                     (t '()))))
    (remove-if-not #'hash-table-p list)))

(defun title-of (object)
  "OBJECT's title as one line of at most +TITLE-CHARS+ characters."
  (nlk:one-line (or (text-at object "title") "") :cap +title-chars+))

(defun hex-of (value)
  "VALUE, an HRESULT the Agent hands over signed, in the unsigned 0x form a
person can paste into a search box: -2145124300 reads as 0x80240034."
  (and (integerp value) (format nil "0x~8,'0X" (logand value #xFFFFFFFF))))

(defun failed-p (raw)
  "Whether RAW's outcome is one that did not end clean."
  (and (member (integer-at raw "result_code") +not-clean+) t))

(defun result-of (code)
  "A ResultCode as the number and what it means."
  (nlk:json-object :when code "code" code
                    "meaning" (or (cdr (assoc code *meanings*)) "no known meaning")))

(defun categories-of (raw)
  "RAW's categories as a JSON array of names."
  (let ((value (field raw "categories")))
    (coerce (cond ((vectorp value) (remove-if-not #'stringp (coerce value 'list)))
                  ((stringp value) (list value))
                  (t '()))
            'vector)))

(defun update-row (raw)
  "One history entry as the tools report it: the outcome in words and, when it
did not end clean, the failure's code in 0x form."
  (let ((hresult (field raw "hresult")))
    (nlk:json-object :opt "date" (text-at raw "date")
                     "title" (title-of raw)
                     "result" (result-of (integer-at raw "result_code"))
                     :when (integerp hresult)
                     "hresult" (nlk:json-object "code" hresult "hex" (hex-of hresult))
                     :opt "support_url" (text-at raw "support_url")
                     "categories" (categories-of raw))))

;;; --- the two reads ----------------------------------------------------------

(defun history-rows (count)
  "The newest COUNT entries the Windows Update Agent keeps: => (values RAW-ROWS
TOTAL), where TOTAL is how many the history holds."
  (let* ((take (max 1 (min (or count *status-count*) +max-count+)))
         (text (checked +history-script+
                        (list (format nil "NODECODE_WU_TAKE=~d" take))
                        "update history"))
         (data (decoded text "update history")))
    (unless (hash-table-p data)
      (fail "the update history answer was not the object the query writes"))
    (values (rows-of (field data "entries"))
            (or (integer-at data "total") 0))))

(defun later-p (a b)
  "Whether A's install date -- then its id -- comes after B's, the order the
quick-fix table is read in. A row with no date goes last."
  (let ((da (text-at a "installed_on"))
        (db (text-at b "installed_on"))
        (ia (or (text-at a "id") ""))
        (ib (or (text-at b "id") "")))
    (cond ((and da db) (or (string> da db) (and (string= da db) (string> ia ib))))
          (da t)
          (db nil)
          (t (string> ia ib)))))

(defun hotfix-rows ()
  "The quick-fix list, newest install date first."
  (sort (rows-of (decoded (checked +hotfix-script+ nil "hotfix list") "hotfix list"))
        #'later-p))

(defun hotfix-row (raw)
  "One quick-fix row: its KB id, what it is, and when it was installed."
  (nlk:json-object
   "id" (or (text-at raw "id") "")
   :opt "description" (text-at raw "description")
   :opt "installed_on" (text-at raw "installed_on")
   :opt "installed_by" (text-at raw "installed_by")))

;;; --- the boundary -----------------------------------------------------------

(nlk:define-contract history-request
  (:doc "The newest updates this Windows machine installed or attempted, newest first, each with its outcome in words and, when it did not end clean, the failure's error code in 0x form. Filter to the ones that did not end clean, or by a title substring such as Defender or a KB number. Use it for 'why is this machine stuck on an old build', 'did the driver update actually install', and 'what kept failing last week'. Read-only: nothing is searched for, downloaded, installed or uninstalled. Windows only.")
  ("count" :integer :default 50 :slot history-count
   :doc "newest entries to consider, 1 to 500")
  ("only_failures" :boolean :default nil
   :doc "only entries whose outcome is not clean: failed, aborted, or succeeded with errors")
  ("query" :string
   :doc "case-insensitive substring of an entry's title, e.g. Defender, KB2267602, driver"))

(nlk:define-contract hotfix-request
  (:doc "The OS-level quick-fix list -- KB ids, security updates, when each was installed and by whom -- for 'when was this machine last patched' at a glance, newest install date first. It carries OS-level updates only: apps, Defender signatures and drivers are win_update_history's. Read-only. Windows only.")
  ("query" :string
   :doc "case-insensitive substring of a KB id, e.g. 5127694 or KB50")
  ("limit" :integer :default 100
   :doc "rows to return, 1 to 400"))

(defmacro guarded (&body body)
  "BODY's JSON text, or the JSON text of the refusal it raised: a tool never
throws at the model, it answers what to fix."
  `(handler-case (progn ,@body)
     (windows-updates-error (condition)
       (nlk:encode-json-object (nlk:json-object "ok" nil "error" (princ-to-string condition))))
     (error (condition)
       (nlk:encode-json-object
        (nlk:json-object "ok" nil
                         "error" (format nil "unexpected windows-updates error: ~a" condition))))))

(defun matching-p (raw only-failures query)
  "Whether RAW passes the call's filter: its outcome, and a substring of its title."
  (and (or (not only-failures) (failed-p raw))
       (or (zerop (length query))
           (search (string-downcase query) (string-downcase (or (text-at raw "title") ""))))))

(defun history-tool (request)
  "win_update_history: the newest updates this machine installed or attempted."
  (guarded
    (let* ((count (max 1 (min (history-request-history-count request) +max-count+)))
           (only-failures (history-request-only-failures request))
           (query (nlk:trimmed (or (history-request-query request) ""))))
      (multiple-value-bind (raws total) (history-rows count)
        (let* ((matching (remove-if-not (lambda (raw) (matching-p raw only-failures query)) raws))
               (kept (if (<= (length matching) count) matching (subseq matching 0 count)))
               (failed (count-if #'failed-p raws))
               (notes (list (format nil "~d of ~d matching update(s) listed, ~d in the history"
                                    (length kept) (length matching) total))))
          (when (> (length matching) (length kept))
            (push (format nil "stopped at count=~d; raise it (up to ~d)" count +max-count+) notes))
          (when (> total (length raws))
            (push (format nil "the history reaches back further than the newest ~d; count goes up to ~d"
                          (length raws) +max-count+) notes))
          (when (and (plusp failed) (not only-failures))
            (push (format nil "~d of the newest ~d did not end clean; only_failures=true lists them"
                          failed (length raws)) notes))
          (when (null kept)
            (push (cond ((plusp (length query))
                         (format nil "nothing matched ~s; try another word" query))
                        (only-failures
                         "every update in this window ended clean; only_failures listed none")
                        (t "nothing matched; drop the query or the filter"))
                  notes))
          (nlk:encode-json-object
           (nlk:json-object "ok" t
                            "updates" (coerce (mapcar #'update-row kept) 'vector)
                            "notes" (coerce (nreverse notes) 'vector))))))))

(defun hotfix-tool (request)
  "win_hotfix_table: the OS-level hotfixes, newest install date first."
  (guarded
    (let* ((query (nlk:trimmed (or (hotfix-request-query request) "")))
           (limit (max 1 (min (hotfix-request-limit request) +max-rows+)))
           (rows (hotfix-rows))
           (matching (if (zerop (length query))
                         rows
                         (remove-if-not
                          (lambda (raw)
                            (search (string-downcase query)
                                    (string-downcase (or (text-at raw "id") ""))))
                          rows)))
           (kept (if (<= (length matching) limit) matching (subseq matching 0 limit)))
           (notes (list (format nil "~d of ~d hotfix(es) listed, ~d known"
                                (length kept) (length matching) (length rows)))))
      (when (> (length matching) (length kept))
        (push (format nil "stopped at limit=~d; raise it or narrow the query" limit) notes))
      (when (null kept)
        (push "nothing matched; drop the query" notes))
      (push "the quick-fix list carries OS-level updates only; win_update_history answers what Windows installed in full"
            notes)
      (nlk:encode-json-object
       (nlk:json-object "ok" t
                        "hotfixes" (coerce (mapcar #'hotfix-row kept) 'vector)
                        "notes" (coerce (nreverse notes) 'vector))))))

;;; --- /windows-updates -------------------------------------------------------

(defun status-text ()
  "The newest updates in a few lines: what ended clean, what did not, and when
the OS itself was last patched."
  (multiple-value-bind (raws total) (history-rows *status-count*)
    (let* ((failed (remove-if-not #'failed-p raws))
           (newest (find-if (lambda (raw) (text-at raw "date")) raws))
           (hot (first (hotfix-rows)))
           (lines (list (format nil "~d update(s) in the history on this machine" total))))
      (when newest
        (push (format nil "newest: ~a (~a)" (title-of newest) (text-at newest "date")) lines))
      (push (format nil "~d of the newest ~d did not end clean"
                    (length failed) (length raws)) lines)
      (when (first failed)
        (push (format nil "the newest of them: ~a~@[ (~a)~]"
                      (title-of (first failed))
                      (hex-of (field (first failed) "hresult"))) lines))
      (push (if hot
                (format nil "last patched: ~a ~a~@[ (~a)~]"
                        (text-at hot "installed_on") (text-at hot "id")
                        (text-at hot "description"))
                "last patched: the quick-fix list is empty")
            lines)
      (push (format nil "/windows-updates history [N] | hotfixes [QUERY] for the rows") lines)
      (format nil "~{~a~%~}" (nreverse lines)))))

(defun history-text (count)
  "The newest COUNT entries as a table a person reads, newest first."
  (multiple-value-bind (raws total) (history-rows count)
    (with-output-to-string (out)
      (format out "windows-updates: the newest ~d of ~d, newest first~%" (length raws) total)
      (dolist (raw raws)
        (format out "  ~a  ~a  ~a~%" (or (text-at raw "date") "(no date)")
                (or (cdr (assoc (integer-at raw "result_code") *meanings*))
                    "no known meaning")
                (title-of raw)))
      (when (null raws)
        (format out "  no update in the history~%")))))

(defun hotfix-text (query)
  "The quick-fix rows matching QUERY as a table a person reads, newest first."
  (let* ((rows (hotfix-rows))
         (matching (if (zerop (length query))
                       rows
                       (remove-if-not
                        (lambda (raw)
                          (search (string-downcase query)
                                  (string-downcase (or (text-at raw "id") ""))))
                        rows))))
    (with-output-to-string (out)
      (format out "windows-updates: ~d quick-fix row(s), newest install date first~%"
              (length matching))
      (dolist (raw (if (<= (length matching) *status-count*)
                       matching
                       (subseq matching 0 *status-count*)))
        (format out "  ~a  ~a~@[  ~a~]~%"
                (or (text-at raw "installed_on") "(no date)")
                (or (text-at raw "id") "")
                (text-at raw "description")))
      (when (null matching)
        (format out "  nothing matched~%")))))

(defun run-slash (args session-id)
  "/windows-updates: what installed lately, what did not end clean, and when the
OS was last patched. A bare call is the status; SUB is history [COUNT],
hotfixes [QUERY] or help."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (space (position-if (lambda (char) (member char '(#\Space #\Tab))) text))
         (head (string-downcase (subseq text 0 space)))
         (rest (if space (nlk:trimmed (subseq text space)) "")))
    (cond ((member head '("" "status") :test #'string=) (status-text))
          ((string= head "history")
           (history-text (or (ignore-errors (parse-integer rest)) *status-count*)))
          ((string= head "hotfixes") (hotfix-text rest))
          ((string= head "help") +usage+)
          (t (format nil "windows-updates: no ~s~%~a" head +usage+)))))

;;; --- the section and the declaration ----------------------------------------

(defun nonblank (value)
  "VALUE as a trimmed string, or NIL when it is empty."
  (let ((trimmed (and value (string-trim '(#\Space #\Tab #\Newline #\Return)
                                          (princ-to-string value)))))
    (and trimmed (plusp (length trimmed)) trimmed)))

(defun read-settings (values table)
  "The declared VALUES, with the executable checked and the bound held here, once."
  (let* ((raw (and table (gethash "powershell" table)))
         (executable (and (not (and (stringp raw) (null (nonblank raw))))
                          (nonblank (getf values :powershell)))))
    (unless executable
      (nlk:config-error
       "no PowerShell executable named: powershell runs the two queries -- powershell.exe on a stock Windows, pwsh on PowerShell 7"))
    (list :powershell executable
          :timeout-seconds (max 1 (min (or (getf values :timeout-seconds) *timeout*) 600)))))

(nle:define-cell windows-updates
  (:section ("windows-updates")
    (:guide "powershell names the executable the two queries run through, powershell.exe on a stock Windows and pwsh on PowerShell 7; timeout_seconds bounds one query. Nothing else is needed, and nothing here writes or installs")
    ("powershell" :string :default "powershell.exe"
     :doc "the executable that runs the two queries; point it at pwsh for PowerShell 7")
    ("timeout_seconds" :integer :default 90 :min 1
     :doc "seconds one query may take before its process tree is killed (at most 600)"))
  (:settings #'read-settings)
  (:tool "win_update_history" (nlk:contract 'history-request) 'history-tool)
  (:tool "win_hotfix_table" (nlk:contract 'hotfix-request) 'hotfix-tool)
  (:command "windows-updates" 'run-slash
            :catches 'windows-updates-error
            :description "Windows updates: what installed lately, what failed, when the OS was last patched"
            :argument-hint "[status|history [COUNT]|hotfixes [QUERY]]")
  (:help :windows-updates
   "windows-updates: the Windows update history and quick-fix list, read-only, as win_update_history and win_hotfix_table - /windows-updates status"
   *primer*))
