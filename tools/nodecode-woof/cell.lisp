;;;; cell.lisp --- WOOF weather runs on this machine's GPU.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A rewrite of recastsystems's Hermes Agent plugin (see NOTICE). The model
;;;; learns the verbs through the manual: while the cell runs, (help :woof)
;;;; answers the primer below, and every request's help section carries one
;;;; line naming it.
;;;;
;;;; WOOF is a command the machine already has (recast-woof from PyPI, its own
;;;; install guide). The cell downloads nothing itself: it writes a run plan,
;;;; asks WOOF to resolve and estimate it, starts it detached, reads the record
;;;; and the event stream it leaves behind, and answers the maps' paths as
;;;; MEDIA: lines. Everything it knows about a run is in a folder under
;;;; runs_dir, so a restart loses nothing.
;;;;
;;;; Rewritten from the twelve woof_* tools of that plugin -- doctor, catalog,
;;;; docs, plan, launch, status, stop, show, compare, campaign, score and
;;;; report -- and all twelve are here: the run itself (size it, start it,
;;;; watch it, stop it, show it, compare it, report it), the catalog WOOF
;;;; keeps, the documentation pack it ships (searched offline), campaigns of
;;;; arms run one at a time, and a finished run scored against observations
;;;; through WOOF's own doors. The watcher that pushed run events into the
;;;; chat is the one part not rewritten: the verbs are polled instead.

(in-package #:nodecode-woof)

(nlk:define-peripheral woof :not-running t)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "WOOF is an open-source weather model that runs on one NVIDIA GPU. The
nodecode-woof cell drives the WOOF command this machine already has: it sizes a
run, starts it detached, reads what the run wrote, and hands you its maps.
  woof_doctor  {}
      The WOOF command this cell found, the version it answers, the folder runs
      live in, and the last lines of WOOF's own doctor report. Run it first: a
      machine without the command can do nothing else here, and WOOF itself
      needs Linux x86_64 with an NVIDIA GPU.
  woof_plan  {plan}
      Size a run before anything starts. plan is one WOOF run plan as a JSON
      object: what to model, the area, the physics and the products. WOOF's own
      resolver and estimator answer: the grid, the memory fit, the download and
      the wall time it predicts. Nothing runs and nothing is downloaded. The
      plan and WOOF's two answers are kept under runs_dir/plans/, and the
      answer says where.
  woof_launch  {plan, label}
      Start one planned run on this machine's GPU, detached, and answer its
      run_id at once. label is a short name you will recognise later. A run is
      a folder under runs_dir/runs/<run_id>/: plan.json, woof.log and the
      events WOOF appends as it goes.
  woof_status  {run_id}
      One run as it stands -- running, stopped or ended -- with its pid, its
      last events and how many maps it has written. With no run_id, the runs
      this cell started, newest first.
  woof_stop  {run_id}
      Terminate a run this cell started, by its own recorded pid, and mark it
      stopped. It refuses when the run is already over.
  woof_compare  {left, right}
      Put two runs side by side with WOOF's own renderer: difference maps when the
      grids match, side-by-side sheets otherwise. Both must have written at least
      one map. The answer carries the images and their MEDIA: lines.
  woof_report  {run_id}
      Everything one run left behind: its setup, how it ended, how many maps it
      wrote, the tail of its log and of WOOF's event stream, and its score when
      one was written. Ask it when a run ends.
  woof_catalog  {kind, query, case_id}
      What WOOF offers on this machine, from WOOF itself: the start-data sources
      it can run (sources), the physics suites and mixes (physics), the historical
      cases it ships (cases, searched by query or read in full by case_id), and its
      research recipes (recipes). Ask it before woof_plan when a plan's options
      are not known.
  woof_show  {run_id, max_images}
      The maps a run has written so far, as absolute paths and as MEDIA: lines.
      Put each MEDIA: line on a line of its own in your reply so the user sees
      the picture. Maps appear while the forecast runs: ask again as it goes.
  woof_docs  {query, max_results}
      WOOF's own documentation, searched offline: the passages that answer a
      question about its physics options, its run plan, its outputs, its
      verification and its limits. Quote it instead of guessing, and prefer it
      over anything you remember about weather models.
  woof_campaign  {action, spec, campaign_id}
      Several runs of one plan that differ along one axis -- a physics suite, a
      cycle, a grid size -- sized together and run one at a time, because one
      card runs one forecast at a time. action=plan builds the arms from spec
      (name, axis, values, base, budget) and answers their total wall and disk;
      action=start launches the first arm; action=status answers where every arm
      stands and starts the next when one has ended; action=stop ends it.
      woof_report with campaign_id answers the arms and their scores together.
  woof_score  {run_id, against}
      Score a finished run against observations: MRMS reflectivity, Stage-IV
      precipitation and ASOS surface reports, fetched and scored through WOOF's
      own doors and its battery scorer, under the one-off registration WOOF's
      registration library builds for the run's length. It writes score.json
      beside the run, which woof_report then carries. A one-off score is
      proposed-unratified, and the answer says so.
The same verbs are Lisp functions in the nodecode-woof package: (nodecode-woof:doctor),
(nodecode-woof:show-catalog call), (nodecode-woof:check-plan plan-object),
(nodecode-woof:launch plan label), (nodecode-woof:run-status run-id),
(nodecode-woof:show-maps run-id n), (nodecode-woof:compare-runs call),
(nodecode-woof:report-run call), (nodecode-woof:stop-run run-id),
(nodecode-woof:search-docs call), (nodecode-woof:campaign call) and
(nodecode-woof:run-score call).")

;;; --- what a tool call crosses with ---------------------------------------------

(defun no-arguments (description)
  "The schema of a tool whose call carries nothing."
  (list :description description
        :parameters (nlk:json-object "type" "object" "properties" (nlk:make-json-object))))

(defparameter +diagnose-schema+
  (no-arguments
   "What this machine can do: the WOOF command the cell found, the version it answers, the folder runs live in, and the last lines of WOOF's own doctor report. Run it first: without the command nothing else here works, and WOOF itself needs Linux x86_64 with an NVIDIA GPU.")
  "woof_doctor takes nothing.")

(nlk:define-contract plan-check
  (:doc "Size one WOOF run before anything starts: WOOF's own resolver and estimator answer the grid, the memory fit, the download size and the wall time they predict. Nothing runs and nothing is downloaded; this is the check to do before woof_launch.")
  ("plan" :object :required t
   :doc "one WOOF run plan as a JSON object: the source and date, the area and grid, the physics choices and the products. The cell writes it verbatim and hands it to WOOF."))

(nlk:define-contract run-launch
  (:doc "Start one planned run on this machine's GPU and answer its run_id at once. It returns before the forecast does: poll woof_status, and put the MEDIA: lines woof_show answers into your reply as the maps arrive.")
  ("plan" :object :required t
   :doc "the WOOF run plan to launch, as a JSON object (size it with woof_plan first)")
  ("label" :string :default ""
   :doc "a short name for this run, so you recognise it in woof_status; empty is unnamed"))

(nlk:define-contract run-read
  (:doc "Runs this cell started. With a run_id, that run as it stands: its state, its pid, its last events and how many maps it has written. With none, every run, newest first.")
  ("run_id" :string :default ""
   :doc "the run_id woof_launch answered; empty lists every run"))

(nlk:define-contract map-read
  (:doc "The maps one run has written so far, as absolute paths and as MEDIA: lines to copy into your reply. Maps appear while the forecast runs.")
  ("run_id" :text :required t
   :doc "the run_id woof_launch answered")
  ("max_images" :integer :default 4
   :doc "the most maps to answer, 1 or more; the run's newest come first"))

(nlk:define-contract catalog-read
  (:doc "What WOOF offers on this machine, from WOOF itself: the start-data sources it can run, the physics suites, the historical cases it ships, or the research recipes. Use it before woof_plan when you do not know what a plan may name.")
  ("kind" :text :required t :options '("sources" "physics" "cases" "recipes")
   :doc "sources: what data a plan may start from; physics: the suites and mixes a plan may choose; cases: the historical events WOOF ships, searched by query; recipes: WOOF's research recipes")
  ("query" :string :default ""
   :doc "for cases: the word to search for, e.g. 'tornado' or 'derecho'; empty searches for 'tornado'")
  ("case_id" :string :default ""
   :doc "for cases: one case's id, to read it in full instead of searching"))

(nlk:define-contract compare-read
  (:doc "Put two finished or running runs side by side, drawn by WOOF's own renderer: difference maps when the grids match, side-by-side sheets otherwise. The answer names the images and carries their MEDIA: lines; both runs must have written at least one map.")
  ("left" :text :required t
   :doc "the first run's run_id")
  ("right" :text :required t
   :doc "the second run's run_id"))

(nlk:define-contract report-read
  (:doc "Everything one run left behind, in one answer: its setup, how it ended, how many maps it wrote, the tail of its log and of WOOF's event stream, and its score when one was written. Give a campaign_id instead to report every arm of that campaign side by side. Ask it when a run ends, or when the thread of a run has been lost.")
  ("run_id" :string :default ""
   :doc "the run_id woof_launch answered")
  ("campaign_id" :string :default ""
   :doc "a campaign_id: report that campaign's arms and their scores instead of one run"))

(nlk:define-contract docs-read
  (:doc "Search WOOF's own documentation, offline: the passages that answer a question about its physics options, its run plan, its outputs, its verification and its limits. Quote what it answers instead of guessing.")
  ("query" :text :required t
   :doc "the words to look for, in the manual's own vocabulary, e.g. 'physics profile', 'restart', 'stage4' or 'render products'")
  ("max_results" :integer :default 4
   :doc "how many passages to answer, the best first; 1 to 10"))

(nlk:define-contract campaign-call
  (:doc "Several runs of one plan that differ along one axis, sized together and run one at a time on the one card. plan answers the arms and their total; start launches the first; status starts the next as each ends and answers where every arm stands; stop ends the campaign.")
  ("action" :text :required t :options '("plan" "start" "status" "stop")
   :doc "plan: build and size the arms from spec; start: launch the first arm; status: where the arms stand and the next one started; stop: signal the running arm")
  ("spec" :object
   :doc "for plan: name, axis, values, base and budget. axis names the key of the base plan to vary (\"physics\", \"cycle\", \"domain.cells\"); values are what each arm puts there; base is the plan woof_plan takes; budget may carry max_wall_minutes and max_disk_gib")
  ("campaign_id" :string :default ""
   :doc "the campaign_id action=plan answered; required for start, status and stop"))

(nlk:define-contract score-call
  (:doc "Score a finished run against observations: MRMS reflectivity, Stage-IV precipitation and ASOS surface reports, fetched and scored through WOOF's own doors, under the one-off registration WOOF's registration library builds for the run's length. It writes score.json beside the run, which woof_report then carries; a one-off score is proposed-unratified and says so.")
  ("run_id" :text :required t
   :doc "the run_id of a run that has ended")
  ("against" :array :of :string
   :doc "which instruments to fetch: any of \"mrms\", \"stage4\" and \"asos\". Empty scores all three; MRMS always comes because reflectivity carries the primary score"))

;;; --- where runs live -------------------------------------------------------------

(defun runs-root ()
  "The folder runs live in: the section's runs_dir, else ~/woof-runs/."
  (let ((configured (setting :runs-dir)))
    (uiop:ensure-directory-pathname
     (if (and (stringp configured) (plusp (length configured)))
         (uiop:parse-native-namestring configured)
         (merge-pathnames "woof-runs/" (user-homedir-pathname))))))

(defun run-directory (run-id)
  "The folder one run lives in, created."
  (ensure-directories-exist
   (merge-pathnames (format nil "runs/~a/" run-id) (runs-root))))

(defun new-run-id ()
  "A run id nobody has used: the time it started, and a share of chance."
  (format nil "woof-~d-~4,'0d" (get-universal-time) (random 10000)))

(defun stamp ()
  "A folder name for one plan check."
  (format nil "~d-~4,'0d" (get-universal-time) (random 10000)))

;;; --- talking to the command ------------------------------------------------------

(defun woof-command ()
  "The WOOF command to run, or a refusal naming the section's one required key."
  (let ((command (nlk:trimmed (or (setting :woof) ""))))
    (if (plusp (length command))
        command
        (fail "the woof section names no command: set \"woof\" in it to the WOOF command's path, or to \"woof\" when it is on PATH"))))

(defun run-woof (argv)
  "Run the WOOF command with ARGV. => (values STDOUT STDERR STATUS).
The tests stub this; nothing else here starts a process to be read."
  (handler-case
      (multiple-value-bind (out err status)
          (uiop:run-program (mapcar #'princ-to-string argv)
                            :output :string :error-output :string
                            :ignore-error-status t)
        (values (or out "") (or err "") (or status 0)))
    (error (condition)
      (values "" (format nil "~a" condition) 127))))

(defun launch-woof (argv log-path)
  "Start the WOOF command detached, its output in LOG-PATH. => its pid, or NIL.
The tests stub this; nothing else here leaves a process running."
  (handler-case
      (let ((process (uiop:launch-program (mapcar #'princ-to-string argv)
                                           :output log-path
                                           :error-output log-path
                                           :if-output-exists :append)))
        (uiop:process-info-pid process))
    (error (condition)
      (declare (ignore condition))
      nil)))

(defun pid-alive-p (pid)
  "Whether PID is a live process on this machine."
  (not (null (and (integerp pid) (plusp pid)
                  (probe-file (format nil "/proc/~d" pid))))))

(defun signal-pid (pid signal)
  "Send SIGNAL to PID. => true when the call did not fail.
The tests stub this; nothing else here signals a process."
  (when (and (integerp pid) (plusp pid))
    (handler-case
        (progn (uiop:run-program (list "kill" (format nil "-~a" signal)
                                        (princ-to-string pid))
                                  :output :string :error-output :string
                                  :ignore-error-status t)
               t)
      (error (condition)
        (declare (ignore condition))
        nil))))

;;; --- small readings ---------------------------------------------------------------

(defun tail-text (text limit)
  "The last LIMIT characters of TEXT, whole lines where they allow it."
  (let ((text (string-right-trim '(#\Return #\Linefeed #\Space) (or text ""))))
    (if (<= (length text) limit)
        text
        (subseq text (- (length text) limit)))))

(defun json-or-text (text)
  "TEXT decoded as JSON when it is JSON, else its last lines as text."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) (or text ""))))
    (or (and (plusp (length trimmed)) (ignore-errors (nlk:decode-json trimmed)))
        (tail-text trimmed 1200))))

(defun json-document (text)
  "TEXT decoded as the JSON document it carries -- the whole text, else its
last line that opens one -- or NIL."
  (let ((trimmed (nlk:trimmed (or text ""))))
    (or (and (plusp (length trimmed)) (ignore-errors (nlk:decode-json trimmed)))
        (loop for line in (reverse (uiop:split-string trimmed :separator '(#\Newline)))
              for start = (position #\{ line)
              when start
                do (return (ignore-errors (nlk:decode-json (subseq line start))))
              finally (return nil)))))

(defun write-json (path object)
  "Write OBJECT to PATH as JSON, and answer PATH."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                             :external-format :utf-8)
    (write-string (nlk:encode-json-object object) out))
  path)

(defun read-json (path)
  "PATH decoded as a JSON object, or NIL when it is absent or unreadable."
  (let ((path (probe-file path)))
    (when path
      (ignore-errors (nlk:decode-json (uiop:read-file-string path))))))

(defun record-path (run-id)
  (merge-pathnames "record.json" (run-directory run-id)))

(defun read-record (run-id)
  "RUN-ID's record object, or NIL when this cell never started it."
  (read-json (record-path run-id)))

(defun record-value (record key &optional type)
  (and record (nlk:json-value record (or type :string) key)))

;;; --- the runs on disk ---------------------------------------------------------------

(defun list-runs ()
  "Every run this cell started, newest first, as records."
  (let* ((root (merge-pathnames "runs/" (runs-root)))
         (directories (and (uiop:directory-exists-p root)
                           (uiop:subdirectories root))))
    (sort (loop for directory in directories
                for record = (read-json (merge-pathnames "record.json" directory))
                when record collect record)
          #'string> :key (lambda (record) (or (record-value record "started") "")))))

(defun image-paths (run-id)
  "Every PNG under RUN-ID's folder, newest first."
  (let ((root (run-directory run-id)))
    (sort (directory (merge-pathnames "**/*.png" root))
          #'> :key (lambda (path) (or (file-write-date path) 0)))))

;;; --- the verbs -------------------------------------------------------------------

(defun doctor (&optional call)
  "woof_doctor: the command, its version, where runs live, and its own doctor."
  (declare (ignore call))
  (let ((command (woof-command)))
    (multiple-value-bind (version version-err version-status)
        (run-woof (list command "version"))
      (declare (ignore version-err))
      (multiple-value-bind (report report-err status)
          (run-woof (list command "doctor"))
        (nlk:encode-json-object
         (nlk:make-json-object
          "ok" (and (= 0 version-status) (= 0 status) t)
          "woof" command
          "version" (nlk:trimmed version)
          "runs_dir" (uiop:native-namestring (runs-root))
          "can_check" (= 0 version-status)
          "doctor" (tail-text (concatenate 'string (or report "") (or report-err "")) 2000)))))))

(defun check-plan (call)
  "woof_plan: write the plan, ask WOOF to resolve and estimate it, keep both."
  (let* ((directory (ensure-directories-exist
                      (merge-pathnames (format nil "plans/~a/" (stamp)) (runs-root))))
         (path (write-json (merge-pathnames "plan.json" directory) (plan-check-plan call)))
         (namestring (uiop:native-namestring path)))
    (multiple-value-bind (resolved resolve-err resolve-status)
        (run-woof (list (woof-command) "run-plan" namestring "--resolve"))
      (multiple-value-bind (estimate estimate-err estimate-status)
          (run-woof (list (woof-command) "run-plan" namestring "--estimate"))
        (write-json (merge-pathnames "resolve.json" directory) (json-or-text resolved))
        (nlk:encode-json-object
         (nlk:make-json-object
          "ok" (and (= 0 resolve-status) (= 0 estimate-status) t)
          "plan" namestring
          "folder" (uiop:native-namestring directory)
          "resolve" (json-or-text resolved)
          "estimate" (json-or-text estimate)
          "refused" (unless (and (= 0 resolve-status) (= 0 estimate-status))
                      (tail-text (concatenate 'string resolve-err estimate-err) 1200))))))))

(defun submit-run (plan-object label campaign-id)
  "Write PLAN-OBJECT into a new run folder, start WOOF detached on it, and
answer the run's record. A campaign's arms come through here too."
  (let* ((run-id (new-run-id))
         (directory (run-directory run-id))
         (path (write-json (merge-pathnames "plan.json" directory) plan-object))
         (log (merge-pathnames "woof.log" directory))
         (pid (launch-woof (list (woof-command) "run-plan" (uiop:native-namestring path)) log))
         (record (nlk:make-json-object
                  "run_id" run-id
                  "label" (or label "")
                  "pid" (or pid 0)
                  "state" (if pid "running" "failed")
                  "started" (princ-to-string (get-universal-time))
                  "campaign_id" (or campaign-id "")
                  "plan" (uiop:native-namestring path)
                  "log" (uiop:native-namestring log))))
    (write-json (merge-pathnames "record.json" directory) record)
    record))

(defun launch (call)
  "woof_launch: write the plan into a run folder and start WOOF detached."
  (let* ((record (submit-run (run-launch-plan call) (run-launch-label call) ""))
         (run-id (record-value record "run_id"))
         (started (record-value record "state")))
    (nlk:encode-json-object
     (nlk:make-json-object
      "ok" (and (equal started "running") t)
      "run_id" run-id
      "pid" (or (record-value record "pid" :integer) 0)
      "folder" (uiop:native-namestring (run-directory run-id))
      "log" (record-value record "log")
      "next" (if (equal started "running")
                  "call woof_status with this run_id, and woof_show for maps as they arrive"
                  "WOOF did not start: read the log")))))

(defun run-state (record)
  "What RECORD's run is doing now: running, stopped or ended."
  (let ((recorded (or (record-value record "state") ""))
        (pid (record-value record "pid" :integer)))
    (cond ((equal recorded "stopped") "stopped")
          ((pid-alive-p pid) "running")
          (t "ended"))))

(defun run-summary (record)
  "One run's line: what it is, and what it has written."
  (let* ((run-id (or (record-value record "run_id") ""))
         (state (run-state record)))
    (nlk:make-json-object
     "run_id" run-id
     "label" (or (record-value record "label") "")
     "state" state
     "pid" (or (record-value record "pid" :integer) 0)
     "started" (or (record-value record "started") "")
     "maps" (length (ignore-errors (image-paths run-id))))))

(defun run-status (call)
  "woof_status: one run as it stands, or every run when no run_id is given."
  (status-text (and call (run-read-run-id call))))

(defun status-text (run-id)
  "RUN-ID's status as JSON text; no RUN-ID lists every run."
  (if (and run-id (plusp (length run-id)))
      (let ((record (read-record run-id)))
        (unless record
          (fail "no run ~a under ~a" run-id (uiop:native-namestring (runs-root))))
        (let ((events (merge-pathnames "events.jsonl" (run-directory run-id))))
          (nlk:encode-json-object
           (nlk:make-json-object
            "ok" t
            "run" (run-summary record)
            "folder" (uiop:native-namestring (run-directory run-id))
            "events" (tail-text (if (probe-file events)
                                     (uiop:read-file-string events)
                                     "")
                                1500)))))
      (nlk:encode-json-object
       (nlk:make-json-object
        "ok" t
        "runs_dir" (uiop:native-namestring (runs-root))
        "runs" (coerce (loop for record in (list-runs) collect (run-summary record)) 'vector)))))

(defun show-maps (call)
  "woof_show: the maps a run has written, as paths and as MEDIA: lines."
  (let* ((run-id (map-read-run-id call))
         (limit (min (map-read-max-images call) (setting :max-images))))
    (unless (read-record run-id)
      (fail "no run ~a under ~a" run-id (uiop:native-namestring (runs-root))))
    (let* ((paths (ignore-errors (image-paths run-id)))
           (shown (subseq paths 0 (min limit (length paths))))
           (names (loop for path in shown collect (uiop:native-namestring path))))
      (nlk:encode-json-object
       (nlk:make-json-object
        "ok" t
        "run_id" run-id
        "state" (run-state (read-record run-id))
        "maps" (coerce names 'vector)
        "total" (length paths)
        "media" (format nil "~{~a~^~%~}"
                        (loop for name in names collect (concatenate 'string "MEDIA:" name)))
        "note" (if (plusp (length paths))
                   "put each MEDIA: line on a line of its own in your reply"
                   "this run has written no map yet; ask again as it goes"))))))

(defun stop-run (call)
  "woof_stop: terminate a run this cell started, by its recorded pid."
  (let* ((run-id (run-read-run-id call))
         (record (read-record run-id)))
    (unless record
      (fail "no run ~a under ~a" run-id (uiop:native-namestring (runs-root))))
    (let ((state (run-state record))
          (pid (or (record-value record "pid" :integer) 0)))
      (if (equal state "running")
          (progn
            (signal-pid pid "TERM")
            (let ((path (merge-pathnames "record.json" (run-directory run-id))))
              (write-json path (nlk:make-json-object
                                "run_id" run-id
                                "label" (or (record-value record "label") "")
                                "pid" pid
                                "state" "stopped"
                                "started" (or (record-value record "started") "")
                                "plan" (or (record-value record "plan") "")
                                "log" (or (record-value record "log") "")))
              (nlk:encode-json-object
               (nlk:make-json-object "ok" t "run_id" run-id "state" "stopped" "pid" pid))))
          (nlk:encode-json-object
           (nlk:make-json-object "ok" t "run_id" run-id "state" state "pid" pid
                                  "note" "the run was already over; nothing was signalled"))))))

;;; --- what WOOF offers ---------------------------------------------------------------

(defun catalog-arguments (kind query case-id)
  "The WOOF command that answers KIND's catalog, as the plugin asks it."
  (let ((command (woof-command)))
    (cond ((equal kind "sources") (list command "run-plan" "--sources"))
          ((equal kind "physics") (list command "physics-catalog" "--json"))
          ((equal kind "recipes") (list command "research" "catalog" "--json"))
          ((and (equal kind "cases") (plusp (length (or case-id ""))))
           (list command "case-catalog" "preview" case-id "--json"))
          ((equal kind "cases")
           (list command "case-catalog" "search" "--query"
                 (if (plusp (length (or query ""))) query "tornado") "--json"))
          (t (fail "~a is not a catalog WOOF keeps: use sources, physics, cases or recipes" kind)))))

(defun show-catalog (call)
  "woof_catalog: WOOF's own catalog for one kind."
  (multiple-value-bind (text err status)
      (run-woof (catalog-arguments (catalog-read-kind call)
                                    (catalog-read-query call)
                                    (catalog-read-case-id call)))
    (nlk:encode-json-object
     (nlk:make-json-object
      "ok" (and (= 0 status) t)
      "kind" (catalog-read-kind call)
      "catalog" (json-or-text text)
      "error" (unless (= 0 status) (tail-text err 1200))))))

;;; --- two runs side by side ------------------------------------------------------------

(defun picture-directory (run-id)
  "The first folder under this run that holds maps, or NIL."
  (let ((root (run-directory run-id)))
    (or (loop for folder in (cons root (uiop:subdirectories root))
              when (uiop:directory-files folder "*.png") return folder)
        (loop for path in (directory (merge-pathnames "**/*.png" root))
              return (uiop:pathname-directory-pathname path)))))

(defun compare-runs (call)
  "woof_compare: draw two runs side by side with WOOF's own renderer."
  (let* ((left (compare-read-left call))
         (right (compare-read-right call))
         (left-record (read-record left))
         (right-record (read-record right)))
    (unless left-record
      (fail "no run ~a under ~a" left (uiop:native-namestring (runs-root))))
    (unless right-record
      (fail "no run ~a under ~a" right (uiop:native-namestring (runs-root))))
    (let ((left-dir (picture-directory left))
          (right-dir (picture-directory right)))
      (unless left-dir (fail "run ~a has written no map yet: compare it once woof_show finds one" left))
      (unless right-dir (fail "run ~a has written no map yet: compare it once woof_show finds one" right))
      (let* ((folder (ensure-directories-exist (merge-pathnames "compare/" (runs-root))))
             (out (merge-pathnames (format nil "compare-~a/" (stamp)) folder))
             (label (lambda (record id)
                      (let ((named (record-value record "label")))
                        (if (and named (plusp (length named))) named id)))))
        (multiple-value-bind (drawn err status)
            (run-woof (list (woof-command) "render" "--pair"
                            (uiop:native-namestring left-dir)
                            (uiop:native-namestring right-dir)
                            "--out" (uiop:native-namestring out)
                            "--pair-labels" (funcall label left-record left)
                            "--pair-title" (format nil "WOOF ~a vs ~a" left right)))
          (declare (ignore drawn))
          (let ((sheets (and (uiop:directory-exists-p out)
                             (directory (merge-pathnames "**/*.png" out)))))
            (nlk:encode-json-object
             (nlk:make-json-object
              "ok" (and (= 0 status) (not (null sheets)) t)
              "folder" (uiop:native-namestring out)
              "images" (coerce (loop for sheet in sheets collect (uiop:native-namestring sheet)) 'vector)
              "media" (format nil "~{~a~^~%~}"
                               (loop for sheet in sheets
                                     collect (concatenate 'string "MEDIA:"
                                                          (uiop:native-namestring sheet))))
              "error" (unless (= 0 status) (tail-text err 1200))
              "next" "put each MEDIA: line on a line of its own in your reply"))))))))

;;; --- what one run left behind ----------------------------------------------------------

(defun report-campaign (campaign-id)
  "woof_report with a campaign_id: every arm, its ending and its score, side by side."
  (let* ((campaign (read-campaign campaign-id))
         (arms (campaign-arms campaign)))
    (nlk:encode-json-object
     (nlk:make-json-object
      "ok" t
      "campaign_id" campaign-id
      "name" (member-value campaign "name")
      "axis" (member-value campaign "axis")
      "state" (member-value campaign "state")
      "total" (member-value campaign "total")
      "arms" (coerce (loop for arm in arms
                            collect (let* ((run-id (member-value arm "run_id"))
                                           (scored (and run-id (plusp (length run-id))
                                                        (score-summary run-id))))
                                       (nlk:make-json-object
                                        "arm_id" (member-value arm "arm_id")
                                        "label" (member-value arm "label")
                                        "run_id" (or run-id "")
                                        "state" (arm-run-state arm)
                                        "wall_seconds" (member-value arm "wall_seconds")
                                        "score" (or scored nil)
                                        "refused" (or (member-value arm "refused") ""))))
                      'vector)
      "scores" (if (some (lambda (arm) (let ((run-id (member-value arm "run_id")))
                                          (and run-id (plusp (length run-id))
                                               (score-summary run-id))))
                         arms)
                    "every arm's score is the summary its own score.json carries"
                    "no arm is scored yet: call woof_score on an arm's run_id")
      "next" "woof_compare two of the arms' run_ids to see them side by side"))))

(defun report-run (call)
  "woof_report: one run's setup, ending, maps, log tail and score; or a
campaign's arms."
  (let ((campaign-id (nlk:trimmed (or (report-read-campaign-id call) ""))))
    (when (plusp (length campaign-id))
      (return-from report-run (report-campaign campaign-id))))
  (let* ((run-id (nlk:trimmed (or (report-read-run-id call) "")))
         (record (and (plusp (length run-id)) (read-record run-id))))
    (unless (plusp (length run-id))
      (fail "give a run_id, or a campaign_id for a campaign's arms"))
    (unless record
      (fail "no run ~a under ~a" run-id (uiop:native-namestring (runs-root))))
    (let* ((folder (run-directory run-id))
           (maps (ignore-errors (image-paths run-id)))
           (shown (subseq maps 0 (min 4 (length maps))))
           (events (merge-pathnames "events.jsonl" folder))
           (log (merge-pathnames "woof.log" folder))
           (score (read-json (merge-pathnames "score.json" folder))))
      (nlk:encode-json-object
       (nlk:make-json-object
        "ok" t
        "run" (run-summary record)
        "plan" (or (record-value record "plan") "")
        "folder" (uiop:native-namestring folder)
        "log" (or (record-value record "log") "")
        "maps" (coerce (loop for map in shown collect (uiop:native-namestring map)) 'vector)
        "media" (format nil "~{~a~^~%~}"
                         (loop for map in shown
                               collect (concatenate 'string "MEDIA:" (uiop:native-namestring map))))
        "events" (tail-text (if (probe-file events) (uiop:read-file-string events) "") 2000)
        "log_tail" (tail-text (if (probe-file log) (uiop:read-file-string log) "") 1200)
        "score" score
        "estimate" "run woof_plan on this run's plan.json to compare WOOF's predicted time with what happened")))))

;;; --- /woof ----------------------------------------------------------------------

(defun run-slash (args session-id)
  "The /woof command: no argument, the command and the runs; a run id, that run."
  (declare (ignore session-id))
  (let ((argument (nlk:trimmed (or args ""))))
    (if (plusp (length argument))
        (status-text argument)
        (let ((object (nlk:decode-json (doctor))))
          (format nil "~a ~a  ~a~%~@{~a~%~}"
                  (or (nlk:json-value object :string "woof") "woof")
                  (or (nlk:json-value object :string "version") "(no version)")
                  (or (nlk:json-value object :string "runs_dir") "")
                  (loop for record in (list-runs)
                        collect (let ((summary (run-summary record)))
                                  (format nil "~a  ~a  ~a maps  ~a"
                                          (record-value summary "run_id")
                                          (record-value summary "state")
                                          (record-value summary "maps" :integer)
                                          (record-value summary "label")))))))))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell woof
  (:section ("woof")
    (:guide "point it at the WOOF command this machine has (pip install recast-woof, or WOOF's own install guide); runs_dir is where runs and their maps live; nothing here downloads WOOF")
    ("woof" :string :required t
     :doc "the WOOF command: its name on PATH, or the absolute path to it")
    ("runs_dir" :path
     :doc "where runs and their maps live; unset, woof-runs/ under your home")
    ("python" :string
     :doc "the interpreter that runs WOOF, for woof_score's observation doors; unset, the python3 beside the woof command")
    ("max_images" :integer :default 4 :min 1
     :doc "the most maps one woof_show answer carries"))
  (:help :woof
         "woof: size, launch, watch and show a WOOF weather simulation on this machine's GPU"
         *manual*)
  (:tool "woof_doctor" +diagnose-schema+ #'doctor)
  (:tool "woof_plan" (nlk:contract 'plan-check) #'check-plan)
  (:tool "woof_launch" (nlk:contract 'run-launch) #'launch)
  (:tool "woof_status" (nlk:contract 'run-read) #'run-status)
  (:tool "woof_show" (nlk:contract 'map-read) #'show-maps)
  (:tool "woof_stop" (nlk:contract 'run-read) #'stop-run)
  (:tool "woof_compare" (nlk:contract 'compare-read) #'compare-runs)
  (:tool "woof_report" (nlk:contract 'report-read) #'report-run)
  (:tool "woof_catalog" (nlk:contract 'catalog-read) #'show-catalog)
  (:tool "woof_docs" (nlk:contract 'docs-read) #'search-docs)
  (:tool "woof_campaign" (nlk:contract 'campaign-call) #'campaign)
  (:tool "woof_score" (nlk:contract 'score-call) #'run-score)
  (:command "woof" 'run-slash
            :description "WOOF: the command, and the weather runs this cell started")
  (:start (lambda ()
            (nle:notice (format nil "woof: runs live in ~a" (uiop:native-namestring (runs-root)))
                        :key "nodecode-woof")
            (nle:on-stop (lambda () (nle:notice nil :key "nodecode-woof"))))))
