;;;; wake-sources.lisp --- what changed outside a wake's own image, read at $0.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A wake asks every function on *WAKE-SOURCES* what happened since its
;;;; cursor, before it spends a model call: each source answers a list of
;;;; one-line strings, and an empty answer from every source lets the tick
;;;; skip. These four look OUTWARD, at what the in-image digest of turns and
;;;; cron outcomes does not see:
;;;;
;;;;   WAKE-SOURCE-RED-CHECKS  CI runs that failed on a repository the prs
;;;;                           cell watches, finished since the cursor
;;;;   WAKE-SOURCE-WAITS       answers marked as waiting on their person that
;;;;                           crossed *WAKE-WAIT-THRESHOLD* since the cursor
;;;;   WAKE-SOURCE-PROMISES    our own answers since the cursor that promise a
;;;;                           follow-up ("I'll keep an eye on it")
;;;;   WAKE-SOURCE-FOLDERS     files changed since the cursor in the folders
;;;;                           *WAKE-WATCHED-FOLDERS* names
;;;;
;;;; Every fact is reported ONCE: a source reads only what crossed into the
;;;; window (SINCE, now], so a cursor that moves on admission never shows the
;;;; same line twice and a skipped tick loses nothing. A source that does not
;;;; apply -- no prs cell loaded, no folder named -- answers NIL. A source
;;;; that cannot read signals: the runner shows it as unavailable, never as
;;;; "nothing changed", because an unknown is not a none.
;;;;
;;;; Each source is a reader and a pure LINES function; the tests drive the
;;;; pure halves with data of the shapes the readers return.

(in-package #:nodecode-cron)

(defvar *wake-sources* '()
  "Functions (SINCE-UNIVERSAL-TIME) -> list of one-line strings the wake runner
calls at a due tick.")

(defparameter *wake-wait-threshold* (* 2 60 60)
  "Seconds an answer may wait on its person before a wake hears of it.")

(defparameter *wake-promise-anchors*
  '("keep an eye" "check back" "follow up" "report back" "let you know" "circle back")
  "The phrases searched for in the log; *WAKE-PROMISE-SCAN* keeps the ones that
are our own first-person promise.")

(defparameter *wake-promise-scan*
  "(?i)\\b(?:I'll|I will|I'm going to|we'll|we will)\\s+(?:\\S+\\s+){0,2}?(?:keep an eye|check back|follow up|report back|let you know|circle back)"
  "A sentence that promises a later look, said by the agent itself.")

(defparameter *wake-watched-folders* '()
  "Folders whose files a wake hears about when they change (not recursive).")

(defparameter *wake-source-cap* 8
  "Lines one source contributes; the rest are counted, not listed.")

(defparameter *wake-line-cap* 200
  "Characters one line may take.")

(defun cap-lines (lines)
  "LINES clipped to one line each, at most *WAKE-SOURCE-CAP* of them, and a
count of the rest."
  (let ((clipped (mapcar (lambda (line) (nlk:one-line line :cap *wake-line-cap*)) lines))
        (extra (- (length lines) *wake-source-cap*)))
    (if (plusp extra)
        (append (subseq clipped 0 *wake-source-cap*) (list (format nil "... and ~d more" extra)))
        clipped)))

(defun in-window-p (at since now)
  "Whether universal time AT falls in (SINCE, NOW]."
  (and at (> at since) (<= at now)))

(defun age-text (seconds)
  (multiple-value-bind (hours rest) (floor seconds 3600)
    (if (plusp hours) (format nil "~dh" hours) (format nil "~dm" (floor rest 60)))))

;;; --- CI runs gone red ------------------------------------------------------------

(defun red-run-lines (repo runs since now)
  "One line per failed run in RUNS (GitHub's workflow_runs objects) of REPO
that finished in (SINCE, NOW]."
  (loop for run in runs
        for finished = (nlk:iso-universal (nlk:json-value run :string "updated_at"))
        for pulls = (coerce (or (nlk:json-array run "pull_requests") #()) 'list)
        when (and (equal (nlk:json-value run :string "conclusion") "failure")
                  (in-window-p finished since now))
          collect (format nil "CI red: ~a ~a on ~a~{ #~d~} -- ~a"
                          repo (nlk:json-value run :string "name")
                          (nlk:json-value run :string "head_branch")
                          (mapcar (lambda (pull) (nlk:json-value pull :integer "number")) pulls)
                          (nlk:json-value run :string "html_url"))))

(defun wake-source-red-checks (since)
  "Failed CI runs on the prs cell's repositories since SINCE; NIL when that
cell is not loaded."
  ;; One listing per repository per tick, through the prs cell's own GitHub
  ;; call and token: the newest failed runs, kept when they finished in the
  ;; window. A run that started before the cursor and failed after it counts.
  (let ((prs (find-package "NODECODE-PRS"))
        (now (get-universal-time)))
    (when prs
      (flet ((prs (name &rest arguments) (apply (find-symbol name prs) arguments)))
        (cap-lines
         (loop for repo in (prs "SETTING" :repos)
               append (red-run-lines
                       repo
                       (coerce (or (nlk:json-array (prs "API" :get (prs "PATH" repo "/actions/runs?status=failure&per_page=30"))
                                                   "workflow_runs")
                                   #())
                               'list)
                       since now)))))))

;;; --- answers waiting on their person ----------------------------------------------

(defun wait-lines (sessions since now)
  "One line per session in SESSIONS (NLK:LIST-SESSIONS plists) whose answer
has waited on its person past *WAKE-WAIT-THRESHOLD*, crossing it in (SINCE, NOW]."
  ;; The directory clears :WAITS once a later turn begins, so a session still
  ;; marked has had no answer since; its :UPDATED-AT is when it went quiet.
  (loop for session in sessions
        for waits = (getf session :waits)
        for quiet = (and waits (nlk:iso-universal (getf session :updated-at)))
        when (and quiet (in-window-p (+ quiet *wake-wait-threshold*) since now))
          collect (format nil "waiting on its person ~a: ~a~@[ -- ~a~]"
                          (age-text (- now quiet))
                          (let ((title (getf session :title)))
                            (if (plusp (length title)) title (getf session :id)))
                          (and (plusp (length waits)) waits))))

(defun wake-source-waits (since)
  "Answers that started waiting too long on their person since SINCE."
  (cap-lines (wait-lines (nlk:list-sessions) since (get-universal-time))))

;;; --- our own promises -------------------------------------------------------------

(defun own-session-p (session-id)
  "Whether SESSION-ID is a cron job's own session, a wake's included: its
promises are its own to keep."
  (uiop:string-prefix-p "cron-" session-id))

(defun promise-lines (rows since now &key (body (lambda (row) (getf row :text))))
  "One line per row in ROWS (NLK:RECALL-SEARCH rows) an assistant said in
(SINCE, NOW] that promises a later look, once per log position, quoting the
promise from where it starts. BODY reads a row's whole text."
  ;; A search row's text is a window around the query's first word, which
  ;; need not hold the promise; BODY is asked only for rows in the window.
  (loop with seen = (make-hash-table :test #'equal)
        for row in rows
        for fresh = (and (equal (getf row :role) "assistant")
                         (in-window-p (nlk:iso-universal (getf row :at)) since now)
                         (not (own-session-p (getf row :session-id)))
                         (not (gethash (getf row :seq) seen)))
        for text = (and fresh (setf (gethash (getf row :seq) seen) t) (or (funcall body row) ""))
        for start = (and text (ppcre:scan *wake-promise-scan* text))
        when start
          collect (format nil "promised in ~a: ~a"
                          (let ((title (getf row :title)))
                            (if (plusp (length title)) title (getf row :session-id)))
                          (subseq text start))))

(defun wake-source-promises (since)
  "Follow-ups our own answers promised since SINCE, from every session's log."
  (cap-lines
   (promise-lines (loop for anchor in *wake-promise-anchors*
                        ;; answers only: tool traffic quoting the phrase crowds them out
                        append (nlk:recall-search anchor :kinds '("turn.assistant_message_completed")
                                                         :limit 30))
                  since (get-universal-time)
                  :body (lambda (row) (getf (nlk:recall-entry (getf row :seq)) :text)))))

;;; --- watched folders --------------------------------------------------------------

(defun folder-lines (folder since now)
  "One line per file directly in FOLDER written in (SINCE, NOW]."
  (unless (uiop:directory-exists-p (uiop:ensure-directory-pathname folder))
    (error "watched folder ~a does not exist" folder))
  (loop for file in (uiop:directory-files (uiop:ensure-directory-pathname folder))
        when (in-window-p (file-write-date file) since now)
          collect (format nil "changed: ~a" (uiop:native-namestring file))))

(defun wake-source-folders (since)
  "Files changed since SINCE in *WAKE-WATCHED-FOLDERS*; NIL when none is named."
  (let ((now (get-universal-time)))
    (cap-lines (loop for folder in *wake-watched-folders*
                     append (folder-lines folder since now)))))

(dolist (source '(wake-source-red-checks wake-source-waits wake-source-promises wake-source-folders))
  (pushnew source *wake-sources*))
