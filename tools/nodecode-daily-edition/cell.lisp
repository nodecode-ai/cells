;;;; cell.lisp --- the daily_edition tool, /daily-edition, the manual, the section.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin this folder rewrites is a desktop page: it composes an edition
;;;; in the renderer, opens stories in place, keeps unread and saved state,
;;;; styles headlines with a model the reader names, and prints a broadsheet
;;;; through the local print queue. A cell contributes verbs, not windows, so
;;;; what is carried over is the edition itself and the way it is asked for:
;;;; interest phrases expanded into query feeds, one story once, cross-posts
;;;; merged with every publisher credited, consensus winning the lead, reprints
;;;; withheld, seven back issues kept under the Nodecode home, an optional
;;;; editorial voice for the headlines, and a plain-text front page the model,
;;;; a cron job or /daily-edition can print.

(in-package #:nodecode-daily-edition)

(defparameter *state* nil
  "The archive in memory: a plist (:archive ISSUES), an issue an
EDITION-SUMMARY. NIL until the cell starts.")

(defparameter +archive-limit+ 7
  "How many back issues are kept, as the plugin kept seven.")

;;; --- the archive ---------------------------------------------------------------------

(defun state-path ()
  "The one file the archive lives in, under the Nodecode home."
  (nlk:home "daily-edition/state.sexp"))

(defun empty-state () (list :archive '()))

(defun load-state ()
  "Read the archive off disk, or start empty; a damaged file never stops the cell."
  (let ((path (state-path)))
    (setf *state*
          (if (probe-file path)
              (handler-case
                  (let* ((*read-eval* nil)
                         (form (read-from-string (uiop:read-file-string path) nil nil)))
                    (if (and (consp form) (listp (getf form :archive)))
                        form
                        (empty-state)))
                (error () (empty-state)))
              (empty-state)))))

(defun save-state ()
  "Write the archive where LOAD-STATE reads it, atomically."
  (let ((*print-pretty* t) (*print-length* nil) (*print-level* nil) (*print-circle* nil))
    (ensure-directories-exist (state-path))
    (nlk:write-file-atomically (state-path) (format nil "~s~%" *state*)
                               :mode #o600 :directory-mode #o700)))

(defun archive () (getf *state* :archive))

(defun (setf archive) (value) (setf (getf *state* :archive) value))

(defun today ()
  (or *now* (get-universal-time)))

(defun previous-issue ()
  "The newest archived issue from a day before today: what a reprint is
withheld against. Re-printing on the same day replaces that day's page."
  (let ((today (iso-date-of (today))))
    (or (find-if-not (lambda (issue) (equal (getf issue :iso-date) today)) (archive))
        (first (archive)))))

(defun record-edition (edition)
  "File EDITION as the newest back issue, one a day, seven kept."
  (setf (archive) (merge-archive (archive) (edition-summary edition) +archive-limit+))
  (ignore-errors (save-state)))

;;; --- the paper -----------------------------------------------------------------------

(defun clean-interests (value)
  "VALUE, the model's interest phrases, as a list of trimmed phrases, or a
refusal naming the rule."
  (when (null value) (return-from clean-interests nil))
  (unless (listp value)
    (fail "interests must be a list of phrases"))
  (let ((phrases (remove "" (mapcar (lambda (item) (nlk:trimmed (princ-to-string item))) value)
                         :test #'equal)))
    (when (> (length phrases) 12)
      (fail "at most 12 interests per call"))
    (dolist (phrase phrases)
      (when (> (length phrase) 120)
        (fail "interest ~s is longer than 120 characters" (nlk:clip phrase 60))))
    phrases))

(defun compose-edition (&key interests max-stories)
  "The front page as it stands: INTERESTS (the section's when NIL) fetched,
clustered, ranked and withheld against the last issue from an earlier day.
=> (values EDITION STANDFIRST ERRORS)."
  (let* ((phrases (or (clean-interests interests) (setting :interests)))
         (sources '())
         (errors '()))
    (unless phrases
      (fail "no interest to compile an edition from: name one in the daily-edition section, ~
             or pass interests"))
    (dolist (phrase phrases)
      (handler-case
          (let ((source (fetch-topic phrase)))
            (push (list :source phrase :items (getf source :items)) sources))
        (error (condition)
          (push (format nil "~a: ~a" phrase (nlk:clip (format nil "~a" condition) 160)) errors))))
    (let* ((previous (previous-issue))
           (edition (build-edition :sources (nreverse sources)
                                    :previous previous
                                    :now (today)
                                    :max-stories (max 1 (min (or max-stories (setting :max-stories)) 40))
                                    :muted (setting :muted)
                                    :watchwords (setting :watchwords))))
      (values edition (edition-diff edition previous) (nreverse errors)))))

(defun answer (text)
  (nlk:clip text (setting :max-chars) :disclose "the daily-edition section's max_chars"))

(defun interests-line ()
  (let ((phrases (setting :interests)))
    (format nil "~d interest~:p: ~{~a~^, ~}" (length phrases) phrases)))

(defun sources-text ()
  "What the paper is drawing on right now."
  (format nil "~a~%provider: ~a~%~@[muted: ~{~a~^, ~}~%~]~@[watchwords: ~{~a~^, ~}~%~]~
               archive: ~d issue~:p~@[ (newest No. ~a, ~a)~]"
          (interests-line)
          (or (setting :provider) +provider+)
          (setting :muted)
          (setting :watchwords)
          (length (archive))
          (getf (first (archive)) :number)
          (getf (first (archive)) :date-label)))

(defun archive-text ()
  "The back issues, newest first."
  (if (null (archive))
      "No back issue yet: a printed edition is filed here."
      (format nil "~{~a~^~%~}"
              (loop for issue in (archive)
                    collect (format nil "No. ~a — ~a — ~d stories — ~a"
                                    (getf issue :number) (getf issue :date-label)
                                    (or (getf issue :story-count) 0)
                                    (let ((title (getf issue :lead-title)))
                                      (if (and title (plusp (length title))) title "no lead")))))))

(defun do-action (action &key interests max-stories)
  "One action of the daily_edition tool and /daily-edition, answered as text."
  (let ((action (string-downcase (or action "print"))))
    (cond
      ((member action '("sources" "sources?") :test #'equal) (sources-text))
      ((member action '("archive" "back-issues" "issues") :test #'equal) (archive-text))
      ((equal action "diff")
       (multiple-value-bind (edition standfirst errors)
           (compose-edition :interests interests :max-stories max-stories)
         (declare (ignore edition))
         (format nil "~a~@[~%~%Sources that could not be read:~%~{- ~a~^~%~}~]"
                 (getf standfirst :line) errors)))
      ((member action '("print" "edition" "paper") :test #'equal)
       (multiple-value-bind (edition standfirst errors)
           (compose-edition :interests interests :max-stories max-stories)
         (when (plusp (getf edition :story-count))
           (record-edition edition))
         (render-edition edition standfirst errors)))
      (t (fail "~s is no action: print, diff, sources or archive" action)))))

;;; --- the boundary --------------------------------------------------------------------

(nlk:define-contract daily-edition-call
  (:doc "Print the reader's Daily Edition: a dated front page composed from their saved interest phrases, with one story once (cross-posted stories merged under every publisher), consensus winning the lead, and anything printed in an earlier day withheld. Deterministic and model-free. Use it for morning briefings, cron jobs and questions about what is in today's paper.")
  ("action" :text :required t :options '("print" "diff" "sources" "archive")
   :doc "print: the front page as text; diff: what changed since the last edition; sources: what the paper draws on; archive: the back issues")
  ("interests" :array :of :string
   :doc "interest phrases to use instead of the saved ones for this call, e.g. [\"fusion energy\", \"housing\"]; at most 12")
  ("max_stories" :integer :default 20
   :doc "how many stories to print, 1 to 40 (20 when unset)"))

(defun daily-edition-tool (call)
  "The daily_edition tool: the action CALL names, answered as text."
  (handler-case
      (answer (do-action (daily-edition-call-action call)
                          :interests (daily-edition-call-interests call)
                          :max-stories (daily-edition-call-max-stories call)))
    (daily-edition-error (condition)
      (format nil "ERROR: ~a" (nlk:clip (format nil "~a" condition) 300)))))

;;; --- /daily-edition -------------------------------------------------------------------

(defun run-slash (args session-id)
  "/daily-edition [diff|sources|archive]: today's front page, what changed, what
it draws on, or the back issues."
  (declare (ignore session-id))
  (handler-case
      (let* ((text (nlk:trimmed (or args "")))
             (head (string-downcase (if (position #\Space text)
                                        (subseq text 0 (position #\Space text))
                                        text))))
        (answer (cond ((string= head "") (do-action "print"))
                      ((member head '("diff" "changed" "what-changed") :test #'equal) (do-action "diff"))
                      ((member head '("sources" "sources?") :test #'equal) (do-action "sources"))
                      ((member head '("archive" "issues" "back-issues") :test #'equal) (do-action "archive"))
                      (t (do-action "print" :interests (list text))))))
    (daily-edition-error (condition) (format nil "daily-edition: ~a" condition))))

;;; --- the declaration --------------------------------------------------------------------

(defun reset-state ()
  "Empty the archive, on disk too: the starting point every test wants, and
an operator's clean shelf."
  (setf *state* (empty-state))
  (ignore-errors (delete-file (state-path)))
  *state*)

(defun interest-list (value)
  "VALUE, the section's interests, one phrase each, or a refusal in words."
  (let ((phrases (remove "" (mapcar #'nlk:trimmed value) :test #'equal)))
    (unless phrases
      (nlk:config-error
       "daily-edition.interests names no interest: give at least one phrase to compile an edition from"))
    (dolist (phrase phrases)
      (when (> (length phrase) 120)
        (nlk:config-error "daily-edition.interests names ~s, longer than 120 characters"
                          (nlk:clip phrase 60))))
    (when (> (length phrases) 30)
      (nlk:config-error "daily-edition.interests names ~d interests: at most 30" (length phrases)))
    phrases))

(defparameter +manual+
  "The daily-edition cell compiles a dated front page from interest phrases -- you name
what you care about, not which feeds to follow:
  daily_edition {action:\"print\"}                     today's edition as text, filed as a back issue
  daily_edition {action:\"print\", interests:[\"fusion energy\"]}  one call's own interests
  daily_edition {action:\"diff\"}                      what changed since the last edition
  daily_edition {action:\"sources\"}                   the interests, the provider, the archive
  daily_edition {action:\"archive\"}                   the seven back issues
Every action is deterministic: no model is involved. An interest is expanded into the
provider's query feed (the section's provider, a public keyless news search by default), so
the phrasing leaves the machine when an edition is compiled. One story is printed once:
the same story from several mastheads is one line crediting each, the story more mastheads
carried leads, and anything the last edition from an earlier day printed is withheld.
/daily-edition prints, /daily-edition diff, /daily-edition sources, /daily-edition archive;
/daily-edition WORDS prints an edition for those words alone."
  "What (help :daily-edition) answers while the cell runs.")

(nle:define-cell daily-edition
  (:section ("daily-edition")
    (:guide "interests is the one required member: a phrase per interest, not a feed url. A present section is enabled; the tool and /daily-edition then compile a dated front page, withholding what an earlier day's edition already printed")
    ("interests" :list :required t
     :doc "the interest phrases the paper is compiled from, e.g. [\"fusion energy\", \"housing in Malaga\"]; the tool may name others for one call")
    ("max_stories" :integer :default 20 :min 1
                   :doc "how many stories one edition prints, 1 to 40")
    ("provider" :string :default +provider+
     :doc "the query feed an interest is expanded into; {query} is replaced by the phrase; the default is a public keyless news search")
    ("timeout_seconds" :integer :default 12 :min 1
                       :doc "one interest's fetch ceiling, in seconds")
    ("muted" :list
     :doc "interest phrases left out of the edition, e.g. [\"sports\"]")
    ("watchwords" :list
     :doc "words that star a story and keep it at the top of the page")
    ("max_chars" :integer :default 12000 :min 500
                 :doc "the longest answer one action gives, in characters"))
  (:settings (lambda (values table)
               (declare (ignore table))
               (setf (getf values :interests) (interest-list (getf values :interests)))
               values))
  (:tool "daily_edition" (nlk:contract 'daily-edition-call) 'daily-edition-tool)
  (:command "daily-edition" 'run-slash
            :catches 'daily-edition-error
            :description "Today's Daily Edition: a dated front page from your interests"
            :argument-hint "[diff | sources | archive]")
  (:help :daily-edition
         "daily-edition: a dated front page compiled from interest phrases - daily_edition {action:\"print\"}"
         +manual+)
  (:start (lambda ()
            (load-state)
            (nle:on-stop (lambda () (setf *state* nil))))))
