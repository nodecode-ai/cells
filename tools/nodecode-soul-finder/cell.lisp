;;;; cell.lisp --- Soul Finder: draft or refine the agent's SOUL.md persona file.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two jobs, as the original plugin has them: a first-run offer when the
;;;; home has no real SOUL.md yet, and a guided interview that drafts one or
;;;; audits an existing one against the lean structure the bundled skill
;;;; describes. The offer is one standing notice, marked as made so it never
;;;; repeats; /soul submits the kickoff into the session the way the plugin
;;;; injected its kickoff message, and the two tools tell the model where the
;;;; file stands and let it close the flow after the operator approved.
;;;;
;;;; Nothing here reaches the network, and nothing leaves the machine: the
;;;; only file read is the SOUL.md the section names (the home's by default),
;;;; the only writes are two marker files of the cell's own, beside it.

(defpackage #:nodecode-soul-finder
  (:use #:cl)
  (:export #:start-cell #:stop-cell
           #:soul-path #:soul-text #:real-soul-p #:soul-mode
           #:soul-finder-status #:soul-finder-guide #:soul-finder-complete
           #:run-slash #:*soul-finder* #:soul-finder-error))

(in-package #:nodecode-soul-finder)

(nlk:define-peripheral soul-finder :not-running t)

(defparameter *home-soul* "SOUL.md"
  "The file an operator's SOUL.md is called under the home, as the channel
kit reads it too.")

(defparameter *marker-directory* "soul-finder"
  "The cell's own directory under the home: one marker file a flag, nothing
else. The original kept the same two booleans in its own state file.")

;;; The folder this file was loaded from: the skill text beside it is read at
;;; run time, never copied into an image, so a reader always sees the file.
(defun cell-directory ()
  "This cell's own folder, asked of ASDF at run time so a cached fasl
answers the folder the cell sits in now, not where it was compiled."
  (or (ignore-errors (asdf:system-source-directory "nodecode-soul-finder"))
      (uiop:pathname-directory-pathname (or *load-truename* *compile-file-truename*))))

(defun skill-file ()
  "The bundled workflow skill: the original's SKILL.md, whole."
  (merge-pathnames "skill/SKILL.md" (cell-directory)))

(defun marker-path (name)
  (nlk:home (format nil "~a/~a" *marker-directory* name)))

(defun flag (name)
  "Whether NAME's marker is there."
  (and (probe-file (marker-path name)) t))

(defun set-flag (name)
  "Keep NAME's marker, and answer what FLAG would."
  (let ((path (marker-path name)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (write-string "1" out))
    t))

(defun soul-path ()
  "The SOUL.md this cell works on: the section's soul_file, else the
home's SOUL.md."
  (let ((named (setting :soul-file)))
    (or (and (stringp named) (plusp (length named)) named)
        (nlk:home *home-soul*))))

(defun soul-text ()
  "The SOUL.md's trimmed text, or NIL when it is absent or blank."
  (let ((file (probe-file (soul-path))))
    (when file
      (let ((text (nlk:trimmed (or (nlk:read-text file) ""))))
        (and (plusp (length text)) text)))))

(defun non-blank-lines (text)
  (count-if (lambda (line)
              (plusp (length (string-trim '(#\Space #\Tab #\Return) line))))
            (uiop:split-string text :separator '(#\Newline))))

(defun real-soul-p ()
  "The original's heuristic: a meaningful SOUL.md, not missing and not a
tiny seed. A hand-written one is left alone."
  (let ((text (soul-text)))
    (and text
         (>= (non-blank-lines text) (setting :min-lines))
         (>= (length text) (setting :min-chars)))))

(defun soul-mode ()
  "\"new\" when there is no real SOUL.md yet, \"refine\" when there is."
  (if (real-soul-p) "refine" "new"))

(defun soul-class ()
  "Which of the skill's two classes the file is: a voice, or a persona."
  (let ((text (soul-text)))
    (cond ((null text) "none")
          ((>= (non-blank-lines text) (setting :persona-lines)) "persona")
          (t "voice"))))

;;; --- the tools -------------------------------------------------------------

(defparameter +status-schema+
  (list :description
        (concatenate 'string
                     "Return SOUL.md state: mode (new|refine), the file, its size and "
                     "class (voice or persona), and whether the flow was finished. "
                     "Use it before starting a soul flow and after finishing one.")
        :parameters (nlk:make-json-object "type" "object"
                                           "properties" (nlk:make-json-object))))

(defparameter +guide-schema+
  (list :description
        (concatenate 'string
                     "Read the Soul Finder workflow: audit what is in SOUL.md, separate "
                     "durable voice from project context, condense, and get the operator's "
                     "approval before writing. Load it before walking anyone through it.")
        :parameters (nlk:make-json-object "type" "object"
                                           "properties" (nlk:make-json-object))))

(defparameter +complete-schema+
  (list :description
        (concatenate 'string
                     "Mark the SOUL.md flow as finished. Call it ONLY after the operator "
                     "explicitly approved and the file was written.")
        :parameters (nlk:make-json-object "type" "object"
                                           "properties" (nlk:make-json-object))))

(defun soul-finder-status (&optional arguments)
  "soul_finder_status: where the SOUL.md stands, as JSON."
  (declare (ignore arguments))
  (let ((text (soul-text)))
    (nlk:encode-json-object
     (nlk:json-object
      "mode" (soul-mode)
      "class" (soul-class)
      "soul_file" (namestring (soul-path))
      "soul_exists" (if (probe-file (soul-path)) t nil)
      "soul_chars" (if text (length text) 0)
      "soul_lines" (if text (non-blank-lines text) 0)
      "flow_complete" (if (flag "complete") t nil)
      "nudge_sent" (if (flag "nudged") t nil)
      "skill_file" (namestring (skill-file))))))

(defun soul-finder-guide (&optional arguments)
  "soul_finder_guide: the bundled workflow skill, whole, as prose."
  (declare (ignore arguments))
  (let ((file (probe-file (skill-file))))
    (if file
        (or (nlk:read-text file) "")
        (format nil "the workflow skill is missing from this cell: ~a"
                (namestring (skill-file))))))

(defun soul-finder-complete (&optional arguments)
  "soul_finder_complete: the flow is done; the first-run offer never returns."
  (declare (ignore arguments))
  (set-flag "complete")
  (nlk:encode-json-object
   (nlk:json-object "success" t "message" "SOUL.md flow marked complete")))

;;; --- /soul -----------------------------------------------------------------

(defparameter *kickoff-new*
  (concatenate 'string
               "Let's build my SOUL.md, the personality file that shapes every "
               "conversation. Load the Soul Finder workflow with soul_finder_guide and "
               "walk me through it, starting with the mode question."))

(defparameter *kickoff-refine*
  (concatenate 'string
               "Let's audit my SOUL.md. Load the Soul Finder workflow with "
               "soul_finder_guide, read the file soul_finder_status names, compare it "
               "against the lean structure, and walk me through the refine flow."))

(defparameter +manual+
  (concatenate 'string
               "Soul Finder: the personality file a channel reads before every message, "
               "drafted or refined through a short interview. /soul alone picks the mode "
               "from the file; /soul new drafts one from scratch, /soul refine audits an "
               "existing one, /soul status prints where it stands. The model reads the "
               "workflow with (soul_finder_guide), asks, drafts, and writes the file only "
               "after approval; (soul_finder_complete) closes the flow so the first-run "
               "offer never comes back. Nothing leaves the machine."))

(defun kick-off (text session-id)
  "Submit the kickoff into SESSION-ID, as the original injected its message,
and answer it as the text every caller shows: a shell with no session, or one
that refuses the submit, still shows what to act on."
  (when session-id
    (ignore-errors
     (nle:submit session-id text :source "soul-finder" :source-id "kickoff")))
  text)

(defun run-slash (args session-id)
  "One /soul: new, refine, status, or the mode the file's own state picks."
  (let ((argument (string-downcase (nlk:trimmed (or args "")))))
    (cond ((member argument '("status" "info" "check") :test #'string=)
           (values (soul-finder-status) nil))
          ((string= argument "new")
           (values (kick-off *kickoff-new* session-id) nil))
          ((string= argument "refine")
           (values (kick-off *kickoff-refine* session-id) nil))
          (t (values (kick-off (if (string= (soul-mode) "new")
                                    *kickoff-new*
                                    *kickoff-refine*)
                                session-id)
                     nil)))))

;;; --- the cell ---------------------------------------------------------------

(nle:define-cell soul-finder
  (:section ("soul-finder")
    (:guide "nothing here is required: soul_file is the SOUL.md to shape (the home's by default), min_lines and min_chars are what counts as a real one, persona_lines where a file stops being a voice and becomes a persona; /soul drafts or audits it")
    ("soul_file" :path :default ""
     :doc "the SOUL.md to work on; empty is the home's SOUL.md")
    ("min_lines" :integer :default 8 :min 1
     :doc "non-blank lines a SOUL.md needs before it counts as written")
    ("min_chars" :integer :default 200 :min 1
     :doc "characters it needs as well")
    ("persona_lines" :integer :default 60 :min 1
     :doc "non-blank lines past which status calls the file a persona, not a voice")
    ("nudge" :boolean :default t
     :doc "offer a first-run SOUL.md once, when the home has none"))
  (:help :soul-finder
   "Soul Finder: draft or refine the SOUL.md a channel reads - /soul [new|refine|status]"
   +manual+)
  (:command "soul" 'run-slash
   :description "Soul Finder: draft or refine your agent's SOUL.md (new|refine|status)")
  (:tool "soul_finder_status" +status-schema+ 'soul-finder-status)
  (:tool "soul_finder_guide" +guide-schema+ 'soul-finder-guide)
  (:tool "soul_finder_complete" +complete-schema+ 'soul-finder-complete)
  (:start (lambda ()
            (when (setting :nudge)
              (unless (or (flag "nudged") (flag "complete") (real-soul-p))
                (set-flag "nudged")
                (nle:notice
                 (concatenate 'string
                              "no SOUL.md yet: /soul drafts one in a few plain "
                              "questions and it never asks twice")
                 :key "nodecode-soul-finder")
                (nle:on-stop (lambda ()
                               (nle:notice nil :key "nodecode-soul-finder"))))))))
