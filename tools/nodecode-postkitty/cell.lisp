;;;; cell.lisp --- the manual, the five tool schemas, /postkitty, the one declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original was five tools plus a skill. The tools are the five here;
;;;; the skill is +PRIMER+, which the model reads with (help :postkitty) and
;;;; whose one line rides every request. The operator's /postkitty lists the
;;;; platforms and their targets, the way the original's command did.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "postkitty": {"output_dir": "", "default_platforms": [], "loudness_lufs": -14}

(in-package #:nodecode-postkitty)

(defparameter +primer+
  "Postkitty turns one post -- photos, a landscape video, a draft, or only text -- into a folder of
files ready to upload to Instagram, TikTok, YouTube Shorts, X, LinkedIn, Threads, Bluesky and
Facebook, each checked against that platform's published limits. It never posts or uploads; the
user uploads the files in ready/ themselves. The tools decide whether a file passes, never you.
Work through the five tools in order; every one answers JSON, and an `error` member means fix the
call or tell the user.
  postkitty_scan  inputs (paths), text (the draft), platforms. Opens a job, reads each file's
      own header, plans one framing per target, and answers inputs, plan, fit (each platform's
      limit, unit and what the draft counts) and text (character, X weighted, grapheme, UTF-16,
      hashtag, mention and link counts). Keep the job id. Works with text only.
  postkitty_copy  job, advice, drafts, alt_text. Counts each draft with that platform's own
      rule, splits over-limit X, Threads and Bluesky text into a numbered thread when
      split_threads is true (the default), and writes post.txt or thread.txt, alt.txt and
      advice.md into ready/. Repeat with rewritten drafts until every platform reports ok.
  postkitty_make  job, targets, overrides. Renders every planned target with ffmpeg, checks the
      file against the platform's limits and puts only passing files in ready/. A target is
      uploadable only when its status is ready; skipped means ffmpeg is not on PATH. Writes
      board.html, the job's one page.
  postkitty_redo  job, target, layout, focus, trim, cover_at. Renders that one target again.
  postkitty_check  path, target. Holds a file the user brings from elsewhere against one target
      without rendering: the answer to \"will this upload to TikTok?\".
Layouts: crop fills the frame and keeps the middle; pad keeps the whole frame on black bars.
How to work: say what the tools measured, quote the numbers they return rather than your memory
of a platform's limits, put the point before the fold, do not put facts in the copy that are not
in the user's material, and never call a file ready unless the tool said so. Ask the user once
when a fact the post needs is missing; do not invent names, numbers, prices or dates."
  "What (help :postkitty) answers while the cell runs.")

;;;; --- the schemas -----------------------------------------------------------

(defun strings-schema (description)
  (nlk:make-json-object "type" "array" "items" (nlk:make-json-object "type" "string")
                         "description" description))

(defun field (type description)
  (nlk:make-json-object "type" type "description" description))

(defun object-schema (&rest pairs)
  (let ((required (loop for (key value) on pairs by #'cddr
                        when (eq value :required) collect key)))
    (nlk:make-json-object "type" "object"
                          "properties" (apply #'nlk:make-json-object
                                              (loop for (key value) on pairs by #'cddr
                                                    unless (eq value :required)
                                                      append (list key value)))
                          "required" (and required (coerce required 'vector)))))

(defun scan-schema ()
  (list :description
        "First step for any social post. Takes the user's photos, landscape videos and draft text, opens a job folder, reads each file, plans a framing per platform target and answers fit facts (sizes, character counts, limits) plus the job id. Renders nothing. Works with text only. Also the first call when the user only wants a draft fixed, or asks what to post where. Never posts or uploads."
        :parameters (object-schema
                     "inputs" (strings-schema "Absolute paths of images or videos.")
                     "text" (field "string" "The user's draft post, if any.")
                     "platforms" (strings-schema "Platform names (instagram, tiktok, youtube, x, linkedin, threads, bluesky, facebook) or target ids. Default: every target that fits the inputs.")
                     "job" (field "string" "Reuse an existing job id."))))

(defparameter +override-properties+
  (nlk:make-json-object
   "layout" (nlk:make-json-object "type" "string" "enum" (vector "crop" "pad")
                                    "description" "crop fills the frame and keeps the middle; pad keeps the whole frame on black bars.")
   "focus" (field "string" "auto, left, center or right: which part of the frame crop keeps.")
   "trim" (nlk:make-json-object "type" "array" "items" (nlk:make-json-object "type" "number")
                                  "description" "[start, end] seconds (video).")
   "cover_at" (field "number" "Cover frame time in seconds (video).")))

(defun copy-schema ()
  (list :description
        "Second step. Stores the advice and the per-platform drafts written for the user, counts each draft with that platform's real rule (X weighted, Bluesky graphemes and bytes, TikTok UTF-16, Instagram hashtags and mentions) and splits over-limit X, Threads and Bluesky text into a numbered thread. Writes advice.md and post.txt, thread.txt and alt.txt into ready/. Call it again with rewritten drafts until every platform reports ok; the drafts sent before stay, so send only the ones that changed."
        :parameters (object-schema
                     "job" (field "string" "The job id postkitty_scan answered.")
                     "job" :required
                     "advice" (field "object" "topic, audience, tone, key_message, ranking (list of {platform, reason}), tips (list of strings).")
                     "drafts" (field "object" "Platform name or target id -> draft string, or a list of strings for a thread.")
                     "alt_text" (field "object" "Target id or file name -> alt text. Describe only what is visible.")
                     "split_threads" (field "boolean" "Split over-limit text into a thread. Default true."))))

(defun make-schema ()
  (list :description
        "Third step. Renders every planned target with ffmpeg, checks each file against the platform's published limits, and puts only passing files into ready/. Writes board.html, the page of the whole job. Answers a status table plus media_line; only a target whose status is ready is ready to upload, and skipped means ffmpeg is not on PATH. A text-only job has nothing to render: make then lists the copy files already in ready/."
        :parameters (object-schema
                     "job" (field "string" "The job id postkitty_scan answered.")
                     "job" :required
                     "targets" (strings-schema "Subset of target ids or platforms.")
                     "overrides" (nlk:make-json-object "type" "object"
                                                         "description" "Target id -> {layout, focus, trim, cover_at}."
                                                         "additionalProperties" (nlk:make-json-object
                                                                                  "type" "object"
                                                                                  "properties" +override-properties+)))))

(defun redo-schema ()
  (list :description
        "Re-render one photo or video target after the user asks for a change: another layout, another part of the frame in focus, a trim or another cover frame. Same answer shape as postkitty_make. For a copy change call postkitty_copy instead."
        :parameters (object-schema
                     "job" (field "string" "The job id postkitty_scan answered.")
                     "job" :required
                     "target" (field "string" "The target id, for example tiktok.")
                     "target" :required
                     "layout" (nlk:make-json-object "type" "string" "enum" (vector "crop" "pad")
                                                      "description" "crop fills the frame and keeps the middle; pad keeps the whole frame on black bars.")
                     "focus" (field "string" "auto, left, center or right.")
                     "trim" (nlk:make-json-object "type" "array" "items" (nlk:make-json-object "type" "number")
                                                    "description" "[start, end] seconds (video).")
                     "cover_at" (field "number" "Cover frame time in seconds (video)."))))

(defun check-schema ()
  (list :description
        "Check a file the user brings from elsewhere against one platform target without rendering, for example 'will this upload to TikTok?'. Give path and target. Files made by postkitty_make or postkitty_redo are already checked."
        :parameters (object-schema
                     "path" (field "string" "The file to check.")
                     "path" :required
                     "target" (field "string" "The target id, for example tiktok or x_image.")
                     "target" :required)))

;;;; --- the five answers -------------------------------------------------------

(defun answer-scan (arguments)
  (answering (tool-scan arguments)))

(defun answer-copy (arguments)
  (answering (tool-copy arguments)))

(defun answer-make (arguments)
  (answering (tool-make arguments)))

(defun answer-redo (arguments)
  (answering (tool-redo arguments)))

(defun answer-check (arguments)
  (answering (tool-check arguments)))

(defun run-command (args session-id)
  "One /postkitty invocation: with no argument what the cell makes and every target, with a
platform name or a target id just that one."
  (declare (ignore session-id))
  (if (plusp (length (nlk:trimmed (or args ""))))
      (overview args)
      (usage-text)))

;;;; --- the settings the cell runs on -------------------------------------------

(defun take-settings ()
  "Copy the declared settings into the globals the tools read, and make the folder."
  (setf *output-dir* (or (setting :output-dir) "")
        *default-platforms* (setting :default-platforms)
        *loudness-lufs* (or (setting :loudness-lufs) -14))
  (when (plusp (length *output-dir*))
    (ensure-directories-exist (output-directory))))

;;;; --- the cell -----------------------------------------------------------------

(nle:define-cell postkitty
  (:section ("postkitty")
    (:guide "nothing is required: output_dir is where job folders go (empty means ./postkitty beside the working directory), default_platforms is what a scan makes when the caller names none, loudness_lufs is the audio target of a rendered video; ffmpeg and ffprobe on PATH are what render and probe video; next step: /postkitty lists every platform and target")
    ("output_dir" :string
     :doc "where job folders are written; empty means ./postkitty beside the working directory")
    ("default_platforms" :list
     :doc "platforms to make when the caller names none (empty means every target that fits)")
    ("loudness_lufs" :integer :default -14 :min -40
     :doc "the integrated loudness target, in LUFS, of a rendered video's audio"))
  (:help :postkitty
         "postkitty: one draft, photo or video into checked, upload-ready files for eight platforms - (help :postkitty)"
         +primer+)
  (:command "postkitty" 'run-command
            :description "Every platform target, with its size and limits"
            :argument-hint "[platform]"
            :session nil)
  (:tool "postkitty_scan" (scan-schema) #'answer-scan)
  (:tool "postkitty_copy" (copy-schema) #'answer-copy)
  (:tool "postkitty_make" (make-schema) #'answer-make)
  (:tool "postkitty_redo" (redo-schema) #'answer-redo)
  (:tool "postkitty_check" (check-schema) #'answer-check)
  (:start (lambda ()
            (take-settings)
            (nle:on-stop (lambda ()
                           (setf *output-dir* nil *default-platforms* nil))))))
