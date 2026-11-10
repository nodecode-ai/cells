;;;; cell.lisp --- the shelf at hermesmade.lol: preview, add and share bots.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin was one Python library with five surfaces: /sharebot, a CLI, a
;;;; desktop page and an agent tool, all over the shelf at hermesmade.lol. Here
;;;; the shelf is three tools and one command: preview a listing before anything
;;;; is installed, add it into the bots directory, and share a folder of your
;;;; own to it. Every byte goes to that one host, and a shared bot is packed
;;;; from an allowlist and scanned, as the plugin packed one.

(in-package #:nodecode-hermesmade)

;;; --- the manual ---------------------------------------------------------------

(defparameter +primer+
  "Bots are shared folders on a shelf at hermesmade.lol, and this cell is how a
session reads it and adds to it, through eval:
  (hermesmade:preview \"<id or link>\")  what the bot is: title, skills, schedules,
      the files it ships. Nothing is installed and nothing is written.
  (hermesmade:add \"<id or link>\")      download it, check its sha256, install it
      under the bots directory, and say where it went.
  (hermesmade:share \"/path/to/bot\" :title \"Chef\" :tagline \"...\")  pack that
      folder (SOUL.md, config.yaml, skills/, cron/), scan it for keys, and upload
      it for review under the section's handle; a name already shared here is
      updated, not duplicated.
  (hermesmade:mine)                      what this machine has shared.
  (hermesmade:unshare \"name\")           delete a listing shared from here.
A shared folder needs a SOUL.md of at least 20 characters. A package downloaded
from the shelf may hold only SOUL.md, config.yaml, distribution.yaml,
hermesmade-bot.json, skills/ and cron/: anything else refuses the install. A bot
is data -- its skills are Markdown and its schedules a JSON list; nothing in it
runs until you read it.
The operator runs the same things from /hermesmade: preview, add, share, mine,
unshare."
  "What (help :hermesmade) answers while the cell runs.")

(defparameter +bot-package-top+ "SOUL.md, config.yaml, distribution.yaml, hermesmade-bot.json, skills/ and cron/"
  "What a downloaded package may hold, in the words a reader needs.")

;;; --- what a listing says --------------------------------------------------------

(defun preview-text (reference)
  "REFERENCE's listing read out: what it ships, before anything is installed."
  (let* ((bot (bot-info (bot-id reference)))
         (entries (untar (gunzip (bot-package bot))))
         (files (loop for (name kind data) in entries
                      when (eq kind :file) collect name))
         (skills (skill-names files)))
    (with-output-to-string (out)
      (format out "Bot: ~a  (installs as `~a`~@[, v~a~])~%"
              (or (nlk:json-value bot :string "title") (nlk:json-value bot :string "name"))
              (nlk:json-value bot :string "name")
              (nlk:json-value bot :string "version"))
      (format out "On the shelf: ~a/bots/~a~%" (site) (bot-id reference))
      (format out "Ships ~d file~:p:~%" (length files))
      (dolist (file (sort (copy-list files) #'string<))
        (format out "  - ~a~%" file))
      (when skills
        (format out "Skills: ~{~a~^, ~}~%" skills))
      (let ((schedules (schedule-count files)))
        (when (plusp schedules)
          (format out "Schedules: ~d, and they arrive paused~%" schedules)))
      (format out "Never shipped: keys, tokens, memories, sessions, chat history, a model login.~%")
      (format out "Add it with: hermesmade add ~a~%" (bot-id reference)))))

(defun add-text (reference name)
  "REFERENCE installed under NAME, and what that did, in words."
  (let ((added (install reference :name name)))
    (with-output-to-string (out)
      (format out "Added ~a as bot `~a` in ~a.~%"
              (getf added :title) (getf added :name) (getf added :dir))
      (format out "~d file~:p written~@[, Skills: ~{~a~^, ~}~]~@[, ~d schedule~:p paused in cron/~].~%"
              (getf added :files) (getf added :skills) (getf added :schedules))
      (format out "Its SOUL.md is the personality: read it before you act as this bot.~%"))))

(defun share-text (folder title tagline &optional handle)
  "FOLDER packed, scanned and uploaded; a record of the listing, in words.
HANDLE overrides the section's, which is what a command's @you does."
  (let* ((plan (pack folder :title title :tagline tagline))
         (given (setting :handle))
         (handle (or (and handle (nlk:trimmed handle))
                     (and (stringp given) (nlk:trimmed given))
                     "")))
    (multiple-value-bind (record message)
        (upload plan handle)
      (with-output-to-string (out)
        (format out "~a~%" message)
        (format out "Listing: #~a~@[ ~a~] (v~a, ~d file~:p)~%"
                (getf record :id) (let ((url (getf record :url)))
                                    (and (plusp (length url)) url))
                (getf record :version) (length (getf plan :files)))
        (format out "Shared as @~a. hermesmade mine lists what this machine has shared.~%"
                (getf record :handle))))))

(defun shares-text ()
  "Every listing this machine has shared, in words."
  (let ((records (read-state)))
    (if (null records)
        "This machine has shared nothing yet: hermesmade share <folder> @you"
        (with-output-to-string (out)
          (format out "~d listing~:p shared from this machine:~%" (length records))
          (dolist (record records)
            (format out "  ~a v~a  ~a~%" (getf record :name) (getf record :version)
                    (let ((url (getf record :url)))
                      (if (plusp (length url)) url "waiting for review"))))))))

;;; --- the model's tools ----------------------------------------------------------

(nlk:define-contract preview-call
  (:doc "Show what a bot on the hermesmade.lol shelf is - its title, the files it ships, its skills
and its schedules - before anything is installed. Writes nothing.")
  ("bot" :text :required t
   :doc "the bot's id, or a hermesmade.lol/bots/... link"))

(defun tool-preview (call)
  "hermesmade_preview: one listing, read out."
  (preview-text (preview-call-bot call)))

(nlk:define-contract add-call
  (:doc "Add a bot from the hermesmade.lol shelf: download its package, verify its checksum, check
every file against what a bot may hold, then install it under the bots directory. Its skills are
Markdown and its schedules a paused list; nothing in it runs by itself.")
  ("bot" :text :required t
   :doc "the bot's id, or a hermesmade.lol/bots/... link")
  ("name" :text
   :doc "the folder name to install it under; the listing's own name when the call names none"))

(defun tool-add (call)
  "hermesmade_add: one bot installed, and where."
  (add-text (add-call-bot call) (add-call-name call)))

(nlk:define-contract share-call
  (:doc "Share a bot folder to the hermesmade.lol shelf for review: SOUL.md, config.yaml, skills/ and
cron/ are packed, scanned for keys, and uploaded under the section's handle. A listing already shared
under the same name is updated.")
  ("folder" :text :required t
   :doc "the folder holding the bot's SOUL.md")
  ("title" :text
   :doc "a human title for the listing")
  ("tagline" :text
   :doc "one line on what the bot does"))

(defun tool-share (call)
  "hermesmade_share: one folder shared, and the listing it is."
  (share-text (share-call-folder call) (share-call-title call) (share-call-tagline call)))

;;; --- the operator's command ------------------------------------------------------

(defun run-command (args session-id)
  "One /hermesmade invocation: preview, add, share, mine, unshare - or what is
shared from here when it names nothing."
  (declare (ignore session-id))
  (let* ((words (remove "" (uiop:split-string (or args "") :separator '(#\Space #\Tab))
                        :test #'string=))
         (verb (string-downcase (or (first words) "")))
         (rest (rest words)))
    (handler-case
        (cond ((member verb '("preview" "show" "peek") :test #'string=)
               (unless (first rest)
                 (fail "hermesmade: name a bot id or link: /hermesmade preview <id>"))
               (preview-text (first rest)))
              ((string= verb "add")
               (unless (first rest)
                 (fail "hermesmade: name a bot id or link: /hermesmade add <id>"))
               (add-text (first rest) (second rest)))
              ((string= verb "share")
               (unless (first rest)
                 (fail "hermesmade: name the folder to share: /hermesmade share <folder> [title] [@you]"))
               (let* ((handle (find-if (lambda (word) (and (plusp (length word))
                                                           (char= #\@ (char word 0))))
                                       rest))
                      (title (second rest))
                      (tagline (third rest)))
                 (if handle
                     (share-text (first rest) title tagline (string-left-trim "@" handle))
                     (share-text (first rest) title tagline nil))))
              ((member verb '("mine" "list") :test #'string=) (shares-text))
              ((member verb '("unshare" "delete") :test #'string=)
               (unless (first rest) (fail "hermesmade: name the listing: /hermesmade unshare <name>"))
               (unshare (first rest)))
              ((zerop (length verb)) (shares-text))
              (t (preview-text (first words))))
      (hermesmade-error (condition) (format nil "ERROR: ~a" (hermesmade-error-detail condition))))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell hermesmade
  (:section ("hermesmade")
    (:guide "nothing here is required to look or add; a handle is asked for only when you share, and it is the credit the listing carries; next step: /hermesmade shows what this machine has shared, and hermesmade preview <id> reads a listing")
    ("site" :string
     :doc "the shelf's address: https://hermesmade.lol unless you run your own")
    ("bots_dir" :path
     :doc "where added bots land: bots/ under the home unless this names a folder")
    ("handle" :string
     :doc "the @handle a shared listing is credited to, letters, digits and underscore")
    ("timeout_seconds" :integer :default 45 :min 1
     :doc "one request's ceiling")
    ("max_bytes" :integer :default 2000000 :min 1000
     :doc "how large a downloaded bot may be, in bytes"))
  (:help :hermesmade
         "hermesmade: the community bot shelf - preview, add and share bots (help :hermesmade)"
         +primer+)
  (:command "hermesmade" 'run-command
            :description "The bot shelf: preview, add or share a bot"
            :argument-hint "preview|add|share|mine|unshare")
  (:tool "hermesmade_preview" (nlk:contract 'preview-call) #'tool-preview)
  (:tool "hermesmade_add" (nlk:contract 'add-call) #'tool-add)
  (:tool "hermesmade_share" (nlk:contract 'share-call) #'tool-share)
  (:start (lambda ()
            (nle:notice (format nil "hermesmade: the shelf at ~a; bots land in ~a"
                                (site) (bots-directory))
                        :key "nodecode-hermesmade")
            (nle:on-stop (lambda () (nle:notice nil :key "nodecode-hermesmade"))))))
