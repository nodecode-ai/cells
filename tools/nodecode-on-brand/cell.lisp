;;;; cell.lisp --- brand profiles, the tools, /on-brand, the outbound gate.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode cell, NOT part of the organism core: nothing in Nodecode names
;;;; this system. The folder loader loads it at boot while its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `on-brand' section, registers the four onbrand_* tools
;;;; and /on-brand, puts (help :on-brand) on the manual, and hooks two
;;;; places:
;;;;
;;;;   :TOOL   an outbound call -- a tool whose name sends, posts or
;;;;           publishes -- carrying copy that breaks a hard rule is refused
;;;;           before it leaves the machine, and the model is told what to fix.
;;;;   NLE::READ-HARNESS-SECTIONS  the active brand's rules ride every model
;;;;           request, so a first draft starts on brand instead of being
;;;;           corrected after the fact.
;;;;
;;;; The linter is in rules.lisp; what passes is decided there, never by a
;;;; model. This file is state, the boundaries, and the surface.
;;;;
;;;; A Nodecode rewrite of Mhiah/On-Brand (see NOTICE).

(in-package #:nodecode-on-brand)

(nlk:define-peripheral on-brand :not-running t :lock "on-brand")

;;; --- the manual --------------------------------------------------------------------
;;; (help :on-brand) answers it while the cell runs; a request carries the one
;;; line the :HELP clause below gives, never this text.

(defparameter +primer+
  "Brand voice and compliance rules, through the nodecode-on-brand cell: four tools the
model calls, and /on-brand. Every check is a deterministic linter -- the same copy always
scores the same, and no model grades its own work. Every draft that still fails says
exactly what is wrong; nothing is silently passed.
  onbrand_check     score copy 0-100 against a brand and list every rule it breaks;
                    fix=true also corrects the exact spellings the profile pins.
  onbrand_rules     create or update a profile: person (third, first, any), tone, banned
                    words, preferred spellings, required phrases, packs, and the
                    character, hashtag, emoji and exclamation limits.
  onbrand_learn     draft a profile from 2 or more approved posts.
  onbrand_voiceover on-brand copy as a script that reads aloud: no markdown or links,
                    numbers and money spelled out.
  /on-brand         status, list, use BRAND, off, show [BRAND], new BRAND,
                    set BRAND FIELD VALUE, ban BRAND WORD, unban BRAND WORD,
                    spell BRAND WRONG RIGHT, require BRAND PHRASE, pack BRAND crypto|health [off],
                    check TEXT, fix TEXT, log, gate on|off, delete BRAND.
While a brand is active its rules ride every model request, and a tool call that sends,
posts or publishes copy breaking a hard rule is refused before it goes out. Profiles and
the check log are files in the folder the `on-brand' section names."
  "What (help :on-brand) answers while the cell runs.")

;;; --- where the brands live -----------------------------------------------------------

(defconstant +history-limit+ 60 "How many checks the log keeps: the newest.")

(defun store-folder ()
  "The folder the profiles and the log are kept in, as the section names it."
  (uiop:ensure-directory-pathname (setting :store-dir)))

(defun state-file ()
  (merge-pathnames "state.lisp" (store-folder)))

(defun empty-state ()
  "The state a home with no profiles yet reads as: a fresh list, so a write
that fills it never grows the one every read starts from."
  (list :version 1 :active nil :profiles nil :history nil))

(defun read-state ()
  "The whole store: profiles, the active brand, the log."
  (let ((file (state-file)))
    (if (probe-file file)
        (handler-case
            (with-open-file (stream file :direction :input :if-does-not-exist nil)
              (let ((*read-eval* nil)
                    (*package* (find-package :keyword)))
                (let ((state (and stream (read stream nil nil))))
                  (if (listp state)
                      (list :version (or (getf state :version) 1)
                            :active (getf state :active)
                            :profiles (getf state :profiles)
                            :history (getf state :history))
                      (empty-state)))))
          (error () (empty-state)))
        (empty-state))))

(defun write-state (state)
  "STATE as the store's one file, whole: a write that fails leaves the old one."
  (ensure-directories-exist (state-file))
  (nlk:write-file-atomically
   (namestring (state-file))
   (with-standard-io-syntax
     (let ((*package* (find-package :keyword))
           (*print-pretty* t)
           (*print-circle* t))
       (format nil "~s~%" state)))))

;;; --- profiles -----------------------------------------------------------------------

(defun profiles ()
  "Every profile in the store, in the order they were created."
  (getf (read-state) :profiles))

(defun find-profile (name)
  "The profile called NAME (any case, slugged), or NIL."
  (and name
       (find (brand-slug name) (profiles)
             :key (lambda (profile) (getf profile :name)) :test #'equal)))

(defun active-name ()
  (getf (read-state) :active))

(defun active-profile ()
  "The brand enforcement is on for, or NIL."
  (let ((name (active-name)))
    (and name (find-profile name))))

(defun (setf active-name) (name)
  (with-on-brand-lock
    (let ((state (read-state)))
      (setf (getf state :active) (and name (brand-slug name)))
      (write-state state)
      (getf state :active))))

(defun save-profile (profile)
  "PROFILE into the store under its name, replacing the one there; answers it."
  (with-on-brand-lock
    (let* ((state (read-state))
           (others (remove (getf profile :name) (getf state :profiles)
                           :key (lambda (one) (getf one :name)) :test #'equal)))
      (setf (getf state :profiles) (append others (list profile)))
      (write-state state)))
  profile)

(defun delete-profile (name)
  "Forget the profile called NAME -- and the active brand when it was it."
  (let ((slug (brand-slug name)))
    (with-on-brand-lock
      (let* ((state (read-state))
             (before (getf state :profiles))
             (after (remove slug before :key (lambda (one) (getf one :name)) :test #'equal)))
        (unless (= (length before) (length after))
          (setf (getf state :profiles) after)
          (when (equal (getf state :active) slug)
            (setf (getf state :active) nil))
          (write-state state)
          t)))))

(defun resolve-profile (name)
  "NAME's profile, or the active one when NAME is blank."
  (if (and name (plusp (length (nlk:trimmed name))))
      (find-profile name)
      (active-profile)))

;;; --- the log -------------------------------------------------------------------------

(defun log-event (&rest pairs)
  "One line in the log: what was checked, by which brand, how it ended."
  (with-on-brand-lock
    (let* ((state (read-state))
           (entry (list* :at (nlk:iso-time (nlk:unix-now) :millis nil) pairs))
           (history (cons entry (getf state :history))))
      (setf (getf state :history) (subseq history 0 (min +history-limit+ (length history))))
      (write-state state)
      entry)))

(defun history (&optional (limit 10))
  "The newest LIMIT entries."
  (subseq (getf (read-state) :history) 0 (min limit (length (getf (read-state) :history)))))

(defun stats ()
  "How many were checked and how they ended, over the whole log."
  (let ((entries (getf (read-state) :history)))
    (list :checked (length entries)
          :passed (count "passed" entries :key (lambda (entry) (getf entry :outcome)) :test #'equal)
          :fixed (count "fixed" entries :key (lambda (entry) (getf entry :outcome)) :test #'equal)
          :blocked (count "blocked" entries :key (lambda (entry) (getf entry :outcome)) :test #'equal))))

;;; --- JSON, in and out ---------------------------------------------------------------

(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES -- (NAME TYPE DOC REQUIRED-P) ... -- as a tool schema."
  (list :description description
        :parameters
        (nlk:json-object
         "type" "object"
         "additionalProperties" :false
         "properties" (apply #'nlk:make-json-object
                             (loop for (name type doc) in properties
                                   nconc (list name (nlk:json-object "type" type "description" doc))))
         "required" (coerce (loop for property in properties
                                   when (fourth property) collect (first property))
                            'vector))))

(defun argument (arguments name)
  "ARGUMENTS' NAME, or NIL; ARGUMENTS is NIL when a call carries none."
  (and (hash-table-p arguments) (gethash name arguments)))

(defun argument-text (arguments name)
  (let ((value (argument arguments name)))
    (and (stringp value) value)))

(defun argument-flag (arguments name)
  (not (null (argument arguments name))))

(defun answer (&rest pairs)
  "PAIRS as a JSON object, as a tool answers."
  (nlk:encode-json-object (apply #'nlk:make-json-object pairs)))

(defun violations-json (verdict)
  "VERDICT's violations as a JSON array of objects."
  (coerce (loop for violation in (getf verdict :violations)
                collect (nlk:json-object "rule" (getf violation :rule)
                                          "severity" (getf violation :severity)
                                          "detail" (getf violation :detail)))
          'vector))

(defun details-json (verdict)
  "VERDICT's violations as a JSON array of their details, what a log line keeps."
  (coerce (loop for violation in (getf verdict :violations)
                collect (getf violation :detail))
          'vector))

(defun profile-json (profile)
  "PROFILE as a JSON object, the shape onbrand_rules answers."
  (nlk:json-object
   "name" (profile-field profile :name)
   "display_name" (profile-field profile :display-name "")
   "person" (profile-field profile :person "any")
   "tone" (profile-field profile :tone "")
   "banned" (coerce (profile-field profile :banned) 'vector)
   "preferred" (apply #'nlk:make-json-object
                       (loop for (wrong . right) in (profile-field profile :preferred)
                             nconc (list wrong right)))
   "required" (coerce (profile-field profile :required) 'vector)
   "packs" (coerce (profile-field profile :packs) 'vector)
   "max_chars" (profile-field profile :max-chars 0)
   "max_hashtags" (profile-field profile :max-hashtags -1)
   "max_emojis" (profile-field profile :max-emojis -1)
   "max_exclamations" (profile-field profile :max-exclamations -1)
   "examples" (coerce (profile-field profile :examples) 'vector)
   "notes" (profile-field profile :notes "")))

;;; --- onbrand_check ------------------------------------------------------------------

(defparameter *check-schema*
  (tool-schema
   "Score copy against a brand's voice and compliance rules (0-100) and list every rule it breaks. Set fix=true to also get a corrected version: the exact spellings the profile pins are repaired, and what is left is listed. Use it before publishing anything for a brand, or when the operator asks whether copy is on brand."
   '(("text" "string" "The copy to check." t)
     ("brand" "string" "Brand profile name. Defaults to the active brand.")
     ("fix" "boolean" "Also answer a corrected version, with fixable rules applied.")))
  "The onbrand_check tool.")

(defun check (text &optional brand)
  "TEXT against BRAND (or the active brand): the verdict plist."
  (let ((profile (resolve-profile brand)))
    (unless profile
      (fail "no brand: pass a brand name, or turn one on with /on-brand use BRAND"))
    (lint text profile)))

(defun tool-check (arguments)
  (let ((text (argument-text arguments "text"))
        (profile (resolve-profile (argument-text arguments "brand")))
        (fix (argument-flag arguments "fix")))
    (unless (and text (plusp (length (nlk:trimmed text))))
      (fail "onbrand_check needs the copy to check, in \"text\""))
    (unless profile
      (fail "no brand: pass \"brand\", or turn one on with /on-brand use BRAND"))
    (let* ((first (lint text profile))
           (fixed (and fix (mechanical-fix text profile)))
           (second (and fixed (lint fixed profile))))
      (log-event :kind "check" :brand (getf profile :name) :brand-label (profile-label profile)
                 :outcome (cond ((null fix) (if (getf first :passed) "passed" "flagged"))
                                ((getf second :passed) "fixed")
                                (t "flagged"))
                 :score-before (getf first :score) :score-after (if second (getf second :score) (getf first :score))
                 :remaining (mapcar (lambda (violation) (getf violation :detail))
                                    (getf (or second first) :violations)))
      (apply #'answer
             (append (list "brand" (profile-label profile)
                           "score" (getf first :score)
                           "passed" (if (getf first :passed) t :false)
                           "violations" (violations-json first))
                     (when fix
                       (list "fixed_text" fixed
                             "fixed_score" (getf second :score)
                             "remaining" (details-json second)
                             "outcome" (if (getf second :passed) "fixed" "flagged"))))))))

;;; --- onbrand_rules ------------------------------------------------------------------

(defparameter *rules-schema*
  (tool-schema
   "Create or update a brand profile's rules, or read them. Use it when the operator describes how a brand should sound: who does the talking, the tone, words to avoid, exact spellings, phrases every draft needs, compliance packs, and the length, hashtag, emoji and exclamation limits. A list replaces the old list; a field you leave out keeps what it had."
   '(("brand" "string" "Brand profile name (created when new)." t)
     ("display_name" "string" "The brand as a person reads it.")
     ("person" "string" "third: talk about the brand; first: talk as 'we'; any.")
     ("tone" "string" "The voice, in a few words.")
     ("banned" "array" "Words and phrases the brand never says.")
     ("preferred" "object" "Wrong spelling -> the brand's exact spelling.")
     ("required" "array" "Phrases every draft must contain.")
     ("packs" "array" "Compliance packs: crypto, health.")
     ("max_chars" "integer" "Longest draft in characters; 0 for no limit.")
     ("max_hashtags" "integer" "Most hashtags a draft carries; -1 for no limit.")
     ("max_emojis" "integer" "Most emoji a draft draws; -1 for no limit.")
     ("max_exclamations" "integer" "Most exclamation marks a draft carries; -1 for no limit.")
     ("examples" "array" "On-brand sample lines.")
     ("activate" "boolean" "Make this the active brand.")))
  "The onbrand_rules tool.")

(defparameter +editable+
  '((:display-name . "display_name") (:person . "person") (:tone . "tone")
    (:banned . "banned") (:preferred . "preferred") (:required . "required")
    (:packs . "packs") (:max-chars . "max_chars") (:max-hashtags . "max_hashtags")
    (:max-emojis . "max_emojis") (:max-exclamations . "max_exclamations")
    (:examples . "examples") (:notes . "notes"))
  "The profile fields onbrand_rules edits, and the wire name each reads.")

(defun tool-rules (arguments)
  (let* ((name (argument-text arguments "brand"))
         (existing (find-profile name))
         (data (loop for (key . wire) in +editable+
                     for value = (argument arguments wire)
                     when (not (null value))
                       nconc (list key value))))
    (unless (and name (plusp (length (nlk:trimmed name))))
      (fail "onbrand_rules needs a brand name"))
    (let* ((base (or (loop for (key . wire) in +editable+
                           for value = (and existing (getf existing key))
                           when value nconc (list key value))
                     (list :name name :display-name name)))
           (profile (apply #'make-profile
                           (list* :name (or (getf existing :name) name) (append data base)))))
      (save-profile profile)
      (when (argument-flag arguments "activate")
        (setf (active-name) (getf profile :name)))
      (answer "saved" (profile-json profile)
              "active" (if (equal (active-name) (getf profile :name)) t :false)
              "created" (if existing :false t)))))

;;; --- onbrand_learn ------------------------------------------------------------------

(defparameter *learn-schema*
  (tool-schema
   "Draft a brand profile from 2 or more sample posts the operator provides (their real, approved copy). It is drafted in Lisp, at no cost, from what the samples show -- who does the talking, how many emoji, hashtags and exclamation marks the brand permits itself, the exact spelling of the brand's name, and whether a compliance pack applies -- saved, and answered for the operator to review and change with onbrand_rules or /on-brand."
   '(("brand" "string" "Brand profile name (created when new)." t)
     ("display_name" "string" "The brand as a person reads it.")
     ("samples" "array" "Approved posts, one per entry; 2 at least." t)
     ("activate" "boolean" "Make this the active brand.")))
  "The onbrand_learn tool.")

(defun learn-profile (name samples &optional display-name)
  "A profile drafted from SAMPLES: what they show, nothing a model guessed."
  (let* ((samples (string-list (or samples '())))
         (display (if (and display-name (plusp (length (nlk:trimmed display-name))))
                      (nlk:trimmed display-name)
                      name))
         (mentions (loop for sample in samples
                         sum (if (scan (phrase-scanner name) sample) 1 0)))
         (plural (loop for sample in samples
                       sum (length (cl-ppcre:all-matches +first-person-plural+ sample))))
         (crypto (pack "crypto"))
         (pack-names (if (and crypto
                               (some (lambda (sample) (scan (getf crypto :triggers) sample)) samples))
                         '("crypto")
                         '())))
    (make-profile
     :name name
     :display-name display
     :person (cond ((and (plusp plural) (> plural mentions)) "first")
                   ((plusp mentions) "third")
                   (t "any"))
     ;; The highest count used in any single sample is the brand's own ceiling.
     :max-emojis (loop for sample in samples maximize (count-emojis sample))
     :max-hashtags (loop for sample in samples maximize (floor (length (cl-ppcre:all-matches +hashtag+ sample)) 2))
     :max-exclamations (loop for sample in samples maximize (count #\! sample))
     :preferred (remove nil (list (unless (string-equal display (string-upcase display))
                                    (cons (string-upcase display) display))
                                  (unless (string-equal display (string-downcase display))
                                    (cons (string-downcase display) display))))
     :packs pack-names
     :examples (subseq samples 0 (min 2 (length samples))))))

(defun tool-learn (arguments)
  (let* ((name (argument-text arguments "brand"))
         (samples (string-list (argument arguments "samples"))))
    (unless (and name (plusp (length (nlk:trimmed name))))
      (fail "onbrand_learn needs a brand name"))
    (when (< (length samples) 2)
      (fail "onbrand_learn needs at least 2 approved posts, in \"samples\""))
    (let ((profile (learn-profile name samples (argument-text arguments "display_name"))))
      (save-profile profile)
      (when (argument-flag arguments "activate")
        (setf (active-name) (getf profile :name)))
      (answer "saved" (profile-json profile)
              "review" (format nil "Review these rules with the operator: the person, ~
the emoji, hashtag and exclamation ceilings and the spelling of the name come from the ~
samples; the tone, the banned words and any required phrase are theirs to add.")))))

;;; --- onbrand_voiceover --------------------------------------------------------------

(defparameter *voiceover-schema*
  (tool-schema
   "Turn on-brand copy into a script that reads naturally aloud: no markdown, links or symbols, numbers and money spelled out, filler openers gone. It checks the copy against the brand first and answers the score beside the script."
   '(("text" "string" "The copy to turn into a script." t)
     ("brand" "string" "Brand profile name. Defaults to the active brand.")
     ("ssml" "boolean" "Add SSML pause tags where the copy breaks a paragraph.")))
  "The onbrand_voiceover tool.")

(defparameter +ones+
  #("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten"
    "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen"
    "eighteen" "nineteen")
  "A number spelled.")

(defparameter +tens+
  #("" "" "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety")
  "A round ten, spelled.")

(defparameter +scale+
  '((1000000000 . "billion") (1000000 . "million") (1000 . "thousand"))
  "The sizes a spoken number names, largest first.")

(defun number-words (number)
  "NUMBER as it is said aloud, to the billions."
  (cond ((minusp number) (concatenate 'string "minus " (number-words (- number))))
        ((< number 20) (aref +ones+ number))
        ((< number 100)
         (let ((tens (floor number 10)) (rest (mod number 10)))
           (if (zerop rest) (aref +tens+ tens)
               (format nil "~a ~a" (aref +tens+ tens) (aref +ones+ rest)))))
        ((< number 1000)
         (let ((hundreds (floor number 100)) (rest (mod number 100)))
           (if (zerop rest) (format nil "~a hundred" (aref +ones+ hundreds))
               (format nil "~a hundred ~a" (aref +ones+ hundreds) (number-words rest)))))
        (t (loop for (size . name) in +scale+
                 when (>= number size)
                   return (let ((head (floor number size)) (rest (mod number size)))
                            (if (zerop rest)
                                (format nil "~a ~a" (number-words head) name)
                                (format nil "~a ~a ~a" (number-words head) name
                                        (number-words rest))))))))

(defun spelled-number (digits)
  "A number written as DIGITS -- commas and a decimal point allowed -- as it is said."
  (let ((digits (remove #\, digits)))
    (if (position #\. digits)
        (let* ((point (position #\. digits))
               (whole (subseq digits 0 point))
               (fraction (subseq digits (1+ point))))
          (format nil "~a point ~{~a~^ ~}"
                  (if (plusp (length whole)) (number-words (parse-integer whole)) "zero")
                  (loop for character across fraction
                        when (digit-char-p character)
                          collect (aref +ones+ (digit-char-p character)))))
        (number-words (or (ignore-errors (parse-integer digits)) 0)))))

(defun speakify (text &key ssml)
  "TEXT as a script for reading aloud => (:text ... :changes (...) :words N
:seconds N)."
  (let ((changes '())
        (text (or text "")))
    (flet ((did (label)
             (unless (member label changes :test #'equal)
               (push label changes))))
      (when (cl-ppcre:scan "```" text) (did "code blocks removed")
           (setf text (cl-ppcre:regex-replace-all "(?s)```.*?```" text " ")))
      (when (cl-ppcre:scan "`[^`]+`" text) (did "inline code unwrapped")
           (setf text (cl-ppcre:regex-replace-all "`([^`]+)`" text "\\1")))
      (when (cl-ppcre:scan "\\[[^\\]]+\\]\\([^)]*\\)" text) (did "links read as their text")
           (setf text (cl-ppcre:regex-replace-all "\\[([^\\]]+)\\]\\([^)]*\\)" text "\\1")))
      (when (cl-ppcre:scan "https?://\\S+" text) (did "web addresses dropped")
           (setf text (cl-ppcre:regex-replace-all "https?://\\S+" text " ")))
      (when (cl-ppcre:scan "(?m)^\\s{0,3}#{1,6}\\s+" text) (did "headings flattened")
           (setf text (cl-ppcre:regex-replace-all "(?m)^\\s{0,3}#{1,6}\\s+" text "")))
      (when (cl-ppcre:scan "(\\*\\*|__|\\*|_)" text) (did "emphasis markers removed")
           (setf text (cl-ppcre:regex-replace-all "(\\*\\*\\*|\\*\\*|__|_)" text "")))
      (when (cl-ppcre:scan "(?m)^\\s*[-*+]\\s+" text) (did "bullets read as sentences")
           (setf text (cl-ppcre:regex-replace-all "(?m)^\\s*[-*+]\\s+" text "")))
      (when (cl-ppcre:scan "(?m)^\\s*\\|.*\\|\\s*$" text) (did "table rows removed")
           (setf text (cl-ppcre:regex-replace-all "(?m)^\\s*\\|.*\\|\\s*$" text " ")))
      (when (plusp (count-emojis text)) (did "emoji removed")
           (setf text (remove-if (lambda (character)
                                    (and (emoji-p character)
                                         (not (member (char-code character) '(#xFE0F #x200D #xFE0E)))))
                                  text)))
      (when (cl-ppcre:scan "^(certainly|absolutely|of course|sure thing|great question)\\b" text)
        (did "filler opener removed")
        (setf text (cl-ppcre:regex-replace "^\\s*(?i:certainly|absolutely|of course|sure thing|great question)[!,.]*\\s*"
                                            text "")))
      (dolist (pair '(("\\be\\.g\\.,?" . "for example,") ("\\bi\\.e\\.,?" . "that is,")
                      ("\\betc\\." . "and so on.") ("\\bvs\\.?" . "versus")
                      ("\\bw/o\\b" . "without") ("\\bw/\\b" . "with")))
        (when (cl-ppcre:scan (car pair) text)
          (did "abbreviations said in full")
          (setf text (cl-ppcre:regex-replace-all (car pair) text (cdr pair)))))
      (dolist (pair '(("\\bdo not\\b" . "don't") ("\\bdoes not\\b" . "doesn't")
                      ("\\bis not\\b" . "isn't") ("\\bare not\\b" . "aren't")
                      ("\\bcannot\\b" . "can't") ("\\bwill not\\b" . "won't")
                      ("\\bit is\\b" . "it's") ("\\bthat is\\b" . "that's")
                      ("\\bwe are\\b" . "we're") ("\\byou are\\b" . "you're")
                      ("\\bthey are\\b" . "they're") ("\\blet us\\b" . "let's")))
        (when (cl-ppcre:scan (car pair) text)
          (did "contractions made")
          (setf text (cl-ppcre:regex-replace-all (car pair) text (cdr pair)))))
      (when (cl-ppcre:scan "\\$\\s?\\d[\\d,]*(\\.\\d+)?" text)
        (did "money spelled out")
        (setf text (cl-ppcre:regex-replace-all
                    "\\$\\s?(\\d[\\d,]*(\\.\\d+)?)"
                    text
                    (lambda (match &rest registers)
                      (declare (ignore match))
                      (format nil "~a dollars" (spelled-number (first registers))))
                    :simple-calls t)))
      (when (cl-ppcre:scan "\\d" text)
        (did "numbers spelled out")
        (setf text (cl-ppcre:regex-replace-all
                    "(?<![\\w.%])(\\d[\\d,]*(?:\\.\\d+)?)(?![\\w%)])"
                    text
                    (lambda (match &rest registers)
                      (declare (ignore match))
                      (spelled-number (first registers)))
                    :simple-calls t)))
      (setf text (cl-ppcre:regex-replace-all "[ \\t]+" text " "))
      (setf text (nlk:trimmed (cl-ppcre:regex-replace-all "\\n{2,}" text (format nil "~%~%"))))
      (when (and ssml (plusp (length text)))
        (did "SSML pauses added")
        (setf text (if (find #\Newline text)
                       (cl-ppcre:regex-replace-all "\\n+" text " <break time=\"0.6s\"/> ")
                       (concatenate 'string text " <break time=\"0.6s\"/>")))))
    (let ((words (count-if (lambda (word) (plusp (length word)))
                           (cl-ppcre:split "\\s+" text))))
      (list :text text
            :changes (nreverse changes)
            :words words
            :seconds (round words 2.5)))))

(defun tool-voiceover (arguments)
  (let* ((text (argument-text arguments "text"))
         (profile (resolve-profile (argument-text arguments "brand"))))
    (unless (and text (plusp (length (nlk:trimmed text))))
      (fail "onbrand_voiceover needs the copy to read, in \"text\""))
    (let* ((verdict (and profile (lint text profile)))
           (spoken (speakify text :ssml (argument-flag arguments "ssml"))))
      (answer "script" (getf spoken :text)
              "brand" (if profile (profile-label profile) :false)
              "brand_score" (if verdict (getf verdict :score) :false)
              "spoken_changes" (coerce (getf spoken :changes) 'vector)
              "words" (getf spoken :words)
              "approx_seconds" (getf spoken :seconds)))))

;;; --- the gate ------------------------------------------------------------------------

(defparameter +gate-patterns+
  '("send" "post" "tweet" "publish" "reply" "cast" "(^|_)dm($|_)" "broadcast" "announce"
    "webhook" "create_message" "discord" "telegram" "slack" "schedule_(post|tweet)"
    "create_(post|tweet)")
  "The tool names that reach people: a call to one carries outbound copy.")

(defparameter +never-gate+
  '("onbrand_check" "onbrand_rules" "onbrand_learn" "onbrand_voiceover" "read_file"
    "search_files" "web_search" "web_extract" "session_search" "react_to_message")
  "The tools whose text is never copy on its way out.")

(defparameter +text-keys+
  '("text" "content" "message" "body" "caption" "tweet" "status" "post" "html" "markdown"
    "description" "comment" "reply")
  "The argument names that hold the copy a tool call would send.")

(defun outbound-p (tool-name)
  "TOOL-NAME sends, posts, publishes or messages => T."
  (let* ((name (string-downcase (or tool-name "")))
         (declared (setting :gate-patterns))
         (patterns (if (or (null declared)
                           (and (vectorp declared) (zerop (length declared))))
                       +gate-patterns+
                       (coerce declared 'list))))
    (and (plusp (length name))
         (not (member name +never-gate+ :test #'equal))
         (some (lambda (pattern)
                 (ignore-errors (cl-ppcre:scan pattern name)))
               patterns))))

(defun outbound-text (arguments)
  "The copy ARGUMENTS would send, one paragraph a field, or NIL."
  (let ((found '()))
    (labels ((walk (node key depth)
               (when (<= depth 4)
                 (typecase node
                   (string (when (and (member key +text-keys+ :test #'equal)
                                      (plusp (length (nlk:trimmed node))))
                             (push (nlk:trimmed node) found)))
                   (hash-table (maphash (lambda (name value)
                                          (walk value (string-downcase name) (1+ depth)))
                                        node))
                   (vector (loop for value across node
                                 do (walk value key (1+ depth))))))))
      (walk arguments "" 0))
    (and found (format nil "~{~a~^~%~%~}" (nreverse found)))))

(defun gate-message (tool-name profile verdict)
  "What the model reads when a call is refused."
  (format nil "ERROR: on-brand refused ~a: this copy breaks ~a's brand rules (~d/100).~%~{  - ~a~%~}~%Rewrite the copy to fix every point above, then call ~a again.~%~a"
          tool-name
          (profile-label profile)
          (getf verdict :score)
          (loop for violation in (getf verdict :violations)
                when (equal (getf violation :severity) +hard+)
                  collect (getf violation :detail))
          tool-name
          (profile-brief profile)))

(defun gate (op next)
  "The :TOOL point: an outbound call carrying off-brand copy fails before it
sends; every other call runs as it was made."
  (let* ((profile (active-profile))
         (tool-name (getf op :name))
         (text (and profile
                     (setting :gate)
                     (outbound-p tool-name)
                     (outbound-text (getf op :arguments)))))
    (if (null text)
        (funcall next op)
        (let ((verdict (lint text profile)))
          (log-event :kind "gate" :brand (getf profile :name) :brand-label (profile-label profile)
                     :tool tool-name
                     :outcome (if (getf verdict :passed) "passed" "blocked")
                     :score (getf verdict :score)
                     :detail (mapcar (lambda (violation) (getf violation :detail))
                                     (getf verdict :violations)))
          (if (getf verdict :passed)
              (funcall next op)
              (nle:failure (gate-message tool-name profile verdict)))))))

;;; --- the standing rules in every request ----------------------------------------------

(defparameter *draft-instruction*
  "Put every piece of copy you write for this brand (posts, tweets, threads, captions, emails,
announcements) in its own fenced block that starts with ```copy and ends with ```. One block per
post. Keep explanations outside the blocks."
  "The instruction that makes a draft findable, and a table its own.")

(defparameter *framing*
  "[On-Brand is active for ~a. Everything you write for this brand is checked against its rules before it ships.] ~a ~a"
  "The one harness section: who is active, the rules, where the copy goes.")

(defun brand-section (next session-id)
  "The standing harness section: the active brand's rules, in every request."
  (let ((profile (active-profile)))
    (if profile
        (acons "on-brand"
               (format nil *framing* (profile-label profile) (profile-brief profile)
                       *draft-instruction*)
               (remove "on-brand" (funcall next session-id) :key #'car :test #'equal))
        (funcall next session-id))))

;;; --- the verbs, through eval ----------------------------------------------------------

(defun use-brand (name)
  "Turn enforcement on for the brand NAME."
  (let ((profile (find-profile name)))
    (unless profile
      (fail "no brand '~a': /on-brand starter adds two to try, /on-brand new ~a makes one"
            name name))
    (setf (active-name) (getf profile :name))
    (format nil "On-Brand is ON for ~a. Its rules ride every request, and outbound copy is ~
checked before it sends." (profile-label profile))))

(defun status ()
  "One line on what is on, and how the checks have gone."
  (let* ((profile (active-profile))
         (stats (stats)))
    (format nil "On-Brand: ~a~%gate=~a~%checked ~d | passed ~d | fixed ~d | blocked ~d~%~a"
            (if profile (format nil "ON for ~a" (profile-label profile)) "off")
            (if (setting :gate) "on" "off")
            (getf stats :checked) (getf stats :passed) (getf stats :fixed) (getf stats :blocked)
            "Type /on-brand help for commands.")))

(defun active-brand ()
  "The active brand's rules as a block, or a refusal."
  (let ((profile (active-profile)))
    (unless profile (fail "no brand is active: /on-brand use BRAND"))
    (profile-brief profile)))

(defun rules (name)
  "The profile NAME's rules as a block."
  (let ((profile (find-profile name)))
    (unless profile (fail "no brand '~a': /on-brand list" name))
    (profile-brief profile)))

(defun learn (name samples)
  "Draft the profile NAME from SAMPLES (a list of approved posts), save it, and
answer its rules for review."
  (setf samples (string-list samples))
  (when (< (length samples) 2)
    (fail "a profile is drafted from 2 approved posts at least"))
  (let ((profile (learn-profile name samples)))
    (save-profile profile)
    (format nil "Drafted ~a from ~d posts; review and change any rule with /on-brand set|ban|spell|require|pack~%~%~a"
            (profile-label profile) (length samples) (profile-brief profile))))

(defun voiceover (text &optional brand)
  "TEXT as a script that reads aloud, checked against BRAND first."
  (let* ((profile (resolve-profile brand))
         (verdict (and profile (lint text profile)))
         (spoken (speakify text)))
    (format nil "~a~%~a"
            (if verdict
                (format nil "~a (~d/100)" (profile-label profile) (getf verdict :score))
                "no brand active: the script is unchecked")
            (getf spoken :text))))

;;; --- /on-brand ------------------------------------------------------------------------

(defparameter +help+
  "On-Brand commands
  /on-brand                      status: active brand, the gate, the totals
  /on-brand list                 every brand profile
  /on-brand use BRAND            turn enforcement on for a brand
  /on-brand off                  turn enforcement off
  /on-brand show [BRAND]        a brand's rules
  /on-brand new BRAND            create an empty profile
  /on-brand starter              add two example profiles to try
  /on-brand set BRAND FIELD VALUE  person, tone, display_name, max_chars,
                                 max_hashtags, max_emojis, max_exclamations
  /on-brand ban BRAND WORD ...   /on-brand unban BRAND WORD
  /on-brand spell BRAND WRONG RIGHT
  /on-brand require BRAND PHRASE
  /on-brand pack BRAND crypto|health [off]
  /on-brand check TEXT           score TEXT against the active brand
  /on-brand fix TEXT             the exact spellings, corrected
  /on-brand log                  the last checks, fixes and blocks
  /on-brand gate on|off          refuse off-brand outbound tool calls
  /on-brand delete BRAND"
  "What /on-brand help answers.")

(defparameter *starter-profiles*
  (list (make-profile :name "nimbus" :display-name "Nimbus Protocol" :person "third"
                      :tone "calm, precise, quietly confident; plain and factual"
                      :banned '("revolutionary" "game-changer" "unlock" "to the moon" "LFG")
                      :preferred '(("NIMBUS" . "Nimbus") ("nimbus protocol" . "Nimbus Protocol")
                                   ("defi" . "DeFi"))
                      :packs '("crypto") :max-chars 280 :max-hashtags 1 :max-emojis 1
                      :max-exclamations 0
                      :examples '("Nimbus Protocol has opened a new feature to its community. The team explains how it works first, then points to where the details live."))
        (make-profile :name "pixel-pals" :display-name "Pixel Pals" :person "first"
                      :tone "playful, warm, community-first; short sentences"
                      :banned '("utility" "roadmap drop" "alpha leak")
                      :preferred '(("pixel pals" . "Pixel Pals") ("PIXEL PALS" . "Pixel Pals"))
                      :packs '("crypto") :max-chars 280 :max-hashtags 2 :max-emojis 3
                      :max-exclamations 2))
  "Two example brands, so a new home has something to try.")

(defparameter +int-fields+ '("max_chars" "max_hashtags" "max_emojis" "max_exclamations")
  "The numeric fields /on-brand set writes.")

(defun show-profile (profile)
  "PROFILE as a person reads it, one rule a line."
  (let ((lines (list (format nil "~a (~a)" (profile-label profile) (getf profile :name)))))
    (loop for (key . label) in '(("person" . :person) ("tone" . :tone) ("banned" . :banned)
                                 ("preferred" . :preferred) ("required" . :required)
                                 ("packs" . :packs) ("max_chars" . :max-chars)
                                 ("max_hashtags" . :max-hashtags) ("max_emojis" . :max-emojis)
                                 ("max_exclamations" . :max-exclamations)
                                 ("examples" . :examples) ("notes" . :notes))
          for value = (profile-field profile label)
          when (and value (not (member value '("" 0 -1 nil) :test #'equal)))
            do (push (format nil "  ~a: ~a" key value) lines))
    (format nil "~{~a~^~%~}" (nreverse lines))))

(defun edit-profile (name function)
  "NAME's profile through FUNCTION, saved; answers it as a person reads it."
  (let ((profile (find-profile name)))
    (unless profile
      (fail "no brand '~a': /on-brand new ~a creates it" name name))
    (let ((edited (funcall function profile)))
      (save-profile (if (consp edited) edited profile)))
    (show-profile (find-profile name))))

(defun run-command (args session-id)
  "One /on-brand invocation => the answer's text."
  (declare (ignore session-id))
  (running-settings)
  (let* ((words (remove "" (cl-ppcre:split "\\s+" (or args "")) :test #'equal))
         (command (string-downcase (or (first words) "")))
         (rest (rest words)))
    (cond
      ((member command '("" "status") :test #'equal) (status))
      ((member command '("help" "-h" "--help") :test #'equal) +help+)
      ((equal command "list")
       (let ((profiles (profiles)))
         (if (null profiles)
             "No brands yet. /on-brand starter adds two to try, /on-brand new BRAND makes one."
             (format nil "~{~a~^~%~}"
                     (loop for profile in profiles
                           collect (format nil "~a ~a (~a)"
                                           (if (equal (getf profile :name) (active-name)) "*" " ")
                                           (profile-label profile) (getf profile :name)))))))
      ((and (equal command "use") rest) (use-brand (first rest)))
      ((equal command "off")
       (setf (active-name) nil)
       "On-Brand is off.")
      ((equal command "show")
       (let ((profile (resolve-profile (first rest))))
         (if profile (show-profile profile) "No brand selected.")))
      ((and (equal command "new") rest)
       (let ((name (format nil "~{~a~^ ~}" rest)))
         (when (find-profile name)
           (fail "'~a' already exists: /on-brand show ~a" name (first rest)))
         (let ((profile (save-profile (make-profile :name name :display-name name))))
           (format nil "Created ~a. Add rules with /on-brand set|ban|spell|require|pack, or ask ~
the model -- it calls onbrand_rules." (profile-label profile)))))
      ((equal command "starter")
       (dolist (profile *starter-profiles*)
         (unless (find-profile (getf profile :name))
           (save-profile profile)))
       "Added example brands: Nimbus Protocol (nimbus), Pixel Pals (pixel-pals). /on-brand use nimbus")
      ((and (equal command "delete") rest)
       (if (delete-profile (first rest))
           (format nil "Deleted ~a." (first rest))
           (format nil "No brand '~a'." (first rest))))
      ((and (equal command "set") (>= (length rest) 3))
       (let* ((name (first rest))
              (field (string-downcase (second rest)))
              (value (format nil "~{~a~^ ~}" (cddr rest))))
         (cond
           ((equal field "person")
            (unless (member value '("third" "first" "any") :test #'equal)
              (fail "person must be third, first or any"))
            (edit-profile name (lambda (profile) (setf (getf profile :person) value))))
           ((member field +int-fields+ :test #'equal)
            (unless (every #'digit-char-p (remove-if (lambda (c) (member c '(#\- #\+))) value))
              (fail "~a must be a number" field))
            (edit-profile name (lambda (profile)
                                 (setf (getf profile (intern (string-upcase (substitute #\- #\_ field)) :keyword))
                                       (parse-integer value)))))
           ((member field '("tone" "display_name" "notes") :test #'equal)
            (edit-profile name (lambda (profile)
                                 (setf (getf profile (intern (string-upcase (substitute #\- #\_ field)) :keyword))
                                       value))))
           (t (fail "unknown field '~a': see /on-brand help" field)))))
      ((and (equal command "ban") (>= (length rest) 2))
       (edit-profile (first rest)
                     (lambda (profile)
                       (dolist (word (rest rest))
                         (pushnew word (getf profile :banned) :test #'equal))
                       profile)))
      ((and (equal command "unban") (>= (length rest) 2))
       (edit-profile (first rest)
                     (lambda (profile)
                       (setf (getf profile :banned)
                             (remove (second rest) (getf profile :banned) :test #'string-equal))
                       profile)))
      ((and (equal command "spell") (= (length rest) 3))
       (edit-profile (first rest)
                     (lambda (profile)
                       (setf (getf profile :preferred)
                             (acons (second rest) (third rest)
                                    (remove (second rest) (getf profile :preferred)
                                            :key #'car :test #'string-equal)))
                       profile)))
      ((and (equal command "require") (>= (length rest) 2))
       (edit-profile (first rest)
                     (lambda (profile)
                       (pushnew (format nil "~{~a~^ ~}" (rest rest)) (getf profile :required)
                                :test #'string-equal)
                       profile)))
      ((and (equal command "pack") (>= (length rest) 2))
       (let ((pack (string-downcase (second rest)))
             (off (and (third rest) (string-equal (third rest) "off"))))
         (unless (pack pack)
           (fail "unknown pack '~a': ~{~a~^, ~}" pack (mapcar (lambda (spec) (getf spec :name)) +packs+)))
         (edit-profile (first rest)
                       (lambda (profile)
                         (setf (getf profile :packs)
                               (if off
                                   (remove pack (getf profile :packs) :test #'equal)
                                   (adjoin pack (getf profile :packs) :test #'equal)))
                         profile))))
      ((and (equal command "check") rest)
       (let ((profile (active-profile))
             (text (format nil "~{~a~^ ~}" rest)))
         (unless profile (fail "no brand is active: /on-brand use BRAND"))
         (let ((verdict (lint text profile)))
           (log-event :kind "check" :brand (getf profile :name)
                      :brand-label (profile-label profile)
                      :outcome (if (getf verdict :passed) "passed" "flagged")
                      :score (getf verdict :score)
                      :detail (mapcar (lambda (violation) (getf violation :detail))
                                      (getf verdict :violations)))
           (format nil "~a: ~a" (profile-label profile) (verdict-summary verdict)))))
      ((and (equal command "fix") rest)
       (let ((profile (active-profile))
             (text (format nil "~{~a~^ ~}" rest)))
         (unless profile (fail "no brand is active: /on-brand use BRAND"))
         (let* ((fixed (mechanical-fix text profile))
                (verdict (lint fixed profile)))
           (log-event :kind "check" :brand (getf profile :name)
                      :brand-label (profile-label profile)
                      :outcome (if (getf verdict :passed) "fixed" "flagged")
                      :score-before (getf (lint text profile) :score)
                      :score-after (getf verdict :score))
           (format nil "~a~%~a: ~a" fixed (profile-label profile) (verdict-summary verdict)))))
      ((equal command "log")
       (let ((rows (history 10)))
         (if (null rows)
             "Nothing checked yet."
             (format nil "~{~a~^~%~}"
                     (loop for row in rows
                           for detail = (getf row :detail)
                           collect (format nil "[~a] ~a ~a ~a: ~a"
                                           (getf row :outcome)
                                           (getf row :brand-label)
                                           (or (getf row :tool) (getf row :kind))
                                           (or (getf row :score) (getf row :score-after))
                                           (if detail (format nil "~{~a~^; ~}" detail) "clean")))))))
      ((member command '("gate" "autofix") :test #'equal)
       (if (member (second rest) '("on" "off") :test #'equal)
           (format nil "~a is set in the on-brand section of config.jsonc (gate): ~a; ~
change it in /settings, then (restart-cells \"nodecode-on-brand\")"
                   command (if (setting :gate) "on" "off"))
           (format nil "~a is ~a; /on-brand gate on|off says where it is set"
                   command (if (setting :gate) "on" "off"))))
      (t +help+))))

;;; --- the declaration ------------------------------------------------------------------

(nle:define-cell on-brand
  (:section ("on-brand")
    (:guide "store_dir is the folder the brand profiles and the check log are kept in; gate refuses a tool call that sends off-brand copy")
    ("store_dir" :path :required t
     :doc "the folder the brand profiles and the check log are kept in")
    ("gate" :boolean :default t
     :doc "refuse a tool call that sends, posts or publishes copy breaking a hard rule")
    ("gate_patterns" :list
     :doc "tool-name patterns that send, post or publish; the built-in list when empty"))
  (:help :on-brand "on-brand: brand voice and compliance rules - /on-brand use BRAND, or onbrand_check" +primer+)
  (:tool "onbrand_check" *check-schema* #'tool-check)
  (:tool "onbrand_rules" *rules-schema* #'tool-rules)
  (:tool "onbrand_learn" *learn-schema* #'tool-learn)
  (:tool "onbrand_voiceover" *voiceover-schema* #'tool-voiceover)
  (:command "on-brand" #'run-command
            :catches 'on-brand-error
            :description "Brand voice rules: switch brands, edit the rules, check copy"
            :argument-hint "[use BRAND | list | show | check TEXT | fix TEXT | log | help]")
  (:hook :tool #'gate)
  (:hook 'nle::read-harness-sections #'brand-section)
  (:start (lambda ()
            ;; A folder has to be named before a profile can have a file: a
            ;; section that names none refuses at start, naming the member.
            (unless (plusp (length (or (setting :store-dir) "")))
              (error 'nlk:config-refusal
                     :detail "on-brand: store_dir names the folder the brand profiles are kept in; set it in the on-brand section of config.jsonc or in /settings"))
            (ensure-directories-exist (state-file)))))
