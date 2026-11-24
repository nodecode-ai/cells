;;;; cell.lisp --- the advice on the stream, the tool, /writing-guard, the section.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of MadeByBrain/hermes-extensions' writing-guard, a
;;;; Hermes plugin that hooked transform_llm_output: it appended one italic
;;;; line under every answer saying what read as machine written, and in fix
;;;; mode replaced the dashes by rule. This rewrite keeps the rules, the
;;;; report and the fix, and moves them onto the seam an answer actually
;;;; crosses: the advice on NLE::EMIT-STREAM-PART, so a fragment is fixed
;;;; where it is made and the line rides the answer itself, in every shell,
;;;; channel and transcript, rather than one harness's post-processing.
;;;;
;;;; The du/Sie switch store of the original (a file under the harness's home
;;;; keyed by session) is not reproduced: the answer's own paragraph mixing
;;;; and a form the operator fixed are read from the text, and a switch
;;;; between answers is what /writing-guard's last report names.

(in-package #:nodecode-writing-guard)

(nlk:define-peripheral writing-guard :not-running t)

;;; --- what this process has done, and the answer being written ------------------

(defvar *stats* (list :deltas 0 :fixed 0 :footers 0)
  "Deltas read, dashes replaced as they streamed, and lines appended, since
this process started: what /writing-guard reports, and /writing-guard reset clears.")

(defvar *paused* nil
  "Whether /writing-guard off has stopped the checks until the next on, or a
restart: the advice reads it live, so the turn already streaming is quiet.")

(defvar *source* nil
  "The answer being written, as the model said it, before any dash was fixed:
an output stream, or NIL between answers.")

(defvar *last* nil
  "The last report this process made, for /writing-guard.")

(defun stats-reset ()
  (setf *stats* (list :deltas 0 :fixed 0 :footers 0)))

(defun stat (key) (getf *stats* key 0))

(defun accumulate (text)
  "Keep what the model said, so the line under the answer can speak of the
whole answer and not of the last fragment."
  (when (and text (plusp (length text)))
    (unless *source* (setf *source* (make-string-output-stream)))
    (write-string text *source*)))

(defun answer-so-far ()
  "The answer accumulated since the stream started, or NIL."
  (when *source*
    (get-output-stream-string *source*)))

(defun quiet-p ()
  "Whether the checks are off, by a setting or by /writing-guard off."
  (or *paused* (string= (setting :mode) "off")))

(defun plist-with (plist key value)
  "PLIST with KEY's value VALUE, in place: the initargs a stream part is made
from, one member replaced."
  (loop for (member value-here) on plist by #'cddr
        if (eq member key)
          append (list member value)
        else
          append (list member value-here)))

;;; --- the check, as the settings read it ---------------------------------------

(defun check-with-settings (text &key fix (address nil) (track-switches t))
  "TEXT checked under the section's settings, the way the hook and the tool
both check it."
  (check-text text
              :fix fix
              :dash-style (setting :dash-style)
              :check-dashes (setting :check-dashes)
              :check-phrases (setting :check-phrases)
              :check-address (setting :check-address)
              :address (or address (setting :address))
              :extra-phrases (setting :extra-phrases)
              :ignore-phrases (setting :ignore-phrases)
              :skip-quotes (setting :skip-quotes)
              :track-switches track-switches))

(defun report-language-for (report)
  "The language the line is written in: the setting's when it names one, the
answer's own when it is auto."
  (let ((chosen (setting :language)))
    (if (and chosen (member chosen '("en" "de") :test #'string=)) chosen (report-language report))))

;;; --- the advice on the stream -------------------------------------------------

(defun watch-delta (next on-part kind initargs)
  "One text fragment: counted, kept, and in fix mode replaced before any
consumer sees it. NEXT is the rest of the chain."
  (incf (getf *stats* :deltas))
  (let ((text (getf initargs :text)))
    (if (and text (plusp (length text)) (string= (setting :mode) "fix")
             (not *paused*) (setting :check-dashes))
        (multiple-value-bind (fixed count) (fix-dashes text :style (setting :dash-style))
          (accumulate text)
          (when (plusp count) (incf (getf *stats* :fixed) count))
          (apply next on-part kind (plist-with initargs :text fixed)))
        (progn (accumulate text)
               (apply next on-part kind initargs)))))

(defun close-answer (next on-part kind initargs)
  "The answer is whole: one report over it, and, when the settings say so,
one italic line under it before the stream finishes."
  (let ((source (answer-so-far)))
    (setf *source* nil)
    (when (and source (plusp (length source)) (not (quiet-p)))
      (let* ((fix (string= (setting :mode) "fix"))
             (report (check-with-settings source :fix fix))
             (line (footer-line report :language (report-language-for report))))
        (setf *last* report)
        (when (and (plusp (length line)) (setting :footer))
          (incf (getf *stats* :footers))
          (apply next on-part :text-delta (list :text (format nil "~%~%~a" line))))))
    (apply next on-part kind initargs)))

(defun watch-part (next on-part kind &rest initargs)
  "NLE::EMIT-STREAM-PART, advised: the answer's fragments are where the
plugin's transform_llm_output once stood. An answer a lane hands over whole
in one fragment is checked whole; a dash split across two fragments is two
fragments, and so is a paragraph break, for the same reason."
  (cond
    ((eq kind :stream-start)
     (setf *source* nil)
     (apply next on-part kind initargs))
    ((eq kind :text-delta) (watch-delta next on-part kind initargs))
    ((eq kind :finish) (close-answer next on-part kind initargs))
    (t (apply next on-part kind initargs))))

;;; --- the standing primer, where the plugin's skill once stood -------------------

(defparameter +primer+
  "writing-guard: before you deliver a text the user will send or publish (a mail, a post, a page, a document), run the writing_guard_check tool on the whole draft with fix true, take its fixed_text as the draft, rewrite every phrase it reports in plain words, and keep one form of address (du or Sie) throughout. Do not mention the check unless asked."
  "One paragraph the model reads every request, when primer is on: the skill
the original shipped, in the one place a Nodecode model looks.")

(defun standing-primer (next session-id)
  "Every request's harness sections, with this cell's paragraph first."
  (if (setting :primer)
      (acons "writing-guard" +primer+
             (remove "writing-guard" (funcall next session-id) :key #'car :test #'equal))
      (funcall next session-id)))

;;; --- the tool -----------------------------------------------------------------

(nlk:define-contract writing-guard-request
  (:doc "Check a draft here, on this machine, before the user sends or publishes it: em and en dashes, stock AI phrases in German and English, and mixed or wrong forms of address (du/Sie) in German. With fix true the answer carries the draft with every dash replaced by rule; the phrases and the form of address are only reported, so rewrite those parts yourself before delivering. Code blocks, inline code, URLs and quoted lines are skipped.")
  ("text" :text :required t :doc "the whole draft, as it would be sent")
  ("fix" :boolean :default nil :doc "answer the draft with every dash replaced; NIL reports them only")
  ("address" :string :default "auto" :options '("auto" "du" "sie")
             :doc "the German form of address the text should keep: auto reports only mixing"))

(defun run-check (request)
  "The writing_guard_check tool: the line the reader would see, then the
fixed draft when one was asked for."
  (let* ((fix (writing-guard-request-fix request))
         (report (check-with-settings (writing-guard-request-text request)
                                      :fix fix
                                      :address (let ((asked (writing-guard-request-address request)))
                                                (if (and asked (not (string= asked "auto")))
                                                    asked
                                                    (setting :address)))
                                      :track-switches nil))
         (line (footer-line report :language (report-language-for report)))
         (fixed (and fix (report-text report))))
    (with-output-to-string (out)
      (write-string (if (plusp (length line)) line "writing-guard: no findings.") out)
      (when fixed
        (format out "~%~%the draft with every dash replaced:~%~%~a" fixed)))))

;;; --- /writing-guard -----------------------------------------------------------

(defun status-text ()
  "What /writing-guard says bare."
  (format nil "writing-guard: ~:[on~;paused~], mode ~a, footer ~:[off~;on~], dashes ~a, address ~a, ~
               language ~a; ~d fragments read, ~a as the answers streamed, ~d lines appended~
               ~@[; last answer: ~a~]"
          (or *paused* (string= (setting :mode) "off"))
          (setting :mode)
          (setting :footer)
          (setting :dash-style)
          (setting :address)
          (setting :language)
          (stat :deltas) (plural (stat :fixed) "dash replaced" "dashes replaced") (stat :footers)
          (and *last* (string-trim "*" (footer-line *last* :language (report-language-for *last*))))))

(defun run-slash (args session-id)
  "The /writing-guard command: the state, on/off/reset, or a text checked
here and now. => the answer's text."
  (declare (ignore session-id))
  (let* ((typed (nlk:trimmed (or args "")))
         (word (first (nlk:split-words typed))))
    (cond
      ((null word) (status-text))
      ((string-equal word "on")
       (setf *paused* nil)
       "writing-guard: on - the next answers are checked.")
      ((string-equal word "off")
       (setf *paused* t)
       "writing-guard: paused - the answers come through unchanged until /writing-guard on or a restart.")
      ((string-equal word "reset")
       (stats-reset)
       "writing-guard: counters cleared.")
      (t
       (let* ((text (if (and (string-equal word "check") (> (length typed) 5))
                        (subseq typed 5)
                        typed))
              (report (check-with-settings text :track-switches nil))
              (line (footer-line report :language (report-language-for report))))
         (if (plusp (length line))
             line
             "writing-guard: no findings."))))))

;;; --- the manual ---------------------------------------------------------------

(defparameter +manual+
  "writing-guard: checks an answer for what makes it read as machine written - em and en dashes (and their rarer siblings), stock AI phrases in German and English (\"Es ist wichtig zu beachten\", \"delve\", \"seamless\"), and in German a paragraph that mixes du and Sie or a wrong form of address - as the answer streams, before any shell, channel or transcript reads it. Mode report changes nothing and appends one italic line under the answer when something was found; mode fix also replaces the dashes by rule (a range stays a hyphen, a bullet stays a bullet, a dash between words becomes a comma and a space, or a spaced hyphen with dash_style hyphen); mode off and /writing-guard off do nothing at all. footer off keeps the line out of the answer entirely. The phrase table is the original's, trimmed to its most frequent rules; extra_phrases adds the operator's own, ignore_phrases takes a rule id or an exact phrase out. Code fences, inline code, URLs, markdown link targets, MEDIA lines and quoted lines are masked out first, so nothing in them is counted or changed. The fix runs on each fragment as it streams, so a dash that opens a fragment reads as a list bullet and becomes a hyphen and a space; the answer is unchanged in every other way. The tool writing_guard_check checks a draft on demand and is what the standing primer sends the model to before it delivers a text; /writing-guard reads the state and the last answer, /writing-guard TEXT checks a text, and (help :writing-guard) is this."
  "What (help :writing-guard) answers while the cell runs.")

;;; --- the cell -----------------------------------------------------------------

(nle:define-cell writing-guard
  (:section ("writing-guard")
    (:guide "nothing here is required: the defaults check every answer and change nothing but one italic line under it; mode fix also replaces the dashes; /writing-guard reads the state or checks a text")
    ("mode" :choice :options '("report" "fix" "off") :default "report"
            :doc "report = one line under the answer, the text unchanged; fix = replace the dashes too; off = nothing")
    ("footer" :boolean :default t
              :doc "show the line under the answer; off leaves the answer exactly as the model said it")
    ("language" :choice :options '("auto" "en" "de") :default "auto"
                :doc "the line's language: auto follows the answer")
    ("dash_style" :choice :options '("comma" "hyphen") :default "comma"
                  :doc "what a dash between two words becomes: a comma and a space, or a spaced hyphen")
    ("address" :choice :options '("auto" "du" "sie") :default "auto"
               :doc "the German form a text should keep: auto reports only a paragraph that mixes both")
    ("check_dashes" :boolean :default t :doc "count and replace dashes")
    ("check_phrases" :boolean :default t :doc "report stock AI phrases")
    ("check_address" :boolean :default t :doc "report du/Sie in German text")
    ("skip_quotes" :boolean :default t :doc "leave lines that start with > alone")
    ("primer" :boolean :default t
              :doc "carry one paragraph in every prompt telling the model to check a draft before delivering it")
    ("extra_phrases" :list :doc "the operator's own stock phrases, plain text, case insensitive")
    ("ignore_phrases" :list :doc "rule ids (en.seamless) or exact phrases to leave out"))
  (:settings (lambda (values table) (declare (ignore table)) values))
  (:hook 'nle::emit-stream-part #'watch-part)
  (:hook 'nle::read-harness-sections #'standing-primer)
  (:tool "writing_guard_check" (nlk:contract 'writing-guard-request) 'run-check)
  (:command "writing-guard" 'run-slash
            :description "What the checking does, or a text checked now"
            :argument-hint "[TEXT | on | off | reset]")
  (:help :writing-guard "writing-guard: dashes, AI phrases and du/Sie, reported under the answer - /writing-guard checks a text"
         +manual+)
  (:start (lambda ()
            (stats-reset)
            (setf *paused* nil *last* nil *source* nil)
            (nle:on-stop (lambda ()
                           (setf *source* nil *last* nil *paused* nil))))))
