;;;; cell.lisp --- the line under the answer, the tool, /ja-writing-guard, the section.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of TakeshiTGAL/hermes-plugin-ja-writing-guard, a
;;;; Hermes plugin that hooked transform_llm_output: it scored each Japanese
;;;; answer for AI-style phrasing and, by default, only reported it. This
;;;; rewrite keeps the rules, the surfaces and the score, and puts the report
;;;; on the seam an answer actually crosses (NLE::EMIT-STREAM-PART), so one
;;;; italic line rides the answer itself in every shell, channel and
;;;; transcript, plus the tool and the command the plugin also shipped.
;;;;
;;;; The opt-in enforce mode (a model call that rewrites an over-threshold
;;;; answer before delivery) is not reproduced: the tool hands the score and
;;;; the hints back to the model that can already rewrite.

(in-package #:nodecode-ja-writing-guard)

(nlk:define-peripheral ja-writing-guard :not-running t)

;;; --- what this process has done, and the answer being written ------------------

(defvar *stats* (list :answers 0 :flagged 0 :lines 0)
  "Answers checked, answers over the threshold, and lines appended, since this
process started: what /ja-writing-guard reports, and reset clears.")

(defvar *paused* nil
  "Whether /ja-writing-guard off stopped the checking until the next on, or a restart.")

(defvar *source* nil
  "The answer being written, as a string output stream, or NIL between answers.")

(defvar *last* nil
  "The last report this process made, for /ja-writing-guard.")

(defun stats-reset () (setf *stats* (list :answers 0 :flagged 0 :lines 0)))
(defun stat (key) (getf *stats* key 0))

(defun quiet-p ()
  (or *paused* (string= (setting :mode) "off")))

;;; --- the check, as the section reads it ----------------------------------------

(defun check-with-settings (text &key surface)
  "TEXT checked under the section's surface and threshold."
  (check-text text
              :surface (or surface (setting :surface))
              :threshold (let ((asked (setting :threshold)))
                           (and (integerp asked) (plusp asked) asked))))

(defun footer-line (report)
  "One italic line naming what read as machine written, or the empty string."
  (let ((findings (report-findings report)))
    (if (or (not (report-japanese report)) (null findings))
        ""
        (let ((cats (report-sorted-categories report)))
          (format nil "*ja-writing-guard: AI的な言い回し ~d/100（~a の基準 ~d）。指摘 ~d 件（~{~a~^・~}）*"
                  (report-score report) (report-surface report) (report-threshold report)
                  (length findings)
                  (loop for c in (subseq cats 0 (min 2 (length cats)))
                        collect (or (cdr (assoc c +categories+ :test #'string=)) c)))))))

;;; --- the line under the answer -------------------------------------------------

(defun answer-so-far ()
  (when *source* (get-output-stream-string *source*)))

(defun close-answer (next on-part kind initargs)
  "The answer is whole: one report over it and, when the section says so, one
italic line under it before the stream finishes."
  (let ((source (answer-so-far)))
    (setf *source* nil)
    (when (and source (>= (length source) (setting :min-chars)) (not (quiet-p))
               (japanese-p source))
      (let* ((report (check-with-settings source))
             (line (footer-line report)))
        (incf (getf *stats* :answers))
        (when (report-over-threshold report) (incf (getf *stats* :flagged)))
        (setf *last* report)
        (when (and (plusp (length line)) (setting :footer))
          (incf (getf *stats* :lines))
          (apply next on-part :text-delta (list :text (format nil "~%~%~a" line))))))
    (apply next on-part kind initargs)))

(defun watch-part (next on-part kind &rest initargs)
  "NLE::EMIT-STREAM-PART, advised: the answer's fragments are where the
plugin's transform_llm_output once stood."
  (case kind
    (:stream-start
     (setf *source* (make-string-output-stream))
     (apply next on-part kind initargs))
    (:text-delta
     (let ((text (getf initargs :text)))
       (when (and text (plusp (length text)))
         (unless *source* (setf *source* (make-string-output-stream)))
         (write-string text *source*)))
     (apply next on-part kind initargs))
    (:finish (close-answer next on-part kind initargs))
    (t (apply next on-part kind initargs))))

;;; --- the tool -----------------------------------------------------------------

(nlk:define-contract ja-writing-check
  (:doc "Proofread Japanese text for AI-style phrasing that is common in Japanese business writing: stock phrases like 〜と言えるでしょう, inflated words, English-calque grammar such as 〜することができます, em dashes and Markdown in emails, monotonous sentence endings, and register that does not fit where the text goes. It checks wording, not authorship. Deterministic and offline: a 0-100 score (higher = more of these patterns), the surface's threshold, and every finding with line, column, excerpt and a fix hint in Japanese. Use it before sending a Japanese mail, notice, post or document, and again after rewriting to confirm the score went down.")
  ("text" :text :required t :doc "the Japanese text to check")
  ("surface" :string :default "chat" :options '("chat" "business_email" "external" "sns" "tech_doc")
             :doc "where the text goes: business_email and external require です・ます and are strictest; chat is lenient; sns allows emoji; tech_doc allows headings, lists and code")
  ("max_findings" :integer :default 30
                  :doc "cap on the findings returned, 1 to 200 (default 30)"))

(defun run-check (request)
  "The ja_writing_check tool: the score, the threshold and the findings."
  (let* ((surface (ja-writing-check-surface request))
         (report (handler-case (check-text (ja-writing-check-text request)
                                           :surface (if (and surface (plusp (length surface)))
                                                        surface "chat"))
                   (error (cause) (return-from run-check (format nil "ja-writing-guard: ~a" cause))))))
    (setf *last* report)
    (format-report report :limit (ja-writing-check-max-findings request))))

;;; --- /ja-writing-guard ---------------------------------------------------------

(defparameter +usage+
  (format nil "使い方: /ja-writing-guard [場面] <文>  場面は ~{~a~^ / ~}。文を省くと、直前の返答の検査結果を表示します。~%Usage: /ja-writing-guard [surface] <text>; omit the text for the last answer's report."
          (loop for (name . label) in +surface-labels+ collect (format nil "~a（~a）" name label))))

(defun status-text ()
  (format nil "ja-writing-guard: ~:[on~;paused~], mode ~a, surface ~a, threshold ~:[the surface's own~;~:*~d~], footer ~:[off~;on~]; ~d answers checked, ~d over the threshold, ~d lines appended~@[; last answer: ~a~]"
          (or *paused* (string= (setting :mode) "off"))
          (setting :mode) (setting :surface) (setting :threshold)
          (setting :footer) (stat :answers) (stat :flagged) (stat :lines)
          (and *last* (format-report *last* :limit 3))))

(defun run-slash (args session-id)
  "The /ja-writing-guard command: the state, or a text checked here and now.
=> the answer's text."
  (declare (ignore session-id))
  (let* ((typed (string-trim '(#\Space #\Tab) (or args "")))
         (word (first (ppcre:split "[ \\t]+" typed))))
    (cond
      ((zerop (length typed)) (status-text))
      ((member word '("help" "-h" "--help") :test #'string-equal) +usage+)
      ((string-equal word "on") (setf *paused* nil)
       "ja-writing-guard: on - the next answers are checked.")
      ((string-equal word "off") (setf *paused* t)
       "ja-writing-guard: paused - the answers come through unchanged until /ja-writing-guard on or a restart.")
      ((string-equal word "reset") (stats-reset) "ja-writing-guard: counters cleared.")
      (t
       (let* ((surface (and (member word (surface-names) :test #'string-equal)
                             (string-downcase word)))
              (text (if surface
                        (string-trim '(#\Space #\Tab) (subseq typed (length word)))
                        typed))
              (report (handler-case (check-with-settings text :surface surface)
                        (error (cause) (return-from run-slash (format nil "ja-writing-guard: ~a" cause))))))
         (setf *last* report)
         (format-report report))))))

;;; --- the manual ---------------------------------------------------------------

(defparameter +manual+
  "ja-writing-guard: scores Japanese text for the phrasing that reads as unedited AI output - stock closing phrases (〜と言えるでしょう, いかがでしたか), inflated words (シームレス, 重要な役割を果たす), English-calque grammar (〜することができます, 〜を行う), template shapes (3つのポイント, お役に立てれば幸いです), em dashes and Markdown where prose belongs, monotonous sentence endings, and 常体 where です・ます is needed - on this machine, with line, column and a fix hint for each. It checks wording, not authorship. The surface says where the text goes: chat, business_email, external, sns or tech_doc; a surface scales each rule's weight, sets the 0-100 score at which the text counts as AI-like, and business_email and external also report plain-form endings. Code fences, inline code, URLs, quoted lines, HTML and text inside 「」 are skipped. In mode report the cell appends one italic line under each Japanese answer over the minimum length; /ja-writing-guard off pauses that; the ja_writing_check tool checks a draft on demand, and /ja-writing-guard TEXT does the same from the keyboard. (help :ja-writing-guard) is this."
  "What (help :ja-writing-guard) answers while the cell runs.")

;;; --- the cell -----------------------------------------------------------------

(nle:define-cell ja-writing-guard
  (:section ("ja-writing-guard")
    (:guide "nothing here is required: every Japanese answer is scored and one italic line says what read as machine written; mode off stops it; /ja-writing-guard checks a text now")
    ("mode" :choice :options '("report" "off") :default "report"
            :doc "report = one line under each Japanese answer, the text unchanged; off = nothing")
    ("surface" :choice :options '("chat" "business_email" "external" "sns" "tech_doc") :default "chat"
              :doc "where the answers go: business_email and external require です・ます and are strictest")
    ("threshold" :integer :default 0 :min 0
                :doc "the score (0-100) at which a text counts as AI-like; 0 = the surface's own (chat 40, business_email 35, external 40, sns 35, tech_doc 45)")
    ("min_chars" :integer :default 40 :min 0
                 :doc "answers shorter than this are not checked under the stream")
    ("footer" :boolean :default t
              :doc "show the line under the answer; off leaves the answer exactly as the model said it")
    ("max_findings" :integer :default 30 :min 1
                    :doc "cap on the findings the tool returns"))
  (:hook 'nle::emit-stream-part #'watch-part)
  (:tool "ja_writing_check" (nlk:contract 'ja-writing-check) 'run-check)
  (:command "ja-writing-guard" 'run-slash
            :description "Score Japanese text for AI-style phrasing, or the last answer's report"
            :argument-hint "[SURFACE] TEXT | help | on | off | reset")
  (:help :ja-writing-guard
         "ja-writing-guard: Japanese AI-style phrasing scored with line, column and a fix hint"
         +manual+)
  (:start (lambda ()
            (stats-reset)
            (setf *paused* nil *last* nil *source* nil)
            (nle:on-stop (lambda () (setf *source* nil *last* nil *paused* nil))))))
