;;;; cell.lisp --- the sanitizer, the hook on the stream, /cjk, the tool.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of evanyudis/cjk_sanitizer, a Hermes Agent plugin
;;;; that hooked the harness's transform_llm_output and stripped the Chinese
;;;; (CJK ideograph) and Cyrillic characters a Chinese-trained model leaks
;;;; mid-sentence. The rewrite keeps the one thing it gives a user - the
;;;; characters gone from what the model says, Korean Hangul and Japanese
;;;; kana kept - and finds it a better seam: the original could only rewrite
;;;; an answer whole and only where one harness had one; here the advice sits
;;;; on NLE::EMIT-STREAM-PART, the funnel every lane's stream part goes
;;;; through, so what is stripped never reaches a shell, a channel, a
;;;; transcript or the history the next round reads back.
;;;;
;;;; The rules are ranges of the code points, read off settings at the call
;;;; and not closed over, so /cjk off and a restart-cells both take effect on
;;;; the turn already streaming. The pure half, SANITIZE-TEXT, is where the
;;;; behaviour is decided and what the tests read; the hook is four lines
;;;; around it.

(defpackage #:nodecode-cjk-sanitizer
  (:use #:cl)
  (:export #:sanitize-text #:strip-char-p #:start-cell #:stop-cell
           #:run-slash #:run-strip))

(in-package #:nodecode-cjk-sanitizer)

(nlk:define-peripheral cjk-sanitizer :not-running t)

;;; --- what is a leak, and what is a language ----------------------------------

(defparameter *chinese-ranges*
  '((#x2E80 . #x2EFF)                       ; CJK radicals supplement
    (#x3400 . #x4DBF)                       ; CJK extension A
    (#x4E00 . #x9FFF)                       ; CJK unified ideographs
    (#xF900 . #xFAFF))                      ; CJK compatibility ideographs
  "The code point ranges the original named as a Chinese leak. Its table is
the authority: Japanese kana (U+3040-U+30FF) and Korean Hangul
(U+AC00-U+D7AF) are outside every range and stay, which is why a bilingual
answer in those scripts survives and a hanzi run does not.")

(defparameter *cyrillic-ranges*
  '((#x0400 . #x04FF))                      ; Cyrillic, the whole block
  "The Cyrillic block, stripped whole as the original stripped it.")

(defparameter *punctuation-ranges*
  '((#x3000 . #x303F))                      ; CJK symbols and punctuation
  "The CJK punctuation a stripped run leaves behind (、。 「」), stripped with
it so an answer does not end in orphaned marks. An addition to the original,
which left them; strip_punctuation turns it off.")

;;; --- the pure half ------------------------------------------------------------

(defun strip-char-p (character ranges)
  "Whether CHARACTER sits in RANGES, a list of (LOW . HIGH) inclusive pairs."
  (let ((code (char-code character)))
    (loop for (low . high) in ranges
          thereis (<= low code high))))

(defun collapse-whitespace (text blank-lines)
  "TEXT with every run of spaces brought to one, and every run of newlines
cut to BLANK-LINES of them - the space an emptied run leaves behind, as the
original collapsed it."
  (with-output-to-string (out)
    (loop with length = (length text)
          with index = 0
          while (< index length)
          for character = (char text index)
          do (cond
               ((char= character #\Space)
                (let ((end index))
                  (loop while (and (< end length) (char= (char text end) #\Space))
                        do (incf end))
                  (write-char #\Space out)
                  (setf index end)))
               ((char= character #\Newline)
                (let ((end index))
                  (loop while (and (< end length) (char= (char text end) #\Newline))
                        do (incf end))
                  (loop repeat (min (- end index) blank-lines)
                        do (write-char #\Newline out))
                  (setf index end)))
               (t (write-char character out)
                  (incf index))))))

(defun sanitize-text (text &key (chinese t) (cyrillic t) (punctuation t)
                           (collapse t) (blank-lines 2))
  "TEXT with the leaked Chinese (CHINESE) and Cyrillic (CYRILLIC) characters
taken out, CJK punctuation too when PUNCTUATION, and the gaps they leave
tidied when COLLAPSE (runs of spaces to one, runs of newlines to BLANK-LINES).
=> (values CLEAN STRIPPED), STRIPPED the characters taken out - the count, not
the text, so a caller can say what it did without quoting what it removed."
  (let* ((ranges (append (and chinese *chinese-ranges*)
                          (and punctuation *punctuation-ranges*)
                          (and cyrillic *cyrillic-ranges*)))
         (stripped 0)
         (clean (with-output-to-string (out)
                  (loop for character across text
                        do (if (and ranges (strip-char-p character ranges))
                               (incf stripped)
                               (write-char character out))))))
    (when collapse
      (setf clean (collapse-whitespace clean blank-lines)))
    (values clean stripped)))

;;; --- what this process has done ------------------------------------------------

(defvar *stats* (list :deltas 0 :stripped 0 :dropped 0)
  "Deltas read, characters taken out and deltas dropped whole since this
process started: what /cjk reports, reset by /cjk reset.")

(defvar *paused* nil
  "Whether /cjk off has stopped the stripping until the cell restarts: the
hook reads it live, so a running turn is quiet at its next delta.")

(defun stats-reset ()
  "Forget what /cjk counts."
  (setf *stats* (list :deltas 0 :stripped 0 :dropped 0)))

(defun stats-get (key) (getf *stats* key))

(defun note-delta (&key stripped)
  "Count one streamed delta, and what it cost."
  (incf (getf *stats* :deltas))
  (when stripped (incf (getf *stats* :stripped) stripped)))

;;; --- the hook on the stream -----------------------------------------------------

(defun plist-with (plist key value)
  "PLIST with KEY's value VALUE, in place: the initargs a stream part is made
from, one member replaced."
  (loop for (member value-here) on plist by #'cddr
        if (eq member key)
          append (list member value)
        else
          append (list member value-here)))

(defun sanitize-part (next on-part kind &rest initargs)
  "NLE::EMIT-STREAM-PART, advised: the answer's text (and, when
strip_reasoning, the thinking stream's) is cleaned as each fragment leaves a
lane, before EVOLVED sees the part at all. A fragment the sanitizer empties
entirely is not emitted, so a page of leaked characters leaves no empty span
behind. NEXT is the rest of the chain; calling it is the pass-through."
  (let ((text (and (member kind '(:text-delta :reasoning-delta)) (getf initargs :text))))
    (if (and text (not *paused*)
             (or (eq kind :text-delta) (setting :strip-reasoning)))
        (multiple-value-bind (clean stripped)
            (sanitize-text text
                           :chinese (setting :strip-chinese)
                           :cyrillic (setting :strip-cyrillic)
                           :punctuation (setting :strip-punctuation)
                           :collapse (setting :collapse-space)
                           :blank-lines (setting :blank-lines))
          (cond
            ((and (plusp stripped) (zerop (length clean)))
             (note-delta)
             (incf (getf *stats* :dropped))
             nil)
            ((plusp stripped)
             (note-delta :stripped stripped)
             (apply next on-part kind (plist-with initargs :text clean)))
            (t
             (note-delta)
             (apply next on-part kind initargs))))
        (apply next on-part kind initargs))))

;;; --- the tool: clean text the operator or the model hands over --------------------

(nlk:define-contract strip-arguments
  (:doc "Take the stray Chinese (CJK ideograph) and Cyrillic characters out of text the answer should have been in plain English: the scripts a Chinese-trained model leaks mid-sentence. Korean Hangul and Japanese kana are kept. Answers the cleaned text, then one line saying how much went.")
  ("text" :text :required t :doc "the text to clean, whole")
  ("scripts" :string :default "chinese,cyrillic,punctuation"
             :doc "comma-separated scripts to take out: chinese, cyrillic, punctuation"))

(defparameter +scripts+ '("chinese" "cyrillic" "punctuation" "spaces")
  "What the tool's scripts member accepts. spaces adds the whitespace
tidying on its own.")

(defun scripts-of (text)
  "TEXT's comma-separated words, downcased; NIL when one is not a script."
  (let ((words (loop for word in (uiop:split-string (or text "") :separator ",")
                     for clean = (string-downcase (nlk:trimmed word))
                     when (plusp (length clean)) collect clean)))
    (unless (every (lambda (word) (member word +scripts+ :test #'string=)) words)
      (return-from scripts-of :unknown))
    (if words words '("none"))))

(defun run-strip (arguments)
  "The cjk_strip tool: ARGUMENTS' text cleaned as its scripts say."
  (let ((scripts (scripts-of (strip-arguments-scripts arguments))))
    (when (eq scripts :unknown)
      (return-from run-strip
        (format nil "cjk_sanitizer: scripts takes any of ~{~a~^, ~}, comma-separated, not ~s."
                +scripts+ (strip-arguments-scripts arguments))))
    (multiple-value-bind (clean stripped)
        (sanitize-text (strip-arguments-text arguments)
                       :chinese (and (member "chinese" scripts :test #'string=) t)
                       :cyrillic (and (member "cyrillic" scripts :test #'string=) t)
                       :punctuation (and (member "punctuation" scripts :test #'string=) t)
                       :collapse t)
      (if (plusp stripped)
          (format nil "~a~%~%cjk_sanitizer: took out ~d character~:p." clean stripped)
          (format nil "~a~%cjk_sanitizer: nothing to take out." clean)))))

;;; --- /cjk ---------------------------------------------------------------------

(defun report-text ()
  "One line per setting, then the counters: what /cjk says bare."
  (format nil "cjk: ~:[paused~;on~] ~
               (chinese ~:[off~;on~], cyrillic ~:[off~;on~], punctuation ~:[off~;on~], ~
               spaces ~:[off~;on~], reasoning ~:[off~;on~]) ~
               · ~d deltas read, ~d characters taken out, ~d dropped"
          (not *paused*)
          (setting :strip-chinese) (setting :strip-cyrillic)
          (setting :strip-punctuation) (setting :collapse-space)
          (setting :strip-reasoning)
          (stats-get :deltas) (stats-get :stripped) (stats-get :dropped)))

(defun run-slash (args session-id)
  "The /cjk command: bare, what it strips and what it has stripped; on, off,
reset. => the answer's text."
  (declare (ignore session-id))
  (running-settings)
  (let ((word (first (nlk:split-words args))))
    (cond
      ((null word) (report-text))
      ((string-equal word "on")
       (setf *paused* nil)
       "cjk: on - the answers are stripped from this delta on.")
      ((string-equal word "off")
       (setf *paused* t)
       "cjk: paused - the answers come through whole until /cjk on or a restart.")
      ((string-equal word "reset")
       (stats-reset)
       "cjk: counters cleared.")
      (t (format nil "cjk: ~s is not one of its words - on, off, reset, or nothing for the report."
                 word)))))

;;; --- the manual ------------------------------------------------------------------

(defparameter +manual+
  "cjk: strips the Chinese (CJK ideograph) and Cyrillic characters a Chinese-trained model leaks into its answer, as each fragment streams, before any shell, channel, transcript or the history's next round reads it; Korean Hangul and Japanese kana are kept, and so is every other script. The section cjk-sanitizer in the config holds it: strip_chinese, strip_cyrillic, strip_punctuation (CJK punctuation U+3000-U+303F, left behind by a stripped run), collapse_space and blank_lines (the tidying of what a stripped run left), strip_reasoning (the thinking stream too). /cjk reads the settings and what this process has stripped; /cjk off pauses it until restart, /cjk on resumes, /cjk reset clears the counters. The tool cjk_strip cleans text handed to it. LANES: the advice sits on the stream, so an answer a provider hands over whole in one delta is cleaned whole too, but a leak split across two fragments arrives as two fragments; collapsing a run of spaces is per fragment for the same reason. Leaks in a recalled tool result or a file's own text are not touched - only what the model says."
  "What (help :cjk) answers while the cell runs.")

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell cjk-sanitizer
  (:section ("cjk-sanitizer")
    (:guide "nothing here is required: the defaults strip Chinese, Cyrillic and the punctuation between, and keep a run of spaces to one; /cjk reports what it has done")
    ("strip_chinese" :boolean :default t
                     :doc "take out CJK ideographs, radicals and compatibility forms (Korean and Japanese kana stay)")
    ("strip_cyrillic" :boolean :default t
                      :doc "take out the Cyrillic block whole, as the original did")
    ("strip_punctuation" :boolean :default t
                         :doc "take out the CJK punctuation a stripped run leaves behind (U+3000-U+303F)")
    ("collapse_space" :boolean :default t
                      :doc "bring runs of spaces to one and runs of blank lines to blank_lines")
    ("blank_lines" :integer :default 2 :min 1
                   :doc "the most consecutive newlines kept - 2 keeps one blank line")
    ("strip_reasoning" :boolean :default nil
                       :doc "the thinking stream too; off by default, since a model thinking in Chinese may mean it"))
  (:settings (lambda (values table)
                (declare (ignore table))
                values))
  (:tool "cjk_strip" (nlk:contract 'strip-arguments) 'run-strip)
  (:command "cjk" 'run-slash
            :description "What the sanitizer strips, and what it has stripped"
            :argument-hint "on | off | reset")
  (:help :cjk "cjk: strip stray Chinese and Cyrillic from the model's answers as they stream - /cjk reports"
         +manual+)
  (:hook 'nle::emit-stream-part #'sanitize-part)
  (:start (lambda ()
            (stats-reset)
            (setf *paused* nil))))
