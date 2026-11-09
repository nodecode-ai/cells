;;;; cell.lisp --- one prompt, rewritten for the model that is going to read it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of Sahil-SS9's hermes-multichannel-prompt-optimizer: the same
;;;; idea -- a prompt is expensive before the model reads it, so say less and
;;;; say it better, tailored to the model family -- with the rewrite asked for
;;;; rather than done behind the operator's back. Three surfaces:
;;;;
;;;;   /optimize PROMPT   the operator's own rewrite, with scores
;;;;   optimize_prompt     the same for the model, through a contract
;;;;   /prompt-stats       what the recorded rewrites did
;;;;
;;;; Every rewrite is one side call (NLE:COMPLETE) on the auxiliary model,
;;;; and what it did is one JSON line under the home; nothing leaves this
;;;; machine but the prompt itself, to the provider the operator already runs.

(defpackage #:nodecode-prompt-optimizer
  (:use #:cl)
  (:export #:rewrite-prompt #:optimize-prompt #:score-prompt #:history
           #:prompt-optimizer-error #:*manual* #:*filler*))

(in-package #:nodecode-prompt-optimizer)

(nlk:define-peripheral prompt-optimizer :not-running t)

;;;; --- what a prompt is worth -------------------------------------------------

(defparameter *filler*
  '("please" "kindly" "maybe" "just" "really" "basically" "actually" "simply" "very"
    "quite" "somewhat" "literally" "thanks" "hey" "um" "well" "obviously")
  "Words a prompt does not need. The clarity score counts them.")

(defparameter +verbs+
  '("explain" "write" "list" "summarize" "summarise" "fix" "add" "remove" "delete"
    "refactor" "build" "compare" "review" "translate" "rewrite" "generate" "find" "check"
    "run" "make" "create" "show" "describe" "convert" "test" "debug" "implement"
    "document" "plan" "design" "analyse" "analyze" "port" "return" "give" "tell" "name"
    "quote" "use" "read" "open" "update" "install" "configure" "draft" "outline"
    "estimate" "compute" "trace" "migrate")
  "The first word of a prompt that names its own task, which is what an actionability score rewards.")

(defparameter +edge+ ".,;:!?\"'()`*_-" "Punctuation stripped off a word before it is read.")

(defun words-of (text)
  "TEXT's words, in order, with the punctuation at their edges stripped."
  (let ((words '()) (start nil) (text (or text "")))
    (flet ((finish (end)
             (when start
               (let ((word (string-trim +edge+ (subseq text start end))))
                 (when (plusp (length word)) (push word words)))
               (setf start nil))))
      (loop for index from 0 below (length text)
            for char = (char text index)
            do (if (member char '(#\Space #\Tab #\Newline #\Return))
                   (finish index)
                   (unless start (setf start index)))
            finally (finish (length text))))
    (nreverse words)))

(defun words-in-tokens (text)
  "Roughly how many tokens TEXT costs: four characters to one."
  (ceiling (count-if (lambda (char) (not (member char '(#\Space #\Tab #\Newline #\Return))))
                      (or text ""))
           4))

(defun marks-of (text)
  "How many of TEXT's words name something exact: a number, a path, a code span."
  (min 4 (+ (if (find-if #'digit-char-p text) 1 0)
            (if (find #\` text) 1 0)
            (if (find #\" text) 1 0)
            (if (find #\= text) 1 0)
            (count-if (lambda (word)
                        (some (lambda (char) (member char '(#\. #\/ #\_ #\:))) word))
                      (words-of text)))))

(defun score-prompt (text)
  "=> (values TOTAL CLARITY SPECIFICITY STRUCTURE ACTIONABILITY); each 0-25,
TOTAL 0-100. The five dimensions the original scored: how much of the prompt
is noise, how much of it names something exact, whether it is laid out as
more than one thought, and whether it says what to do."
  (let* ((words (words-of text))
         (lowered (mapcar #'string-downcase words))
         (count (length words))
         (clarity (max 0 (- 25 (* 3 (count-if (lambda (word) (member word *filler* :test #'string=))
                                              lowered)))))
         (specificity (min 25 (+ 6 (* 3 (marks-of (or text ""))) (if (>= count 6) 3 0))))
         (structure (min 25 (+ 8
                              (if (find #\Newline (or text "")) 6 0)
                              (if (find #\: (or text "")) 5 0)
                              (if (find #\- (or text "")) 6 0))))
         (actionability (min 25 (+ (if (and lowered (member (first lowered) +verbs+ :test #'string=))
                                        20 5)
                                   (if (find #\? (or text "")) 5 0)))))
    (values (+ clarity specificity structure actionability)
            clarity specificity structure actionability)))

;;;; --- the model the prompt is going to --------------------------------------

(defparameter *family-markers*
  '(("claude" "claude" "anthropic/" "sonnet" "opus" "haiku")
    ("openai" "gpt-" "openai/" "o1" "o3" "o4" "codex")
    ("google" "gemini" "google/" "vertex")
    ("deepseek" "deepseek")
    ("qwen" "qwen" "qwq"))
  "What a model name reads as: the vendor families this cell knows.")

(defparameter *family-tactics*
  '(("claude" "Mark the parts with tags: <task>, <constraints>, <answer>"
              "State the boundaries of the answer explicitly")
    ("openai" "Put the instruction first, in one sentence"
              "Name the shape and length of the answer the prompt wants")
    ("google" "Put the task before its context"
              "Ask for the answer directly, without preamble")
    ("deepseek" "Separate the request from its constraints with a blank line"
                "Name any computation the answer has to show")
    ("qwen" "State the role in one clause, then the task"))
  "What each family's own prompting guide says, kept short.")

(defun family-of (target)
  "The family TARGET's model name reads as, or NIL for one this cell does not know."
  (let ((name (string-downcase (or target ""))))
    (when (plusp (length name))
      (loop for (family . markers) in *family-markers*
            when (some (lambda (marker) (search marker name)) markers)
              return family))))

(defun resolved-family (target)
  "The family to write for: the section's choice, or the one TARGET names in auto."
  (let ((chosen (string-downcase (string (setting :family)))))
    (if (equal chosen "auto") (family-of target) chosen)))

(defparameter +instruction+
  "You rewrite one prompt so the model that receives it reads it better: shorter, clearer, the
same meaning. Answer with the rewritten prompt alone -- no preamble, no quote marks, no
commentary, no answer to what it asks.
- Keep the language the prompt is written in; never translate it.
- Keep every fact, name, path, number, code span and constraint; drop filler, hedging and repetition.
- Open with one imperative verb that names the task.
- One instruction a sentence; a list when the prompt asks for several things.
- Add no requirement the prompt did not state."
  "The instruction every rewrite runs under, before the family's tactics.")

(defun instruction (target)
  "The whole instruction for TARGET: the rewriting rule with its family's tactics under it."
  (let ((family (resolved-family target))
        (tactics (cdr (assoc (resolved-family target) *family-tactics* :test #'string=))))
    (with-output-to-string (out)
      (write-string +instruction+ out)
      (when tactics
        (format out "~%The model reading it is a ~a model; write for that family:~%~{  - ~a~%~}"
                family tactics)))))

;;;; --- the rewrite -----------------------------------------------------------

(defun clean-answer (text)
  "TEXT without the wrapper a model tends to add: a fence around the whole answer, or quotes."
  (let ((text (nlk:trimmed (or text ""))))
    (when (and (> (length text) 6)
               (uiop:string-prefix-p "```" text)
               (string= "```" text :start2 (- (length text) 3)))
      (let ((body (subseq text 3 (- (length text) 3))))
        (let ((newline (position #\Newline body)))
          (setf text (nlk:trimmed (if newline (subseq body (1+ newline)) body))))))
    (when (and (> (length text) 1)
               (member (char text 0) '(#\" #\'))
               (char= (char text 0) (char text (1- (length text)))))
      (setf text (nlk:trimmed (subseq text 1 (1- (length text))))))
    text))

(defun rewritten (draft target)
  "DRAFT as the model rewrote it, or a refusal saying why it stands as it is."
  (let ((answer (handler-case (nle:complete (instruction target) draft
                                            :max-tokens (setting :max-tokens))
                  (error (condition)
                    (fail "the model call failed: ~a" (nlk:one-line (princ-to-string condition)))))))
    (let ((text (clean-answer answer)))
      (when (zerop (length text))
        (fail "the model answered nothing; the prompt stands as it was"))
      text)))

(defun history-file ()
  "The one file the recorded rewrites are lines of."
  (nlk:home "prompt-optimizer/rewrites.jsonl"))

(defun record (draft final family)
  "Write one line about the rewrite, when the section says to keep a history."
  (when (setting :record)
    (nlk:append-json-line
     (history-file)
     (nlk:make-json-object
      "at" (get-universal-time)
      "family" (or family "unknown")
      "words_before" (length (words-of draft))
      "words_after" (length (words-of final))
      "score_before" (score-prompt draft)
      "score_after" (score-prompt final))))
  final)

(defun rewrite-report (draft final family)
  "The operator's page: what the rewrite changed, and the prompt itself."
  (let ((before (multiple-value-list (score-prompt draft)))
        (after (multiple-value-list (score-prompt final))))
    (format nil "prompt~@[ (~a family)~]: ~d -> ~d points (~@d)~%  clarity ~d->~d  specificity ~d->~d  structure ~d->~d  actionability ~d->~d~%  ~d -> ~d words, about ~d -> ~d tokens~%~%~a"
            family
            (first before) (first after) (- (first after) (first before))
            (second before) (second after)
            (third before) (third after)
            (fourth before) (fourth after)
            (fifth before) (fifth after)
            (length (words-of draft)) (length (words-of final))
            (words-in-tokens draft) (words-in-tokens final)
            final)))

(define-verb rewrite-prompt (draft &key target)
  "DRAFT rewritten, with what the rewrite changed before it. TARGET is the model
or family the prompt is going to, when it is known."
  (let ((draft (nlk:trimmed (or draft ""))))
    (when (zerop (length draft))
      (fail "type the prompt to rewrite: (rewrite-prompt \"...\") or /optimize PROMPT"))
    (when (< (length (words-of draft)) (setting :min-words))
      (return-from rewrite-prompt
        (format nil "~d word~:p already: it stands as it is~%~%~a"
                (length (words-of draft)) draft)))
    (let* ((family (resolved-family target))
           (final (rewritten draft target)))
      (record draft final family)
      (rewrite-report draft final family))))

;;;; --- what the recorded rewrites did -----------------------------------------

(defun history-rows ()
  "Every recorded rewrite, oldest first."
  (nlk:read-json-lines (history-file)))

(defun number-in (row key)
  (let ((value (gethash key row))) (if (numberp value) value 0)))

(defun mixed (rows key)
  (if (null rows) 0 (round (loop for row in rows sum (number-in row key)) (length rows))))

(defun history-line (row)
  (format nil "  ~a: ~d -> ~d points, ~d -> ~d words"
          (or (gethash "family" row) "unknown")
          (number-in row "score_before") (number-in row "score_after")
          (number-in row "words_before") (number-in row "words_after")))

(define-verb history ()
  "What the recorded prompt rewrites did: how many, their average before and
after, and the last few."
  (let ((rows (history-rows)))
    (if (null rows)
        "no rewrite recorded yet: run /optimize PROMPT first"
        (format nil "~d rewrite~:p recorded~%  average score ~d -> ~d (~@d)~%  average words ~d -> ~d (~@d)~%~{~a~%~}"
                (length rows)
                (mixed rows "score_before") (mixed rows "score_after")
                (- (mixed rows "score_after") (mixed rows "score_before"))
                (mixed rows "words_before") (mixed rows "words_after")
                (- (mixed rows "words_after") (mixed rows "words_before"))
                (mapcar #'history-line
                        (last rows (min 3 (length rows))))))))

;;;; --- the two commands --------------------------------------------------------

(defun run-slash (args session-id)
  "/optimize PROMPT: the same prompt, rewritten for the model that will read it,
with what the rewrite gained. Typed alone it asks for the prompt."
  (declare (ignore session-id))
  (let ((prompt (nlk:trimmed (or args ""))))
    (if (plusp (length prompt))
        (rewrite-prompt prompt)
        (values "optimize: type the prompt after /optimize, or write it in the box and run /optimize"
                (nle:list-dialog
                 :title "Optimize"
                 :summary "A prompt rewritten shorter and clearer, scored before and after"
                 :rows (list (nle:ask-row "Optimize a prompt" "Prompt" "/optimize"
                                           :placeholder "what you want to ask" :verb "optimize")))))))

(defun run-stats (args session-id)
  "/prompt-stats: what the recorded rewrites did."
  (declare (ignore args session-id))
  (history))

;;;; --- the tool ----------------------------------------------------------------

(nlk:define-contract prompt-rewrite
  (:doc "Rewrite one prompt so the model that receives it reads it better: shorter, clearer, the same meaning. Use it when the operator asks to polish, tighten or improve a prompt, or before sending a long hedged request yourself. It answers the rewritten prompt under a line of before-and-after scores.")
  ("prompt" :text :required t :doc "the prompt to rewrite, as it would be sent")
  ("target" :text :default ""
            :doc "the model the prompt is going to, when it is known: a model name, or a family (claude, openai, google, deepseek, qwen)"))

(defun optimize-prompt (call)
  "The optimize_prompt tool: CALL's prompt, rewritten."
  (rewrite-prompt (prompt-rewrite-prompt call) :target (prompt-rewrite-target call)))

;;;; --- what the model reads of it ------------------------------------------------

(defparameter *manual*
  "Prompt optimizer: a prompt rewritten shorter and clearer for the model that will read it, and scored
before and after on clarity, specificity, structure and actionability, 0-100.
  /optimize PROMPT   the operator's rewrite: what it gained, then the prompt itself
  /prompt-stats      how many rewrites are recorded, their averages, the last few
  (nodecode-prompt-optimizer:rewrite-prompt \"...\" :target \"claude-opus-4-7\")  the same from eval
  optimize_prompt    the same as a tool, for a prompt the operator wants polished
The target picks the tactics: a claude target is asked for tagged structure, an openai one for the
instruction first, and an unknown model gets the plain rule. The family is read off the model name
when the section's family is auto. Language is never changed: a French prompt is rewritten in French.
Each rewrite is one side call on the auxiliary model (general.auxiliary_model); a record of it is
kept locally under the home, and nothing is sent anywhere else. ERROR: PROMPT-OPTIMIZER-ERROR says
why a rewrite was refused; the prompt then stands as it was."
  "What (help :prompt-optimizer) answers while the cell runs.")

(nle:define-cell prompt-optimizer
  (:section ("prompt-optimizer")
    (:guide "nothing here is required; family picks the model tactics (auto reads the target's name), max_tokens caps the rewrite, min_words leaves short prompts alone, record keeps a local history for /prompt-stats")
    ("family" :choice :options (quote ("auto" "claude" "openai" "google" "deepseek" "qwen")) :default "auto"
              :doc "which family's tactics every rewrite carries; auto reads the target model's name")
    ("max_tokens" :integer :default 400 :min 64
                  :doc "the most the rewriting model may answer")
    ("min_words" :integer :default 3 :min 1
                 :doc "a prompt of fewer words than this stands as it is")
    ("record" :boolean :default t
              :doc "keep a local record of every rewrite, which /prompt-stats reads"))
  (:tool "optimize_prompt" (nlk:contract 'prompt-rewrite) 'optimize-prompt)
  (:help :prompt-optimizer
         "prompt-optimizer: rewrites a prompt shorter and clearer for the model that reads it; /optimize, /prompt-stats"
         *manual*)
  (:command "optimize" 'run-slash
            :argument-hint "PROMPT"
            :catches 'prompt-optimizer-error
            :description "Rewrite a prompt shorter and clearer for the model that reads it")
  (:command "prompt-stats" 'run-stats
            :catches 'prompt-optimizer-error
            :description "What the recorded prompt rewrites did"))
