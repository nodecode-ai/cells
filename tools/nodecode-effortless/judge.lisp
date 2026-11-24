;;;; judge.lisp --- the effort ladder, the prompt heuristics and the judges.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Rewritten from the plugin's judge.py (see NOTICE): the ladder and its
;;;; clamping, "a named level in the message wins", "a short follow-up keeps
;;;; the current effort", the TypeSafe Jev system-one request, and the plain
;;;; OpenAI-compatible judge any local server or custom API answers. Nothing
;;;; here reads the config itself: cell.lisp passes the values, so each piece
;;;; is a pure function a test can call.

(in-package #:nodecode-effortless)

(defparameter +efforts+ '("low" "medium" "high" "xhigh" "max")
  "The effort ladder, cheapest first, as the wire spells the middle one.")

(defparameter +jev-url+ "https://api.typesafe.ai/v1/systemone"
  "TypeSafe's system-one route, the plugin's default judge.")

(defparameter +jev-model+ "jev-latest"
  "The judge model the plugin asks for by default.")

(defparameter +system-prompt+
  "You choose which reasoning effort an agentic assistant (it reads files, runs
tools and edits things, not only code) should use for the user's next message.
Pick the cheapest effort that will still do the job well.

Effort: low for quick answers, medium for normal work, high for hard problems,
xhigh or max only for very hard ones.

Judge the SCOPE and the amount of work, not whether it is code. A short message
can ask for a lot: \"go through my whole drive and clean it up\", \"review the
entire repo\", \"migrate everything\" are big, multi-step, tool-heavy jobs where
mistakes are costly: never low, usually high. Low is only for answers that need
no tools and no planning.

If the person asks for deep thought (\"think hard\", \"ultrathink\", \"be
thorough\"), pick at least high; if they ask for a quick answer, pick low.

If the message is a short follow-up to ongoing work (\"yes\", \"go\", \"ok\",
\"continue\", or the same in any language), keep the current effort.

Reply with JSON only: {\"effort\":\"low|medium|high|xhigh|max\",\"sure\":0.0-1.0
how sure you are of the effort,\"why\":\"at most 6 words\"}"
  "The instruction a plain OpenAI-compatible judge is given (the plugin's JUDGE_SYSTEM).")

(defparameter +jev-task+
  "Choose the reasoning effort an agentic assistant should use for the next user
message. It reads files, runs tools and edits things, not only code. Pick the
cheapest that still does the job well. Judge the scope and the amount of work,
not whether it is code: \"go through my whole drive and clean it up\" is a big
tool-heavy job. A short follow-up (\"yes\", \"go\", \"ok\", in any language)
keeps the current effort. \"think hard\" or \"be thorough\" means at least
high; \"quick question\" means low."
  "The task a Jev judge is given (the plugin's JEV_TASK).")

(defparameter +jev-criteria+
  '(("low" . "quick answer, no tools, no planning")
    ("medium" . "normal work: a few tool calls or edits")
    ("high" . "hard or large multi-step job: many tool calls, costly mistakes")
    ("xhigh" . "very hard: a long investigation")
    ("max" . "the hardest research-level work"))
  "What each rung means, sent with the Jev question.")

;;; --- the verdict ---------------------------------------------------------------

(defstruct (verdict (:constructor make-verdict (&key effort sure why by unsure)))
  "One judge's answer: EFFORT a rung of +EFFORTS+, SURE its confidence when
the judge gave one, WHY a short reason and BY the judge that said it."
  (effort "medium" :type string)
  sure
  (why "" :type string)
  (by "judge" :type string)
  unsure)

(defun verdict-sure-text (verdict)
  "The confidence of VERDICT as \"82% sure\", or NIL when it gave none."
  (let ((sure (verdict-sure verdict)))
    (and (numberp sure) (format nil "~d% sure" (round (* 100 sure))))))

;;; --- the ladder: bounds and clamping --------------------------------------------

(defun rung-index (effort)
  "EFFORT's place on the ladder, or NIL when it is not a rung."
  (and (stringp effort) (position (string-downcase effort) +efforts+ :test #'string=)))

(defun apply-bounds (effort floor ceiling bias)
  "EFFORT moved by BIAS and kept between FLOOR and CEILING, as the plugin's
apply_bounds does. An unnamed EFFORT reads as medium, its own fallback."
  (let* ((i (or (rung-index effort) (rung-index "medium") 1))
         (i (if (and (integerp bias) (not (zerop bias))) (max (+ i bias) 0) i))
         (i (max i (or (rung-index floor) 0)))
         (i (min i (or (rung-index ceiling) (1- (length +efforts+))))))
    (nth i +efforts+)))

(defun nearest-rung (effort accepted)
  "EFFORT when ACCEPTED holds it, else the nearest rung below it, else the
lowest rung ACCEPTED holds: the plugin's clamp_to_wire, over a ladder given."
  (cond ((member effort accepted :test #'string=) effort)
        (t (let ((i (or (rung-index effort) 1)))
             (or (loop for j downfrom (1- i) to 0
                       for rung = (nth j +efforts+)
                       when (member rung accepted :test #'string=)
                         return rung)
                 (first (member-if (lambda (rung) (member rung accepted :test #'string=))
                                   +efforts+))
                 (first +efforts+))))))

;;; --- "a named level in the message wins" -----------------------------------------

(defparameter +level-words+
  '(("low" . "low") ("medium" . "medium") ("high" . "high")
    ("xhigh" . "xhigh") ("x-high" . "xhigh") ("max" . "max") ("maximum" . "max"))
  "Every way a message names a rung, and the rung it names.")

(defparameter +negations+ '("not" "no" "don't" "dont" "never" "without" "instead")
  "A level word after one of these, within two words, is not a request for it.")

(defun level-of (word)
  "The rung WORD names, or NIL."
  (cdr (assoc (string-downcase word) +level-words+ :test #'string=)))

(defun negated-p (words index)
  "Whether WORDS at INDEX follow a negation in the two words before it."
  (loop for j from (max 0 (- index 2)) below index
        thereis (member (nth j words) +negations+ :test #'string=)))

(defun named-effort (text)
  "The rung TEXT asks for by name (\"max effort\", \"effort: high\"), or NIL.
A negated one (\"don't use max effort\") names nothing."
  (let ((words (mapcar (lambda (word) (string-right-trim ":,.;!?" word))
                       (nlk:split-words (string-downcase (or text ""))))))
    (loop for tail on words
          for i from 0
          for word = (first tail)
          do (let ((rung nil))
               (cond ((and (member word '("effort" "efforts") :test #'string=)
                           (not (negated-p words i)))
                      (setf rung (some #'level-of (subseq tail 1 (min (length tail) 5)))))
                     ((and (level-of word)
                           (member (second tail) '("effort" "efforts") :test #'string=)
                           (not (negated-p words i)))
                      (setf rung (level-of word)))
                     ;; the plugin matched a bare xhigh anywhere: the word is
                     ;; unambiguous where low, high and max are not.
                     ((and (member word '("xhigh" "x-high") :test #'string=)
                           (not (negated-p words i)))
                      (setf rung (level-of word))))
               (when rung (return-from named-effort rung))))))

(defun follow-up-p (text)
  "Whether TEXT is the plugin's follow-up: at most twelve characters, at most
two words, which keeps the effort already in force."
  (let* ((trimmed (nlk:trimmed (or text "")))
         (words (nlk:split-words trimmed)))
    (and (plusp (length trimmed))
         (<= (length trimmed) 12)
         (<= (length words) 2))))

;;; --- one POST, and the two wires ------------------------------------------------

(defun clip (text limit)
  "TEXT at most LIMIT characters, from its start."
  (let ((text (or text "")))
    (if (> (length text) limit) (subseq text 0 limit) text)))

(defun post-json (url payload headers timeout)
  "PAYLOAD, one JSON object, POSTed to URL with HEADERS added; the answer
decoded. Signals EFFORTLESS-ERROR when the wire refuses it."
  (multiple-value-bind (body status)
      (nlk:http :post url
                :headers (append (list (cons "content-type" "application/json")
                                       (cons "accept" "application/json"))
                                 headers)
                :content (nlk:encode-json-object payload)
                :timeout (max 1 (or timeout 3)))
    (unless (and (integerp status) (< status 400))
      (fail "~a answered HTTP ~a" url status))
    (nlk:decode-json (nlk:body-text body))))

(defun jev-payload (model current context message)
  "The system-one request of the plugin's ask_jev: the task, the effort now in
force, a short excerpt of the conversation and the next message."
  (apply #'nlk:make-json-object
         (append (list "model" model
                       "state" (apply #'nlk:make-json-object
                                      (append (list "task" +jev-task+)
                                              (when current (list "current_effort" current))
                                              (list "recent_conversation" (clip context 4000)
                                                    "next_message" (clip message 4000))))
                       "questions"
                       (nlk:make-json-object
                        "effort"
                        (nlk:make-json-object
                         "type" "choice"
                         "instructions" "Which effort fits the next message?"
                         "criteria" (apply #'nlk:make-json-object
                                           (loop for (rung . meaning) in +jev-criteria+
                                                 collect rung collect meaning))))))))

(defun jev-verdict (data current)
  "The verdict in a system-one answer, or NIL when it named no rung."
  (let* ((answer (nlk:json-value data :object "answers" "effort"))
         (rung (string-downcase (or (nlk:json-value answer :text "choice") "")))
         (sure (nlk:json-value answer :number "confidence")))
    (when (rung-index rung)
      (make-verdict :effort rung :sure sure :by "jev"
                     :unsure (and current (numberp sure) (< sure 0.5))))))

(defun openai-payload (model question)
  "The chat request a plain OpenAI-compatible judge is sent."
  (nlk:make-json-object
   "model" model
   "messages" (vector (nlk:make-json-object "role" "system" "content" +system-prompt+)
                      (nlk:make-json-object "role" "user" "content" question))
   "max_tokens" 200
   "temperature" 0))

(defun openai-question (current context message)
  "The one user message the OpenAI-compatible judge reads."
  (format nil "Current: ~a~@[~%~%Recent conversation:~%~a~]~%~%Next message:~%~a"
          (or current "none")
          (let ((context (clip context 1500)))
            (and (plusp (length context)) context))
          (clip message 2000)))

(defun parse-verdict-text (text)
  "The verdict in a judge reply, JSON possibly embedded in prose, or NIL."
  (let* ((text (or text ""))
         (start (position #\{ text))
         (end (position #\} text :from-end t)))
    (when (and start end (< start end))
      (let ((raw (ignore-errors (nlk:decode-json (subseq text start (1+ end))))))
        (when raw
          (let ((rung (string-downcase (or (nlk:json-value raw :text "effort") "")))
                (sure (nlk:json-value raw :number "sure"))
                (why (nlk:json-value raw :text "why")))
            (when (rung-index rung)
              (make-verdict :effort rung :sure sure :by "judge"
                            :why (clip (or why "") 80)))))))))

(defun openai-verdict (data)
  "The verdict in an OpenAI-compatible chat answer, or NIL."
  (let* ((choices (nlk:json-value data :array "choices"))
         (first-choice (and choices (plusp (length choices)) (aref choices 0)))
         (message (nlk:json-value first-choice :object "message"))
         (content (nlk:json-value message :string "content")))
    (parse-verdict-text content)))
