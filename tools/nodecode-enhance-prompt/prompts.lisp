;;;; prompts.lisp --- the rewrite's own words: the base guide, the three stage
;;;; hints, and the wrappers a model puts around an answer.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin keeps these in prompts.py (SYSTEM, USER_TEMPLATE)
;;;; and in the Desktop half's stage hints; the sparkle and the agent tool
;;;; read the same text so both halves behave alike. Here they are the one
;;;; copy the tool, the command and (help :enhance-prompt) all speak from.

(in-package #:nodecode-enhance-prompt)

(defparameter +min-draft-chars+ 8
  "A draft shorter than this is not rewritten: there is nothing to deepen,
and the original plugin ignores one too.")

(defparameter *system-prompt*
  "You are a prompt engineer. Rewrite user requests into clear, well-structured agent prompts.

Your job is NOT to answer the request - it is to reformulate it so an AI agent can execute it correctly on the first attempt.

## Format

Write ONE goal line followed by numbered items (1. 2. 3.). Preserve the original language.

**For short or already-clear inputs:** return the same intent in fewer words as 1-3 tight bullets. Do not pad.

**For substantive inputs (multi-sentence, real task, no clear shape):** produce a production-grade brief:

1. **Goal** - one line stating what success looks like
2. **Role & context** - who the agent is, what tools it has, any runtime constraints
3. **Scope** - what is in scope (IN:) and what is out of scope (OUT:)
4. **Requirements** - 3-7 concrete numbered requirements, each one a single verifiable action
5. **Deliverable** - what form the output takes: files, format, coverage
6. **Quality gates** - what \"done\" means, how the agent should verify completion
7. **Anti-patterns** - 2-4 things to NOT do

Use only the sections that apply. Do not invent sections.

## Rules

- Match the original language (English, Malay, etc.)
- Do NOT answer the request - restate it as an actionable task
- Do NOT add goals the user did not mention
- Do NOT write tutorial-style output (\"First, do X, then Y...\")
- Prefer concrete nouns over vague ones (\"table\" not \"the data structure\")
- Output exactly as much as the brief needs. A 5-sentence input may produce 200 words; a paragraph-long input may produce 1,000+ words. Do not artificially pad or cut - write the brief that an agent actually needs to execute the task correctly."
  "The base instruction, verbatim from the original's prompts.py: the same text
the Desktop sparkle and the agent tool are given, so a rewrite is the same
rewrite on every surface.")

(defparameter *user-preamble* "Rewrite this request as an agent prompt:"
  "What the draft is introduced as, the original's USER_TEMPLATE.")

(defparameter *stage-hints*
  (vector
   nil
   "## Stage 1 - Clarify
Produce a clean, legible brief. One goal line, then 3-5 numbered
requirements. The output should be a short, dense version of the
user's intent - readable in 30 seconds."
   "## Stage 2 - Detail
Take the brief above and break each requirement into a concrete
sub-task. For each numbered item, add:
- the tool or action the agent should use
- the context or inputs it needs
- the deliverable (file, response, or state change)

Result is an executable task list an agent can follow step by step."
   "## Stage 3 - Horizon
Reframe the task as a long-running project. Add:
- Phases or milestones (numbered, with checkpoints)
- Dependencies between phases
- Success criteria per phase (how the agent knows it's done)
- A time-horizon framing (\"end of day\", \"weekly\", \"milestone-based\")

The result is a phased plan, not a one-shot request. The agent
should be able to resume from any checkpoint.")
  "The three stage hints, verbatim from the original's desktop half: a click
that deepens a draft does not ask for a longer version of the same text, it
asks for a different transformation. Index 0 is unused, so a stage is
1, 2 or 3.")

(defun stage-hint (stage)
  "The hint for STAGE, kept inside 1-3."
  (aref *stage-hints* (max 1 (min 3 (or stage 1)))))

(defun system-prompt (stage)
  "The whole system message one rewrite at STAGE runs with: the base guide and
that stage's hint."
  (format nil "~a~%~%~a" *system-prompt* (stage-hint stage)))

(defun user-message (draft)
  "The single user message DRAFT is rewritten from."
  (format nil "~a~%~%~a~%~%---" *user-preamble* draft))

;;; --- what a model wraps around its answer ------------------------------------

(defparameter +space-chars+ '(#\Space #\Tab #\Newline #\Return #\Page)
  "What a line of an answer may leave at either end.")

(defparameter +quote-chars+
  (list #\' #\" #\` (code-char #x201C) (code-char #x201D)
        (code-char #x2018) (code-char #x2019))
  "Straight and curly quotes: a model that quotes the brief it wrote, or the
code fence around it, did not mean either as part of the brief.")

(defun strip-wrappers (text)
  "TEXT with what a serving backend adds taken off: thinking blocks, the
fullwidth channel tags some backends emit, the code fence around the whole
answer, and outer quotes. The original's strip_wrappers, in the same order."
  (let ((s (string-trim +space-chars+ (if text (string text) ""))))
    (setf s (ppcre:regex-replace-all "(?is)<think.*?</think>" s ""))
    (setf s (ppcre:regex-replace-all
             (format nil "<~c[^~c]*~c>" (code-char #xFF5C) (code-char #xFF5C) (code-char #xFF5C))
             s ""))
    (setf s (string-trim +space-chars+ s))
    (when (and (>= (length s) 3) (string= "```" s :end2 3))
      (let ((newline (position #\Newline s)))
        (setf s (if newline (subseq s (1+ newline)) ""))))
    (when (and (>= (length s) 3) (string= "```" s :start2 (- (length s) 3)))
      (setf s (subseq s 0 (- (length s) 3))))
    (string-trim +quote-chars+ (string-trim +space-chars+ s))))






