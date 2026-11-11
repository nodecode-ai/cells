;;;; cell.lisp --- the declaration, four tools, /model-tiers, and the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The hierarchy answers from DATA.LISP with no request at all. One verb,
;;;; model_tiers_quote, reads the leaked file itself -- that is the direct access
;;;; this cell exists for: the model's own words, at a line number, checkable.

(in-package #:nodecode-model-tiers)

(nlk:define-peripheral model-tiers :not-running t)

;;; --- the manual --------------------------------------------------------------------

(defparameter +primer+
  "The Claude model hierarchy as Claude Code's own prompt describes it, quoted with line numbers from a
leaked copy of that prompt (ANTHROPIC/CLAUDE-CODE-FABLE-5.2.txt in elder-plinius/CL4R1T4S, 957,315 bytes,
7,832 lines). What the file says that a vendor page does not: above Opus sits a Mythos tier, and the model
answering you, Fable 5.2, is a Mythos-tier model with extra safety measures for biology and cybersecurity.
  model_tiers_list    the ladder, tier by tier, top down; narrow to one tier
  model_tiers_get     one model whole: where it sits, whether a person can have it, and the file's words
  model_tiers_search  free text over names, tiers, availability, notes, quotes and the timeline
  model_tiers_quote   the file itself: every line carrying a word, or a line range, numbered
Say where the words come from, every time: a leaked system prompt, not a vendor's page. Where it and a
vendor disagree, give the file as the file's reading of itself. Do not present the hierarchy as Anthropic's
own announcement -- nobody published it; it was read out of a 7,832-line file, and the line numbers are
there so a person can check. The dates are the file's: 9 June 2026 for the first release, 12 June for the
export-control suspension, 1 July for the restoration."
  "What (help :model-tiers) answers while the cell runs.")

;;; --- the boundaries the model crosses ------------------------------------------------

(nlk:define-contract model-tiers-list
  (:doc "The Claude model hierarchy as the leaked Claude Code prompt describes it: the ladder tier by tier, top down, with what each tier is and which models sit on it. Narrow to one tier (mythos, opus, sonnet, haiku). Use it when the user asks what models there are, what is above Opus, or what a tier means. Say the words come from a leaked prompt, and give source_line.")
  ("tier" :text
          :doc "one of mythos, opus, sonnet, haiku"))

(nlk:define-contract model-tiers-get
  (:doc "One model whole: where it sits on the ladder, whether a person can have it, its API string, what is ours in a note, and the file's own sentences about it with the line each stands on. The name may be approximate -- 'mythos' or 'fable' finds it. Use it when the user names a model.")
  ("name" :text :required t
          :doc "the model's name, or enough of it to name one"))

(nlk:define-contract model-tiers-search
  (:doc "Search the hierarchy by any word: a model name, a tier, 'kernel', 'export', 'biology', 'same model'. Answers the models whose entry or quote mentions it, one line each. Use it when the user describes something without naming a model.")
  ("text" :text :required t
          :doc "a word or phrase to look for"))

(nlk:define-contract model-tiers-quote
  (:doc "The leaked file's own words: give a word and get every line carrying it, numbered, at most twenty; or give lines '117' or '116-122' and get exactly those lines. Use it to quote the prompt itself, to check a line number the other answers give, or when the user asks what the prompt actually says about something. This is the one verb that reads the file: it fetches it once from elder-plinius/CL4R1T4S and caches it, then reads the cache.")
  ("text" :text
          :doc "a word or phrase to find in the file")
  ("lines" :text
           :doc "a line number, or a range as 116-122, instead of a word"))

;;; --- what the model calls ----------------------------------------------------------

(defun list-text (&key tier)
  "The ladder, or one tier of it, with the provenance under it."
  (format nil "~a~%~a"
          (if (or (null tier) (zerop (length tier)))
              (format nil "~a~%~%~a" (hierarchy-text) (timeline-text))
              (model-list-text (models-matching :tier tier)
                               :heading (format nil "the ~a tier" (tier-name tier))))
          (source-line)))

(defun find-text (name)
  "One model, whole, or the whole list when NAME names none."
  (let ((model (model-by-name name)))
    (if model
        (format nil "~a~%~a" (model-block model) (source-line))
        (format nil "no model in the file is named ~s. ~a~%~a"
                name (model-list-text (models) :heading "what the file names") (source-line)))))

(defun search-text (text)
  "The models and the questions that mention TEXT."
  (format nil "~a~%~@[~%~a~]~%~a"
          (model-list-text (models-matching :text text)
                           :heading (format nil "models mentioning ~s" text))
          (let ((facts (facts-matching text)))
            (when facts
              (format nil "~{~a~^~%~}"
                      (mapcar (lambda (fact)
                                (format nil "  ~a - ~a" (fact-question fact) (fact-answer fact)))
                              facts))))
          (source-line)))

(defun list-tool (arguments)
  "The model_tiers_list tool: the ladder, or one tier."
  (list-text :tier (model-tiers-list-tier arguments)))

(defun get-tool (arguments)
  "The model_tiers_get tool: one model, whole."
  (find-text (model-tiers-get-name arguments)))

(defun search-tool (arguments)
  "The model_tiers_search tool: the entries that mention a word."
  (search-text (model-tiers-search-text arguments)))

(defun quote-tool (arguments)
  "The model_tiers_quote tool: the file's own lines."
  (quote-text :text (model-tiers-quote-text arguments)
              :lines-text (model-tiers-quote-lines arguments)))

;;; --- /model-tiers -------------------------------------------------------------------

(defparameter +usage+
  "usage: /model-tiers [tier|model|source|lines N or A-B|words]
  /model-tiers            the whole ladder, the timeline, and where it comes from
  /model-tiers mythos     one tier, and the models on it
  /model-tiers fable      one model whole, with the file's lines quoted beside it
  /model-tiers source     the file itself: size, digest, commit, link
  /model-tiers biology    search the hierarchy
  /model-tiers lines 116-122   the leaked file's own lines, as they are
  /model-tiers kernel     every line of the file that carries a word")

(defun tier-named-p (text)
  "Whether TEXT names a tier the file describes."
  (and (plusp (length text))
       (member (string-upcase text) '("MYTHOS" "OPUS" "SONNET" "HAIKU") :test #'string=)))

(defun run-slash (args session-id)
  "One /model-tiers invocation: the ladder, one tier, one model, the source, the file's lines, or a search."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (six (min 6 (length text))))
    (cond ((zerop (length text)) (format nil "~a~%~a" (list-text) +usage+))
          ((string-equal text "source") (source-block))
          ((tier-named-p text) (list-text :tier text))
          ((and (> (length text) 6) (string-equal (subseq text 0 six) "lines "))
           (quote-text :lines-text (nlk:trimmed (subseq text six))))
          ((model-by-name text) (find-text text))
          (t (format nil "~a~%~a" (quote-text :text text) +usage+)))))

;;; --- the entry ------------------------------------------------------------------------

(nle:define-cell model-tiers
  (:section ("model-tiers")
    (:guide "nothing is required: the hierarchy ships with the cell. model_tiers_quote reads
 ANTHROPIC/CLAUDE-CODE-FABLE-5.2.txt from elder-plinius/CL4R1T4S once, caches it, and reads the cache
 after that."))
  (:help :model-tiers
         "The Claude model hierarchy as the leaked Claude Code prompt tells it, with line numbers"
         +primer+)
  (:tool "model_tiers_list" (nlk:contract 'model-tiers-list) 'list-tool)
  (:tool "model_tiers_get" (nlk:contract 'model-tiers-get) 'get-tool)
  (:tool "model_tiers_search" (nlk:contract 'model-tiers-search) 'search-tool)
  (:tool "model_tiers_quote" (nlk:contract 'model-tiers-quote) 'quote-tool)
  (:command "model-tiers" 'run-slash
            :description "The Claude model hierarchy, as the leaked Claude Code prompt describes it"
            :argument-hint "tier|model|source|lines N|words")
  (:start (lambda () nil)))
