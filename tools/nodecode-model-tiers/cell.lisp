;;;; cell.lisp --- the declaration, five tools, /model-tiers, and the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The hierarchy and the archive index answer from DATA.LISP with no request at
;;;; all. One verb, model_tiers_quote, reads a document itself -- that is the
;;;; direct access this cell exists for: the words, at a line number, checkable.

(in-package #:nodecode-model-tiers)

(nlk:define-peripheral model-tiers :not-running t)

;;; --- the manual --------------------------------------------------------------------

(defparameter +primer+
  "The Claude model hierarchy, and the leaked prompt archive behind it, quoted with line numbers.
What the archive says that a vendor page does not: above Opus sits a Mythos tier, and Fable 5.2 is a
Mythos-tier model with extra safety measures for biology and cybersecurity, sharing one underlying model
with Mythos 5.2. The generation before (Fable 5.1) says the same, in its own file.
  model_tiers_files   the archive: thirteen documents, what each one is, its vendor, size and digest
  model_tiers_list    the ladder, tier by tier, top down; narrow to one tier
  model_tiers_get     one model whole: where it sits, whether a person can have it, and the words
  model_tiers_search  free text over the models, the tiers and the documents' own summaries
  model_tiers_quote   one document's own lines: every line carrying a word, or a line range, numbered
Say where the words come from, every time: a leaked system prompt from elder-plinius/CL4R1T4S, not a
vendor's page. Where the archive and a vendor disagree, give the archive as its reading of itself. Do
not present any of it as a vendor announcement -- nobody published it; it was read out of files, and the
line numbers are there so a person can check. The dates are the archive's: 9 June 2026 for the first
release, 12 June for the export-control suspension, 1 July for the restoration."
  "What (help :model-tiers) answers while the cell runs.")

;;; --- the boundaries the model crosses ------------------------------------------------

(nlk:define-contract model-tiers-files
  (:doc "The leaked prompt archive this cell quotes: every document it holds, with its key, its vendor, what the document is, its size, its line count and its digest. Narrow by vendor (Anthropic, OpenAI, Z.ai, Meta, Cursor, Cline) or by a word. Use it when the user asks what models or agents the archive covers, what a document is, or which key to give model_tiers_quote.")
  ("vendor" :text
            :doc "one of Anthropic, OpenAI, Z.ai, Meta, Cursor, Cline")
  ("text" :text
          :doc "a word to look for in the keys, the documents and their summaries"))

(nlk:define-contract model-tiers-list
  (:doc "The Claude model hierarchy as the leaked prompt archive describes it: the ladder tier by tier, top down, with what each tier is and which models sit on it. Narrow to one tier (mythos, opus, sonnet, haiku). Use it when the user asks what models there are, what is above Opus, or what a tier means. Say the words come from a leaked prompt, and give source_line.")
  ("tier" :text
          :doc "one of mythos, opus, sonnet, haiku"))

(nlk:define-contract model-tiers-get
  (:doc "One model whole: where it sits on the ladder, whether a person can have it, its API string, what is ours in a note, and the archive's own sentences about it with the document and line each stands on. The name may be approximate -- 'mythos' or 'fable' finds it. Use it when the user names a model.")
  ("name" :text :required t
          :doc "the model's name, or enough of it to name one"))

(nlk:define-contract model-tiers-search
  (:doc "Search the hierarchy and the archive by any word: a model name, a tier, a vendor, 'kernel', 'export', 'biology', 'Codex', 'Cursor'. Answers the models and documents that match, one line each. Use it when the user describes something without naming a model.")
  ("text" :text :required t
          :doc "a word or phrase to look for"))

(nlk:define-contract model-tiers-quote
  (:doc "One document of the leaked prompt archive, in its own words: give a word and get every line carrying it, numbered, at most twenty; or give lines '117' or '116-122' and get exactly those lines. FILE names the document (its key from model_tiers_files; the default is the Claude Code prompt for Fable 5.2). Use it to quote a prompt itself, to check a line number the other answers give, or when the user asks what a prompt actually says. This is the one verb that reads a document: it fetches it once from elder-plinius/CL4R1T4S and caches it.")
  ("text" :text
          :doc "a word or phrase to find in the document")
  ("lines" :text
           :doc "a line number, or a range as 116-122, instead of a word")
  ("file" :text
          :doc "the document's key, as model_tiers_files lists it"))

;;; --- what the model calls ----------------------------------------------------------

(defun files-answer (&key vendor text)
  "The archive index, with the provenance under it."
  (format nil "~a~%~a"
          (files-text (files-matching :vendor vendor :text text)
                      :heading (cond ((and vendor text) (format nil "~a documents mentioning ~s" vendor text))
                                     (vendor (format nil "~a documents" vendor))
                                     (text (format nil "documents mentioning ~s" text))
                                     (t (format nil "the archive, ~a" (getf +archive+ :name)))))
          (source-line)))

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
        (format nil "no model in the archive is named ~s. ~a~%~a"
                name (model-list-text (models) :heading "what the archive names") (source-line)))))

(defun search-text (text)
  "The models, the questions and the documents that mention TEXT."
  (let ((models (models-matching :text text))
        (documents (files-matching :text text))
        (facts (facts-matching text)))
    (format nil "~a~%~@[~%~a~]~@[~%~a~]~%~a"
            (model-list-text models :heading (format nil "models mentioning ~s" text))
            (when documents (files-text documents :heading (format nil "documents mentioning ~s" text)))
            (when facts
              (format nil "~{~a~^~%~}"
                      (mapcar (lambda (fact)
                                (format nil "  ~a - ~a" (fact-question fact) (fact-answer fact)))
                              facts)))
            (source-line))))

(defun files-tool (arguments)
  "The model_tiers_files tool: the archive index."
  (files-answer :vendor (model-tiers-files-vendor arguments)
                :text (model-tiers-files-text arguments)))

(defun list-tool (arguments)
  "The model_tiers_list tool: the ladder, or one tier."
  (list-text :tier (model-tiers-list-tier arguments)))

(defun get-tool (arguments)
  "The model_tiers_get tool: one model, whole."
  (find-text (model-tiers-get-name arguments)))

(defun search-tool (arguments)
  "The model_tiers_search tool: the entries and documents that mention a word."
  (search-text (model-tiers-search-text arguments)))

(defun quote-tool (arguments)
  "The model_tiers_quote tool: one document's own lines."
  (quote-text :text (model-tiers-quote-text arguments)
              :lines-text (model-tiers-quote-lines arguments)
              :file (model-tiers-quote-file arguments)))

;;; --- /model-tiers -------------------------------------------------------------------

(defparameter +usage+
  "usage: /model-tiers [files|tier|model|source|lines N or A-B|words]
  /model-tiers            the whole ladder, the timeline, and where it comes from
  /model-tiers files      the archive: every document it can quote
  /model-tiers mythos     one tier, and the models on it
  /model-tiers fable      one model whole, with the archive's lines quoted beside it
  /model-tiers source     the archive, its head, and the primary document's digest
  /model-tiers biology    search the models and the documents
  /model-tiers lines 116-122   the primary document's own lines, as they are
  /model-tiers kernel     every line of the primary document that carries a word")

(defun tier-named-p (text)
  "Whether TEXT names a tier the archive describes."
  (and (plusp (length text))
       (member (string-upcase text) '("MYTHOS" "OPUS" "SONNET" "HAIKU") :test #'string=)))

(defun run-slash (args session-id)
  "One /model-tiers invocation: the ladder, the archive, one tier, one model, the source, lines, or a search."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (six (min 6 (length text))))
    (cond ((zerop (length text)) (format nil "~a~%~a" (list-text) +usage+))
          ((string-equal text "source") (source-block))
          ((string-equal text "files") (files-answer))
          ((tier-named-p text) (list-text :tier text))
          ((and (> (length text) 6) (string-equal (subseq text 0 six) "lines "))
           (quote-text :lines-text (nlk:trimmed (subseq text six))))
          ((model-by-name text) (find-text text))
          (t (format nil "~a~%~a" (quote-text :text text) +usage+)))))

;;; --- the entry ------------------------------------------------------------------------

(nle:define-cell model-tiers
  (:section ("model-tiers")
    (:guide "nothing is required: the hierarchy and the archive index ship with the cell.
 model_tiers_quote reads one document from elder-plinius/CL4R1T4S once, caches it under
 ~/.cache/nodecode-model-tiers/, and reads the cache after that."))
  (:help :model-tiers
         "The Claude model hierarchy, and the leaked prompt archive behind it, with line numbers"
         +primer+)
  (:tool "model_tiers_files" (nlk:contract 'model-tiers-files) 'files-tool)
  (:tool "model_tiers_list" (nlk:contract 'model-tiers-list) 'list-tool)
  (:tool "model_tiers_get" (nlk:contract 'model-tiers-get) 'get-tool)
  (:tool "model_tiers_search" (nlk:contract 'model-tiers-search) 'search-tool)
  (:tool "model_tiers_quote" (nlk:contract 'model-tiers-quote) 'quote-tool)
  (:command "model-tiers" 'run-slash
            :description "The Claude model hierarchy, and the leaked prompt archive behind it"
            :argument-hint "files|tier|model|source|lines N|words")
  (:start (lambda () nil)))
