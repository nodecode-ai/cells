;;;; cell.lisp --- the declaration, five tools, /jailbreaks, and the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The catalogue answers from DATA.LISP with no request at all. Two verbs read
;;;; a prompt itself -- jailbreaks_quote and jailbreaks_grep -- and they fetch it
;;;; from the author's repo once, then read the cache. That is the direct access
;;;; this cell exists for: 51 prompts, quotable by line, with no clone.

(in-package #:nodecode-jailbreaks)

(nlk:define-peripheral jailbreaks :not-running t)

;;; --- the manual --------------------------------------------------------------------

(defparameter +primer+
  "A working collection of jailbreak prompts: 51 files in 19 folders, over the models people actually run.
What a reader gets that a git clone does not: which model each file targets, what kind of file it is
(a deployment specification, a persona, a content policy, a recovered system prompt, an Ollama Modelfile),
which files are byte-identical to each other, and any file's own lines on demand.
  jailbreaks_list    the catalogue: every prompt, its target, its kind, its size; narrow by folder, kind or word
  jailbreaks_get     one prompt whole: what it targets, its digest, and its opening lines quoted
  jailbreaks_search  a word over the catalogue - keys, targets, kinds, folders and what each file does
  jailbreaks_grep    a word inside the prompts themselves: every line that carries it, across all or one
  jailbreaks_quote   one prompt's own lines: a range as 1-40, or every line carrying a word, numbered
The collection has NO licence file (togg53192-cmd/jailbreaks, 16,208 stars at reading). So give it as it is:
quote with the file name and the line, say the prompts are its author's, and do not present any of them as
ours. The kinds matter when you answer: a 'system-prompt' is the model's own prompt as it was run, which is
evidence; a 'persona' or a 'workspace-policy' is a rewrite someone applied, which is a technique. Do not
call a recovered system prompt a jailbreak, and do not claim a prompt works on a model it was not written for
-- the collection itself ships one register unadapted for four targets, which is the exception that proves it."
  "What (help :jailbreaks) answers while the cell runs.")

;;; --- the boundaries the model crosses ------------------------------------------------

(nlk:define-contract jailbreaks-list
  (:doc "The catalogue of the collection: every prompt with the model it targets, the kind of file it is, its folder, its size and one line on what it does. Narrow by folder (vendor), by kind (persona, workspace-policy, content-policy, system-prompt, modelfile, withdrawn, empty) or by a word. Use it when the user asks what jailbreaks exist, what there is for a model, or which of these are real system prompts.")
  ("vendor" :text
            :doc "a folder of the collection: Alibaba, Anthropic, DeepSeek, Google, Meta, Moonshot AI, OpenAI, xAI, Zhipu AI, and the authors' own 0x8D jailbreaks, DarkyHArry, raghavraut, Stealth Models, and the rest")
  ("kind" :text
          :doc "one of persona, workspace-policy, content-policy, system-prompt, modelfile, withdrawn, empty")
  ("text" :text
          :doc "a word to look for in the keys, the targets, the folders and what each file does"))

(nlk:define-contract jailbreaks-get
  (:doc "One prompt whole: the model it targets, the kind of file it is, where it lives, its size, its line count, its sha256, whether it is one of a set of identical files, and its own opening lines quoted. The name may be approximate -- 'jack' or 'vless' finds it. Use it when the user names a jailbreak, or asks what one of them actually says.")
  ("name" :text :required t
          :doc "the prompt's key, or enough of it to name one"))

(nlk:define-contract jailbreaks-search
  (:doc "Search the catalogue by any word: a model name, a technique, a folder, 'sandbox', 'fiction', 'AGENTS.md', 'opencode'. Answers the prompts that match, one entry each, and any question the collection answers as a whole. Use it when the user describes what they want without naming a file.")
  ("text" :text :required t
          :doc "a word or phrase to look for"))

(nlk:define-contract jailbreaks-grep
  (:doc "Search INSIDE the prompts: every line carrying a word, with its file and line number, across all 51 (or one, when FILE is given). Each file is fetched once from the author's repository and cached. Use it when the user asks where something appears in the collection, or which prompts share a phrase - 'UNRESTRICTED', 'content policy', 'fiction', 'never refuse'.")
  ("text" :text :required t
          :doc "a word or phrase to find inside the prompts")
  ("file" :text
          :doc "one prompt's key, to search only that file"))

(nlk:define-contract jailbreaks-quote
  (:doc "One prompt in its own words: give LINES as a range ('1-40', '120') and get exactly those lines; or give TEXT and get every line carrying it, numbered. FILE names the prompt (its key from jailbreaks_list). Use it to read a jailbreak properly, or to check a line the other answers give. This verb fetches the file once and caches it.")
  ("file" :text :required t
          :doc "the prompt's key, as jailbreaks_list lists it")
  ("text" :text
          :doc "a word or phrase to find in that prompt")
  ("lines" :text
           :doc "a line number, or a range as 1-40, instead of a word"))

;;; --- what the model calls ----------------------------------------------------------

(defun list-answer (&key vendor kind text)
  "The catalogue, with the provenance under it."
  (format nil "~a~%~a~@[~%~a~]"
          (entries-text (entries-matching :vendor vendor :kind kind :text text)
                        :heading (cond ((and vendor kind) (format nil "~a, ~a" vendor (kind-name kind)))
                                       (vendor (format nil "the ~a folder" vendor))
                                       (kind (format nil "every ~a" (kind-name kind)))
                                       (text (format nil "prompts mentioning ~s" text))
                                       (t (format nil "the collection, ~a" (getf +archive+ :name)))))
          (source-line)
          (when (or vendor kind text) nil)))

(defun get-answer (name)
  "One prompt, whole, or the catalogue when NAME names none."
  (let ((entry (entry-by-key name)))
    (if entry
        (format nil "~a~%~a~%  the opening:~%~a"
                (entry-block entry) (source-line) (opening-text entry))
        (format nil "no prompt in the collection is named ~s.~%~a~%~a"
                name (entries-text (entries) :heading "the collection") (source-line)))))

(defun entry-block (entry)
  "One entry as the block a person reads."
  (format nil "~a~%  kind:    ~a - ~a~%  targets: ~a~%  folder:  ~a~%  file:    ~a~%  ~:d bytes, ~:d lines, sha256 ~a~@[~%  part of: ~a (~d files, byte-identical)~]~%  ~a"
          (entry-key entry)
          (kind-name (entry-kind entry))
          (or (kind-sentence (entry-kind entry)) "a kind DATA.LISP does not describe")
          (entry-target entry)
          (entry-vendor entry)
          (entry-path entry)
          (entry-bytes entry) (entry-lines entry) (entry-sha entry)
          (entry-group entry)
          (let ((group (find (entry-group entry) (groups) :test #'string= :key #'group-name)))
            (and group (length (group-paths group))))
          (entry-says entry)))

(defun search-answer (text)
  "The prompts and the collection's own answers that mention TEXT."
  (let ((entries (entries-matching :text text))
        (facts (facts-matching text)))
    (format nil "~a~@[~%~%~a~]~@[~%groups:~%~a~]~%~a"
            (entries-text entries :heading (format nil "prompts mentioning ~s" text))
            (when facts
              (format nil "~{~a~^~%~}"
                      (mapcar (lambda (fact) (format nil "  ~a~%    ~a" (fact-question fact) (fact-answer fact)))
                              facts)))
            (when (some (lambda (e) (entry-group e)) entries) (groups-text))
            (source-line))))

(defun list-tool (arguments)
  "The jailbreaks_list tool: the catalogue."
  (list-answer :vendor (jailbreaks-list-vendor arguments)
               :kind (jailbreaks-list-kind arguments)
               :text (jailbreaks-list-text arguments)))

(defun get-tool (arguments)
  "The jailbreaks_get tool: one prompt whole."
  (get-answer (jailbreaks-get-name arguments)))

(defun search-tool (arguments)
  "The jailbreaks_search tool: the prompts that mention a word."
  (search-answer (jailbreaks-search-text arguments)))

(defun grep-tool (arguments)
  "The jailbreaks_grep tool: inside the prompts."
  (grep-text (jailbreaks-grep-text arguments) :entry (jailbreaks-grep-file arguments)))

(defun quote-tool (arguments)
  "The jailbreaks_quote tool: one prompt's own lines."
  (quote-text :file nil
              :entry (jailbreaks-quote-file arguments)
              :text (jailbreaks-quote-text arguments)
              :lines-text (jailbreaks-quote-lines arguments)))

;;; --- /jailbreaks -------------------------------------------------------------------

(defparameter +usage+
  "usage: /jailbreaks [list|kind|get KEY|grep WORDS|lines KEY A-B|search WORDS]
  /jailbreaks             what is in the collection, by kind, and the duplicated files
  /jailbreaks kinds       what the kinds mean
  /jailbreaks persona     every prompt of one kind
  /jailbreaks Anthropic   one folder
  /jailbreaks jack        one prompt, with its opening lines
  /jailbreaks lines jack 1-40   that prompt's own lines
  /jailbreaks grep UNRESTRICTED inside the prompts, file and line for each hit")

(defun run-slash (args session-id)
  "One /jailbreaks invocation."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (space (position #\Space text))
         (word (if space (subseq text 0 space) text))
         (rest (if space (nlk:trimmed (subseq text (1+ space))) ""))
         (space2 (position #\Space rest))
         (head (if space2 (subseq rest 0 space2) rest))
         (tail (if space2 (nlk:trimmed (subseq rest (1+ space2))) "")))
    (cond
      ((zerop (length text)) (format nil "~a~%~%~a" (summary-text) +usage+))
      ((string-equal word "kinds")
       (format nil "~{~a~^~%~}"
               (mapcar (lambda (kind) (format nil "  ~va ~a" (kind-width) (kind-name kind) (kind-sentence kind)))
                       (kinds))))
      ((member (string-downcase word) (mapcar #'kind-name (kinds)) :test #'string=)
       (format nil "~a~%~a" (entries-text (entries-matching :kind word)
                                           :heading (format nil "every ~a" (kind-name word)))
               (source-line)))
      ((member word (vendors) :test #'string-equal)
       (format nil "~a~%~a" (entries-text (entries-matching :vendor word)
                                           :heading (format nil "the ~a folder" word))
               (source-line)))
      ((string-equal word "grep")
       (if (zerop (length rest))
           (format nil "grep what? ~a" +usage+)
           (grep-text rest)))
      ((string-equal word "lines")
       (if (zerop (length rest))
           (format nil "lines of which prompt? ~a" +usage+)
           (quote-text :entry head :lines-text tail)))
      ((string-equal word "search") (search-answer rest))
      ((entry-by-key (if (zerop (length rest)) word text))
       (let ((entry (entry-by-key (if (zerop (length rest)) word text))))
         (format nil "~a~%~a~%  the opening:~%~a" (entry-block entry) (source-line) (opening-text entry))))
      (t (format nil "~a~%~a" (search-answer text) +usage+)))))

;;; --- the entry ------------------------------------------------------------------------

(nle:define-cell jailbreaks
  (:section ("jailbreaks")
    (:guide "nothing is required: the catalogue ships with the cell. Reading a prompt fetches that
 one file from github.com/togg53192-cmd/jailbreaks once, caches it under
 ~/.cache/nodecode-jailbreaks/, and reads the cache after that."))
  (:help :jailbreaks
         "A working collection of jailbreak prompts, indexed: 51 files, their targets, their kinds, and any of them quoted by line"
         +primer+)
  (:tool "jailbreaks_list" (nlk:contract 'jailbreaks-list) 'list-tool)
  (:tool "jailbreaks_get" (nlk:contract 'jailbreaks-get) 'get-tool)
  (:tool "jailbreaks_search" (nlk:contract 'jailbreaks-search) 'search-tool)
  (:tool "jailbreaks_grep" (nlk:contract 'jailbreaks-grep) 'grep-tool)
  (:tool "jailbreaks_quote" (nlk:contract 'jailbreaks-quote) 'quote-tool)
  (:command "jailbreaks" 'run-slash
            :description "A working collection of jailbreak prompts, indexed and quotable"
            :argument-hint "list|kinds|KIND|FOLDER|KEY|grep WORDS|lines KEY A-B")
  (:start (lambda () nil)))
