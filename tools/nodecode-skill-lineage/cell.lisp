;;;; cell.lisp --- the two tools, /lineage, and what the cell declares.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A record verb the model calls when it writes a skill, and a query verb for
;;;; everything the record answers afterwards. Nothing here runs on its own:
;;;; no thread, no hook, no timer. The chain is a file under the home, the
;;;; verbs are ordinary functions in the lineage package, and the model reaches
;;;; them through eval or through the two tools.

(in-package #:nodecode-skill-lineage)

;;; --- the manual ---------------------------------------------------------------

(defparameter +primer+
  "skill-lineage records where a skill version's content came from, and lets you
verify the record afterwards. Lisp functions in the lineage package, called
through eval; each answers a string, and a refusal reads ERROR: SKILL-LINEAGE-ERROR.
  (lineage:record \"skill\" :after TEXT :before TEXT :sources (LIST OF TEXTS) :asked TEXT)
      record one version: the skill's body after the write (required) and before
      it, the texts it drew on -- a page fetched, a file read, a tool result --
      and what the operator typed this session. Only keyed hashes are kept: no
      text is stored. Call it once per skill write, after the write.
  (lineage:trace \"skill\")          where its newest version came from
  (lineage:descendants TARGET)      every skill version that came from a
      source node, a URL the agent read, or a skill name (:mentions t also
      starts from sources that merely name a URL; :weak t follows context)
  (lineage:report)                   versions with operative tokens -- a URL, a
      command, a path -- that no recorded source explains
  (lineage:verify)                   is the chain intact; pass :head HASH, a
      hash recorded elsewhere, to tell truncation from an intact chain
  (lineage:head)                     the newest seq and hash, to record elsewhere
  (lineage:status)                   is anything being recorded, and where
An edge is strong (3 or more shared 8-word shingles, :strong-min), anchor (a
shared operative token that is not common across sources), or context (seen in
the session, nothing measurable shared). Leaning on the record: it proves where
content could have come from, not that it caused the write."
  "What (help :lineage) answers while the cell runs.")

;;; --- the tools ------------------------------------------------------------------

(defparameter +actions+ '("status" "trace" "descendants" "report" "verify" "head")
  "What the query tool answers.")

(nlk:define-contract lineage-write
  (:doc "Record where one skill version's content came from, after writing it. Pass the skill's name, the body as it now reads, the body it had, and the texts it drew on this session. Only keyed hashes are kept -- the text itself is never stored -- and the record is a hash chain that (lineage:verify) re-walks. Call it once per skill write, after the file is written.")
  ("skill" :text :required t
           :doc "the skill's name, as its folder or frontmatter spells it")
  ("after" :text :required t
           :doc "the skill's whole body as it now reads; it is hashed and never stored")
  ("before" :string :default ""
            :doc "the body it had before this write; empty for a skill that did not exist")
  ("sources" :array :of :string :default #()
             :doc "the texts this version drew on: page text, a file's contents, a tool result")
  ("asked" :string :default ""
           :doc "what the operator typed this session, when the skill came from their words"))

(nlk:define-contract lineage-query
  (:doc "Ask the skill-lineage record where a skill version's content came from: trace one skill, list everything a source produced, list versions with untraceable operative tokens, verify the chain, or print its head. Every action answers text.")
  ("action" :text :required t :options +actions+
            :doc "status, trace, descendants, report, verify or head")
  ("skill" :string :doc "the skill's name, for trace")
  ("target" :string :doc "for descendants: a source node id, a URL the agent read, or a skill name")
  ("weak" :boolean :doc "follow context edges too: sources the session saw that share nothing measurable")
  ("mentions" :boolean :doc "for a URL: also start from sources that only name it")
  ("head" :string :doc "for verify: a head hash recorded elsewhere, which tells truncation from an intact chain"))

(defun record-tool (args)
  "The skill_lineage_record tool: one line saying what was written."
  (record (lineage-write-skill args)
          :after (lineage-write-after args)
          :before (or (lineage-write-before args) "")
          :sources (coerce (or (lineage-write-sources args) #()) 'list)
          :asked (or (lineage-write-asked args) "")))

(defun query-tool (args)
  "The skill_lineage_query tool: the answer's text for one action."
  (let ((action (string-downcase (lineage-query-action args)))
        (weak (lineage-query-weak args))
        (mentions (lineage-query-mentions args)))
    (cond
      ((equal action "status") (status))
      ((equal action "report") (report))
      ((equal action "head") (head))
      ((equal action "verify") (verify :head (lineage-query-head args)))
      ((equal action "trace")
       (trace (or (lineage-query-skill args) (lineage-query-target args) "")
              :weak weak))
      ((equal action "descendants")
       (descendants (or (lineage-query-target args) (lineage-query-skill args) "")
                    :weak weak :mentions mentions))
      (t (fail "~a is not one of its actions: ~{~a~^, ~}" action +actions+)))))

;;; --- /lineage --------------------------------------------------------------------

(defun split-verb (text)
  "TEXT's first word, lowercased, and the rest of it trimmed."
  (let* ((text (string-trim " " (or text "")))
         (space (position-if (lambda (c) (member c '(#\Space #\Tab))) text)))
    (if space
        (values (string-downcase (subseq text 0 space)) (string-trim " " (subseq text space)))
        (values (string-downcase text) ""))))

(defun run-slash (args session-id)
  "The answer to one /lineage invocation: the record's own verbs, typed."
  (declare (ignore session-id))
  (multiple-value-bind (verb rest) (split-verb args)
    (cond
      ((or (equal verb "") (equal verb "status")) (status))
      ((equal verb "trace")
       (if (plusp (length rest))
           (trace rest)
           "usage: /lineage trace <skill>"))
      ((equal verb "descendants")
       (if (plusp (length rest))
           (descendants rest)
           "usage: /lineage descendants <url|skill|node id>"))
      ((equal verb "report") (report))
      ((equal verb "verify")
       (if (plusp (length rest)) (verify :head rest) (verify)))
      ((equal verb "head") (head))
      (t (format nil "~a is not one of its verbs: status, trace, descendants, report, verify, head" verb)))))

;;; --- the declaration -------------------------------------------------------------

(nle:define-cell skill-lineage
  (:section ("skill_lineage")
    (:guide "nothing here is required: the chain is a file under the home. strong_min is how many shared 8-word shingles make a strong edge; context_edges is how many context edges one version lists")
    ("strong_min" :integer :default 3 :min 1
                  :doc "shared 8-word shingles that make a strong edge")
    ("context_edges" :integer :default 50 :min 0
                     :doc "context edges one version lists; the rest are counted, not listed"))
  (:help :lineage
         "where each skill version's content came from, in a hash chain you can verify; /lineage"
         +primer+)
  (:tool "skill_lineage_record" (nlk:contract 'lineage-write) 'record-tool)
  (:tool "skill_lineage_query" (nlk:contract 'lineage-query) 'query-tool)
  (:command "lineage" 'run-slash
            :catches 'skill-lineage-error
            :description "Where a skill version's content came from, and whether the record is intact"
            :argument-hint "status|trace SKILL|descendants TARGET|report|verify|head"))
