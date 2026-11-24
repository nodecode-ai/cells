;;;; cell.lisp --- the declaration: four tools, one command, one manual topic.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin gives a host agent two prose skills and leaves the
;;;; typing to it: prefix every call, pick the smallest query, do not invent a
;;;; base branch, do not merge. Here the prefix and the argv are the cell's
;;;; job, the smallest-query advice is the tools' descriptions, and the rest of
;;;; the prose is the (help :sem) topic the model reads before it calls them.

(in-package #:nodecode-sem)

(nlk:define-peripheral sem :not-running t)

(defparameter +primer+
  "Sem and Weave answer through the nodecode-sem cell: four tools, each one call to the `sem' or
`weave' CLI the operator installed, in the session's repository, answering what that CLI printed.
  sem_find     names, callers, context, budget, in  - where an entity is defined, which
      entities call it (callers), or its body with the context around it (context, budget how
      many characters of it); `in' narrows a duplicated name to one file. Ask for several
      known names in one call rather than one call each.
  sem_impact   name, tests, file  - what depends on an entity; tests also lists the tests that
      reach it. Disambiguate a shared name with file.
  sem_diff     range  - changed entities, not changed lines; empty reviews the working tree
      against HEAD, or name the refs you were given as BASE..HEAD. Never invent a base branch,
      and untracked files are not in it: check git status when those matter.
  weave_preview branch, file  - how that branch would merge into HEAD, without merging it;
      file narrows a known conflict. Needs Weave installed.
Choose the smallest query that answers the question, and read bodies or surrounding source only
when the task needs them. Static callers and tests can be incomplete, so an empty result is not
proof that none exists: fall back to text search and file reads for configuration, unsupported
syntax and omitted context. The tools never edit, stage, commit or merge, and never run
`sem check' (that executes the repository's own build, linter and tests). A clean merge preview
is not compiler or behavioral verification. Report the changed entities, their callers and the
unresolved questions, with the paths the CLI printed."
  "What (help :sem) answers while the cell runs.")

;;; --- the boundary of each tool --------------------------------------------

(nlk:define-contract sem-find-query
  (:doc "Ask Sem where an entity is defined, who calls it, or what its body looks like with the
context around it. One call may name several known entities. Answers `sem find ... --json'.")
  ("names" :array :of :string :required t
           :doc "the entity names to look for, as the code spells them")
  ("callers" :boolean :default nil
             :doc "also list the entities that call it")
  ("context" :boolean :default nil
             :doc "include the body with the context around it, up to budget characters")
  ("budget" :integer :default 3000
            :doc "how many characters of context to allow when context is true")
  ("in" :string :default ""
        :doc "a path or directory to narrow the search to, when a name is defined twice"))

(nlk:define-contract sem-diff-query
  (:doc "Review changed entities rather than changed lines. Answers `sem diff [RANGE] --json'.")
  ("range" :string :default ""
           :doc "a ref, or BASE..HEAD, to diff; empty reviews the working tree against HEAD"))

(nlk:define-contract sem-impact-query
  (:doc "Ask Sem what depends on one entity, optionally which tests reach it. Answers
`sem impact NAME --json'.")
  ("name" :text :required t
          :doc "the entity whose dependents you want")
  ("tests" :boolean :default nil
           :doc "also list the tests that reach it")
  ("file" :string :default ""
          :doc "the file, when two entities share the name"))

(nlk:define-contract weave-preview-query
  (:doc "Preview how a branch would merge into the current HEAD, without applying anything.
Answers `weave preview BRANCH'.")
  ("branch" :text :required t
            :doc "the branch or ref to preview against HEAD; it is one argument, however it is spelled")
  ("file" :string :default ""
          :doc "one path to narrow the preview to, when a conflict is already known"))

;;; --- the four tools --------------------------------------------------------

(defun non-empty (text)
  "TEXT when it is a string with something in it, else NIL: how an optional
 argument that the CLI should not see is dropped from the argv."
  (and (stringp text) (plusp (length text)) text))

(defun sem-find (query)
  "The sem_find tool: where the named entities are, who calls them, or their
 bodies with context -- one `sem find --json' per call."
  (running-settings)
  (let ((names (coerce (sem-find-query-names query) 'list)))
    (unless names
      (fail "sem_find needs at least one name to look for"))
    (call "sem find"
          (append (sem-argv "find")
                  names
                  (when (sem-find-query-callers query) (list "--callers"))
                  (when (sem-find-query-context query)
                    (list "--context" "--budget"
                          (princ-to-string (sem-find-query-budget query))))
                  (let ((in (non-empty (sem-find-query-in query))))
                    (when in (list "--in" in)))
                  (list "--json")))))

(defun sem-diff (query)
  "The sem_diff tool: changed entities for a range, or for the working tree
 against HEAD."
  (running-settings)
  (call "sem diff"
        (append (sem-argv "diff")
                (let ((range (non-empty (sem-diff-query-range query))))
                  (when range (list range)))
                (list "--json"))))

(defun sem-impact (query)
  "The sem_impact tool: what depends on the named entity, and which tests
 reach it."
  (running-settings)
  (call "sem impact"
        (append (sem-argv "impact")
                (list (sem-impact-query-name query))
                (when (sem-impact-query-tests query) (list "--tests"))
                (let ((file (non-empty (sem-impact-query-file query))))
                  (when file (list "--file" file)))
                (list "--json"))))

(defun weave-preview (query)
  "The weave_preview tool: how a branch would merge into HEAD, without
 applying the merge."
  (running-settings)
  (call "weave preview"
        (append (weave-argv "preview")
                (list (weave-preview-query-branch query))
                (let ((file (non-empty (weave-preview-query-file query))))
                  (when file (list "--file" file))))))

;;; --- the command and the prerequisite --------------------------------------

(defun prerequisite-text ()
  "What `/sem' answers with no arguments: both CLIs as the machine has them,
 and where the tools are."
  (format nil "sem: ~a~%weave: ~a~%~%~a"
          (or (version-of (setting :sem-path))
              (format nil "not found as ~s on PATH; /sem again after installing it" (setting :sem-path)))
          (or (version-of (setting :weave-path))
              (format nil "not found as ~s on PATH (needed only for merge previews)" (setting :weave-path)))
          "The tools are sem_find, sem_diff, sem_impact and weave_preview; (help :sem) says what each
answers and /sem find NAME --json runs Sem directly."))

(defun run-command (args session-id)
  "The /sem command: with no arguments, whether both CLIs are installed and
 where the tools are; with arguments, `sem ARGS...' itself."
  (declare (ignore session-id))
  (running-settings)
  (let ((typed (nlk:trimmed (or args ""))))
    (if (plusp (length typed))
        (call "sem" (append (sem-argv) (words typed)))
        (prerequisite-text))))

(defun missing-programs ()
  "The configured programs that are not there, by the name they were asked for."
  (remove nil
          (list (unless (version-of (setting :sem-path)) (setting :sem-path))
                (unless (version-of (setting :weave-path)) (setting :weave-path)))))

(defun start ()
  "Say once, on the board, what is not installed: the prerequisite the model
 would otherwise discover through a failing tool."
  (let ((missing (missing-programs)))
    (when missing
      (nle:notice (format nil "sem: ~{~a~^ and ~} not found on PATH; the sem tools answer the ~
prerequisite until they are installed, or the sem section names where they are" missing)
                  :level :warning :key "nodecode-sem")
      (nle:on-stop (lambda () (nle:notice nil :key "nodecode-sem"))))))

(nle:define-cell sem
  (:section ("sem")
    (:guide "nothing is required: sem and weave are looked for on PATH; set sem_path or weave_path when one lives elsewhere; offline keeps both CLIs off the network; directory is the repository they run in, the session's own otherwise")
    ("sem_path" :string :default "sem"
                :doc "the Sem executable (Ataraxy Labs' Sem, 0.27.0 or later), found on PATH")
    ("weave_path" :string :default "weave"
                  :doc "the Weave executable, found on PATH; only merge previews need it")
    ("directory" :string :default ""
                 :doc "the repository the CLIs run in; the asking session's directory when empty")
    ("offline" :boolean :default t
               :doc "run both CLIs with SEM_NO_NETWORK=1 SEM_NO_TELEMETRY=1, so neither reaches the cloud")
    ("timeout" :integer :default 120 :min 5
               :doc "seconds one CLI call may take before its whole process tree is killed")
    ("max_chars" :integer :default 30000 :min 500
                 :doc "how much of one answer the model is given"))
  (:tool "sem_find" (nlk:contract 'sem-find-query) 'sem-find)
  (:tool "sem_diff" (nlk:contract 'sem-diff-query) 'sem-diff)
  (:tool "sem_impact" (nlk:contract 'sem-impact-query) 'sem-impact)
  (:tool "weave_preview" (nlk:contract 'weave-preview-query) 'weave-preview)
  (:help :sem "sem: where an entity is defined, its callers and dependents, changed entities, and a branch's merge preview" +primer+)
  (:command "sem" 'run-command
            :description "Which of Sem and Weave are installed; or run sem directly"
            :argument-hint "FIND NAME --JSON")
  (:start #'start))
